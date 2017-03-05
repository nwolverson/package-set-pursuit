{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import           Control.Arrow ((&&&))
import           Control.Exception (finally)
import qualified Control.Foldl as Fold
import           Control.Lens ((<.), toListOf, to, ifolded, withIndex)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON, decodeStrict, encode, (.=))
import qualified Data.Aeson as A
import           Data.ByteString (ByteString)
import           Data.Foldable (for_)
import           Data.Map (Map, toList, fromList)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe, fromJust, maybeToList)
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (decodeUtf8')
import           Data.Traversable (for)
import           Filesystem.Path ((</>), (<.>))
import           Filesystem.Path.CurrentOS (fromText, toText, encodeString)
import           GHC.Generics (Generic)
import           Turtle
import Control.Monad.Except
import qualified Web.Bower.PackageMeta as Bower


import qualified Data.Version as DV

import System.FilePath.Glob (glob)

import Debug.Trace

import Control.Arrow ((***))
import Control.Category ((>>>))

import Text.ParserCombinators.ReadP


import Language.PureScript.Publish.ErrorsWarnings
import Language.PureScript.Publish.Utils
import Language.PureScript.Publish(PrepareM(), runPrepareM)

import Control.Monad.Writer.Strict (MonadWriter, WriterT, runWriterT, tell)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Data.Text as T

import qualified Language.PureScript as P

import qualified Language.PureScript.Docs as D

main :: IO ()
main = do
  jsonString <- B.readFile "packages.json"
  case decodePackagesSpec jsonString of
    Nothing -> die "Bad JSON file"
    Just ps -> recursePackageSet ps


---


data Package a = Package
  { pkgVersion              :: DV.Version
  , pkgVersionTag           :: Text
  -- TODO: When this field was introduced, it was given the Maybe type for the
  -- sake of backwards compatibility, as older JSON blobs will not include the
  -- field. It should eventually be changed to just UTCTime.
  , pkgTagTime              :: Maybe UTCTime
  , pkgModules              :: [D.Module]
  , pkgModuleMap            :: Map P.ModuleName Bower.PackageName
  , pkgResolvedDependencies :: [(PackageName, DV.Version)]
  , pkgGithub               :: (D.GithubUser, D.GithubRepo)
  , pkgUploader             :: a
  , pkgCompilerVersion      :: DV.Version
    -- ^ The version of the PureScript compiler which was used to generate
    -- this data. We store this in order to reject packages which are too old.
  }
  deriving (Show, Eq, Ord)


instance A.ToJSON a => A.ToJSON (Package a) where
  toJSON Package{..} =
    A.object $
      [ "version"              .= DV.showVersion pkgVersion
      , "versionTag"           .= pkgVersionTag
      , "modules"              .= pkgModules
      , "moduleMap"            .= D.assocListToJSON P.runModuleName
                                                  Bower.runPackageName
                                                  (M.toList pkgModuleMap)
      , "resolvedDependencies" .= D.assocListToJSON id
                                                  (T.pack . DV.showVersion)
                                                  pkgResolvedDependencies
      , "github"               .= pkgGithub
      , "uploader"             .= pkgUploader
      , "compilerVersion"      .= DV.showVersion P.version
      ] ++
      fmap (\t -> "tagTime" .= D.formatTime t) (maybeToList pkgTagTime)

type UploadedPackage = Package D.NotYetKnown
type VerifiedPackage = Package D.GithubUser



getPackage :: PackageName -> PackageSpec -> IO VerifiedPackage
getPackage name PackageSpec { repo, version, dependencies } = do
  -- (pkgVersionTag, pkgVersion) <- publishGetVersion opts
  pkgVersion <- case parseVersion version of
    Just v -> pure v
    Nothing -> error $ ">> couldn't parse version " <> unpack version
  let pkgVersionTag = version
  let pkgTagTime = Nothing
  pkgGithub <- case extractGithub repo of
    Just gh -> pure gh
    Nothing -> error $ "extracting github from " <> show repo

  pkgs <- glob ( "packages/" <> (T.unpack name) <> "/*/src/**/*.purs")
  traceShowM ("Count for " <> name, length pkgs)
  pkgdeps <- traverse (\dep ->
    do
      files <- glob ( "packages/" <> (T.unpack dep) <> "/*/src/**/*.purs")
      traceShowM ("Count deps for " <> name <> ": " <> dep, length files)
      depName <- either (error "Bad package name") pure $ Bower.mkPackageName dep
      pure $ map (depName,) files
      ) dependencies
  traceShowM pkgdeps


  (modules', moduleMap) <-  either (error "parseFiles") fst <$> runPrepareM (parseFilesInPackages pkgs (concat pkgdeps))
  (pkgModules, pkgModuleMap) <- case runExcept (D.convertModulesInPackage modules' moduleMap) of
    Right modules -> return (modules, moduleMap)
    Left err -> do
      traceShowM ("Converting modules", name, err)
      pure ([], mempty)

  -- let (pkgModules, pkgModuleMap) = (modules', mempty) -- <- getModules
  let pkgMeta = Nothing

  -- let declaredDeps = map fst (bowerDependencies pkgMeta ++
  --                             bowerDevDependencies pkgMeta)
  -- pkgResolvedDependencies     <- getResolvedDependencies declaredDeps
  let pkgResolvedDependencies = map (,DV.makeVersion [1,0,0]) dependencies -- TODO


  let pkgUploader = D.GithubUser "nwolverson"
  let pkgCompilerVersion = P.version

  traceShowM pkgResolvedDependencies

  return Package{..}

  where
    parseVersion str =
      let digits = fromMaybe str (T.stripPrefix "v" str) in
      Just $ fst $ last $ readP_to_S DV.parseVersion (unpack digits)

    parseFilesInPackages inputFiles depsFiles = do
      r <- liftIO . runExceptT $ D.parseFilesInPackages inputFiles depsFiles
      case r of
        Right r' ->
          return r'
        Left err ->
          error "Error parsing files"
          -- userError (CompileError err)

extractGithub :: Text -> Maybe (D.GithubUser, D.GithubRepo)
extractGithub = stripGitHubPrefixes
   >>> fmap (T.splitOn "/")
   >=> takeTwo
   >>> fmap (D.GithubUser *** (D.GithubRepo . dropDotGit))

  where
  takeTwo :: [a] -> Maybe (a, a)
  takeTwo [x, y] = Just (x, y)
  takeTwo _ = Nothing

  stripGitHubPrefixes :: Text -> Maybe Text
  stripGitHubPrefixes = stripPrefixes [ "git://github.com/"
                                      , "https://github.com/"
                                      , "git@github.com:"
                                      ]

  stripPrefixes :: [Text] -> Text -> Maybe Text
  stripPrefixes prefixes str = msum $ (`T.stripPrefix` str) <$> prefixes

  dropDotGit :: Text -> Text
  dropDotGit str
    | ".git" `T.isSuffixOf` str = T.take (T.length str - 4) str
    | otherwise = str
---

type PackagesSpec = Map PackageName PackageSpec

type Repo = Text

type Version = Text

type PackageName = Text

data PackageSpec = PackageSpec
  { repo         :: Repo
  , version      :: Version
  , dependencies :: [PackageName]
  } deriving (Show, Eq, Generic, FromJSON, ToJSON)

decodePackagesSpec :: ByteString -> Maybe PackagesSpec
decodePackagesSpec = decodeStrict

getGitRepoList :: PackagesSpec -> [(PackageName, (Repo, Version))]
getGitRepoList = toListOf ((ifolded <. to (repo &&& version)) . withIndex)

clone :: Repo -> Version -> Turtle.FilePath -> IO ()
clone repo version into = sh $
    procs "git"
          [ "clone"
          , repo
          , "-b", version
          , toTextUnsafe into
          ] empty


toTextUnsafe :: Turtle.FilePath -> Text
toTextUnsafe = explode . toText where
  explode = either (error . show) id

pushd :: Turtle.FilePath -> IO a -> IO a
pushd dir x = do
  cur <- pwd
  cd dir
  a <- x `finally` cd cur
  return a

recursePackageSet :: PackagesSpec -> IO ()
recursePackageSet ps = do
  -- Clone all repos into the packages/ directory
  paths <- fromList <$>
    (for (getGitRepoList ps) (\(name, (repo, version)) -> do
      let pkgDir = "packages" </> fromText name </> fromText version
      exists <- testdir pkgDir
      unless exists $ clone repo version pkgDir
      return (name, pkgDir)))

  for_ (toList ps) $ \(name, pspec@PackageSpec{}) -> do
    let dirFor name = fromMaybe (error $ "verifyPackageSet: no directory " <> unpack name) . (`M.lookup` paths) $ name
    echo (unsafeTextToLine $ "Package " <> name)
    pkg <- getPackage name pspec
    let dir = fromText "data" </> fromText name
    mkdir dir
    LB.writeFile (encodeString $ dir </> fromString (DV.showVersion $ pkgVersion pkg) <.> "json") (encode pkg)

    pure ()
