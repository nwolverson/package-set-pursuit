{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude as P

import           Control.Arrow ((&&&))
import           Control.Exception (finally, tryJust)
import           Control.Lens ((<.), toListOf, to, ifolded, withIndex)
import           Data.Aeson (FromJSON, ToJSON, decodeStrict, encode)
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import           Data.ByteString (ByteString)
import           Data.Foldable (for_)
import           Data.Map (Map, fromList)
import qualified Data.Map as M
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text, unpack)
import           Data.Traversable (for)
import           Data.Foldable as Foldable
import           Filesystem.Path ((</>), (<.>))
import           Filesystem.Path.CurrentOS (fromText, toText, encodeString)
import           GHC.Generics (Generic)
import           Turtle hiding (fp, input, x, s, err, env)
import           System.IO.Error (isDoesNotExistError)

import qualified Web.Bower.PackageMeta as Bower

import qualified Data.Version as DV

import System.FilePath.Glob (glob)

import Control.Arrow ((***))
import Control.Category ((>>>))

import Text.ParserCombinators.ReadP

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import qualified Data.Text as T

import qualified Language.PureScript as P
import Language.PureScript.Docs (Package(..), VerifiedPackage)
import qualified Language.PureScript.Docs as D

import qualified Language.PureScript.Make.Monad as MM


main :: IO ()
main = do
  jsonString <- B.readFile "packages.json"
  case decodePackagesSpec jsonString of
    Nothing -> die "Bad JSON file"
    Just ps -> recursePackageSet ps

defaultPackageMeta :: Text -> Bower.PackageMeta
defaultPackageMeta pkgName = Bower.PackageMeta
  { Bower.bowerName = unsafeMkPackageName pkgName
  , Bower.bowerDescription = Nothing
  , Bower.bowerMain = []
  , Bower.bowerModuleType = []
  , Bower.bowerLicense = []
  , Bower.bowerIgnore = []
  , Bower.bowerKeywords = []
  , Bower.bowerAuthors = []
  , Bower.bowerHomepage = Nothing
  , Bower.bowerRepository = Nothing
  , Bower.bowerDependencies = []
  , Bower.bowerDevDependencies = []
  , Bower.bowerResolutions = []
  , Bower.bowerPrivate = False
  }

unsafeMkPackageName :: Text -> Bower.PackageName
unsafeMkPackageName = either (error "Bad package name") id . Bower.mkPackageName

getTransitiveDeps :: PackagesSpec -> [PackageName] -> [PackageName]
getTransitiveDeps db deps =
  let collected = (map (go Set.empty) deps) :: [Set PackageName]
  in
    Set.toList $ Foldable.fold collected
  where
    go :: Set PackageName -> PackageName -> Set PackageName
    go seen pkg
      | pkg `Set.member` seen =
          error $ T.unpack ("Cycle in package dependencies at package " <> pkg)
      | otherwise =
        case M.lookup pkg db of
          Nothing ->
            error $ "Package not found: " <> T.unpack pkg
          Just PackageSpec{ dependencies } ->
            Set.insert pkg $ Foldable.fold $ map (go (Set.insert pkg seen)) dependencies

genPackage :: PackageName -> PackageSpec -> PackagesSpec -> IO VerifiedPackage
genPackage name (PackageSpec{ repo, version, dependencies = specifiedDependencies }) specs = do
  let dependencies = getTransitiveDeps specs specifiedDependencies
  pkgs <- pkgGlob name
  pkgdeps <- traverse (\dep ->
    do
      files <- pkgGlob dep
      pure $ map (unsafeMkPackageName dep,) files
      ) dependencies


  -- Boilerplate
  pkgVersion <- case parseVersion version of
    Just v -> pure v
    Nothing -> error $ ">> couldn't parse version '" <> unpack version <> "'"
  let pkgVersionTag = version
  let pkgTagTime = Nothing
  pkgGithub <- case extractGithub repo of
    Just gh -> pure gh
    Nothing -> error $ "extracting github from " <> show repo

  let pkgResolvedDependencies = map (\pkgName -> (unsafeMkPackageName pkgName, fromMaybe (error $ "couldn't parse version for " <> T.unpack pkgName) $ (parseDVVersion =<< getVersion pkgName))) dependencies
  let pkgMeta = (defaultPackageMeta name) {
    Bower.bowerDependencies = flip map dependencies $ \packageName ->
      (unsafeMkPackageName packageName, Bower.VersionRange $ fromMaybe "1.0.0-unknown-version" $ getVersion packageName)
  }

  let pkgUploader = D.GithubUser "nwolverson" -- TODO
  let pkgCompilerVersion = P.version

  (res, _warnings) <- MM.runMake (P.defaultOptions { P.optionsCodegenTargets = Set.singleton P.CoreFn }) $ do
          (pkgModules, pkgModuleMap) <- D.collectDocs "output" pkgs (concat pkgdeps)
          pure (pkgModules, pkgModuleMap)
  case res of
    Left errs -> do
      (echo . unsafeTextToLine . T.pack . P.prettyPrintMultipleErrors P.defaultPPEOptions) errs
      error $ "Errors building " <> T.unpack name
    Right (pkgModules', pkgModuleMap) -> do
      let pkgModules = map snd pkgModules'
      return Package{..}
  

  where
  pkgGlob pkg = glob ("packages/" <> T.unpack pkg <> "/*/src/**/*.purs")

  parseDVVersion :: Text -> Maybe DV.Version
  parseDVVersion vVersionString =
    let versionString = fromMaybe vVersionString (T.stripPrefix "v" vVersionString) in
    fmap fst $ List.find (null . snd) (readP_to_S DV.parseVersion $ unpack versionString)

  getVersion :: PackageName -> Maybe Version
  getVersion pkgName = fmap (\(PackageSpec { version = v }) -> v) $ M.lookup pkgName specs

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist inner = do
  r <- tryJust (guard . isDoesNotExistError) inner
  case r of
    Left () ->
      return Nothing
    Right x ->
      return (Just x)

readJSONFile :: Aeson.FromJSON a => P.FilePath -> IO (Maybe a)
readJSONFile path = do
    r <- catchDoesNotExist $ Aeson.decodeFileStrict' path
    return $ join r

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

parseVersion :: Text -> Maybe DV.Version
parseVersion str =
  let digits = fromMaybe str (T.stripPrefix "v" str) in
  Just $ fst $ last $ readP_to_S DV.parseVersion (unpack digits)

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
  _ <- fromList <$>
    (for (getGitRepoList ps) (\(name, (repo, version)) -> do
      let pkgDir = "packages" </> fromText name </> fromText version
      exists <- testdir pkgDir
      unless exists $ clone repo version pkgDir
      return (name, pkgDir)))

  for_ (M.toList ps) $ \(name, pspec@PackageSpec{}) -> do
    echo (unsafeTextToLine $ "Package " <> name)
    pkg <- genPackage name pspec ps
    let dir = fromText "data" </> fromText name
    mktree dir
    LB.writeFile (encodeString $ dir </> fromString (DV.showVersion $ pkgVersion pkg) <.> "json") (encode pkg)

    pure ()
