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
import           Control.Exception (finally, tryJust, toException)
import           Control.Lens ((<.), toListOf, to, ifolded, withIndex)
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON, ToJSON, decodeStrict, encode, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import           Data.ByteString (ByteString)
import           Data.Foldable (for_)
import           Data.Map (Map, toList, fromList)
import qualified Data.Map as M
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.Maybe (fromMaybe, fromJust, maybeToList, catMaybes, mapMaybe)
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (decodeUtf8')
import           Data.Traversable (for)
import           Data.Foldable as Foldable
import           Filesystem.Path ((</>), (<.>))
import           Filesystem.Path.CurrentOS (fromText, toText, encodeString)
import           GHC.Generics (Generic)
import           Turtle
import           System.IO.Error (tryIOError, isDoesNotExistError)
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
import Language.PureScript.Docs (Package(..), VerifiedPackage)
import qualified Language.PureScript.Docs as D
import qualified Language.PureScript.Docs.Convert as D

import qualified Language.PureScript.Make.Monad as MM

import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.Ide.Util as IDE

import System.Directory (makeAbsolute) 
import System.IO.UTF8 (readUTF8FileT)
import Data.List.NonEmpty (NonEmpty(..))
import Language.PureScript.Sugar.Names (externsEnv, primEnv)

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
          Just info@PackageSpec{ dependencies } ->
            let s = Foldable.fold $ map (go (Set.insert pkg seen)) dependencies
            in Set.insert pkg s

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


  let allFiles = pkgs <> concatMap (map snd) pkgdeps
  (res, warnings) <- MM.runMake (P.defaultOptions { P.optionsCodegenTargets = Set.singleton P.CoreFn }) $ do
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
  quotePkgGlob pkg = T.pack ("packages/" <> T.unpack pkg <> "/*/src/**/*.purs")

  parseDVVersion :: Text -> Maybe DV.Version
  parseDVVersion vVersionString =
    let versionString = fromMaybe vVersionString (T.stripPrefix "v" vVersionString) in
    fmap fst $ List.find (null . snd) (readP_to_S DV.parseVersion $ unpack versionString)

  getVersion :: PackageName -> Maybe Version
  getVersion pkgName = fmap (\(PackageSpec { version }) -> version) $ M.lookup pkgName specs


getPackage :: PackageName -> PackageSpec -> PackagesSpec -> IO VerifiedPackage
getPackage name (PackageSpec{ repo, version, dependencies = specifiedDependencies }) specs = do
  let dependencies = getTransitiveDeps specs specifiedDependencies
  pkgVersion <- case parseVersion version of
    Just v -> pure v
    Nothing -> error $ ">> couldn't parse version '" <> unpack version <> "'"
  let pkgVersionTag = version
  let pkgTagTime = Nothing
  pkgGithub <- case extractGithub repo of
    Just gh -> pure gh
    Nothing -> error $ "extracting github from " <> show repo

  pkgs <- pkgGlob name
  pkgdeps <- traverse (\dep ->
    do
      files <- pkgGlob dep
      pure $ map (unsafeMkPackageName dep,) files
      ) dependencies

  let allFiles = pkgs <> concatMap (map snd) pkgdeps
  
  modules <- forM pkgs $ \pkg -> do
    (fp, input) <- ideReadFile pkg
    case CST.parseFromFile fp input of
      Left (err :| _) ->
        error $ "Error for " <> fp <> ": " <> 
          CST.prettyPrintError err
      Right mod -> pure mod
  
  externsFiles <- glob ("packages/output/*/externs.json")

  externs :: [P.ExternsFile] <- catMaybes <$>  traverse readJSONFile externsFiles
  let externsMap :: M.Map P.ModuleName P.ExternsFile = M.fromList $ map (\ef -> (P.efModuleName ef, ef)) externs

  modRes <- forM [head modules] $ \modl ->
    MM.runMake P.defaultOptions $ do
       externs' <- sortExterns modl externsMap

       echo $ unsafeTextToLine  $"Module externs: " <> P.runModuleName (P.getModuleName modl) <> " " <> T.pack (show externs')

       exEnv <- fmap fst . runWriterT $ foldM externsEnv primEnv externs'

       let env = foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment externs'
           withPrim = P.importPrim modl
       ((_, env'), _) <- P.runSupplyT 0 $ do
            P.desugar exEnv externs' [withPrim] >>= \case
              [desugared] -> P.runCheck' (P.emptyCheckState env) $ P.typeCheckModule desugared
      
       modl' <- D.convertModule externs' exEnv env' modl
       pure modl
  let errs = catMaybes $
             map ((\case Left x -> Just x 
                         Right _ -> Nothing) . fst)
             modRes
  forM errs $ \err ->
    echo $ unsafeTextToLine $ T.pack $ P.prettyPrintMultipleErrors P.defaultPPEOptions err

  let pkgResolvedDependencies = map (\pkgName -> (unsafeMkPackageName pkgName, fromMaybe (error $ "couldn't parse version for " <> T.unpack pkgName) $ (parseDVVersion =<< getVersion pkgName))) dependencies
  let pkgMeta = (defaultPackageMeta name) {
    Bower.bowerDependencies = flip map dependencies $ \packageName ->
      (unsafeMkPackageName packageName, Bower.VersionRange $ fromMaybe "1.0.0-unknown-version" $ getVersion packageName)
  }

  let pkgUploader = D.GithubUser "nwolverson" -- TODO
  let pkgCompilerVersion = P.version

  return Package{..}

  where
    ideReadFile' fileReader fp = do
      absPath <- liftIO (makeAbsolute fp)
      contents <- liftIO (fileReader absPath)
      pure (absPath, contents)

    ideReadFile = ideReadFile' readUTF8FileT

    getVersion :: PackageName -> Maybe Version
    getVersion pkgName = fmap (\(PackageSpec { version }) -> version) $ M.lookup pkgName specs

    parseDVVersion :: Text -> Maybe DV.Version
    parseDVVersion vVersionString =
      let versionString = fromMaybe vVersionString (T.stripPrefix "v" vVersionString) in
      fmap fst $ List.find (null . snd) (readP_to_S DV.parseVersion $ unpack versionString)
    pkgGlob pkg = glob ("packages/" <> T.unpack pkg <> "/*/src/**/*.purs")

    sortExterns m ex = do
      sorted' <- runExceptT
              . P.sortModules P.moduleSignature
              . (:) m
              . map mkShallowModule
              . M.elems
              . M.delete (P.getModuleName m) $ ex
      case sorted' of
        Left err ->
          error "error sorting externs"
        Right (sorted, graph) -> do
          let deps = fromJust (List.lookup (P.getModuleName m) graph)
          pure $ mapMaybe getExtern (deps `inOrderOf` map P.getModuleName sorted)
      where
        mkShallowModule P.ExternsFile{..} =
          P.Module (P.internalModuleSourceSpan "<rebuild>") [] efModuleName (map mkImport efImports) Nothing
        mkImport (P.ExternsImport mn it iq) =
          P.ImportDeclaration (P.internalModuleSourceSpan "<rebuild>", []) mn it iq
        getExtern mn = M.lookup mn ex
        -- Sort a list so its elements appear in the same order as in another list.
        inOrderOf :: (Ord a) => [a] -> [a] -> [a]
        inOrderOf xs ys = let s = Set.fromList xs in filter (`Set.member` s) ys

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
  paths <- fromList <$>
    (for (getGitRepoList ps) (\(name, (repo, version)) -> do
      let pkgDir = "packages" </> fromText name </> fromText version
      exists <- testdir pkgDir
      unless exists $ clone repo version pkgDir
      return (name, pkgDir)))

  for_ (M.toList ps) $ \(name, pspec@PackageSpec{}) -> do
    let dirFor name = fromMaybe (error $ "verifyPackageSet: no directory " <> unpack name) . (`M.lookup` paths) $ name
    echo (unsafeTextToLine $ "Package " <> name)
    pkg <- genPackage name pspec ps
    let dir = fromText "data" </> fromText name
    mktree dir
    LB.writeFile (encodeString $ dir </> fromString (DV.showVersion $ pkgVersion pkg) <.> "json") (encode pkg)

    pure ()
