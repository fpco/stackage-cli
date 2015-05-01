{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import qualified ClassyPrelude.Conduit as ClassyPrelude
import ClassyPrelude.Conduit hiding ((<>))
import Control.Applicative
import Crypto.Hash
import Crypto.Hash.Conduit (sinkHash)
import Data.Aeson (FromJSON(..), withObject, (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Monoid
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Encoding as LText
import qualified Data.Yaml as Yaml
import Options.Applicative (Parser, strArgument, metavar, value)
import Stackage.CLI
import Network.HTTP.Client.Conduit
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (hUserAgent)
import Filesystem
import qualified Filesystem.Path.CurrentOS as Path
import System.FilePath (searchPathSeparator, getSearchPath)
import System.Environment (lookupEnv, getEnv, setEnv)
import System.Exit (exitFailure)
import System.Process (callProcess, readProcess)
import qualified Paths_stackage_cli as CabalInfo

import qualified Prelude
import Prelude (Bool(..))

version :: String
version = $(simpleVersion CabalInfo.version)

header :: String
header = "Retrieve ghc, cabal, etc, for use with stackage"

progDesc :: String
progDesc = header

userAgent :: ByteString
userAgent = "stackage-setup"

stackageHostDefault :: String
stackageHostDefault = "https://www.stackage.org"


main :: IO ()
main = do
  (target,()) <- simpleOptions
    version
    header
    progDesc
    setupTargetParser
    empty
  withManagerSettings tlsManagerSettings
    $ withConfig
    $ curryReaderT R
    $ setup target

data R = R
  { rConfig :: Config
  , rManager :: Manager
  }

instance HasHttpManager R where
  getHttpManager = rManager

data Config = Config
  { configStackageRoot :: FilePath
  , configStackageHost :: String
  }

data StackageConfig = StackageConfig
  { _stackageHost :: String }

instance FromJSON StackageConfig where
  parseJSON = withObject "StackageConfig" $ \obj -> do
    _stackageHost <- obj .:? "stackage-host" .!= stackageHostDefault
    return StackageConfig{..}

-- Check if given processes appear to be present.
-- Halts the program with exit failure if any are missing.
-- TODO: make cross platform
checkDependencies :: [String] -> IO ()
checkDependencies deps = do
  missing <- mapM checkIsMissing deps
  when (or missing) $ do
    hPutStrLn stderr $ asText $
      "Please install missing dependencies and try again."
    exitFailure

-- return True if it appears to be missing
checkIsMissing :: String -> IO Bool
checkIsMissing process = do
  isMissing <- procIsMissing process
    `catch` \(e :: IOException) -> return True
  if isMissing
    then do
      hPutStrLn stderr $
        "Couldn't find required dependency: " <> process
      return True
    else return False

-- return True if it appears to be missing
procIsMissing :: String -> IO Bool
procIsMissing process = do
  output <- readProcess process ["--version"] ""
  return $ null output


getStackageRoot :: GetConfig m => m FilePath
getStackageRoot = liftM configStackageRoot getConfig

getStackageHost :: GetConfig m => m String
getStackageHost = liftM configStackageHost getConfig

class HasConfig env where
  accessConfig :: env -> Config
instance HasConfig R where
  accessConfig = rConfig

-- TODO: check environment properly
getStackageRootIO :: IO FilePath
getStackageRootIO = do
  stackageRoot <- lookupEnv "STACKAGE_ROOT" >>= \case
    Just dir -> return $ Path.decodeString dir
    Nothing -> do
      -- TODO: windows-ify
      home <- lookupEnv "HOME" >>= \case
        Just homeStr -> return (fromString homeStr)
        Nothing -> throwIO StackageRootNotFound
      return (home </> ".stackage")
  createTree stackageRoot
  return stackageRoot

readFileMay :: FilePath -> IO (Maybe ByteString)
readFileMay path = do
  exists <- isFile path
  if exists
    then Just <$> ClassyPrelude.readFile path
    else return Nothing

getFirstJust :: [IO (Maybe a)] -> IO (Maybe a)
getFirstJust [] = return Nothing
getFirstJust (xIO:xsIO) = do
  xMay <- xIO
  case xMay of
    Just{} -> return xMay
    Nothing -> getFirstJust xsIO

getStackageHostIO :: FilePath -> IO String
getStackageHostIO stackageRoot = do
  bsMay <- getFirstJust
    [ readFileMay (stackageRoot </> "config")
    , readFileMay (stackageRoot </> "config.yaml")
    , readFileMay (stackageRoot </> "config.yml")
    ]
  case bsMay of
    Nothing -> return stackageHostDefault
    Just bs -> case Yaml.decodeEither' bs of
      Left{} -> return stackageHostDefault
      Right stackageConfig -> return (_stackageHost stackageConfig)


getConfigIO :: IO Config
getConfigIO = do
  configStackageRoot <- getStackageRootIO
  configStackageHost <- getStackageHostIO configStackageRoot
  return Config{..}

class Monad m => GetConfig m where
  getConfig :: m Config
instance GetConfig IO where
  getConfig = getConfigIO
instance (HasConfig env, Monad m)
  => GetConfig (ReaderT env m) where
  getConfig = liftM accessConfig ask


withConfig :: (MonadIO m)
  => ReaderT Config m a -> m a
withConfig m = do
  config <- liftIO getConfigIO
  runReaderT m config

curryReaderT :: (r1 -> r2 -> env) -> ReaderT env m a -> ReaderT r1 (ReaderT r2 m) a
curryReaderT tup m =
  ReaderT $ \r1 ->
  ReaderT $ \r2 ->
  runReaderT m $ tup r1 r2

arch :: String
arch = "linux64"

ltsSnapshotsReq :: (MonadThrow m, GetConfig m) => m Request
ltsSnapshotsReq = do
  stackageHost <- getStackageHost
  parseUrl $ stackageHost <> "/download/lts-snapshots.json"

ghcMajorVersionReq :: (MonadThrow m, GetConfig m) => Snapshot -> m Request
ghcMajorVersionReq snapshot = do
  stackageHost <- getStackageHost
  parseUrl $ stackageHost <> "/snapshot/" <> snapshot <> "/ghc-major-version"

getLinksReq :: (MonadThrow m, GetConfig m) => GhcMajorVersion -> m Request
getLinksReq ghcMajorVersion = do
  stackageHost <- getStackageHost
  parseUrl $ stackageHost <> "/download/" <> arch <> "/ghc-" <> ghcMajorVersion <> "-links.yaml"

setupTargetParser :: Parser SetupTarget
setupTargetParser = strArgument mods where
  mods = metavar "TARGET" <> value "lts"

refreshLtsSnapshots ::
  ( HasHttpManager env
  , MonadReader env m
  , GetConfig m
  , MonadThrow m
  , MonadIO m
  ) => m (HashMap String String)
refreshLtsSnapshots = do
  stackageRoot <- getStackageRoot
  let path = Path.encodeString $ stackageRoot </> ltsSnapshotsPath

  response <- httpLbs =<< ltsSnapshotsReq
  let lbs = responseBody response
  liftIO $ LByteString.writeFile path lbs

  either (throwM . ParseLtsSnapshotsError) return $ Aeson.eitherDecode lbs


getLinks ::
  ( MonadIO m
  , MonadThrow m
  , MonadReader env m
  , GetConfig m
  , HasHttpManager env
  ) => GhcMajorVersion -> m [Download]
getLinks ghcMajorVersion = do
  stackageRoot <- getStackageRoot
  response <- httpLbs =<< getLinksReq ghcMajorVersion
  let lbs = responseBody response
      bs = LByteString.toStrict lbs
      path = Path.encodeString $ stackageRoot </> linksPath ghcMajorVersion
  liftIO $ LByteString.writeFile path lbs

  either (throwM . ParseLinksError) return $ Yaml.decodeEither' bs

data Download = Download
  { downloadName :: Text
  , downloadVersion :: Text
  , downloadUrl :: String
  , downloadSha1 :: String
  , downloadInstructions :: [Text]
  }

instance FromJSON Download where
  parseJSON = withObject "Download" $ \obj -> do
    downloadName         <- obj .: "name"
    downloadVersion      <- obj .: "version"
    downloadUrl          <- obj .: "url"
    downloadSha1         <- obj .: "sha1"
    downloadInstructions <- obj .: "instructions"
    return Download{..}

type SetupTarget = String
type GhcMajorVersion = String
type Series = String
type Snapshot = String

data SetupExceptions
  = SeriesNotFound Series
  | ParseLtsSnapshotsError String
  | ParseLinksError Yaml.ParseException
  | StackageRootNotFound
  deriving (Show, Typeable)
instance Exception SetupExceptions

readSeries :: String -> Maybe Series
readSeries s@"lts" = Just s
readSeries s@(stripPrefix "lts-" -> Just sver)
  | all Char.isNumber sver = Just s
readSeries (stripPrefix "lts/" -> Just sver)
  | all Char.isNumber sver = Just $ "lts-" <> sver
readSeries _ = Nothing

readGhcVersion :: String -> Maybe GhcMajorVersion
readGhcVersion (stripPrefix "ghc-" -> Just s) = case break (== '.') s of
  (m1, '.':m2) | all Char.isNumber m1 && all Char.isNumber m2
    -> Just s
  _ -> Nothing
readGhcVersion _ = Nothing

getGhcMajorVersion ::
  ( HasHttpManager env
  , MonadReader env m
  , GetConfig m
  , MonadThrow m
  , MonadIO m
  ) => String -> m GhcMajorVersion
getGhcMajorVersion target = case readGhcVersion target of
  Just version -> return version
  Nothing -> do
    snapshot <- case readSeries target of
      Just series -> lookupSnapshot series
      Nothing -> return target -- just try using it as a snapshot
    putStrLn $ "Setup for snapshot: " <> pack snapshot
    lookupGhcMajorVersion snapshot

lookupSnapshot ::
  ( HasHttpManager env
  , MonadReader env m
  , GetConfig m
  , MonadThrow m
  , MonadIO m
  ) => Series -> m Snapshot
lookupSnapshot series = do
  ltsSnapshots <- refreshLtsSnapshots
  case HashMap.lookup series ltsSnapshots of
    Just snapshot -> return snapshot
    Nothing -> throwM $ SeriesNotFound series


lookupGhcMajorVersion ::
  ( HasHttpManager env
  , MonadReader env m
  , GetConfig m
  , MonadThrow m
  , MonadIO m
  ) => Snapshot -> m GhcMajorVersion
lookupGhcMajorVersion snapshot = do
  response <- httpLbs =<< ghcMajorVersionReq snapshot
  let lbs = responseBody response
  return $ unpack $ LText.toStrict $ LText.decodeUtf8 lbs


setup ::
  ( HasHttpManager env
  , MonadReader env m
  , GetConfig m
  , MonadThrow m
  , MonadIO m
  , MonadBaseControl IO m
  ) => SetupTarget -> m ()
setup target = do
  ghcMajorVersion <- getGhcMajorVersion target
  putStrLn $ "Selecting ghc-" <> pack ghcMajorVersion
  links <- getLinks ghcMajorVersion

  stackageRoot <- getStackageRoot
  forM_ links $ \d@Download{..} -> do
    let dir = stackageRoot </> downloadDir downloadName
    liftIO $ createTree dir

    let versionedDir = dir </> downloadPath downloadName downloadVersion
    exists <- liftIO $ isDirectory versionedDir

    if (not exists)
      then do
        liftIO $ checkDependencies (depsFor downloadName)
        download downloadUrl downloadSha1 dir
        postDownload d dir versionedDir
      else putStrLn $ "Already have: " <> downloadName <> "-" <> downloadVersion

    augmentPath (versionedDir </> "bin")

-- The dependencies required for stackage-setup's implementation of
-- the instructions for a given download item.
depsFor :: Text -> [String]
depsFor "ghc"      = ["make", "tar", "xz"]
depsFor "cabal"    = ["tar", "gzip"]
depsFor "stackage" = ["tar", "xz"]
depsFor "alex"     = ["tar", "xz"]
depsFor "happy"    = ["tar", "xz"]
depsFor _          = []

-- TODO: ensure this works cross-platform
augmentPath :: MonadIO m => FilePath -> m ()
augmentPath pathHead = liftIO $ do
  pathRest <- getSearchPath
  let paths = fpToString pathHead : pathRest
      path = intercalate [searchPathSeparator] paths
  setEnv "PATH" path


postDownload :: (MonadIO m)
  => Download -> Path.FilePath -> Path.FilePath -> m ()
postDownload d@Download{..} dir versionedDir = do
  expectInstructions downloadName d dir versionedDir

expectInstructions :: (MonadIO m) => Text -> Download -> Path.FilePath -> Path.FilePath -> m ()
expectInstructions "ghc" = expectGhcInstructions
expectInstructions "cabal" = expectCabalInstructions
expectInstructions "stackage" = justUnpackInstructions
expectInstructions "alex" = justUnpackInstructions
expectInstructions "happy" = justUnpackInstructions
expectInstructions t = unexpectedInstructions t

unexpectedInstructions :: (MonadIO m) => Text -> Download -> FilePath -> FilePath -> m ()
unexpectedInstructions t Download{..} dir _ = do
  putStrLn $ "Unexpected download: " <> t
  when (not $ null downloadInstructions) $ do
    putStrLn $ "Manual instructions:"
    putStrLn $ "$ cd " <> fpToText dir
    mapM_ (putStrLn . ("$ " <>)) downloadInstructions

justUnpackInstructions :: (MonadIO m) => Download -> FilePath -> FilePath -> m ()
justUnpackInstructions Download{..} dir _ = do
    liftIO $ go downloadInstructions
  where
    go :: [Text] -> IO ()
    go [] = return ()
    go ( (stripPrefix "tar xJf " -> Just file)
       : next
       ) = unzipXZ dir (Path.fromText file) >> go next
    go ( (stripPrefix "rm " -> Just _file)
       : next
       ) = go next -- already done in unzipXZ
    go (t:_) = fail $ "command not recognized: " <> unpack t


expectGhcInstructions :: (MonadIO m) => Download -> Path.FilePath -> Path.FilePath -> m ()
expectGhcInstructions Download{..} dir versionedDir =
    liftIO $ go downloadInstructions
  where
    go :: [Text] -> IO ()
    go [] = return ()
    go ( (stripPrefix "tar xJf " -> Just file)
       : next
       ) = unzipXZ dir (Path.fromText file) >> go next
    go ( (stripPrefix "rm ghc-" -> Just _file)
       : next
       ) = go next -- already done in unzipXZ
    go ( (stripPrefix "cd ghc-" -> Just version)
       : "./configure --prefix=`pwd`"
       : "make install"
       : "cd .."
       : next
       ) | version == downloadVersion
       = ghcConfigureInstall versionedDir >> go next
    go (t:_) = fail $ "command not recognized: " <> unpack t


inDir :: Path.FilePath -> IO a -> IO a
inDir dir action = do
  workingDir <- getWorkingDirectory
  let enter = setWorkingDirectory dir
      exit = setWorkingDirectory workingDir
  bracket_ enter exit action


ghcConfigureInstall :: (MonadIO m) => Path.FilePath -> m ()
ghcConfigureInstall versionedDir = liftIO $ do
  let srcDir = versionedDir <.> "src"
  whenM (isDirectory srcDir) $ removeTree srcDir -- TODO: reuse instead
  rename versionedDir srcDir
  createTree versionedDir
  let go = inDir srcDir $ do
        callProcess "./configure" ["--prefix=" <> Path.encodeString versionedDir]
        callProcess "make" ["install"]
  go `onException` removeTree versionedDir
  removeTree srcDir

expectCabalInstructions :: (MonadIO m) => Download -> Path.FilePath -> Path.FilePath -> m ()
expectCabalInstructions Download{..} dir versionedDir =
    liftIO $ rememberingOldCabal $ go downloadInstructions
  where
    go [] = return ()
    go ( (stripPrefix "tar xzf " -> Just file)
       : next
       ) = unzipGZ dir (Path.fromText file) >> go next
    go ( (stripPrefix "rm cabal-install-" ->
          Just (stripSuffix ".tar.gz" -> Just _file))
       : next
       ) = go next -- already done in unzipGZ
    go ( (stripPrefix "cd cabal-install-" -> Just version)
       : "./bootstrap.sh"
       : "cd .."
       : (stripPrefix "rm -r cabal-install-" -> Just version')
       : next
       ) | version == downloadVersion && version == version'
       = cabalBootstrap dir version >> go next
    go ( (stripPrefix "mkdir cabal-" -> Just version)
       : (stripPrefix "mv $HOME/.cabal/bin/cabal cabal-" -> Just _newLoc)
       : next
       ) | version == downloadVersion
       = cabalMoveExe versionedDir >> go next
    go (t:_) = fail $ "command not recognized: " <> unpack t

cabalBootstrap :: FilePath -> Text -> IO ()
cabalBootstrap dir version = do
  let cabalInstallVersionedDir = dir </> Path.fromText ("cabal-install-" <> version)
  inDir cabalInstallVersionedDir $ do
    callProcess "./bootstrap.sh" []
  removeTree cabalInstallVersionedDir

cabalMoveExe :: FilePath -> IO ()
cabalMoveExe versionedDir = do
  let versionedDirBin = versionedDir </> "bin"
  createTree versionedDirBin
  homeCabal <- getHomeCabal
  rename homeCabal (versionedDirBin </> "cabal")

getHomeCabal :: IO FilePath
getHomeCabal = do
  -- TODO: errors
  Just home <- lookupEnv "HOME"
  return $ Path.decodeString home </> ".cabal" </> "bin" </> "cabal"

rememberingOldCabal :: IO a -> IO a
rememberingOldCabal action = do
  homeCabal <- getHomeCabal
  exists <- isFile homeCabal
  if exists
    then do
      let tempCabal = homeCabal <.> "old"
          toTemp = rename homeCabal tempCabal
          fromTemp = rename tempCabal homeCabal
      bracket_ toTemp fromTemp action
    else action

-- TODO: use Text more than String
download ::
  ( MonadIO m
  , MonadBaseControl IO m
  , MonadThrow m
  , MonadReader env m
  , HasHttpManager env
  ) => String -> String -> Path.FilePath -> m ()
download url sha1 dir = do
  -- TODO: deal with partial function last
  let fname = List.drop (List.last slashes + 1) url
      slashes = List.findIndices (== '/') url
      file = dir </> Path.decodeString fname
  putStrLn $ "Downloading: " <> pack fname
  req0 <- parseUrl url
  let req = req0
        { requestHeaders = [(hUserAgent, userAgent)] }
  runResourceT $ withResponse req $ \res -> do
    let source = responseBody res
    let sink = getZipSink $
          ZipSink sinkHash <* ZipSink (sinkFile file)
    hash <- source $$ sink
    let _ = hash :: Digest SHA1
    when (show hash /= sha1) $ do
      fail "Corrupted download"
    putStrLn $ "Verified sha1: " <> pack sha1

-- TODO: make cross platform
unzipDownload :: (MonadIO m)
  => Path.FilePath -> Path.FilePath -> m ()
unzipDownload dir file = case stripSuffix ".tar.xz" (fpToText file) of
  Just{} -> unzipXZ dir file
  _ -> case stripSuffix ".tar.gz" (fpToText file) of
    Just {} -> unzipGZ dir file
    _ -> fail $ "unzipDownload: unknown extension"
  -- TODO: other extensions

-- TODO: make cross platform
unzipXZ :: (MonadIO m)
  => Path.FilePath -> Path.FilePath -> m ()
unzipXZ dir file = liftIO $ inDir dir $ do
  putStrLn "Decompressing XZ archive"
  callProcess "tar"
    ["xJf"
    , Path.encodeString file
    ]
  removeFile file


-- TODO: make cross platform
unzipGZ :: (MonadIO m)
  => Path.FilePath -> Path.FilePath -> m ()
unzipGZ dir file = liftIO $ inDir dir $ do
  putStrLn "Decompressing GZ archive"
  callProcess "tar"
    ["xzf"
    , Path.encodeString file
    ]
  removeFile file

downloadDir :: Text -> Path.FilePath
downloadDir name =
  "environment" </> Path.fromText name

downloadPath :: Text -> Text -> Path.FilePath
downloadPath name version =
  Path.fromText (name <> "-" <> version)

ltsSnapshotsPath :: Path.FilePath
ltsSnapshotsPath = Path.fromText "lts-snapshots.json"

linksPath :: GhcMajorVersion -> Path.FilePath
linksPath ghcMajorVersion = Path.fromText $ "ghc-" <> pack ghcMajorVersion <> "-links.yaml"
