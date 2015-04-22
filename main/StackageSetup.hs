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

import ClassyPrelude.Conduit hiding ((<>))
import Control.Applicative
import Crypto.Hash
import Crypto.Hash.Conduit (sinkHash)
import Data.Aeson (FromJSON(..), withObject, (.:))
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
import Network.HTTP.Types (hUserAgent)
import Filesystem
import qualified Filesystem.Path.CurrentOS as Path
import System.Environment (lookupEnv)
import System.Process (callProcess)
import qualified Paths_stackage_cli as CabalInfo

import Prelude (Bool(..))

version :: String
version = $(simpleVersion CabalInfo.version)

header :: String
header = "Retrieve ghc, cabal, etc, for use with stackage"

progDesc :: String
progDesc = header

userAgent :: ByteString
userAgent = "stackage-setup"

main :: IO ()
main = do
  (target,()) <- simpleOptions
    version
    header
    progDesc
    setupTargetParser
    empty
  withManager
    $ withStackageHome
    $ curryReaderT R
    $ setup target

data R = R
  { rStackageHome :: StackageHome
  , rManager :: Manager
  }

instance HasHttpManager R where
  getHttpManager = rManager

type StackageHome = Path.FilePath

class HasStackageHome env where
  accessStackageHome :: env -> StackageHome
instance HasStackageHome R where
  accessStackageHome = rStackageHome

-- TODO: check environment properly
getStackageHomeIO :: IO StackageHome
getStackageHomeIO = lookupEnv "STACKAGE_ROOT_DIR" >>= \case
  Just dir -> return $ Path.decodeString dir
  Nothing -> lookupEnv "HOME" >>= \case
    Just dir -> return $ Path.decodeString dir </> ".stackage"
    Nothing -> fail "Couldn't find stackage root dir"

class GetStackageHome m where
  getStackageHome :: m StackageHome
instance GetStackageHome IO where
  getStackageHome = getStackageHomeIO
instance (HasStackageHome env, Monad m)
  => GetStackageHome (ReaderT env m) where
  getStackageHome = liftM accessStackageHome ask


withStackageHome :: (MonadIO m)
  => ReaderT StackageHome m a -> m a
withStackageHome m = do
  stackageHome <- liftIO getStackageHomeIO
  runReaderT m stackageHome

curryReaderT :: (r1 -> r2 -> env) -> ReaderT env m a -> ReaderT r1 (ReaderT r2 m) a
curryReaderT tup m =
  ReaderT $ \r1 ->
  ReaderT $ \r2 ->
  runReaderT m $ tup r1 r2

stackageHost :: String
stackageHost = "http://localhost:3000"

arch :: String
arch = "linux64"

ltsSnapshotsReq :: MonadThrow m => m Request
ltsSnapshotsReq = parseUrl $ stackageHost <> "/download/lts-snapshots.json"

ghcMajorVersionReq :: MonadThrow m => Snapshot -> m Request
ghcMajorVersionReq snapshot = parseUrl $
  stackageHost <> "/snapshot/" <> snapshot <> "/ghc-major-version"

getLinksReq :: MonadThrow m => GhcMajorVersion -> m Request
getLinksReq ghcMajorVersion = parseUrl $
  stackageHost <> "/static/setup/" <> arch <> "/ghc-" <> ghcMajorVersion <> "-links.yaml"

setupTargetParser :: Parser SetupTarget
setupTargetParser = strArgument mods where
  mods = metavar "TARGET" <> value "lts"

refreshLtsSnapshots ::
  ( HasHttpManager env
  , MonadReader env m
  , GetStackageHome m
  , MonadThrow m
  , MonadIO m
  ) => m (HashMap String String)
refreshLtsSnapshots = do
  stackageHome <- getStackageHome
  let path = Path.encodeString $ stackageHome </> ltsSnapshotsPath

  response <- httpLbs =<< ltsSnapshotsReq
  let lbs = responseBody response
  liftIO $ LByteString.writeFile path lbs

  either (throwM . ParseLtsSnapshotsError) return $ Aeson.eitherDecode lbs


getLinks ::
  ( MonadIO m
  , MonadThrow m
  , MonadReader env m
  , HasHttpManager env
  ) => GhcMajorVersion -> m [Download]
getLinks ghcMajorVersion = do
  response <- httpLbs =<< getLinksReq ghcMajorVersion
  let lbs = responseBody response
      bs = LByteString.toStrict lbs
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
  , GetStackageHome m
  , MonadThrow m
  , MonadIO m
  ) => String -> m GhcMajorVersion
getGhcMajorVersion target = case readGhcVersion target of
  Just version -> return version
  Nothing -> do
    snapshot <- case readSeries target of
      Just series -> lookupSnapshot series
      Nothing -> return target -- just try using it as a snapshot
    putStrLn $ "setup for snapshot: " <> pack snapshot
    lookupGhcMajorVersion snapshot

lookupSnapshot ::
  ( HasHttpManager env
  , MonadReader env m
  , GetStackageHome m
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
  , GetStackageHome m
  , MonadThrow m
  , MonadIO m
  , MonadBaseControl IO m
  ) => SetupTarget -> m ()
setup target = do
  ghcMajorVersion <- getGhcMajorVersion target
  putStrLn $ "Selecting ghc-" <> pack ghcMajorVersion
  links <- getLinks ghcMajorVersion

  forM_ links $ \d@Download{..} -> do
    stackageHome <- getStackageHome

    let dir = stackageHome </> downloadDir downloadName
    liftIO $ createTree dir

    let versionedDir = dir </> downloadPath downloadName downloadVersion
    exists <- liftIO $ isDirectory versionedDir

    if (not exists)
      then do
        download downloadUrl downloadSha1 dir
        postDownload d dir versionedDir
      else putStrLn $ "Already have: " <> downloadName <> "-" <> downloadVersion


postDownload :: (MonadIO m)
  => Download -> Path.FilePath -> Path.FilePath -> m ()
postDownload Download{..} dir versionedDir = do
  mapM_ (runInstruction dir) downloadInstructions

runInstruction :: MonadIO m => Path.FilePath -> Text -> m ()
runInstruction dir = go where
  go = putStrLn -- TODO: unstub

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
  putStrLn $ "downloading: " <> pack fname
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
      fail "corrupted download"
    putStrLn $ "Verified sha1: " <> pack sha1
      -- TODO: nicer error message
    unzipDownload dir file

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
unzipXZ dir file = liftIO $ do
  workingDir <- getWorkingDirectory
  let inDir = setWorkingDirectory dir
      outDir = setWorkingDirectory workingDir
  bracket_ inDir outDir $ do
    putStrLn "Decompressing XZ archive"
    callProcess "tar"
      ["xJf"
      , Path.encodeString file
      ]
    removeFile file


-- TODO: make cross platform
unzipGZ :: (MonadIO m)
  => Path.FilePath -> Path.FilePath -> m ()
unzipGZ dir file = liftIO $ do
  workingDir <- getWorkingDirectory
  let inDir = setWorkingDirectory dir
      outDir = setWorkingDirectory workingDir
  bracket_ inDir outDir $ do
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
