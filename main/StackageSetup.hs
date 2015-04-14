{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import ClassyPrelude.Conduit hiding ((<>))
import Control.Applicative
import Crypto.Hash
import Crypto.Hash.Conduit (sinkHash)
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Data.Monoid
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
  (snapshot,()) <- simpleOptions
    version
    header
    progDesc
    snapshotParser
    empty
  withManager
    $ withStackageHome
    $ curryReaderT R
    $ setup snapshot

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


type Snapshot = String

snapshotParser :: Parser Snapshot
snapshotParser = strArgument mods where
  mods = metavar "SNAPSHOT" <> value "lts"


isSeries :: Snapshot -> Bool
isSeries "lts" = True
isSeries (stripPrefix "lts-" -> Just s) = all Char.isNumber s
isSeries (stripPrefix "lts/" -> Just s) = all Char.isNumber s
isSeries _ = False



refreshLtsSnapshots :: MonadIO m => m (HashMap String String)
refreshLtsSnapshots = do
  -- TODO: unstub
  return $ HashMap.fromList
    [ ("lts", "lts-2.3")
    , ("lts-2", "lts-2.3")
    , ("lts-1", "lts-1.15")
    ]

downloadSnapshotData :: MonadIO m => Snapshot -> m (HashMap Text Download)
downloadSnapshotData "lts-2.3" = do
  -- TODO: unstub
  let ghc = Download
        { downloadName = "ghc"
        , downloadVersion = "7.8.4"
        , downloadUrl = "http://www.haskell.org/ghc/dist/7.8.4/ghc-7.8.4-x86_64-unknown-linux-deb7.tar.xz"
        , downloadSha1 = "11aec12d4bb27f6fa59dcc8535a7a3b3be8cb787"
        }
      cabal = Download
        { downloadName = "cabal"
        , downloadVersion = "1.20.0.3"
        , downloadUrl = "http://www.haskell.org/cabal/release/cabal-install-1.20.0.3/cabal-1.20.0.3-i386-unknown-linux.tar.gz"
        , downloadSha1 = "647ae3e561343a709b09ed70fa6bc7b1ce39e25b"
        }
  return $ HashMap.fromList
    [ ("ghc", ghc)
    , ("cabal", cabal)
    ]
downloadSnapshotData _ = error "downloadSnapshotData: stubbed out"

data Download = Download
  { downloadName :: Text
  , downloadVersion :: Text
  , downloadUrl :: String
  , downloadSha1 :: String
  }

setup ::
  ( HasHttpManager env
  , MonadReader env m
  , GetStackageHome m
  , MonadThrow m
  , MonadIO m
  , MonadBaseControl IO m
  ) => Snapshot -> m ()
setup snapshot0 = do
  snapshot <- if isSeries snapshot0
    then do
      ltsSnapshots <- refreshLtsSnapshots
      case HashMap.lookup snapshot0 ltsSnapshots of
        Just snapshot -> return snapshot
        Nothing -> return snapshot0 -- TODO: error?
    else return snapshot0
  env <- downloadSnapshotData snapshot

  forM_ env $ \d@Download{..} -> do
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
  case downloadName of
    "ghc" -> return () -- TODO: install
    "cabal" -> liftIO $ do
      createTree versionedDir
      rename (dir </> "cabal") (versionedDir </> "cabal")
    _ -> return () -- TODO: error?

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
