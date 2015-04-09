{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Filesystem
import Control.Applicative
import Control.Monad
import Stackage.CLI
import Options.Applicative (Parser)
import Options.Applicative.Builder (strArgument, metavar, value)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable (Typeable)
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header (hUserAgent)
import qualified Data.ByteString.Lazy as LBS
import System.Exit (exitFailure)
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import Control.Exception

type Snapshot = String

data InitException
  = InvalidSnapshot
  | SnapshotNotFound
  | UnexpectedHttpException HttpException
  | CabalConfigExists
  deriving (Show, Typeable)
instance Exception InitException

version :: String
version = "0.1"

header :: String
header = "Initializes cabal.config"

progDesc :: String
progDesc = header

userAgent :: Text
userAgent = "stackage-init/" <> T.pack version

snapshotParser :: Parser Snapshot
snapshotParser = strArgument mods where
  mods = (metavar "SNAPSHOT" <> value "lts")

toUrl :: Snapshot -> String
toUrl t = "http://stackage.org/" <> t <> "/cabal.config"

snapshotReq :: Snapshot -> IO Request
snapshotReq snapshot = case parseUrl (toUrl snapshot) of
  Left _ -> throwIO $ InvalidSnapshot
  Right req -> return req
    { requestHeaders = [(hUserAgent, T.encodeUtf8 userAgent)]
    }

downloadSnapshot :: Snapshot -> IO LBS.ByteString
downloadSnapshot snapshot = withManager defaultManagerSettings $ \manager -> do
  let getResponseLbs req = do
        response <- httpLbs req manager
        return $ responseBody response
  let handle404 firstTry (StatusCodeException s _ _)
        | statusCode s == 404 = if firstTry
          then do
            req <- snapshotReq $ "snapshot/" <> snapshot
            getResponseLbs req `catch` handle404 False
          else do
            throwIO $ SnapshotNotFound
      handle404 _ e = throwIO $ UnexpectedHttpException e
  req <- snapshotReq snapshot
  getResponseLbs req `catch` handle404 True

initSnapshot :: Snapshot -> IO ()
initSnapshot snapshot = do
  configExists <- isFile "cabal.config"
  when configExists $ throwIO $ CabalConfigExists
  downloadSnapshot snapshot >>= LBS.writeFile "cabal.config"

handleInitExceptions :: Snapshot -> InitException -> IO ()
handleInitExceptions snapshot e = hPutStrLn stderr (err e) >> exitFailure where
  err InvalidSnapshot
    = "Invalid snapshot: " <> snapshot
  err SnapshotNotFound
    = "Snapshot not found: " <> snapshot
  err CabalConfigExists
    = "Warning: Cabal config already exists.\n"
   <> "No action taken."
  err (UnexpectedHttpException e)
    = "Unexpected http exception:\n"
   <> show e

main = do
  (snapshot, ()) <- simpleOptions
    version
    header
    progDesc
    snapshotParser -- global parser
    (Left ())    -- subcommands
  initSnapshot snapshot `catch` handleInitExceptions snapshot
