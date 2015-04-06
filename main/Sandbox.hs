{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (when)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Filesystem.Path.CurrentOS as Path
import Filesystem (isFile, createTree)
import Options.Applicative hiding (header, progDesc)
import Stackage.CLI
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.Process (callProcess, readProcess)

type Snapshot = Text
data Action
  = Init (Maybe Snapshot)
  | PackageDb
  -- TODO: other commands

version :: String
version = "0.1"

header :: String
header = "Initializes cabal.sandbox.config"

progDesc :: String
progDesc = header

snapshotParser :: Parser Snapshot
snapshotParser = T.pack <$> strArgument mods where
  mods = metavar "SNAPSHOT"

packageDbDesc :: String
packageDbDesc = "Prints '--package-db $db', or prints nothing"

subcommands = mconcat
  [ simpleCommand "init" "Init" Init (optional snapshotParser)
  , simpleCommand "package-db" packageDbDesc (const PackageDb) (pure ())
  -- TODO: other commands
  ]

-- TODO: handle errors gracefully
cabalSandboxInit :: Path.FilePath -> IO ()
cabalSandboxInit dir = do
  dirText <- case toText dir of
    Left e -> fail $ "Couldn't decode path: " <> T.unpack e
    Right e -> return e
  let args =
        [ "sandbox"
        , "init"
        , "--sandbox"
        , T.unpack dirText
        ]
  callProcess "cabal" args

-- precondition: cabal.config exists
-- TODO: better errors?
parseConfigSnapshot :: IO Snapshot
parseConfigSnapshot = do
  ls <- T.lines <$> T.readFile "cabal.config"
  let p = "-- Stackage snapshot from: http://www.stackage.org/snapshot/"
  case ls of
    (l:_) -> case T.stripPrefix p l of
      Just snapshot -> return snapshot
      Nothing -> fail "cabal.config doesn't look like it's from stackage"
    _ -> fail "No contents found in cabal.config"

-- TODO: a stricter check?
snapshotEq :: Snapshot -> Snapshot -> Bool
snapshotEq short full = T.isPrefixOf (T.replace "/" "-" short) full

sandboxVerify :: IO ()
sandboxVerify = putStrLn "TODO: stackage sandbox verify"

-- TODO: handle errors gracefully
getGhcVersion :: IO Text
getGhcVersion = do
  output <- readProcess "ghc" ["--version"] ""
  case words output of
    [] -> fail "Couldn't determine ghc version"
    ws -> return $ "ghc-" <> T.pack (last ws)

sandboxInit :: Maybe Snapshot -> IO ()
sandboxInit msnapshot = do
  cabalSandboxConfigExists <- isFile "cabal.sandbox.config"
  when cabalSandboxConfigExists $ do
    putStrLn $ "Warning: cabal.sandbox.config already exists"
    putStrLn $ "No action taken"
    exitFailure

  cabalConfigExists <- isFile "cabal.config"
  when (not cabalConfigExists) $ do
    runStackagePlugin "init" (maybeToList msnapshot)
    -- TODO: catch plugin exceptions

  configSnapshot <- parseConfigSnapshot
  snapshot <- case msnapshot of
    Just s | snapshotEq s configSnapshot -> return configSnapshot
    Just s -> do
      T.putStrLn
         $ "Warning: given snapshot [" <> s <> "] "
        <> "doesn't match cabal.config snapshot [" <> configSnapshot <> "]"
      putStrLn "No action taken"
      exitFailure
    Nothing -> return configSnapshot

  T.putStrLn $ "Initializing at snapshot: " <> snapshot

  -- TODO: handle env exceptions
  home <- T.pack <$> getEnv "HOME"
  ghcVersion <- getGhcVersion
  let dir = Path.fromText home </> ".stackage" </> "sandboxes"
        </> Path.fromText ghcVersion </> Path.fromText snapshot
  createTree dir
  cabalSandboxInit dir
  sandboxVerify

-- copied from Purge.hs, tweaked
-- TODO: remove duplication
parsePackageDb :: IO (Maybe Text)
parsePackageDb = do
  cabalSandboxConfigExists <- isFile "cabal.sandbox.config"
  if cabalSandboxConfigExists
    then do
      t <- T.readFile "cabal.sandbox.config"
      let packageDbLine = T.stripPrefix "package-db: "
      return $ listToMaybe $ mapMaybe packageDbLine $ T.lines t
    else
      return Nothing


getPackageDb :: IO Text
getPackageDb = parsePackageDb >>= \mdb -> case mdb of
  Just packageDb -> return $ "--package-db=" <> packageDb
  Nothing -> return ""

printPackageDb :: IO ()
printPackageDb = getPackageDb >>= T.putStrLn

main = do
  ((), action) <- simpleOptions
    version
    header
    progDesc
    (pure ())
    (Right subcommands)
  case action of
    Init target -> sandboxInit target
    PackageDb -> printPackageDb
    -- TODO: other commands
