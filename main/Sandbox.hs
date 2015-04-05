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

type Snapshot = Text
data Action
  = Init (Maybe Snapshot)
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

subcommands = mconcat
  [ simpleCommand "init" "Init" Init (optional snapshotParser)
  -- TODO: other commands
  ]

cabalSandboxInit :: Path.FilePath -> IO ()
cabalSandboxInit dir = putStrLn "TODO: cabal sandbox init"

-- TODO
parseConfigSnapshot :: IO Snapshot
parseConfigSnapshot = return "lts-2.0"

-- TODO: be more lenient about this
snapshotEq :: Snapshot -> Snapshot -> Bool
snapshotEq = (==)

sandboxVerify :: IO ()
sandboxVerify = putStrLn "TODO: stackage sandbox verify"

-- TODO
getGhcVersion :: IO Text
getGhcVersion = return "ghc-7.8.4"

sandboxInit :: Maybe Snapshot -> IO ()
sandboxInit msnapshot = do
  cabalSandboxConfigExists <- isFile "cabal.sandbox.config"
  when cabalSandboxConfigExists $ do
    putStrLn $ "Warning: cabal.sandbox.config already exists"
    putStrLn $ "No action taken"
    exitFailure

  runStackagePlugin "init" (maybeToList msnapshot)
  -- TODO: catch plugin exceptions

  configSnapshot <- parseConfigSnapshot
  snapshot <- case msnapshot of
    Just s | snapshotEq s configSnapshot -> return configSnapshot
    Just s -> do
      T.putStrLn
         $ "Warning: given snapshot [" <> s <> "]"
        <> "doesn't match cabal.config snapshot [" <> configSnapshot <> "]"
      exitFailure
    Nothing -> return configSnapshot

  -- TODO: handle env exceptions
  home <- T.pack <$> getEnv "HOME"
  ghcVersion <- getGhcVersion
  let dir = Path.fromText home </> ".stackage" </> "sandboxes"
        </> Path.fromText ghcVersion </> Path.fromText snapshot
  createTree dir
  cabalSandboxInit dir
  sandboxVerify

main = do
  ((), action) <- simpleOptions
    version
    header
    progDesc
    (pure ())
    (Right subcommands)
  case action of
    Init target -> sandboxInit target
    -- TODO: other commands
