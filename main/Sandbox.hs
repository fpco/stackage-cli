{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (bracket_)
import Control.Monad (when)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Filesystem.Path.CurrentOS as Path
import Filesystem
import Options.Applicative hiding (header, progDesc)
import Stackage.CLI
import System.Environment (getEnv)
import System.Exit (exitFailure, exitSuccess)
import System.Process (callProcess, readProcess)

type Snapshot = Text
type Package = Text
data Action
  = Init (Maybe Snapshot)
  | PackageDb
  | List (Maybe Package)
  | Unregister Package
  | Delete (Maybe Snapshot)
  | Upgrade (Maybe Snapshot)
  -- TODO: other commands

mSnapshotToArgs :: Maybe Snapshot -> [String]
mSnapshotToArgs = fmap T.unpack . maybeToList

version :: String
version = "0.1"

header :: String
header = "Initializes cabal.sandbox.config"

progDesc :: String
progDesc = header

snapshotParser :: Parser Snapshot
snapshotParser = T.pack <$> strArgument mods where
  mods = metavar "SNAPSHOT"

packageParser :: Parser Package
packageParser = T.pack <$> strArgument mods where
  mods = metavar "PACKAGE"

packageDbDesc :: String
packageDbDesc = "Prints '--package-db $db', or prints nothing"

listDesc :: String
listDesc = "Calls `ghc-pkg list` with the sandbox package-db"

unregisterDesc :: String
unregisterDesc = "Calls `ghc-pkg unregister PACKAGE with the sandbox package-db"

deleteDesc :: String
deleteDesc = "Deletes cabal.config and cabal.sandbox.config. "
  <> "If provided with a SNAPSHOT, instead deletes that snapshot's folder."

upgradeDesc :: String
upgradeDesc = "Upgrade to the given SNAPSHOT. Defaults to the latest LTS."

subcommands = mconcat
  [ simpleCommand "init" "Init" Init (optional snapshotParser)
  , simpleCommand "package-db" packageDbDesc (const PackageDb) (pure ())
  , simpleCommand "list" listDesc List (optional packageParser)
  , simpleCommand "unregister" unregisterDesc Unregister packageParser
  , simpleCommand "delete" deleteDesc Delete (optional snapshotParser)
  , simpleCommand "upgrade" upgradeDesc Upgrade (optional snapshotParser)
  -- TODO: other commands
  ]

toText' :: Path.FilePath -> IO Text
toText' p = case toText p of
  Left e -> fail $ "Couldn't decode path: " <> T.unpack e
  Right t -> return t

-- TODO: handle errors gracefully
cabalSandboxInit :: Path.FilePath -> IO ()
cabalSandboxInit dir = do
  dirText <- toText' dir
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

-- TODO: verify things about the packages in the sandbox
sandboxVerify :: IO ()
sandboxVerify = do
  cabalConfigExists <- isFile "cabal.config"
  cabalSandboxConfigExists <- isFile "cabal.sandbox.config"
  if (cabalConfigExists && cabalSandboxConfigExists)
    then do
      packageDb <- getPackageDb
      snapshot <- parseConfigSnapshot
      snapshotDir <- getSnapshotDir snapshot
      snapshotDirText <- toText' snapshotDir
      when (not $ T.isPrefixOf snapshotDirText packageDb) $ do
        putStrLn "verify: package db isn't in the expected location:"
        T.putStrLn $ "dir: " <> snapshotDirText
        T.putStrLn $ "db: " <> packageDb
        exitFailure
    else do
      when (not cabalConfigExists) $
        putStrLn "verify: cabal.config not present"
      when (not cabalSandboxConfigExists) $
        putStrLn "verify: cabal.sandbox.config not present"
      exitFailure

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
    runStackagePlugin "init" (mSnapshotToArgs msnapshot)
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

  dir <- getSnapshotDir snapshot
  createTree dir
  cabalSandboxInit dir
  sandboxVerify

getSnapshotDir :: Snapshot -> IO Path.FilePath
getSnapshotDir snapshot = do
  -- TODO: handle env exceptions
  home <- T.pack <$> getEnv "HOME"
  ghcVersion <- getGhcVersion
  let dir = Path.fromText home </> ".stackage" </> "sandboxes"
        </> Path.fromText ghcVersion </> Path.fromText snapshot
  return dir

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
  Just packageDb -> return packageDb
  Nothing -> fail "Couldn't find sandbox package-db"

getPackageDbArg :: IO Text
getPackageDbArg = parsePackageDb >>= \mdb -> case mdb of
  Just packageDb -> return $ "--package-db=" <> packageDb
  Nothing -> return ""


printPackageDb :: IO ()
printPackageDb = getPackageDbArg >>= T.putStrLn

ghcPkgList :: Maybe Package -> IO ()
ghcPkgList mPackage = do
  packageDb <- getPackageDbArg
  let args
        = ["list"]
       <> case mPackage of
            Nothing -> []
            Just package -> [T.unpack package]
       <> [T.unpack packageDb]
  callProcess "ghc-pkg" args

ghcPkgUnregister :: Package -> IO ()
ghcPkgUnregister package = do
  packageDb <- getPackageDbArg
  callProcess "ghc-pkg" ["unregister", T.unpack package, T.unpack packageDb]

sandboxDelete :: IO ()
sandboxDelete = do
  cabalConfigExists <- isFile "cabal.config"
  when cabalConfigExists $ do
    removeFile "cabal.config"
  cabalSandboxConfigExists <- isFile "cabal.sandbox.config"
  when cabalSandboxConfigExists $ do
    removeFile "cabal.sandbox.config"

sandboxDeleteSnapshot :: Snapshot -> IO ()
sandboxDeleteSnapshot snapshot
  = getSnapshotDir snapshot >>= removeTree

-- Find the canonical name for a snapshot by looking it up on stackage.org.
-- This can change over time. e.g. "lts" used to mean lts-1.0.
downloadSnapshot :: Maybe Snapshot -> IO Snapshot
downloadSnapshot mSnapshot = do
  workingDir <- getWorkingDirectory
  let tempDir = ".stackage-sandbox-tmp"
      enterTempDir = do
        createDirectory False tempDir
        setWorkingDirectory tempDir
      exitTempDir = do
        setWorkingDirectory workingDir
        removeTree tempDir
  bracket_ enterTempDir exitTempDir $ do
    runStackagePlugin "init" (mSnapshotToArgs mSnapshot)
    parseConfigSnapshot


sandboxUpgrade :: Maybe Snapshot -> IO ()
sandboxUpgrade mSnapshot = do
  cabalConfigExists <- isFile "cabal.config"
  cabalSandboxConfigExists <- isFile "cabal.sandbox.config"

  mConfigSnapshot <- if cabalConfigExists
    then Just <$> parseConfigSnapshot
    else return Nothing

  snapshot <- downloadSnapshot mSnapshot

  when (Just snapshot == mConfigSnapshot && cabalSandboxConfigExists) $ do
    packageDb <- getPackageDb
    snapshotDir <- getSnapshotDir snapshot
    snapshotDirText <- toText' snapshotDir
    if T.isPrefixOf snapshotDirText packageDb
      then do
        -- TODO: more verification
        T.putStrLn $ "Already at snapshot: " <> snapshot
        exitSuccess
      else return ()

  sandboxDelete
  sandboxInit mSnapshot

main = do
  ((), action) <- simpleOptions
    version
    header
    progDesc
    (pure ())
    (Right subcommands)
  case action of
    Init mSnapshot -> sandboxInit mSnapshot
    PackageDb -> printPackageDb
    List mPackage -> ghcPkgList mPackage
    Unregister package -> ghcPkgUnregister package
    Delete mSnapshot -> case mSnapshot of
      Just snapshot -> sandboxDeleteSnapshot snapshot
      Nothing -> sandboxDelete
    Upgrade mSnapshot -> sandboxUpgrade mSnapshot
