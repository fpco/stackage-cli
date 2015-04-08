{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Exception (Exception, bracket_, catch, throwIO)
import Control.Monad (when)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable (Typeable)
import Filesystem.Path.CurrentOS as Path
import Filesystem
import Options.Applicative hiding (header, progDesc)
import Stackage.CLI
import System.Environment (getEnv)
import System.Exit
import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
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

data SandboxException
  = NoHomeEnvironmentVariable
  | AlreadyAtSnapshot
  | PackageDbLocMismatch Text Text
  | MissingConfig Bool Bool
  | SnapshotMismatch Text Text
  | NonStackageCabalConfig
  | EmptyCabalConfig
  | DecodePathFail Text
  | ConfigAlreadyExists
  | InvalidSnapshot Snapshot
  deriving (Show, Typeable)
instance Exception SandboxException

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
  ]

toText' :: Path.FilePath -> IO Text
toText' p = case toText p of
  Left e -> throwIO $ DecodePathFail e
  Right t -> return t

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
      Nothing -> throwIO NonStackageCabalConfig
    _ -> throwIO EmptyCabalConfig

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
        throwIO $ PackageDbLocMismatch snapshotDirText packageDb
    else do
      throwIO $ MissingConfig cabalConfigExists cabalSandboxConfigExists

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
    throwIO ConfigAlreadyExists

  cabalConfigExists <- isFile "cabal.config"
  when (not cabalConfigExists) $ do
    runStackagePlugin "init" (mSnapshotToArgs msnapshot)

  configSnapshot <- parseConfigSnapshot
  snapshot <- case msnapshot of
    Just s | snapshotEq s configSnapshot -> return configSnapshot
    Just s -> throwIO $ SnapshotMismatch s configSnapshot
    Nothing -> return configSnapshot

  T.putStrLn $ "Initializing at snapshot: " <> snapshot

  dir <- getSnapshotDir snapshot
  createTree dir
  cabalSandboxInit dir
  sandboxVerify

getHome :: IO Text
getHome = T.pack <$> getEnv "HOME" `catch` dne where
  dne e
    | isDoesNotExistError e = throwIO NoHomeEnvironmentVariable
    | otherwise = throwIO e

getSnapshotDir :: Snapshot -> IO Path.FilePath
getSnapshotDir snapshot = do
  home <- getHome
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

snapshotSanityCheck :: Snapshot -> IO ()
snapshotSanityCheck snapshot =
  if any (`T.isInfixOf` snapshot) ["..", "/"]
    then throwIO $ InvalidSnapshot snapshot
    else return ()

sandboxDeleteSnapshot :: Snapshot -> IO ()
sandboxDeleteSnapshot snapshot = do
  snapshotSanityCheck snapshot
  getSnapshotDir snapshot >>= removeTree

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
        T.putStrLn $ "Already at snapshot: " <> snapshot
        -- TODO: more verification
        throwIO AlreadyAtSnapshot
      else return ()

  sandboxDelete
  sandboxInit mSnapshot

handleSandboxExceptions :: SandboxException -> IO ()
handleSandboxExceptions NoHomeEnvironmentVariable = do
  hPutStrLn stderr "Couldn't find the HOME environment variable"
  exitFailure
handleSandboxExceptions AlreadyAtSnapshot = exitSuccess
handleSandboxExceptions (PackageDbLocMismatch dir db) = do
  hPutStrLn stderr "verify: package db isn't in the expected location:"
  T.hPutStrLn stderr $ "dir: " <> dir
  T.hPutStrLn stderr $ "db: " <> db
  exitFailure
handleSandboxExceptions (MissingConfig cabalConfigExists cabalSandboxConfigExists) = do
  when (not cabalConfigExists) $
    hPutStrLn stderr "verify: cabal.config not present"
  when (not cabalSandboxConfigExists) $
    hPutStrLn stderr "verify: cabal.sandbox.config not present"
  exitFailure
handleSandboxExceptions (SnapshotMismatch s configSnapshot) = do
  T.hPutStrLn stderr
     $ "Warning: given snapshot [" <> s <> "] "
    <> "doesn't match cabal.config snapshot [" <> configSnapshot <> "]"
  hPutStrLn stderr "No action taken"
  exitFailure
handleSandboxExceptions ConfigAlreadyExists = do
  hPutStrLn stderr $ "Warning: cabal.sandbox.config already exists"
  hPutStrLn stderr $ "No action taken"
  exitFailure
handleSandboxExceptions NonStackageCabalConfig = do
  hPutStrLn stderr $ "cabal.config doesn't look like it's from stackage"
  exitFailure
handleSandboxExceptions EmptyCabalConfig = do
  hPutStrLn stderr $ "No contents found in cabal.config"
  exitFailure
handleSandboxExceptions (DecodePathFail e) = do
  hPutStrLn stderr $ "Unexpected failure decoding path:"
  T.hPutStrLn stderr e
  exitFailure
handleSandboxExceptions (InvalidSnapshot snapshot) = do
  T.hPutStrLn stderr $ "Invalid snapshot: " <> snapshot
  exitFailure


handlePluginExceptions :: PluginException -> IO ()
handlePluginExceptions (PluginNotFound _ p) = do
  hPutStrLn stderr $ "stackage-sandbox: requires plugin stackage " <> T.unpack p
  exitFailure
handlePluginExceptions (PluginExitFailure _ i) = do
  exitWith (ExitFailure i)

main :: IO ()
main = do
  ((), action) <- simpleOptions
    version
    header
    progDesc
    (pure ())
    (Right subcommands)
  let go = case action of
        Init mSnapshot -> sandboxInit mSnapshot
        PackageDb -> printPackageDb
        List mPackage -> ghcPkgList mPackage
        Unregister package -> ghcPkgUnregister package
        Delete mSnapshot -> case mSnapshot of
          Just snapshot -> sandboxDeleteSnapshot snapshot
          Nothing -> sandboxDelete
        Upgrade mSnapshot -> sandboxUpgrade mSnapshot
  go `catch` handleSandboxExceptions
     `catch` handlePluginExceptions
