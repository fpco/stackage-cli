{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Module.IO
  ( procModule
  , readProcModule
  , discoverSubmodulesOf
  , lookupSubmoduleOf
  , moduleProcessName
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Data.Conduit
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Conduit.List as CL
import Data.Conduit.Lift (evalStateC)
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Text (Text, pack, unpack)
import Data.Monoid
import System.Directory
import System.Process (CreateProcess, proc, readProcess)
import System.FilePath ((</>))
import System.Environment (getEnv)

import Module.Types


moduleProcessName :: Module -> Text
moduleProcessName m = contextName <> moduleName m where
  contextName = case moduleContext m of
    Nothing -> mempty
    Just parent -> moduleProcessName parent <> "-"

moduleProcessFilePath :: Module -> FilePath
moduleProcessFilePath = unpack . moduleProcessName

-- | Runs a module with the given arguments.
--
-- * The process is given an empty stdin.
-- * The process's stdout is captured and returned as Text when it terminates.
-- * The process's stderr is passed through.
readProcModule :: Module -> [Text] -> IO Text
readProcModule m args =
    pack <$> readProcess (moduleProcessFilePath m) (map unpack args) ""

-- | Describes how to create a process out of a module and arguments.
-- You may use "Data.Process" and "Data.Conduit.Process" and "Data.Process"
-- to manage the process's stdin/stdout/stderr in various ways.
procModule :: Module -> [Text] -> CreateProcess
procModule m args = proc (moduleProcessFilePath m) (map unpack args)

-- | Search for a particular plugin on the PATH.
lookupSubmoduleOf :: Module -> ModuleName -> IO (Maybe Module)
lookupSubmoduleOf m name
  = discoverSubmodulesOf m
 $$ CL.filter ((== name) . moduleName)
 =$ CL.head

-- | Find the plugins for a given module by inspecting everything on the PATH.
discoverSubmodulesOf :: Module -> Producer IO Module
discoverSubmodulesOf m
  = getPathDirs
 $= clNub -- unique dirs on path
 $= awaitForever (executablesPrefixed $ moduleProcessFilePath m <> "-")
 $= clNub -- unique executables
 $= CL.map (submoduleOf m . pack)

executablesPrefixed :: (MonadIO m) => FilePath -> FilePath -> Producer m FilePath
executablesPrefixed prefix dir
  = pathToContents dir
 $= CL.mapMaybe (L.stripPrefix $ prefix)
 $= clFilterM (fileExistsIn dir prefix)
 $= clFilterM (isExecutableIn dir prefix)
 
getPathDirs :: (MonadIO m) => Producer m FilePath
getPathDirs = do
  path <- liftIO $ getEnv "PATH"
  CL.sourceList $ splitOn ":" path

pathToContents :: (MonadIO m) => FilePath -> Producer m FilePath
pathToContents dir = do
  exists <- liftIO $ doesDirectoryExist dir
  when exists $ do
    contents <- liftIO $ getDirectoryContents dir
    CL.sourceList contents

fileExistsIn :: (MonadIO m) => FilePath -> FilePath -> FilePath -> m Bool
fileExistsIn dir prefix file = liftIO $ doesFileExist $ dpFile dir prefix file

dpFile :: FilePath -> FilePath -> FilePath -> FilePath
dpFile dir prefix file = dir </> (prefix <> file)

isExecutableIn :: (MonadIO m) => FilePath -> FilePath -> FilePath -> m Bool
isExecutableIn dir prefix file = liftIO $ do
  perms <- getPermissions $ dpFile dir prefix file
  return (executable perms)

clFilterM :: Monad m => (a -> m Bool) -> Conduit a m a
clFilterM pred = awaitForever $ \a -> do
  predPassed <- lift $ pred a
  when predPassed $ yield a

clNub :: (Monad m, Eq a, Hashable a)
  => Conduit a m a
clNub = evalStateC HashSet.empty clNubState

clNubState :: (Monad m, Eq a, Hashable a)
  => Conduit a (StateT (HashSet a) m) a
clNubState = awaitForever $ \a -> do
  seen <- lift get
  unless (HashSet.member a seen) $ do
    lift $ put $ HashSet.insert a seen
    yield a
