{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Module.IO
  ( callModule
  , discoverSubmodulesOf
  , moduleProcessName
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, put)
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Conduit.Lift (evalStateC)
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Text (Text, pack, unpack)
import Data.Monoid
import System.Directory
import System.Process (readProcess)
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

callModule :: Module -> [Text] -> IO Text
callModule m args =
  pack <$> readProcess (moduleProcessFilePath m) (map unpack args) ""

discoverSubmodulesOf :: Module -> Producer IO Module
discoverSubmodulesOf m
  = evalStateC []
  $ getPathDirs
 $= awaitForever (executablesPrefixed $ moduleProcessFilePath m <> "-")
 $= clNub
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

clNub :: (Monad m, Eq a) => Conduit a (StateT [a] m) a
clNub = awaitForever $ \a -> do
  seen <- lift get
  unless (a `elem` seen) $ do
    lift $ put (a:seen)
    yield a
