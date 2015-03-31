{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import Control.Applicative
import Control.Exception (Exception, throwIO)
import Data.Text (Text, pack)
import Data.Typeable (Typeable)

import Stackage.CLI

import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess, createProcess, waitForProcess)

-- TODO: move some of this to stackage-cli
data StackagePluginException
  = StackagePluginUnavailable Text
  | StackagePluginExitFailure Int
  deriving (Show, Typeable)
instance Exception StackagePluginException

execProcess :: CreateProcess -> IO ()
execProcess cp = do
  (_, _, _, p) <- createProcess cp
  e <- waitForProcess p
  case e of
    ExitFailure i -> throwIO $ StackagePluginExitFailure i
    ExitSuccess -> return ()

runStackagePlugin :: Text -> [Text] -> IO ()
runStackagePlugin name args = do
  mm <- lookupSubmoduleOf stackageModule name
  case mm of
    Just m -> execProcess $ procModule m args
    Nothing -> throwIO $ StackagePluginUnavailable name

version :: String
version = "0.1"

summary :: String
summary = "Upgrade stackage stuff"

-- TODO: no-op if at latest already
-- TODO: use simpleOpts
main = do
  args <- getArgs
  case args of
    ["--version"] -> putStrLn version
    ["--summary"] -> putStrLn summary
    _ -> do
      runStackagePlugin "purge" []
      runStackagePlugin "init" $ map pack args
