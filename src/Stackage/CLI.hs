{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Stackage.CLI
  ( stackageModule

  -- * Discovering and calling plugins (modules)
  , runStackagePlugin
  , StackagePluginException (..)

  , Module
  , ModuleName
  , discoverSubmodulesOf
  , lookupSubmoduleOf
  , procModule
  , readProcModule
  , execModule

  -- * Defining a plugin
  , submoduleOf
  , Subcommand (..)
  , subcommandsOf
  , simpleCommand
  , simpleOptions
  , getSubcommands
  ) where

import Module
import SimpleOptions

import Control.Exception (Exception, throwIO)
import Data.Text (Text)
import Data.Typeable (Typeable)
import System.Exit (ExitCode (..))
import System.Process (CreateProcess, createProcess, waitForProcess)


-- | A reference to the stackage executable.
--
-- * You can use this to run the stackage executable via
--   `callModule stackageModule`.
-- * You can dynamically discover available plugins via
--   `discoverSubmodulesOf stackageModule`.
-- * You can verify the existence of a particular stackage plugin via
--   `lookupSubmoduleOf stackageModule`.
stackageModule :: Module
stackageModule = theModuleNamed "stackage"

stackageSubmodule :: ModuleName -> Module
stackageSubmodule = submoduleOf stackageModule

-- | Things that can go wrong when running a plugin.
data StackagePluginException
  = StackagePluginUnavailable Text
  | StackagePluginExitFailure Text Int
  deriving (Show, Typeable)
instance Exception StackagePluginException

execModule :: Module -> [Text] -> IO ()
execModule m args = do
  (_, _, _, p) <- createProcess $ procModule m args
  e <- waitForProcess p
  case e of
    ExitFailure i -> throwIO $ StackagePluginExitFailure (moduleName m) i
    ExitSuccess -> return ()

-- | Runs a stackage plugin with the given arguments.
--
-- Sample usage:
-- > main = runStackagePlugin "init" ["nightly"]
runStackagePlugin
  :: Text -- | plugin name
  -> [Text] -- | command-line arguments for the plugin
  -> IO ()
runStackagePlugin name args = do
  mm <- lookupSubmoduleOf stackageModule name
  case mm of
    Just m -> execModule m args
    Nothing -> throwIO $ StackagePluginUnavailable name
