{-# LANGUAGE OverloadedStrings #-}

module Stackage.CLI
  ( stackageModule

  -- * Discovering and calling plugins (modules)
  , Module
  , ModuleName
  , discoverSubmodulesOf
  , lookupSubmoduleOf
  , procModule
  , readProcModule

  -- * Defining a plugin
  , submoduleOf
  , Subcommand (..)
  , subcommandsOf
  , simpleCommand
  , simpleOptions
  ) where

import Module
import SimpleOptions

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
