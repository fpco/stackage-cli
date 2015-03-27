{-# LANGUAGE OverloadedStrings #-}

module Stackage.CLI
  ( stackageModule
  , stackageSubmodule

  -- from Module
  , Module
  , discoverSubmodulesOf
  , callModule
  , Subcommand (..)
  , subcommandsFor

  -- from SimpleOptions
  , simpleCommand
  , simpleOptions
  ) where

import Module
import SimpleOptions

stackageModule :: Module
stackageModule = theModuleNamed "stackage"

stackageSubmodule :: ModuleName -> Module
stackageSubmodule = submoduleOf stackageModule
