{-# LANGUAGE OverloadedStrings #-}

-- | Functions for creating and calling Stackage plugins.
module Stackage.CLI
  ( -- * Discovering and calling plugins
    runStackagePlugin
  , Plugins
  , findPlugins
  , callPlugin
  , PluginException (..)

    -- * Creating your own plugin
  , simpleOptions
  , addCommand
  , commandsFromPlugins

    -- * Finer-grained inspection of plugins
  , listPlugins
  , lookupPlugin
  , Plugin
  , pluginPrefix
  , pluginName
  , pluginSummary
  , pluginProc
  ) where

import Data.Text (Text)
import Plugins
import Plugins.Commands
import SimpleOptions

-- | Runs a stackage plugin. Handy for dynamic one-off runs,
-- but if you'll be running multiple plugins, it is recommended
-- that you use @findPlugins "stackage"@ so that the plugin search
-- is performed only once.
runStackagePlugin :: Text -> [String] -> IO ()
runStackagePlugin name args = do
  stackage <- findPlugins "stackage"
  callPlugin stackage name args
