{-# LANGUAGE OverloadedStrings #-}

module Stackage.CLI
  ( Plugin
  , pluginPrefix
  , pluginName
  , pluginSummary
  , pluginProc

  , Plugins
  , findPlugins
  , listPlugins
  , lookupPlugin
  , callPlugin
  , runStackagePlugin

  , PluginException (..)

  , simpleCommand
  , simpleOptions
  , commandsFromPlugins
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
