module Plugins.Commands
  ( commandsFromPlugins
  ) where

import Data.Text (Text, unpack)
import Data.Foldable (foldMap)
import Plugins
import SimpleOptions

commandsFromPlugins :: Plugins -> Either Text (Mod CommandFields Text)
commandsFromPlugins plugins = Right $ foldMap toCommand (listPlugins plugins) where
  toCommand :: Plugin -> Mod CommandFields Text
  toCommand plugin = simpleCommand
    (unpack $ pluginName plugin)
    (unpack $ pluginSummary plugin)
    id
    (pure $ pluginName plugin)
