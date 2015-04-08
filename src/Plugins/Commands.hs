-- | Using Plugins with SimpleOptions
module Plugins.Commands
  ( commandsFromPlugins
  , toCommand
  ) where

import Data.Text (Text, unpack)
import Data.Foldable (foldMap)
import Plugins
import SimpleOptions

-- | Generate the "commands" argument to simpleOptions
-- based on available plugins.
commandsFromPlugins :: Plugins -> Mod CommandFields Text
commandsFromPlugins plugins = foldMap toCommand (listPlugins plugins)

-- | Convert a single plugin into a command.
toCommand :: Plugin -> Mod CommandFields Text
toCommand plugin = simpleCommand
  (unpack $ pluginName plugin)
  (unpack $ pluginSummary plugin)
  id
  (pure $ pluginName plugin)
