-- | Using Plugins with SimpleOptions
module Plugins.Commands
  ( commandsFromPlugins
  , toCommand
  ) where

import Control.Monad.Trans.Either (EitherT)
import Control.Monad.Trans.Writer (Writer)
import Data.Text (Text, unpack)
import Data.Foldable (foldMap)
import Plugins
import Options.Applicative.Simple

-- | Generate the "commands" argument to simpleOptions
-- based on available plugins.
commandsFromPlugins :: Plugins -> EitherT Text (Writer (Mod CommandFields Text)) ()
commandsFromPlugins plugins = mapM_ toCommand (listPlugins plugins)

-- | Convert a single plugin into a command.
toCommand :: Plugin -> EitherT Text (Writer (Mod CommandFields Text)) ()
toCommand plugin = addCommand
  (unpack $ pluginName plugin)
  (unpack $ pluginSummary plugin)
  id
  (pure $ pluginName plugin)
