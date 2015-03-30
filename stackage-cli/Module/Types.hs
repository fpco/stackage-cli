module Module.Types
  ( ModuleName
  , Module (..)
  , submoduleOf
  , theModuleNamed
  ) where

import Data.Text (Text)

type ModuleName = Text

-- | A module is a dynamic reference to an executable on your path.
-- Some modules are plugins to other modules.
data Module = Module
  { moduleName :: ModuleName
  , moduleContext :: Maybe Module
  } deriving (Show, Eq)

-- | Name yourself as a plugin.
--
-- Sample usage:
--
-- > module Stackage.Init.CLI (stackageInitModule) where
-- > stackageInitModule = submoduleOf stackageModule "init"
--
-- You can also use this to assume the presence of a plugin with a given name,
-- but it's safer to verify a plugin's existence via
-- `lookupSubmodule stackageModule "pluginName"`,
-- or by importing the plugin's `Module` value.
submoduleOf :: Module -> ModuleName -> Module
submoduleOf context name = Module
  { moduleName = name
  , moduleContext = Just context
  }

-- | Name yourself as an executable.
--
-- Sample usage:
--
-- > module Stackage.CLI (stackageModule) where
-- > stackageModule = theModuleNamed "stackage"
theModuleNamed :: ModuleName -> Module
theModuleNamed name = Module
  { moduleName = name
  , moduleContext = Nothing
  }
