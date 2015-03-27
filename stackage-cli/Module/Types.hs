module Module.Types
  ( ModuleName
  , Module (..)
  , submoduleOf
  , theModuleNamed
  ) where

import Data.Text (Text)

type ModuleName = Text
data Module = Module
  { moduleName :: ModuleName
  , moduleContext :: Maybe Module
  } deriving (Show, Eq)

submoduleOf :: Module -> ModuleName -> Module
submoduleOf context name = Module
  { moduleName = name
  , moduleContext = Just context
  }

theModuleNamed :: ModuleName -> Module
theModuleNamed name = Module
  { moduleName = name
  , moduleContext = Nothing
  }
