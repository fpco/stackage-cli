{-# LANGUAGE OverloadedStrings #-}
module Stackage.CLI.Upgrade
  ( stackageUpgradeModule
  , simpleOptions
  ) where

import Stackage.CLI

stackageUpgradeModule :: Module
stackageUpgradeModule = submoduleOf stackageModule "upgrade"
