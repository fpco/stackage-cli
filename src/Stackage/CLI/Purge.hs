{-# LANGUAGE OverloadedStrings #-}
module Stackage.CLI.Purge
  ( stackagePurgeModule
  , simpleOptions
  ) where

import Stackage.CLI

stackagePurgeModule :: Module
stackagePurgeModule = submoduleOf stackageModule "purge"
