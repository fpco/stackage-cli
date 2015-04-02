{-# LANGUAGE OverloadedStrings #-}

module Stackage.CLI.Init
  ( stackageInitModule
  , simpleOptions
  ) where

import Stackage.CLI

stackageInitModule = submoduleOf stackageModule "init"
