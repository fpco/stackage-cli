{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text, pack)

import Stackage.CLI
import System.Environment (getArgs)


version :: String
version = "0.1"

summary :: String
summary = "Upgrade stackage stuff"

-- TODO: no-op if at latest already
-- TODO: use simpleOpts
main = do
  args <- getArgs
  case args of
    ["--version"] -> putStrLn version
    ["--summary"] -> putStrLn summary
    _ -> do
      runStackagePlugin "purge" []
      runStackagePlugin "init" $ map pack args
