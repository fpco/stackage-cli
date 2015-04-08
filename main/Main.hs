{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import           Control.Applicative
import           Control.Exception (catch)
import           Control.Monad
import           Data.Maybe (isJust)
import           Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Stackage.CLI
import           System.Environment
import           System.IO (hPutStr, stderr)
import           System.Exit

onPluginErr :: PluginException -> IO ()
onPluginErr (PluginNotFound _ name) = do
  hPutStr stderr $ "Stackage plugin unavailable: " ++ T.unpack name
  exitFailure
onPluginErr (PluginExitFailure _ i) = do
  exitWith (ExitFailure i)


main :: IO ()
main = do
  stackage <- findPlugins "stackage"
  args <- getArgs
  case dropWhile (List.isPrefixOf "-") args of
    ((T.pack -> name):args')
      | isJust (lookupPlugin stackage name) ->
          callPlugin stackage name args' `catch` onPluginErr
    _ -> do
      simpleOptions
        "0.1"
        "Run stackage commands"
        "Run stackage commands"
        (pure ())
        (Right $ commandsFromPlugins stackage)
      return ()
