{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Exception (catch)
import           Control.Monad
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Stackage.CLI
import           System.Environment
import           System.IO (hPutStr, stderr)
import           System.Exit

onPluginErr :: StackagePluginException -> IO ()
onPluginErr (StackagePluginUnavailable name) = do
  hPutStr stderr $ "Stackage plugin unavailable: " ++ T.unpack name
  exitFailure
onPluginErr (StackagePluginExitFailure name i) = do
  exitWith (ExitFailure i)


main :: IO ()
main =
  do subcommands <-
       fmap (map fst)
            (getSubcommands stackageModule)
     args <- fmap (map T.pack) getArgs
     case dropWhile (T.isPrefixOf "-") args of
       (name:args')
         | elem name subcommands ->
           runStackagePlugin name args' `catch` onPluginErr
       _ ->
         do desc <- subcommandsOf stackageModule
            void (simpleOptions "0.1"
                                "Run stackage commands"
                                "Run stackage commands"
                                (pure ())
                                (Right desc))
