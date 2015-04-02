{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Control.Monad
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Stackage.CLI
import           System.Environment

main :: IO ()
main =
  do subcommands <-
       fmap (map fst)
            (getSubcommands stackageModule)
     args <- fmap (map T.pack) getArgs
     case filter (not .
                  T.isSuffixOf "-")
                 args of
       (name:args)
         | elem name subcommands ->
           execModule (Module name (Just stackageModule))
                      args
       _ ->
         do desc <- subcommandsOf stackageModule
            void (simpleOptions "0.1"
                                "Run stackage commands"
                                "Run stackage commands"
                                (pure ())
                                desc)
