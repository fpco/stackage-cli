{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Exception (catch)
import Stackage.CLI
import System.Environment (getArgs)
import Data.Monoid
import Data.Text (unpack)
import Options.Applicative (long, short, help, metavar, value, switch, Parser, strArgument)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure, exitWith, ExitCode (..))

data Opts = Opts
  { purgeArgs :: [String]
  , initArgs :: [String]
  }


version :: String
version = "0.1"

summary :: String
summary = "Upgrade stackage stuff"

header :: String
header = summary

progDesc :: String
progDesc = summary

optsParser :: Parser Opts
optsParser = Opts <$> purgeArgsParser <*> initArgsParser

initArgsParser :: Parser [String]
initArgsParser = targetToArgs  <$> initOptsParser where
  targetToArgs t = [t]

-- As seen in Init.hs
initOptsParser :: Parser String
initOptsParser = strArgument mods where
  mods = metavar "SNAPSHOT" <> value "lts"

purgeArgsParser :: Parser [String]
purgeArgsParser = forceToArgs <$> purgeOptsParser where
  forceToArgs force = if force then ["--force"] else []

-- As seen in Purge.hs
purgeOptsParser :: Parser Bool
purgeOptsParser = switch mods where
  mods = long "force"
      <> help "Purge all packages without prompt"

handlePluginException :: PluginException -> IO ()
handlePluginException (PluginNotFound _ p) = do
  hPutStrLn stderr $ "stackage-upgrade: requires plugin stackage " <> unpack p
  exitFailure
handlePluginException (PluginExitFailure _ i) = do
  exitWith (ExitFailure i)

-- TODO: no-op if at desired target already
main = do
  (opts, ()) <- simpleOptions
    version
    header
    progDesc
    optsParser   -- global parser
    (Left ())    -- subcommands

  stackage <- findPlugins "stackage"
  callPlugin stackage "purge" (purgeArgs opts)
    `catch` handlePluginException
  callPlugin stackage "init" (initArgs opts)
    `catch` handlePluginException
