{-# LANGUAGE OverloadedStrings #-}

module Main where

import Filesystem
import Control.Monad
import Stackage.CLI.Init
import Options.Applicative (Parser)
import Options.Applicative.Builder (strArgument, metavar, value)
import Data.Monoid
import Network.HTTP.Client
import qualified Data.ByteString.Lazy as LBS
import System.Exit (exitFailure)
import System.Environment (getArgs)

type Target = String

targetParser :: Parser Target
targetParser = strArgument mods where
  mods = metavar "SNAPSHOT" <> value "lts"

isValidTarget :: Target -> IO Bool
isValidTarget "lts" = return True
isValidTarget _ = return False

initTarget :: Target -> IO ()
initTarget t = do
  validTarget <- isValidTarget t
  unless validTarget $ do
    putStrLn $ "Invalid target: " <> t
    exitFailure

  configExists <- isFile "cabal.config"
  when configExists $ do
    putStrLn $ "Warning: cabal.config already exists"
    putStrLn $ "No action taken"
    exitFailure

  withManager defaultManagerSettings $ \manager -> do
    req <- parseUrl "http://stackage.org/lts/cabal.config"
    response <- httpLbs req manager
    let lbs = responseBody response
    LBS.writeFile "cabal.config" lbs


version :: String
version = "0.1"

header :: String
header = "Initializes cabal.config"

progDesc :: String
progDesc = header

main = do
  (target, ()) <- simpleOptions
    version
    header
    progDesc
    targetParser -- global parser
    mempty       -- subcommands
  initTarget target
