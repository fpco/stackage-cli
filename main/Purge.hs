{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (listToMaybe, mapMaybe)
import Stackage.CLI.Purge
import Filesystem
import Control.Monad
import Control.Applicative
import Data.Monoid
import Options.Applicative (Parser, flag, long, help)
import System.Process (readProcess)
import Data.Char (toLower)
import System.IO (stdout, hFlush)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import System.Environment (getArgs)

data Force = Prompt | Force
data PurgeOpts = PurgeOpts
  { purgeOptsForce :: Force }

prompt :: String -> IO String
prompt str = putStr str >> hFlush stdout >> getLine

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust Nothing _ = return ()

pluralize :: Int -> a -> a -> a
pluralize 1 a _ = a
pluralize _ _ a = a

unregisterPackages :: String -> [String] -> IO ()
unregisterPackages packageDb = mapM_ unregister where
  unregister package = do
    putStrLn $ "Unregistering: " <> package
    _ <- readProcess "ghc-pkg" (args package) ""
    return ()
  args package =
    [ "unregister"
    , package
    , "--force"
    , "--no-user-package-db"
    , "--package-db"
    , packageDb
    ]

parsePackageDb :: IO (Maybe String)
parsePackageDb = do
  cabalSandboxConfigExists <- isFile "cabal.sandbox.config"
  if cabalSandboxConfigExists
    then do
      t <- T.decodeUtf8 <$> Filesystem.readFile "cabal.sandbox.config"
      let packageDbLine = T.stripPrefix "package-db: "
      return $ fmap T.unpack $ listToMaybe $ mapMaybe packageDbLine $ T.lines t
    else
      return Nothing

getGlobalPackageDb :: IO (Maybe String)
getGlobalPackageDb = do
  let fakePackage = "asdklfjasdklfajsdlkghaiwojgadjfkq"
  output <- readProcess "ghc-pkg" ["list", fakePackage] ""
  return $ listToMaybe (lines output)

getPackages :: String -> IO [String]
getPackages packageDb = words <$> readProcess "ghc-pkg" args "" where
  args =
    [ "list"
    , "--simple-output"
    , "--no-user-package-db"
    , "--package-db"
    , packageDb
    ]

purge :: PurgeOpts -> IO ()
purge opts = do
  cabalConfigExists <- isFile "cabal.config"
  when cabalConfigExists $ do
    removeFile "cabal.config"

  globalPackageDbMay <- getGlobalPackageDb
  whenJust globalPackageDbMay $ \globalPackageDb -> do
    putStrLn "Detected global package database:"
    putStrLn globalPackageDb

    packageDbMay <- parsePackageDb
    whenJust packageDbMay $ \packageDb -> do
      putStrLn "Detected sandbox package database:"
      putStrLn packageDb

      packages <- getPackages packageDb
      let nPackages = length packages
      let showNPackages
             = show nPackages
            <> " "
            <> pluralize nPackages "package" "packages"

      when (nPackages > 0 && nPackages < 15) $ mapM_ putStrLn packages
      putStrLn
         $ "Detected "
        <> showNPackages
        <> " to unregister"

      when (nPackages > 0) $ do
        shouldUnregister <- case purgeOptsForce opts of
          Force -> return True
          Prompt -> do
            line <- prompt
              $ "Unregister " <> showNPackages <> " (y/n)? [default: n] "
            case map toLower line of
              "y"   -> return True
              "yes" -> return True
              _   -> return False
        when shouldUnregister $ unregisterPackages packageDb packages

purgeOptsParser :: Parser PurgeOpts
purgeOptsParser = PurgeOpts <$> forceOpt where
  forceOpt = flag Prompt Force mods
  mods = long "force"
      <> help "Purge all packages without prompt"


version :: String
version = "0.1"

header :: String
header = "Purge stackage junk"

progDesc :: String
progDesc = header

-- TODO: use simpleOptions main below
--main :: IO ()
--main = do
--  args <- getArgs
--  case args of
--    ["--summary"] -> putStrLn header
--    ["--force"]   -> purge (PurgeOpts Force)
--    _             -> print args >> purge (PurgeOpts Prompt)

main = do
  (opts, ()) <- simpleOptions
    version
    header
    progDesc
    purgeOptsParser -- global parser
    (Left ())       -- subcommands
  purge opts
