{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

module Main where

import Data.Maybe (listToMaybe, mapMaybe)
import Stackage.CLI
import Filesystem
import Control.Exception (Exception, catch)
import Control.Monad
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Applicative
import Data.Monoid
import Data.Typeable (Typeable)
import Options.Applicative (Parser, flag, long, help)
import System.Process (readProcess)
import Data.Char (toLower)
import System.IO (stdout, stderr, hFlush, hPutStrLn)
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Paths_stackage_cli as CabalInfo

import Text.Parsec hiding ((<|>), many)
type ParsecParser = Parsec String ()

data Force = Prompt | Force
data PurgeOpts = PurgeOpts
  { purgeOptsForce :: Force }

data PackageGroup = PackageGroup
  { packageGroupDb :: String
  , packageGroupPackages :: [String]
  }

data PurgeException
  = ParsePackagesError ParseError
  deriving (Show, Typeable)
instance Exception PurgeException

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
    ] <> dbToArgs (Just packageDb)

parsePackageDb :: IO (Maybe String)
parsePackageDb = do
  cabalSandboxConfigExists <- isFile "cabal.sandbox.config"
  if cabalSandboxConfigExists
    then do
      t <- Filesystem.readTextFile "cabal.sandbox.config"
      let packageDbLine = T.stripPrefix "package-db: "
      return $ fmap T.unpack $ listToMaybe $ mapMaybe packageDbLine $ T.lines t
    else
      return Nothing

dbToArgs :: Maybe String -> [String]
dbToArgs Nothing = []
dbToArgs (Just packageDb) =
  [ "--package-db"
  , packageDb
  ]

getGlobalPackageDb :: IO (Maybe String)
getGlobalPackageDb = do
  let fakePackage = "asdklfjasdklfajsdlkghaiwojgadjfkq"
  output <- readProcess "ghc-pkg" ["list", fakePackage] ""
  return $ fmap init $ listToMaybe (lines output)
  -- fmap init is to get rid of the trailing colon

getPackages :: Maybe String -> IO [PackageGroup]
getPackages mPackageDb = parsePackages =<< readProcess "ghc-pkg" args "" where
  args = ["list"] <> dbToArgs mPackageDb

parsePackages :: MonadThrow m => String -> m [PackageGroup]
parsePackages
  = either (throwM . ParsePackagesError) return
  . parse packagesParser ""

-- #28
#if !MIN_VERSION_parsec(3,1,6)
endOfLine :: ParsecParser Char
endOfLine = newline <|> crlf <?> "new-line"
#endif

ending :: ParsecParser ()
ending = eof <|> void endOfLine

packagesParser :: ParsecParser [PackageGroup]
packagesParser = many1 parseGroup

parseGroup :: ParsecParser PackageGroup
parseGroup = PackageGroup <$> parseDb <*> parseDbPackages <* many endOfLine

parseDb :: ParsecParser String
parseDb = manyTill anyChar $ try (char ':' *> ending)

parseDbPackages :: ParsecParser [String]
parseDbPackages = try parseNoPackages <|> many1 parsePackage

parseNoPackages :: ParsecParser [String]
parseNoPackages = many1 (char ' ') *> string "(no packages)" *> ending *> pure []

parsePackage :: ParsecParser String
parsePackage = many1 (char ' ') *> manyTill anyChar ending


purge :: PurgeOpts -> IO ()
purge opts = do
  cabalConfigExists <- isFile "cabal.config"
  when cabalConfigExists $ do
    removeFile "cabal.config"

  globalPackageDbMay <- getGlobalPackageDb
  sandboxPackageDbMay <- parsePackageDb

  let displaySandbox s
        | Just s == globalPackageDbMay =
          "(Global) " <> s
        | Just s == sandboxPackageDbMay =
          "(Sandbox) " <> s
        | otherwise = s

  packages <- getPackages sandboxPackageDbMay
  forM_ packages $ \(PackageGroup db packages) -> do
    putStrLn $ displaySandbox db
    let nPackages = length packages
    let showNPackages
           = show nPackages
          <> " "
          <> pluralize nPackages "package" "packages"

    putStrLn
      $ "Detected "
     <> showNPackages
     <> " to purge from this database"

    when (nPackages > 0) $ do
      when (nPackages < 15) $ mapM_ putStrLn packages
      shouldUnregister <- case purgeOptsForce opts of
        Force -> do
          putStrLn $ "(--force) Unregistering " <> showNPackages
          return True
        Prompt -> do
          line <- prompt
            $ "Unregister " <> showNPackages <> " (y/n)? [default: n] "
          case map toLower line of
            "y"   -> return True
            "yes" -> return True
            _   -> return False
      when shouldUnregister $ unregisterPackages db packages
  return ()

purgeOptsParser :: Parser PurgeOpts
purgeOptsParser = PurgeOpts <$> forceOpt where
  forceOpt = flag Prompt Force mods
  mods = long "force"
      <> help "Purge all packages without prompt"

version :: String
version = $(simpleVersion CabalInfo.version)

header :: String
header = "Delete cabal.config and purge your package database"

progDesc :: String
progDesc = header

handlePurgeExceptions :: PurgeException -> IO ()
handlePurgeExceptions (ParsePackagesError _) = do
  hPutStrLn stderr $ "Failed to parse ghc-pkg output"
  exitFailure

main :: IO ()
main = do
  (opts, ()) <- simpleOptions
    version
    header
    progDesc
    purgeOptsParser -- global parser
    empty           -- subcommands
  purge opts `catch` handlePurgeExceptions
