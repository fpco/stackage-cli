{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Module.Parser
  ( Subcommand (..)
  , subcommandsOf
  ) where


import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import Data.Text (Text, pack, unpack)
import qualified Data.Conduit.List as CL
import System.Environment (getArgs)

import SimpleOptions
import Module.Types
import Module.IO

-- | Describes the intent to invoke a module with the specified arguments
data Subcommand = Subcommand
  { subcommandModule :: Module
  , subcommandArgs :: [Text]
  }

-- | Dynamically create a description of an executable's commands
-- by discovering its plugins and calling them with `--summary`.
subcommandsOf :: Module -> IO (Mod CommandFields Subcommand)
subcommandsOf m
  = discoverSubmodulesOf m
 $= awaitForever moduleParser
 $$ CL.foldMap id


moduleParser :: Module -> Producer IO (Mod CommandFields Subcommand)
moduleParser m = do
  -- Don't process the rest of the arguments
  -- if we're dispatching to this submodule.
  -- I wish optparse-applicative had a better way to do this
  -- that could avoid unfortunate name clashes.
  allArgs <- liftIO $ map pack <$> getArgs
  let args = dropWhile (/= moduleName m) allArgs

  -- Try to get a summary, yield a command if we get one.
  mSummary <- liftIO $ getSummary m
  case mSummary of
    Nothing -> return ()
    Just summary ->
      yield $ simpleCommand
        (unpack $ moduleName m)
        (unpack summary)
        (Subcommand m)
        (givenArgsParser args)

getSummary :: Module -> IO (Maybe Text)
getSummary m = (Just <$> readProcModule m ["--summary"])
  `catch` \(_ :: IOError) -> return Nothing

givenArgsParser :: [Text] -> Parser [Text]
givenArgsParser = pure
