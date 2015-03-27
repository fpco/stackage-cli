{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Module.Parser
  ( Subcommand (..)
  , subcommandsFor
  ) where


import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit
import Data.Text (Text, pack, unpack)
import qualified Data.Conduit.List as CL

import SimpleOptions
import Module.Types
import Module.IO


data Subcommand = Subcommand
  { subcommandModule :: Module
  , subcommandArgs :: [Text]
  }

subcommandsFor :: Module -> IO (Mod CommandFields Subcommand)
subcommandsFor m
  = discoverSubmodulesOf m
 $= awaitForever moduleParser
 $$ CL.foldMap id

moduleParser :: Module -> Producer IO (Mod CommandFields Subcommand)
moduleParser m = do
  mSummary <- liftIO $ getSummary m
  case mSummary of
    Nothing -> return ()
    Just summary ->
      yield $ simpleCommand
        (unpack $ moduleName m)
        (unpack summary)
        (Subcommand m)
        varargsParser

getSummary :: Module -> IO (Maybe Text)
getSummary m = (Just <$> callModule m ["--summary"])
  `catch` \(_ :: IOError) -> return Nothing

varargsParser :: Parser [Text]
varargsParser = many (pack <$> strArgument (metavar "ARGUMENTS"))
