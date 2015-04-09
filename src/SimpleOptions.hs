{-# LANGUAGE RankNTypes #-}

-- | A convenience wrapper around Options.Applicative.
module SimpleOptions
    ( simpleOptions
    , Options.addCommand
    , Options.simpleVersion
    ) where

import           Control.Applicative
import           Control.Monad.Trans.Either (EitherT)
import           Control.Monad.Trans.Writer (Writer)
import           Data.Monoid
import qualified Options.Applicative.Simple as Options

-- | This is a drop-in replacement for simpleOptions from
-- Options.Applicative.Simple, with the added feature of a `--summary` flag
-- that prints out the header. (Should be one line)
simpleOptions
  :: String
  -- ^ version string
  -> String
  -- ^ header
  -> String
  -- ^ program description
  -> Options.Parser a
  -- ^ global settings
  -> EitherT b (Writer (Options.Mod Options.CommandFields b)) ()
  -- ^ commands (use 'addCommand')
  -> IO (a,b)
simpleOptions versionString h pd globalParser mcommands =
    Options.simpleOptions
              versionString h pd globalParser' mcommands
  where
    globalParser' = summaryOption <*> globalParser
    summaryOption = Options.infoOption h
      $ Options.long "summary"
     <> Options.help "Show program summary"
