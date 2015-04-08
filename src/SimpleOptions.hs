{-# LANGUAGE RankNTypes #-}

-- | A convenience wrapper around Options.Applicative.
module SimpleOptions
    ( module SimpleOptions
    , module Options.Applicative
    ) where

import           Options.Applicative

-- | Create a simple program options parser that responds to
-- the `--help`, `--version`, and `--summary` flags.
simpleOptions :: String                          -- ^ version string
              -> String                          -- ^ header
              -> String                          -- ^ program description
              -> Parser a                        -- ^ global settings
              -> Either b (Mod CommandFields b)  -- ^ commands
              -> IO (a, b)
simpleOptions versionString h pd globalParser mcommands = do
    let config = (,) <$> globalParser <*> either pure subparser mcommands
    execParser $ info (helpOption <*> versionOption <*> summaryOption <*> config) desc
  where
    desc = fullDesc <> header h <> progDesc pd
    helpOption =
        abortOption ShowHelpText $
        long "help" <>
        help "Show this help text"
    versionOption =
        infoOption
            versionString
            (long "version" <>
             help "Show version")
    summaryOption =
        infoOption
            h -- reusing the "header" TODO: something else?
            (long "summary" <>
             help "Show program summary")

-- | Create a simple command for use with `simpleOptions`.
simpleCommand :: String   -- ^ command string
           -> String   -- ^ title of command
           -> (a -> b) -- ^ constructor to wrap up command in common data type
           -> Parser a -- ^ command parser
           -> Mod CommandFields b
simpleCommand cmd title constr inner = command
    cmd
    (info (constr <$> inner) (progDesc title))
