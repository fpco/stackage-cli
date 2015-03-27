{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Module.IO
  ( callModule
  , discoverSubmodulesOf
  , moduleProcessName
  ) where

import Control.Applicative
import Data.Conduit
import Data.Text (Text, pack, unpack)
import Data.Monoid
import System.Process (readProcess)

import Module.Types


moduleProcessName :: Module -> Text
moduleProcessName m = contextName <> moduleName m where
  contextName = case moduleContext m of
    Nothing -> mempty
    Just parent -> moduleProcessName parent <> "-"

callModule :: Module -> [Text] -> IO Text
callModule m args =
  pack <$> readProcess (unpack $ moduleProcessName m) (map unpack args) ""

discoverSubmodulesOf :: Module -> Producer IO Module
discoverSubmodulesOf m = return ()
