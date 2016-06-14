{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- This module contains functions to work with compile-time
-- configuration and data type representing this configuration.

module RSCoin.Core.CompileConfig
    ( RSCoinConfig (..)
    , readRSCoinConfig
    ) where

import           Data.ByteString (ByteString)
import           Data.FileEmbed  (embedStringFile, makeRelativeToProject)
import           Data.Maybe      (fromMaybe)
import           Data.Yaml       (FromJSON, decode)
import           GHC.Generics    (Generic)

data RSCoinConfig = RSCoinConfig
    { shardDivider :: !Int
    , shardDelta   :: !Int
    } deriving (Generic, Show)

instance FromJSON RSCoinConfig

rscoinConfig :: ByteString
rscoinConfig = $(makeRelativeToProject "rscoin.yaml" >>= embedStringFile)

-- | Reading configuration from `rscoin.yaml` file and return it in data type.
readRSCoinConfig :: RSCoinConfig
readRSCoinConfig =
    fromMaybe (error "FATAL: failed to parse rscoin.yaml") $ decode rscoinConfig