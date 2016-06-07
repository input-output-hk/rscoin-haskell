{-# LANGUAGE TemplateHaskell #-}

-- | Config for remote benchmark.

module Bench.RSCoin.RemoteConfig
       ( RemoteConfig (..)
       , readRemoteConfig
       ) where

import qualified Data.Aeson.TH as A
import           Data.Maybe    (fromMaybe)
import qualified Data.Yaml     as Y

data RemoteConfig = RemoteConfig
    { usersNum :: Word
    } deriving (Show)

$(A.deriveJSON A.defaultOptions ''RemoteConfig)

readRemoteConfig :: FilePath -> IO RemoteConfig
readRemoteConfig fp =
    fromMaybe (error "FATAL: failed to parse config") <$> Y.decodeFile fp
