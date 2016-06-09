{-# LANGUAGE TemplateHaskell #-}

-- Functions that work with compile-time configuration.

module RSCoin.Core.OwnersConfig
    ( shardSizeOption
    , getShardFunction
    ) where

import           Data.String           (IsString)
import           System.Environment    (getEnvironment)

import           Language.Haskell.TH   (Exp, Q, runIO)

import           RSCoin.Core.Constants (shardDivider)

-- | Environment option to use during compilation.
shardSizeOption :: IsString s => s
shardSizeOption = "SHARD_SIZE"

-- | Defines how many mintettes are responsible for one address (shard
-- size), given the number of mintettes total.
shardSizeScaled :: Int -> Int
shardSizeScaled i
    | i < 1     = error "You can't chose majority from < 1 mintette."
    | otherwise = max 2 (i `div` shardDivider)

-- | Like 'shardSizeScaled' but always return given size.
constShardSize :: Int -> Int -> Int
constShardSize fixedSize i
    | i < 1     = error "You can't chose majority from < 1 mintette."
    | otherwise = min fixedSize i

-- | Looks up a compile-time environment variable.
lookupCompileEnv :: String -> Q (Maybe String)
lookupCompileEnv key = lookup key <$> runIO getEnvironment

-- | Returns proper function whitch depends whether SHARD_SIZE
-- environment option was passed or not.
getShardFunction :: Q Exp
getShardFunction = do
    mShardSize <- lookupCompileEnv shardSizeOption
    case mShardSize of
         Just _  -> [| const $ constShardSize 3 |]
         Nothing -> [| shardSizeScaled          |]
