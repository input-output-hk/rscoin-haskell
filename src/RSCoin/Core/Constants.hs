{-# LANGUAGE TemplateHaskell #-}

-- | This module contains all constants in rscoin.

module RSCoin.Core.Constants
        ( configDirectory
        , defaultAccountsNumber
        , defaultConfigurationPath
        , defaultConfigurationFileName
        , defaultEpochDelta
        , defaultPort
        , defaultSecretKeyPath
        , defaultPeriodDelta
        , emissionHash
        , genesisValue
        , localhost
        , notaryMSAttemptsLimit
        , periodReward
        , rpcTimeout
        , shardDelta
        , shardDivider
        ) where

import           Data.Binary                (Binary)
import           Data.String                (IsString)
import           Data.Time.Units            (Second)

import           Language.Haskell.TH.Syntax (Lift (lift))

import           System.Directory           (getHomeDirectory)
import           System.FilePath            ((</>))

import qualified RSCoin.Core.CompileConfig  as CC
import           RSCoin.Core.Crypto         (Hash, unsafeHash)
import           RSCoin.Core.Primitives     (Coin (..), Transaction)

-- | Configuration directory (~/.rscoin/ on linux)
configDirectory :: IO FilePath
configDirectory = (</> ".rscoin") <$> getHomeDirectory

-- | Path used by default to read/write secret key.
defaultSecretKeyPath :: IO FilePath
defaultSecretKeyPath = (</> "key") <$> configDirectory

-- | The default number of accounts (sk-pk pairs) generated with the
-- wallet (user part)
defaultAccountsNumber :: Int
defaultAccountsNumber = 3

localhost :: IsString s => s
localhost = "127.0.0.1"

-- | Default port used by applications.
defaultPort :: Num a => a
defaultPort = 3000

defaultPeriodDelta :: Second
defaultPeriodDelta = 100

defaultEpochDelta :: Second
defaultEpochDelta = 5

emissionHash :: Binary t => t -> Hash Transaction
emissionHash a =
    unsafeHash ("This emission hash is needed for all generative" ++
          "transactions to be different" :: String, a)

-- | This value is sent to genesisAddress in genesis transaction
genesisValue :: Coin
genesisValue = 100000000

-- | This value is allocated by Bank in the end of a period.
-- It's then sent distributed accross participating mintettes and Bank.
-- Ideally it should change over time, but let's make it simple.
periodReward :: Coin
periodReward = 1000

-- | The amount of mintettes divided my shardDivider plus shardDelta
-- equals to shard size.
shardDivider :: Int
shardDivider = $(lift $ CC.rscShardDivider CC.rscoinConfig)

-- | The amount of mintettes to be added to shard size.
shardDelta :: Int
shardDelta = $(lift $ CC.rscShardDelta CC.rscoinConfig)

-- | Timeout for rpc calls in microsecons.
-- If timeout exceedes TimeoutError is thrown.
rpcTimeout :: Second
rpcTimeout = $(lift $ CC.rscRpcTimeout CC.rscoinConfig)

-- @TODO move to Notary config
notaryMSAttemptsLimit :: Int
notaryMSAttemptsLimit = 5

-- | File name for running app with configuration of 'configurator' libary.
defaultConfigurationPath :: IO FilePath
defaultConfigurationPath = (</> defaultConfigurationFileName) <$> configDirectory

defaultConfigurationFileName :: IsString s => s
defaultConfigurationFileName = "deploy-rscoin.cfg"
