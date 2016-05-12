{-# LANGUAGE TemplateHaskell #-}

-- | This module contains all constants in rscoin.

module RSCoin.Core.Constants
       ( defaultSecretKeyPath
       , defaultAccountsNumber
       , defaultPort
       , bankHost
       , bankPort
       , periodDelta
       , epochDelta
       , emissionHash
       , genesisAddress
       , genesisValue
       , periodReward
       , shardDivider
       , rpcTimeout
       , bankSecretKey
       ) where

import           Data.Binary            (Binary)
import           Data.FileEmbed         (embedFile, makeRelativeToProject)
import           Data.Maybe             (fromJust)
import           Data.String            (IsString)
import           Data.Time.Units        (Second)
import           System.Directory       (getHomeDirectory)
import           System.FilePath        ((</>))

import           RSCoin.Core.Crypto     (Hash, SecretKey, constructPublicKey,
                                         constructSecretKey, hash)
import           RSCoin.Core.Primitives (Address (Address), Coin)

-- | Path used by default to read/write secret key.
defaultSecretKeyPath :: IO FilePath
defaultSecretKeyPath = (\h -> h </> ".rscoin" </> "key") <$> getHomeDirectory

-- | The default number of accounts (sk-pk pairs) generated with the
-- wallet (user part)
defaultAccountsNumber :: Int
defaultAccountsNumber = 5

-- | Default port used by applications.
defaultPort :: Num a => a
defaultPort = 3000

bankHost :: IsString s => s
bankHost = "127.0.0.1"

bankPort :: Num a => a
bankPort = defaultPort

periodDelta :: Second
periodDelta = 50

epochDelta :: Second
epochDelta = 5

emissionHash :: Binary t => t -> Hash
emissionHash a =
    hash ("This emission hash is needed for all generative" ++
          "transactions to be different" :: String, a)

-- | Special address used as output in genesis transaction
genesisAddress :: Address
genesisAddress =
    Address $
    fromJust $
    constructPublicKey "AtRoNkuSjCkIPdUz+He0xKqmcH0I0OTLal+4vIRitm4/"

-- | This value is sent to genesisAddress in genesis transaction
genesisValue :: Coin
genesisValue = 100000000

-- | This value is allocated by Bank in the end of a period.
-- It's then sent distributed accross participating mintettes and Bank.
-- Ideally it should change over time, but let's make it simple.
periodReward :: Coin
periodReward = 1000

-- | The amount of mintettes divided my shardDivider equals to shard
-- size.
shardDivider :: Int
shardDivider = 3

-- | Timeout for rpc calls in microsecons.
-- If timeout exceedes TimeoutError is thrown.
rpcTimeout :: Second
rpcTimeout = 5

-- | Bank's secret key which can be used to spend coins from genesis transaction.
-- It's needed only for tests/benchmarks.
bankSecretKey :: SecretKey
bankSecretKey =
    constructSecretKey $ $(makeRelativeToProject "rscoin-key" >>= embedFile)
