{-# LANGUAGE TemplateHaskell #-}

-- | This module contains all constants in rscoin.

module RSCoin.Core.Constants
       ( defaultSecretKeyPath
       , defaultAccountsNumber
       , defaultPort
       , defaultBankHost
       , localhost
       , localPlatformLayout
       , defaultLayout
       , defaultLayout'
       , bankPort
       , defaultPeriodDelta
       , epochDelta
       , emissionHash
       , genesisValue
       , periodReward
       , shardDivider
       , shardDelta
       , testBankPublicKey
       , rpcTimeout
       , notaryMSAttemptsLimit
       ) where

import           Data.Bifunctor             (first)
import           Data.Binary                (Binary)
import           Data.Maybe                 (fromMaybe)
import           Data.String                (IsString)
import           Data.Time.Units            (Second)

import           Language.Haskell.TH.Syntax (Lift (lift))

import           System.Directory           (getHomeDirectory)
import           System.FilePath            ((</>))

import qualified RSCoin.Core.CompileConfig  as CC
import           RSCoin.Core.Crypto         (Hash, PublicKey,
                                             constructPublicKey, hash)
import           RSCoin.Core.Primitives     (Coin (..))
import           RSCoin.Core.NodeConfig     (Host, NodeContext (..), Port)

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

localhost :: IsString s => s
localhost = "127.0.0.1"

-- | Settings for local deployment
localPlatformLayout :: NodeContext
localPlatformLayout = NodeContext
    { _bankAddr      = (localhost, 3000)
    , _notaryAddr    = (localhost, 4001)
    , _bankPublicKey = testBankPublicKey
    , _bankSecretKey = Nothing
    }

defaultLayout :: NodeContext
defaultLayout = NodeContext
    { _bankAddr      = $(lift $ CC.toAddr $ CC.rscDefaultBank   CC.rscoinConfig)
    , _notaryAddr    = $(lift $ CC.toAddr $ CC.rscDefaultNotary CC.rscoinConfig)
    , _bankPublicKey = testBankPublicKey
    , _bankSecretKey = Nothing
    }

defaultLayout' :: Host -> NodeContext
defaultLayout' bankHost =
    defaultLayout { _bankAddr = first (const bankHost) (_bankAddr defaultLayout) }

bankPort :: Port
bankPort = snd $ _bankAddr defaultLayout

defaultBankHost :: Host
defaultBankHost = fst $ _bankAddr defaultLayout

defaultPeriodDelta :: Second
defaultPeriodDelta = 100

epochDelta :: Second
epochDelta = 5

emissionHash :: Binary t => t -> Hash
emissionHash a =
    hash ("This emission hash is needed for all generative" ++
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

-- | This Bank public key should be used only for tests and benchmarks.
testBankPublicKey :: PublicKey
testBankPublicKey = fromMaybe
    (error "[FATAL] Failed to parse hardcoded Bank's public key")
    $ constructPublicKey "YblQ7+YCmxU/4InsOwSGH4Mm37zGjgy7CLrlWlnHdnM="

-- @TODO move to Notary config
notaryMSAttemptsLimit :: Int
notaryMSAttemptsLimit = 5
