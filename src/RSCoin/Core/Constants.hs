{-# LANGUAGE TemplateHaskell #-}

-- | This module contains all constants in rscoin.

module RSCoin.Core.Constants
       ( PlatformLayout (..)
       , defaultSecretKeyPath
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
       , bankPublicKey
       , genesisAddress
       , genesisValue
       , periodReward
       , shardDivider
       , shardDelta
       , rpcTimeout
       , bankSecretKey
       , attainPublicKey
       , attainSecretKey
       , chainRootPKs
       , notaryMSAttemptsLimit
       ) where

import           Data.Binary                (Binary)
import           Data.FileEmbed             (embedFile, makeRelativeToProject)
import           Data.Maybe                 (fromMaybe)
import           Data.String                (IsString)
import           Data.Time.Units            (Second)

import           Language.Haskell.TH.Syntax (Lift (lift))

import           System.Directory           (getHomeDirectory)
import           System.FilePath            ((</>))

import qualified RSCoin.Core.CompileConfig  as CC
import           RSCoin.Core.Crypto         (Hash, PublicKey, SecretKey,
                                             constructPublicKey,
                                             constructSecretKey,
                                             deterministicKeyGen, hash)
import           RSCoin.Core.Primitives     (Address (Address), Coin (..))
import           RSCoin.Timed.MonadRpc      (Host, PlatformLayout (..), Port)

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
localPlatformLayout :: PlatformLayout
localPlatformLayout = PlatformLayout
    { getBankAddr   = (localhost, 3000)
    , getNotaryAddr = (localhost, 4001)
    }

defaultLayout :: PlatformLayout
defaultLayout = PlatformLayout
    { getBankAddr   = $(lift $ CC.toAddr $ CC.rscDefaultBank   CC.rscoinConfig)
    , getNotaryAddr = $(lift $ CC.toAddr $ CC.rscDefaultNotary CC.rscoinConfig)
    }

defaultLayout' :: Host -> PlatformLayout
defaultLayout' bankHost
    = let PlatformLayout (_, bPort) sAddr = defaultLayout
       in PlatformLayout (bankHost, bPort) sAddr

bankPort :: Port
bankPort = snd $ getBankAddr defaultLayout

defaultBankHost :: Host
defaultBankHost = fst $ getBankAddr defaultLayout

defaultPeriodDelta :: Second
defaultPeriodDelta = 100

epochDelta :: Second
epochDelta = 5

emissionHash :: Binary t => t -> Hash
emissionHash a =
    hash ("This emission hash is needed for all generative" ++
          "transactions to be different" :: String, a)

bankPublicKey :: PublicKey
bankPublicKey =
    fromMaybe (error "[FATAL] Failed to parse Bank's public key") $
    constructPublicKey "YblQ7+YCmxU/4InsOwSGH4Mm37zGjgy7CLrlWlnHdnM="

-- | Special address used as output in genesis transaction
genesisAddress :: Address
genesisAddress = Address bankPublicKey

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

-- | Bank's secret key which can be used to spend coins from genesis transaction.
-- It's needed only for tests/benchmarks.
bankSecretKey :: SecretKey
bankSecretKey =
    constructSecretKey $ $(makeRelativeToProject "rscoin-key" >>= embedFile)

-- | Attain public key pair. It's needed for multisignature address allocation.
attainKeyPair :: (PublicKey, SecretKey)
attainKeyPair =
    fromMaybe (error "Invalid Attain address seed")
    $ deterministicKeyGen "attain-service-public-addressgen"

-- | Known public key of attain
attainPublicKey :: PublicKey
attainPublicKey = fst attainKeyPair

-- @TODO Move it in proper place so nobody can know about it
attainSecretKey :: SecretKey
attainSecretKey = snd attainKeyPair

-- | Built-in know root public keys.
chainRootPKs :: [PublicKey]
chainRootPKs = [attainPublicKey]

-- @TODO move to Notary config
notaryMSAttemptsLimit :: Int
notaryMSAttemptsLimit = 5
