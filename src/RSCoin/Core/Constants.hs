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
       ) where

import           Data.Maybe             (fromJust)
import           Data.String            (IsString)
import           Data.Time.Units        (Second)
import           System.Directory       (getHomeDirectory)
import           System.FilePath        ((</>))

import           RSCoin.Core.Crypto     (Hash, constructPublicKey, hash)
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

emissionHash :: Hash
emissionHash =
    hash ("This emission hash is needed for all generative" ++
          "transactions to be different" :: String)

-- | Special address used as output in genesis transaction
genesisAddress :: Address
genesisAddress =
    Address $
    fromJust $
    constructPublicKey "AtRoNkuSjCkIPdUz+He0xKqmcH0I0OTLal+4vIRitm4/"

-- | This value is sent to genesisAddress in genesis transaction
genesisValue :: Coin
genesisValue = 1000000

-- | This value is allocated by Bank in the end of a period.
-- It's then sent distributed accross participating mintettes and Bank.
-- Ideally it should change over time, but let's make it simple.
periodReward :: Coin
periodReward = 10000

-- | The amount of mintettes divided my shardDivider equals to shard
-- size.
shardDivider :: Int
shardDivider = 3
