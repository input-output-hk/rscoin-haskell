module RSCoin.Core.Constants
       ( defaultSecretKeyPath
       , defaultAccountsNumber
       , defaultPort
       , bankHost
       , bankPort
       , periodDelta
       , epochDelta
       , genesisAddress
       , genesisValue
       , periodReward
       , shardSizeScaled
       ) where

import           Data.Maybe             (fromJust)
import           Data.String            (IsString)
import           Data.Time.Units        (Second)

import           RSCoin.Core.Crypto     (constructPublicKey)
import           RSCoin.Core.Primitives (Address (Address), Coin)

-- | Path used by default to read/write secret key.
-- It's not cross-platform because we don't have time. :(
defaultSecretKeyPath :: IsString s => s
defaultSecretKeyPath = "~/.rscoin/key"

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

shardSizeScaled :: Int -> Int
shardSizeScaled i | i <= 0 = error "Shouldn't have the empty list of mintettes."
shardSizeScaled i | i < 2 = 1
shardSizeScaled i = max 2 (i `div` 3)
