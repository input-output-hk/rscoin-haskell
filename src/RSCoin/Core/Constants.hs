module RSCoin.Core.Constants
       ( bankAddress
       , bankPort
       , periodDelta
       , epochDelta
       , genesisAddress
       , genesisValue
       , periodReward
       ) where

import           Data.Maybe             (fromJust)
import           Data.Time.Units        (Second)

import           RSCoin.Core.Crypto     (constructPublicKey)
import           RSCoin.Core.Primitives (Address (Address), Coin)

bankAddress :: String
bankAddress = undefined

bankPort :: Int
bankPort = undefined

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
