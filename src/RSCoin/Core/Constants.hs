module RSCoin.Core.Constants
       ( periodDelta
       , epochDelta
       , genesisAddress
       , genesisValue
       ) where

import           Data.Maybe             (fromJust)
import           Data.Time.Units        (Second)

import           RSCoin.Core.Crypto     (constructPublicKey)
import           RSCoin.Core.Primitives (Address (Address), Coin)

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
