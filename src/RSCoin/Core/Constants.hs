module RSCoin.Core.Constants
       ( periodDelta
       , epochDelta
       , genesisAddress
       ) where

import           Data.Maybe             (fromJust)
import           Data.Time.Units        (Second)

import           RSCoin.Core.Crypto     (constructPublicKey)
import           RSCoin.Core.Primitives (Address (Address))

periodDelta :: Second
periodDelta = 50

epochDelta :: Second
epochDelta = 5

-- | Special address used as output in genesis transaction
genesisAddress :: Address
genesisAddress = Address $ fromJust $ constructPublicKey undefined
