-- | Canonical implementation of owners function which maps transaction id
-- into list of mintettes responsible for it.

module RSCoin.Core.Owners
       ( owners
       ) where

import           RSCoin.Core.Primitives (TransactionId)
import           RSCoin.Core.Types      (Mintettes)

-- | Takes list of mintettes which is active and stable over current period
-- and index of transaction and returns list of mintettes responsible for it.
owners :: Mintettes -> TransactionId -> Mintettes
owners = undefined
