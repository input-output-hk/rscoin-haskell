-- | Canonical implementation of owners function which maps transaction id
-- into list of mintettes responsible for it.

module RSCoin.Core.Owners
       ( owners
       , isOwner
       ) where

import           RSCoin.Core.Primitives (TransactionId)
import           RSCoin.Core.Types      (MintetteId, Mintettes)

-- | Takes list of mintettes which is active and stable over current period
-- and hash of transaction and returns list of mintettes responsible for it.
owners :: Mintettes -> TransactionId -> [MintetteId]
owners = undefined

-- | This function checks whether given mintette is owner of given transaction.
-- TODO: most likely it will be possible to implement it more efficiently
isOwner :: Mintettes -> TransactionId -> MintetteId -> Bool
isOwner mts tx mId = mId `elem` owners mts tx
