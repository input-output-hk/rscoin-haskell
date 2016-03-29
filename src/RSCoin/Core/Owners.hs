-- | Canonical implementation of owners function which maps transaction id
-- into list of mintettes responsible for it.

module RSCoin.Core.Owners
       ( owners
       , isOwner
       ) where

import qualified Data.ByteString        as BS
import           Data.Hashable          (hash)
import           Data.List              (nub)

import           RSCoin.Core.Constants  (shardSizeScaled)
import           RSCoin.Core.Crypto     (getHash)
import qualified RSCoin.Core.Crypto     as C
import           RSCoin.Core.Primitives (TransactionId)
import           RSCoin.Core.Types      (MintetteId, Mintettes)

-- | Takes list of mintettes which is active and stable over current period
-- and hash of transaction and returns list of mintettes responsible for it.
owners :: Mintettes -> TransactionId -> [MintetteId]
owners mintettes h =
    let shardSize = shardSizeScaled l
    in take shardSize $ nub $ map (\i -> hash i `mod` l) $ iterate hashProduce h
  where
    l = length mintettes
    hashProduce i = C.hash $ getHash i `BS.append` getHash h

-- TODO: most likely it will be possible to implement it more efficiently
isOwner :: Mintettes -> TransactionId -> MintetteId -> Bool
isOwner mts tx mId = mId `elem` owners mts tx
