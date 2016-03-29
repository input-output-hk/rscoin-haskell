-- | Canonical implementation of owners function which maps transaction id
-- into list of mintettes responsible for it.

module RSCoin.Core.Owners
       ( owners
       , isOwner
       ) where

import qualified Data.ByteString        as BS
import           Data.Hashable          (hash)
import           Data.List              (nub)

import           RSCoin.Core.Constants  (shardDivider)
import           RSCoin.Core.Crypto     (getHash)
import qualified RSCoin.Core.Crypto     as C
import           RSCoin.Core.Primitives (TransactionId)
import           RSCoin.Core.Types      (MintetteId, Mintettes)

-- | Defines how many mintettes are responsible for one address (shard
-- size), given the number of mintettes total.
shardSizeScaled :: Int -> Int
shardSizeScaled i | i < 2 = error "You can't chose majority from <2 mintettes."
shardSizeScaled i = max 2 (i `div` shardDivider)

-- | Takes list of mintettes which is active and stable over current period
-- and hash of transaction and returns list of mintettes responsible for it.
owners :: Mintettes -> TransactionId -> [MintetteId]
owners mintettes h =
    let shardSize = shardSizeScaled l
    in take shardSize $ nub $ map (\i -> hash i `mod` l) $ iterate hashProduce h
  where
    l = length mintettes
    hashProduce i = C.hash $ getHash i `BS.append` getHash h

-- | This function checks whether given mintette is owner of given transaction.
-- TODO: most likely it will be possible to implement it more efficiently
isOwner :: Mintettes -> TransactionId -> MintetteId -> Bool
isOwner mts tx mId = mId `elem` owners mts tx
