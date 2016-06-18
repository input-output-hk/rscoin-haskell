{-# LANGUAGE TemplateHaskell #-}

-- | Canonical implementation of owners function which maps transaction id
-- into list of mintettes responsible for it.

module RSCoin.Core.Owners
       ( shardSize
       , owners
       , isOwner
       ) where

import qualified Data.ByteString        as BS
import           Data.Hashable          (hash)
import           Data.List              (nub)

import           RSCoin.Core.Constants  (shardDelta, shardDivider)
import           RSCoin.Core.Crypto     (getHash)
import qualified RSCoin.Core.Crypto     as C
import           RSCoin.Core.Primitives (TransactionId)
import           RSCoin.Core.Types      (MintetteId, Mintettes)

-- | Defines how many mintettes are responsible for one address (shard
-- size), given the number of mintettes total.
shardSize :: Int -> Int
shardSize i
    | i < 1             = error "You can't chose majority from < 1 mintette."
    | shardDivider == 0 = min i shardDelta
    | otherwise         = max 1 $ i `div` shardDivider + shardDelta

-- | Takes list of mintettes which is active and stable over current period
-- and hash of transaction and returns list of mintettes responsible for it.
owners :: [a] -> TransactionId -> [MintetteId]
owners [] _ = []
owners [_] _ = [0]
owners mintettes h =
    take size $ nub $ map (\i -> hash i `mod` l) $ iterate hashProduce h
  where
    l = length mintettes
    size = shardSize l
    hashProduce i = C.hash $ getHash i `BS.append` getHash h

-- | This function checks whether given mintette is owner of given transaction.
-- TODO: most likely it will be possible to implement it more efficiently
isOwner :: Mintettes -> TransactionId -> MintetteId -> Bool
isOwner mts tx mId = mId `elem` owners mts tx
