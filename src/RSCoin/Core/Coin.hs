-- | Functions related to Coin datatype

module RSCoin.Core.Coin
       ( onColor
       , sameColor
       , coinsToList
       , coinsToMap
       , groupCoinsList
       , coinsMapConsistent
       , mergeCoinsMaps
       ) where

import           Control.Exception      (assert)
import           Data.List              (groupBy, sortBy)
import qualified Data.Map               as M
import           Data.Maybe             (fromJust)
import           Data.Ord               (comparing)

import           RSCoin.Core.Primitives (Coin (..), Color)

onColor :: Coin -> Coin -> Ordering
onColor = comparing getColor

sameColor :: Coin -> Coin -> Bool
sameColor a b = EQ == onColor a b

-- | Given a list of arbitrary coins, it sums the coins with the same
-- color and returns list of coins of distinct color sorted by the
-- color. Also deletes negative coins.
groupCoinsList :: [Coin] -> [Coin]
groupCoinsList coins =
    sortBy onColor $
    map sum $
    groupBy sameColor $
    filter ((> 0) . getCoin)
    coins

-- | Translates a map of coins to the list, sorted by color
coinsToList :: M.Map Color Coin -> [Coin]
coinsToList coinsMap = groupCoinsList $ M.elems coinsMap

-- | Translates a list of coins to the map
coinsToMap :: [Coin] -> M.Map Color Coin
coinsToMap = M.fromList . map (\c -> (getColor c, c)) . groupCoinsList

-- | Checks a consistency of map from color to coin
coinsMapConsistent :: M.Map Color Coin -> Bool
coinsMapConsistent coins = all keyValid $ M.keys coins
  where
    keyValid k = let coin = fromJust $ M.lookup k coins
                 in getColor coin == k

-- | Given a empty list of coin maps (map
mergeCoinsMaps :: [M.Map Color Coin] -> M.Map Color Coin
mergeCoinsMaps [] = M.empty
mergeCoinsMaps coinMaps =
    assert (all coinsMapConsistent coinMaps) $
    foldr1 (M.unionWith (+)) coinMaps
