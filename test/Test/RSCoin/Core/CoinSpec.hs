{-# LANGUAGE ViewPatterns #-}

-- | RSCoin.Core.Coin specification

module Test.RSCoin.Core.CoinSpec
       ( spec
       ) where

import qualified Data.Map.Strict            as M (mapWithKey, member, size, (!))
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (NonEmptyList (..))

import qualified RSCoin.Core                as C

import           Test.RSCoin.Core.Arbitrary ()

spec :: Spec
spec =
    describe "Coin" $ do
        describe "sameColor" $ do
            prop "returns true iff coins have same color" sameColorCorrect
        describe "sumCoin" $ do
            prop description_sumCoinReturnsSum sumCoinReturnsSum
        describe "coinsToMap" $ do
            prop description_coinsToMapTest verifyStructureCoinsListToMap
        describe "addCoinsMap" $ do
            prop description_coinsMapSum verifyCoinsMapSum
        describe "subtractCoinsMap" $ do
            prop description_coinsMapSubtract verifyCoinsMapSubtract
  where
    description_sumCoinReturnsSum = "given non-empty list of coins with " ++
      "the same color, returns coin with the same color and value equal to " ++
      "sum of values"
    description_coinsToMapTest =
      "given list of coins with the color, returns map with one element " ++
      "(key is this color, value is sum of coins)"
    description_coinsMapSum =
      "for each color in the first map, if there exists this color in " ++
      "the second map, then value in the first map is increased by " ++
      "corresponding value from the second map."
    description_coinsMapSubtract =
      "for each color in the first map, if there exists this color in " ++
      "the second map, then value in the first map is decreased by " ++
      "corresponding value from the second map."

sameColorCorrect :: C.Coin -> C.Coin -> Bool
sameColorCorrect c1 c2 = (C.getColor c1 == C.getColor c2) == C.sameColor c1 c2

sumCoinReturnsSum :: C.Color -> NonEmptyList Rational -> Bool
sumCoinReturnsSum color (getNonEmpty -> values) =
    let coins = map (C.Coin color) values
        s = C.sumCoin coins
        expected = C.Coin color $ sum values
    in s == expected

-- | This property does the following:
-- * generate list of coins, giving them all the same color;
-- * transform this list into a CoinMap cmap;
-- * check that cmap has size one (only one color as a key).

verifyStructureCoinsListToMap :: [C.Coin] -> Bool
verifyStructureCoinsListToMap coins =
    let coins' = filter ((/=0) . C.getCoin) coins
        col = C.getColor $ head coins'
        sameCol = map (\(C.Coin _ c) ->
                       C.Coin col c) coins'
        cMap = C.coinsToMap sameCol
    in if null coins'
           then True
           else M.size cMap == 1

-- | These properties do the following:
-- * generate two CoinsMap m1 and m2
-- * for every color in the map:
-- * give its corresponding coin that color (so the operations are safe);
-- * perform addCoinsMap (subtractCoinsMap, respectively) on these two maps;
-- * for every color in this third map:
-- * verify it's the sum (subtraction, respectively) of
-- * the corresponding values in m1 and m2.

verifyCoinsMapSum :: C.CoinsMap -> C.CoinsMap -> Bool
verifyCoinsMapSum m1 m2 = coinsMapOperation (+) C.addCoinsMap m1 m2

verifyCoinsMapSubtract :: C.CoinsMap -> C.CoinsMap -> Bool
verifyCoinsMapSubtract m1 m2 = coinsMapOperation (-) C.subtractCoinsMap m1 m2

coinsMapOperation :: (C.Coin -> C.Coin -> C.Coin)
                  -> (C.CoinsMap -> C.CoinsMap -> C.CoinsMap)
                  -> C.CoinsMap
                  -> C.CoinsMap
                  -> Bool
coinsMapOperation op mapFun mp1 mp2 =
    let f = M.mapWithKey (\col (C.Coin _ cn) ->
                                    C.Coin col cn)
        (m1,m2) = (f mp1, f mp2)
        opMap = mapFun m1 m2
        step col coin = if col `M.member` m2
                               then coin == (m1 M.! col) `op` (m2 M.! col)
                               else True
    in and $ M.mapWithKey step opMap
