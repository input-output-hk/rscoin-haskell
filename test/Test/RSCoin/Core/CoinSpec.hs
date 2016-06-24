{-# LANGUAGE ViewPatterns #-}

-- | RSCoin.Core.Coin specification

module Test.RSCoin.Core.CoinSpec
       ( spec
       ) where

import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (NonEmptyList (..))
import qualified Data.Map.Strict            as M (size)

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
            prop description_coinsToMapTest coinsToMapTest
  where
    description_sumCoinReturnsSum = "given non-empty list of coins with " ++
      "the same color, returns coin with the same color and value equal to " ++
      "sum of values"
    description_coinsToMapTest =
        "returns true if for a list of arbitrary coins with only one color " ++
        "coinsToMap will return a map with a single color as its key."

sameColorCorrect :: C.Coin -> C.Coin -> Bool
sameColorCorrect c1 c2 = (C.getColor c1 == C.getColor c2) == C.sameColor c1 c2

sumCoinReturnsSum :: C.Color -> NonEmptyList Rational -> Bool
sumCoinReturnsSum color (getNonEmpty -> values) =
    let coins = map (C.Coin color) values
        s = C.sumCoin coins
        expected = C.Coin color $ sum values
    in s == expected

coinsToMapTest :: [C.Coin] -> Bool
coinsToMapTest coins =
    let coins' = filter ((/=0) . C.getCoin) coins
        col = C.getColor $ head coins'
        sameCol = map (\(C.Coin _ c) ->
                       C.Coin col c) coins'
        cMap = C.coinsToMap sameCol
    in if null coins'
           then True
           else M.size cMap == 1
