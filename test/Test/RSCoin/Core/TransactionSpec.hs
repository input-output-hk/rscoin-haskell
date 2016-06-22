{-# LANGUAGE ScopedTypeVariables #-}

-- | RSCoin.Core.Transaction specification

module Test.RSCoin.Core.TransactionSpec
       ( spec
       ) where

import           Data.Bifunctor             (first, second)
import           Data.List                  (genericLength)
import qualified Data.Map.Strict            as M (elems, lookup, (!))
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen,
                                             NonEmptyList (..), choose, vector)

import qualified RSCoin.Core                as C

import           Test.RSCoin.Core.Arbitrary ()

newtype TransactionValid = TransactionValid
    { getTr :: C.Transaction
    } deriving (Show)

genRationalInRange :: Rational -> Rational -> Gen Rational
genRationalInRange lo hi =
    (toRational :: Double -> Rational) <$>
    choose (fromRational lo, fromRational hi)

genCoinInRange :: C.Color -> Rational -> Rational -> Gen C.Coin
genCoinInRange col lo hi = C.Coin col <$> genRationalInRange lo hi

instance Arbitrary TransactionValid where
    arbitrary =
        TransactionValid <$>
        do trid <- arbitrary :: Gen C.Hash
           inps :: [(Int, C.Coin)] <-
               map (second abs) . getNonEmpty <$> arbitrary
           let coins :: C.CoinsMap
               inputs :: [C.AddrId]
               (coins,inputs) =
                   first C.coinsToMap $
                   unzip $
                   map
                       (\(ind,coin) ->
                             (coin, (trid, ind, coin)))
                       inps
               genOutput C.Coin{..} =
                   (,) <$> arbitrary <*> genCoinInRange getColor 0 getCoin
           unpaintedOutputsMap <- mapM genOutput coins
           padCols <- arbitrary :: Gen [C.Color]
           let l
                   :: Num a
                   => a
               l = genericLength padCols
               helper adr cl cn = (adr, C.Coin cl cn)
           padAddrs <- vector l :: Gen [C.Address]
           C.Transaction inputs .
               (M.elems unpaintedOutputsMap ++) <$>
                 case M.lookup 0 unpaintedOutputsMap of
                     Nothing -> return []
                     Just (_,v) ->
                         if null padCols
                             then return []
                             else do let (outpv,inpv) = (C.getCoin v, C.getCoin $ coins M.! 0)
                                     v' <- genRationalInRange 0 (inpv-outpv)
                                     return $ zipWith3 helper padAddrs padCols (repeat (v' / l))

spec :: Spec
spec =
    describe "Transaction" $ do
        describe "validateSum" $ do
            prop description_validateSumForValid validateSumCorrectForValid
    where
      description_validateSumForValid =
        "returns true if total amount of grey coins in inputs is not less than " ++
        "amount of grey coins in outputs plus amount of coins spent to color coins"

validateSumCorrectForValid :: TransactionValid -> Bool
validateSumCorrectForValid = C.validateSum . getTr
