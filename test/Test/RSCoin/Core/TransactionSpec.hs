-- | RSCoin.Core.Transaction specification

module Test.RSCoin.Core.TransactionSpec
       ( spec
       ) where

import           Test.Hspec                     (Spec, describe)
import           Test.Hspec.QuickCheck          (prop)
import           Test.QuickCheck                (Arbitrary (arbitrary), vector)
import           Data.Tuple.Select              (sel3)
import           RSCoin.Core.Primitives         (Transaction (..), Coin (..))
import           Data.List                      (sort, nub)
import qualified RSCoin.Core as C               (validateSum, Coin (..), Color, Transaction (..))

import           Test.RSCoin.Core.Arbitrary     ()

newtype TransactionValid = TransactionValid { getTr :: C.Transaction} deriving Show

instance Arbitrary TransactionValid where
    arbitrary = TransactionValid <$>
                do trid <- arbitrary :: Gen C.Hash
                   NonEmpty inps <- arbitrary :: Gen (NonEmptyList (Int,C.Coin))
                   let (coins,inputs) = fork C.coinsToMap id $
                                        unzip $
                                        map (\(ind,coin) -> (coin,(trid,ind,coin))) inps
                       fork f g (x,y) = (f x, g y)
                       fun coin = do v_c <- arbitrary :: Gen Rational
                                     addr <- arbitrary :: Gen C.Address
                                     let cn = C.getCoin coin
                                     return $ (addr, if (C.getColor coin) == 0
                                                         then v_c `mod` cn
                                                         else mod ((C.getCoin $ coins M.! 0) - v_c) cn)
                   coinMap <- mapM fun coins
                   padCols <- arbitrary :: Gen [C.Color]
                   let v = snd $ coinMap M.! 0
                       l = length padCols
                       helper adr cl cn = (adr,C.Coin cl cn)
                   padAddrs <- vector l :: Gen [C.Address]
                   return $ C.Transaction inputs $ M.elems coinMap ++
                       if l == 0
                            then []
                            else zipWith3 helper padAddrs padCols (repeat (v / fromIntegral l))

spec :: Spec
spec =
    describe "Transaction" $ do
    prop
        "sum of inputs is less than sum of outputs"
        validateSum'

validateSum' = C.validateSum . getTr
