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

newtype TransactionValid = TransactionValid { getTr :: C.Transaction} deriving Show

instance Arbitrary TransactionValid where
    arbitrary = TransactionValid <$>
                do tid <- arbitrary
                   list <- arbitrary
                   let len = length list
                   cl <- arbitrary
                   colors <- vector (cl `mod` len)
                   let cList = colors ++ cList
                   return $
                       uncurry C.Transaction $
                       unzip $
                       map (helper tid) $
                       zip colors list
              where helper hash (col,(cn1,cn2,addr,ind)) = ((hash,ind,C.Coin col cn1),(addr, C.Coin col cn2))

spec :: Spec
spec =
    describe "Transaction" $ do
    prop
        "sum of inputs is less than sum of outputs"
        validateSum'

validateSum' = C.validateSum . getTr
