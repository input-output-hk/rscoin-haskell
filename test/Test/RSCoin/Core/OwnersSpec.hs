-- | HSpec specification for `Owners` module

module Test.RSCoin.Core.OwnersSpec
       ( spec
       ) where

import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)

import qualified RSCoin.Core                as C
import           Test.RSCoin.Core.Arbitrary ()

spec :: Spec
spec =
    describe "Owners" $
    describe "owners" $ do
    prop "tx has at least one owner if mintettes list is not empty"
        hasOwnersIffNotEmpty

hasOwnersIffNotEmpty :: C.Mintettes -> C.TransactionId -> Bool
hasOwnersIffNotEmpty mts tx = null (C.owners mts tx) == null mts
