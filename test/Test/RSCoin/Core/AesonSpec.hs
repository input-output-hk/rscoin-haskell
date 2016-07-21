{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.AesonSpec
       ( spec
       ) where

import           Data.Aeson                 (ToJSON, FromJSON,
                                             encode, decode)
import           Data.Maybe                 (fromJust)
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            ((===))

import qualified RSCoin.Core                as C

import           Test.RSCoin.Core.Arbitrary ()

spec :: Spec
spec =
    describe "Aeson" $ do
        describe "Identity Properties" $ do
            {-prop "Coin" $
                \(a :: C.Coin) -> a === aesonMid a-}
            prop "Signature" $
                \(a :: C.Signature) -> a === aesonMid a
            prop "Address" $
                \(a :: C.Address) -> a === aesonMid a
            prop "Hash" $
                \(a :: C.Hash) -> a === aesonMid a
            {-prop "SmallHBlock" $
                \(a :: SmallHBlock) -> a === aesonMid a-}
            prop "PartyAddress" $
                \(a :: C.PartyAddress) -> a === aesonMid a
            prop "AllocationAddress" $
                \(a :: C.AllocationAddress) -> a === aesonMid a
            prop "AllocationStrategy" $
                \(a :: C.AllocationStrategy) -> a === aesonMid a

aesonMid :: (ToJSON a, FromJSON a) => a -> a
aesonMid = fromJust . decode . encode
