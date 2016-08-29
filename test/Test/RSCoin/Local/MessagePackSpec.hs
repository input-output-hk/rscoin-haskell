{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Local.MessagePackSpec
       ( spec
       ) where

import           Data.Maybe                  (fromJust)
import           Data.MessagePack            (MessagePack (..), pack, unpack)
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Arbitrary (arbitrary), Gen, scale,
                                              (===))

import qualified RSCoin.Core                 as C
import           RSCoin.Mintette.Error       (MintetteError)
import           RSCoin.Notary.Error         (NotaryError)

import           Test.RSCoin.Local.Arbitrary ()

spec :: Spec
spec =
    describe "MessagePack" $
        describe "Identity Properties" $ do
            prop "Either MintetteError CheckConfirmation" $
                \(a :: Either MintetteError C.CheckConfirmation) -> a === mid a
            prop "MintetteError" $
                \(a :: MintetteError) -> a === mid a
            prop "NotaryError" $
                \(a :: NotaryError) -> a === mid a

mid :: MessagePack a => a -> a
mid = fromJust . unpack . pack
