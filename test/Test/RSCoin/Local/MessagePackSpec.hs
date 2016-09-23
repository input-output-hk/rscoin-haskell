{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Local.MessagePackSpec
       ( spec
       ) where

import           Data.MessagePack            (MessagePack (..), pack, unpack)
import           Data.Proxy                  (Proxy (Proxy))
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Arbitrary, (===))

import qualified RSCoin.Core                 as C
import           RSCoin.Mintette.Error       (MintetteError)
import           RSCoin.Notary.Error         (NotaryError)

import           Test.RSCoin.Local.Arbitrary ()

spec :: Spec
spec =
    describe "MessagePack" $
        describe "Identity Properties" $ do
            makeMsgPackProp "Either MintetteError CheckConfirmation"
                (Proxy :: Proxy (Either MintetteError C.CheckConfirmation))
            makeMsgPackProp "MintetteError" (Proxy :: Proxy MintetteError)
            makeMsgPackProp "NotaryError" (Proxy :: Proxy NotaryError)

makeMsgPackProp
    :: forall a.
       (Show a, Eq a, MessagePack a, Arbitrary a)
    => String -> Proxy a -> Spec
makeMsgPackProp s Proxy = prop s $ \(x :: a) -> x === mid x

mid :: MessagePack a => a -> a
mid = maybe err id . unpack . pack
  where
    err = error "[MessagePackSpec] Failed MessagePack decoding"
