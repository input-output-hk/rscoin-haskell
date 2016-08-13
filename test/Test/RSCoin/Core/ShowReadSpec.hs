{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.ShowReadSpec
       ( spec
       ) where

import qualified RSCoin.Core as C

import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            ((===))

import           RSCoin.Core                (SecretKey, Signature)

import           Test.RSCoin.Core.Arbitrary ()

spec :: Spec
spec =
    describe "ShowRead" $ do
        describe "Identity Properties" $ do
            prop "Signature" $
                \(a :: Signature) -> a === showMid a
            prop "SecretKey" $
                \(a :: SecretKey) -> a === showMid a

showMid :: (Show a, Read a) => a -> a
showMid = read . show
