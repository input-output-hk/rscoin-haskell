{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.MessagePackSpec
       ( spec
       ) where

import           Data.Maybe                 (fromJust)
import           Data.MessagePack           (MessagePack (..),
                                             pack, unpack)
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            ((===))

import qualified RSCoin.Core                as C

import           Test.RSCoin.Core.Arbitrary ()

spec :: Spec
spec =
    describe "MessagePack" $ do
        describe "Identity Properties" $ do
            prop "Either Int Int" $
                \(a :: Either Int Int) -> a === mid a
            prop "Either Int (Either Int Int)" $
                \(a :: Either Int (Either Int Int)) -> a === mid a
            prop "Either (Either Int Int) Int" $
                \(a :: Either (Either Int Int) Int) -> a === mid a
            prop "Coin" $
                \(a :: C.Coin) -> a === mid a
            prop "Mintette" $
                \(a :: C.Mintette) -> a === mid a
            prop "Hash" $
                \(a :: C.Hash) -> a === mid a
            prop "Integer" $
                \(a :: Integer) -> a === mid a
            prop "Rational" $
                \(a :: Rational) -> a === mid a
            -- TODO: write the rest of Core.MessagePack instances

mid :: MessagePack a => a -> a
mid = fromJust . unpack . pack
