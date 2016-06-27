{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.MessagePackSpec
       ( spec
       ) where

import           Data.Int                   (Int64)
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
            prop "Int64" $
                \(a :: Int64) -> a === mid a
            prop "Address" $
                \(a :: C.Address) -> a === mid a
            prop "NewPeriodData" $
                \(a :: C.NewPeriodData) -> a === mid a
            prop "LBlock" $
                \(a :: C.LBlock) -> a === mid a
            prop "HBlock" $
                \(a :: C.HBlock) -> a === mid a
            prop "Transaction" $
                \(a :: C.Transaction) -> a === mid a
            prop "CheckConfirmation" $
                \(a :: C.CheckConfirmation) -> a === mid a
            prop "ActionLogEntry" $
                \(a :: C.ActionLogEntry) -> a === mid a

mid :: MessagePack a => a -> a
mid = fromJust . unpack . pack
