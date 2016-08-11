{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.BinarySpec
       ( spec
       ) where

import           Data.Binary                (Binary, decode, encode)
import qualified Data.Set                   as S
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            ((===))

import qualified RSCoin.Core                as C

import           Test.RSCoin.Core.Arbitrary ()

spec :: Spec
spec =
    describe "Binary" $ do
        describe "Identity Properties" $ do
            prop "Integer" $
                \(a :: Integer) -> a === binMid a
            prop "Rational" $
                \(a :: Rational) -> a === binMid a
            prop "Either Int Int" $
                \(a :: Either Int Int) -> a === binMid a
            prop "Either Int (Either Int Int)" $
                \(a :: Either Int (Either Int Int)) -> a === binMid a
            prop "Either (Either Int Int) Int" $
                \(a :: Either (Either Int Int) Int) -> a === binMid a
            {-prop "Either MintetteError CheckConfirmation" $
                \(a :: Either MintetteError C.CheckConfirmation) -> a === binMid a-}
            prop "Coin" $
                \(a :: C.Coin) -> a === binMid a
            prop "Signature" $
                \(a :: C.Signature) -> a === binMid a
            prop "Address" $
                \(a :: C.Address) -> a === binMid a
            prop "Mintette" $
                \(a :: C.Mintette) -> a === binMid a
            prop "Hash" $
                \(a :: C.Hash) -> a === binMid a
            prop "Explorer" $
                \(a :: C.Explorer) -> a === binMid a
            {-prop "SmallNewPeriodData" $
                \(a :: SmallNewPeriodData) -> a === binMid a-}
            {-prop "SmallLBlock" $
                \(a :: SmallLBlock) -> a === binMid a-}
            prop "Transaction" $
                \(a :: C.Transaction) -> a === binMid a
            {-prop "SmallTransaction" $
                \(a :: SmallTransaction) -> a === binMid a-}
            prop "CheckConfirmation" $
                \(a :: C.CheckConfirmation) -> a === binMid a
            prop "CommitAcknowledgment" $
                \(a :: C.CommitAcknowledgment) -> a === binMid a
            prop "HBlock" $
                \(a :: C.HBlock) -> a === binMid a
            {-prop "SmallHBlock" $
                \(a :: SmallHBlock) -> a === binMid a-}
            prop "TxStrategy" $
                \(a :: C.TxStrategy) -> a === binMid a
            prop "PartyAddress" $
                \(a :: C.PartyAddress) -> a === binMid a
            prop "AllocationAddress" $
                \(a :: C.AllocationAddress) -> a === binMid a
            prop "AllocationStrategy" $
                \(a :: C.AllocationStrategy) -> a === binMid a
            prop "Set" $
                \(a :: S.Set Int) -> a === binMid a
            prop "ActionLogEntry" $
                \(a :: C.ActionLogEntry) -> a === binMid a

binMid :: Binary a => a -> a
binMid = decode . encode
