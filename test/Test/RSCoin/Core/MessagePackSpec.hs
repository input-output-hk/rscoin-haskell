{-# LANGUAGE ScopedTypeVariables #-}

module Test.RSCoin.Core.MessagePackSpec
       ( spec
       ) where

import           Data.Int                   (Int64)
import           Data.Maybe                 (fromJust)
import           Data.MessagePack           (MessagePack (..), pack, unpack)
import qualified Data.Set                   as S
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen, scale,
                                             (===))

import qualified RSCoin.Core                as C
import           RSCoin.Mintette.Error      (MintetteError)
import           RSCoin.Notary.Error        (NotaryError)

import           Test.RSCoin.Core.Arbitrary ()

makeSmall :: Gen a -> Gen a
makeSmall = scale f
  where
    -- f = (round . (sqrt :: Double -> Double) . realToFrac . (`div` 3))
    f 0 = 0
    f 1 = 1
    f 2 = 2
    f 3 = 3
    f 4 = 3
    f n
      | n < 0 = n
      | otherwise =
          (round . (sqrt :: Double -> Double) . realToFrac . (`div` 3)) n

newtype SmallLBlock =
    SmallLBlock C.LBlock
    deriving (MessagePack,Show,Eq)

instance Arbitrary SmallLBlock where
    arbitrary = SmallLBlock <$> makeSmall arbitrary

newtype SmallHBlock =
    SmallHBlock C.HBlock
    deriving (MessagePack,Show,Eq)

instance Arbitrary SmallHBlock where
    arbitrary = SmallHBlock <$> makeSmall arbitrary

newtype SmallNewPeriodData =
    SmallNewPeriodData C.NewPeriodData
    deriving (MessagePack,Show,Eq)

instance Arbitrary SmallNewPeriodData where
    arbitrary = SmallNewPeriodData <$> makeSmall arbitrary

newtype SmallTransaction =
    SmallTransaction C.Transaction
    deriving (MessagePack,Show,Eq)

instance Arbitrary SmallTransaction where
    arbitrary = SmallTransaction <$> makeSmall arbitrary

spec :: Spec
spec =
    describe "MessagePack" $ do
        describe "Identity Properties" $ do
            prop "Int64" $
                \(a :: Int64) -> a === mid a
            prop "Integer" $
                \(a :: Integer) -> a === mid a
            prop "Rational" $
                \(a :: Rational) -> a === mid a
            prop "Either Int Int" $
                \(a :: Either Int Int) -> a === mid a
            prop "Either Int (Either Int Int)" $
                \(a :: Either Int (Either Int Int)) -> a === mid a
            prop "Either (Either Int Int) Int" $
                \(a :: Either (Either Int Int) Int) -> a === mid a
            prop "Either MintetteError CheckConfirmation" $
                \(a :: Either MintetteError C.CheckConfirmation) -> a === mid a
            prop "Coin" $
                \(a :: C.Coin) -> a === mid a
            prop "Signature" $
                \(a :: C.Signature Int) -> a === mid a
            prop "Address" $
                \(a :: C.Address) -> a === mid a
            prop "Mintette" $
                \(a :: C.Mintette) -> a === mid a
            prop "Hash" $
                \(a :: C.Hash Int) -> a === mid a
            prop "Explorer" $
                \(a :: C.Explorer) -> a === mid a
            {-prop "NewPeriodData" $
                \(a :: C.NewPeriodData) -> a === mid a-}
            prop "SmallNewPeriodData" $
                \(a :: SmallNewPeriodData) -> a === mid a
            {-prop "LBlock" $
                \(a :: C.LBlock) -> a === mid a-}
            prop "SmallLBlock" $
                \(a :: SmallLBlock) -> a === mid a
            {-prop "Transaction" $
                \(a :: C.Transaction) -> a === mid a-}
            prop "SmallTransaction" $
                \(a :: SmallTransaction) -> a === mid a
            prop "CheckConfirmation" $
                \(a :: C.CheckConfirmation) -> a === mid a
            prop "CommitAcknowledgment" $
                \(a :: C.CommitAcknowledgment) -> a === mid a
            {-prop "HBlock" $
                \(a :: C.HBlock) -> a === mid a-}
            prop "SmallHBlock" $
                \(a :: SmallHBlock) -> a === mid a
            prop "TxStrategy" $
                \(a :: C.TxStrategy) -> a === mid a
            prop "PartyAddress" $
                \(a :: C.PartyAddress) -> a === mid a
            prop "AllocationAddress" $
                \(a :: C.AllocationAddress) -> a === mid a
            prop "AllocationStrategy" $
                \(a :: C.AllocationStrategy) -> a === mid a
            prop "AllocationInfo" $
                \(a :: C.AllocationInfo) -> a === mid a
            prop "Set" $
                \(a :: S.Set Int) -> a === mid a
            prop "ActionLogEntry" $
                \(a :: C.ActionLogEntry) -> a === mid a
            prop "MintetteError" $
                \(a :: MintetteError) -> a === mid a
            prop "NotaryError" $
                \(a :: NotaryError) -> a === mid a
            prop "BankLocalControlRequest" $
                \(a :: C.BankLocalControlRequest) -> a === mid a

mid :: MessagePack a => a -> a
mid = fromJust . unpack . pack
