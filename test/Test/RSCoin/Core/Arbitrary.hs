{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Arbitrary instances for Core types.

module Test.RSCoin.Core.Arbitrary
       (
       ) where

import qualified Data.Map              as M
import qualified Data.Set              as S
import qualified Data.Data             as D
import           Data.List             (intersect)
import           Data.Text             (Text, pack)
import           Test.QuickCheck       (Arbitrary (arbitrary), Gen, NonNegative (..),
                                        choose, oneof)

import qualified RSCoin.Core           as C
import           RSCoin.Mintette.Error (MintetteError (..))
import           RSCoin.Notary.Error   (NotaryError (..))

instance Arbitrary C.Coin where
    arbitrary = do
        col <- arbitrary
        NonNegative coin <- arbitrary
        return $ C.Coin col coin

instance Arbitrary C.Mintette where
    arbitrary = C.Mintette <$> arbitrary <*> arbitrary

instance Arbitrary C.Explorer where
    arbitrary = C.Explorer <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary C.Hash where
    arbitrary = (C.hash :: C.Mintette -> C.Hash) <$> arbitrary

instance Arbitrary C.Address where
    arbitrary = C.Address <$> arbitrary

instance Arbitrary C.Transaction where
    arbitrary = C.Transaction <$> arbitrary <*> arbitrary

instance Arbitrary C.LBlock where
    arbitrary =
        C.LBlock <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary C.HBlock where
    arbitrary =
        C.HBlock <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
        pure M.empty

instance Arbitrary C.CheckConfirmation where
    arbitrary =
        C.CheckConfirmation <$> arbitrary <*> arbitrary <*> arbitrary <*>
        (abs <$> arbitrary)

instance Arbitrary C.CommitAcknowledgment where
    arbitrary = C.CommitAcknowledgment <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary C.Signature where
    arbitrary = C.sign <$> arbitrary <*> (arbitrary :: Gen String)

instance Arbitrary C.TxStrategy where
    arbitrary = oneof [ pure C.DefaultStrategy
                      , uncurry C.MOfNStrategy <$> gen']
      where gen' = do ls <- arbitrary
                      flip (,) ls <$> choose (1, length ls)

instance Arbitrary C.NewPeriodData where
    arbitrary =
        C.NewPeriodData
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary
        <*> arbitrary

instance Arbitrary C.AllocationAddress where
    arbitrary = oneof [ C.TrustAlloc <$> arbitrary
                      , C.UserAlloc <$> arbitrary
                      ]

instance Arbitrary C.PartyAddress where
    arbitrary = oneof [ C.TrustParty <$> arbitrary <*> arbitrary
                      , C.UserParty <$> arbitrary
                      ]

instance Arbitrary C.AllocationStrategy where
    arbitrary = C.AllocationStrategy <$> arbitrary <*> arbitrary

instance Arbitrary C.ActionLogEntry where
    arbitrary = oneof [ C.QueryEntry <$> arbitrary
                      , C.CommitEntry <$> arbitrary <*> arbitrary
                      , C.CloseEpochEntry <$> arbitrary
                      ]

{-instance Arbitrary [(C.Color, C.Coin)] where
    arbitrary = do
        list <- arbitrary :: Gen [(C.Color, NonNegative Rational)]
        return $ map (\(col,NonNegative rt) -> (col, C.Coin col rt)) list

instance Arbitrary C.CoinsMap where
    arbitrary = do
        list <- arbitrary
        return $ M.fromListWith (+) list

--this instance isn't working at the moment, causes an error.
-}


{- Section for errors. Created after some crazy ResultMismatchError bug. -}
instance Arbitrary Text where
    arbitrary = pack <$> arbitrary

-- @TODO: these instances are not typesafe enough
instance Arbitrary MintetteError where
    arbitrary = do
        let list = [ D.toConstr $ MEInternal undefined
                   , D.toConstr MEInactive
                   , D.toConstr $ MEPeriodMismatch undefined undefined
                   , D.toConstr MEInvalidTxSums
                   , D.toConstr $ MEInconsistentRequest undefined
                   , D.toConstr $ MENotUnspent undefined 
                   , D.toConstr MEInvalidSignature
                   , D.toConstr MENotConfirmed
                   , D.toConstr MEAlreadyActive
                   ]
            consList = D.dataTypeConstrs $ D.dataTypeOf (undefined :: MintetteError)
        if (length $ intersect list consList) < (length consList)
            then error "Missing constructors in MintetteError"
            else helper

helper :: Gen MintetteError
helper = oneof [      MEInternal            <$> arbitrary
               , pure MEInactive
               ,      MEPeriodMismatch      <$> arbitrary <*> arbitrary
               , pure MEInvalidTxSums
               ,      MEInconsistentRequest <$> arbitrary
               ,      MENotUnspent          <$> arbitrary
               , pure MEInvalidSignature
               , pure MENotConfirmed
               , pure MEAlreadyActive
               ]                 
        
instance Arbitrary NotaryError where
    arbitrary = oneof [ pure NEAddrNotRelativeToTx
                      ,      NEAddrIdNotInUtxo <$> arbitrary
                      , pure NEBlocked
                      ,      NEInvalidArguments <$> arbitrary
                      ,      NEInvalidChain <$> arbitrary
                      , pure NEInvalidSignature
                      ,      NEStrategyNotSupported <$> arbitrary
                      ,      NEUnrelatedSignature <$> arbitrary
                      ]
