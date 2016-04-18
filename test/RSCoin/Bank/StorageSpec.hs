{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- | HSpec specification of Bank's Storage.

module RSCoin.Bank.StorageSpec
       ( spec
       , Update
       , UpdateVoid
       , StorageAndKey
       , getStorageAndKey
       ) where

import           Control.Lens               (makeLenses)

import           Control.Monad              (void)
import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen, frequency)

import qualified RSCoin.Bank.Error     as S
import qualified RSCoin.Bank.Storage   as S
import qualified RSCoin.Core           as C

import           RSCoin.Core.Arbitrary ()
import qualified RSCoin.Core.Storage   as T

spec :: Spec
spec =
    describe "Bank storage" $ do
    describe "startNewPeriod" $ do
        return ()
            -- prop "Increments periodId" startNewPeriodIncrementsPeriodId

type Update = T.Update S.BankError S.Storage
type UpdateVoid = Update ()

class CanUpdate a where
    doUpdate :: a -> UpdateVoid

data SomeUpdate = forall a . CanUpdate a => SomeUpdate a

data EmptyUpdate = EmptyUpdate
    deriving Show

instance Arbitrary EmptyUpdate where
    arbitrary = pure EmptyUpdate

instance CanUpdate EmptyUpdate where
    doUpdate _ = return ()

data AddMintette = AddMintette C.Mintette C.PublicKey

instance Arbitrary AddMintette where
  arbitrary = AddMintette <$> arbitrary <*> arbitrary

instance CanUpdate AddMintette where
    doUpdate (AddMintette m k) = S.addMintette m k

instance Arbitrary SomeUpdate where
    arbitrary =
        frequency
            [ (1, SomeUpdate <$> (arbitrary :: Gen EmptyUpdate))
            , (10, SomeUpdate <$> (arbitrary :: Gen AddMintette))]

newtype StorageAndKey = StorageAndKey
    { _getStorageAndKey :: (S.Storage, C.SecretKey)
    }

$(makeLenses ''StorageAndKey)

instance Show StorageAndKey where
  show = const "Bank StorageAndKey"

instance Arbitrary StorageAndKey where
    arbitrary = do
        sk <- arbitrary
        SomeUpdate upd <- arbitrary
        return . StorageAndKey . (, sk) $ T.execUpdate (doUpdate upd) S.mkStorage

startNewPeriodIncrementsPeriodId :: StorageAndKey -> Bool
startNewPeriodIncrementsPeriodId = undefined
