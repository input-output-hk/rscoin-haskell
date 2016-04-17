{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-- | HSpec specification of Bank's Storage.

module RSCoin.Bank.StorageSpec
       ( spec
       ) where

import           Control.Exception          (throw, fromException)
import           Control.Monad              (void)
import           Control.Monad.Catch        (MonadThrow (throwM), SomeException (..))
import           Control.Monad.Except       (runExceptT)
import           Control.Monad.State        (runState, State)
import           Control.Monad.State.Class  (MonadState)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except (ExceptT, throwE)
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
    { getStorageAndKey :: (S.Storage, C.SecretKey)
    }

instance Show StorageAndKey where
  show = const "StorageAndKey"

instance Arbitrary StorageAndKey where
    arbitrary = do
        sk <- arbitrary
        SomeUpdate upd <- arbitrary
        return . StorageAndKey . (, sk) $ T.execUpdate (doUpdate upd) S.mkStorage

startNewPeriodIncrementsPeriodId :: StorageAndKey -> Bool
startNewPeriodIncrementsPeriodId (getStorageAndKey -> (st, sk)) =
    undefined
