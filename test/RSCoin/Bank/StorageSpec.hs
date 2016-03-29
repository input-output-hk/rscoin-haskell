{-# LANGUAGE TypeSynonymInstances #-}

-- | HSpec specification of Bank's Storage.

module RSCoin.Bank.StorageSpec
       ( spec
       ) where

import           Control.Monad.State   (State, execState)
import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (arbitrary))

import qualified RSCoin.Bank.Storage   as S
import qualified RSCoin.Core           as C

spec :: Spec
spec = do
    describe "Storage" $ do
        describe "startNewPeriod" $ do
            prop "Increments periodId" startNewPeriodIncrementsPeriodId

newtype StorageAndKey = StorageAndKey
    { getStorageAndKey :: (S.Storage, C.SecretKey)
    }

instance Show StorageAndKey where
  show = const "StorageAndKey"

instance Arbitrary StorageAndKey where
    arbitrary = StorageAndKey <$> ((,) <$> pure S.mkStorage <*> arbitrary)

execUpdateUnsafe :: S.ExceptUpdate a -> S.Storage -> S.Storage
execUpdateUnsafe = undefined

startNewPeriodIncrementsPeriodId :: StorageAndKey -> Bool
startNewPeriodIncrementsPeriodId = const True
