{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- | HSpec specification of Bank's Storage.

module RSCoin.Bank.StorageSpec
       ( spec
       ) where

-- import           Control.Exception     (throw)
-- import           Control.Monad         (void)
-- import           Control.Monad.Except  (runExceptT)
-- import           Control.Monad.State   (runState)
-- import           Control.Monad.Trans   (lift)
import           Test.Hspec (Spec, describe)
-- import           Test.Hspec.QuickCheck (prop)
-- import           Test.QuickCheck       (Arbitrary (arbitrary), Gen, frequency)

-- import qualified RSCoin.Bank.Storage   as S
-- import qualified RSCoin.Core           as C

-- import           RSCoin.Core.Arbitrary ()

spec :: Spec
spec =
    describe "Storage" $ do
    describe "startNewPeriod" $ do
        return ()
            -- prop "Increments periodId" startNewPeriodIncrementsPeriodId

-- type Update = S.ExceptUpdate
-- type UpdateVoid = Update ()

-- class CanUpdate a where
--     doUpdate :: a -> UpdateVoid

-- instance CanUpdate (Update a) where
--     doUpdate = void

-- data SomeUpdate = forall a . CanUpdate a => SomeUpdate a

-- data EmptyUpdate = EmptyUpdate

-- instance Arbitrary EmptyUpdate where
--   arbitrary = pure EmptyUpdate

-- instance CanUpdate EmptyUpdate where
--     doUpdate _ = return ()

-- data AddMintette = AddMintette C.Mintette C.PublicKey

-- instance Arbitrary AddMintette where
--   arbitrary = AddMintette <$> arbitrary <*> arbitrary

-- instance CanUpdate AddMintette where
--     doUpdate (AddMintette m k) = lift $ S.addMintette m k

-- instance Arbitrary SomeUpdate where
--     arbitrary =
--         frequency
--             [ (1, SomeUpdate <$> (arbitrary :: Gen EmptyUpdate))
--             , (10, SomeUpdate <$> (arbitrary :: Gen AddMintette))]

-- newtype StorageAndKey = StorageAndKey
--     { getStorageAndKey :: (S.Storage, C.SecretKey)
--     }

-- instance Show StorageAndKey where
--   show = const "StorageAndKey"

-- instance Arbitrary StorageAndKey where
--     arbitrary = do
--         sk <- arbitrary
--         SomeUpdate upd <- arbitrary
--         return . StorageAndKey . (, sk) $ execUpdate (doUpdate upd) S.mkStorage

-- execUpdate :: Update a -> S.Storage -> S.Storage
-- execUpdate u = snd . runUpdate u

-- runUpdate :: Update a -> S.Storage -> (a, S.Storage)
-- runUpdate upd storage = either throw (, newStorage) res
--   where
--     (res, newStorage) = runState (runExceptT upd) storage

-- startNewPeriodIncrementsPeriodId :: StorageAndKey -> Bool
-- startNewPeriodIncrementsPeriodId = const True
