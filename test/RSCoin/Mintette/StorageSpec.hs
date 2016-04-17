{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE ViewPatterns              #-}

-- | HSpec specification of Mintette's Storage.

module RSCoin.Mintette.StorageSpec
       ( spec
       ) where

import           Test.Hspec                 (Spec, describe)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen, frequency)

import qualified RSCoin.Mintette.Error     as S
import qualified RSCoin.Mintette.Storage   as S
import qualified RSCoin.Core               as C

import           RSCoin.Core.Arbitrary ()
import qualified RSCoin.Core.Storage       as T

spec :: Spec
spec =
    describe "Mintette storage" $ do
        return ()

type Update = T.Update S.MintetteError S.Storage
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

data CheckNotDoubleSpent = CheckNotDoubleSpent C.SecretKey C.Transaction C.AddrId C.Signature
    deriving Show

instance Arbitrary SomeUpdate where
    arbitrary =
        frequency
            [ (1, SomeUpdate <$> (arbitrary :: Gen EmptyUpdate))
            --, (10, SomeUpdate <$> (arbitrary :: Gen CheckNotDoubleSpent))
            ]

newtype StorageAndKey = StorageAndKey
    { getStorageAndKey :: (S.Storage, C.SecretKey)
    }

instance Show StorageAndKey where
  show = const "Mintette StorageAndKey"

instance Arbitrary StorageAndKey where
    arbitrary = do
        sk <- arbitrary
        SomeUpdate upd <- arbitrary
        return . StorageAndKey . (, sk) $ T.execUpdate (doUpdate upd) S.mkStorage
