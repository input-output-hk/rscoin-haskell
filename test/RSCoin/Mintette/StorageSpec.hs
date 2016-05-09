{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- | HSpec specification of Mintette's Storage.

module RSCoin.Mintette.StorageSpec
       ( spec
       , Update
       , UpdateVoid
       , MintetteState (..)
       , mintetteStorage
       , mintetteKey
       ) where

import           Control.Lens              (makeLenses)

import           Control.Monad             (void)
import           Test.Hspec                (Spec, describe)
import           Test.QuickCheck           (Arbitrary (arbitrary), Gen, frequency)

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

data FinishPeriod = FinishPeriod C.SecretKey C.PeriodId
    deriving Show

instance Arbitrary FinishPeriod where
    arbitrary = FinishPeriod <$> arbitrary <*> arbitrary

instance CanUpdate FinishPeriod where
    doUpdate (FinishPeriod sk pId) = void $ S.finishPeriod sk pId

-- data StartPeriod = StartPeriod C.NewPeriodData
--     deriving Show
-- 
-- instance CanUpdate StartPeriod where
--     doUpdate (StartPeriod pd) = void $ S.startPeriod pd
-- 
instance Arbitrary SomeUpdate where
    arbitrary =
        frequency
            [ (1, SomeUpdate <$> (arbitrary :: Gen EmptyUpdate))
            , (10, SomeUpdate <$> (arbitrary :: Gen FinishPeriod))
--            , (10, SomeUpdate <$> (arbitrary :: Gen StartPeriod))
            ]

data MintetteState = MintetteState
    { _mintetteStorage :: S.Storage
    , _mintetteKey     :: C.SecretKey
    }

$(makeLenses ''MintetteState)

instance Show MintetteState where
  show = const "MintetteState"

instance Arbitrary MintetteState where
    arbitrary = do
        sk <- arbitrary
        SomeUpdate upd <- arbitrary
        return . flip MintetteState sk $ T.execUpdate (doUpdate upd) S.mkStorage
