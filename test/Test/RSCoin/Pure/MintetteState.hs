{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- | WIP

module Test.RSCoin.Pure.MintetteState
       ( Update
       , UpdateVoid
       , MintetteState (..)
       , mintetteStorage
       , mintetteKey
       ) where

import           Control.Lens               (makeLenses)

import           Control.Monad              (void)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen,
                                             frequency)

import qualified RSCoin.Core                as C
import qualified RSCoin.Mintette.Error      as S
import qualified RSCoin.Mintette.Storage    as S

import           Test.RSCoin.Core.Arbitrary ()
import qualified Test.RSCoin.Pure.Update    as T

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
