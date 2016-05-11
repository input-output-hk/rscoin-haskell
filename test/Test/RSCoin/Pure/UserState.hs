{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeSynonymInstances      #-}

-- | WIP

module Test.RSCoin.Pure.UserState
       ( spec
       , Update
       , UpdateVoid
       , UserState (..)
       , userStorage
       , userKey
       ) where

import           Control.Lens               (makeLenses)

import           Test.Hspec                 (Spec, describe)
import           Test.QuickCheck            (Arbitrary (arbitrary), Gen,
                                             frequency)

import qualified RSCoin.Core                as C
import qualified RSCoin.User.Error          as S
import qualified RSCoin.User.Wallet         as S

import           Test.RSCoin.Core.Arbitrary ()
import qualified Test.RSCoin.Core.Storage   as T

spec :: Spec
spec =
    describe "User storage" $ do
        return ()

type Update = T.Update S.UserError S.WalletStorage
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

instance Arbitrary SomeUpdate where
    arbitrary =
        frequency
            [ (1, SomeUpdate <$> (arbitrary :: Gen EmptyUpdate))
            ]

data UserState = UserState
    { _userStorage :: S.WalletStorage
    , _userKey     :: C.SecretKey
    }

$(makeLenses ''UserState)

instance Show UserState where
  show = const "UserState"

instance Arbitrary UserState where
    arbitrary = do
        sk <- arbitrary
        SomeUpdate upd <- arbitrary
        return . flip UserState sk $ T.execUpdate (doUpdate upd) S.emptyWalletStorage
