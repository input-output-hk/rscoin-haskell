{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Wrap Storage into AcidState.

module RSCoin.Mintette.AcidState
       ( State
       , openState
       , closeState
       , CheckNotDoubleSpent (..)
       ) where

import           Control.Exception       (throw)
import           Control.Monad.Catch     (MonadThrow (throwM))
import           Data.Acid               (AcidState, Update, closeAcidState,
                                          makeAcidic, openLocalStateFrom)
import           Data.SafeCopy           (base, deriveSafeCopy)

import           RSCoin.Core             (AddrId, Signature, Transaction)

import qualified RSCoin.Mintette.Storage as MS

type State = AcidState MS.Storage

$(deriveSafeCopy 0 'base ''MS.Storage)

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp MS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState


instance MonadThrow (Update s) where
    throwM = throw

checkNotDoubleSpent :: Transaction
                    -> AddrId
                    -> Signature
                    -> Update MS.Storage Bool
checkNotDoubleSpent = MS.checkNotDoubleSpent

$(makeAcidic ''MS.Storage
             [ 'checkNotDoubleSpent
             ])
