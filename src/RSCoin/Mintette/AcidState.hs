{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Wrap Storage into AcidState.

module RSCoin.Mintette.AcidState
       ( State
       , openState
       , closeState
       , CheckNotDoubleSpent (..)
       , FinishPeriod (..)
       , StartPeriod (..)
       ) where

import           Control.Exception       (throw)
import           Control.Monad.Catch     (MonadThrow (throwM))
import           Data.Acid               (AcidState, Update, closeAcidState,
                                          makeAcidic, openLocalStateFrom)
import           Data.SafeCopy           (base, deriveSafeCopy)

import           RSCoin.Core             (AddrId, CheckConfirmation,
                                          NewPeriodData, PeriodId, PeriodResult,
                                          SecretKey, Signature, Transaction)

import qualified RSCoin.Mintette.Storage as MS

type State = AcidState MS.Storage

$(deriveSafeCopy 0 'base ''MS.Storage)

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp MS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState


instance MonadThrow (Update s) where
    throwM = throw

checkNotDoubleSpent
    :: SecretKey
    -> Transaction
    -> AddrId
    -> Signature
    -> Update MS.Storage (Maybe CheckConfirmation)
checkNotDoubleSpent = MS.checkNotDoubleSpent

finishPeriod :: PeriodId -> Update MS.Storage PeriodResult
finishPeriod = MS.finishPeriod

startPeriod :: NewPeriodData -> Update MS.Storage ()
startPeriod = MS.startPeriod

$(makeAcidic ''MS.Storage
             [ 'checkNotDoubleSpent
             , 'finishPeriod
             , 'startPeriod
             ])
