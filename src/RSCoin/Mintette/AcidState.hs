{-# LANGUAGE TemplateHaskell #-}

-- | Wrap Storage into AcidState.

module RSCoin.Mintette.AcidState
       ( State
       , getUtxoPset
       , getBlocks
       , getLogs
       , getPeriodId
       , previousMintetteId
       , checkNotDoubleSpent
       , commitTx
       , finishPeriod
       , startPeriod
       , finishEpoch
       ) where

import           Control.Exception       (throw)
import           Control.Monad.Catch     (MonadThrow (throwM))
import           Data.Acid               (AcidState, Query, Update)
import           Data.SafeCopy           (base, deriveSafeCopy)

import           RSCoin.Core             (ActionLog, AddrId, Address,
                                          CheckConfirmation, CheckConfirmations,
                                          CommitAcknowledgment, LBlock,
                                          MintetteId, NewPeriodData, PeriodId,
                                          PeriodResult, Pset, SecretKey,
                                          Signature, Transaction, Utxo)

import qualified RSCoin.Mintette.Storage as MS

type State = AcidState MS.Storage

$(deriveSafeCopy 0 'base ''MS.Storage)

instance MonadThrow (Update s) where
    throwM = throw

getUtxoPset :: Query MS.Storage (Utxo,Pset)
getUtxoPset = MS.getUtxoPset

getBlocks :: PeriodId -> Query MS.Storage (Maybe [LBlock])
getBlocks = MS.getBlocks

getLogs :: PeriodId -> Query MS.Storage (Maybe ActionLog)
getLogs = MS.getLogs

getPeriodId :: Query MS.Storage PeriodId
getPeriodId = MS.getPeriodId

previousMintetteId :: Query MS.Storage (Maybe MintetteId)
previousMintetteId = MS.previousMintetteId

checkNotDoubleSpent
    :: SecretKey
    -> Transaction
    -> AddrId
    -> [(Address, Signature)]
    -> Update MS.Storage CheckConfirmation
checkNotDoubleSpent = MS.checkNotDoubleSpent

commitTx :: SecretKey
         -> Transaction
         -> CheckConfirmations
         -> Update MS.Storage CommitAcknowledgment
commitTx = MS.commitTx

finishPeriod :: SecretKey -> PeriodId -> Update MS.Storage PeriodResult
finishPeriod = MS.finishPeriod

startPeriod :: NewPeriodData -> Update MS.Storage ()
startPeriod = MS.startPeriod

finishEpoch :: SecretKey -> Update MS.Storage ()
finishEpoch = MS.finishEpoch
