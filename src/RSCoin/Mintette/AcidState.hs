{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Wrap Storage into AcidState.

module RSCoin.Mintette.AcidState
       ( State
       , query
       , update

       , getBlocks
       , getLogs
       , getPeriodId
       , getStorage
       , getUtxoPset
       , previousMintetteId

       , checkNotDoubleSpent
       , commitTx
       , finishEpoch
       , finishPeriod
       , startPeriod
       ) where

import           Control.Monad.Reader    (ask)
import           Control.Monad.Trans     (MonadIO)
import           Data.Acid               (EventResult, EventState, Query,
                                          QueryEvent, Update, UpdateEvent)
import           Data.SafeCopy           (base, deriveSafeCopy)

import           Serokell.AcidState      (ExtendedState, queryExtended,
                                          updateExtended)

import           RSCoin.Core             (ActionLog, AddrId, Address,
                                          CheckConfirmation, CheckConfirmations,
                                          CommitAcknowledgment, LBlock,
                                          MintetteId, NewPeriodData, PeriodId,
                                          PeriodResult, Pset, SecretKey,
                                          Signature, Transaction, Utxo)

import qualified RSCoin.Mintette.Storage as MS

type State = ExtendedState MS.Storage

$(deriveSafeCopy 0 'base ''MS.Storage)

query
    :: (EventState event ~ MS.Storage, QueryEvent event, MonadIO m)
    => State -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ MS.Storage, UpdateEvent event, MonadIO m)
    => State -> event -> m (EventResult event)
update = updateExtended

getUtxoPset :: Query MS.Storage (Utxo,Pset)
getUtxoPset = MS.getUtxoPset

getBlocks :: PeriodId -> Query MS.Storage (Maybe [LBlock])
getBlocks = MS.getBlocks

getLogs :: PeriodId -> Query MS.Storage (Maybe ActionLog)
getLogs = MS.getLogs

getPeriodId :: Query MS.Storage PeriodId
getPeriodId = MS.getPeriodId

getStorage :: Query MS.Storage MS.Storage
getStorage = ask

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
