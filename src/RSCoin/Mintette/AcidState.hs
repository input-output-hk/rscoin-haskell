{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Wrap Storage into AcidState.

module RSCoin.Mintette.AcidState
       ( State (..)
       , query
       , toAcidState
       , update

       , checkNotDoubleSpent
       , commitTx
       , finishEpoch
       , finishPeriod
       , getBlocks
       , getLogs
       , getPeriodId
       , getUtxoPset
       , previousMintetteId
       , startPeriod
       ) where

import           Control.Exception       (throw)
import           Control.Monad.Catch     (MonadThrow (throwM))
import           Control.Monad.Trans     (MonadIO)
import           Data.Acid               (AcidState, EventResult, EventState,
                                          Query, QueryEvent, Update,
                                          UpdateEvent)
import           Data.Acid.Advanced      (query', update')
import           Data.SafeCopy           (base, deriveSafeCopy)

import           RSCoin.Core             (ActionLog, AddrId, Address,
                                          CheckConfirmation, CheckConfirmations,
                                          CommitAcknowledgment, LBlock,
                                          MintetteId, NewPeriodData, PeriodId,
                                          PeriodResult, Pset, SecretKey,
                                          Signature, Transaction, Utxo)

import qualified RSCoin.Mintette.Storage as MS

type AState = AcidState MS.Storage

data State
    = LocalState AState
                 FilePath
    | MemoryState AState

toAcidState :: State -> AState
toAcidState (LocalState st _) = st
toAcidState (MemoryState st)  = st

$(deriveSafeCopy 0 'base ''MS.Storage)

instance MonadThrow (Update s) where
    throwM = throw

query
    :: (EventState event ~ MS.Storage, QueryEvent event, MonadIO m)
    => State -> event -> m (EventResult event)
query st = query' (toAcidState st)

update
    :: (EventState event ~ MS.Storage, UpdateEvent event, MonadIO m)
    => State -> event -> m (EventResult event)
update st = update' (toAcidState st)

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
