{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}

-- | Wrap Storage into AcidState.

module RSCoin.Mintette.AcidState
       ( State
       , query
       , update

       , ConvertUpdateInEnv (..)

       , getLastLBlocks
       , getLogs
       , getPeriodId
       , getStorage
       , getUtxoPset
       , getPreviousMintetteId

       , applyExtraAddresses
       , applyExtraUtxo
       , checkNotDoubleSpent
       , commitTx
       , finishEpoch
       , finishPeriod
       , startPeriod
       ) where

import           Control.Monad.Reader    (ReaderT, ask, runReaderT)
import           Control.Monad.Trans     (MonadIO)
import           Data.Acid               (EventResult, EventState, Query, QueryEvent,
                                          Update, UpdateEvent)

import           Serokell.AcidState      (ExtendedState, queryExtended, updateExtended)

import           RSCoin.Core             (ActionLog, AddrId, Address,
                                          AddressToTxStrategyMap, CheckConfirmation,
                                          CheckConfirmations, CommitAcknowledgment,
                                          LBlock, MintetteId, NewPeriodData, PeriodId,
                                          PeriodResult, Pset, Signature, Transaction,
                                          Utxo)

import           RSCoin.Mintette.Env     (RuntimeEnv)
import qualified RSCoin.Mintette.Storage as MS

type State = ExtendedState MS.Storage

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

getLastLBlocks :: Query MS.Storage [LBlock]
getLastLBlocks = MS.getLastLBlocks

getLogs :: PeriodId -> Query MS.Storage (Maybe ActionLog)
getLogs = MS.getLogs

getPeriodId :: Query MS.Storage PeriodId
getPeriodId = MS.getPeriodId

getStorage :: Query MS.Storage MS.Storage
getStorage = ask

getPreviousMintetteId :: Query MS.Storage (Maybe MintetteId)
getPreviousMintetteId = MS.getPrevMintetteId

class ConvertUpdateInEnv a b  | b -> a where
    convertUpdateInEnv :: a -> b

instance ConvertUpdateInEnv (ReaderT env (Update storage) a) (env -> Update storage a) where
    convertUpdateInEnv = runReaderT

instance ConvertUpdateInEnv (arg1 -> ReaderT env (Update storage) a) (arg1 -> env -> Update storage a) where
    convertUpdateInEnv f = runReaderT . f

instance ConvertUpdateInEnv (arg1 -> arg2 -> ReaderT env (Update storage) a) (arg1 -> arg2 -> env -> Update storage a) where
    convertUpdateInEnv f a = runReaderT . f a

instance ConvertUpdateInEnv (arg1 -> arg2 -> arg3 -> ReaderT env (Update storage) a) (arg1 -> arg2 -> arg3 -> env -> Update storage a) where
    convertUpdateInEnv f a b = runReaderT . f a b

instance ConvertUpdateInEnv (arg1 -> arg2 -> arg3 -> arg4 -> ReaderT env (Update storage) a) (arg1 -> arg2 -> arg3 -> arg4 -> env -> Update storage a) where
    convertUpdateInEnv f a b c = runReaderT . f a b c

checkNotDoubleSpent
    :: Transaction
    -> AddrId
    -> [(Address, Signature Transaction)]
    -> RuntimeEnv
    -> Update MS.Storage CheckConfirmation
checkNotDoubleSpent = convertUpdateInEnv MS.checkNotDoubleSpent

commitTx
    :: Transaction
    -> CheckConfirmations
    -> RuntimeEnv
    -> Update MS.Storage CommitAcknowledgment
commitTx = convertUpdateInEnv MS.commitTx

finishPeriod :: Int -> Int -> PeriodId -> RuntimeEnv -> Update MS.Storage PeriodResult
finishPeriod = convertUpdateInEnv MS.finishPeriod

startPeriod :: NewPeriodData -> RuntimeEnv -> Update MS.Storage ()
startPeriod = convertUpdateInEnv MS.startPeriod

finishEpoch :: RuntimeEnv -> Update MS.Storage ()
finishEpoch = convertUpdateInEnv MS.finishEpoch

applyExtraUtxo :: Utxo -> Update MS.Storage ()
applyExtraUtxo = MS.applyExtraUtxo

applyExtraAddresses :: AddressToTxStrategyMap -> Update MS.Storage ()
applyExtraAddresses = MS.applyExtraAddresses
