{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | AcidState boilerplate.

module RSCoin.Explorer.AcidState
       ( State
       , closeState
       , openState
       , openMemState
       , query
       , tidyState
       , update

       , GetAddressBalance (..)
       , GetAddressTransactions (..)
       , GetAddressTxNumber (..)
       , GetLastPeriodId (..)
       , GetTx (..)
       , GetTxSummary (..)
       , IsAddressKnown (..)

       , AddHBlock (..)
       ) where

import           Control.Monad.Trans       (MonadIO)
import           Data.Acid                 (EventResult, EventState, Query,
                                            QueryEvent, Update, UpdateEvent,
                                            makeAcidic)

import           Serokell.AcidState        (ExtendedState, closeExtendedState,
                                            openLocalExtendedState,
                                            openMemoryExtendedState,
                                            queryExtended, tidyExtendedState,
                                            updateExtended)
import           Serokell.Data.Variant     (Variant)

import qualified RSCoin.Core               as C

import qualified RSCoin.Explorer.Storage   as ES
import           RSCoin.Explorer.Summaries (CoinsMapSummary, TransactionSummary)

type State = ExtendedState ES.Storage

query
    :: (EventState event ~ ES.Storage, QueryEvent event, MonadIO m)
    => State -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ ES.Storage, UpdateEvent event, MonadIO m)
    => State -> event -> m (EventResult event)
update = updateExtended

openState :: MonadIO m => Bool -> FilePath -> m State
openState deleteIfExists fp = openLocalExtendedState deleteIfExists fp ES.mkStorage

openMemState :: MonadIO m => m State
openMemState = openMemoryExtendedState ES.mkStorage

closeState :: MonadIO m => State -> m ()
closeState = closeExtendedState

tidyState :: MonadIO m => State -> m ()
tidyState = tidyExtendedState

getAddressBalance :: C.Address -> Query ES.Storage (C.PeriodId, CoinsMapSummary)
getAddressBalance = ES.getAddressBalance

getAddressTxNumber :: C.Address -> Query ES.Storage (C.PeriodId, Word)
getAddressTxNumber = ES.getAddressTxNumber

getAddressTransactions
    :: C.Address
    -> (Word, Word)
    -> Query ES.Storage (C.PeriodId, [(Word, TransactionSummary)])
getAddressTransactions = ES.getAddressTransactions

getLastPeriodId :: Query ES.Storage (Maybe C.PeriodId)
getLastPeriodId = ES.getLastPeriodId

getTx :: C.TransactionId -> Query ES.Storage (Maybe C.Transaction)
getTx = ES.getTx

getTxSummary :: C.TransactionId -> Query ES.Storage (Maybe TransactionSummary)
getTxSummary = ES.getTxSummary

isAddressKnown :: C.Address -> Query ES.Storage Bool
isAddressKnown = ES.isAddressKnown

addHBlock :: C.PeriodId -> C.WithMetadata C.HBlock Variant -> Update ES.Storage ()
addHBlock = ES.addHBlock

$(makeAcidic ''ES.Storage
             [ 'getAddressBalance
             , 'getAddressTxNumber
             , 'getAddressTransactions
             , 'getLastPeriodId
             , 'getTx
             , 'getTxSummary
             , 'isAddressKnown
             , 'addHBlock
             ])
