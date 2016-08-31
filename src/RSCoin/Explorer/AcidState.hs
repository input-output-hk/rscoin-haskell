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
       , GetExpectedPeriodId (..)
       , GetTx (..)
       , GetTxExtended (..)
       , IsAddressKnown (..)
       , IsTransactionKnown (..)

       , AddHBlock (..)
       ) where

import           Control.Monad.Trans      (MonadIO)
import           Data.Acid                (EventResult, EventState, Query,
                                           QueryEvent, Update, UpdateEvent,
                                           makeAcidic)

import           Serokell.AcidState       (ExtendedState, closeExtendedState,
                                           openLocalExtendedState,
                                           openMemoryExtendedState,
                                           queryExtended, tidyExtendedState,
                                           updateExtended)

import qualified RSCoin.Core              as C

import           RSCoin.Explorer.Extended (CoinsMapExtended,
                                           TransactionExtended)
import qualified RSCoin.Explorer.Storage  as ES

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

getAddressBalance :: C.Address -> Query ES.Storage (C.PeriodId, CoinsMapExtended)
getAddressBalance = ES.getAddressBalance

getAddressTxNumber :: C.Address -> Query ES.Storage (C.PeriodId, Word)
getAddressTxNumber = ES.getAddressTxNumber

getAddressTransactions
    :: C.Address
    -> (Word, Word)
    -> Query ES.Storage (C.PeriodId, [(Word, TransactionExtended)])
getAddressTransactions = ES.getAddressTransactions

getExpectedPeriodId :: Query ES.Storage C.PeriodId
getExpectedPeriodId = ES.getExpectedPeriodId

getTx :: C.TransactionId -> Query ES.Storage (Maybe C.Transaction)
getTx = ES.getTx

getTxExtended :: C.TransactionId -> Query ES.Storage (Maybe TransactionExtended)
getTxExtended = ES.getTxExtended

isAddressKnown :: C.Address -> Query ES.Storage Bool
isAddressKnown = ES.isAddressKnown

isTransactionKnown :: C.TransactionId -> Query ES.Storage Bool
isTransactionKnown = ES.isTransactionKnown

addHBlock :: C.PeriodId
          -> C.WithMetadata C.HBlock C.HBlockMetadata
          -> Update ES.Storage ()
addHBlock = ES.addHBlock

$(makeAcidic ''ES.Storage
             [ 'getAddressBalance
             , 'getAddressTxNumber
             , 'getAddressTransactions
             , 'getExpectedPeriodId
             , 'getTx
             , 'getTxExtended
             , 'isAddressKnown
             , 'isTransactionKnown
             , 'addHBlock
             ])
