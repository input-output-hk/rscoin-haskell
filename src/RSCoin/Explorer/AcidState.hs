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
       , GetAddressInterestingTransactions (..)
       , GetAddressTransactions (..)
       , GetAddressTxNumber (..)
       , GetExpectedPeriodId (..)
       , GetHBlocksExtended (..)
       , GetHBlockExtended (..)
       , GetInterestingTxsGlobal (..)
       , GetTx (..)
       , GetTxExtended (..)
       , GetTxExtensions (..)
       , GetTxsGlobal (..)
       , IsAddressKnown (..)
       , IsTransactionKnown (..)

       , AddHBlock (..)
       ) where

import           Control.Monad.Trans     (MonadIO)
import           Data.Acid               (EventResult, EventState, QueryEvent,
                                          UpdateEvent, makeAcidic)

import           Serokell.AcidState      (ExtendedState, closeExtendedState,
                                          openLocalExtendedState,
                                          openMemoryExtendedState,
                                          queryExtended, tidyExtendedState,
                                          updateExtended)

import qualified RSCoin.Explorer.Storage as ES

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

$(makeAcidic ''ES.Storage
             [ 'ES.getAddressBalance
             , 'ES.getAddressTxNumber
             , 'ES.getAddressTransactions
             , 'ES.getAddressInterestingTransactions
             , 'ES.getExpectedPeriodId
             , 'ES.getHBlocksExtended
             , 'ES.getHBlockExtended
             , 'ES.getTx
             , 'ES.getTxExtended
             , 'ES.getTxExtensions
             , 'ES.getTxsGlobal
             , 'ES.getInterestingTxsGlobal
             , 'ES.isAddressKnown
             , 'ES.isTransactionKnown
             , 'ES.addHBlock
             ])
