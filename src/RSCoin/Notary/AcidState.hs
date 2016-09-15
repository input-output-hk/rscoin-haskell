{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ViewPatterns    #-}

-- | Wrap Storage into AcidState.

module RSCoin.Notary.AcidState
       ( NotaryState

         -- * acid-state query and update data types
       , AddSignedTransaction (..)
       , AllocateMSAddress (..)
       , AnnounceNewPeriods (..)
       , CheckIfSynchronized (..)
       , GetPeriodId (..)
       , GetSignatures (..)
       , OutdatedAllocs (..)
       , PollPendingTxs (..)
       , QueryAllMSAdresses (..)
       , QueryCompleteMSAdresses (..)
       , QueryMyMSRequests (..)
       , RemoveCompleteMSAddresses (..)
       , SetSynchronization (..)

         -- * Encapsulations
       , closeState
       , openState
       , openMemState
       , query
       , tidyState
       , update
       ) where

import           Control.Monad.Trans    (MonadIO)
import           Data.Acid              (EventResult, EventState, QueryEvent,
                                         UpdateEvent, makeAcidic)
import           Data.Optional          (Optional, defaultTo)
import           Data.SafeCopy          (base, deriveSafeCopy)

import           Serokell.AcidState     (ExtendedState, closeExtendedState,
                                         openLocalExtendedState,
                                         openMemoryExtendedState, queryExtended,
                                         tidyExtendedState, updateExtended)

import           RSCoin.Core            (PeriodId, PublicKey)
import           RSCoin.Notary.Defaults (defaultAllocationEndurance,
                                         defaultTransactionEndurance)
import           RSCoin.Notary.Storage  (Storage (..))
import qualified RSCoin.Notary.Storage  as S

type NotaryState = ExtendedState Storage

$(deriveSafeCopy 0 'base ''Storage)

query
    :: (EventState event ~ Storage, QueryEvent event, MonadIO m)
    => NotaryState -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ Storage, UpdateEvent event, MonadIO m)
    => NotaryState -> event -> m (EventResult event)
update = updateExtended

openStateHelper
    :: MonadIO m
    => (Storage -> m NotaryState)
    -> [PublicKey]
    -> Optional PeriodId
    -> Optional PeriodId
    -> m NotaryState
openStateHelper
    stateOpener
    masterKeys
    (defaultTo defaultAllocationEndurance  -> allocationEndurance)
    (defaultTo defaultTransactionEndurance -> transactionEndurance)
  =
    stateOpener notaryStorage
  where
    notaryStorage = S.emptyNotaryStorage
                    { _allocationEndurance  = allocationEndurance
                    , _transactionEndurance = transactionEndurance
                    , _masterKeys           = masterKeys
                    }


openState
    :: MonadIO m
    => Bool
    -> FilePath
    -> [PublicKey]
    -> Optional PeriodId
    -> Optional PeriodId
    -> m NotaryState
openState deleteIfExists fp =
    openStateHelper (openLocalExtendedState deleteIfExists fp)

openMemState
    :: MonadIO m
    => [PublicKey]
    -> Optional PeriodId
    -> Optional PeriodId
    -> m NotaryState
openMemState = openStateHelper openMemoryExtendedState

closeState :: MonadIO m => NotaryState -> m ()
closeState = closeExtendedState

tidyState :: MonadIO m => NotaryState -> m ()
tidyState = tidyExtendedState

$(makeAcidic ''Storage
             [ 'S.addSignedTransaction
             , 'S.allocateMSAddress
             , 'S.announceNewPeriods
             , 'S.checkIfSynchronized
             , 'S.getPeriodId
             , 'S.getSignatures
             , 'S.outdatedAllocs
             , 'S.pollPendingTxs
             , 'S.queryAllMSAdresses
             , 'S.queryCompleteMSAdresses
             , 'S.queryMyMSRequests
             , 'S.removeCompleteMSAddresses
             , 'S.setSynchronization
             ])
