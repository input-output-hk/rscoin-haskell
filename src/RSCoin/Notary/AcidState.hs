{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Wrap Storage into AcidState.

module RSCoin.Notary.AcidState
       ( NotaryState

         -- * acid-state query and update data types
       , AcquireSignatures (..)
       , AddSignedTransaction (..)
       , AllocateMSAddress (..)
       , AnnounceNewPeriods (..)
       , GetPeriodId (..)
       , GetSignatures (..)
       , PollPendingTxs (..)
       , QueryAllMSAdresses (..)
       , QueryCompleteMSAdresses (..)
       , QueryMyMSRequests (..)
       , RemoveCompleteMSAddresses (..)

         -- * Encapsulations
       , closeState
       , openState
       , openMemState
       , query
       , tidyState
       , update
       ) where

import           Control.Monad.Trans   (MonadIO)
import           Data.Acid             (EventResult, EventState, QueryEvent,
                                        UpdateEvent, makeAcidic)
import           Data.SafeCopy         (base, deriveSafeCopy)

import           Serokell.AcidState    (ExtendedState, closeExtendedState,
                                        openLocalExtendedState,
                                        openMemoryExtendedState, queryExtended,
                                        tidyExtendedState, updateExtended)

import           RSCoin.Core           (PublicKey)
import           RSCoin.Notary.Storage (Storage (..))
import qualified RSCoin.Notary.Storage as S

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

openState
    :: MonadIO m
    => FilePath -> [PublicKey] -> m NotaryState
openState fp trustedKeys = openLocalExtendedState fp st
  where
    st =
        S.emptyNotaryStorage
        { _masterKeys = trustedKeys
        }

openMemState
    :: MonadIO m
    => [PublicKey] -> m NotaryState
openMemState trustedKeys = openMemoryExtendedState st
  where
    st =
        S.emptyNotaryStorage
        { _masterKeys = trustedKeys
        }

closeState :: MonadIO m => NotaryState -> m ()
closeState = closeExtendedState

tidyState :: MonadIO m => NotaryState -> m ()
tidyState = tidyExtendedState

$(makeAcidic ''Storage
             [ 'S.acquireSignatures
             , 'S.addSignedTransaction
             , 'S.allocateMSAddress
             , 'S.announceNewPeriods
             , 'S.getPeriodId
             , 'S.getSignatures
             , 'S.pollPendingTxs
             , 'S.queryAllMSAdresses
             , 'S.queryCompleteMSAdresses
             , 'S.queryMyMSRequests
             , 'S.removeCompleteMSAddresses
             ])
