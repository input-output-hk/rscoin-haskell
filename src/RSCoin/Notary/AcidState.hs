{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ViewPatterns    #-}

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
       , OutdatedAllocs (..)

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
import           Data.Optional         (Optional, defaultTo)
import           Data.SafeCopy         (base, deriveSafeCopy)

import           Serokell.AcidState    (ExtendedState, closeExtendedState,
                                        openLocalExtendedState,
                                        openMemoryExtendedState, queryExtended,
                                        tidyExtendedState, updateExtended)

import           RSCoin.Core           (PeriodId, PublicKey,
                                        notaryAliveSizeDefault)
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
    => Bool
    -> FilePath
    -> [PublicKey]
    -> Optional PeriodId
    -> m NotaryState
openState deleteIfExists fp trustedKeys (defaultTo notaryAliveSizeDefault -> aliveSize) =
    openLocalExtendedState deleteIfExists fp st
  where
    st = S.emptyNotaryStorage
             { _masterKeys = trustedKeys
             , _aliveSize  = aliveSize
             }

openMemState
    :: MonadIO m
    => [PublicKey]
    -> Optional PeriodId
    -> m NotaryState
openMemState trustedKeys (defaultTo notaryAliveSizeDefault -> aliveSize) =
    openMemoryExtendedState st
  where
    st = S.emptyNotaryStorage
             { _masterKeys = trustedKeys
             , _aliveSize  = aliveSize
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
             , 'S.outdatedAllocs
             ])
