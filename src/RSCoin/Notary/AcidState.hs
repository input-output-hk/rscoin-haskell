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
       , PollTransactions (..)
       , QueryAllMSAdresses (..)
       , QueryCompleteMSAdresses (..)
       , QueryMyMSRequests (..)
       , RemoveCompleteMSAddresses (..)

         -- * Encapsulations
       , closeState
       , openState
       , openMemState
       , query
       , update
       ) where

import           Control.Monad.Trans   (MonadIO (liftIO))
import           Data.Acid             (AcidState, EventResult, EventState,
                                        QueryEvent, UpdateEvent, closeAcidState,
                                        makeAcidic, openLocalStateFrom)
import           Data.Acid.Advanced    (query', update')
import           Data.Acid.Memory      (openMemoryState)
import           Data.SafeCopy         (base, deriveSafeCopy)

import           RSCoin.Core           (PublicKey)
import           RSCoin.Notary.Storage (Storage (..))
import qualified RSCoin.Notary.Storage as S

type AState = AcidState Storage

data NotaryState
    = LocalState AState
                 FilePath
    | MemoryState AState

toAcidState :: NotaryState -> AState
toAcidState (LocalState st _) = st
toAcidState (MemoryState st)  = st

$(deriveSafeCopy 0 'base ''Storage)

query
    :: (EventState event ~ Storage, QueryEvent event, MonadIO m)
    => NotaryState -> event -> m (EventResult event)
query st = query' (toAcidState st)

update
    :: (EventState event ~ Storage, UpdateEvent event, MonadIO m)
    => NotaryState -> event -> m (EventResult event)
update st = update' (toAcidState st)

openState
    :: MonadIO m
    => FilePath -> [PublicKey] -> m NotaryState
openState fp trustedKeys =
    liftIO $
    flip LocalState fp <$>
    openLocalStateFrom
        fp
        S.emptyNotaryStorage
        { _masterKeys = trustedKeys
        }

openMemState
    :: MonadIO m
    => [PublicKey] -> m NotaryState
openMemState trustedKeys =
    liftIO $
    MemoryState <$>
    openMemoryState
        S.emptyNotaryStorage
        { _masterKeys = trustedKeys
        }

closeState :: MonadIO m => NotaryState -> m ()
closeState = liftIO . closeAcidState . toAcidState

$(makeAcidic ''Storage
             [ 'S.acquireSignatures
             , 'S.addSignedTransaction
             , 'S.allocateMSAddress
             , 'S.announceNewPeriods
             , 'S.getPeriodId
             , 'S.getSignatures
             , 'S.pollTransactions
             , 'S.queryAllMSAdresses
             , 'S.queryCompleteMSAdresses
             , 'S.queryMyMSRequests
             , 'S.removeCompleteMSAddresses
             ])
