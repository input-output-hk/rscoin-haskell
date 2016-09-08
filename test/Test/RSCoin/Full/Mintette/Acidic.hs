{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Wrap Storage into AcidState (modified version for configurable mintettes).

module Test.RSCoin.Full.Mintette.Acidic
       ( openMemState
       , closeState
       , tidyState
       , CheckNotDoubleSpent (..)
       , CommitTx (..)
       ) where

import           Control.Monad.Trans               (MonadIO)
import           Data.Acid                         (Update, makeAcidic)

import           Serokell.AcidState                (closeExtendedState,
                                                    openMemoryExtendedState,
                                                    tidyExtendedState)

import           RSCoin.Core                       (AddrId, Address,
                                                    CheckConfirmation,
                                                    CheckConfirmations,
                                                    CommitAcknowledgment,
                                                    Signature, Transaction)
import           RSCoin.Mintette.AcidState         (State)
import qualified RSCoin.Mintette.AcidState         as OMA
import           RSCoin.Mintette.Env               (RuntimeEnv)
import qualified RSCoin.Mintette.Storage           as OMS

import           Test.RSCoin.Full.Mintette.Config  (MintetteConfig)
import qualified Test.RSCoin.Full.Mintette.Storage as MS

openMemState :: MonadIO m => m State
openMemState = openMemoryExtendedState OMS.mkStorage

closeState :: MonadIO m => State -> m ()
closeState = closeExtendedState

tidyState :: MonadIO m => State -> m ()
tidyState = tidyExtendedState

checkNotDoubleSpent
    :: MintetteConfig
    -> Transaction
    -> AddrId
    -> [(Address, Signature Transaction)]
    -> RuntimeEnv
    -> Update OMS.Storage CheckConfirmation
checkNotDoubleSpent = OMA.convertUpdateInEnv MS.checkNotDoubleSpent

commitTx
    :: MintetteConfig
    -> Transaction
    -> CheckConfirmations
    -> RuntimeEnv
    -> Update OMS.Storage CommitAcknowledgment
commitTx = OMA.convertUpdateInEnv MS.commitTx

$(makeAcidic ''OMS.Storage
             [
             -- Original
               'OMA.getUtxoPset
             , 'OMA.getPreviousMintetteId
             , 'OMA.finishPeriod
             , 'OMA.startPeriod
             , 'OMA.finishEpoch
             , 'OMA.getLogs

             -- New
             , 'checkNotDoubleSpent
             , 'commitTx
             ])
