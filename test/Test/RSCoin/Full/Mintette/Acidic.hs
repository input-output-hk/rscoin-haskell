{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Wrap Storage into AcidState (modified version for configurable mintettes).

module Test.RSCoin.Full.Mintette.Acidic
       ( openState
       , openMemState
       , closeState
       , CheckNotDoubleSpent (..)
       , CommitTx (..)
       ) where

import           Data.Acid                         (AcidState, Query, Update,
                                                    closeAcidState, makeAcidic,
                                                    openLocalStateFrom)
import           Data.Acid.Memory                  (openMemoryState)
import           Data.SafeCopy                     (base, deriveSafeCopy)

import           RSCoin.Core                       (ActionLog, AddrId,
                                                    CheckConfirmation,
                                                    CheckConfirmations,
                                                    CommitConfirmation, LBlock,
                                                    MintetteId, NewPeriodData,
                                                    PeriodId, PeriodResult,
                                                    Pset, SecretKey, Signature,
                                                    Transaction, Utxo)
import           RSCoin.Mintette.AcidState         (State)
import qualified RSCoin.Mintette.AcidState         as OMA
import qualified RSCoin.Mintette.Storage           as OMS

import           Test.RSCoin.Full.Mintette.Config  (MintetteConfig)
import qualified Test.RSCoin.Full.Mintette.Storage as MS

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp OMS.mkStorage

openMemState :: IO State
openMemState = openMemoryState OMS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState

checkNotDoubleSpent
    :: MintetteConfig
    -> SecretKey
    -> Transaction
    -> AddrId
    -> Signature
    -> Update OMS.Storage CheckConfirmation
checkNotDoubleSpent = MS.checkNotDoubleSpent

commitTx
    :: MintetteConfig
    -> SecretKey
    -> Transaction
    -> PeriodId
    -> CheckConfirmations
    -> Update OMS.Storage CommitConfirmation
commitTx = MS.commitTx

$(makeAcidic ''OMS.Storage
             [
             -- Original
               'OMA.getUtxoPset
             , 'OMA.previousMintetteId
             , 'OMA.finishPeriod
             , 'OMA.startPeriod
             , 'OMA.finishEpoch
             , 'OMA.getBlocks
             , 'OMA.getLogs
             -- New
             , 'checkNotDoubleSpent
             , 'commitTx
             ])
