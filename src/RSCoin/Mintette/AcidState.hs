{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Wrap Storage into AcidState.

module RSCoin.Mintette.AcidState
       ( State
       , openState
       , closeState
       , GetUtxoPset (..)
       , PreviousMintetteId (..)
       , CheckNotDoubleSpent (..)
       , CommitTx (..)
       , FinishPeriod (..)
       , StartPeriod (..)
       , FinishEpoch (..)
       , GetBlocks (..)
       ) where

import           Control.Exception       (throw)
import           Control.Monad.Catch     (MonadThrow (throwM))
import           Data.Acid               (AcidState, Query, Update,
                                          closeAcidState, makeAcidic,
                                          openLocalStateFrom)
import           Data.SafeCopy           (base, deriveSafeCopy)

import           RSCoin.Core             (AddrId, CheckConfirmation,
                                          CheckConfirmations,
                                          CommitConfirmation, MintetteId,
                                          NewPeriodData, PeriodId, PeriodResult,
                                          Pset, SecretKey, Signature,
                                          Transaction, Utxo, LBlock)

import qualified RSCoin.Mintette.Storage as MS

type State = AcidState MS.Storage

$(deriveSafeCopy 0 'base ''MS.Storage)

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp MS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState

instance MonadThrow (Update s) where
    throwM = throw

getUtxoPset :: Query MS.Storage (Utxo,Pset)
getUtxoPset = MS.getUtxoPset

getBlocks :: PeriodId -> Query MS.Storage (Maybe [LBlock])
getBlocks = MS.getBlocks

previousMintetteId :: Query MS.Storage (Maybe MintetteId)
previousMintetteId = MS.previousMintetteId

checkNotDoubleSpent
    :: SecretKey
    -> Transaction
    -> AddrId
    -> Signature
    -> Update MS.Storage CheckConfirmation
checkNotDoubleSpent = MS.checkNotDoubleSpent

commitTx :: SecretKey
         -> Transaction
         -> PeriodId
         -> CheckConfirmations
         -> Update MS.Storage CommitConfirmation
commitTx = MS.commitTx

finishPeriod :: SecretKey -> PeriodId -> Update MS.Storage PeriodResult
finishPeriod = MS.finishPeriod

startPeriod :: NewPeriodData -> Update MS.Storage ()
startPeriod = MS.startPeriod

finishEpoch :: SecretKey -> Update MS.Storage ()
finishEpoch = MS.finishEpoch

$(makeAcidic ''MS.Storage
             [ 'getUtxoPset
             , 'previousMintetteId
             , 'checkNotDoubleSpent
             , 'commitTx
             , 'finishPeriod
             , 'startPeriod
             , 'finishEpoch
             , 'getBlocks
             ])
