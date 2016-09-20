{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This module generates Acidic instance. It's separated from the
-- main AcidState only in this way extending AcidState is possible
-- (and it's done in testing framework)

module RSCoin.Mintette.Acidic
       ( State
       , closeState
       , getStatistics
       , openState
       , openMemState
       , tidyState

       , GetUtxoPset (..)
       , GetPreviousMintetteId (..)
       , CheckNotDoubleSpent (..)
       , CommitTx (..)
       , FinishPeriod (..)
       , StartPeriod (..)
       , FinishEpoch (..)
       , ApplyExtraAddresses (..)
       , ApplyExtraUtxo (..)
       , GetLastLBlocks (..)
       , GetLogs (..)
       , GetPeriodId (..)
       ) where

import           Control.Lens                  (to)
import           Control.Monad.Trans           (MonadIO)
import           Data.Acid                     (makeAcidic)
import           Data.Text                     (Text)
import           Formatting                    (bprint, stext, (%))

import           Serokell.AcidState            (closeExtendedState,
                                                openLocalExtendedState,
                                                openMemoryExtendedState,
                                                tidyExtendedState)
import           Serokell.AcidState.Statistics (StoragePart (..), estimateMemoryUsage)
import           Serokell.Data.Memory.Units    (Byte, memory)
import           Serokell.Util.Text            (listBuilderJSONIndent, show')

import           RSCoin.Mintette.AcidState     (State)
import qualified RSCoin.Mintette.AcidState     as S
import qualified RSCoin.Mintette.Storage       as MS

openState :: MonadIO m => Bool -> FilePath -> m State
openState deleteIfExists fp = openLocalExtendedState deleteIfExists fp MS.mkStorage

openMemState :: MonadIO m => m State
openMemState = openMemoryExtendedState MS.mkStorage

closeState :: MonadIO m => State -> m ()
closeState = closeExtendedState

tidyState :: MonadIO m => State -> m ()
tidyState = tidyExtendedState

$(makeAcidic ''MS.Storage
             [ 'S.getUtxoPset
             , 'S.getPreviousMintetteId
             , 'S.getLastLBlocks
             , 'S.getLogs
             , 'S.getPeriodId

             , 'S.getStorage

             , 'S.checkNotDoubleSpent
             , 'S.commitTx
             , 'S.finishPeriod
             , 'S.startPeriod
             , 'S.finishEpoch
             , 'S.applyExtraAddresses
             , 'S.applyExtraUtxo
             ])

getStatistics
    :: MonadIO m
    => State -> m Text
getStatistics st =
    show' . listBuilderJSONIndent 3 . map toBuilder . estimateMemoryUsage parts <$>
    S.query st GetStorage
  where
    parts =
        [ StoragePart "utxo" MS.utxo
        , StoragePart "utxoAdded" MS.utxoAdded
        , StoragePart "utxoDeleted" MS.utxoDeleted
        , StoragePart "pset" MS.pset
        , StoragePart "txset" MS.txset
        , StoragePart "lBlocks" MS.lBlocks
        , StoragePart "actionLogs" MS.actionLogs
        , StoragePart "logHeads" MS.logHeads

        , StoragePart "Storage" (to id)
        ]
    toBuilder (name,size :: Byte) = bprint (stext % ": " % memory) name size
