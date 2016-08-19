{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | This module generates Acidic instance. It's separated from the
-- main AcidState only in this way extending AcidState is possible
-- (and it's done in testing framework)

module RSCoin.Mintette.Acidic
       ( State
       , closeState
       , openState
       , openMemState
       , tidyState

       , GetUtxoPset (..)
       , PreviousMintetteId (..)
       , CheckNotDoubleSpent (..)
       , CommitTx (..)
       , FinishPeriod (..)
       , StartPeriod (..)
       , FinishEpoch (..)
       , GetBlocks (..)
       , GetLogs (..)
       , GetPeriodId (..)
       ) where

import           Control.Monad.Trans       (MonadIO)
import           Data.Acid                 (makeAcidic)

import           Serokell.Util.AcidState   (closeExtendedState,
                                            openLocalExtendedState,
                                            openMemoryExtendedState,
                                            tidyExtendedState)

import           RSCoin.Mintette.AcidState (State)
import qualified RSCoin.Mintette.AcidState as S
import qualified RSCoin.Mintette.Storage   as MS

openState :: MonadIO m => FilePath -> m State
openState fp = openLocalExtendedState fp MS.mkStorage

openMemState :: MonadIO m => m State
openMemState = openMemoryExtendedState MS.mkStorage

closeState :: MonadIO m => State -> m ()
closeState = closeExtendedState

tidyState :: MonadIO m => State -> m ()
tidyState = tidyExtendedState

$(makeAcidic ''MS.Storage
             [ 'S.getUtxoPset
             , 'S.previousMintetteId
             , 'S.checkNotDoubleSpent
             , 'S.commitTx
             , 'S.finishPeriod
             , 'S.startPeriod
             , 'S.finishEpoch
             , 'S.getBlocks
             , 'S.getLogs
             , 'S.getPeriodId
             ])
