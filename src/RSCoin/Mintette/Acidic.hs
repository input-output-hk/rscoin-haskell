{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | This module generates Acidic instance. It's separated from the
-- main AcidState only in this way extending AcidState is possible
-- (and it's done in testing framework)

module RSCoin.Mintette.Acidic
       ( openState
       , openMemState
       , closeState
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

import           Data.Acid                 (closeAcidState, makeAcidic,
                                            openLocalStateFrom)
import           Data.Acid.Memory          (openMemoryState)

import           RSCoin.Mintette.AcidState (State)
import qualified RSCoin.Mintette.AcidState as S
import qualified RSCoin.Mintette.Storage   as MS

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp MS.mkStorage

openMemState :: IO State
openMemState = openMemoryState MS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState

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
