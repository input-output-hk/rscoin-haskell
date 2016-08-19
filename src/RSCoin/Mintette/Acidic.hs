{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | This module generates Acidic instance. It's separated from the
-- main AcidState only in this way extending AcidState is possible
-- (and it's done in testing framework)

module RSCoin.Mintette.Acidic
       ( State (..)
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

import           Data.Acid                 (closeAcidState, makeAcidic,
                                            openLocalStateFrom)
import           Data.Acid.Memory          (openMemoryState)

import           Serokell.Util.AcidState   (tidyLocalState)

import           RSCoin.Mintette.AcidState (State (..), toAcidState)
import qualified RSCoin.Mintette.AcidState as S
import qualified RSCoin.Mintette.Storage   as MS

openState :: FilePath -> IO State
openState fp = flip LocalState fp <$> openLocalStateFrom fp MS.mkStorage

openMemState :: IO State
openMemState = MemoryState <$> openMemoryState MS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState . toAcidState

tidyState :: State -> IO ()
tidyState (LocalState st fp) = tidyLocalState st fp
tidyState (MemoryState _)    = return ()

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
