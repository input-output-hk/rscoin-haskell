{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Wrap Storage into AcidState

module RSCoin.Bank.AcidState
       ( State
       , openState
       , closeState
       , GetMintettes (..)
       , GetPeriodId (..)
       , AddMintette (..)
       , StartNewPeriod (..)
       ) where

import           Control.Lens            (view)
import           Data.Acid               (AcidState, Query, closeAcidState,
                                          makeAcidic, openLocalStateFrom)
import           Data.SafeCopy           (base, deriveSafeCopy)

import           Serokell.Util.AcidState (exceptStateToUpdate, stateToUpdate)

import           RSCoin.Core             (Mintettes, PeriodId)

import qualified RSCoin.Bank.Storage     as BS

type State = AcidState BS.Storage

$(deriveSafeCopy 0 'base ''BS.Storage)

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp BS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState

getMintettes :: Query BS.Storage Mintettes
getMintettes = view BS.getMintettes
getPeriodId :: Query BS.Storage PeriodId
getPeriodId = view BS.getPeriodId

addMintette a = stateToUpdate . BS.addMintette a
startNewPeriod a = exceptStateToUpdate . BS.startNewPeriod a

$(makeAcidic ''BS.Storage
             [ 'getMintettes
             , 'getPeriodId
             , 'addMintette
             , 'startNewPeriod
             ])
