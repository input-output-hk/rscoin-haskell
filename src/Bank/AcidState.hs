{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Wrap Storage into AcidState

module AcidState
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

import qualified Storage                 as BS

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

addMintette m = stateToUpdate . BS.addMintette m
startNewPeriod = exceptStateToUpdate . BS.startNewPeriod

$(makeAcidic ''BS.Storage
             [ 'getMintettes
             , 'getPeriodId
             , 'addMintette
             , 'startNewPeriod
             ])
