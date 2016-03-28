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
       , GetHBlock (..)
       , AddMintette (..)
       , StartNewPeriod (..)
       ) where

import           Control.Lens            (view)
import           Data.Acid               (AcidState, Query, closeAcidState,
                                          makeAcidic, openLocalStateFrom)
import           Data.SafeCopy           (base, deriveSafeCopy)

import           Serokell.Util.AcidState (exceptStateToUpdate, stateToUpdate)

import           RSCoin.Core             (HBlock, Mintettes, PeriodId)

import qualified RSCoin.Bank.Storage     as BS

type State = AcidState BS.Storage

$(deriveSafeCopy 0 'base ''BS.Storage)

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp BS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState

getMintettes :: Query BS.Storage Mintettes
getPeriodId :: Query BS.Storage PeriodId
getHBlock :: PeriodId -> Query BS.Storage (Maybe HBlock)
getMintettes = view BS.getMintettes
getPeriodId = view BS.getPeriodId
getHBlock = view . BS.getHBlock

addMintette a = stateToUpdate . BS.addMintette a
startNewPeriod a = exceptStateToUpdate . BS.startNewPeriod a

$(makeAcidic ''BS.Storage
             [ 'getMintettes
             , 'getPeriodId
             , 'getHBlock
             , 'addMintette
             , 'startNewPeriod
             ])
