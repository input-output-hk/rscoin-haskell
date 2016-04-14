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
       , GetHBlocks (..)
       , GetLogs (..)
       , AddMintette (..)
       , StartNewPeriod (..)
       ) where

import           Control.Exception   (throw)
import           Control.Lens        (view)
import           Control.Monad.Catch (MonadThrow (throwM))
import           Data.Acid           (AcidState, Query, Update, closeAcidState,
                                      makeAcidic, openLocalStateFrom)
import           Data.SafeCopy       (base, deriveSafeCopy)

import           RSCoin.Core         (ActionLog, HBlock, Mintette, MintetteId,
                                      Mintettes, NewPeriodData, PeriodId,
                                      PeriodResult, PublicKey, SecretKey)

import qualified RSCoin.Bank.Storage as BS

type State = AcidState BS.Storage

$(deriveSafeCopy 0 'base ''BS.DeadMintetteState)
$(deriveSafeCopy 0 'base ''BS.Storage)

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp BS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState

instance MonadThrow (Update s) where
    throwM = throw

getMintettes :: Query BS.Storage Mintettes
getMintettes = view BS.getMintettes

getPeriodId :: Query BS.Storage PeriodId
getPeriodId = view BS.getPeriodId

getHBlock :: PeriodId -> Query BS.Storage (Maybe HBlock)
getHBlock = view . BS.getHBlock

-- Dumping Bank state

getHBlocks :: PeriodId -> PeriodId -> Query BS.Storage [HBlock]
getHBlocks from to = view $ BS.getHBlocks from to

getLogs :: MintetteId -> Int -> Int -> Query BS.Storage (Maybe ActionLog)
getLogs m from to = view $ BS.getLogs m from to

-- Dumping Bank state

addMintette :: Mintette -> PublicKey -> Update BS.Storage ()
addMintette = BS.addMintette

startNewPeriod
    :: SecretKey
    -> [Maybe PeriodResult]
    -> Update BS.Storage [NewPeriodData]
startNewPeriod = BS.startNewPeriod

$(makeAcidic ''BS.Storage
             [ 'getMintettes
             , 'getPeriodId
             , 'getHBlock
             , 'getHBlocks
             , 'getLogs
             , 'addMintette
             , 'startNewPeriod
             ])
