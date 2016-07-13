{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Wrap Storage into AcidState

module RSCoin.Bank.AcidState
       ( State
       , openState
       , openMemState
       , closeState
       , GetMintettes (..)
       , GetAddresses (..)
       , GetExplorers (..)
       , GetExplorersAndPeriods (..)
       , GetPeriodId (..)
       , GetHBlock (..)
       , GetHBlocks (..)
       , GetTransaction (..)
       , GetLogs (..)
       , AddMintette (..)
       , AddAddress (..)
       , AddExplorer (..)
       , SetExplorerPeriod (..)
       , SuspendExplorer (..)
       , RestoreExplorers (..)
       , StartNewPeriod (..)
       ) where

import           Control.Exception   (throw)
import           Control.Lens        (view)
import           Control.Monad.Catch (MonadThrow (throwM))
import           Data.Acid           (AcidState, Query, Update, closeAcidState,
                                      makeAcidic, openLocalStateFrom)
import           Data.Acid.Memory    (openMemoryState)

import           RSCoin.Core         (ActionLog, Address, AddressToStrategyMap,
                                      Explorer, Explorers, HBlock, Mintette,
                                      MintetteId, Mintettes, NewPeriodData,
                                      PeriodId, PeriodResult, PublicKey,
                                      SecretKey, Strategy, Transaction,
                                      TransactionId)


import qualified RSCoin.Bank.Storage as BS

type State = AcidState BS.Storage

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp BS.mkStorage

openMemState :: IO State
openMemState = openMemoryState BS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState

instance MonadThrow (Update s) where
    throwM = throw

getAddresses :: Query BS.Storage AddressToStrategyMap
getAddresses = view BS.getAddresses

getMintettes :: Query BS.Storage Mintettes
getMintettes = view BS.getMintettes

getExplorers :: Query BS.Storage Explorers
getExplorers = view BS.getExplorers

getExplorersAndPeriods :: Query BS.Storage [(Explorer, PeriodId)]
getExplorersAndPeriods = view BS.getExplorersAndPeriods

getPeriodId :: Query BS.Storage PeriodId
getPeriodId = view BS.getPeriodId

getHBlock :: PeriodId -> Query BS.Storage (Maybe HBlock)
getHBlock = view . BS.getHBlock

getTransaction :: TransactionId -> Query BS.Storage (Maybe Transaction)
getTransaction = view . BS.getTransaction

getHBlocks :: PeriodId -> PeriodId -> Query BS.Storage [HBlock]
getHBlocks from to = view $ BS.getHBlocks from to

getLogs :: MintetteId -> Int -> Int -> Query BS.Storage (Maybe ActionLog)
getLogs m from to = view $ BS.getLogs m from to

-- Dumping Bank state

addAddress :: Address -> Strategy -> Update BS.Storage ()
addAddress = BS.addAddress


addMintette :: Mintette -> PublicKey -> Update BS.Storage ()
addMintette = BS.addMintette

addExplorer :: Explorer -> PeriodId -> Update BS.Storage ()
addExplorer = BS.addExplorer

setExplorerPeriod :: Explorer -> PeriodId -> Update BS.Storage ()
setExplorerPeriod = BS.setExplorerPeriod

suspendExplorer :: Explorer -> Update BS.Storage ()
suspendExplorer = BS.suspendExplorer

restoreExplorers :: Update BS.Storage ()
restoreExplorers = BS.restoreExplorers

startNewPeriod
    :: SecretKey
    -> [Maybe PeriodResult]
    -> Update BS.Storage [NewPeriodData]
startNewPeriod = BS.startNewPeriod

$(makeAcidic ''BS.Storage
             [ 'getMintettes
             , 'getAddresses
             , 'getExplorers
             , 'getExplorersAndPeriods
             , 'getPeriodId
             , 'getHBlock
             , 'getHBlocks
             , 'getTransaction
             , 'getLogs
             , 'addMintette
             , 'addAddress
             , 'addExplorer
             , 'setExplorerPeriod
             , 'suspendExplorer
             , 'restoreExplorers
             , 'startNewPeriod
             ])
