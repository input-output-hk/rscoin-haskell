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
       , GetEmission (..)
       , GetEmissions (..)
       , GetAddresses (..)
       , GetExplorers (..)
       , GetExplorersAndPeriods (..)
       , GetPeriodId (..)
       , GetHBlock (..)
       , GetHBlocks (..)
       , GetLogs (..)
       , AddAddress (..)
       , AddMintette (..)
       , AddExplorer (..)
       , RemoveMintette (..)
       , RemoveExplorer (..)
       , SetExplorerPeriod (..)
       , SuspendExplorer (..)
       , RestoreExplorers (..)
       , StartNewPeriod (..)
       ) where

import           Control.Lens        (view)
import           Data.Acid           (AcidState, Query, Update, closeAcidState,
                                      makeAcidic, openLocalStateFrom)
import           Data.Acid.Memory    (openMemoryState)

import           RSCoin.Core         (ActionLog, Address,
                                      AddressToTxStrategyMap, Explorer,
                                      Explorers, HBlock, Mintette, MintetteId,
                                      Mintettes, NewPeriodData, PeriodId,
                                      PeriodResult, PublicKey, SecretKey,
                                      TransactionId, TxStrategy)

import qualified RSCoin.Bank.Storage as BS

type State = AcidState BS.Storage

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp BS.mkStorage

openMemState :: IO State
openMemState = openMemoryState BS.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState

getEmission :: PeriodId -> Query BS.Storage (Maybe TransactionId)
getEmission = view . BS.getEmission

getEmissions :: PeriodId -> PeriodId -> Query BS.Storage [TransactionId]
getEmissions from to = view $ BS.getEmissions from to

getAddresses :: Query BS.Storage AddressToTxStrategyMap
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

getHBlocks :: PeriodId -> PeriodId -> Query BS.Storage [HBlock]
getHBlocks from to = view $ BS.getHBlocks from to

getLogs :: MintetteId -> Int -> Int -> Query BS.Storage (Maybe ActionLog)
getLogs m from to = view $ BS.getLogs m from to

-- Dumping Bank state

addAddress :: Address -> TxStrategy -> Update BS.Storage ()
addAddress = BS.addAddress


addMintette :: Mintette -> PublicKey -> Update BS.Storage ()
addMintette = BS.addMintette

addExplorer :: Explorer -> PeriodId -> Update BS.Storage ()
addExplorer = BS.addExplorer

removeMintette :: String -> Int -> Update BS.Storage ()
removeMintette = BS.removeMintette

removeExplorer :: String -> Int -> Update BS.Storage ()
removeExplorer = BS.removeExplorer


setExplorerPeriod :: Explorer -> PeriodId -> Update BS.Storage ()
setExplorerPeriod = BS.setExplorerPeriod

suspendExplorer :: Explorer -> Update BS.Storage ()
suspendExplorer = BS.suspendExplorer

restoreExplorers :: Update BS.Storage ()
restoreExplorers = BS.restoreExplorers

startNewPeriod
    :: PublicKey
    -> Address
    -> SecretKey
    -> [Maybe PeriodResult]
    -> Update BS.Storage [NewPeriodData]
startNewPeriod = BS.startNewPeriod

$(makeAcidic ''BS.Storage
             [ 'getMintettes
             , 'getEmission
             , 'getEmissions
             , 'getAddresses
             , 'getExplorers
             , 'getExplorersAndPeriods
             , 'getPeriodId
             , 'getHBlock
             , 'getHBlocks
             , 'getLogs
             , 'addAddress
             , 'addMintette
             , 'addExplorer
             , 'removeMintette
             , 'removeExplorer
             , 'setExplorerPeriod
             , 'suspendExplorer
             , 'restoreExplorers
             , 'startNewPeriod
             ])
