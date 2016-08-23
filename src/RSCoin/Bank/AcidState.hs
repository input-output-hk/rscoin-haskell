{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | Wrap Storage into AcidState

module RSCoin.Bank.AcidState
       ( State
       , closeState
       , getStatistics
       , openState
       , openMemState
       , query
       , tidyState
       , update

         -- | Queries
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

         -- | Updates
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

import           Control.Lens                  (view)
import           Control.Monad.Reader          (ask)
import           Control.Monad.Trans           (MonadIO)
import           Data.Acid                     (EventResult, EventState, Query,
                                                QueryEvent, Update, UpdateEvent,
                                                makeAcidic)
import           Data.Text                     (Text)
import           Formatting                    (bprint, stext, (%))

import           RSCoin.Core                   (ActionLog, Address,
                                                AddressToTxStrategyMap,
                                                Explorer, Explorers, HBlock,
                                                Mintette, MintetteId, Mintettes,
                                                NewPeriodData, PeriodId,
                                                PeriodResult, PublicKey,
                                                SecretKey, TransactionId,
                                                TxStrategy)

import           Serokell.AcidState            (ExtendedState,
                                                closeExtendedState,
                                                openLocalExtendedState,
                                                openMemoryExtendedState,
                                                queryExtended,
                                                tidyExtendedState,
                                                updateExtended)
import           Serokell.AcidState.Statistics (StoragePart (..),
                                                estimateMemoryUsage)
import           Serokell.Data.Memory.Units    (Byte, memory)
import           Serokell.Util.Text            (listBuilderJSONIndent, show')

import qualified RSCoin.Bank.Storage           as BS

type State = ExtendedState BS.Storage

query
    :: (EventState event ~ BS.Storage, QueryEvent event, MonadIO m)
    => State -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ BS.Storage, UpdateEvent event, MonadIO m)
    => State -> event -> m (EventResult event)
update = updateExtended

tidyState :: MonadIO m => State -> m ()
tidyState = tidyExtendedState

openState :: FilePath -> IO State
openState fp = openLocalExtendedState fp BS.mkStorage

openMemState :: IO State
openMemState = openMemoryExtendedState BS.mkStorage

closeState :: State -> IO ()
closeState = closeExtendedState

getStorage :: Query BS.Storage BS.Storage
getStorage = ask

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

             , 'getStorage

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

getStatistics
    :: MonadIO m
    => State -> m Text
getStatistics st =
    show' . listBuilderJSONIndent 3 . map toBuilder . estimateMemoryUsage parts <$>
    query st GetStorage
  where
    parts = [StoragePart "mintettes" BS.getMintettes]
    toBuilder (name,size :: Byte) = bprint (stext % ": " % memory) name size
