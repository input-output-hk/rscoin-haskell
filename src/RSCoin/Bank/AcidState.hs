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
       , GetAddresses (..)
       , GetExplorers (..)
       , GetExplorersAndPeriods (..)
       , GetPeriodId (..)
       , GetHBlock (..)
       , GetHBlockWithMetadata (..)
       , GetHBlocks (..)
       , GetLogs (..)
       , GetStatisticsId (..)

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
       , CheckAndBumpStatisticsId (..)
       ) where

import           Control.Lens                  (Getter, to, view)
import           Control.Monad.Reader          (ask)
import           Control.Monad.Trans           (MonadIO)
import           Data.Acid                     (EventResult, EventState, Query,
                                                QueryEvent, Update, UpdateEvent,
                                                makeAcidic)
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import           Data.Time.Clock.POSIX         (POSIXTime)
import           Formatting                    (bprint, stext, (%))
import           Safe                          (headMay)

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

import           RSCoin.Core                   (ActionLog, Address,
                                                AddressToTxStrategyMap,
                                                Explorer, Explorers, HBlock,
                                                Mintette, MintetteId, Mintettes,
                                                NewPeriodData, PeriodId,
                                                PeriodResult, PublicKey,
                                                SecretKey, TxStrategy)
import qualified RSCoin.Core                   as C

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

openState :: Bool -> FilePath -> IO State
openState deleteIfExists fp = openLocalExtendedState deleteIfExists fp BS.mkStorage

openMemState :: IO State
openMemState = openMemoryExtendedState BS.mkStorage

closeState :: State -> IO ()
closeState = closeExtendedState

getStorage :: Query BS.Storage BS.Storage
getStorage = ask

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

getHBlockWithMetadata :: C.PeriodId
                      -> Query BS.Storage (Maybe (C.WithMetadata C.HBlock C.HBlockMetadata))
getHBlockWithMetadata = view . BS.getHBlockWithMetadata

getHBlocks :: PeriodId -> PeriodId -> Query BS.Storage [HBlock]
getHBlocks fromIdx toIdx = view $ BS.getHBlocks fromIdx toIdx

getLogs :: MintetteId -> Int -> Int -> Query BS.Storage (Maybe ActionLog)
getLogs m fromIdx toIdx = view $ BS.getLogs m fromIdx toIdx

getStatisticsId :: Query BS.Storage Int
getStatisticsId = view BS.getStatisticsId

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
    :: POSIXTime
    -> SecretKey
    -> [Maybe PeriodResult]
    -> Update BS.Storage [NewPeriodData]
startNewPeriod = BS.startNewPeriod

checkAndBumpStatisticsId :: Int -> Update BS.Storage Bool
checkAndBumpStatisticsId = BS.checkAndBumpStatisticsId

$(makeAcidic ''BS.Storage
             [ 'getMintettes
             , 'getAddresses
             , 'getExplorers
             , 'getExplorersAndPeriods
             , 'getPeriodId
             , 'getHBlock
             , 'getHBlockWithMetadata
             , 'getHBlocks
             , 'getLogs
             , 'getStatisticsId

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
             , 'checkAndBumpStatisticsId
             ])

getStatistics
    :: MonadIO m
    => State -> m Text
getStatistics st =
    show' . listBuilderJSONIndent 3 . map toBuilder . estimateMemoryUsage parts <$>
    query st GetStorage
  where
    parts =
        [ StoragePart "mintettes" BS.getMintettes
        , StoragePart "dpk" BS.getDpk

        , StoragePart "actionLogs" BS.getAllActionLogs
        , StoragePart "actionLogs[0]" firstActionLog
        , StoragePart "hashes from actionLogs[0]" firstActionLogHashes
        , StoragePart "entries from actionLogs[0]" firstActionLogEntries
        , StoragePart "query entries from actionLogs[0]" firstActionLogQueryEntries
        , StoragePart "commit entries from actionLogs[0]" firstActionLogCommitEntries
        , StoragePart "close epoch entries from actionLogs[0]" firstActionLogCloseEpochEntries

        , StoragePart "explorers" BS.getExplorersAndPeriods

        , StoragePart "addresses" BS.getAddresses

        , StoragePart "blocks" BS.getAllHBlocks
        , StoragePart "utxo" BS.getUtxo

        , StoragePart "Storage" (to id)
        ]
    toBuilder (name,size :: Byte) = bprint (stext % ": " % memory) name size
    firstActionLog :: Getter BS.Storage ActionLog
    firstActionLog = BS.getAllActionLogs . to (fromMaybe [] . headMay)
    firstActionLogHashes :: Getter BS.Storage [C.ActionLogEntryHash]
    firstActionLogHashes = firstActionLog . to (map snd)
    firstActionLogEntries :: Getter BS.Storage [C.ActionLogEntry]
    firstActionLogEntries = firstActionLog . to (map fst)
    firstActionLogQueryEntries :: Getter BS.Storage [C.ActionLogEntry]
    firstActionLogQueryEntries =
        firstActionLogEntries . to (filter C.isQueryEntry)
    firstActionLogCommitEntries :: Getter BS.Storage [C.ActionLogEntry]
    firstActionLogCommitEntries =
        firstActionLogEntries . to (filter C.isCommitEntry)
    firstActionLogCloseEpochEntries :: Getter BS.Storage [C.ActionLogEntry]
    firstActionLogCloseEpochEntries =
        firstActionLogEntries . to (filter C.isCloseEpochEntry)
