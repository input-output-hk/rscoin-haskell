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
       , dumpUtxo

         -- | Queries
       , GetMintettes (..)
       , GetPermittedMintettes (..)
       , GetAddresses (..)
       , GetExplorers (..)
       , GetExplorersAndPeriods (..)
       , GetPeriodId (..)
       , GetHBlock (..)
       , GetHBlockWithMetadata (..)
       , GetHBlocks (..)
       , GetAllHBlocks (..)
       , GetUtxo (..)
       , GetLogs (..)
       , GetStatisticsId (..)

         -- | Updates
       , AddAddress (..)
       , AddMintette (..)
       , AddMintetteIfPermitted (..)
       , AddExplorer (..)
       , CheckAndBumpStatisticsId (..)
       , PermitMintette (..)
       , RestoreExplorers (..)
       , RemoveMintette (..)
       , RemoveExplorer (..)
       , SetExplorerPeriod (..)
       , SuspendExplorer (..)
       , StartNewPeriod (..)
       ) where

import           Control.Lens                  (Getter, to, view)
import           Control.Monad.Reader          (ask)
import           Control.Monad.Trans           (MonadIO)
import           Data.Acid                     (EventResult, EventState, Query,
                                                QueryEvent, UpdateEvent, makeAcidic)
import           Data.Aeson                    (encode, object, (.=))
import           Data.ByteString.Lazy.Char8    as BS hiding (filter, map)
import           Data.Maybe                    (fromMaybe)
import qualified Data.Set                      as Set
import           Data.Text                     (Text)
import           Formatting                    (bprint, stext, (%))
import           Safe                          (headMay)

import           Serokell.AcidState            (ExtendedState, closeExtendedState,
                                                openLocalExtendedState,
                                                openMemoryExtendedState, queryExtended,
                                                tidyExtendedState, updateExtended)
import           Serokell.AcidState.Statistics (StoragePart (..), estimateMemoryUsage)
import           Serokell.Data.Memory.Units    (Byte, memory)
import           Serokell.Util.Text            (listBuilderJSONIndent, show')

import           RSCoin.Core                   (ActionLog, AddressToTxStrategyMap,
                                                Explorer, Explorers, HBlock, MintetteId,
                                                Mintettes, PeriodId, PublicKey)
import qualified RSCoin.Core                   as C
import           RSCoin.Core.AesonJS           (utxoAsBalances)

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

getPermittedMintettes :: Query BS.Storage (Set.Set PublicKey)
getPermittedMintettes = view BS.getPermittedMintettes

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

getAllHBlocks :: Query BS.Storage [HBlock]
getAllHBlocks = view BS.getAllHBlocks

getUtxo :: Query BS.Storage C.Utxo
getUtxo = view BS.getUtxo

getLogs :: MintetteId -> Int -> Int -> Query BS.Storage (Maybe ActionLog)
getLogs m fromIdx toIdx = view $ BS.getLogs m fromIdx toIdx

getStatisticsId :: Query BS.Storage Int
getStatisticsId = view BS.getStatisticsId

$(makeAcidic ''BS.Storage
             [ 'getMintettes
             , 'getPermittedMintettes
             , 'getAddresses
             , 'getExplorers
             , 'getExplorersAndPeriods
             , 'getPeriodId
             , 'getHBlock
             , 'getHBlockWithMetadata
             , 'getHBlocks
             , 'getAllHBlocks
             , 'getUtxo
             , 'getLogs
             , 'getStatisticsId

             , 'getStorage

             , 'BS.addAddress
             , 'BS.addMintette
             , 'BS.addMintetteIfPermitted
             , 'BS.addExplorer
             , 'BS.permitMintette
             , 'BS.removeMintette
             , 'BS.removeExplorer
             , 'BS.setExplorerPeriod
             , 'BS.suspendExplorer
             , 'BS.restoreExplorers
             , 'BS.startNewPeriod
             , 'BS.checkAndBumpStatisticsId
             ])

dumpUtxo :: FilePath -> FilePath -> IO ()
dumpUtxo fp outputFp = do
    state  <- openState False fp
    utxo   <- query state GetUtxo
    BS.writeFile outputFp . encode $ object
      [ "utxo"   .= utxoAsBalances utxo
      ]
    pure ()

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
