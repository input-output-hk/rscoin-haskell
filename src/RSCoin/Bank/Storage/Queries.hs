{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TupleSections         #-}

-- | Storage queries.

module RSCoin.Bank.Storage.Queries
       ( Query

         -- | Action logs
       , getAllActionLogs
       , getLogs

         -- | Participants
       , getExplorers
       , getExplorersAndPeriods
       , getMintettes
       , getPermittedMintettes

         -- | HBlocks
       , getAllHBlocks
       , getHBlock
       , getHBlockWithMetadata
       , getHBlocks

         -- | Other
       , getAddresses
       , getAddressFromUtxo
       , getDpk
       , getPeriodId
       , getStatisticsId
       , getUtxo
       ) where

import           Control.Lens                  (Getter, to)
import qualified Data.HashMap.Strict           as HM
import qualified Data.Set                      as Set
import           Safe                          (atMay)

import           RSCoin.Core                   (PeriodId)
import qualified RSCoin.Core                   as C

import qualified RSCoin.Bank.Storage.Addresses as AS
import qualified RSCoin.Bank.Storage.Explorers as ES
import qualified RSCoin.Bank.Storage.Mintettes as MS
import           RSCoin.Bank.Storage.Storage   (Storage, addressesStorage,
                                                blocks, explorersStorage,
                                                mintettesStorage, periodId,
                                                statisticsId, utxo)

type Query a = Getter Storage a

-- | Returns addresses (to strategies) map
getAddresses :: Query C.AddressToTxStrategyMap
getAddresses = addressesStorage . AS.getAddresses

-- | Resolves addrid into address using local utxo
getAddressFromUtxo :: C.AddrId -> Query (Maybe C.Address)
getAddressFromUtxo addrId = utxo . to (HM.lookup addrId)

-- | Returns mintettes list
getMintettes :: Query C.Mintettes
getMintettes = mintettesStorage . MS.getMintettes

getPermittedMintettes :: Query (Set.Set C.PublicKey)
getPermittedMintettes = mintettesStorage . MS.getPermittedMintettes

-- | Returns explorers list
getExplorers :: Query C.Explorers
getExplorers = explorersStorage . ES.getExplorers

-- | Returns a map from all available explorers and periodIds related
-- to them
getExplorersAndPeriods :: Query [(C.Explorer, C.PeriodId)]
getExplorersAndPeriods = explorersStorage . ES.getExplorersAndPeriods

-- | Get dpk
getDpk :: Query C.Dpk
getDpk = mintettesStorage . MS.getDpk



-- | Get current periodId
getPeriodId :: Query C.PeriodId
getPeriodId = periodId

-- | Get higher-level block by periodId.
getHBlock :: C.PeriodId -> Query (Maybe C.HBlock)
getHBlock pId = getHBlockWithMetadata pId . to (fmap C.wmValue)

-- | Get higher-level block by periodId with metadata.
getHBlockWithMetadata :: C.PeriodId -> Query (Maybe (C.WithMetadata C.HBlock C.HBlockMetadata))
getHBlockWithMetadata pId = blocks . to (\b -> b `atMay` (length b - pId - 1))

-- | Given two indices `(a,b)` swap them so `a < b` if needed, then
-- take exactly `b` elements of list that come after first `a`.
reverseFromTo :: Int -> Int -> [a] -> [a]
reverseFromTo from to' = drop small . take big . reverse
  where
    (small,big) = (min from to', max from to')

-- | Return all HBLocks that are in blockchain now
getAllHBlocks :: Query [C.HBlock]
getAllHBlocks = blocks . to (map C.wmValue)

-- | Return HBLocks that are in blockchain now
getHBlocks :: PeriodId -> PeriodId -> Query [C.HBlock]
getHBlocks left right =
    blocks . to (reverseFromTo left right) . to (map C.wmValue)

-- | Get all actionlogs
getAllActionLogs :: Query [C.ActionLog]
getAllActionLogs = mintettesStorage . MS.getActionLogs

-- | Get actionlogs
getLogs :: C.MintetteId -> Int -> Int -> Query (Maybe C.ActionLog)
getLogs m left right =
    getAllActionLogs . to (fmap (reverseFromTo left right) . (`atMay` m))

-- | Get utxo.
getUtxo :: Query C.Utxo
getUtxo = utxo

-- | Get statistics id.
getStatisticsId :: Query Int
getStatisticsId = statisticsId
