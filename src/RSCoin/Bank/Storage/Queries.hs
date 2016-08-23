{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TupleSections         #-}

-- | Storage queries.

module RSCoin.Bank.Storage.Queries
       ( Query

       , getAddresses
       , getAddressFromUtxo
       , getDpk
       , getEmission
       , getEmissions
       , getExplorers
       , getExplorersAndPeriods
       , getHBlock
       , getHBlocks
       , getLogs
       , getMintettes
       , getPeriodId
       ) where

import           Control.Lens                  (Getter, to)
import qualified Data.Map                      as MP
import           Safe                          (atMay)

import           RSCoin.Core                   (PeriodId)
import qualified RSCoin.Core                   as C

import qualified RSCoin.Bank.Storage.Addresses as AS
import qualified RSCoin.Bank.Storage.Explorers as ES
import qualified RSCoin.Bank.Storage.Mintettes as MS
import           RSCoin.Bank.Storage.Storage   (Storage, addressesStorage,
                                                blocks, emissionHashes,
                                                explorersStorage,
                                                mintettesStorage, periodId,
                                                utxo)

type Query a = Getter Storage a

-- | Returns emission hash made in provided period
getEmission :: C.PeriodId -> Query (Maybe C.TransactionId)
getEmission pId = emissionHashes . to (\b -> b `atMay` (length b - pId))

-- | Return emission hashes provided in period range
getEmissions :: C.PeriodId -> C.PeriodId -> Query [C.TransactionId]
getEmissions left right = emissionHashes . to (reverseFromTo (max left 1) right)

-- | Returns addresses (to strategies) map
getAddresses :: Query C.AddressToTxStrategyMap
getAddresses = addressesStorage . AS.getAddresses

-- | Resolves addrid into address using local utxo
getAddressFromUtxo :: C.AddrId -> Query (Maybe C.Address)
getAddressFromUtxo addrId = utxo . to (MP.lookup addrId)

-- | Returns mintettes list
getMintettes :: Query C.Mintettes
getMintettes = mintettesStorage . MS.getMintettes

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

-- | Get last block by periodId
getHBlock :: C.PeriodId -> Query (Maybe C.HBlock)
getHBlock pId = blocks . to (\b -> b `atMay` (length b - pId - 1))

-- | Given two indices `(a,b)` swap them so `a < b` if needed, then
-- take exactly `b` elements of list that come after first `a`.
reverseFromTo :: Int -> Int -> [a] -> [a]
reverseFromTo from to' = drop small . take big . reverse
    where (small, big) = (min from to', max from to')

-- | Return HBLocks that are in blockchain now
getHBlocks :: PeriodId -> PeriodId -> Query [C.HBlock]
getHBlocks left right = blocks . to (reverseFromTo left right)

-- | Get actionlogs
getLogs :: C.MintetteId -> Int -> Int -> Query (Maybe C.ActionLog)
getLogs m left right =
    mintettesStorage .
    MS.getActionLogs . to (fmap (reverseFromTo left right) . (`atMay` m))
