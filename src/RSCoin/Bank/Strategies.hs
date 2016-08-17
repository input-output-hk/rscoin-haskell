-- | This module defines data types for various strategies which Bank
-- may use for coins allocation, fraud detection, etc. It also
-- contains implementation of such strategies.

module RSCoin.Bank.Strategies
       ( AllocateCoinsF
       , AllocateCoinsStrategy (..)
       , allocateCoins
       ) where

import qualified Data.Set    as S

import qualified RSCoin.Core as C

-- | Function of type AllocateCoinsF takes id of finished period, list
-- of new action log for each mintette and returns pair of two
-- values. The first value specifies how much coins should be
-- transfered to bank's address. The second value is a list, where
-- i-th element specifies how much coins should be transfered to i-th
-- Mintette's address.
type AllocateCoinsF = C.PeriodId -> [C.ActionLog] -> (C.Coin, [C.Coin])

-- | This data type lists all existing strategies for coins
-- allocation.
data AllocateCoinsStrategy =
    AllocateCoinsDefault  -- ^ Default strategy generates `C.periodReward`
                          -- coins and awards them only to the bank.
  | AllocateCoinsToMintettes -- ^ This strategy ignores mintettes
                             -- whose log is completely empty, generates
                             -- `C.periodReward` coins and distributes
                             -- them among bank and participating
                             -- mintettes evenly (bank may get slightly
                             -- more because of rounding)

allocateCoins :: AllocateCoinsStrategy -> AllocateCoinsF
allocateCoins AllocateCoinsDefault = allocateCoinsDefault
allocateCoins AllocateCoinsToMintettes = allocateCoinsToMintettes

allocateCoinsDefault :: AllocateCoinsF
allocateCoinsDefault _ actionLogs = (bankReward, mintetteRewards)
  where
    mintetteCnt = length actionLogs
    mintetteRewards = replicate mintetteCnt 0
    bankReward = C.periodReward


allocateCoinsToMintettes :: AllocateCoinsF
allocateCoinsToMintettes _ actionLogs = (bankReward, mintetteRewards)
  where
    mintetteCnt = length actionLogs
    awarded =
        S.fromList . map fst . filter (not . null . snd) $
        zip [0 ..] actionLogs
    awardedCnt = fromIntegral $ S.size awarded
    -- FIXME: is this ok?
    mintetteReward = (fromIntegral :: Integer -> C.Coin) . floor $
        C.getCoin C.periodReward / C.getCoin (awardedCnt + 1)
    bankReward = C.periodReward - awardedCnt * mintetteReward
    mintetteRewards =
        map
            (\i ->
                  if i `S.member` awarded
                      then mintetteReward
                      else 0)
            [0 .. mintetteCnt - 1]
