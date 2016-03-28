-- | Functions tightly related to LBlock

module RSCoin.Core.LBlock
       ( mkLBlock
       , checkLBlock
       ) where

import           Control.Exception      (assert)

import           RSCoin.Core.Crypto     (Hash, PublicKey, SecretKey, hash, sign,
                                         verify)
import           RSCoin.Core.Primitives (Transaction)
import           RSCoin.Core.Types      (ActionLog,
                                         ActionLogEntry (CloseEpochEntry),
                                         ActionLogHeads, LBlock (..))

-- | Construct lower-level block from txset, mintette's secret key,
-- hash of previous higher-level block, heads of known log chains
-- and action log up to (and not including) CloseEpoch action.
mkLBlock :: [Transaction]
         -> SecretKey
         -> Hash
         -> ActionLogHeads
         -> ActionLog
         -> LBlock
mkLBlock lbTransactions sk prevHBlockHash lbHeads actionLog =
    assert (not $ null actionLog) $ LBlock { .. }
  where
    prevActionHead = snd $ head actionLog
    lbHash = hash (prevHBlockHash, prevActionHead, lbHeads, lbTransactions)
    lbSignature = sign sk lbHash

-- | Check that lower-level block is valid using mintette's public key,
-- hash of previous higher-level block and action log sent with block.
checkLBlock :: PublicKey -> Hash -> ActionLog -> LBlock -> Bool
checkLBlock pk prevHBlockHash actionLog LBlock{..} =
    and [ length actionLog >= 2
        , isCloseEpoch $ fst $ head actionLog
        , lbHash == hash (prevHBlockHash, prevActionHead, lbHeads, lbTransactions)
        , verify pk lbSignature lbHash
        ]
  where
    isCloseEpoch (CloseEpochEntry _) = True
    isCloseEpoch _ = False
    prevActionHead = snd $ actionLog !! 1
