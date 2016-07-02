-- | Functions tightly related to HBlock

module RSCoin.Core.HBlock
       ( initialTx
       , mkHBlock
       , mkGenesisHBlock
       , checkHBlock
       ) where

import qualified Data.Map               as M
import           RSCoin.Core.Constants  (genesisAddress, genesisValue)
import           RSCoin.Core.Crypto     (Hash, PublicKey, SecretKey, hash, sign,
                                         verify)
import           RSCoin.Core.Primitives (Transaction (..))
import           RSCoin.Core.Types      (AddressStrategyMap, Dpk, HBlock (..))

initialHash :: Hash
initialHash = hash ()

initialTx :: Transaction
initialTx =
    Transaction
    { txInputs = []
    , txOutputs = [(genesisAddress, genesisValue)]
    }

-- | Construct higher-level block from txset, Bank's secret key, DPK
-- and previous block.
mkHBlock :: [Transaction] -> HBlock -> AddressStrategyMap -> SecretKey -> Dpk -> HBlock
mkHBlock txset prevBlock newAddrs sk dpk = mkHBlockDo txset newAddrs sk dpk (hbHash prevBlock)

-- | Construct genesis higher-level block using Bank's secret key and DPK.
mkGenesisHBlock :: SecretKey -> Dpk -> HBlock
mkGenesisHBlock sk dpk = mkHBlockDo [initialTx] M.empty sk dpk initialHash

mkHBlockDo :: [Transaction] -> AddressStrategyMap -> SecretKey -> Dpk -> Hash -> HBlock
mkHBlockDo hbTransactions hbAddresses sk hbDpk prevHash = HBlock {..}
  where
    hbHash = hash (prevHash, hbTransactions)
    hbSignature = sign sk hbHash

-- | Check that higher-level block is valid using Bank's public key
-- and previous block (unless it's genesis block).
checkHBlock :: PublicKey -> Maybe HBlock -> HBlock -> Bool
checkHBlock pk Nothing blk = checkHBlockDo pk initialHash blk
checkHBlock pk (Just b) blk = checkHBlockDo pk (hbHash b) blk

checkHBlockDo :: PublicKey -> Hash -> HBlock -> Bool
checkHBlockDo pk prevHash HBlock{..} =
    and (checkHash : checkSignature : map checkDpk hbDpk)
  where
    checkHash = hbHash == hash (prevHash, hbTransactions)
    checkSignature = verify pk hbSignature hbHash
    checkDpk (mPk,signature) = verify pk signature mPk
