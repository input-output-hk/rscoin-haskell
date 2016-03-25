{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Storage for Bank data

module RSCoin.Bank.Storage
       ( Storage
       , mkStorage
       , getMintettes
       , getPeriodId
       , addMintette
       , startNewPeriod
       ) where

import           Control.Lens               (Getter, makeLenses, use, (%=),
                                             (+=), (.=))
import           Control.Monad              (guard, unless)
import           Control.Monad.State        (State)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.Typeable              (Typeable)

import           RSCoin.Core                (Dpk, HBlock (..), LBlock (..),
                                             Mintette, Mintettes, PeriodId,
                                             PeriodResult, PublicKey, hash,
                                             sign, verify)

import           RSCoin.Bank.Error          (BankError (BEInternal))

-- | Storage contains all the data used by Bank
data Storage = Storage
    { _mintettes        :: Mintettes
    , _pendingMintettes :: [(Mintette, PublicKey)]
    , _periodId         :: PeriodId
    , _blocks           :: [HBlock]
    , _dpk              :: Dpk
    } deriving (Typeable)

$(makeLenses ''Storage)

-- | Make empty storage
mkStorage :: Storage
mkStorage = Storage [] [] 0 [] []

type Query a = Getter Storage a

getMintettes :: Query Mintettes
getMintettes = mintettes

getPeriodId :: Query PeriodId
getPeriodId = periodId

type Update = State Storage
type ExceptUpdate = ExceptT BankError (State Storage)

-- | Add given mintette to storage and associate given key with it.
addMintette :: Mintette -> PublicKey -> Update ()
addMintette m k = pendingMintettes %= ((m, k):)

-- | When period finishes, Bank receives period results from mintettes,
-- updates storage and starts new period with potentially different set
-- of mintettes.
startNewPeriod :: [Maybe PeriodResult] -> ExceptUpdate ()
startNewPeriod results = do
    mts <- use mintettes
    unless (length mts == length results) $
        throwE $
        BEInternal
            "Length of results is different from the length of mintettes"
    pId <- use periodId
    startNewPeriodDo pId mts results

startNewPeriodDo :: PeriodId
                 -> Mintettes
                 -> [Maybe PeriodResult]
                 -> ExceptUpdate ()
startNewPeriodDo 0 _ _ = do
    startNewPeriodFinally [] undefined
startNewPeriodDo pId mts results = do
    lastHBlock <- head <$> use blocks
    curDpk <- use dpk
    let keys = map fst curDpk
    unless (length keys == length results) $
        throwE $
        BEInternal "Length of keys is different from the length of results"
    let checkedResults = map (checkResult pId lastHBlock) $ zip results keys
    undefined

startNewPeriodFinally :: [Int] -> HBlock -> ExceptUpdate ()
startNewPeriodFinally goodMintettes newBlock = do
    periodId += 1
    updateMintettes goodMintettes
    blocks %= (newBlock:)

checkResult :: PeriodId
            -> HBlock
            -> (Maybe PeriodResult, PublicKey)
            -> Maybe PeriodResult
checkResult expectedPid lastHBlock (r, key) = do
    (pId, lBlocks, logActions) <- r
    guard $ pId == expectedPid
    mapM_ (checkBlock (hbHash lastHBlock)) lBlocks
    r
  where
    checkBlock lastBankHash LBlock{..} = do
        guard $
            lbHash ==
            hash (lastBankHash, (undefined :: Int), lbHeads, lbTransactions)
        guard $ verify key lbSignature lbHash

updateMintettes :: [Int] -> ExceptUpdate ()
updateMintettes goodMintettes = do
    existing <- use mintettes
    pending <- use pendingMintettes
    mintettes .= map (existing !!) goodMintettes ++ map fst pending
    currentDpk <- use dpk
    dpk .= map (currentDpk !!) goodMintettes ++ map doSign pending
  where
    doSign (_, mpk) = (mpk, sign undefined mpk)
