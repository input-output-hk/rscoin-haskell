{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Storage for Bank data

module Storage
       ( Storage
       , mkStorage
       , getMintettes
       , getPeriodId
       , addMintette
       , startNewPeriod
       ) where

import           Control.Lens               (Getter, makeLenses, to, use, (%=))
import           Control.Monad              (guard, unless)
import           Control.Monad.State        (State)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import           Data.Typeable              (Typeable)

import           RSCoin.Core                (HBlock (..), LBlock (..), Mintette,
                                             Mintettes, PeriodId, PeriodResult,
                                             PublicKey, hash, verify)

import           Error                      (BankError (BEInternal))

-- | Storage contains all the data used by Bank
data Storage = Storage
    { _mintettes        :: [(Mintette, PublicKey)]
    , _pendingMintettes :: [(Mintette, PublicKey)]
    , _periodId         :: PeriodId
    , _blocks           :: [HBlock]
    } deriving (Typeable)

$(makeLenses ''Storage)

-- | Make empty storage
mkStorage :: Storage
mkStorage = Storage [] [] 0 []

type Query a = Getter Storage a

getMintettes :: Query Mintettes
getMintettes = mintettes . to (map fst)

getPeriodId :: Query PeriodId
getPeriodId = periodId

type Update = State Storage
type ExceptUpdate = ExceptT BankError (State Storage)

-- | Add given mintette to storage and associate given key with it.
-- Overrides existing record if it already exists.
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
    lastHBlock <- head <$> use blocks
    let keys = map snd mts
    let checkedResults = map (checkResult pId lastHBlock) $ zip results keys
    undefined
  where
    checkResult expectedPid lastHBlock (r,key) = do
        (pId, lBlocks, logActions) <- r
        guard $ pId == expectedPid
        mapM_ (checkBlock (hbHash lastHBlock) key) lBlocks
        return r
    checkBlock lastBankHash key LBlock{..} = do
        guard $
            lbHash ==
            hash (lastBankHash, (undefined :: Int), lbHeads, lbTransactions)
        guard $ verify key lbSignature lbHash
