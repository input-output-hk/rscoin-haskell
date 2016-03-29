{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

-- | Storage for Bank data

module RSCoin.Bank.Storage
       ( Storage
       , mkStorage
       , Query
       , Update
       , ExceptUpdate
       , getMintettes
       , getPeriodId
       , getHBlock
       , addMintette
       , startNewPeriod
       ) where

import           Control.Lens               (Getter, makeLenses, to, use, (%=),
                                             (+=), (.=))
import           Control.Monad              (guard, unless)
import           Control.Monad.State        (State)
import           Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.HashMap.Lazy          as M
import qualified Data.HashSet               as S
import           Data.Maybe                 (catMaybes)
import           Data.Typeable              (Typeable)
import           Safe                       (atMay, headMay)

import           RSCoin.Core                (ActionLog, Address (..), Coin (..),
                                             Dpk, HBlock (..), Mintette,
                                             MintetteId, Mintettes,
                                             NewPeriodData (..), PeriodId,
                                             PeriodResult, PublicKey, SecretKey,
                                             Transaction (..), checkActionLog,
                                             checkLBlock, derivePublicKey, hash,
                                             lbTransactions, mkGenesisHBlock,
                                             mkHBlock, owners, periodReward,
                                             sign)

import           RSCoin.Bank.Error          (BankError (BEInternal))

-- | Storage contains all the data used by Bank
data Storage = Storage
    { _mintettes        :: Mintettes
    , _pendingMintettes :: [(Mintette, PublicKey)]
    , _periodId         :: PeriodId
    , _blocks           :: [HBlock]
    , _dpk              :: Dpk
    , _actionLogs       :: [ActionLog]
    } deriving (Typeable)

$(makeLenses ''Storage)

-- | Make empty storage
mkStorage :: Storage
mkStorage = Storage [] [] 0 [] [] []

type Query a = Getter Storage a

getMintettes :: Query Mintettes
getMintettes = mintettes

getPeriodId :: Query PeriodId
getPeriodId = periodId

getHBlock :: PeriodId -> Query (Maybe HBlock)
getHBlock pId = blocks . to (flip atMay pId)

type Update = State Storage
type ExceptUpdate = ExceptT BankError (State Storage)

-- | Add given mintette to storage and associate given key with it.
addMintette :: Mintette -> PublicKey -> Update ()
addMintette m k = pendingMintettes %= ((m, k):)

-- | When period finishes, Bank receives period results from mintettes,
-- updates storage and starts new period with potentially different set
-- of mintettes.
startNewPeriod :: SecretKey
               -> [Maybe PeriodResult]
               -> ExceptUpdate NewPeriodData
startNewPeriod sk results = do
    mts <- use mintettes
    unless (length mts == length results) $
        throwE $
        BEInternal
            "Length of results is different from the length of mintettes"
    pId <- use periodId
    startNewPeriodDo sk pId results
    NewPeriodData <$> use periodId <*> use mintettes <*> use dpk

startNewPeriodDo :: SecretKey
                 -> PeriodId
                 -> [Maybe PeriodResult]
                 -> ExceptUpdate ()
startNewPeriodDo sk 0 _ = do
    startNewPeriodFinally sk [] mkGenesisHBlock
startNewPeriodDo sk pId results = do
    lastHBlock <- head <$> use blocks
    curDpk <- use dpk
    logs <- use actionLogs
    let keys = map fst curDpk
    unless (length keys == length results) $
        throwE $
        BEInternal "Length of keys is different from the length of results"
    let checkedResults =
            map (checkResult pId lastHBlock) $ zip3 results keys logs
    let filteredResults =
            catMaybes $ map filterCheckedResults $ zip [0 ..] checkedResults
    mts <- use mintettes
    let pk = derivePublicKey sk
    let blockTransactions =
            allocateCoins pk keys filteredResults :
            mergeTransactions mts filteredResults
    startNewPeriodFinally
        sk
        filteredResults
        (mkHBlock blockTransactions lastHBlock)
  where
    filterCheckedResults (idx,mres) = (idx, ) <$> mres

startNewPeriodFinally :: SecretKey
                      -> [(MintetteId, PeriodResult)]
                      -> (SecretKey -> Dpk -> HBlock)
                      -> ExceptUpdate ()
startNewPeriodFinally sk goodMintettes newBlockCtor = do
    periodId += 1
    updateMintettes sk goodMintettes
    newBlock <- newBlockCtor sk <$> use dpk
    blocks %= (newBlock:)

checkResult :: PeriodId
            -> HBlock
            -> (Maybe PeriodResult, PublicKey, ActionLog)
            -> Maybe PeriodResult
checkResult expectedPid lastHBlock (r, key, storedLog) = do
    (pId, lBlocks, actionLog) <- r
    guard $ pId == expectedPid
    guard $ checkActionLog (headMay storedLog) actionLog
    mapM_ (guard . checkLBlock key (hbHash lastHBlock) actionLog) lBlocks
    r

allocateCoins :: PublicKey
              -> [PublicKey]
              -> [(MintetteId, PeriodResult)]
              -> Transaction
allocateCoins pk mintetteKeys goodResults =
    Transaction
    { txInputs = []
    , ..
    }
  where
    bankAddress = Address pk
    awarded = map fst $ filter checkParticipation goodResults
    checkParticipation (_, (_, blks, _)) = checkParticipationBlocks blks
    checkParticipationBlocks [] = False
    checkParticipationBlocks (block:blks) =
        (not $ null $ lbTransactions block) || checkParticipationBlocks blks
    awardedCnt = fromIntegral $ length awarded
    mintetteReward = (getCoin periodReward) `div` (awardedCnt + 1)
    bankReward = (getCoin periodReward) - awardedCnt * mintetteReward
    mintetteOutputs =
        map
            (\idx -> (Address (mintetteKeys !! idx), Coin mintetteReward))
            awarded
    txOutputs = (bankAddress, Coin bankReward) : mintetteOutputs

mergeTransactions :: Mintettes -> [(MintetteId, PeriodResult)] -> [Transaction]
mergeTransactions mts goodResults = M.foldrWithKey appendTxChecked [] txMap
  where
    txMap :: M.HashMap Transaction (S.HashSet MintetteId)
    txMap = foldr insertResult M.empty goodResults
    insertResult (mintId, (_, blks, _)) m = foldr (insertBlock mintId) m blks
    insertBlock mintId blk m = foldr (insertTx mintId) m (lbTransactions blk)
    insertTx mintId tx m = M.insertWith (S.union) tx (S.singleton mintId) m
    appendTxChecked :: Transaction
                    -> S.HashSet MintetteId
                    -> [Transaction]
                    -> [Transaction]
    appendTxChecked tx committedMintettes
      | checkMajority tx committedMintettes = (tx :)
      | otherwise = id
    checkMajority :: Transaction -> S.HashSet MintetteId -> Bool
    checkMajority tx committedMintettes =
        let ownersSet = S.fromList $ owners mts (hash tx)
        in S.size (ownersSet `S.intersection` committedMintettes) >
           (S.size ownersSet `div` 2)

updateMintettes :: SecretKey -> [(MintetteId, PeriodResult)] -> ExceptUpdate ()
updateMintettes sk goodMintettes = do
    let (goodIndices, goodResults) = unzip goodMintettes
    existing <- use mintettes
    pending <- use pendingMintettes
    mintettes .= map (existing !!) goodIndices ++ map fst pending
    pendingMintettes .= []
    currentDpk <- use dpk
    dpk .= map (currentDpk !!) goodIndices ++ map doSign pending
    currentLogs <- use actionLogs
    actionLogs .= map (appendNewLog currentLogs) (zip goodIndices goodResults) ++
        replicate (length pending) []
  where
    doSign (_, mpk) = (mpk, sign sk mpk)
    appendNewLog :: [ActionLog] -> (MintetteId, PeriodResult) -> ActionLog
    appendNewLog currentLogs (i, (_, _, newLog)) = newLog ++ currentLogs !! i
