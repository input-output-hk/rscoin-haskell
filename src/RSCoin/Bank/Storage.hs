{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

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
       , getHBlocks
       , getLogs
       , addMintette
       , startNewPeriod
       ) where

import           Control.Lens              (Getter, ix, makeLenses, to, use,
                                            uses, (%=), (&), (+=), (.=), (.~))
import           Control.Monad             (forM_, guard, unless)
import           Control.Monad.Catch       (MonadThrow (throwM))
import           Control.Monad.State.Class (MonadState)
import qualified Data.HashMap.Lazy         as M
import qualified Data.HashSet              as S
import           Data.List                 (unfoldr, (\\))
import qualified Data.Map                  as MP
import           Data.Maybe                (mapMaybe)
import           Data.Typeable             (Typeable)
import           Safe                      (atMay, headMay)

import           RSCoin.Core               (ActionLog,
                                            ActionLogEntry (CloseEpochEntry),
                                            AddrId, Address (..), Coin (..),
                                            Dpk, HBlock (..), Mintette,
                                            MintetteId, Mintettes,
                                            NewPeriodData (..), PeriodId,
                                            PeriodResult, PublicKey, SecretKey,
                                            Transaction (..), Utxo,
                                            checkActionLog, checkLBlock,
                                            computeOutputAddrids,
                                            derivePublicKey, emissionHash, hash,
                                            lbTransactions, mkGenesisHBlock,
                                            mkHBlock, owners, periodReward,
                                            sign)

import           RSCoin.Bank.Error         (BankError (..))

-- | Storage contains all the data used by Bank
data Storage = Storage
    { _mintettes        :: Mintettes
    , _pendingMintettes :: [(Mintette, PublicKey)]
    , _periodId         :: PeriodId
    , _blocks           :: [HBlock]
    , _utxo             :: Utxo
    , _dpk              :: Dpk
    , _actionLogs       :: [ActionLog]
    } deriving (Typeable)

$(makeLenses ''Storage)

-- | Make empty storage
mkStorage :: Storage
mkStorage = Storage [] [] 0 [] MP.empty [] []

type Query a = Getter Storage a

getMintettes :: Query Mintettes
getMintettes = mintettes

getPeriodId :: Query PeriodId
getPeriodId = periodId

getHBlock :: PeriodId -> Query (Maybe HBlock)
getHBlock pId = blocks . to (\b -> b `atMay` (length b - pId - 1))

-- Dumping Bank state

reverseFromTo :: Int -> Int -> [a] -> [a]
reverseFromTo from to' = drop small . take big . reverse
    where (small, big) = (min from to', max from to')

getHBlocks :: PeriodId -> PeriodId -> Query [HBlock]
getHBlocks left right = blocks . to (reverseFromTo left right)

getLogs :: MintetteId -> Int -> Int -> Query (Maybe ActionLog)
getLogs m left right =
    actionLogs . to (fmap (reverseFromTo left right) . (`atMay` m))

-- Dumping Bank state

type Update a = forall m . MonadState Storage m => m a
type ExceptUpdate a = forall m . (MonadThrow m, MonadState Storage m) => m a

-- | Add given mintette to storage and associate given key with it.
addMintette :: Mintette -> PublicKey -> Update ()
addMintette m k = do
    banksDpk <- use dpk
    unless (k `elem` map fst banksDpk) $ pendingMintettes %= ((m, k) :)

-- | When period finishes, Bank receives period results from
-- mintettes, updates storage and starts new period with potentially
-- different set of mintettes. Return value is a pair -- first is a
-- list of size (length mintettes) of NewPeriodDatas that should be
-- sent to mintettes. Second one is a fake NewPeriodData (shouldn't be
-- sent!) with "Nothing" in npdNewIdPayload field, just for logging.
startNewPeriod :: SecretKey
               -> [Maybe PeriodResult]
               -> ExceptUpdate ([NewPeriodData], NewPeriodData)
startNewPeriod sk results = do
    mts <- use mintettes
    unless (length mts == length results) $
        throwM $
        BEInconsistentResponse
            "Length of results is different from the length of mintettes"
    pId <- use periodId
    changedMintetteIx <- startNewPeriodDo sk pId results
    currentMintettes <- use mintettes
    payload' <- formPayload currentMintettes changedMintetteIx
    periodId' <- use periodId
    mintettes' <- use mintettes
    hblock' <- uses blocks head
    dpk' <- use dpk
    let npdPattern pl = NewPeriodData periodId' mintettes' hblock' pl dpk'
        usersNPDs =
          map (\i -> npdPattern ((i,) <$> (i `MP.lookup` payload')))
              [0 .. length currentMintettes - 1]
        fakeNPD = npdPattern Nothing
    return (usersNPDs, fakeNPD)

startNewPeriodDo :: SecretKey
                 -> PeriodId
                 -> [Maybe PeriodResult]
                 -> ExceptUpdate [MintetteId]
startNewPeriodDo sk 0 _ =
    startNewPeriodFinally sk [] mkGenesisHBlock
startNewPeriodDo sk pId results = do
    lastHBlock <- head <$> use blocks
    curDpk <- use dpk
    logs <- use actionLogs
    let keys = map fst curDpk
    unless (length keys == length results) $
        throwM $
        BEInconsistentResponse
            "Length of keys is different from the length of results"
    let checkedResults =
            map (checkResult pId lastHBlock) $ zip3 results keys logs
    let filteredResults =
            mapMaybe filterCheckedResults (zip [0 ..] checkedResults)
    mts <- use mintettes
    let pk = derivePublicKey sk
    let blockTransactions =
            allocateCoins pk keys filteredResults pId :
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
                      -> ExceptUpdate [MintetteId]
startNewPeriodFinally sk goodMintettes newBlockCtor = do
    periodId += 1
    updateIds <- updateMintettes sk goodMintettes
    newBlock <- newBlockCtor sk <$> use dpk
    updateUtxo $ hbTransactions newBlock
    blocks %= (newBlock:)
    return updateIds

updateUtxo :: [Transaction] -> ExceptUpdate ()
updateUtxo newTxs = do
    let shouldBeAdded = concatMap computeOutputAddrids newTxs
        shouldBeDeleted = concatMap txInputs newTxs
    utxo %= MP.union (MP.fromList shouldBeAdded)
    forM_ shouldBeDeleted (\d -> utxo %= MP.delete d)

checkResult :: PeriodId
            -> HBlock
            -> (Maybe PeriodResult, PublicKey, ActionLog)
            -> Maybe PeriodResult
checkResult expectedPid lastHBlock (r,key,storedLog) = do
    (pId,lBlocks,actionLog) <- r
    let g1 = guard $ pId == expectedPid
    let g2 = guard $ checkActionLog (headMay storedLog) actionLog
    g1
    g2
    let logsToCheck = formLogsToCheck actionLog
    let g3 = length logsToCheck == length lBlocks
    mapM_
        (\(blk,lg) ->
              guard $ checkLBlock key (hbHash lastHBlock) lg blk) $
        zip lBlocks (formLogsToCheck actionLog)
    r
  where
    formLogsToCheck = unfoldr step
    step [] = Nothing
    step actionLog = Just (actionLog, dropEpoch actionLog)
    dropEpoch = dropWhile (not . isCloseEpoch) . drop 1
    isCloseEpoch (CloseEpochEntry _,_) = True
    isCloseEpoch _ = False

allocateCoins :: PublicKey
              -> [PublicKey]
              -> [(MintetteId, PeriodResult)]
              -> PeriodId
              -> Transaction
allocateCoins pk mintetteKeys goodResults pId =
    Transaction{..}
  where
    bankAddress = Address pk
    awarded = map fst $ filter checkParticipation goodResults
    checkParticipation (_, (_, blks, _)) = checkParticipationBlocks blks
    checkParticipationBlocks [] = False
    checkParticipationBlocks (block:blks) =
        (not $ null $ lbTransactions block) || checkParticipationBlocks blks
    awardedCnt = fromIntegral $ length awarded
    mintetteReward = getCoin periodReward `div` (awardedCnt + 1)
    bankReward = getCoin periodReward - awardedCnt * mintetteReward
    mintetteOutputs =
        map
            (\idx -> (Address (mintetteKeys !! idx), Coin mintetteReward))
            awarded
    txOutputs = (bankAddress, Coin bankReward) : mintetteOutputs
    txInputs = [(emissionHash pId, 0, periodReward)]

mergeTransactions :: Mintettes -> [(MintetteId, PeriodResult)] -> [Transaction]
mergeTransactions mts goodResults =
    M.foldrWithKey appendTxChecked [] txMap
  where
    txMap :: M.HashMap Transaction (S.HashSet MintetteId)
    txMap = foldr insertResult M.empty goodResults
    insertResult (mintId, (_, blks, _)) m = foldr (insertBlock mintId) m blks
    insertBlock mintId blk m = foldr (insertTx mintId) m (lbTransactions blk)
    insertTx mintId tx m = M.insertWith S.union tx (S.singleton mintId) m
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

formPayload :: [a] -> [MintetteId] -> ExceptUpdate (MP.Map MintetteId Utxo)
formPayload mintettes' changedId = do
    curUtxo <- use utxo
    let payload = MP.foldlWithKey' gatherPayload MP.empty curUtxo
        gatherPayload :: MP.Map MintetteId Utxo
                      -> AddrId
                      -> Address
                      -> MP.Map MintetteId Utxo
        gatherPayload prev addrid@(txhash,_,_) address =
            MP.unionWith
                MP.union
                prev
                (MP.fromListWith MP.union $
                 mapMaybe
                     (\changed ->
                           if changed `elem` owners mintettes' txhash
                               then Just (changed, MP.singleton addrid address)
                               else Just (changed, MP.empty))
                     changedId)
    return payload

-- Given the list of bad indeces, new list to append, data list, and
-- generator (use []), this function returns datalist with appended
-- data and removed bad indeces so that number of elements that change
-- their place is minimized.
replaceWithCare :: [Int] -> [a] -> [a] -> [Int] -> ([a], [Int])
replaceWithCare [] [] list acc = (list, acc)
replaceWithCare [] pending list acc =
    (list ++ pending, acc ++ [length list .. length list + length pending - 1])
replaceWithCare (bad:bads) [] list acc =
    replaceWithCare
        bads
        []
        (take (length list - 1) list & ix bad .~ last list)
        (bad : acc)
replaceWithCare (bad:bads) (pendh:pends) list acc =
    replaceWithCare
        bads
        pends
        (list & ix bad .~ pendh)
        (bad : acc)

updateMintettes :: SecretKey -> [(MintetteId, PeriodResult)] -> ExceptUpdate [MintetteId]
updateMintettes sk goodMintettes = do
    let (goodIndices,goodResults) = unzip goodMintettes
    existing <- use mintettes
    pending <- use pendingMintettes
    let badIndices = [0 .. length existing - 1] \\ goodIndices
        (newMintettes,updatedIndices) =
            replaceWithCare badIndices (map fst pending) existing []
    mintettes .= newMintettes
    pendingMintettes .= []
    currentDpk <- use dpk
    dpk .= map (currentDpk !!) goodIndices ++ map doSign pending
    currentLogs <- use actionLogs
    actionLogs .= map (appendNewLog currentLogs) (zip goodIndices goodResults) ++
        replicate (length pending) []
    return updatedIndices
  where
    doSign (_,mpk) = (mpk, sign sk mpk)
    appendNewLog :: [ActionLog] -> (MintetteId, PeriodResult) -> ActionLog
    appendNewLog currentLogs (i,(_,_,newLog)) = newLog ++ currentLogs !! i
