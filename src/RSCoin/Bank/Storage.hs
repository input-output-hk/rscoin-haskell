{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

-- | Storage for Bank data

module RSCoin.Bank.Storage
       ( DeadMintetteState
       , Storage
       , mkStorage
       , Query
       , Update
       , ExceptUpdate
       , getMintettes
       , getAddresses
       , getAddressFromUtxo
       , getPeriodId
       , getHBlock
       , getHBlocks
       , getTransaction
       , getLogs
       , addMintette
       , addAddress
       , startNewPeriod
       ) where

import           Control.Lens              (Getter, ix, makeLenses, to, use,
                                            uses, (%=), (&), (+=), (.=), (.~))
import           Control.Monad             (forM_, guard, unless)
import           Control.Monad.Catch       (MonadThrow (throwM))
import           Control.Monad.State.Class (MonadState)
import           Data.Bifunctor            (first)
import           Data.Foldable             (foldl')
import qualified Data.HashMap.Lazy         as M
import qualified Data.HashSet              as S
import           Data.List                 (unfoldr, (\\))
import qualified Data.Map                  as MP
import           Data.Maybe                (mapMaybe)
import           Data.Tuple.Select         (sel3)
import           Data.Typeable             (Typeable)
import           Safe                      (atMay, headMay)

import           RSCoin.Core               (ActionLog,
                                            ActionLogEntry (CloseEpochEntry),
                                            AddrId, Address (..),
                                            AddressStrategyMap, Dpk,
                                            HBlock (..), Mintette, MintetteId,
                                            Mintettes, NewPeriodData (..),
                                            PeriodId, PeriodResult, PublicKey,
                                            SecretKey, Strategy,
                                            Transaction (..), TransactionId,
                                            Utxo, checkActionLog, checkLBlock,
                                            computeOutputAddrids,
                                            derivePublicKey, emissionHash, hash,
                                            lbTransactions, mkGenesisHBlock,
                                            mkHBlock, owners, sign)

import           Serokell.Util             (enumerate)

import           RSCoin.Bank.Error         (BankError (..))
import qualified RSCoin.Bank.Strategies    as Strategies

-- | DeadMintetteState represents state of mintette which was removed
-- from storage. It's needed to restore data if mintette resurrects.
data DeadMintetteState = DeadMintetteState
    { dmsActionLog :: ActionLog
    }

type DeadMintetteMap = MP.Map PublicKey DeadMintetteState

-- | Storage contains all the data used by Bank
data Storage = Storage
    { _mintettes        :: Mintettes                -- ^ List of active mintettes.
    , _pendingMintettes :: [(Mintette, PublicKey)]  -- ^ List of mintettes which were added
                                                    -- in current period and will become
                                                    -- active for the next period.
    , _periodId         :: PeriodId                 -- ^ Id of ongoing period. Doesn't
                                                    -- mean anything if there is no active
                                                    -- period.
    , _blocks           :: [HBlock]                 -- ^ List of all blocks from the
                                                    -- very beginning. Head of this list is
                                                    -- the most recent block.
    , _utxo             :: Utxo                     -- ^ Utxo for all the transaction
                                                    -- ever made.
    , _dpk              :: Dpk                      -- ^ DPK set for the ongoing period.
                                                    -- Doesn't mean anything if there is no
                                                    -- active period.
    , _actionLogs       :: [ActionLog]              -- ^ Mintettes' action logs. actionLogs[i]
                                                    -- stores action log for i-th mintette.
                                                    -- Head of action log is the most
                                                    -- recent entry.
    , _deadMintettes    :: DeadMintetteMap          -- ^ State of all known dead mintettes.
    , _transactionMap   :: MP.Map TransactionId Transaction
    , _addresses        :: AddressStrategyMap       -- ^ Known addresses accompanied with their strategies.
                                                    -- Note that every address with non-default strategy should be stored here in order to participate in transaction.
    , _pendingAddresses :: AddressStrategyMap       -- ^ Pending addresses to publish within next HBlock
    } deriving (Typeable)

$(makeLenses ''Storage)

-- | Make empty storage
mkStorage :: Storage
mkStorage =
    Storage
    { _mintettes = []
    , _pendingMintettes = []
    , _periodId = 0
    , _blocks = []
    , _utxo = MP.empty
    , _dpk = []
    , _actionLogs = []
    , _deadMintettes = MP.empty
    , _transactionMap = MP.empty
    , _addresses = MP.empty
    , _pendingAddresses = MP.empty
    }

type Query a = Getter Storage a

getAddresses :: Query AddressStrategyMap
getAddresses = addresses

getMintettes :: Query Mintettes
getMintettes = mintettes

getPeriodId :: Query PeriodId
getPeriodId = periodId

getHBlock :: PeriodId -> Query (Maybe HBlock)
getHBlock pId = blocks . to (\b -> b `atMay` (length b - pId - 1))

getAddressFromUtxo :: AddrId -> Query (Maybe Address)
getAddressFromUtxo addrId = utxo . to (MP.lookup addrId)

getTransaction :: TransactionId -> Query (Maybe Transaction)
getTransaction tId = transactionMap . to (MP.lookup tId)

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

-- | Add given address to storage and associate given strategy with it.
-- @TODO: Mind behaviour when address is being added more than once per period
addAddress :: Address -> Strategy -> Update ()
addAddress addr strategy = do
    curAddresses <- use addresses
    unless (addr `MP.member` curAddresses) $ pendingAddresses %= MP.insert addr strategy

-- | Add given mintette to storage and associate given key with it.
addMintette :: Mintette -> PublicKey -> Update ()
addMintette m k = do
    banksDpk <- use dpk
    unless (k `elem` map fst banksDpk) $ pendingMintettes %= ((m, k) :)

-- | When period finishes, Bank receives period results from
-- mintettes, updates storage and starts new period with potentially
-- different set of mintettes. Return value is a list of size (length
-- mintettes) of NewPeriodDatas that should be sent to mintettes.
startNewPeriod :: SecretKey
               -> [Maybe PeriodResult]
               -> ExceptUpdate [NewPeriodData]
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
    addresses' <- use addresses
    hblock' <- uses blocks head
    dpk' <- use dpk
    let npdPattern pl = NewPeriodData periodId' mintettes' hblock' pl dpk'
        usersNPDs =
          map (\i -> npdPattern ((,,) i <$> (i `MP.lookup` payload') <*> pure addresses'))
              [0 .. length currentMintettes - 1]
    return usersNPDs

startNewPeriodDo :: SecretKey
                 -> PeriodId
                 -> [Maybe PeriodResult]
                 -> ExceptUpdate [MintetteId]
startNewPeriodDo sk 0 _ =
    startNewPeriodFinally sk [] $ const mkGenesisHBlock
startNewPeriodDo sk pId results = do
    lastHBlock <- head <$> use blocks
    curDpk <- use dpk
    logs <- use actionLogs
    let keys = map fst curDpk
    unless (length keys == length results) $
        throwM $
        BEInconsistentResponse
            "Length of keys is different from the length of results"
    mts <- use mintettes
    let checkedResults =
            map (checkResult pId lastHBlock) $ zip3 results keys logs
        filteredResults =
            mapMaybe filterCheckedResults (zip [0 ..] checkedResults)
        blockTransactions =
            allocateCoins pk keys filteredResults pId :
            mergeTransactions mts filteredResults
    startNewPeriodFinally
        sk
        filteredResults
        (mkHBlock blockTransactions lastHBlock)
  where
    pk = derivePublicKey sk
    filterCheckedResults (idx,mres) = (idx, ) <$> mres

startNewPeriodFinally :: SecretKey
                      -> [(MintetteId, PeriodResult)]
                      -> (AddressStrategyMap -> SecretKey -> Dpk -> HBlock)
                      -> ExceptUpdate [MintetteId]
startNewPeriodFinally sk goodMintettes newBlockCtor = do
    periodId += 1
    updateIds <- updateMintettes sk goodMintettes
    newAddrs <- updateAddresses
    newBlock <- newBlockCtor newAddrs sk <$> use dpk
    updateUtxo $ hbTransactions newBlock
    blocks %= (newBlock:)
    transactionMap %= (\map' -> foldl' (\m t -> MP.insert (hash t) t m)
        map' (hbTransactions newBlock))
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
    guard $ pId == expectedPid
    guard $ checkActionLog (headMay storedLog) actionLog
    let logsToCheck =
            formLogsToCheck $ dropWhile (not . isCloseEpoch) actionLog
    let g3 = length logsToCheck == length lBlocks
    guard g3
    mapM_
        (\(blk,lg) ->
              guard $ checkLBlock key (hbHash lastHBlock) lg blk) $
        zip lBlocks logsToCheck
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
    Transaction
    { txInputs = [(emissionHash pId, 0, inputValue)]
    , txOutputs = (bankAddress, bankReward) : mintetteOutputs
    }
  where
    bankAddress = Address pk
    (bankReward,goodMintetteRewards) =
        Strategies.allocateCoins
            Strategies.AllocateCoinsDefault
            pId
            (map sel3 . map snd $ goodResults)
    inputValue = sum (bankReward : goodMintetteRewards)
    idxInGoodToGlobal idxInGood = fst $ goodResults !! idxInGood
    mintetteOutputs =
        map (first $ Address . (mintetteKeys !!) . idxInGoodToGlobal) $
        enumerate goodMintetteRewards

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

-- Given the list of bad indices, new list to append and data list,
-- this function returns datalist with appended data and removed bad
-- indices so that number of elements that change their place is
-- minimized.
replaceWithCare :: [Int] -> [a] -> [a] -> ([a], [Int])
replaceWithCare bad new old = replaceWithCare' bad new old []

replaceWithCare' :: [Int] -> [a] -> [a] -> [Int] -> ([a], [Int])
replaceWithCare' [] [] list acc = (list, acc)
replaceWithCare' [] pending list acc =
    (list ++ pending, acc ++ [length list .. length list + length pending - 1])
replaceWithCare' (bad:bads) [] list acc =
    replaceWithCare'
        bads
        []
        (take (length list - 1) list & ix bad .~ last list)
        (bad : acc)
replaceWithCare' (bad:bads) (pendh:pends) list acc =
    replaceWithCare'
        bads
        pends
        (list & ix bad .~ pendh)
        (bad : acc)

updateAddresses :: Update AddressStrategyMap
updateAddresses = do
  oldKnown <- use addresses
  pending <- use pendingAddresses
  addresses %= flip MP.union pending
  pendingAddresses .= MP.empty
  return $ pending MP.\\ oldKnown

-- type MintetteInfo = (Mintette, (PublicKey, Signature), ActionLog)
updateMintettes :: SecretKey
                -> [(MintetteId, PeriodResult)]
                -> ExceptUpdate [MintetteId]
updateMintettes sk goodMintettes = do
    pendingPairs <- use pendingMintettes
    let pendingMts = map fst pendingPairs
        pendingDpk = map doSign pendingPairs
    pendingLogs <- formPendingLogs pendingPairs
    existingMts <- use mintettes
    existingDpk <- use dpk
    appendNewLogs
    existingLogs <- use actionLogs
    let pendingMintetteInfo = zip3 pendingMts pendingDpk pendingLogs
        existingMintetteInfo = zip3 existingMts existingDpk existingLogs
        badIndices = [0 .. length existingMintetteInfo - 1] \\ goodIndices
        (newMintetteInfo,updatedIndices) =
            replaceWithCare badIndices pendingMintetteInfo existingMintetteInfo
        (newMintettes,newDpk,newActionLogs) = unzip3 newMintetteInfo
    storeDeadState badIndices
    pendingMintettes .= []
    mintettes .= newMintettes
    dpk .= newDpk
    actionLogs .= newActionLogs
    -- @TODO introduce a better solution if needed once
    if length existingMts /= length newMintettes
       -- Discard all mitettes' utxo lists in case of list lengths' mismatch
       -- Needed, cause `owners` function relies on the size of mintette list to provide an even distribution
       then return [0 .. length newMintettes - 1]
       else return updatedIndices
  where
    goodIndices = map fst goodMintettes
    doSign (_,mpk) = (mpk, sign sk mpk)
    formPendingLogs pendingPairs = do
        dm <- use deadMintettes
        return $
            map (maybe [] dmsActionLog . flip MP.lookup dm . snd) pendingPairs
    appendNewLogs = mapM_ appendNewLogDo goodMintettes
    appendNewLogDo (idx,(_,_,newLog)) = actionLogs . ix idx %= (newLog ++)

storeDeadState :: [MintetteId] -> ExceptUpdate ()
storeDeadState badIndices = do
    pks <- map fst <$> use dpk
    logs <- use actionLogs
    mapM_
        (\i ->
              deadMintettes %=
              MP.insert (pks !! i) (DeadMintetteState $ logs !! i))
        badIndices
