{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

-- | Storage containing whole bank's data.

module RSCoin.Bank.Storage.Whole
       ( Storage
       , mkStorage
       , Query
       , Update
       , ExceptUpdate
       , explorersStorage
       , getEmission
       , getEmissions
       , getAddresses
       , getAddressFromUtxo
       , getMintettes
       , getExplorers
       , getExplorersAndPeriods
       , getPeriodId
       , getHBlock
       , getHBlocks
       , getTransaction
       , getLogs
       , addAddress
       , addMintette
       , addExplorer
       , setExplorerPeriod
       , suspendExplorer
       , restoreExplorers
       , startNewPeriod
       ) where

import           Control.Lens                  (Getter, makeLenses, to, use,
                                                uses, (%%=), (%=), (+=), (.=), (^.))
import           Control.Monad                 (forM_, guard, unless, when)
import           Control.Monad.Catch           (MonadThrow (throwM))
import           Control.Monad.State           (MonadState, execState, runState)
import           Data.Bifunctor                (first)
import           Data.Foldable                 (foldl')
import qualified Data.HashMap.Lazy             as M
import qualified Data.HashSet                  as S
import           Data.List                     (unfoldr)
import qualified Data.Map                      as MP
import           Data.Maybe                    (fromJust, isJust, mapMaybe)
import           Data.SafeCopy                 (base, deriveSafeCopy)
import           Data.Tuple.Select             (sel3)
import           Data.Typeable                 (Typeable)
import           Safe                          (atMay, headMay)

import           Serokell.Util                 (enumerate)

import           RSCoin.Core                   (ActionLog,
                                                ActionLogEntry (CloseEpochEntry),
                                                AddrId, Address (..), Dpk,
                                                HBlock (..), MintetteId,
                                                Mintettes, NewPeriodData (..),
                                                PeriodId, PeriodResult,
                                                PublicKey, SecretKey,
                                                Transaction (..), bankPublicKey,
                                                checkActionLog, checkLBlock,
                                                computeOutputAddrids,
                                                emissionHash, hash,
                                                lbTransactions, mkGenesisHBlock,
                                                mkHBlock, owners)
import qualified RSCoin.Core                   as C
import           RSCoin.Core.NodeConfig        (NodeContext)

import           RSCoin.Bank.Error             (BankError (..))
import qualified RSCoin.Bank.Storage.Explorers as ES
import qualified RSCoin.Bank.Storage.Mintettes as MS
import qualified RSCoin.Bank.Strategies        as Strategies

-- | Storage contains all the data used by Bank.
data Storage = Storage
    {
      -- | Data about mintettes.
      _mintettesStorage :: MS.MintettesStorage
      -- | Data about explorers.
    , _explorersStorage :: ES.ExplorersStorage
      -- | Id of ongoing period. Doesn't mean anything if there is no
      -- active period.
    , _periodId         :: C.PeriodId
      -- | List of all blocks from the very beginning. Head of this
      -- list is the most recent block.
    , _blocks           :: [C.HBlock]
      -- | Utxo for all the transaction ever made.
    , _utxo             :: C.Utxo
      -- | Mapping from transaction id to actual transaction with this id.
    , _transactionMap   :: MP.Map C.TransactionId C.Transaction
      -- | List off all emission hashes from the very beginning.
    , _emissionHashes   :: [C.TransactionId]
      -- | Known addresses accompanied with their strategies. Note that every address with
      -- non-default strategy should be stored here in order to participate in transaction.
    , _addresses        :: C.AddressToTxStrategyMap
      -- | Pending addresses to publish within next HBlock.
    , _pendingAddresses :: C.AddressToTxStrategyMap
    } deriving (Typeable)

$(makeLenses ''Storage)
$(deriveSafeCopy 0 'base ''Storage)

-- | Make empty storage
mkStorage :: Storage
mkStorage =
    Storage
    { _mintettesStorage = MS.mkMintettesStorage
    , _explorersStorage = ES.mkExplorersStorage
    , _periodId = 0
    , _blocks = []
    , _utxo = MP.empty
    , _transactionMap = MP.empty
    , _emissionHashes = []
    , _addresses = MP.empty
    , _pendingAddresses = MP.empty
    }

type Query a = Getter Storage a

-- | Returns emission hash made in provided period
getEmission :: C.PeriodId -> Query (Maybe C.TransactionId)
getEmission pId = emissionHashes . to (\b -> b `atMay` (length b - pId))

-- | Return emission hashes provided in period range
getEmissions :: PeriodId -> PeriodId -> Query [C.TransactionId]
getEmissions left right = emissionHashes . to (reverseFromTo (max left 1) right)

-- | Returns addresses (to strategies) map
getAddresses :: Query C.AddressToTxStrategyMap
getAddresses = addresses

-- | Resolves addrid into address using local utxo
getAddressFromUtxo :: AddrId -> Query (Maybe Address)
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

-- | Resolve transaction hash into transaction
getTransaction :: C.TransactionId -> Query (Maybe C.Transaction)
getTransaction tId = transactionMap . to (MP.lookup tId)

-- Dumping Bank state

-- | Given two indices `(a,b)` swap them so `a < b` if needed, then
-- take exactly `b` elements of list that come after first `a`.
reverseFromTo :: Int -> Int -> [a] -> [a]
reverseFromTo from to' = drop small . take big . reverse
    where (small, big) = (min from to', max from to')

-- | Return HBLocks that are in blockchain now
getHBlocks :: PeriodId -> PeriodId -> Query [HBlock]
getHBlocks left right = blocks . to (reverseFromTo left right)

-- | Get actionlogs
getLogs :: MintetteId -> Int -> Int -> Query (Maybe ActionLog)
getLogs m left right =
    mintettesStorage .
    MS.getActionLogs . to (fmap (reverseFromTo left right) . (`atMay` m))

-- Dumping Bank state

type Update a = forall m . MonadState Storage m => m a
type ExceptUpdate a = forall m . (MonadThrow m, MonadState Storage m) => m a

-- | Add given address to storage and associate given strategy with it.
-- @TODO: Mind behaviour when address is being added more than once per period
addAddress :: Address -> C.TxStrategy -> Update ()
addAddress addr strategy = do
    curAddresses <- use addresses
    unless (addr `MP.member` curAddresses) $ pendingAddresses %= MP.insert addr strategy

-- | Add given mintette to storage and associate given key with it.
addMintette :: C.Mintette -> C.PublicKey -> Update ()
addMintette m k = mintettesStorage %= execState (MS.addMintette m k)

-- | Add given explorer to storage and associate given PeriodId with
-- it. If explorer exists, it is updated.
addExplorer :: C.Explorer -> C.PeriodId -> Update ()
addExplorer e expectedPid =
    explorersStorage %= execState (ES.addExplorer e expectedPid)

-- | Update expected period id of given explorer. Adds explorer if it
-- doesn't exist.
setExplorerPeriod :: C.Explorer -> C.PeriodId -> Update ()
setExplorerPeriod e expectedPid =
    explorersStorage %= execState (ES.setExplorerPeriod e expectedPid)

-- | Temporarily delete explorer from storage until `restoreExplorers`
-- is called.
suspendExplorer :: C.Explorer -> Update ()
suspendExplorer e = explorersStorage %= execState (ES.suspendExplorer e)

-- | Restore all suspended explorers.
restoreExplorers :: Update ()
restoreExplorers = explorersStorage %= execState ES.restoreExplorers

-- | When period finishes, Bank receives period results from
-- mintettes, updates storage and starts new period with potentially
-- different set of mintettes. Return value is a list of size (length
-- mintettes) of NewPeriodDatas that should be sent to mintettes.
startNewPeriod
    :: NodeContext
    -> SecretKey
    -> [Maybe PeriodResult]
    -> ExceptUpdate [NewPeriodData]
startNewPeriod nodeCtx sk results = do
    mintettes <- use getMintettes
    unless (length mintettes == length results) $
        throwM $
        BEInconsistentResponse
            "Length of results is different from the length of mintettes"
    pId <- use periodId
    changedMintetteIx <- startNewPeriodDo nodeCtx sk pId results
    currentMintettes <- use getMintettes
    payload' <- formPayload currentMintettes changedMintetteIx
    periodId' <- use periodId
    mintettes' <- use getMintettes
    addresses' <- use addresses
    hblock' <- uses blocks head
    dpk <- use getDpk
    let npdPattern pl = NewPeriodData periodId' mintettes' hblock' pl dpk
        usersNPDs =
          map (\i -> npdPattern ((i,,) <$> (i `MP.lookup` payload') <*> pure addresses'))
              [0 .. length currentMintettes - 1]
    return usersNPDs

-- | Calls a startNewPeriodFinally, previously processing
-- PeriodResults, sorting them relatevely to logs and dpk. Also
-- merging LBlocks and adding generative transaction.
startNewPeriodDo
    :: NodeContext
    -> SecretKey
    -> PeriodId
    -> [Maybe PeriodResult]
    -> ExceptUpdate [MintetteId]
startNewPeriodDo nodeCtx sk 0 _ =
    startNewPeriodFinally sk [] (const $ mkGenesisHBlock nodeCtx) Nothing
startNewPeriodDo nodeCtx sk pId results = do
    lastHBlock <- head <$> use blocks
    curDpk <- use getDpk
    logs <- use $ mintettesStorage . MS.getActionLogs
    let keys = map fst curDpk
    unless (length keys == length results) $
        throwM $
        BEInconsistentResponse
            "Length of keys is different from the length of results"
    mintettes <- use getMintettes
    let checkedResults =
            map (checkResult pId lastHBlock) $ zip3 results keys logs
        filteredResults =
            mapMaybe filterCheckedResults (zip [0 ..] checkedResults)
        emissionTransaction = allocateCoins nodeCtx keys filteredResults pId
        checkEmission [(tid,_,_)] = return tid
        checkEmission _ = throwM $ BEInternal "Emission transaction should have one transaction hash"
        blockTransactions =
            emissionTransaction : mergeTransactions mintettes filteredResults
    emissionTransactionId <- checkEmission $ C.txInputs emissionTransaction
    startNewPeriodFinally
        sk
        filteredResults
        (mkHBlock blockTransactions lastHBlock)
        (Just emissionTransactionId)
  where
    filterCheckedResults (idx,mres) = (idx, ) <$> mres

-- | Finalize the period start. Update mintettes, addresses, create a
-- new block, add transactions to transaction resolving map. Return a
-- list of mintettes that should update their utxo.
startNewPeriodFinally
    :: SecretKey
    -> [(MintetteId, PeriodResult)]
    -> (C.AddressToTxStrategyMap -> SecretKey -> Dpk -> HBlock)
    -> C.EmissionId
    -> ExceptUpdate [MintetteId]
startNewPeriodFinally sk goodMintettes newBlockCtor emissionTid = do
    periodId += 1
    updateIds <- updateMintettes sk goodMintettes
    newAddrs <- updateAddresses
    newBlock <- newBlockCtor newAddrs sk <$> use getDpk
    updateUtxo $ hbTransactions newBlock
    -- TODO: this can be written more elegantly !
    when (isJust emissionTid) $
        emissionHashes %= (fromJust emissionTid :)
    blocks %= (newBlock:)
    transactionMap %= (\map' -> foldl' (\m t -> MP.insert (hash t) t m)
        map' (hbTransactions newBlock))
    return updateIds

-- | Add pending addresses to addresses map (addr -> strategy), return
-- it as an argument
updateAddresses :: Update C.AddressToTxStrategyMap
updateAddresses = do
    oldKnown <- use addresses
    pending <- use pendingAddresses
    addresses %= flip MP.union pending
    pendingAddresses .= MP.empty
    return $ pending MP.\\ oldKnown

-- | Given a set of transactions, change utxo in a correspondent way
updateUtxo :: [Transaction] -> ExceptUpdate ()
updateUtxo newTxs = do
    let shouldBeAdded = concatMap computeOutputAddrids newTxs
        shouldBeDeleted = concatMap txInputs newTxs
    utxo %= MP.union (MP.fromList shouldBeAdded)
    forM_ shouldBeDeleted (\d -> utxo %= MP.delete d)

-- | Process a check over PeriodResult to filter them, includes checks
-- regarding pid and action logs check
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

-- | Perform coins allocation based on default allocation strategy
-- (hardcoded). Given the mintette's public keys it splits reward
-- among bank and mintettes.
allocateCoins
    :: NodeContext
    -> [PublicKey]
    -> [(MintetteId, PeriodResult)]
    -> PeriodId
    -> Transaction
allocateCoins nodeCtx mintetteKeys goodResults pId =
    Transaction
    { txInputs = [(emissionHash pId, 0, inputValue)]
    , txOutputs = (bankAddress, bankReward) : mintetteOutputs
    }
  where
    bankAddress = Address $ nodeCtx^.bankPublicKey
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

-- | Return all transactions that appear in periodResults collected
-- from mintettes.
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

-- | Given a list of mintettes with ids that changed it returns a map
-- from mintette id to utxo it should now adopt.
formPayload :: [a] -> [MintetteId] -> ExceptUpdate (MP.Map MintetteId C.Utxo)
formPayload mintettes' changedId = do
    curUtxo <- use utxo
    let payload = MP.foldlWithKey' gatherPayload MP.empty curUtxo
        gatherPayload :: MP.Map MintetteId C.Utxo
                      -> AddrId
                      -> Address
                      -> MP.Map MintetteId C.Utxo
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

-- | Process mintettes, kick out some, return ids that changed (and
-- need to update their utxo).
updateMintettes :: SecretKey
                -> [(MintetteId, PeriodResult)]
                -> Update [MintetteId]
updateMintettes sk goodMintettes =
    mintettesStorage %%= runState (MS.updateMintettes sk goodMintettes)
