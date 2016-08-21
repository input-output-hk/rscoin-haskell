{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

-- | Storage for mintette's data.

module RSCoin.Mintette.Storage
       ( Storage
       , mkStorage
       , checkNotDoubleSpent
       , commitTx
       , finishPeriod
       , startPeriod
       , finishEpoch
       , previousMintetteId
       , getUtxoPset
       , getBlocks
       , getLogs
       , getPeriodId
       -- | Other helper methods
       , checkIsActive
       , checkTxSum
       , pset
       , addresses
       , logHead
       , logSize
       , utxo
       , pushLogEntry
       , utxoDeleted
       , checkPeriodId
       , mintettes
       , mintetteId
       , dpk
       , logHeads
       , utxoAdded
       , txset
       , periodId
       ) where

import           Control.Applicative        ((<|>))
import           Control.Lens               (Getter, at, makeLenses, to, use,
                                             uses, view, views, (%=), (+=),
                                             (.=), (<>=), (<~), (^.), _1)
import           Control.Monad              (unless, when)
import           Control.Monad.Catch        (MonadThrow (throwM))
import           Control.Monad.Extra        (whenJust)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, fromMaybe, isJust,
                                             isNothing)
import qualified Data.Set                   as S
import           Data.Tuple.Curry           (uncurryN)
import           Safe                       (atMay, headMay)

import           RSCoin.Core                (ActionLog, ActionLogHeads,
                                             AddressToTxStrategyMap,
                                             HBlock (..), HBlockHash, LBlock,
                                             MintetteId, Mintettes, PeriodId,
                                             Pset, SecretKey, TxStrategy (..),
                                             Utxo, computeOutputAddrids,
                                             derivePublicKey, hbTransactions,
                                             isOwner, isStrategyCompleted,
                                             mkCheckConfirmation, mkLBlock,
                                             owners, sign,
                                             verifyCheckConfirmation)
import qualified RSCoin.Core                as C
import           RSCoin.Mintette.Error      (MintetteError (..))

data Storage = Storage
    { _utxo          :: !Utxo                    -- ^ Unspent transaction outputs
    , _utxoDeleted   :: !Utxo                    -- ^ Entries, deleted from utxo
    , _utxoAdded     :: !Utxo                    -- ^ Entries, added to utxo
    , _pset          :: !Pset                    -- ^ Set of checked transactions
    , _txset         :: !(S.Set C.Transaction)   -- ^ List of transaction sealing into ledger
    , _lBlocks       :: ![[LBlock]]              -- ^ Blocks are stored per period
    , _actionLogs    :: ![ActionLog]             -- ^ Logs are stored per period
    , _logSize       :: !Int                     -- ^ Total size of actionLogs
    , _mintettes     :: !Mintettes               -- ^ Mintettes for current period
    , _mintetteId    :: !(Maybe MintetteId)      -- ^ Id for current period
    , _invMintetteId :: !(Maybe MintetteId)      -- ^ Invariant for mintetteId
    , _dpk           :: !C.Dpk                   -- ^ DPK for current period
    , _logHeads      :: !ActionLogHeads          -- ^ All known heads of logs
    , _lastBankHash  :: !(Maybe HBlockHash)      -- ^ Hash of the last HBlock
    , _addresses     :: !AddressToTxStrategyMap  -- ^ Complete list of system's addresses
                                                 -- accompanied with their strategies.
                                                 -- Should be up-to-date with
                                                 -- Bank::Storage::_addresses (updates are
                                                 -- propagated via HBlocks)
    }

$(makeLenses ''Storage)

-- | Make storage for the 0-th period.
mkStorage :: Storage
mkStorage =
    Storage
    { _utxo = M.empty
    , _utxoDeleted = M.empty
    , _utxoAdded = M.empty
    , _pset = M.empty
    , _txset = S.empty
    , _lBlocks = [[]]
    , _actionLogs = [[]]
    , _logSize = 0
    , _mintettes = []
    , _mintetteId = Nothing
    , _invMintetteId = Nothing
    , _dpk = []
    , _logHeads = M.empty
    , _lastBankHash = Nothing
    , _addresses = M.empty
    }

type Query a = Getter Storage a

logHead :: Query (Maybe (C.ActionLogEntry, C.ActionLogEntryHash))
logHead = actionLogs . to f
  where
    f = foldr ((<|>) . headMay) Nothing

periodId :: Query PeriodId
periodId = lBlocks . to (pred . length)

isActive :: Query Bool
isActive = mintetteId . to isJust

-- Dumping Mintette state

getBlocks :: (MonadReader Storage m) => PeriodId -> m (Maybe [LBlock])
getBlocks pId = view $ lBlocks . to (\b -> b `atMay` (length b - pId - 1))

getLogs :: (MonadReader Storage m) => PeriodId -> m (Maybe ActionLog)
getLogs pId =
    view $ actionLogs . to (\b -> b `atMay` (length b - pId - 1))

getPeriodId :: (MonadReader Storage m) => m PeriodId
getPeriodId = views lBlocks $ pred . length

getUtxoPset :: (MonadReader Storage m) => m (Utxo, Pset)
getUtxoPset = (,) <$> view utxo <*> view pset

previousMintetteId :: (MonadReader Storage m) => m (Maybe MintetteId)
previousMintetteId = view invMintetteId

type Update a = forall m . MonadState Storage m => m a
type ExceptUpdate a = forall m . (MonadThrow m, MonadState Storage m) => m a

-- | Validate structure of transaction, check input AddrId for
-- double spent and signature, update state if everything is valid.
-- Result is True iff everything is valid.
checkNotDoubleSpent :: C.SecretKey
                    -> C.Transaction
                    -> C.AddrId
                    -> [(C.Address, C.Signature C.Transaction)]
                    -> ExceptUpdate C.CheckConfirmation
checkNotDoubleSpent sk tx addrId sg = do
    checkIsActive
    checkTxSum tx
    unless (addrId `elem` C.txInputs tx) $
        throwM $ MEInconsistentRequest "AddrId is not part of inputs"
    inPset <- M.lookup addrId <$> use pset
    maybe notInPsetCase inPsetCase inPset
  where
    inPsetCase storedTx
      | storedTx == tx = finishCheck
      | otherwise = throwM $ MENotUnspent addrId
    notInPsetCase = do
          addr <- M.lookup addrId <$> use utxo
          maybe (throwM $ MENotUnspent addrId) checkSignaturesAndFinish addr
    checkSignaturesAndFinish addr = do
          strategy <- uses addresses $ fromMaybe DefaultStrategy . M.lookup addr
          if isStrategyCompleted strategy addr sg tx
             then finishCheck
             else throwM MEInvalidSignature
    finishCheck = do
        pushLogEntry $ C.QueryEntry tx
        utxoEntry <- uses utxo (M.lookup addrId)
        utxo %= M.delete addrId
        whenJust
            utxoEntry
            (\e ->
                  utxoDeleted %= M.insert addrId e)
        pset %= M.insert addrId tx
        hsh <- uses logHead (snd . fromJust)
        logSz <- use logSize
        pId <- use periodId
        return $ mkCheckConfirmation sk tx addrId (hsh, logSz - 1) pId

-- | Check that transaction is valid and whether it falls within
-- mintette's remit.  If it's true, add transaction to storage and
-- return signed confirmation.
commitTx :: C.SecretKey
         -> C.Transaction
         -> C.CheckConfirmations
         -> ExceptUpdate C.CommitAcknowledgment
commitTx sk tx@C.Transaction{..} bundle = do
    checkIsActive
    checkTxSum tx
    mts <- use mintettes
    mId <- fromJust <$> use mintetteId
    unless (C.isOwner mts (C.hash tx) mId) $ throwM $
        MEInconsistentRequest "I'm not an owner!"
    curDpk <- use dpk
    isConfirmed <- and <$> mapM (checkInputConfirmed mts curDpk) txInputs
    res <- commitTxChecked isConfirmed sk tx bundle
    mapM_ (updateLogHeads curDpk) $ M.assocs bundle
    return res
  where
    checkInputConfirmed mts curDpk addrid = do
        pId <- use periodId
        let addridOwners = owners mts (addrid ^. _1)
            ownerConfirmed owner =
                maybe
                    False
                    (\proof ->
                          verifyCheckConfirmation proof tx addrid pId &&
                          verifyDpk curDpk owner proof) $
                M.lookup (owner, addrid) bundle
            filtered = filter ownerConfirmed addridOwners
        return (length filtered > length addridOwners `div` 2)
    verifyDpk curDpk ownerId C.CheckConfirmation{..} =
        maybe
            False
            (\(k,_) ->
                  ccMintetteKey == k) $
        curDpk `atMay`
        ownerId
    updateLogHeads curDpk ((mId,_),C.CheckConfirmation{..}) =
        maybe (return ()) (updateLogHeadsDo mId ccHead . fst) $ curDpk `atMay`
        mId
    updateLogHeadsDo mId lh pk = do
        myId <- fromJust <$> use mintetteId
        unless (mId == myId) $ logHeads . at pk .= Just lh

commitTxChecked
    :: Bool
    -> C.SecretKey
    -> C.Transaction
    -> C.CheckConfirmations
    -> ExceptUpdate C.CommitAcknowledgment
commitTxChecked False _ _ _ = throwM MENotConfirmed
commitTxChecked True sk tx bundle = do
    pushLogEntry $ C.CommitEntry tx bundle
    utxo <>= M.fromList (computeOutputAddrids tx)
    utxoAdded <>= M.fromList (computeOutputAddrids tx)
    txset %= S.insert tx
    let pk = derivePublicKey sk
    hsh <- uses logHead (snd . fromJust)
    logSz <- use logSize
    let logChainHead = (hsh, logSz)
    return $ C.CommitAcknowledgment pk (sign sk (tx, logChainHead)) logChainHead

-- | Finish ongoing period, returning its result.
-- Do nothing if period id is not an expected one.
finishPeriod :: C.SecretKey -> C.PeriodId -> ExceptUpdate C.PeriodResult
finishPeriod sk pId = do
    checkIsActive
    checkPeriodId pId
    finishEpoch sk
    mintetteId .= Nothing
    (pId, , ) <$> use (lBlocks . to head) <*> use (actionLogs . to head)

-- | Start new period. False return value indicates that mintette
-- didn't start the period because it received some other index, so
-- `setNewIndex` should be called and then `startPeriod` again.
startPeriod :: C.NewPeriodData -> ExceptUpdate ()
startPeriod C.NewPeriodData{..} = do
    lastPeriodId <- use periodId
    when (lastPeriodId >= npdPeriodId) $
        throwM $ MEPeriodMismatch (lastPeriodId + 1) npdPeriodId
    alreadyActive <- use isActive
    when alreadyActive discardCurrentPeriod
    -- we don't check if new mid /= old one, because there is
    -- Nothing == Nothing situation at the very start
    whenJust
        npdNewIdPayload
        (uncurryN onMintetteIdChanged)
    invMId <- use invMintetteId
    when (isNothing npdNewIdPayload && isNothing invMId) $
        throwM $
        MEInternal "Bank didn't send us utxo, but we're waiting for it."
    lBlocks %= (replicate (npdPeriodId - lastPeriodId) [] ++)
    actionLogs %= (replicate (npdPeriodId - lastPeriodId) [] ++)
    mintettes .= npdMintettes
    mintetteId <~ use invMintetteId
    mId <- use invMintetteId
    addresses %= M.union (hbAddresses npdHBlock)
    dpk .= npdDpk
    pset .= M.empty
    unless (isJust npdNewIdPayload) $
        do let blockTransactions :: [C.Transaction]
               blockTransactions = hbTransactions npdHBlock
               blockOutputs :: C.Utxo
               blockOutputs =
                   M.fromList $
                   concatMap computeOutputAddrids blockTransactions
               blockInputs :: S.Set C.AddrId
               blockInputs =
                   S.fromList $ concatMap C.txInputs blockTransactions
           -- those are transactions that we approved, but didn't go
           -- into blockchain, so they should still be in utxo
           deletedNotInBlockchain :: C.Utxo <-
               uses
                   utxoDeleted
                   (M.filterWithKey $
                    \k _ ->
                         (k `S.notMember` blockInputs))
           -- those are transactions that were commited but for some
           -- reason bank rejected LBlock and they didn't go into
           -- blockchain
           addedNotInBlockchain :: C.Utxo <-
               uses
                   utxoAdded
                   (M.filterWithKey $
                    \k _ ->
                         (k `M.notMember` blockOutputs))
           deleted <- use utxoDeleted
           -- bank can (and in fact it does) insert in HBlock transactions
           -- that didn't pass through mintette (like fee allocation).
           let miscTransactions :: C.Utxo
               miscTransactions =
                   M.filterWithKey
                       (\k _ ->
                             k `M.notMember` deleted &&
                             isOwner npdMintettes (k ^. _1) (fromJust mId))
                       blockOutputs -- just for verbosity
           utxo <>= deletedNotInBlockchain
           utxo %= (`M.difference` addedNotInBlockchain)
           utxo <>= miscTransactions
    utxoDeleted .= M.empty
    utxoAdded .= M.empty
    lastBankHash .= Just (C.hbHash npdHBlock)

-- | Update mintette id, set new utxo. Utxo should be correct. No
-- correct checks are made here, so the server should make his best.
-- It also doesn't set the mintetteId field, only invMintetteId, so
-- startPeriod should be called after.
onMintetteIdChanged :: MintetteId -> Utxo -> AddressToTxStrategyMap -> Update ()
onMintetteIdChanged newMid newUtxo newAddrs  = do
    utxoDeleted .= M.empty
    utxoAdded .= M.empty
    utxo .= newUtxo
    addresses .= newAddrs
    invMintetteId .= Just newMid

-- | This function creates new LBlock with transactions from txset
-- and adds CloseEpochEntry to log.
-- It does nothing if txset is empty.
finishEpoch :: SecretKey -> ExceptUpdate ()
finishEpoch sk = do
    checkIsActive
    txList <- S.toList <$> use txset
    finishEpochList sk txList

finishEpochList :: C.SecretKey -> [C.Transaction] -> Update ()
finishEpochList _ [] = return ()
finishEpochList sk txList = do
    heads <- use logHeads
    prevRecord <- fromJust <$> use logHead
    prevHBlockHash <- fromJust <$> use lastBankHash
    let lBlock = mkLBlock txList sk prevHBlockHash heads prevRecord
    topBlocks <- head <$> use lBlocks
    let newTopBlocks = lBlock : topBlocks
    lBlocks %= (\l -> newTopBlocks : tail l)
    pushLogEntry $ C.CloseEpochEntry heads
    txset .= S.empty

pushLogEntry :: C.ActionLogEntry -> Update ()
pushLogEntry entry = do
    prev <- use logHead
    topLog <- uses actionLogs head
    let newTopLog = C.actionLogNext prev entry : topLog
    actionLogs %= (\l -> newTopLog : tail l)
    logSize += 1

checkIsActive :: ExceptUpdate ()
checkIsActive = do
    v <- use isActive
    unless v $ throwM MEInactive

checkTxSum :: C.Transaction -> ExceptUpdate ()
checkTxSum tx = unless (C.validateSum tx) $ throwM MEInvalidTxSums

checkPeriodId :: PeriodId -> ExceptUpdate ()
checkPeriodId received = do
    expected <- use periodId
    unless (expected == received) $ throwM $ MEPeriodMismatch expected received

discardCurrentPeriod :: Update ()
discardCurrentPeriod = do
    mintettes .= []
    invMintetteId .= Nothing
    mintetteId .= Nothing
    dpk .= []
