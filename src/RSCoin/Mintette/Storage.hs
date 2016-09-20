{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

-- | Storage for mintette's data.

module RSCoin.Mintette.Storage
       (
         -- * Type and constructor
         Storage
       , mkStorage

         -- * Lenses
       , addresses
       , actionLogs
       , curMintetteId
       , dpk
       , logHeads
       , logSize
       , lBlocks
       , mintettes
       , periodId
       , prevMintetteId
       , pset
       , txset
       , utxo
       , utxoAdded
       , utxoDeleted

         -- * Queries
       , Query
       , getCurMintetteId
       , getLastLBlocks
       , getLogs
       , getPeriodId
       , getPrevMintetteId
       , getUtxoPset

         -- * Updates
       , Update
       , UpdateInEnv
       , ExceptUpdate
       , ExceptUpdateInEnv
       , checkNotDoubleSpent
       , commitTx
       , finishPeriod
       , startPeriod
       , finishEpoch

       -- * Other helper methods
       , checkIsActive
       , checkTxSum
       , getLogHead
       , pushLogEntry
       , checkPeriodId
       , readerToState
       ) where

import           Control.Applicative   ((<|>))
import           Control.Lens          (at, makeLenses, use, uses, view, views, (%=),
                                        (+=), (.=), (<>=), (<~), (^.), _1)
import           Control.Monad         (unless, when)
import           Control.Monad.Catch   (MonadThrow (throwM))
import           Control.Monad.Extra   (unlessM, whenJust, whenM)
import           Control.Monad.Reader  (MonadReader, Reader, ReaderT, runReader)
import           Control.Monad.State   (MonadState, gets)
import qualified Data.HashMap.Strict   as HM
import           Data.List             (genericLength, genericTake)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust, fromMaybe, isJust, isNothing)
import           Data.SafeCopy         (base, deriveSafeCopy)
import qualified Data.Set              as S
import           Data.Tuple.Curry      (uncurryN)
import           Safe                  (atMay, headMay)

import           RSCoin.Core           (ActionLog, ActionLogHeads, AddressToTxStrategyMap,
                                        HBlock (..), HBlockHash, LBlock, MintetteId,
                                        Mintettes, PeriodId, Pset, TxStrategy (..), Utxo,
                                        computeOutputAddrids, derivePublicKey,
                                        hbTransactions, isOwner, isStrategyCompleted,
                                        mkCheckConfirmation, mkLBlock, owners, sign,
                                        verifyCheckConfirmation)
import qualified RSCoin.Core           as C

import           RSCoin.Mintette.Env   (RuntimeEnv, reActionLogsLimit, reSecretKey)
import           RSCoin.Mintette.Error (MintetteError (..))

data Storage = Storage
    { _utxo           :: !Utxo                    -- ^ Unspent transaction outputs
    , _utxoDeleted    :: !Utxo                    -- ^ Entries, deleted from utxo
    , _utxoAdded      :: !Utxo                    -- ^ Entries, added to utxo
    , _periodId       :: !PeriodId                -- ^ Id of ongoing period
    , _pset           :: !Pset                    -- ^ Set of checked transactions
    , _txset          :: !(S.Set C.Transaction)   -- ^ List of transaction sealing into ledger
    , _lBlocks        :: ![LBlock]                -- ^ Blocks are stored only for current period
    , _actionLogs     :: ![ActionLog]             -- ^ Logs are stored per period
    , _logSize        :: !Int                     -- ^ Total size of actionLogs
    , _mintettes      :: !Mintettes               -- ^ Mintettes for current period
    , _curMintetteId  :: !(Maybe MintetteId)      -- ^ Id for current period
    , _prevMintetteId :: !(Maybe MintetteId)      -- ^ Id for previous period
    , _dpk            :: !C.Dpk                   -- ^ DPK for current period
    , _logHeads       :: !ActionLogHeads          -- ^ All known heads of logs
    , _lastBankHash   :: !(Maybe HBlockHash)      -- ^ Hash of the last HBlock
    , _addresses      :: !AddressToTxStrategyMap  -- ^ Complete list of system's addresses
                                                  -- accompanied with their strategies.
                                                  -- Should be up-to-date with
                                                  -- Bank::Storage::_addresses (updates are
                                                  -- propagated via HBlocks)
    }

$(makeLenses ''Storage)
$(deriveSafeCopy 0 'base ''Storage)

-- | Make initial storage for the 0-th period.
mkStorage :: Storage
mkStorage =
    Storage
    { _utxo = mempty
    , _utxoDeleted = mempty
    , _utxoAdded = mempty
    , _periodId = 0
    , _pset = mempty
    , _txset = mempty
    , _lBlocks = mempty
    , _actionLogs = [mempty]
    , _logSize = 0
    , _mintettes = mempty
    , _curMintetteId = Nothing
    , _prevMintetteId = Nothing
    , _dpk = mempty
    , _logHeads = mempty
    , _lastBankHash = Nothing
    , _addresses = mempty
    }

type Query a = forall m . MonadReader Storage m => m a

getLogHead :: Query (Maybe (C.ActionLogEntry, C.ActionLogEntryHash))
getLogHead = views actionLogs f
  where
    f = foldr ((<|>) . headMay) Nothing

isActive :: Query Bool
isActive = views curMintetteId isJust

getLogs :: PeriodId -> Query (Maybe ActionLog)
getLogs pId = do
    curPeriodId <- getPeriodId
    views actionLogs (\b -> b `atMay` (curPeriodId - pId))

getPeriodId :: Query PeriodId
getPeriodId = view periodId

getUtxoPset :: Query (Utxo, Pset)
getUtxoPset = (,) <$> view utxo <*> view pset

getCurMintetteId :: Query (Maybe MintetteId)
getCurMintetteId = view curMintetteId

getPrevMintetteId :: Query (Maybe MintetteId)
getPrevMintetteId = view prevMintetteId

getLastLBlocks :: Query [C.LBlock]
getLastLBlocks = view lBlocks

type Update a = forall m. MonadState Storage m =>
                          m a
type UpdateInEnv a = forall m. MonadState Storage m =>
                               ReaderT RuntimeEnv m a
type ExceptUpdate a = forall m. (MonadThrow m, MonadState Storage m) =>
                                m a
type ExceptUpdateInEnv a = forall m. (MonadThrow m, MonadState Storage m) =>
                               ReaderT RuntimeEnv m a

-- TODO: maybe move somewhere
readerToState
    :: MonadState s m
    => Reader s a -> m a
readerToState = gets . runReader

-- | Validate structure of transaction, check input AddrId for
-- double spent and signature, update state if everything is valid.
-- MintetteError is thrown if something is invalid.
checkNotDoubleSpent :: C.Transaction
                    -> C.AddrId
                    -> [(C.Address, C.Signature C.Transaction)]
                    -> ExceptUpdateInEnv C.CheckConfirmation
checkNotDoubleSpent tx addrId sg = do
    checkIsActive
    checkTxSum tx
    unless (addrId `elem` C.txInputs tx) $
        throwM $ MEInconsistentRequest "AddrId is not part of inputs"
    inPset <- HM.lookup addrId <$> use pset
    maybe notInPsetCase inPsetCase inPset
  where
    inPsetCase storedTx
        | storedTx == tx = finishCheck
        | otherwise = throwM $ MENotUnspent addrId
    notInPsetCase = do
        addr <- HM.lookup addrId <$> use utxo
        maybe (throwM $ MENotUnspent addrId) checkSignaturesAndFinish addr
    checkSignaturesAndFinish addr = do
        strategy <- uses addresses $ fromMaybe DefaultStrategy . M.lookup addr
        unless (isStrategyCompleted strategy addr sg tx) $
            throwM MEInvalidSignature
        finishCheck
    finishCheck = do
        pushLogEntry $ C.QueryEntry tx
        utxoEntry <- uses utxo (HM.lookup addrId)
        utxo %= HM.delete addrId
        whenJust utxoEntry (\e -> utxoDeleted %= HM.insert addrId e)
        pset %= HM.insert addrId tx
        hsh <- snd . fromJust <$> readerToState getLogHead
        logSz <- use logSize
        sk <- view reSecretKey
        mkCheckConfirmation sk tx addrId (hsh, logSz - 1) <$> use periodId

-- | Check that transaction is valid and whether it falls within
-- mintette's remit.  If it's true, add transaction to storage and
-- return signed confirmation.
commitTx :: C.Transaction
         -> C.CheckConfirmations
         -> ExceptUpdateInEnv C.CommitAcknowledgment
commitTx tx@C.Transaction {..} bundle = do
    checkIsActive
    checkTxSum tx
    mts <- use mintettes
    mintetteId <- fromJust <$> use curMintetteId
    unless (C.isOwner mts (C.hash tx) mintetteId) $ throwM $
        MEInconsistentRequest "I'm not an owner!"
    curDpk <- use dpk
    isConfirmed <- and <$> mapM (checkInputConfirmed mts curDpk) txInputs
    res <- commitTxChecked isConfirmed tx bundle
    mapM_ (updateLogHeads curDpk) $ M.toList bundle
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
    verifyDpk curDpk ownerId C.CheckConfirmation {..} =
        maybe False (\(k, _) -> ccMintetteKey == k) $ curDpk `atMay` ownerId
    updateLogHeads curDpk ((mId, _), C.CheckConfirmation {..}) =
        maybe (return ()) (updateLogHeadsDo mId ccHead . fst) $ curDpk `atMay`
        mId
    updateLogHeadsDo mId lh pk = do
        myId <- fromJust <$> use curMintetteId
        unless (mId == myId) $ logHeads . at pk .= Just lh

commitTxChecked
    :: Bool
    -> C.Transaction
    -> C.CheckConfirmations
    -> ExceptUpdateInEnv C.CommitAcknowledgment
commitTxChecked False _ _ = throwM MENotConfirmed
commitTxChecked True tx bundle = do
    pushLogEntry $ C.CommitEntry tx bundle
    let toAddIntoUtxo = HM.fromList (computeOutputAddrids tx)
    utxo <>= toAddIntoUtxo
    utxoAdded <>= toAddIntoUtxo
    txset %= S.insert tx
    whenM ((C.maxLBlockSize <=) <$> uses txset S.size) finishEpoch
    sk <- view reSecretKey
    let pk = derivePublicKey sk
    hsh <- snd . fromJust <$> readerToState getLogHead
    logSz <- use logSize
    let logChainHead = (hsh, logSz)
    return $ C.CommitAcknowledgment pk (sign sk (tx, logChainHead)) logChainHead

-- | Finish ongoing period, returning its result.
-- Do nothing if period id is not an expected one.
finishPeriod :: Int -> Int -> C.PeriodId -> ExceptUpdateInEnv C.PeriodResult
finishPeriod blocksLimit logsLimit pId = do
    checkIsActive
    checkPeriodId pId
    finishEpoch
    prevMintetteId <~ use curMintetteId
    curMintetteId .= Nothing
    blocksRes <- use lBlocks
    actionLog <- uses actionLogs head
    return
        C.PeriodResult
        { prPeriodId = pId
        , prBlocks = take blocksLimit blocksRes
        , prBlocksNumber = genericLength blocksRes
        , prActionLog = take logsLimit actionLog
        , prActionLogSize = genericLength actionLog
        }

-- | Start new period. False return value indicates that mintette
-- didn't start the period because it received some other index, so
-- `setNewIndex` should be called and then `startPeriod` again.
startPeriod :: C.NewPeriodData -> ExceptUpdateInEnv ()
startPeriod C.NewPeriodData {..} = do
    lastPeriodId <- use periodId
    when (lastPeriodId >= npdPeriodId) $
        throwM $ MEPeriodMismatch (lastPeriodId + 1) npdPeriodId
    alreadyActive <- readerToState isActive
    when alreadyActive discardCurrentPeriod
    -- we don't check if new mid /= old one, because there is
    -- Nothing == Nothing situation at the very start
    prevMId <- use prevMintetteId
    when (isNothing npdNewIdPayload && isNothing prevMId) $
        throwM $
        MEInternal "Bank didn't send us utxo, but we're waiting for it."
    maybe
        (curMintetteId <~ use prevMintetteId)
        (uncurryN onMintetteIdChanged)
        npdNewIdPayload
    lBlocks .= mempty
    actionLogs %= (replicate (npdPeriodId - lastPeriodId) [] ++)
    actionLogsLimit <- view reActionLogsLimit
    actionLogs %= genericTake actionLogsLimit
    mintettes .= npdMintettes
    newMintetteId <- uses curMintetteId fromJust
    addresses <>= hbAddresses npdHBlock
    dpk .= npdDpk
    pset .= mempty
    periodId .= npdPeriodId
    unless (isJust npdNewIdPayload) $
        do let blockTransactions :: [C.Transaction]
               blockTransactions = hbTransactions npdHBlock
               blockOutputs :: C.Utxo
               blockOutputs =
                   HM.fromList $
                   concatMap computeOutputAddrids blockTransactions
               blockInputs :: S.Set C.AddrId
               blockInputs = S.fromList $ concatMap C.txInputs blockTransactions
           -- those are transactions that we approved, but didn't go
           -- into blockchain, so they should still be in utxo
           deletedNotInBlockchain :: C.Utxo <-
               uses
                   utxoDeleted
                   (HM.filterWithKey $ \k _ -> (k `S.notMember` blockInputs))
           -- those are transactions that were commited but for some
           -- reason bank rejected LBlock and they didn't go into
           -- blockchain
           addedNotInBlockchain :: C.Utxo <-
               uses
                   utxoAdded
                   (HM.filterWithKey $
                    \k _ -> (not $ k `HM.member` blockOutputs))
           deleted <- use utxoDeleted
           -- bank can (and in fact it does) insert in HBlock transactions
           -- that didn't pass through mintette (like fee allocation).
           let miscTransactions :: C.Utxo
               miscTransactions =
                   HM.filterWithKey
                       (\k _ ->
                             not (k `HM.member` deleted) &&
                             isOwner npdMintettes (k ^. _1) newMintetteId)
                       blockOutputs -- just for verbosity
           utxo <>= deletedNotInBlockchain
           utxo %= (`HM.difference` addedNotInBlockchain)
           utxo <>= miscTransactions
    utxoDeleted .= mempty
    utxoAdded .= mempty
    lastBankHash .= Just (C.hbHash npdHBlock)
  where
    onMintetteIdChanged :: MintetteId
                        -> Utxo
                        -> AddressToTxStrategyMap
                        -> Update ()
    onMintetteIdChanged newMId newUtxo newAddrs = do
        utxoDeleted .= mempty
        utxoAdded .= mempty
        utxo .= newUtxo
        addresses .= newAddrs
        curMintetteId .= Just newMId

finishEpoch :: UpdateInEnv ()
finishEpoch = finishEpochDo =<< uses txset S.toList

finishEpochDo :: [C.Transaction] -> UpdateInEnv ()
finishEpochDo [] = return ()
finishEpochDo txList = do
    heads <- use logHeads
    prevRecord <- fromJust <$> readerToState getLogHead
    prevHBlockHash <- fromJust <$> use lastBankHash
    sk <- view reSecretKey
    let lBlock = mkLBlock txList sk prevHBlockHash heads prevRecord
    lBlocks %= (lBlock :)
    pushLogEntry $ C.CloseEpochEntry heads
    txset .= mempty

pushLogEntry :: C.ActionLogEntry -> Update ()
pushLogEntry entry = do
    prev <- readerToState getLogHead
    topLog <- uses actionLogs head
    let newTopLog = C.actionLogNext prev entry : topLog
    actionLogs %= (\l -> newTopLog : tail l)
    logSize += 1

checkIsActive :: ExceptUpdate ()
checkIsActive = unlessM (readerToState isActive) $ throwM MEInactive

checkTxSum :: C.Transaction -> ExceptUpdate ()
checkTxSum tx = unless (C.isValidTx tx) $ throwM MEInvalidTxSums

checkPeriodId :: PeriodId -> ExceptUpdate ()
checkPeriodId received = do
    expected <- use periodId
    unless (expected == received) $ throwM $ MEPeriodMismatch expected received

discardCurrentPeriod :: Update ()
discardCurrentPeriod = do
    mintettes .= mempty
    prevMintetteId .= Nothing
    curMintetteId .= Nothing
    dpk .= mempty
