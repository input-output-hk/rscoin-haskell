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
       -- | Other helper methods
       , checkIsActive
       , checkTxSum
       , pset
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
       ) where

import           Control.Applicative        ((<|>))
import           Control.Lens               (Getter, at, makeLenses, to, use,
                                             uses, view, (%=), (+=), (.=),
                                             (<>=), (<~))
import           Control.Monad              (unless, when)
import           Control.Monad.Catch        (MonadThrow (throwM))
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState)
import           Data.Foldable              (forM_)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, isJust, isNothing)
import qualified Data.Set                   as S
import           Data.Tuple.Select          (sel1)
import           Safe                       (atMay, headMay)

import           RSCoin.Core                (ActionLog, ActionLogHeads, AddrId,
                                             Hash, LBlock, MintetteId,
                                             Mintettes, PeriodId, Pset,
                                             SecretKey, Signature,
                                             Transaction (..), Utxo,
                                             computeOutputAddrids,
                                             derivePublicKey, hbTransactions,
                                             isOwner, mkCheckConfirmation,
                                             mkLBlock, owners, sign,
                                             validateSignature,
                                             verifyCheckConfirmation)
import qualified RSCoin.Core                as C
import           RSCoin.Mintette.Error      (MintetteError (..))

data Storage = Storage
    { _utxo          :: Utxo               -- ^ Unspent transaction outputs
    , _utxoDeleted   :: Utxo               -- ^ Entries, deleted from utxo
    , _utxoAdded     :: Utxo               -- ^ Entries, added to utxo
    , _pset          :: Pset               -- ^ Set of checked transactions
    , _txset         :: S.Set Transaction  -- ^ List of transaction sealing into ledger
    , _lBlocks       :: [[LBlock]]         -- ^ Blocks are stored per period
    , _actionLogs    :: [ActionLog]        -- ^ Logs are stored per period
    , _logSize       :: Int                -- ^ Total size of actionLogs
    , _mintettes     :: Mintettes          -- ^ Mintettes for current period
    , _mintetteId    :: Maybe MintetteId   -- ^ Id for current period
    , _invMintetteId :: Maybe MintetteId   -- ^ Invariant for mintetteId
    , _dpk           :: C.Dpk              -- ^ DPK for current period
    , _logHeads      :: ActionLogHeads     -- ^ All known heads of logs
    , _lastBankHash  :: Maybe Hash         -- ^ Hash of the last HBlock
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
    }

type Query a = Getter Storage a

logHead :: Query (Maybe (C.ActionLogEntry, Hash))
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

-- Dumping Mintette state

getUtxoPset :: (MonadReader Storage m) => m (Utxo, Pset)
getUtxoPset = (,) <$> view utxo <*> view pset

previousMintetteId :: (MonadReader Storage m) => m (Maybe MintetteId)
previousMintetteId = view invMintetteId

type Update a = forall m . MonadState Storage m => m a
type ExceptUpdate a = forall m . (MonadThrow m, MonadState Storage m) => m a

-- | Validate structure of transaction, check input AddrId for
-- double spent and signature, update state if everything is valid.
-- Result is True iff everything is valid.
checkNotDoubleSpent :: SecretKey
                    -> Transaction
                    -> AddrId
                    -> Signature
                    -> ExceptUpdate C.CheckConfirmation
checkNotDoubleSpent sk tx addrId sg = do
    checkIsActive
    checkTxSum tx
    unless (addrId `elem` txInputs tx) $
        throwM $ MEInconsistentRequest "AddrId is not part of inputs"
    inPset <- M.lookup addrId <$> use pset
    maybe notInPsetCase inPsetCase inPset
  where
    inPsetCase storedTx
      | storedTx == tx = finishCheck
      | otherwise = throwM $ MENotUnspent addrId
    notInPsetCase = do
        addr <- M.lookup addrId <$> use utxo
        maybe (throwM $ MENotUnspent addrId) checkSignatureAndFinish addr
    checkSignatureAndFinish a
      | validateSignature sg a tx = finishCheck
      | otherwise = throwM MEInvalidSignature
    finishCheck = do
        pushLogEntry $ C.QueryEntry tx
        utxoEntry <- uses utxo (M.lookup addrId)
        utxo %= M.delete addrId
        when (isJust utxoEntry)
             (utxoDeleted %= M.insert addrId (fromJust utxoEntry))
        pset %= M.insert addrId tx
        hsh <- uses logHead (snd . fromJust)
        logSz <- use logSize
        return $ mkCheckConfirmation sk tx addrId (hsh, logSz - 1)

-- | Check that transaction is valid and whether it falls within
-- mintette's remit.  If it's true, add transaction to storage and
-- return signed confirmation.
commitTx :: SecretKey
         -> Transaction
         -> PeriodId
         -> C.CheckConfirmations
         -> ExceptUpdate C.CommitConfirmation
commitTx sk tx@Transaction{..} pId bundle = do
    checkIsActive
    checkPeriodId pId
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
        let addridOwners = owners mts (sel1 addrid)
            ownerConfirmed owner =
                maybe
                    False
                    (\proof ->
                          verifyCheckConfirmation proof tx addrid &&
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
    -> SecretKey
    -> Transaction
    -> C.CheckConfirmations
    -> ExceptUpdate C.CommitConfirmation
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
    return (pk, sign sk (tx, logChainHead), logChainHead)

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
    forM_ npdNewIdPayload (uncurry onMintetteIdChanged)
    invMId <- use invMintetteId
    when (isNothing npdNewIdPayload && isNothing invMId) $
        throwM $
        MEInternal "Bank didn't send us utxo, but we're waiting for it."
    lBlocks %= (replicate (npdPeriodId - lastPeriodId) [] ++)
    actionLogs %= (replicate (npdPeriodId - lastPeriodId) [] ++)
    mintettes .= npdMintettes
    mintetteId <~ use invMintetteId
    mId <- use invMintetteId
    dpk .= npdDpk
    pset .= M.empty
    unless (isJust npdNewIdPayload) $
        do let txOutsPaired =
                   concatMap computeOutputAddrids $ hbTransactions npdHBlock
           -- those are transactions that we approved, but that didn't
           -- go into blockchain, so they should still be in utxo
           deletedNotInBlockchain <-
               uses
                   utxoDeleted
                   (filter (not . (`elem` txOutsPaired)) . M.assocs)
           -- those are transactions that were commited but for some
           -- reason bank rejected LBlock and they didn't got into
           -- blockchain
           addedNotInBlockchain <-
               uses utxoAdded (filter (not . (`elem` txOutsPaired)) . M.assocs)
           -- bank can (and in fact it does) insert in HBlock transactions
           -- that didn't pass through mintette (like fee allocation).
           let miscTransactions =
                   filter
                       (\x ->
                             isOwner npdMintettes (sel1 $ fst x) (fromJust mId))
                       txOutsPaired -- just for verbosity
           utxo %= M.union (M.fromList deletedNotInBlockchain)
           utxo %= (`M.difference` M.fromList addedNotInBlockchain)
           utxo %= M.union (M.fromList miscTransactions)
    utxoDeleted .= M.empty
    utxoAdded .= M.empty
    lastBankHash .= Just (C.hbHash npdHBlock)

-- | Update mintette id, set new utxo. Utxo should be correct. No
-- correct checks are made here, so the server should make his best.
-- It also doesn't set the mintetteId field, only invMintetteId, so
-- startPeriod should be called after.
onMintetteIdChanged :: MintetteId -> Utxo -> Update ()
onMintetteIdChanged newMid newUtxo = do
    utxoDeleted .= M.empty
    utxoAdded .= M.empty
    utxo .= newUtxo
    invMintetteId .= Just newMid

-- | This function creates new LBlock with transactions from txset
-- and adds CloseEpochEntry to log.
-- It does nothing if txset is empty.
finishEpoch :: SecretKey -> ExceptUpdate ()
finishEpoch sk = do
    checkIsActive
    txList <- S.toList <$> use txset
    finishEpochList sk txList

finishEpochList :: SecretKey -> [Transaction] -> Update ()
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
    topLog <- head <$> use actionLogs
    let newTopLog = C.actionLogNext prev entry : topLog
    actionLogs %= (\l -> newTopLog : tail l)
    logSize += 1

checkIsActive :: ExceptUpdate ()
checkIsActive = do
    v <- use isActive
    unless v $ throwM MEInactive

checkTxSum :: Transaction -> ExceptUpdate ()
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
