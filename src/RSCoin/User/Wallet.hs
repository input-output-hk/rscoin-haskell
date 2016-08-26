{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

-- | This module describes user wallet state & actions

module RSCoin.User.Wallet
       ( validateKeyPair
       , TxHStatus (..)
       , TxHistoryRecord (..)
       , WalletStorageError (..)
       , WalletStorage
       , emptyWalletStorage
       , isInitialized

         -- * Queries
       , getSecretKey
       , findUserAddress
       , getDependentAddresses
       , getUserAddresses
       , getOwnedAddresses
       , getOwnedDefaultAddresses
       , getOwnedAddrIds
       , getTransactions
       , getLastBlockId
       , getTxsHistory
       , getAddressStrategy
       , resolveAddressLocally
       , getAllocationStrategies
       , getIgnoredAllocationStrategies
       , getAllocationByIndex
       , getPendingTxs

         -- * Updates
       , handleToAdd
       , getToRemove
       , addTemporaryTransaction
       , withBlockchainUpdate
       , addAddress
       , deleteAddress
       , updateAllocationStrategies
       , blacklistAllocation
       , whitelistAllocation
       , updatePendingTxs
       , initWallet
       ) where

import           Control.Applicative
import           Control.Exception      (Exception)
import           Control.Lens           ((%=), (.=), (<>=))
import qualified Control.Lens           as L
import           Control.Monad          (forM_, unless, when)
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.Extra    (whenJust)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)
import           Control.Monad.State    (MonadState, get)
import           Data.Bifunctor         (first, second)
import           Data.Function          (on)
import           Data.List              (delete, find, groupBy, intersect, nub,
                                         sortOn, (\\))
import qualified Data.Map               as M
import           Data.Maybe             (catMaybes, fromJust, isJust, mapMaybe)
import           Data.Ord               (comparing)
import qualified Data.Set               as S
import qualified Data.Text              as T
import           Data.Tuple             (swap)
import           Formatting             (build, int, sformat, (%))

import           Serokell.Util.Text     (listBuilderJSON)

import qualified RSCoin.Core            as C
import           RSCoin.Core.Crypto     (PublicKey, SecretKey)
import           RSCoin.Core.Primitives (AddrId, Address (..), Transaction (..))
import           RSCoin.Core.Strategy   (AllocationInfo, MSAddress)
import           RSCoin.Core.Types      (PeriodId)


validateKeyPair :: Address -> SecretKey -> Bool
validateKeyPair addr sk = C.checkKeyPair (sk, getAddress addr)

-- | This datatype represents the status of transaction in
-- history. Rejected transactions are those who were "sent"
-- successfully but didn't get into blockchain.
data TxHStatus = TxHConfirmed | TxHUnconfirmed | TxHRejected
                 deriving (Show, Eq)

-- | Record of a certain transaction in history
data TxHistoryRecord = TxHistoryRecord
    { txhTransaction   :: Transaction
    , -- ^ We don't have transaction /time/ as date, so let's indicate
      -- it with height
      txhHeight        :: Int
    , txhStatus        :: TxHStatus
    , -- ^ This map should contain an entry for every tx input that we
      -- own
      txhAddrsInvolved :: S.Set Address
    } deriving (Show,Eq)

instance Ord TxHistoryRecord where
    compare = comparing $ \TxHistoryRecord{..} ->
                          (txhHeight, C.hash txhTransaction)

-- | Wallet, that holdls all information needed for the user to 'own'
-- bitcoins. Plus some meta-information that's crucially needed for
-- this implementation to work.
type TrAddrId = (Transaction, AddrId)
data WalletStorage = WalletStorage
    { -- | Address that user own. Owning is dictated by strategy. In
      -- default strategy you should have secret key. But in general
      -- it's not necessary.
      _ownedAddresses  :: M.Map Address (Maybe SecretKey)
      -- | Transactions that user is aware of and that reference user
      -- addresses, that were not spent (utxo basically)
    , _userTxAddrids   :: M.Map Address (S.Set TrAddrId)
      -- | Support list for deleted events
    , _periodDeleted   :: S.Set (Address, TrAddrId)
      -- | Support list for added events
    , _periodAdded     :: S.Set (Address, TrAddrId)
      -- | Last blockchain height known to user (if known)
    , _lastBlockId     :: Maybe PeriodId
      -- | History of all transactions being made (incomes + outcomes)
    , _historyTxs      :: S.Set TxHistoryRecord
      -- | Map from addresses to strategies that we own
    , _addrStrategies  :: M.Map Address C.TxStrategy
      -- | List of pairs (multisignatureAddress, strategy) with user as party.
    , _msAddrAllocs    :: M.Map MSAddress AllocationInfo
      -- | Allocations that are ignored from the main list
    , _msIgnoredAllocs :: M.Map MSAddress AllocationInfo
      -- | List of transactions that are pending on the notary side
    , _pendingTxs      :: S.Set Transaction
    } deriving (Show)

$(L.makeLenses ''WalletStorage)

-- | The error describing possible wrong behaviour of wallet storage.
data WalletStorageError
    = BadRequest T.Text
    | InternalError T.Text
    | NotInitialized
    deriving (Show)

instance Exception WalletStorageError

-- | Creates empty WalletStorage. Uninitialized.
emptyWalletStorage :: WalletStorage
emptyWalletStorage =
    WalletStorage
        M.empty
        M.empty
        S.empty
        S.empty
        Nothing
        S.empty
        M.empty
        M.empty
        M.empty
        S.empty

-- =======
-- Queries
-- =======

type ExceptQuery a = forall m. (MonadReader WalletStorage m, MonadThrow m) => m a


checkInitR :: (MonadReader WalletStorage m, MonadThrow m) => m a -> m a
checkInitR action = do
    a <- L.views ownedAddresses (not . M.null)
    b <- L.views lastBlockId isJust
    if a && b
        then action
        else throwM NotInitialized

isInitialized :: ExceptQuery Bool
isInitialized = do
    a <- L.views ownedAddresses (not . M.null)
    b <- L.views lastBlockId isJust
    return $ a && b

-- | Returns Nothing if address isn't in storage, Just Nothing if it's
-- added without the secret key and Just (Just sk) if sk is provided
getSecretKey :: Address -> ExceptQuery (Maybe (Maybe SecretKey))
getSecretKey addr = checkInitR $ L.views ownedAddresses (M.lookup addr)

-- | Searches UserAddress correspondent to Address. Returns (sk,pk)
-- keypair *associated* with this address, the one that proves the
-- ownership.  In case of DefaultStrategy it's just a keypair of
-- address and it's sk itself. In case of MOfNStrategy it's another
-- keypair of share you own (we assume there can be only one per
-- wallet).
findUserAddress :: Address -> ExceptQuery (Address, Maybe SecretKey)
findUserAddress addr = checkInitR $ do
    secretKey <- getSecretKey addr
    case secretKey of
        -- we don't own this address
        Nothing        -> return (addr, Nothing)
        -- we own secret key
        Just (Just sk) -> return (addr, Just sk)
        -- we don't own the secret key of this address
        Just Nothing   -> do
            strategy <- fromJust <$> L.views addrStrategies (M.lookup addr)
            case strategy of
                C.DefaultStrategy -> return (addr, Nothing)
                C.MOfNStrategy _ addrs -> do
                    -- genesisAddr should be used as a last argument but
                    -- we don't care about the order of addresses in the
                    -- result
                    defaultOwnerAddress <-
                        fromJust .
                        find (`elem` addrs) <$>
                        getOwnedDefaultAddresses addr
                    L.views ownedAddresses $
                        (defaultOwnerAddress,) .
                        fromJust . M.lookup defaultOwnerAddress

-- | For the given address return a list of addresses that depend on
-- it, e.g. mofn msig addresses that has input address as a
-- party. This function is used when deleting A -- then all depending
-- keys addresses should be deleted as well. The address itself is
-- excluded from the returned list.
getDependentAddresses :: Address -> ExceptQuery [Address]
getDependentAddresses addr = checkInitR $ do
    allAddrs <- L.views ownedAddresses M.keys
    mappedAddrs <- mapM (\a -> (a,) . fst <$> findUserAddress a) allAddrs
    return [a | (a,b) <- mappedAddrs, b == addr && a /= addr]

-- | Get all available user addresses that have private keys
getUserAddresses :: ExceptQuery [(Address,SecretKey)]
getUserAddresses =
    checkInitR $
    L.views ownedAddresses $
    map (second fromJust) . filter (isJust . snd) . M.assocs

-- | Puts bank's address on the first place if found
rescheduleBankFirst :: Address -> [Address] -> [Address]
rescheduleBankFirst genesisAddr addrs =
    case find (== genesisAddr) addrs of
        Nothing -> addrs
        Just _  -> genesisAddr : delete genesisAddr addrs

-- | Get all available user addresses
getOwnedAddresses :: Address -> ExceptQuery [Address]
getOwnedAddresses genesisAddr =
    checkInitR $ rescheduleBankFirst genesisAddr <$> L.views ownedAddresses M.keys

-- | Get all user addresses with DefaultStrategy
getOwnedDefaultAddresses :: Address -> ExceptQuery [Address]
getOwnedDefaultAddresses genesisAddr = checkInitR $ do
    addrs <- getOwnedAddresses genesisAddr
    strategies <- L.view addrStrategies
    return $
        filter (\addr -> M.lookup addr strategies == Just C.DefaultStrategy)
               addrs

-- | Gets user-related UTXO (addrids he owns)
getOwnedAddrIds :: Address -> Address -> ExceptQuery [AddrId]
getOwnedAddrIds genesisAddr addr = do
    addrOurs <- getOwnedAddresses genesisAddr
    unless (addr `elem` addrOurs) $
        throwM $
        BadRequest $
        sformat ("Tried to getTransactions for addr we don't own: " % build) addr
    addrids <- fmap (map snd . S.toList) <$> L.views userTxAddrids (M.lookup addr)
    maybe
        (throwM $
         InternalError $
         sformat
             ("TrAddrIds map (utxo) in wallet doesn't contain " % build %
              " as address (but should).")
             addr)
        return
        addrids

-- | Gets transaction that are somehow affect specified
-- address. Address should be owned by wallet in MonadReader.
getTransactions :: Address -> Address -> ExceptQuery [Transaction]
getTransactions genesisAddr addr = checkInitR $ do
    addrOurs <- getOwnedAddresses genesisAddr
    unless (addr `elem` addrOurs) $
        throwM $
        BadRequest $
        sformat ("Tried to getTransactions for addr we don't own: " % build) addr
    txs <- fmap (nub . map fst . S.toList) <$> L.views userTxAddrids (M.lookup addr)
    maybe
        (throwM $
         InternalError $
         sformat
             ("TrAddrIds map (utxo) in wallet doesn't contain " % build %
              " as address (but should).")
             addr)
         return
        txs

-- | Get last blockchain height saved in wallet state
getLastBlockId :: ExceptQuery Int
getLastBlockId = checkInitR $ L.views lastBlockId fromJust

getTxsHistory :: ExceptQuery [TxHistoryRecord]
getTxsHistory = L.views historyTxs S.toList

getAddressStrategy :: Address -> ExceptQuery (Maybe C.TxStrategy)
getAddressStrategy addr = checkInitR $ L.views addrStrategies $ M.lookup addr

-- | Returns an address we own that owns that addrid, if that address
-- is present and addrid isn't out of our current utxo.
resolveAddressLocally :: AddrId -> ExceptQuery (Maybe Address)
resolveAddressLocally addrid =
    checkInitR $
    L.views userTxAddrids $
    \addridMap ->
         find
             (\k -> case M.lookup k addridMap of
                        Nothing  -> False
                        Just set -> addrid `S.member` (S.map snd set)) $
         M.keys addridMap

-- | Get all 'M.Map' of MS addresses in which current user is party.
getAllocationStrategies :: ExceptQuery (M.Map MSAddress AllocationInfo)
getAllocationStrategies = checkInitR $ L.view msAddrAllocs

-- | Get all blacklisted allocation strategies
getIgnoredAllocationStrategies :: ExceptQuery (M.Map MSAddress AllocationInfo)
getIgnoredAllocationStrategies = checkInitR $ L.view msIgnoredAllocs

-- | Get element in allocation strategies Map by index. Index should be in [0..
getAllocationByIndex :: Int -> ExceptQuery (MSAddress, AllocationInfo)
getAllocationByIndex i = checkInitR $ do
    msAddrs <- L.view msAddrAllocs
    return $ M.elemAt i msAddrs

-- | Get pending transactions
getPendingTxs :: ExceptQuery [Transaction]
getPendingTxs = checkInitR $ L.views pendingTxs S.toList

-- ===============
-- Updates section
-- ===============

type ExceptUpdate a = forall m. (MonadState WalletStorage m, MonadThrow m) => m a

runReaderInside :: (MonadState r m) => ReaderT r m a -> m a
runReaderInside reader = runReaderT reader =<< get

checkInitS :: (MonadState WalletStorage m, MonadThrow m) => m a -> m a
checkInitS action = do
    a <- L.uses ownedAddresses (not . null)
    b <- L.uses lastBlockId isJust
    if a && b
        then action
        else throwM NotInitialized

-- | This function insert history record into the storage with the
-- following difference: if it's not added, it's just added; if there
-- is a same history record in the storage, differring only in the
-- `txhAddrsInvolved` argument, it's updated by merging fields of the
-- given history record and the one already in the storage.  That's
-- used to update txrecord if we import and address and it is related
-- to this transaction so should be included in this field.
populateHistoryRecords :: TxHistoryRecord -> ExceptUpdate ()
populateHistoryRecords hr = do
    almostSame <-
        L.uses historyTxs $
        find
            (\hr' ->
                  txhTransaction hr == txhTransaction hr' &&
                  txhHeight hr == txhHeight hr' &&
                  txhStatus hr == txhStatus hr') .
        S.toList
    case almostSame of
        Nothing -> historyTxs %= S.insert hr
        Just hr' -> do
            let newHistoryTx =
                    TxHistoryRecord
                    { txhTransaction = txhTransaction hr
                    , txhHeight = txhHeight hr
                    , txhStatus = txhStatus hr
                    , txhAddrsInvolved =
                          txhAddrsInvolved hr `S.union` txhAddrsInvolved hr'
                    }
            historyTxs %= S.delete hr'
            historyTxs %= S.insert newHistoryTx

-- | Cleanup periodDeleted and periodAdded to the state before new
-- period.
restoreTransactions :: ExceptUpdate ()
restoreTransactions = do
    deleted <- L.use periodDeleted
    forM_ deleted $ \(uaddr,p) ->
        userTxAddrids %= M.insertWith S.union uaddr (S.singleton p)
    periodDeleted .= S.empty
    added <- L.use periodAdded
    forM_ added $ \(uaddr,p) ->
        userTxAddrids %= M.adjust (S.delete p) uaddr
    periodAdded .= S.empty

-- | Temporary adds transaction to wallet state. Should be used, when
-- transaction is commited by mintette and "thought" to drop into
-- blockchain eventually. PeriodId here stands for the "next" period
-- which we expect to see tx in.
addTemporaryTransaction :: PeriodId -> Transaction -> ExceptUpdate ()
addTemporaryTransaction periodId tx@Transaction{..} = do
    ownedAddrs <- L.uses ownedAddresses M.keys
    ownedTransactions <- L.uses userTxAddrids M.assocs
    forM_ txInputs $ \newaddrid ->
        forM_ ownedTransactions $ \(address,traddrids) ->
            forM_ traddrids $ \p@(_,addrid) ->
                when (addrid == newaddrid) $ do
                    userTxAddrids %= M.adjust (S.delete p) address
                    periodDeleted %= S.insert (address, p)
    let outputAddr :: [(Address, [AddrId])]
        outputAddr = map (\a@((_,address):_) -> (address, map fst a)) $
                     filter (not . null) $
                     groupBy ((==) `on` snd) $
                     sortOn snd $
                     C.computeOutputAddrids tx
    forM_ outputAddr $ \(address,addrids) ->
        when (address `elem` ownedAddrs) $
           forM_ addrids $ \addrid -> do
               let p = (tx, addrid)
               userTxAddrids %= M.insertWith S.union address (S.singleton p)
               periodAdded %= S.insert (address, p)
    resolveSetIn <- S.fromList . catMaybes <$>
        mapM (\addrid -> runReaderInside $ resolveAddressLocally addrid)
        txInputs
    let outputsWeOwn = S.fromList $ intersect ownedAddrs $ map fst txOutputs
        txHistoryRecord = TxHistoryRecord tx periodId TxHUnconfirmed $
                           resolveSetIn `S.union` outputsWeOwn
    historyTxs %= S.insert txHistoryRecord


-- | Called from withBlockchainUpdate. Takes all transactions with
-- unconfirmed status, modifies them either to confirmed or rejected
-- depending on if they are in period transaction list. Also updates
-- their height to current blockchain height.
putHistoryRecords :: PeriodId -> [Transaction] -> ExceptUpdate ()
putHistoryRecords newHeight transactions = do
    unconfirmed <-
        S.toList <$>
        L.uses
            historyTxs
            (S.filter $ \TxHistoryRecord{..} -> txhStatus == TxHUnconfirmed)
    historyTxs %= (`S.difference` S.fromList unconfirmed)
    let inList =
            filter
                (\TxHistoryRecord{..} ->
                      txhTransaction `elem` transactions)
                unconfirmed
        nonInList = unconfirmed \\ inList
        confirmed = map setConfirmed inList
        rejected = map setRejected nonInList
    historyTxs %= S.union (S.fromList $ confirmed ++ rejected)
  where
    setConfirmed TxHistoryRecord{..} =
        TxHistoryRecord
        { txhHeight = newHeight
        , txhStatus = TxHConfirmed
        , ..
        }
    setRejected TxHistoryRecord{..} =
        TxHistoryRecord
        { txhHeight = newHeight
        , txhStatus = TxHRejected
        , ..
        }

-- | Given a set of owned addresses, transaction and handler performs
-- handler action over every (addr,addrid) that this transaction adds
-- to utxo
handleToAdd
    :: (Monad m)
    => [Address] -> Transaction -> (Address -> AddrId -> m ()) -> m ()
handleToAdd ownedAddrs tx@Transaction{..} handler = do
    -- first! add transactions that have output with address that we own
    let outputAddr :: [(Address, [AddrId])]
        outputAddr = map (\a@((_,address):_) -> (address, map fst a)) $
                         filter (not . null) $
                         groupBy ((==) `on` snd) $
                         sortOn snd $
                         C.computeOutputAddrids tx
    forM_ outputAddr $ \(address,addrids) ->
        when (address `elem` ownedAddrs) $
            forM_ addrids $ \addrid' -> handler address addrid'

-- | Given a current set of addresses and transactions it returns ones
-- that are spent in this state and can't be used anymore
getToRemove
    :: (Monad m)
    => M.Map Address (S.Set (Transaction, AddrId))
    -> Transaction
    -> m [(Address, (Transaction, AddrId))]
getToRemove currentTxs Transaction{..} = do
    let savedTransactions :: [(Address, (Transaction, AddrId))]
        savedTransactions =
            concatMap (\(addr,txs) -> map (addr, ) $ S.toList txs) $ M.assocs currentTxs
    -- second! remove transactions that are used as input of this tx
    let transformFoo addrid =
            filter (\(_,(_,addrid')) -> addrid' == addrid) savedTransactions
    return $ nub $ concatMap transformFoo txInputs

-- | Update state with bunch of transactions from new unobserved
-- blockchain blocks. Sum validation for transactions
-- is disabled, because HBlocks contain generative transactions for
-- fee allocation.
withBlockchainUpdate :: PeriodId -> C.HBlock -> ExceptUpdate ()
withBlockchainUpdate newHeight hblock@C.HBlock{..} =
    checkInitS $
    do currentHeight <- L.uses lastBlockId fromJust
       when (newHeight <= currentHeight) $
           reportBadRequest $
           sformat
               ("New blockchain height " % int %
                " is less or equal to the old one: " % int)
               newHeight currentHeight
       unless (currentHeight + 1 == newHeight) $
           reportBadRequest $
           sformat
               ("New blockchain height " % int %
                " should be exactly " % int % " + 1. " %
                "Only incremental updates are allowed.")
               newHeight currentHeight
       restoreTransactions
       withBlockchainUpdateInternal newHeight hblock
  where
    reportBadRequest = throwM . BadRequest

withBlockchainUpdateInternal :: PeriodId -> C.HBlock -> ExceptUpdate ()
withBlockchainUpdateInternal newHeight C.HBlock{..} = do
    ownedAddrs0 <- L.uses ownedAddresses M.keys
    -- Get strategies that we are related to
    let newStrategies = M.filterWithKey (ownStrategy ownedAddrs0) hbAddresses
    -- Add them to strategy list
    addrStrategies <>= newStrategies
    -- Also add addresses that we now control (e.g. multisig). There's
    -- a system predicate that only default strategy addresses control
    -- others. And no others can control anything except themselves.
    -- So adding this one time is enough.
    forM_ (M.keys newStrategies) $ \addr -> do
        ownedAddrs <- L.uses ownedAddresses M.keys
        unless (addr `elem` ownedAddrs) $ do
          ownedAddresses %= M.insert addr Nothing
          userTxAddrids %= M.insert addr S.empty

    ownedAddrs1 <- L.uses ownedAddresses M.keys
    forM_ ownedAddrs1 $
        \addr -> userTxAddrids %= M.insertWith S.union addr S.empty

    putHistoryRecords newHeight hbTransactions

    ownedAddrs <- L.uses ownedAddresses M.keys
    forM_ hbTransactions $ \tx -> do
         handleToAdd ownedAddrs tx $ \address addrid ->
             -- those are transactions that are expanding utxo
             userTxAddrids %= M.insertWith S.union address (S.singleton (tx, addrid))
         populateHistoryRecords $
             TxHistoryRecord tx newHeight TxHConfirmed $
             S.fromList $ intersect ownedAddrs $ map fst $ txOutputs tx
    forM_ hbTransactions $ \tx -> do
        currentTxs <- L.use userTxAddrids
        toRemove <- getToRemove currentTxs tx
        forM_ toRemove $
            \(useraddr,pair') ->
                 userTxAddrids %= M.adjust (S.delete pair') useraddr
        unless (null toRemove) $
            populateHistoryRecords $
                 TxHistoryRecord tx newHeight TxHConfirmed $
                 S.fromList $ map fst toRemove

    lastBlockId .= Just newHeight
  where
    ownStrategy _ _ C.DefaultStrategy = False
--        addr `elem` ownedAddrs -- We don't need to import those
    ownStrategy ownedAddrs _ (C.MOfNStrategy _ owners) =
        not $ null $ S.elems owners `intersect` ownedAddrs

-- | Puts given address (default strategy only!) and it's related
-- transactions (that contain it as output S_{out}) into wallet. The
-- hblocks list must be continuous list of blocks
-- [from..walletHeight], where from is not negative. Strategy assigned
-- is default.
addAddress :: C.Address
           -> Maybe SecretKey
           -> M.Map C.PeriodId C.HBlock
           -> ExceptUpdate ()
addAddress address skMaybe hblocks = do
    whenJust skMaybe $
        \sk ->
             unless (validateKeyPair address sk) $
             throwM $
             BadRequest $
             sformat
                 ("Tried to add invalid address into storage (" % build %
                  ". SecretKey doesn't match PublicKey.")
                 address
    let periods = M.keys hblocks
        minPeriod = minimum periods
        maxPeriod = maximum periods
        missing = [ s | s <- [minPeriod, maxPeriod] , s `notElem` periods ]
    unless (M.null hblocks) $ do
        unless (minPeriod >= 0) $
            throwM $
            BadRequest $
            sformat ("Minimum period is " % int % " and negative") minPeriod
        currentHeight <- L.uses lastBlockId fromJust
        unless (maxPeriod == currentHeight) $
            throwM $
            BadRequest $
            sformat
                ("Max period is " % int % " but must be the same as wallet height " %
                 int)
                maxPeriod
                currentHeight
        unless (null missing) $
            throwM $
            BadRequest $
            sformat
                ("These periods are missing (range must be continuous): " % build) $
            listBuilderJSON missing
    ownedAddresses %= M.insert address skMaybe
    addrStrategies %= M.insert address C.DefaultStrategy
    userTxAddrids %= M.insert address S.empty
    mapM_ (uncurry withBlockchainUpdateInternal) $ sortOn fst $ M.assocs hblocks

deleteAddress :: C.Address -> ExceptUpdate ()
deleteAddress address =
    checkInitS $
    do weOwnIt <- L.uses ownedAddresses $ (address `elem`) . M.keys
       unless weOwnIt $ throwM $ BadRequest $
           sformat
               ("Tried to delete address " % build % " which we don't own")
               address
       dependentAddresses <- runReaderInside $ getDependentAddresses address
       forM_ (address : dependentAddresses) wipeAddress
  where
    historyTxFilter addr TxHistoryRecord{..} =
        case S.delete addr txhAddrsInvolved of
            s | S.null s -> Nothing
            newSet ->
                Just TxHistoryRecord
                { txhAddrsInvolved = newSet
                , .. }
    wipeAddress addr = do
        ownedAddresses %= M.delete addr
        userTxAddrids %= M.delete addr
        periodDeleted %= S.filter ((== addr) . fst)
        periodAdded %= S.filter ((== addr) . fst)
        addrStrategies %= M.delete addr
        msAddrAllocs %= M.delete addr
        msIgnoredAllocs %= M.delete addr
        historyTxs %= S.fromList . mapMaybe (historyTxFilter addr) . S.toList

-- | Update '_msAddrsAllocations' by 'M.Map' from Notary.
updateAllocationStrategies :: M.Map MSAddress AllocationInfo -> ExceptUpdate ()
updateAllocationStrategies newMap = checkInitS $ do
    blacklisted <- L.use msIgnoredAllocs
    let common = M.intersection blacklisted newMap
        notCommon = M.difference newMap common
    msIgnoredAllocs .= common
    msAddrAllocs .= notCommon

-- | Puts an msaddr's allocation info into blacklist and ignores it forever
blacklistAllocation :: MSAddress -> ExceptUpdate ()
blacklistAllocation msaddr = checkInitS $ do
    maybeAllocinfo <- L.uses msAddrAllocs $ M.lookup msaddr
    maybe (throwM $ BadRequest $ sformat ("Can't blacklist msaddr " %
           build % "because it's not in storage.") msaddr)
          (\e -> do msIgnoredAllocs %= M.insert msaddr e
                    msAddrAllocs %= M.delete msaddr)
          maybeAllocinfo

-- | Updates pending txs set to the given one
updatePendingTxs :: S.Set Transaction -> ExceptUpdate ()
updatePendingTxs txs = checkInitS $ pendingTxs .= txs

-- | Whitelists address's allocation info back from the blacklist
whitelistAllocation :: MSAddress -> ExceptUpdate ()
whitelistAllocation msaddr = checkInitS $ do
    maybeAllocinfo <- L.uses msIgnoredAllocs $ M.lookup msaddr
    maybe (throwM $ BadRequest $ sformat ("Can't whitelist msaddr " %
           build % "because it's not in storage.") msaddr)
          (\e -> do msAddrAllocs %= M.insert msaddr e
                    msIgnoredAllocs %= M.delete msaddr)
          maybeAllocinfo

-- | Initialize wallet with list of addresses to hold and
-- mode-specific parameter startHeight: if it's (Just i), then the
-- height is set to i, if Nothing, then to -1 (bank-mode).
initWallet :: [(SecretKey,PublicKey)] -> Maybe Int -> ExceptUpdate ()
initWallet addrs startHeight = do
    let newHeight = startHeight <|> Just (-1)
    lastBlockId .= newHeight
    forM_ (map (first Address . swap) addrs) $ \(pk,sk) ->
        addAddress pk (Just sk) M.empty
