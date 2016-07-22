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
       , findUserAddress
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
       , getAllocationByIndex

         -- * Updates
       , handleToAdd
       , getToRemove
       , addTemporaryTransaction
       , withBlockchainUpdate
       , addAddress
       , updateAllocationStrategies
       , initWallet
       ) where

import           Control.Applicative
import           Control.Exception          (Exception)
import           Control.Lens               ((%=), (.=), (<>=))
import qualified Control.Lens               as L
import           Control.Monad              (forM_, unless, when)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState)
import           Data.Bifunctor             (first, second)
import           Data.Function              (on)
import           Data.List                  (delete, find, groupBy, intersect,
                                             nub, sortOn, (\\))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, isJust)
import           Data.Monoid                ((<>))
import           Data.Ord                   (comparing)
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Tuple                 (swap)

import           Serokell.Util.Text         (format', formatSingle')

import qualified RSCoin.Core                as C
import           RSCoin.Core.Crypto         (PublicKey, SecretKey)
import           RSCoin.Core.Primitives     (AddrId, Address (..),
                                             Transaction (..))
import           RSCoin.Core.Strategy       (AllocationInfo, MSAddress)
import           RSCoin.Core.Types          (PeriodId)


validateKeyPair :: Address -> SecretKey -> Bool
validateKeyPair addr sk = C.checkKeyPair (sk, getAddress addr)

-- | This datatype represents the status of transaction in
-- history. Rejected transactions are those who were "sent"
-- successfully but didn't get into blockchain.
data TxHStatus = TxHConfirmed | TxHUnconfirmed | TxHRejected
                 deriving (Show, Eq)

-- | Record of a certain transaction in history
data TxHistoryRecord = TxHistoryRecord
    { txhTransaction :: Transaction
    , txhHeight      :: Int  -- we don't have transaction /time/ as date,
                             -- so let's indicate it with height
    , txhStatus      :: TxHStatus
    } deriving (Show, Eq)

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
      _ownedAddresses    :: M.Map Address (Maybe SecretKey)
      -- | Transactions that user is aware of and that reference user
      -- addresses, that were not spent (utxo basically)
    , _userTxAddrids     :: M.Map Address [TrAddrId]
      -- | Support list for deleted events
    , _periodDeleted     :: [(Address, TrAddrId)]
      -- | Support list for added events
    , _periodAdded       :: [(Address, TrAddrId)]
      -- | Last blockchain height known to user (if known)
    , _lastBlockId       :: Maybe PeriodId
      -- | History of all transactions being made (incomes + outcomes)
    , _historyTxs        :: S.Set TxHistoryRecord
      -- | Map from addresses to strategies that we own
    , _addrStrategies    :: M.Map Address C.TxStrategy
      -- | List of pairs (multisignatureAddress, strategy) with user as party.
    , _msAddrAllocations :: M.Map MSAddress AllocationInfo
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
emptyWalletStorage = WalletStorage M.empty M.empty [] [] Nothing S.empty M.empty M.empty

-- =======
-- Queries
-- =======

type ExceptQuery a = forall m. (MonadReader WalletStorage m, MonadThrow m) => m a


checkInitR :: (MonadReader WalletStorage m, MonadThrow m) => m a -> m a
checkInitR action = do
    a <- L.views ownedAddresses (not . null)
    b <- L.views lastBlockId isJust
    if a && b
        then action
        else throwM NotInitialized

isInitialized :: ExceptQuery Bool
isInitialized = do
    a <- L.views ownedAddresses (not . null)
    b <- L.views lastBlockId isJust
    return $ a && b

-- | Searches UserAddress correspondent to Address. Returns (sk,pk)
-- keypair *associated* with this address, the one that proves the
-- ownership.  In case of DefaultStrategy it's just a keypair of
-- address and it's sk itself. In case of MOfNStrategy it's another
-- keypair of share you own (we assume there can be only one per
-- wallet).
findUserAddress :: Address -> ExceptQuery (Maybe (Address, SecretKey))
findUserAddress addr = checkInitR $ do
    secretKey <- L.views ownedAddresses (M.lookup addr)
    case secretKey of
        -- we don't own this address
        Nothing        -> return Nothing
        -- we own secret key
        Just (Just sk) -> return $ Just (addr,sk)
        -- we don't own the secret key of this address
        Just Nothing   -> do
            strategy <- fromJust <$> L.views addrStrategies (M.lookup addr)
            case strategy of
                C.DefaultStrategy -> throwM $ InternalError $
                    "We have a notion of default strategy dut " <>
                    "I can't find a correspondent secret key."
                C.MOfNStrategy _ addrs -> do
                    defaultOwnerAddress <-
                        fromJust .
                        find (`elem` addrs) <$>
                        getOwnedDefaultAddresses
                    fmap (defaultOwnerAddress,) . fromJust <$>
                        L.views ownedAddresses (M.lookup defaultOwnerAddress)

-- | Get all available user addresses that have private keys
getUserAddresses :: ExceptQuery [(Address,SecretKey)]
getUserAddresses =
    checkInitR $
    L.views ownedAddresses $
    map (second fromJust) . filter (isJust . snd) . M.assocs

-- | Puts bank's address on the first place if found
rescheduleBankFirst :: [Address] -> [Address]
rescheduleBankFirst addrs =
    case find (== C.genesisAddress) addrs of
        Nothing -> addrs
        Just _ -> C.genesisAddress : delete C.genesisAddress addrs

-- | Get all available user addresses
getOwnedAddresses :: ExceptQuery [Address]
getOwnedAddresses = checkInitR $ rescheduleBankFirst <$> L.views ownedAddresses M.keys

-- | Get all user addresses with DefaultStrategy
getOwnedDefaultAddresses :: ExceptQuery [Address]
getOwnedDefaultAddresses = checkInitR $ do
    addrs <- getOwnedAddresses
    strategies <- L.view addrStrategies
    return $
        filter (\addr -> M.lookup addr strategies == Just C.DefaultStrategy)
               addrs

-- | Gets user-related UTXO (addrids he owns)
getOwnedAddrIds :: Address -> ExceptQuery [AddrId]
getOwnedAddrIds addr = do
    addrOurs <- getOwnedAddresses
    unless (addr `elem` addrOurs) $
        throwM $
        BadRequest $
        formatSingle' "Tried to getTransactions for addr we don't own: {}" addr
    addrids <- fmap (map snd) <$> L.views userTxAddrids (M.lookup addr)
    maybe
        (throwM $
         InternalError $
         formatSingle'
             "TrAddrIds map (utxo) in wallet doesn't contain {} as address (but should)."
             addr)
        return
        addrids

-- | Gets transaction that are somehow affect specified
-- address. Address should be owned by wallet in MonadReader.
getTransactions :: Address -> ExceptQuery [Transaction]
getTransactions addr = checkInitR $ do
    addrOurs <- getOwnedAddresses
    unless (addr `elem` addrOurs) $
        throwM $
        BadRequest $
        formatSingle' "Tried to getTransactions for addr we don't own: {}" addr
    txs <- fmap (nub . map fst) <$> L.views userTxAddrids (M.lookup addr)
    maybe
        (throwM $
         InternalError $
         formatSingle'
             "TrAddrIds map (utxo) in wallet doesn't contain {} as address (but should)."
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

resolveAddressLocally :: AddrId -> ExceptQuery (Maybe Address)
resolveAddressLocally addrid =
    checkInitR $
    L.views userTxAddrids $
    \addridMap ->
         find
             (\k -> case M.lookup k addridMap of
                        Nothing -> False
                        Just list -> addrid `elem` (map snd list)) $
         M.keys addridMap

-- | Get all 'M.Map' of MS addresses in which current user is party.
getAllocationStrategies :: ExceptQuery (M.Map MSAddress AllocationInfo)
getAllocationStrategies = checkInitR $ L.view msAddrAllocations

-- | Get element in allocation strategies Map by index.
getAllocationByIndex :: Int -> ExceptQuery (MSAddress, AllocationInfo)
getAllocationByIndex i = checkInitR $ do
    msAddrs <- L.view msAddrAllocations
    when (i >= M.size msAddrs) $
        throwM $ BadRequest "index i is greater than size of local ms list"
    return $ M.elemAt i msAddrs

-- ===============
-- Updates section
-- ===============

type ExceptUpdate a = forall m. (MonadState WalletStorage m, MonadThrow m) => m a

checkInitS :: (MonadState WalletStorage m, MonadThrow m) => m a -> m a
checkInitS action = do
    a <- L.uses ownedAddresses (not . null)
    b <- L.uses lastBlockId isJust
    if a && b
        then action
        else throwM NotInitialized

-- | Cleanup periodDeleted and periodAdded to the state before new
-- period.
restoreTransactions :: ExceptUpdate ()
restoreTransactions = do
    deleted <- L.use periodDeleted
    forM_ deleted (\(uaddr,p) ->
                        userTxAddrids %= M.insertWith (++) uaddr [p])
    periodDeleted .= []
    added <- L.use periodAdded
    forM_ added (\(uaddr,p) ->
                        userTxAddrids %= M.adjust (delete p) uaddr)
    periodAdded .= []

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
                    userTxAddrids %= M.adjust (delete p) address
                    periodDeleted <>= [(address, p)]
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
               userTxAddrids %= M.insertWith (++) address [p]
               periodAdded <>= [(address, p)]
    historyTxs %= S.insert (TxHistoryRecord tx periodId TxHUnconfirmed)

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
    => [(Address, [(Transaction, AddrId)])]
    -> Transaction
    -> m [(Address, (Transaction, AddrId))]
getToRemove currentTxs Transaction{..} = do
    let savedTransactions :: [(Address, (Transaction, AddrId))]
        savedTransactions =
            concatMap (\(addr,txs) -> map (addr, ) txs) currentTxs
    -- second! remove transactions that are used as input of this tx
    let transformFoo addrid =
            filter (\(_,(_,addrid')) -> addrid' == addrid) savedTransactions
    return $ nub $ concatMap transformFoo txInputs

-- | Update state with bunch of transactions from new unobserved
-- blockchain blocks. Sum validation for transactions
-- is disabled, because HBlocks contain generative transactions for
-- fee allocation.
withBlockchainUpdate :: PeriodId -> C.HBlock -> ExceptUpdate ()
withBlockchainUpdate newHeight C.HBlock{..} =
    checkInitS $
    do currentHeight <- L.uses lastBlockId fromJust
       unless (currentHeight < newHeight) $
           reportBadRequest $
           format'
               "New blockchain height {} is less or equal to the old one: {}"
               (newHeight, currentHeight)
       unless (currentHeight + 1 == newHeight) $
           reportBadRequest $
           format'
               ("New blockchain height {} should be exactly {} + 1. " <>
                "Only incremental updates are available.")
               (newHeight, currentHeight)

       restoreTransactions

       ownedAddrs0 <- L.uses ownedAddresses M.keys
       -- Get strategies that we are related to
       let newStrategies = M.filterWithKey (ownStrategy ownedAddrs0) hbAddresses
       -- Add them to strategy list
       addrStrategies <>= newStrategies
       -- Also add addresses that we now control (e.g. multisig)
       forM_ (M.keys newStrategies) $ \addr -> do
           ownedAddrs <- L.uses ownedAddresses M.keys
           unless (addr `elem` ownedAddrs) $
             ownedAddresses %= M.insert addr Nothing

       ownedAddrs1 <- L.uses ownedAddresses M.keys
       forM_ ownedAddrs1 $ \addr -> userTxAddrids %= M.insertWith (++) addr []

       putHistoryRecords newHeight hbTransactions

       ownedAddrs <- L.uses ownedAddresses M.keys
       forM_ hbTransactions $ \tx ->
            handleToAdd ownedAddrs tx $ \address addrid -> do
                userTxAddrids %= M.insertWith (++) address [(tx, addrid)]
                historyTxs %= S.insert (TxHistoryRecord tx newHeight TxHConfirmed)
       forM_ hbTransactions $ \tx -> do
           currentTxs <- L.uses userTxAddrids M.assocs
           toRemove <- getToRemove currentTxs tx
           forM_ toRemove $
               \(useraddr,pair') ->
                    userTxAddrids %= M.adjust (delete pair') useraddr
           unless (null toRemove) $
               historyTxs %= S.insert (TxHistoryRecord tx newHeight TxHConfirmed)


       lastBlockId .= Just newHeight
  where
    reportBadRequest = throwM . BadRequest
    ownStrategy ownedAddrs addr C.DefaultStrategy =
        addr `elem` ownedAddrs
    ownStrategy ownedAddrs _ (C.MOfNStrategy _ owners) =
        not $ null $ S.elems owners `intersect` ownedAddrs

-- | Puts given address and it's related transactions (that contain it
-- as output S_{out}) into wallet. Blockchain won't be queried.
-- Strategy assigned is default.
addAddress :: (C.Address,SecretKey) -> [Transaction] -> PeriodId -> ExceptUpdate ()
addAddress addressPair@(address,sk) txs periodId = do
    unless (uncurry validateKeyPair addressPair) $
        throwM $
        BadRequest $
        format'
            ("Tried to add invalid address into storage ({},{}). " <>
             "SecretKey doesn't match with PublicKey.")
            addressPair
    let mappedTxs :: [Transaction]
        mappedTxs = filter (null . C.getAddrIdByAddress address) txs
    unless (null mappedTxs) $
        throwM $
        BadRequest $
        format'
            ("Error while adding address ({},{}) to storage: {} transactions dosn't " <>
             "contain it (address) as output. First bad transaction: {}")
            (address, sk, length mappedTxs, head mappedTxs)
    historyTxs <>=
        S.fromList (map (\tx -> TxHistoryRecord tx periodId TxHConfirmed) txs)
    ownedAddresses %= M.insert address (Just sk)
    userTxAddrids <>=
        M.singleton
            address
            (concatMap
                 (\t -> map (t, ) $
                        C.getAddrIdByAddress address t)
                 txs)
    addrStrategies %= M.insert address C.DefaultStrategy

-- | Update '_msAddrsAllocations' by 'M.Map' from Notary.
updateAllocationStrategies :: M.Map MSAddress AllocationInfo -> ExceptUpdate ()
updateAllocationStrategies newMap = checkInitS $ msAddrAllocations .= newMap

-- | Initialize wallet with list of addresses to hold and
-- mode-specific parameter startHeight: if it's (Just i), then the
-- height is set to i, if Nothing, then to -1 (bank-mode).
initWallet :: [(SecretKey,PublicKey)] -> Maybe Int -> ExceptUpdate ()
initWallet addrs startHeight = do
    let newHeight = startHeight <|> Just (-1)
    lastBlockId .= newHeight
    forM_ (map (first Address . swap) addrs) $ \p ->
        addAddress p [] $ fromJust newHeight
