{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

-- | This module describes user wallet state & actions

module RSCoin.User.Wallet
       ( UserAddress
       , publicAddress
       , privateAddress
       , makeUserAddress
       , toAddress
       , validateUserAddress
       , TxHStatus (..)
       , TxHistoryRecord (..)
       , WalletStorageError (..)
       , WalletStorage
       , emptyWalletStorage
       , isInitialized
       , getAllAddresses
       , getPublicAddresses
       , addTemporaryTransaction
       , getTransactions
       , getLastBlockId
       , getTxsHistory
       , withBlockchainUpdate
       , addAddresses
       , initWallet
       ) where

import           Control.Applicative
import           Control.Exception          (Exception)
import           Control.Lens               ((%=), (.=), (<>=), (^.))
import qualified Control.Lens               as L
import           Control.Monad              (forM_, unless, when)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState)
import           Data.Function              (on)
import           Data.List                  (delete, find, groupBy, nub, sortOn,
                                             (\\))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, isJust)
import           Data.Monoid                ((<>))
import           Data.Ord                   (comparing)
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Text.Buildable        (Buildable (build))
import           Data.Text.Lazy.Builder     (fromString)

import           Serokell.Util.Text         (format', formatSingle', show')

import qualified RSCoin.Core                as C
import           RSCoin.Core.Crypto         (PublicKey, SecretKey)
import           RSCoin.Core.Primitives     (AddrId, Address (..),
                                             Transaction (..))
import           RSCoin.Core.Types          (PeriodId)

-- | User address as stored and seen by wallet owner.
data UserAddress = UserAddress
    { _privateAddress :: SecretKey -- ^ Secret key of the address
    , _publicAddress  :: PublicKey -- ^ Public key aka 'address' as
                                   -- visible the outside
    } deriving (Show,Eq,Ord)

$(L.makeLenses ''UserAddress)

instance Buildable UserAddress where
    build addr =
        mconcat
            [ "UserAddress with PK: "
            , build $ addr ^. publicAddress
            , "; SK: "
            , fromString $
              hideLast 10 (T.unpack $ show' $ addr ^. privateAddress)]
      where
        hideLast n str =
            if n > length str
                then str ++ "***"
                else take (length str - n) str ++ replicate n '*'

makeUserAddress :: SecretKey -> PublicKey -> UserAddress
makeUserAddress = UserAddress

toAddress :: UserAddress -> Address
toAddress userAddress = C.Address $ userAddress ^. publicAddress

validateUserAddress :: UserAddress -> Bool
validateUserAddress uaddr =
    C.checkKeyPair (uaddr ^. privateAddress, uaddr ^. publicAddress)

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
    { _userAddresses     :: [UserAddress]                -- ^ Addresses that user owns
    , _inputAddressesTxs :: M.Map UserAddress [TrAddrId] -- ^ Transactions that user is aware
                                                         -- of and that reference user
                                                         -- addresses, that were not spent
                                                         -- (utxo basically)
    , _periodDeleted     :: [(UserAddress, TrAddrId)]    -- ^ Support list for deleted events
    , _periodAdded       :: [(UserAddress, TrAddrId)]    -- ^ Support list for added events
    , _lastBlockId       :: Maybe PeriodId               -- ^ Last blockchain height known
                                                         -- to user (if known)

    , _historyTxs        :: S.Set TxHistoryRecord        -- ^ History of all transactions
                                                         -- being made (incomes + outcomes)
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
emptyWalletStorage = WalletStorage [] M.empty [] [] Nothing S.empty

-- Queries

type ExceptQuery a = forall m. (MonadReader WalletStorage m, MonadThrow m) => m a

checkInitR :: (MonadReader WalletStorage m, MonadThrow m) => m a -> m a
checkInitR action = do
    a <- L.views userAddresses (not . null)
    b <- L.views lastBlockId isJust
    if a && b
        then action
        else throwM NotInitialized

isInitialized :: ExceptQuery Bool
isInitialized = do
    a <- L.views userAddresses (not . null)
    b <- L.views lastBlockId isJust
    return $ a && b

-- | Get all available user addresses with private keys
getAllAddresses :: ExceptQuery [UserAddress]
getAllAddresses = checkInitR $ L.view userAddresses

-- | Get all available user addresses w/o private keys
getPublicAddresses :: ExceptQuery [PublicKey]
getPublicAddresses = checkInitR $ L.views userAddresses (map _publicAddress)

-- | Gets transaction that are somehow affect specified
-- address. Address should be owned by wallet in MonadReader.
getTransactions :: UserAddress -> ExceptQuery [Transaction]
getTransactions addr = checkInitR $ do
    addrOurs <- L.views userAddresses (elem addr)
    unless addrOurs $
        throwM $
        BadRequest $
        formatSingle' "Tried to getTransactions for addr we don't own: {}" addr
    txs <- fmap (nub . map fst) <$> L.views inputAddressesTxs (M.lookup addr)
    maybe
        (throwM $
         InternalError $
         formatSingle'
             "Transaction map in wallet doesn't contain {} as value (but should)."
             addr)
        return
        txs

-- | Get last blockchain height saved in wallet state
getLastBlockId :: ExceptQuery Int
getLastBlockId = checkInitR $ L.views lastBlockId fromJust

getTxsHistory :: ExceptQuery [TxHistoryRecord]
getTxsHistory = L.views historyTxs S.toList

-- Updates

type ExceptUpdate a = forall m. (MonadState WalletStorage m, MonadThrow m) => m a

checkInitS :: (MonadState WalletStorage m, MonadThrow m) => m a -> m a
checkInitS action = do
    a <- L.uses userAddresses (not . null)
    b <- L.uses lastBlockId isJust
    if a && b
        then action
        else throwM NotInitialized

restoreTransactions :: ExceptUpdate ()
restoreTransactions = do
    deleted <- L.use periodDeleted
    forM_ deleted (\(uaddr,p) ->
                        inputAddressesTxs %= M.insertWith (++) uaddr [p])
    periodDeleted .= []
    added <- L.use periodAdded
    forM_ added (\(uaddr,p) ->
                        inputAddressesTxs %= M.adjust (delete p) uaddr)
    periodAdded .= []

-- | Temporary adds transaction to wallet state. Should be used, when
-- transaction is commited by mintette and "thought" to drop into
-- blockchain eventually. PeriodId here stands for the "next" period
-- which we expect to see tx in.
addTemporaryTransaction :: PeriodId -> Transaction -> ExceptUpdate ()
addTemporaryTransaction periodId tx@Transaction{..} = do
    ownedAddressesRaw <- L.use userAddresses
    ownedAddresses <- L.uses userAddresses $ map toAddress
    ownedTransactions <- L.uses inputAddressesTxs M.assocs
    let getByAddress address =
            fromJust $
            find ((==) address . toAddress) ownedAddressesRaw
    forM_ ownedTransactions $ \(address,traddrids) ->
        forM_ traddrids $ \p@(_,addrid) ->
            forM_ txInputs $ \newaddrid ->
                when (addrid == newaddrid) $ do
                    inputAddressesTxs %= M.adjust (delete p) address
                    periodDeleted <>= [(address, p)]
    let outputAddr :: [(Address, [AddrId])]
        outputAddr = map (\a@((_,address):_) -> (address, map fst a)) $
                     filter (not . null) $
                     groupBy ((==) `on` snd) $
                     sortOn snd $
                     C.computeOutputAddrids tx
    forM_ outputAddr $ \(address,addrids) ->
        when (address `elem` ownedAddresses) $
            do let userAddr = getByAddress address
               forM_ addrids $ \addrid -> do
                   inputAddressesTxs %= M.insertWith (++) userAddr [(tx, addrid)]
                   periodAdded <>= [(userAddr, (tx,addrid))]
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
    addrs <- L.uses userAddresses $ map toAddress
    -- suppose nobody ever spends your output, i.e.
    -- outcome transactions are always sent by you
    let incomes = filter (\tx -> any (`elem` addrs) (map fst (C.txOutputs tx))) transactions
        mapped = map (\tx -> TxHistoryRecord tx newHeight TxHConfirmed) incomes
    historyTxs %= S.union (S.fromList mapped)
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

-- | Update state with bunch of transactions from new unobserved
-- blockchain blocks. Sum validation for transactions
-- is disabled, because HBlocks contain generative transactions for
-- fee allocation.
withBlockchainUpdate :: PeriodId -> [Transaction] -> ExceptUpdate ()
withBlockchainUpdate newHeight transactions =
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
       putHistoryRecords newHeight transactions

       ownedAddressesRaw <- L.use userAddresses
       ownedAddresses <- L.uses userAddresses (map (Address . _publicAddress))
       let addSomeTransactions tx@Transaction{..} = do
               -- first! add transactions that have output with address that we own
               let outputAddr :: [(Address, [AddrId])]
                   outputAddr = map (\a@((_,address):_) -> (address, map fst a)) $
                                    filter (not . null) $
                                    groupBy ((==) `on` snd) $
                                    sortOn snd $
                                    C.computeOutputAddrids tx
               forM_ outputAddr $ \(address,addrids) ->
                   when (address `elem` ownedAddresses) $
                       do let userAddr = getByAddress address
                          forM_ addrids $ \addrid' ->
                              inputAddressesTxs %= M.insertWith (++) userAddr [(tx, addrid')]
           getByAddress address =
               fromJust $
               find ((==) address . Address . _publicAddress) ownedAddressesRaw
           removeSomeTransactions Transaction{..} = do
               (savedTransactions :: [(UserAddress, (Transaction, AddrId))]) <-
                   concatMap
                       (\(addr,txs) ->
                             map (addr, ) txs) <$>
                   L.uses inputAddressesTxs M.assocs
               -- second! remove transactions that are used as input of this tx
               let toRemove :: [(UserAddress, (Transaction, AddrId))]
                   toRemove =
                       nub $
                       concatMap
                           (\addrid ->
                                 filter
                                     (\(_,(_,addrid')) ->
                                           addrid' == addrid)
                                     savedTransactions)
                           txInputs
               forM_ toRemove $
                   \(useraddr,pair') ->
                            inputAddressesTxs %= M.adjust (delete pair') useraddr
       forM_ transactions addSomeTransactions
       forM_ transactions removeSomeTransactions
       lastBlockId .= Just newHeight
  where
    reportBadRequest = throwM . BadRequest

-- | Puts given address and it's related transactions (that contain it
-- as output S_{out}) into wallet. Blockchain won't be queried.
addAddresses :: UserAddress -> [Transaction] -> ExceptUpdate ()
addAddresses userAddress txs = do
    unless (validateUserAddress userAddress) $
        throwM $
        BadRequest $
        formatSingle'
            ("Tried to add invalid address into storage {}. " <>
             "SecretKey doesn't match with PublicKey.")
            userAddress
    let mappedTxs :: [Transaction]
        mappedTxs =
            filter (null . C.getAddrIdByAddress (toAddress userAddress)) txs
    unless (null mappedTxs) $
        throwM $
        BadRequest $
        format'
            ("Error while adding address {} to storage: {} transactions dosn't " <>
             "contain it (address) as output. First bad transaction: {}")
            (userAddress, length mappedTxs, head mappedTxs)
    userAddresses <>= [userAddress]
    inputAddressesTxs <>=
        M.singleton
            userAddress
            (concatMap
                 (\t -> map (t, ) $
                        C.getAddrIdByAddress (toAddress userAddress) t)
                 txs)

-- | Initialize wallet with list of addresses to hold and
-- mode-specific parameter startHeight: if it's (Just i), then the
-- height is set to i, if Nothing, then to -1 (bank-mode).
initWallet :: [UserAddress] -> Maybe Int -> ExceptUpdate ()
initWallet addrs startHeight = do
    lastBlockId .= (startHeight <|> Just (-1))
    forM_ addrs $ flip addAddresses []
