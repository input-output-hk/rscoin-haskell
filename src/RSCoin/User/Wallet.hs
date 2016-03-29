{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
       , WalletStorageError (..)
       , WalletStorage
       , emptyWalletStorage
       , getAllAddresses
       , getPublicAddresses
       , getTransactions
       , getLastBlockId
       , withBlockchainUpdate
       , addAddresses
       ) where

import           Control.Exception          (Exception, throw)
import           Control.Lens               ((%=), (<>=), (^.))
import qualified Control.Lens               as L
import           Control.Monad              (replicateM, unless)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState)
import           Data.List                  (find)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, maybeToList)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Data.Text.Buildable        (Buildable (build))
import           Data.Text.Lazy.Builder     (fromString)

import           Serokell.Util.Text         (format', formatSingle', show')

import qualified RSCoin.Core                as C
import           RSCoin.Core.Crypto         (PublicKey, SecretKey, keyGen)
import           RSCoin.Core.Primitives     (Address (..), Transaction (..))
import           RSCoin.Core.Types          (PeriodId)
import           RSCoin.User.Logic          (getBlockchainHeight)

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

-- | Wallet, that holdls all information needed for the user to 'own'
-- bitcoins. Plus some meta-information that's crucially needed for
-- this implementation to work.
data WalletStorage = WalletStorage
    { _userAddresses     :: [UserAddress]                   -- ^ Addresses that user owns
    , _inputAddressesTxs :: M.Map UserAddress [Transaction] -- ^ Transactions that user is aware
                                                            -- of and that reference user
                                                            -- addresses
                                                            -- (as input or output)
    , _lastBlockId       :: PeriodId                        -- ^ Last blochain height known
                                                            -- to user
    } deriving (Show)

$(L.makeLenses ''WalletStorage)

-- | The error describing possible wrong behaviour of wallet storage.
data WalletStorageError
    = BadRequest T.Text
    | InternalError T.Text
      deriving (Show)

instance Exception WalletStorageError

-- | Creates empty WalletStorage given the amount of addresses to
-- generate initially and optional address to include also (needed if
-- working in bank-mode).
emptyWalletStorage :: Int -> Maybe UserAddress -> IO WalletStorage
emptyWalletStorage addrNum _
  | addrNum <= 0 =
      throw $
      BadRequest $
      formatSingle'
          "Attempt to create wallet with negative number of addresses: {}"
          addrNum
emptyWalletStorage addrNum bankAddr = do
    _userAddresses <- (++ maybeToList bankAddr) .
                      map (uncurry UserAddress) <$>
                      replicateM addrNum keyGen
    let _inputAddressesTxs = foldr (\addr -> M.insert addr []) M.empty _userAddresses
    _lastBlockId <- C.unCps getBlockchainHeight
    return WalletStorage {..}


-- Queries

-- | Get all available user addresses with private keys
getAllAddresses :: (MonadReader WalletStorage m) => m [UserAddress]
getAllAddresses = L.view userAddresses

-- | Get all available user addresses w/o private keys
getPublicAddresses :: (MonadReader WalletStorage m) => m [PublicKey]
getPublicAddresses = L.views userAddresses (map _publicAddress)

-- | Gets transaction that are somehow affect specified
-- address. Address should be owned by wallet in MonadReader.
getTransactions :: (MonadReader WalletStorage m, MonadThrow m) => UserAddress -> m [Transaction]
getTransactions addr = do
    addrOurs <- L.views userAddresses (elem addr)
    unless addrOurs $
        throwM $
        BadRequest $
        formatSingle' "Tried to getTransactions for addr we don't own: {}" addr
    txs <- L.views inputAddressesTxs (M.lookup addr)
    maybe
        (throwM $
         InternalError $
         formatSingle'
             "Transaction map in wallet doesn't contain {} as value (but should)."
             addr)
        return
        txs

-- | Get last blockchain height saved in wallet state
getLastBlockId :: (MonadReader WalletStorage m) => m Int
getLastBlockId = L.view lastBlockId


-- Updates

-- | Update state with bunch of transactions from new unobserved
-- blockchain blocks. Each transaction should contain at least one of
-- users public addresses in outputs.
withBlockchainUpdate
    :: (MonadState WalletStorage m, MonadThrow m)
    => PeriodId -> [Transaction] -> m ()
withBlockchainUpdate newHeight transactions = do
    currentHeight <- L.use lastBlockId
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
    unless (all C.validateSum transactions) $
        reportBadRequest $
        formatSingle'
            "Tried to update with invalid transactions -- at least this one: {} " $
        head $ filter (not . C.validateSum) transactions
    knownAddresses <- L.use userAddresses
    knownPublicAddresses <- L.uses userAddresses (map _publicAddress)
    let hasFilter out = out `elem` knownPublicAddresses
        outputs = getAddress . fst
        hasSomePublicAddress Transaction{..} =
            any hasFilter $ map outputs txOutputs
    unless (all hasSomePublicAddress transactions) $
        reportBadRequest $
        T.pack $
        "Failed to verify that any transaction has at least one output " ++
        "address that we own. Transactions: " ++ show transactions
    let mappedTransactions :: [([UserAddress], Transaction)]
        mappedTransactions =
            map
                (\tr ->
                      ( map
                            (\out ->
                                  fromJust $
                                  find
                                      (\x ->
                                            x ^. publicAddress == out)
                                      knownAddresses) $
                        filter hasFilter (map outputs $ txOutputs tr)
                      , tr))
                transactions
        flattenedTxs :: [(UserAddress, Transaction)]
        flattenedTxs =
            concatMap
                (\(list,tx) ->
                      map (, tx) list)
                mappedTransactions
    inputAddressesTxs %=
        (\(mp :: M.Map UserAddress [Transaction]) ->
              foldr
                  (\(a,b) c ->
                        M.insertWith (++) a [b] c)
                  mp
                  flattenedTxs)
  where
    reportBadRequest = throwM . BadRequest

-- | Puts given address and it's related transactions (that contain it
-- as output S_{out}) into wallet. Blockchain won't be queried.
addAddresses
    :: (MonadState WalletStorage m, MonadThrow m)
    => UserAddress -> [Transaction] -> m ()
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
    inputAddressesTxs <>= M.singleton userAddress txs
