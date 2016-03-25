{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
-- | This module describes user wallet state & actions

module Wallet
       ( UserAddress
       , WalletStorage
       , emptyWalletStorage
       , getAllAddresses
       , getPublicAddresses
       , getTransactions
       , getLastBlockId
       , withBlockchainUpdate
       , generateAddresses
       ) where

import           Control.Exception          (Exception, throw)
import           Control.Lens               ((%=), (<>=), (^.))
import qualified Control.Lens               as L
import           Control.Monad              (replicateM, unless)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState)
import           Data.List                  (find)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Data.Monoid                ((<>))
import qualified Data.Text                  as T
import           Data.Text.Buildable        (Buildable (build))

import           Serokell.Util.Text         (format', formatSingle')

import           RSCoin.Core.Crypto         (PublicKey (..), SecretKey (..),
                                             keyGen)
import           RSCoin.Core.Primitives     (Address (..), Transaction (..))

-- | User address as stored and seen by wallet owner.
data UserAddress = UserAddress
    { _privateAddress :: SecretKey -- ^ Secret key of the address
    , _publicAddress  :: PublicKey -- ^ Public key aka 'address' as
                                   -- visible the outside
    } deriving (Eq,Ord)

$(L.makeLenses ''UserAddress)

instance Buildable SecretKey where
    build = undefined

instance Buildable UserAddress where
    build addr =
        "UserAddress { pk: " <> build (addr ^. privateAddress) <> ", sk: " <>
        build (addr ^. publicAddress) <>
        " }"

instance Show UserAddress where
    show ud =
        "UserAddress with PK: " ++
        (show . getPublicKey $ ud ^. publicAddress) ++
        "; SK: " ++ hideLast 10 (show . getSecretKey $ ud ^. privateAddress)
      where
        hideLast n str =
            if n > length str
                then str ++ "***"
                else (take ((length str) - n) str) ++ (replicate n '*')


-- | Wallet, that holdls all information needed for the user to 'own'
-- bitcoins. Plus some meta-information that's crucially needed for
-- this implementation to work.
data WalletStorage = WalletStorage
    { _userAddresses     :: [UserAddress]                   -- ^ Addresses that user owns
    , _inputAddressesTxs :: M.Map UserAddress [Transaction] -- ^ Transactions that user is aware
                                                            -- of and that reference user
                                                            -- addresses
                                                            -- (as input or output)
    , _lastBlockId       :: Int                             -- ^ Last blochain height known
                                                            -- to user
    } deriving (Show)

$(L.makeLenses ''WalletStorage)

data WalletStorageError
    = BadRequest T.Text
    | InternalError T.Text
      deriving (Show)

instance Exception WalletStorageError

_MOCK_getBlockchainLength :: IO Int
_MOCK_getBlockchainLength = return 0

emptyWalletStorage :: Int -> IO WalletStorage
emptyWalletStorage addrNum
  | addrNum <= 0 =
      throw $
      BadRequest $
      formatSingle'
          "Attempt to create wallet with negative number of addresses: {}"
          addrNum
emptyWalletStorage addrNum = do
    _userAddresses <- map (uncurry UserAddress) <$> replicateM addrNum keyGen
    let _inputAddressesTxs = foldr (\addr -> M.insert addr []) M.empty _userAddresses
    _lastBlockId <- _MOCK_getBlockchainLength
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
    => Int -> [Transaction] -> m ()
withBlockchainUpdate newHeight transactions = do
    currentHeight <- L.use lastBlockId
    unless (currentHeight < newHeight) $
        throwM $
        BadRequest $
        format'
            "New blockchain height {} is less or equal to the old one: {}"
            (newHeight, currentHeight)
    knownAddresses <- L.use userAddresses
    knownPublicAddresses <- L.uses userAddresses (map _publicAddress)
    let hasFilter out = any (== out) knownPublicAddresses
        outputs = getAddress . fst
        hasSomePublicAddress Transaction{..} =
            any hasFilter $ map outputs txOutputs
    unless (all hasSomePublicAddress transactions) $
        throwM $
        BadRequest $
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

generateAddresses
    :: (MonadState WalletStorage m, MonadThrow m, MonadIO m)
    => Int -> m ()
generateAddresses addrNum = do
    unless (addrNum > 0) $
        throwM $
        BadRequest $
        formatSingle'
            "Failed on attempt to generate negative number of addresses: {}"
            addrNum
    newAddrs <-
        liftIO $ map (uncurry UserAddress) <$> replicateM addrNum keyGen
    userAddresses <>= newAddrs
