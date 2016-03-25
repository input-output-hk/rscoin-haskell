{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
-- | This module describes user wallet state & actions

module Wallet
       ( UserAddress
       , WalletStorage
       , emptyWallet
       , getAllAddresses
       , getPublicAddresses
       , getLastBlockId
       , withBlockchainUpdate
       ) where

import           Control.Exception          (Exception)
import           Control.Lens               ((%=), (^.))
import qualified Control.Lens               as L
import           Control.Monad              (replicateM, unless)
import           Control.Monad.Catch        (MonadThrow, throwM)
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState)
import           Data.List                  (find)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T

import           Serokell.Util.Text         (format')

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
    { _userAddresses     :: [UserAddress]                 -- ^ Addresses that user owns
    , _inputAddressesTxs :: M.Map UserAddress Transaction -- ^ Transactions that user is aware
                                                          -- of and that reference user addresses
                                                          -- (as input or output)
    , _lastBlockId       :: Int                           -- ^ Last blochain height known
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

emptyWallet :: Int -> IO WalletStorage
emptyWallet walletsNumber = do
    wallets <- replicateM walletsNumber keyGen
    let _userAddresses = map (uncurry UserAddress) wallets
    let _inputAddressesTxs = M.empty
    _lastBlockId <- _MOCK_getBlockchainLength
    return WalletStorage {..}

-- | Get all available user addresses with private keys
getAllAddresses :: (MonadReader WalletStorage m) => m [UserAddress]
getAllAddresses = L.view userAddresses

-- | Get all available user addresses w/o private keys
getPublicAddresses :: (MonadReader WalletStorage m) => m [PublicKey]
getPublicAddresses = L.views userAddresses (map _publicAddress)

-- | Get last blockchain height saved in wallet state
getLastBlockId :: (MonadReader WalletStorage m) => m Int
getLastBlockId = L.view lastBlockId

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
        (\(mp :: M.Map UserAddress Transaction) ->
              foldr
                  (\(a,b) c ->
                        M.insert a b c)
                  mp
                  flattenedTxs)
