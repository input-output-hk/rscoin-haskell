{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | This module wraps RSCoin.User.Wallet into ACID state.

module RSCoin.User.AcidState
       ( RSCoinUserState
       , openState
       , openMemState
       , closeState
       , initState
       , initStateBank

       -- * Queries
       , IsInitialized (..)
       , FindUserAddress (..)
       , GetUserAddresses (..)
       , GetOwnedAddresses (..)
       , GetOwnedDefaultAddresses (..)
       , GetOwnedAddrIds (..)
       , GetTransactions (..)
       , GetLastBlockId (..)
       , GetTxsHistory (..)
       , GetAddressStrategy (..)
       , ResolveAddressLocally (..)

       -- * Updates
       , WithBlockchainUpdate (..)
       , AddAddress (..)
       , AddTemporaryTransaction (..)
       , InitWallet (..)
       ) where

import           Control.Exception   (throw, throwIO)
import           Control.Monad       (replicateM, unless)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.Trans (MonadIO, liftIO)
import           Data.Acid           (makeAcidic)
import qualified Data.Acid           as A
import           Data.Acid.Memory    as AM
import           Data.SafeCopy       (base, deriveSafeCopy)

import qualified RSCoin.Core         as C
import           RSCoin.Core.Crypto  (keyGen)
import           RSCoin.Timed        (WorkMode)
import           RSCoin.User.Logic   (getBlockchainHeight)
import           RSCoin.User.Wallet  (TxHStatus, TxHistoryRecord, WalletStorage)
import qualified RSCoin.User.Wallet  as W

$(deriveSafeCopy 0 'base ''TxHStatus)
$(deriveSafeCopy 0 'base ''TxHistoryRecord)
$(deriveSafeCopy 0 'base ''WalletStorage)

type RSCoinUserState = A.AcidState WalletStorage

instance MonadThrow (A.Query WalletStorage) where
    throwM = throw

instance MonadThrow (A.Update WalletStorage) where
    throwM = throw

-- | Opens ACID state. If not there, it returns unitialized
-- unoperatable storage.
openState :: FilePath -> IO RSCoinUserState
openState path = do
    st <- A.openLocalStateFrom path W.emptyWalletStorage
    A.createCheckpoint st >> return st

openMemState :: IO RSCoinUserState
openMemState = AM.openMemoryState W.emptyWalletStorage

-- | Closes the ACID state.
closeState :: RSCoinUserState -> IO ()
closeState = A.closeAcidState

isInitialized :: A.Query WalletStorage Bool
findUserAddress :: C.Address -> A.Query WalletStorage (Maybe (C.Address, C.SecretKey))
getUserAddresses :: A.Query WalletStorage [(C.Address,C.SecretKey)]
getOwnedAddresses :: A.Query WalletStorage [C.Address]
getOwnedDefaultAddresses :: A.Query WalletStorage [C.Address]
getOwnedAddrIds :: C.Address -> A.Query WalletStorage [C.AddrId]
getTransactions :: C.Address -> A.Query WalletStorage [C.Transaction]
getLastBlockId :: A.Query WalletStorage Int
getTxsHistory :: A.Query WalletStorage [TxHistoryRecord]
getAddressStrategy :: C.Address -> A.Query WalletStorage (Maybe C.Strategy)
resolveAddressLocally :: C.AddrId -> A.Query WalletStorage (Maybe C.Address)

isInitialized = W.isInitialized
findUserAddress = W.findUserAddress
getUserAddresses = W.getUserAddresses
getOwnedAddresses = W.getOwnedAddresses
getOwnedDefaultAddresses = W.getOwnedDefaultAddresses
getOwnedAddrIds = W.getOwnedAddrIds
getTransactions = W.getTransactions
getLastBlockId = W.getLastBlockId
getTxsHistory = W.getTxsHistory
getAddressStrategy = W.getAddressStrategy
resolveAddressLocally = W.resolveAddressLocally

withBlockchainUpdate :: C.PeriodId -> C.HBlock -> A.Update WalletStorage ()
addTemporaryTransaction :: C.PeriodId -> C.Transaction -> A.Update WalletStorage ()
addAddress :: (C.Address,C.SecretKey) -> [C.Transaction] -> C.PeriodId ->A.Update WalletStorage ()
initWallet :: [(C.SecretKey,C.PublicKey)] -> Maybe Int -> A.Update WalletStorage ()

withBlockchainUpdate = W.withBlockchainUpdate
addTemporaryTransaction = W.addTemporaryTransaction
addAddress = W.addAddress
initWallet = W.initWallet

$(makeAcidic
      ''WalletStorage
      [ 'isInitialized
      , 'findUserAddress
      , 'getUserAddresses
      , 'getOwnedAddresses
      , 'getOwnedDefaultAddresses
      , 'getOwnedAddrIds
      , 'getTransactions
      , 'getLastBlockId
      , 'getTxsHistory
      , 'getAddressStrategy
      , 'resolveAddressLocally
      , 'withBlockchainUpdate
      , 'addTemporaryTransaction
      , 'addAddress
      , 'initWallet])

-- | This function generates 'n' new addresses ((pk,sk) pairs
-- essentially), and if the boolean flag 'is-bank-mode' is set, it
-- also loads secret bank key from ~/.rscoin/bankPrivateKey and adds
-- it to known addresses (public key is hardcoded in
-- RSCoin.Core.Constants).
initState :: WorkMode m => RSCoinUserState -> Int -> Maybe FilePath -> m ()
initState st n (Just skPath) = liftIO $ do
    sk <- C.readSecretKey skPath
    initStateBank st n sk
initState st n Nothing = do
    height <- pred <$> getBlockchainHeight
    liftIO $
        do addresses <- replicateM n keyGen
           A.update st $ InitWallet addresses (Just height)
           A.createCheckpoint st

-- | This function is is similar to the previous one, but is intended to
-- be used in tests/benchmark, where bank's secret key is embeded and
-- is not contained in file.
initStateBank :: MonadIO m => RSCoinUserState -> Int -> C.SecretKey -> m ()
initStateBank st n sk =
    liftIO $
    do let bankAddress = (sk, C.getAddress C.genesisAddress)
       unless (W.validateKeyPair C.genesisAddress sk) $
           throwIO $
           W.BadRequest "Imported bank's secret key doesn't belong to bank."
       addresses <- replicateM n keyGen
       A.update st $ InitWallet (bankAddress : addresses) Nothing
       A.createCheckpoint st
