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

       -- * Queries
       , GetAllAddresses (..)
       , GetPublicAddresses (..)
       , GetTransactions (..)
       , GetLastBlockId (..)
       , DumpAllTransactions (..)

       -- * Updates
       , WithBlockchainUpdate (..)
       , AddAddresses (..)
       , AddTemporaryTransaction (..)
       , InitWallet (..)
       ) where

import qualified RSCoin.Core         as C
import           RSCoin.Core.Crypto  (keyGen)
import           RSCoin.Test         (WorkMode)
import           RSCoin.User.Logic   (getBlockchainHeight)
import           RSCoin.User.Wallet  (UserAddress, WalletStorage)
import qualified RSCoin.User.Wallet  as W

import           Control.Exception   (throw, throwIO)
import           Control.Monad       (replicateM, unless)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Control.Monad.Trans (liftIO)
import           Data.Acid           (makeAcidic)
import qualified Data.Acid           as A
import           Data.Acid.Memory    as AM
import           Data.SafeCopy       (base, deriveSafeCopy)

$(deriveSafeCopy 0 'base ''UserAddress)
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

getAllAddresses :: A.Query WalletStorage [UserAddress]
getPublicAddresses :: A.Query WalletStorage [C.PublicKey]
getTransactions :: UserAddress -> A.Query WalletStorage [C.Transaction]
getLastBlockId :: A.Query WalletStorage Int
dumpAllTransactions :: A.Query WalletStorage [(UserAddress, (C.Transaction, C.AddrId))]

getAllAddresses = W.getAllAddresses
getPublicAddresses = W.getPublicAddresses
getTransactions = W.getTransactions
getLastBlockId = W.getLastBlockId
dumpAllTransactions = W.dumpAllTransactions

withBlockchainUpdate :: Int -> [C.Transaction] -> A.Update WalletStorage ()
addTemporaryTransaction :: C.Transaction -> A.Update WalletStorage ()
addAddresses :: UserAddress -> [C.Transaction] -> A.Update WalletStorage ()
initWallet :: [UserAddress] -> Maybe Int -> A.Update WalletStorage ()

withBlockchainUpdate = W.withBlockchainUpdate
addTemporaryTransaction = W.addTemporaryTransaction
addAddresses = W.addAddresses
initWallet = W.initWallet

$(makeAcidic
      ''WalletStorage
      [ 'getAllAddresses
      , 'getPublicAddresses
      , 'getTransactions
      , 'getLastBlockId
      , 'dumpAllTransactions
      , 'withBlockchainUpdate
      , 'addTemporaryTransaction
      , 'addAddresses
      , 'initWallet])

-- | This function generates 'n' new addresses ((pk,sk) pairs
-- essentially), and if the boolean flag 'is-bank-mode' is set, it
-- also loads secret bank key from ~/.rscoin/bankPrivateKey and adds
-- it to known addresses (public key is hardcoded in
-- RSCoin.Core.Constants).
initState :: WorkMode m => RSCoinUserState -> Int -> Maybe FilePath -> m ()
initState st n (Just skPath) = liftIO $ do
    sk <- C.readSecretKey skPath
    let bankAddress = W.makeUserAddress sk $ C.getAddress C.genesisAddress
    unless (W.validateUserAddress bankAddress) $
        throwIO $ W.BadRequest "Imported bank's secret key doesn't belong to bank."
    addresses <- map (uncurry W.makeUserAddress) <$> replicateM n keyGen
    A.update st $ InitWallet (bankAddress : addresses) Nothing
    A.createCheckpoint st
initState st n Nothing = do
    height <- pred <$> getBlockchainHeight
    liftIO $ do
        addresses <- map (uncurry W.makeUserAddress) <$> replicateM n keyGen
        A.update st $ InitWallet addresses (Just height)
        A.createCheckpoint st
