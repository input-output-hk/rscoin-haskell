{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | This module wraps RSCoin.User.Wallet into ACID state.

module RSCoin.User.AcidState
       ( RSCoinUserState
       , openState
       , closeState

       -- * Queries
       , GetAllAddresses (..)
       , GetPublicAddresses (..)
       , GetTransactions (..)
       , GetLastBlockId (..)

       -- * Updates
       , WithBlockchainUpdate (..)
       , AddAddresses (..)
       ) where

import qualified RSCoin.Core         as C
import           RSCoin.User.Wallet  (UserAddress, WalletStorage)
import qualified RSCoin.User.Wallet  as W

import           Control.Exception   (throw, throwIO)
import           Control.Monad       (unless)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Data.Acid           (makeAcidic)
import qualified Data.Acid           as A
import           Data.SafeCopy       (base, deriveSafeCopy)

$(deriveSafeCopy 0 'base ''UserAddress)
$(deriveSafeCopy 0 'base ''WalletStorage)

type RSCoinUserState = A.AcidState WalletStorage

instance MonadThrow (A.Query WalletStorage) where
    throwM = throw

instance MonadThrow (A.Update WalletStorage) where
    throwM = throw

-- | this function opens the ACID state wallet from the given path, or
-- creates it if it's not there. When creating, it generates 'n' new
-- addresses ((pk,sk) pairs essentially), and if the boolean flag
-- 'is-bank-mode' is set, it also loads secret bank key from
-- ~/.rscoin/bankPrivateKey and adds it to known addresses (public key
-- is hardcoded in RSCoin.Core.Constants).
openState :: FilePath -> Int -> Maybe FilePath -> IO RSCoinUserState
openState path n (Just skPath) = do
    sk <- C.readSecretKey skPath
    let bankAddress = W.makeUserAddress sk $ C.getAddress C.genesisAddress
    unless (W.validateUserAddress bankAddress) $
        throwIO $ W.BadRequest "Imported bank's secret key doesn't belong to bank."
    st <- A.openLocalStateFrom path =<< W.emptyWalletStorage n (Just bankAddress)
    A.createCheckpoint st >> return st
openState path n Nothing = do
    st <- A.openLocalStateFrom path =<< W.emptyWalletStorage n Nothing
    A.createCheckpoint st >> return st

-- | Closes the ACID state.
closeState :: RSCoinUserState -> IO ()
closeState = A.closeAcidState

getAllAddresses :: A.Query WalletStorage [UserAddress]
getPublicAddresses :: A.Query WalletStorage [C.PublicKey]
getTransactions :: UserAddress -> A.Query WalletStorage [C.Transaction]
getLastBlockId :: A.Query WalletStorage Int

getAllAddresses = W.getAllAddresses
getPublicAddresses = W.getPublicAddresses
getTransactions = W.getTransactions
getLastBlockId = W.getLastBlockId

withBlockchainUpdate :: Int -> [C.Transaction] -> A.Update WalletStorage ()
addAddresses :: UserAddress -> [C.Transaction] -> A.Update WalletStorage ()

withBlockchainUpdate = W.withBlockchainUpdate
addAddresses = W.addAddresses

$(makeAcidic
      ''WalletStorage
      [ 'getAllAddresses
      , 'getPublicAddresses
      , 'getTransactions
      , 'getLastBlockId
      , 'withBlockchainUpdate
      , 'addAddresses])
