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
       , GetAllocationStrategies (..)
       , GetAllocationByIndex (..)

       -- * Updates
       , WithBlockchainUpdate (..)
       , AddAddress (..)
       , AddTemporaryTransaction (..)
       , UpdateAllocationStrategies (..)
       , InitWallet (..)
       ) where

import           Control.Exception    (throw, throwIO)
import           Control.Lens         ((^.))
import           Control.Monad        (replicateM, unless)
import           Control.Monad.Catch  (MonadThrow, throwM)
import           Control.Monad.Trans  (liftIO)
import           Data.Acid            (makeAcidic)
import qualified Data.Acid            as A
import           Data.Acid.Memory     as AM
import           Data.Map             (Map)
import           Data.SafeCopy        (base, deriveSafeCopy)

import qualified RSCoin.Core          as C
import           RSCoin.Core.Crypto   (keyGen)
import           RSCoin.Core.Strategy (AllocationInfo, MSAddress)
import           RSCoin.Timed         (MonadRpc (getNodeContext), WorkMode)
import           RSCoin.User.Logic    (getBlockchainHeight)
import           RSCoin.User.Wallet   (TxHStatus, TxHistoryRecord,
                                       WalletStorage)
import qualified RSCoin.User.Wallet   as W

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
    st <$ A.createCheckpoint st

openMemState :: IO RSCoinUserState
openMemState = AM.openMemoryState W.emptyWalletStorage

-- | Closes the ACID state.
closeState :: RSCoinUserState -> IO ()
closeState = A.closeAcidState

isInitialized :: A.Query WalletStorage Bool
findUserAddress :: C.NodeContext -> C.Address -> A.Query WalletStorage (Maybe (C.Address, C.SecretKey))
getUserAddresses :: A.Query WalletStorage [(C.Address,C.SecretKey)]
getOwnedAddresses :: C.NodeContext -> A.Query WalletStorage [C.Address]
getOwnedDefaultAddresses :: C.NodeContext -> A.Query WalletStorage [C.Address]
getOwnedAddrIds :: C.NodeContext -> C.Address -> A.Query WalletStorage [C.AddrId]
getTransactions :: C.NodeContext -> C.Address -> A.Query WalletStorage [C.Transaction]
getLastBlockId :: A.Query WalletStorage Int
getTxsHistory :: A.Query WalletStorage [TxHistoryRecord]
getAddressStrategy :: C.Address -> A.Query WalletStorage (Maybe C.TxStrategy)
resolveAddressLocally :: C.AddrId -> A.Query WalletStorage (Maybe C.Address)
getAllocationStrategies :: A.Query WalletStorage (Map MSAddress AllocationInfo)
getAllocationByIndex :: Int -> A.Query WalletStorage (MSAddress, AllocationInfo)

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
getAllocationStrategies = W.getAllocationStrategies
getAllocationByIndex = W.getAllocationByIndex

withBlockchainUpdate :: C.PeriodId -> C.HBlock -> A.Update WalletStorage ()
addTemporaryTransaction :: C.PeriodId -> C.Transaction -> A.Update WalletStorage ()
addAddress :: (C.Address,Maybe C.SecretKey) -> [C.Transaction] -> C.PeriodId -> A.Update WalletStorage ()
updateAllocationStrategies :: Map MSAddress AllocationInfo -> A.Update WalletStorage ()
initWallet :: [(C.SecretKey,C.PublicKey)] -> Maybe Int -> A.Update WalletStorage ()

withBlockchainUpdate = W.withBlockchainUpdate
addTemporaryTransaction = W.addTemporaryTransaction
addAddress = W.addAddress
updateAllocationStrategies = W.updateAllocationStrategies
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
      , 'getAllocationStrategies
      , 'getAllocationByIndex
      , 'resolveAddressLocally
      , 'withBlockchainUpdate
      , 'addTemporaryTransaction
      , 'addAddress
      , 'updateAllocationStrategies
      , 'initWallet])

-- | This function generates 'n' new addresses ((pk,sk) pairs
-- essentially), and if the boolean flag 'is-bank-mode' is set, it
-- also loads secret bank key from ~/.rscoin/bankPrivateKey and adds
-- it to known addresses (public key is hardcoded in
-- RSCoin.Core.Constants).
initState :: WorkMode m => RSCoinUserState -> Int -> Maybe FilePath -> m ()
initState st n (Just skPath) = do
    sk <- liftIO $ C.readSecretKey skPath
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
initStateBank :: WorkMode m => RSCoinUserState -> Int -> C.SecretKey -> m ()
initStateBank st n sk = do
    nodeCtx <- getNodeContext
    let ctxGenAddress = nodeCtx ^. C.genesisAddress
    liftIO $ do
       let bankAddress = (sk, C.getAddress ctxGenAddress)
       unless (W.validateKeyPair ctxGenAddress sk) $
           throwIO $
           W.BadRequest "Imported bank's secret key doesn't belong to bank."
       addresses <- replicateM n keyGen
       A.update st $ InitWallet (bankAddress : addresses) Nothing
       A.createCheckpoint st
