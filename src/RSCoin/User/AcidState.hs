{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | This module wraps RSCoin.User.Wallet into ACID state.

module RSCoin.User.AcidState
       ( UserState
       , openState
       , openMemState
       , closeState
       , initState
       , initStateBank
       , query
       , tidyState
       , update

       -- * Queries
       , IsInitialized (..)
       , GetSecretKey (..)
       , FindUserAddress (..)
       , GetUserAddresses (..)
       , GetDependentAddresses (..)
       , GetOwnedAddresses (..)
       , GetOwnedDefaultAddresses (..)
       , GetOwnedAddrIds (..)
       , GetTransactions (..)
       , GetLastBlockId (..)
       , GetTxsHistory (..)
       , GetAddressStrategy (..)
       , ResolveAddressLocally (..)
       , GetAllocationStrategies (..)
       , GetIgnoredAllocationStrategies (..)
       , GetAllocationByIndex (..)
       , GetPendingTxs (..)

       -- * Updates
       , WithBlockchainUpdate (..)
       , AddAddress (..)
       , DeleteAddress (..)
       , AddTemporaryTransaction (..)
       , UpdateAllocationStrategies (..)
       , BlacklistAllocation (..)
       , WhitelistAllocation (..)
       , UpdatePendingTxs (..)
       , InitWallet (..)
       ) where

import           Control.Exception    (throwIO)
import           Control.Lens         ((^.))
import           Control.Monad        (replicateM, unless)
import           Control.Monad.Trans  (MonadIO (liftIO))
import           Data.Acid            (EventResult, EventState, QueryEvent,
                                       UpdateEvent, makeAcidic)
import qualified Data.Acid            as A
import           Data.Map             (Map)
import           Data.SafeCopy        (base, deriveSafeCopy)
import           Data.Set             (Set)

import           Serokell.AcidState   (ExtendedState, closeExtendedState,
                                       openLocalExtendedState,
                                       openMemoryExtendedState, queryExtended,
                                       tidyExtendedState, updateExtended)

import qualified RSCoin.Core          as C
import           RSCoin.Core.Crypto   (keyGen)
import           RSCoin.Core.Strategy (AllocationInfo, MSAddress)

import           RSCoin.User.Logic    (getBlockchainHeight)
import           RSCoin.User.Wallet   (TxHStatus, TxHistoryRecord,
                                       WalletStorage)
import qualified RSCoin.User.Wallet   as W

$(deriveSafeCopy 0 'base ''TxHStatus)
$(deriveSafeCopy 0 'base ''TxHistoryRecord)
$(deriveSafeCopy 0 'base ''WalletStorage)

type UserState = ExtendedState WalletStorage

query
    :: (EventState event ~ WalletStorage, QueryEvent event, MonadIO m)
    => UserState -> event -> m (EventResult event)
query = queryExtended

update
    :: (EventState event ~ WalletStorage, UpdateEvent event, MonadIO m)
    => UserState -> event -> m (EventResult event)
update = updateExtended

-- | Opens ACID state. If not there, it returns unitialized
-- unoperatable storage.
openState :: MonadIO m => Bool -> FilePath -> m UserState
openState deleteIfExists path = do
    st <- openLocalExtendedState deleteIfExists path W.emptyWalletStorage
    st <$ tidyState st

openMemState :: MonadIO m => m UserState
openMemState = openMemoryExtendedState W.emptyWalletStorage

-- | Closes the ACID state.
closeState :: MonadIO m => UserState -> m ()
closeState st = tidyState st >> closeExtendedState st

-- | Tidies the ACID state.
tidyState :: MonadIO m => UserState -> m ()
tidyState = tidyExtendedState

isInitialized :: A.Query WalletStorage Bool
getSecretKey :: C.Address -> A.Query WalletStorage (Maybe (Maybe C.SecretKey))
findUserAddress :: C.Address -> A.Query WalletStorage (C.Address, Maybe C.SecretKey)
getDependentAddresses :: C.Address -> A.Query WalletStorage [C.Address]
getUserAddresses :: A.Query WalletStorage [(C.Address,C.SecretKey)]
getOwnedAddresses :: C.Address -> A.Query WalletStorage [C.Address]
getOwnedDefaultAddresses :: C.Address -> A.Query WalletStorage [C.Address]
getOwnedAddrIds :: C.Address -> C.Address -> A.Query WalletStorage [C.AddrId]
getTransactions :: C.Address -> C.Address -> A.Query WalletStorage [C.Transaction]
getLastBlockId :: A.Query WalletStorage Int
getTxsHistory :: A.Query WalletStorage [TxHistoryRecord]
getAddressStrategy :: C.Address -> A.Query WalletStorage (Maybe C.TxStrategy)
resolveAddressLocally :: C.AddrId -> A.Query WalletStorage (Maybe C.Address)
getAllocationStrategies :: A.Query WalletStorage (Map MSAddress AllocationInfo)
getIgnoredAllocationStrategies :: A.Query WalletStorage (Map MSAddress AllocationInfo)
getAllocationByIndex :: Int -> A.Query WalletStorage (MSAddress, AllocationInfo)
getPendingTxs :: A.Query WalletStorage [C.Transaction]

getSecretKey = W.getSecretKey
isInitialized = W.isInitialized
findUserAddress = W.findUserAddress
getDependentAddresses = W.getDependentAddresses
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
getIgnoredAllocationStrategies = W.getIgnoredAllocationStrategies
getAllocationByIndex = W.getAllocationByIndex
getPendingTxs = W.getPendingTxs

withBlockchainUpdate :: C.PeriodId -> C.HBlock -> A.Update WalletStorage ()
addTemporaryTransaction :: C.PeriodId -> C.Transaction -> A.Update WalletStorage ()
addAddress :: C.Address -> Maybe C.SecretKey -> Map C.PeriodId C.HBlock -> A.Update WalletStorage ()
deleteAddress :: C.Address -> A.Update WalletStorage ()
updateAllocationStrategies :: Map MSAddress AllocationInfo -> A.Update WalletStorage ()
blacklistAllocation :: MSAddress -> A.Update WalletStorage ()
whitelistAllocation :: MSAddress -> A.Update WalletStorage ()
updatePendingTxs :: Set C.Transaction -> A.Update WalletStorage ()
initWallet :: [(C.SecretKey,C.PublicKey)] -> Maybe Int -> A.Update WalletStorage ()

withBlockchainUpdate = W.withBlockchainUpdate
addTemporaryTransaction = W.addTemporaryTransaction
addAddress = W.addAddress
deleteAddress = W.deleteAddress
updateAllocationStrategies = W.updateAllocationStrategies
blacklistAllocation = W.blacklistAllocation
whitelistAllocation = W.whitelistAllocation
updatePendingTxs = W.updatePendingTxs
initWallet = W.initWallet

$(makeAcidic
      ''WalletStorage
      [ 'isInitialized
      , 'getSecretKey
      , 'findUserAddress
      , 'getDependentAddresses
      , 'getUserAddresses
      , 'getOwnedAddresses
      , 'getOwnedDefaultAddresses
      , 'getOwnedAddrIds
      , 'getTransactions
      , 'getLastBlockId
      , 'getTxsHistory
      , 'getAddressStrategy
      , 'resolveAddressLocally
      , 'getAllocationStrategies
      , 'getIgnoredAllocationStrategies
      , 'getAllocationByIndex
      , 'getPendingTxs
      , 'withBlockchainUpdate
      , 'addTemporaryTransaction
      , 'addAddress
      , 'deleteAddress
      , 'updateAllocationStrategies
      , 'blacklistAllocation
      , 'whitelistAllocation
      , 'updatePendingTxs
      , 'initWallet])

-- | This function generates 'n' new addresses ((pk,sk) pairs
-- essentially), and if the boolean flag 'is-bank-mode' is set, it
-- also loads secret bank key from ~/.rscoin/bankPrivateKey and adds
-- it to known addresses (public key is hardcoded in
-- RSCoin.Core.Constants).
initState :: C.WorkMode m => UserState -> Int -> Maybe FilePath -> m ()
initState st n (Just skPath) = do
    sk <- liftIO $ C.readSecretKey skPath
    initStateBank st n sk
initState st n Nothing = do
    height <- pred <$> getBlockchainHeight
    liftIO $
        do addresses <- replicateM n keyGen
           update st $ InitWallet addresses (Just height)
           tidyState st

-- | This function is is similar to the previous one, but is intended to
-- be used in tests/benchmark, where bank's secret key is embeded and
-- is not contained in file.
initStateBank :: C.WorkMode m => UserState -> Int -> C.SecretKey -> m ()
initStateBank st n sk = do
    genAdr <- (^. C.genesisAddress) <$> C.getNodeContext
    liftIO $ do
       let bankAddress = (sk, C.getAddress genAdr)
       unless (W.validateKeyPair genAdr sk) $
           throwIO $
           W.BadRequest "Imported bank's secret key doesn't belong to bank."
       addresses <- replicateM n keyGen
       update st $ InitWallet (bankAddress : addresses) Nothing
       tidyState st
