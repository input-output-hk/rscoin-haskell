{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

-- | Functions launching Bank.

module RSCoin.Bank.Launcher
       ( bankWrapperReal
       , launchBankReal
       , launchBank
       , addMintetteIO
       , addAddressIO
       ) where

import           Control.Monad.Catch   (bracket)
import           Control.Monad.Trans   (liftIO)
import           Data.Acid.Advanced    (update')
import           Data.Functor          (void)
import           Data.Time.Units       (TimeUnit)

import           RSCoin.Core           (Address, Mintette, PublicKey, SecretKey,
                                        Strategy)
import           RSCoin.Timed          (MsgPackRpc, ThreadId, WorkMode, fork,
                                        killThread, runRealModeLocal)

import           RSCoin.Bank.AcidState (AddAddress (AddAddress),
                                        AddMintette (AddMintette), State,
                                        closeState, openState)
import           RSCoin.Bank.Server    (serve)
import           RSCoin.Bank.Worker    (runWorkerWithPeriod)

bankWrapperReal :: FilePath -> (State -> MsgPackRpc a) -> IO a
bankWrapperReal storagePath =
    runRealModeLocal .
    bracket (liftIO $ openState storagePath) (liftIO . closeState)

launchBankReal :: (TimeUnit t) => t -> FilePath -> SecretKey -> IO ()
launchBankReal periodDelta storagePath sk =
    bankWrapperReal storagePath $ void . launchBank periodDelta sk

launchBank
    :: (TimeUnit t, WorkMode m)
    => t -> SecretKey -> State -> m ThreadId
launchBank periodDelta sk st = do
    workerThread <- fork $ runWorkerWithPeriod periodDelta sk st
    workerThread <$ serve st workerThread restartWorkerAction
  where
    restartWorkerAction tId = do
        killThread tId
        fork $ runWorkerWithPeriod periodDelta sk st

addAddressIO :: FilePath -> Address -> Strategy -> IO ()
addAddressIO storagePath a s =
    bankWrapperReal storagePath $ flip update' (AddAddress a s)

addMintetteIO :: FilePath -> Mintette -> PublicKey -> IO ()
addMintetteIO storagePath m k =
    bankWrapperReal storagePath $ flip update' (AddMintette m k)
