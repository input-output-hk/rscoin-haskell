module Bench.RSCoin.UserSingle
        ( runSingleSuperUser
        ) where

import           Prelude                  hiding (appendFile)

import           Control.Monad            (forM_, forever, void, when)
import           Control.Monad.Trans      (liftIO)

import           Data.IORef               (IORef, atomicWriteIORef,
                                           newIORef, readIORef)
import           Data.Text.IO             (appendFile)
import           Formatting               (int, sformat, shown, (%))

import           RSCoin.Core              (Address (..),
                                           bankSecretKey, keyGen)
import           RSCoin.Timed             (MsgPackRpc, Second, for, fork,
                                           killThread, myThreadId, sec, wait)
import           RSCoin.User.AcidState    (RSCoinUserState, initStateBank)
import           RSCoin.User.Cache        (mkUserCache)

import           Bench.RSCoin.Logging     (logDebug, logInfo)
import           Bench.RSCoin.UserCommons (executeTransaction)
import           Bench.RSCoin.TimeUtils   (getCurrentTime)

dumpPeriod :: Second
dumpPeriod = 10

dumpWorker :: IORef Word -> FilePath -> MsgPackRpc ()
dumpWorker countRef dumpFile = do
    startTime <- liftIO $ getCurrentTime

    forever $ do
        wait $ for dumpPeriod sec

        currentTxNum <- liftIO $ readIORef countRef
        when (currentTxNum == maxBound) $ killThread =<< myThreadId

        currentTime <- liftIO $ getCurrentTime
        let rowStats = sformat (shown % "," % shown % "," % int)
                                startTime
                                currentTime
                                currentTxNum
        liftIO $ appendFile dumpFile rowStats

runSingleSuperUser :: Word -> FilePath -> RSCoinUserState -> MsgPackRpc ()
runSingleSuperUser txNum dumpFile bankUserState = do
    address <- Address . snd <$> liftIO keyGen
    let additionalBankAddreses = 0

    logDebug "Before initStateBank"
    initStateBank bankUserState additionalBankAddreses bankSecretKey
    logDebug "After initStateBank"

    cache   <- liftIO mkUserCache
    txCount <- liftIO $ newIORef 0
    void $ fork $ dumpWorker txCount dumpFile

    -- execute transactions
    forM_ [1 .. txNum] $ \i -> do
        executeTransaction bankUserState cache 1 address
        liftIO $ atomicWriteIORef txCount i

        when (i `mod` (txNum `div` 5) == 0) $
            logInfo $ sformat ("Executed " % int % " transactions") i

    liftIO $ atomicWriteIORef txCount maxBound
    wait $ for dumpPeriod sec