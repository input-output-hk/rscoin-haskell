
{-# LANGUAGE ScopedTypeVariables #-}

module Bench.RSCoin.UserSingle
        ( InfoStatus (..)
        , runSingleUser
        , runSingleSuperUser
        ) where

import           Prelude                  hiding (appendFile)

import           Control.Monad            (forM_, forever, when)
import           Control.Monad.Trans      (liftIO)
import           Data.IORef               (IORef, atomicWriteIORef, newIORef,
                                           readIORef)
import           Data.Maybe               (fromMaybe)
import           Data.Text.IO             (appendFile)
import           Formatting               (int, sformat, shown, (%))

import           Serokell.Util.Bench      (getWallTime)

import           RSCoin.Core              (Address (..), bankSecretKey, keyGen)
import           RSCoin.Timed             (MsgPackRpc, Second, for, fork,
                                           killThread, sec, wait)
import           RSCoin.User.AcidState    (RSCoinUserState, initStateBank)
import           RSCoin.User.Cache        (mkUserCache)

import           Bench.RSCoin.Logging     (logDebug, logInfo)
import           Bench.RSCoin.UserCommons (executeTransaction)

data InfoStatus = InProcess | Final
    deriving (Read, Show)

writeFileStats :: InfoStatus -> Second -> Word -> FilePath -> IO ()
writeFileStats status startTime txNum dumpFile = do
    currentTime :: Second <- getWallTime
    let rowStats = sformat (shown % "," % shown % "," % shown % "," % int % "\n")
                            status
                            startTime
                            currentTime
                            txNum
    liftIO $ appendFile dumpFile rowStats

dumpWorker :: IORef Word -> Second -> FilePath -> MsgPackRpc ()
dumpWorker countRef startTime dumpFile = forever $ do
    wait $ for dumpPeriod sec
    curTxNum <- liftIO $ readIORef countRef
    liftIO $ writeFileStats InProcess startTime curTxNum dumpFile
  where
    dumpPeriod = 10 :: Second

runSingleUser :: Maybe Word -> Word -> FilePath -> RSCoinUserState -> MsgPackRpc ()
runSingleUser logIntervalMaybe txNum dumpFile st = do
    startTime <- getWallTime
    cache     <- liftIO mkUserCache
    txCount   <- liftIO $ newIORef 0
    workerId  <- fork $ dumpWorker txCount startTime dumpFile
    address   <- Address . snd <$> liftIO keyGen
    let logInterval = fromMaybe (txNum `div` 5) logIntervalMaybe

    -- execute transactions
    forM_ [1 .. txNum] $ \i -> do
        executeTransaction st cache 0 1 address
        liftIO $ atomicWriteIORef txCount i

        when (i `mod` logInterval == 0) $
            logInfo $ sformat ("Executed " % int % " transactions") i

    killThread workerId
    liftIO $ writeFileStats Final startTime txNum dumpFile

runSingleSuperUser :: Maybe Word -> Word -> FilePath -> RSCoinUserState -> MsgPackRpc ()
runSingleSuperUser logInterval txNum dumpFile bankUserState = do
    let additionalBankAddreses = 0
    logDebug "Before initStateBank"
    initStateBank bankUserState additionalBankAddreses bankSecretKey
    logDebug "After initStateBank"
    runSingleUser logInterval txNum dumpFile bankUserState
