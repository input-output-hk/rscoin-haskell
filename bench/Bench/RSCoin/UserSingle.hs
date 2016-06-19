{-# LANGUAGE ScopedTypeVariables #-}

module Bench.RSCoin.UserSingle
        ( InfoStatus (..)
        , itemsAndTPS
        , printDynamicTPS
        , runSingleSuperUser
        , runSingleUser
        ) where

import           Prelude                  hiding (appendFile, readFile)

import           Control.Monad            (forM_, forever, when)
import           Control.Monad.Extra      (whenM)
import           Control.Monad.Trans      (liftIO)
import           Data.IORef               (IORef, atomicWriteIORef, newIORef,
                                           readIORef)
import           Data.Maybe               (fromMaybe)
import qualified Data.Text                as T
import qualified Data.Text.IO             as TIO
import           Formatting               (float, int, sformat, shown, stext, (%))
import           System.Directory         (doesFileExist, removeFile)

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

itemsAndTPS :: [T.Text] -> ([[T.Text]], [Double], Double)
itemsAndTPS textLines
    = let rows       = map (T.splitOn ",") textLines
          tpsPerLine = flip map (map tail rows) $ \[start, end, count] ->
              let startTime :: Second = read $ T.unpack start
                  endTime   :: Second = read $ T.unpack end
                  txNum     :: Double = read $ T.unpack count
              in txNum / fromIntegral (endTime - startTime)
          totalTPS   = sum tpsPerLine
      in (rows, tpsPerLine, totalTPS)

printDynamicTPS :: FilePath -> Bool -> IO ()
printDynamicTPS dumpFile shouldPrintTPS = when shouldPrintTPS $ do
    fileContent <- TIO.readFile dumpFile
    let tpsLines = T.lines fileContent
    let (rows, tpsPerLine, _) = itemsAndTPS tpsLines
    let rowsWithTPS = zipWith
            (\row tps -> sformat ("TX executed: " % stext % ", TPS: " % float) (row !! 4) tps)
            rows
            tpsPerLine
    TIO.putStrLn $ T.unlines rowsWithTPS

writeFileStats :: InfoStatus -> Second -> Word -> FilePath -> IO ()
writeFileStats status startTime txNum dumpFile = do
    currentTime :: Second <- getWallTime
    let rowStats = sformat (shown % "," % shown % "," % shown % "," % int % "\n")
                            status
                            startTime
                            currentTime
                            txNum
    liftIO $ TIO.appendFile dumpFile rowStats

dumpWorker :: IORef Word -> Second -> FilePath -> MsgPackRpc ()
dumpWorker countRef startTime dumpFile = forever $ do
    wait $ for dumpPeriod sec
    curTxNum <- liftIO $ readIORef countRef
    liftIO $ writeFileStats InProcess startTime curTxNum dumpFile
  where
    dumpPeriod = 10 :: Second

runSingleUser :: Maybe Word -> Word -> FilePath -> RSCoinUserState -> MsgPackRpc ()
runSingleUser logIntervalMaybe txNum dumpFile st = do
    -- clear file before printing results in it
    liftIO $ whenM (doesFileExist dumpFile) $ removeFile dumpFile

    startTime <- getWallTime
    cache     <- liftIO mkUserCache
    txCount   <- liftIO $ newIORef 0
    workerId  <- fork $ dumpWorker txCount startTime dumpFile
    address   <- Address . snd <$> liftIO keyGen
    let logInterval = fromMaybe (txNum `div` 5) logIntervalMaybe

    -- execute transactions
    forM_ [1 .. txNum] $ \i -> do
        executeTransaction st cache 1 address
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
