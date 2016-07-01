{-# LANGUAGE ViewPatterns #-}

module Bench.RSCoin.UserCommons
        ( benchUserTransactions
        , executeTransaction
        , initializeBank
        , initializeUser
        , userThread
        , userThreadWithPath
        ) where

import           Control.Monad              (forM_, when)
import           Control.Monad.Catch        (bracket)
import           Control.Monad.Trans        (liftIO)
import           Data.Acid                  (createCheckpoint)
import           Data.ByteString            (ByteString)
import           Data.Optional              (Optional, defaultTo, empty)
import           Formatting                 (int, sformat, (%))
import           System.FilePath            ((</>))

import           RSCoin.Core                (Address (..), Coin (..), Color,
                                             bankSecretKey, finishPeriod,
                                             keyGen)
import           RSCoin.Timed               (MsgPackRpc, for, runRealMode, sec,
                                             wait)
import qualified RSCoin.User                as U
import           RSCoin.User.Operations     (TransactionData (..),
                                             submitTransactionRetry)

import           Bench.RSCoin.FilePathUtils (dbFormatPath, walletPathPrefix)
import           Bench.RSCoin.Logging       (logDebug, logInfo)

userThread
    :: ByteString
    -> FilePath
    -> (Word -> U.RSCoinUserState -> MsgPackRpc a)
    -> Word
    -> IO a
userThread bankHost benchDir userAction userId
    = userThreadWithPath bankHost benchDir userAction userId empty

userThreadWithPath
    :: ByteString
    -> FilePath
    -> (Word -> U.RSCoinUserState -> MsgPackRpc a)
    -> Word
    -> Optional FilePath
    -> IO a
userThreadWithPath
    bankHost
    benchDir
    userAction
    userId
    (defaultTo (benchDir </> dbFormatPath walletPathPrefix userId) -> walletPath)
  =
    runRealMode bankHost $ bracket
        (liftIO $ U.openState walletPath)
        (\userState -> liftIO $ do
            createCheckpoint userState
            U.closeState userState)
        (userAction userId)

queryMyAddress :: U.RSCoinUserState -> MsgPackRpc Address
queryMyAddress userState = head <$> U.getAllAddresses userState

-- | Create user with 1 address and return it.
initializeUser :: Word -> U.RSCoinUserState -> MsgPackRpc Address
initializeUser userId userState = do
    let userAddressesNumber = 1
    logDebug $ sformat ("Initializing user " % int % "…") userId
    U.initState userState userAddressesNumber Nothing
    queryMyAddress userState <*
        logDebug (sformat ("Initialized user " % int % "…") userId)

executeTransaction :: U.RSCoinUserState
                   -> U.UserCache
                   -> Color
                   -> Rational
                   -> Address
                   -> MsgPackRpc ()
executeTransaction userState cache coinColor coinAmount addrToSend =
    () <$ submitTransactionRetry maxRetries userState (Just cache) td
  where
    maxRetries = 50
    outputMoney = [Coin coinColor coinAmount]
    inputMoneyInfo = [(0, outputMoney)]
    td = TransactionData inputMoneyInfo addrToSend outputMoney

-- | Create user in `bankMode` and send coins to every user.
initializeBank :: Word -> [Address] -> U.RSCoinUserState -> MsgPackRpc ()
initializeBank coinsNum userAddresses bankUserState = do
    logInfo "Initializaing user in bankMode…"
    let additionalBankAddreses = 0
    logDebug "Before initStateBank"
    U.initStateBank bankUserState additionalBankAddreses bankSecretKey
    logDebug "After initStateBank"
    cache <- U.mkUserCache
    forM_ userAddresses $
        executeTransaction bankUserState cache 0 (fromIntegral coinsNum)
    logDebug "Sent initial coins from bank to users"
    logInfo
        "Initialized user in bankMode, finishing period"
    finishPeriod bankSecretKey
    wait $ for 1 sec

-- | Do `txNum` transactions to random address.
benchUserTransactions :: Word
                      -> Word
                      -> U.RSCoinUserState
                      -> MsgPackRpc ()
benchUserTransactions txNum userId userState = do
    let loggingStep = txNum `div` 5
    addr <- Address . snd <$> liftIO keyGen
    cache <- U.mkUserCache
    forM_ [0 .. txNum - 1] $
        \i ->
             do when (i /= 0 && i `mod` loggingStep == 0) $
                    logInfo $
                    sformat
                        ("User " % int % " has executed " % int %
                         " transactions")
                        userId
                        i
                executeTransaction userState cache 0 1 addr
