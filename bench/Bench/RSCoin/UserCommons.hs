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
import           Data.Acid.Advanced         (query')
import           Data.ByteString            (ByteString)
import           Data.Optional              (Optional, defaultTo, empty)
import           Formatting                 (int, sformat, (%))
import           System.FilePath            ((</>))

import           RSCoin.Core                (Address (..), Coin (..), Color,
                                             bankSecretKey, finishPeriod,
                                             keyGen)

import           RSCoin.Timed               (MsgPackRpc, for, runRealMode, sec,
                                             wait)
import qualified RSCoin.User.AcidState      as A
import           RSCoin.User.Cache          (UserCache, mkUserCache)
import           RSCoin.User.Operations     (FormTransactionData (..),
                                             formTransactionRetry)
import           RSCoin.User.Wallet         (UserAddress)

import           Bench.RSCoin.FilePathUtils (dbFormatPath, walletPathPrefix)
import           Bench.RSCoin.Logging       (logDebug, logInfo)

userThread
    :: ByteString
    -> FilePath
    -> (Word -> A.RSCoinUserState -> MsgPackRpc a)
    -> Word
    -> IO a
userThread bankHost benchDir userAction userId
    = userThreadWithPath bankHost benchDir userAction userId empty

userThreadWithPath
    :: ByteString
    -> FilePath
    -> (Word -> A.RSCoinUserState -> MsgPackRpc a)
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
        (liftIO $ A.openState walletPath)
        (\userState -> liftIO $ do
            createCheckpoint userState
            A.closeState userState)
        (userAction userId)

queryMyAddress :: A.RSCoinUserState -> MsgPackRpc UserAddress
queryMyAddress userState = head <$> query' userState A.GetAllAddresses

-- | Create user with 1 address and return it.
initializeUser :: Word -> A.RSCoinUserState -> MsgPackRpc UserAddress
initializeUser userId userState = do
    let userAddressesNumber = 1
    logDebug $ sformat ("Initializing user " % int % "…") userId
    A.initState userState userAddressesNumber Nothing
    queryMyAddress userState <*
        logDebug (sformat ("Initialized user " % int % "…") userId)

executeTransaction :: A.RSCoinUserState
                   -> UserCache
                   -> Color
                   -> Rational
                   -> Address
                   -> MsgPackRpc ()
executeTransaction userState cache coinColor coinAmount addrToSend =
    () <$ formTransactionRetry maxRetries userState (Just cache) ftd
  where
    maxRetries = 50
    outputMoney = [Coin coinColor coinAmount]
    inputMoneyInfo = [(0, outputMoney)]
    ftd = FormTransactionData inputMoneyInfo addrToSend outputMoney

-- | Create user in `bankMode` and send coins to every user.
initializeBank :: Word -> [Address] -> A.RSCoinUserState -> MsgPackRpc ()
initializeBank coinsNum userAddresses bankUserState = do
    logInfo "Initializaing user in bankMode…"
    let additionalBankAddreses = 0
    logDebug "Before initStateBank"
    A.initStateBank bankUserState additionalBankAddreses bankSecretKey
    logDebug "After initStateBank"
    cache <- liftIO mkUserCache
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
                      -> A.RSCoinUserState
                      -> MsgPackRpc ()
benchUserTransactions txNum userId userState = do
    let loggingStep = txNum `div` 5
    addr <- Address . snd <$> liftIO keyGen
    cache <- liftIO mkUserCache
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
