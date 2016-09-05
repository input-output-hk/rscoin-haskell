{-# LANGUAGE ViewPatterns #-}

module Bench.RSCoin.UserCommons
        ( benchUserTransactions
        , executeTransaction
        , finishBankPeriod
        , initializeBank
        , initializeUser
        , userThread
        , userThreadWithPath
        ) where

import           Control.Lens               ((^.))
import           Control.Monad              (forM_, when)
import           Control.Monad.Catch        (bracket)
import           Control.Monad.Trans        (liftIO)

import           Data.Optional              (Optional, defaultTo, empty)
import           Formatting                 (int, sformat, (%))
import           System.FilePath            ((</>))

import           RSCoin.Core                (Address (..), BankLocalControlRequest (FinishPeriod),
                                             Coin (..), CoinAmount (..), Color,
                                             ContextArgument (CADefault),
                                             RealMode, RealMode, genesisAddress,
                                             getBlockchainHeight,
                                             getNodeContext, keyGen, logDebug,
                                             logInfo, runRealModeUntrusted,
                                             sendBankLocalControlRequest, sign,
                                             userLoggerName)
import           RSCoin.Core.NodeConfig     (testBankSecretKey)
import qualified RSCoin.User                as U
import           RSCoin.User.Operations     (TransactionData (..),
                                             submitTransactionRetry)
import           RSCoin.Util.Timed          (for, sec, wait)

import           Bench.RSCoin.FilePathUtils (dbFormatPath, walletPathPrefix)

userThread
    :: FilePath
    -> (Word -> U.UserState -> RealMode a)
    -> Word
    -> IO a
userThread benchDir userAction userId
    = userThreadWithPath benchDir userAction userId empty

userThreadWithPath
    :: FilePath
    -> (Word -> U.UserState -> RealMode a)
    -> Word
    -> Optional FilePath
    -> IO a
userThreadWithPath benchDir userAction userId (defaultTo
                                                   (benchDir </>
                                                    dbFormatPath
                                                        walletPathPrefix
                                                        userId) -> walletPath) =
    runRealModeUntrusted userLoggerName CADefault $
    bracket (U.openState False walletPath) U.closeState (userAction userId)

queryMyAddress :: U.UserState -> Address -> RealMode Address
queryMyAddress userState =
    fmap head . U.query userState . U.GetOwnedDefaultAddresses

-- | Create user with 1 address and return it.
initializeUser :: Word -> U.UserState -> RealMode Address
initializeUser userId userState = do
    let userAddressesNumber = 1
    logDebug $ sformat ("Initializing user " % int % "…") userId
    U.initState userState userAddressesNumber Nothing
    genAddr <- (^. genesisAddress) <$> getNodeContext
    queryMyAddress userState genAddr <*
        logDebug (sformat ("Initialized user " % int % "…") userId)

executeTransaction :: U.UserState
                   -> U.UserCache
                   -> Color
                   -> Rational
                   -> Address
                   -> RealMode ()
executeTransaction userState cache coinColor coinAmount addrToSend =
    () <$ submitTransactionRetry maxRetries userState (Just cache) td
  where
    maxRetries = 50
    outputMoney = [Coin coinColor (CoinAmount coinAmount)]
    inputMoneyInfo = [(0, outputMoney)]
    td = TransactionData inputMoneyInfo addrToSend outputMoney

-- | Finishes current bank period.
finishBankPeriod :: RealMode ()
finishBankPeriod = do
    currentPeriodId <- getBlockchainHeight
    let signature    = sign testBankSecretKey currentPeriodId
    sendBankLocalControlRequest $ FinishPeriod signature

-- | Create user in `bankMode` and send coins to every user.
initializeBank :: Word -> [Address] -> U.UserState -> RealMode ()
initializeBank coinsNum userAddresses bankUserState = do
    logInfo "Initializaing user in bankMode…"
    let additionalBankAddreses = 0
    logDebug "Before initStateBank"
    U.initStateBank
        bankUserState
        additionalBankAddreses
        testBankSecretKey
    logDebug "After initStateBank"
    cache <- U.mkUserCache
    forM_ userAddresses $
        executeTransaction bankUserState cache 0 (fromIntegral coinsNum)
    logDebug "Sent initial coins from bank to users"
    logInfo "Initialized user in bankMode, finishing period"
    finishBankPeriod
    wait $ for 1 sec

-- | Do `txNum` transactions to random address.
benchUserTransactions :: Word
                      -> Word
                      -> U.UserState
                      -> RealMode ()
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
