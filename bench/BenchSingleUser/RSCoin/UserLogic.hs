{-# LANGUAGE PatternGuards #-}

module BenchSingleUser.RSCoin.UserLogic
        ( runBankUser
        , userThread
        ) where


import           Control.Monad                        (forM_, replicateM, when)
import           Control.Monad.Catch                  (bracket)
import           Control.Monad.Trans                  (liftIO)
import           Data.Acid                            (createCheckpoint)
import           Data.Int                             (Int64)
import           Formatting                           (int, sformat, (%))
import           System.FilePath                      ((</>))

import           RSCoin.Core                          (Address (..), Coin (..),
                                                       bankSecretKey, keyGen)

import           RSCoin.Timed                         (MsgPackRpc, runRealMode)
import qualified RSCoin.User.AcidState                as A
import           RSCoin.User.Operations               (formTransactionRetry,
                                                       updateBlockchain)

import           BenchSingleUser.RSCoin.FilePathUtils (dbFormatPath)
import           BenchSingleUser.RSCoin.Logging       (logDebug, logInfo)

transactionNum :: Num a => a
transactionNum = 50

userThread :: FilePath -> (A.RSCoinUserState -> MsgPackRpc a) -> Int64 -> IO a
userThread benchDir userAction userId = runRealMode $ bracket
    (liftIO $ A.openState $ benchDir </> dbFormatPath "wallet-db" userId)
    (\userState -> liftIO $ do
        createCheckpoint userState
        A.closeState userState
    )
    userAction

executeTransaction :: A.RSCoinUserState -> Int64 -> Address -> MsgPackRpc ()
executeTransaction userState coinAmount addrToSend = do
    () <$ updateBlockchain userState False
    formTransactionRetry
        maxBound
        userState
        False
        inputMoneyInfo
        addrToSend
        outputMoney
  where
    outputMoney = Coin coinAmount
    inputMoneyInfo = [(1, outputMoney)]

runBankUser :: A.RSCoinUserState -> MsgPackRpc ()
runBankUser bankUserState = do
    addresses <-
        map (Address . snd) <$> replicateM transactionNum (liftIO keyGen)
    let additionalBankAddreses = 0
    logDebug "Before initStateBank"
    A.initStateBank bankUserState additionalBankAddreses bankSecretKey
    logDebug "After initStateBank"
    forM_ (zip [(1 :: Int) ..] addresses) $
        \(i,addr) ->
             do executeTransaction bankUserState transactionNum addr
                when (i `mod` (transactionNum `div` 5) == 0) $
                    logInfo $ sformat ("Executed " % int % " transactions") i
