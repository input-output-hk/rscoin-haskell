{-# LANGUAGE PatternGuards #-}

module Bench.RSCoin.UserLogic
        ( benchUserTransactions
        , initializeBank
        , initializeUser
        , userThread
        ) where


import           Control.Exception          (SomeException, fromException)
import           Control.Monad              (forM_)
import           Control.Monad.Catch        (MonadCatch, MonadThrow, bracket,
                                             catch, throwM)
import           Control.Monad.Trans        (liftIO)
import           Data.Acid                  (createCheckpoint)
import           Data.Acid.Advanced         (query')
import           Data.Int                   (Int64)
import           Data.Text                  (isPrefixOf)
import           Debug.Trace                (trace)
import           System.FilePath            ((</>))

import           Serokell.Util              (indexModulo)

import           RSCoin.Core                (Coin (..), bankSecretKey)

import           RSCoin.Timed               (MsgPackRpc, runRealMode)
import qualified RSCoin.User.AcidState      as A
import           RSCoin.User.Error          (UserError (InputProcessingError))
import           RSCoin.User.Operations     (formTransactionRetry,
                                             updateBlockchain)
import           RSCoin.User.Wallet         (UserAddress, toAddress)

import           Bench.RSCoin.FilePathUtils (dbFormatPath)

transactionNum :: Int64
transactionNum = 2

userThread :: FilePath -> (A.RSCoinUserState -> MsgPackRpc a) -> Int64 -> IO a
userThread benchDir userAction userId = runRealMode $ bracket
    (liftIO $ A.openState $ benchDir </> dbFormatPath "wallet-db" userId)
    (\userState -> liftIO $ do
        createCheckpoint userState
        A.closeState userState
    )
    userAction

queryMyAddress :: A.RSCoinUserState -> MsgPackRpc UserAddress
queryMyAddress userState = head <$> query' userState A.GetAllAddresses

-- | Create user with 1 address and return it.
initializeUser :: A.RSCoinUserState -> MsgPackRpc UserAddress
initializeUser userState = do
    let userAddressesNumber = 1
    A.initState userState userAddressesNumber Nothing
    queryMyAddress userState

executeTransaction :: A.RSCoinUserState -> Int64 -> UserAddress -> MsgPackRpc ()
executeTransaction userState coinAmount addrToSend = do
    let outputMoney = Coin coinAmount
    let inputMoneyInfo = [(1, outputMoney)]
    let transactionAction =
            updateBlockchain userState False >>
            formTransactionRetry
                maxBound
                userState
                False
                inputMoneyInfo
                (toAddress addrToSend)
                outputMoney
    walletHeightHandler transactionAction
  where
    walletHeightHandler
        :: (MonadCatch m, MonadThrow m)
        => m () -> m ()
    walletHeightHandler transactionAction =
        transactionAction `catch` repeatIfHeightMismatch transactionAction
    repeatIfHeightMismatch
        :: (MonadCatch m, MonadThrow m)
        => m () -> SomeException -> m ()
    repeatIfHeightMismatch transactionAction e
      | Just (InputProcessingError errText) <- fromException e
      , "Wallet isn't updated" `isPrefixOf` errText  -- TODO: not so good check
       =
          trace "T: wallet height" $ walletHeightHandler transactionAction
      | otherwise = trace "WAAAAAAT?" $ throwM e

-- | Create user in `bankMode` and send `transactionNum` coins to
-- every user from list.
initializeBank :: [UserAddress] -> A.RSCoinUserState -> MsgPackRpc ()
initializeBank userAddresses bankUserState = do
    let additionalBankAddreses = 0
    A.initStateBank bankUserState additionalBankAddreses bankSecretKey
    forM_ userAddresses $ executeTransaction bankUserState transactionNum

-- | Start user with provided addresses of other users and do
-- `transactionNum` transactions.
benchUserTransactions :: [UserAddress] -> A.RSCoinUserState -> MsgPackRpc ()
benchUserTransactions allAddresses userState = do
    myAddress <- queryMyAddress userState
    let otherAddresses = filter (/= myAddress) allAddresses
    forM_ [0 .. transactionNum - 1] $
        executeTransaction userState 1 . (otherAddresses `indexModulo`)
