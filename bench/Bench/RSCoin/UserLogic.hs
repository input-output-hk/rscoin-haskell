module Bench.RSCoin.UserLogic
        ( initializeBank
        , initializeUser
        , userThread
        ) where

import           Bench.RSCoin.FilePathUtils (dbFormatPath)

import           Control.Monad              (forM_)
import           Control.Monad.Trans        (liftIO)
import           Data.Acid                  (createCheckpoint, query)
import           Data.Int                   (Int64)

import           RSCoin.Core                (Coin (..))

import qualified RSCoin.User.AcidState      as A
import           RSCoin.User.Commands       (UserCommand (UpdateBlockchain),
                                             proceedCommand)
import           RSCoin.User.Operations     (formTransaction)
import qualified RSCoin.User.Wallet         as W

import           RSCoin.Timed               (MsgPackRpc, bracket', runRealMode)

import           System.FilePath            ((</>))

queryMyAddress :: A.RSCoinUserState -> MsgPackRpc W.UserAddress
queryMyAddress userState = do
    allAddresess <- liftIO $ query userState A.GetAllAddresses
    return $ head allAddresess

initializeUser :: A.RSCoinUserState -> MsgPackRpc W.UserAddress
initializeUser userState = do
    let userAddressesNumber = 1
    A.initState userState userAddressesNumber Nothing
    queryMyAddress userState

initializeBank :: FilePath -> [W.UserAddress] -> A.RSCoinUserState -> MsgPackRpc ()
initializeBank bankKeyFilePath userAddresses bankUserState = do
    let additionalBankAddreses = 0
    A.initState bankUserState additionalBankAddreses (Just bankKeyFilePath)

    let outputNumber   = 1000
    let outputMoney    = Coin outputNumber
    let inputMoneyInfo = [(1, outputNumber)]
    forM_ userAddresses $ \userAddr -> do
        proceedCommand bankUserState UpdateBlockchain
        formTransaction bankUserState inputMoneyInfo (W.toAddress userAddr) outputMoney

userThread :: FilePath -> (A.RSCoinUserState -> MsgPackRpc a) -> Int64 -> IO a
userThread benchDir userAction userId = runRealMode $ bracket'
    (liftIO $ A.openState $ benchDir </> dbFormatPath "wallet-db" userId)
    (\userState -> liftIO $ do
        createCheckpoint userState
        A.closeState userState
    )
    userAction