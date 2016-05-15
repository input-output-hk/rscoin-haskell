module Bench.RSCoin.UserLogic
        ( benchUserTransactions
        , initializeBank
        , initializeUser
        , userThread
        ) where

import           Bench.RSCoin.FilePathUtils (dbFormatPath)

import           Control.Monad              (forM_)
import           Control.Monad.Catch        (bracket)
import           Control.Monad.Trans        (liftIO)
import           Data.Acid                  (createCheckpoint, query)
import           Data.Int                   (Int64)

import           RSCoin.Core                (Coin (..))

import qualified RSCoin.User.AcidState      as A
import           RSCoin.User.Commands       (UserCommand (UpdateBlockchain),
                                             proceedCommand)
import           RSCoin.User.Operations     (getAmount, formTransaction)
import qualified RSCoin.User.Wallet         as W

import           RSCoin.Timed               (MsgPackRpc, runRealMode)

import           System.FilePath            ((</>))

userThread :: FilePath -> (A.RSCoinUserState -> MsgPackRpc a) -> Int64 -> IO a
userThread benchDir userAction userId = runRealMode $ bracket
    (liftIO $ A.openState $ benchDir </> dbFormatPath "wallet-db" userId)
    (\userState -> liftIO $ do
        createCheckpoint userState
        A.closeState userState
    )
    userAction

queryMyAddress :: A.RSCoinUserState -> MsgPackRpc W.UserAddress
queryMyAddress userState = do
    allAddresess <- liftIO $ query userState A.GetAllAddresses
    return $ head allAddresess

-- | Create user with 1 address and return it.
initializeUser :: A.RSCoinUserState -> MsgPackRpc W.UserAddress
initializeUser userState = do
    let userAddressesNumber = 1
    A.initState userState userAddressesNumber Nothing
    queryMyAddress userState

-- | Create user in `bankMode` and send 1000 coins to every user from list.
initializeBank :: FilePath -> [W.UserAddress] -> A.RSCoinUserState -> MsgPackRpc ()
initializeBank bankKeyFilePath userAddresses bankUserState = do
    let additionalBankAddreses = 0
    A.initState bankUserState additionalBankAddreses (Just bankKeyFilePath)

    let outputMoney    = Coin 1000
    let inputMoneyInfo = [(1, outputMoney)]
    forM_ userAddresses $ \userAddr -> do
        proceedCommand bankUserState UpdateBlockchain
        formTransaction bankUserState inputMoneyInfo (W.toAddress userAddr) outputMoney

-- | Start user with provided addresses of other users and do 1000 transactions.
benchUserTransactions :: [W.UserAddress] -> A.RSCoinUserState -> MsgPackRpc ()
benchUserTransactions allAddresses userState = do
    myAddress         <- queryMyAddress userState
    liftIO $ putStrLn $ "Real addr: " ++ show myAddress
    myAmount <- getAmount userState myAddress
    liftIO $ putStrLn $ "My amount: " ++ show myAmount

    let otherAddresses = filter (/= myAddress) allAddresses
    let numberOfUsers  = length otherAddresses
    let outputMoney    = Coin 1
    let inputMoneyInfo = [(1, outputMoney)]
    forM_ [0..3] $ \i -> do
        liftIO $ putStrLn $ "Iter: " ++ show i
        let userAddrToSend = otherAddresses !! (i `mod` numberOfUsers)
        proceedCommand userState UpdateBlockchain
        formTransaction userState inputMoneyInfo (W.toAddress userAddrToSend) outputMoney