module AcidExecutor where

import Control.Monad.STM
import Control.Concurrent.STM.TBQueue

import Control.Lens

import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class

import Data.Acid
import Data.Int

import Graphics.UI.Gtk

import RSCoin.Core
import RSCoin.User
import RSCoin.Test

import OutputWidgets

import System.IO

data Operation = Send [(Int, Int64)] Address Coin | Update | Exit

updateUI :: RSCoinUserState -> OutputWidgets -> IO ()
updateUI st ow = do
    return ()
    putStrLn "Updating UI."
    as <- liftIO $ query st GetAllAddresses
    let a = head as
    amount <- getAmount st a
    postGUIAsync $ do
        labelSetText (balanceLabel ow) $ show $ getCoin amount
        labelSetText (myWalletLabel ow) $ show $ a ^. publicAddress

run :: TBQueue Operation -> OutputWidgets -> IO ()
run queue ow = runRealMode $ bracket'
    (liftIO $ openState "wallet-db")
    (\st -> liftIO $ do
        createCheckpoint st
        closeState st)
    (\st -> do
        dskp <- liftIO defaultSecretKeyPath
        handleUninitialized (run' st) $ initState st 1 $ Just dskp)
  where
    run' :: WorkMode m => RSCoinUserState -> m ()
    run' st = do
        o <- liftIO $ atomically $ readTBQueue queue
        case o of
            Send i a c -> formTransaction st i a c >> run' st
            Update     -> do
                walletHeight    <- liftIO $ query st GetLastBlockId
                lastBlockHeight <- pred <$> getBlockchainHeight
                if walletHeight < lastBlockHeight then do
                    forM_ [walletHeight + 1 .. lastBlockHeight] $ updateToBlockHeight st
                    liftIO $ updateUI st ow
                else return ()
                run' st
            Exit        -> return ()
    handleUninitialized :: (MonadIO m, MonadCatch m) => m () -> m () -> m ()
    handleUninitialized action initialize = action `catch` handler initialize action
    handler i a NotInitialized = liftIO (putStrLn "Initializing storage ...") >> i >> a
