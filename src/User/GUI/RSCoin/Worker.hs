{-# LANGUAGE ScopedTypeVariables #-}
module GUI.RSCoin.Worker (startWorker) where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Exception       (SomeException)
import           Control.Monad           (forM_, forever, void, when)
import           Control.Monad.Catch     (catch, throwM)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Monoid             ((<>))
import qualified Graphics.UI.Gtk         as G

import           Serokell.Util.Text      (format', formatSingle')

import           GUI.RSCoin.AddressesTab (updateAddressTab)
import           GUI.RSCoin.GUIAcid      (GUIState)
import qualified GUI.RSCoin.MainWindow   as M
import           GUI.RSCoin.WalletTab    (updateWalletTab)
import qualified RSCoin.Core             as C
import           RSCoin.Timed            (WorkMode, runRealModeLocal)
import qualified RSCoin.User             as U

updateBlockchainWithProgress
    :: WorkMode m
    => U.RSCoinUserState -> M.MainWindow -> m Bool
updateBlockchainWithProgress st M.MainWindow{..} =
    work `catch` handler
  where
    handler (_ :: SomeException) = do
        postGUI $ G.labelSetText labelSync ("Offline" :: String)
        return False
    work = do
      walletHeight <- U.getLastBlockId st
      lastBlockHeight <- pred <$> C.getBlockchainHeight
      when (walletHeight > lastBlockHeight) $
          throwM $
          U.StorageError $
          U.InternalError $
          format'
              ("Last block height in wallet ({}) is greater than last " <>
               "block's height in bank ({}). Critical error.")
              (walletHeight, lastBlockHeight)
      when (lastBlockHeight /= walletHeight) $ do
          let diff = toInteger $ lastBlockHeight - walletHeight
              step = 1.0 / fromInteger diff
              idleTime = round $ 100 / toRational diff
              idleTimeShort = round $ 40 / toRational diff
          postGUI $ do
            G.progressBarSetFraction progressBarUpdate 0
            G.labelSetText labelSync $ formatSingle' "Syncing 0/{}" diff
          forM_
              [walletHeight + 1 .. lastBlockHeight]
              (\h -> do
                  postGUI $
                      G.labelSetText labelSync $
                      format' "Syncing {}/{}" (h - walletHeight, diff)
                  increasePG (step/2)
                  liftIO $ threadDelay $ idleTime * 1000
                  U.updateToBlockHeight st h
                  increasePG (step/2)
                  liftIO $ threadDelay $ idleTimeShort * 1000)
      postGUI $ do
          G.progressBarSetFraction progressBarUpdate 1
          G.labelSetText labelSync $ formatSingle' "Synced at height {}" lastBlockHeight
      return $ lastBlockHeight /= walletHeight
    postGUI = liftIO . G.postGUIAsync
    increasePG t = postGUI $ do
        v <- G.progressBarGetFraction progressBarUpdate
        G.progressBarSetFraction progressBarUpdate $ min 1.0 (v + t)

startWorker :: U.RSCoinUserState -> GUIState -> M.MainWindow -> IO ()
startWorker st gst mw@M.MainWindow{..} = void $ forkIO $ forever $ do
    threadDelay $ 1 * 1000000
    updated <- runRealModeLocal $ updateBlockchainWithProgress st mw
    when updated $ do
        G.postGUIAsync $ updateWalletTab st gst mw
        G.postGUIAsync $ updateAddressTab st mw
    threadDelay $ 2 * 1000000
