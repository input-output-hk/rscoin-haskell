module GUI.RSCoin.Worker (startWorker) where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Monad           (forever, void, when)
import qualified Graphics.UI.Gtk         as G

import           RSCoin.Timed            (runRealMode)
import qualified RSCoin.User             as U

import           GUI.RSCoin.AddressesTab (updateAddressTab)
import           GUI.RSCoin.GUIAcid      (GUIState)
import qualified GUI.RSCoin.MainWindow   as M
import           GUI.RSCoin.WalletTab    (updateWalletTab)


startWorker :: U.RSCoinUserState -> GUIState -> M.MainWindow -> IO ()
startWorker st gst mw@M.MainWindow{..} = void $ forkIO $ forever $ do
    threadDelay $ 2 * 1000000
    updated <- runRealMode $ U.updateBlockchain st False
    when updated $ do
        G.postGUIAsync $ updateWalletTab st gst mw
        G.postGUIAsync $ updateAddressTab st mw
