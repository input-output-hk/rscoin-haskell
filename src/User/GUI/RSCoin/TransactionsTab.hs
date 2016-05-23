{-# LANGUAGE NoOverloadedStrings #-}

module GUI.RSCoin.TransactionsTab
       ( createTransactionsTab
       , initTransactionsTab
       ) where

import           Control.Monad         (unless, void)
import qualified Data.Text             as T
import           Graphics.UI.Gtk       (on)
import qualified Graphics.UI.Gtk       as G

import           GUI.RSCoin.Glade      (GladeMainWindow (..))
import           GUI.RSCoin.MainWindow (TransactionsTab (..))
import qualified GUI.RSCoin.MainWindow as M
import qualified RSCoin.Core           as C
import           RSCoin.Timed          (runRealMode)
import qualified RSCoin.User           as U

import           Serokell.Util.Text    (readUnsignedDecimal)

createTransactionsTab :: GladeMainWindow -> IO TransactionsTab
createTransactionsTab GladeMainWindow{..} =
    return $
    TransactionsTab
        gEntryPayTo
        gButtonChooseContacts
        gSpinButtonSendAmount
        gButtonConfirmSend
        gButtonClearSend

initTransactionsTab :: U.RSCoinUserState -> M.MainWindow -> IO ()
initTransactionsTab st mw = do
    let tr@TransactionsTab{..} = M.tabTransactions mw
    sendAmountAdjustment <- G.adjustmentNew 0 0 1000000000 1 1 1
    G.spinButtonSetAdjustment spinButtonSendAmount sendAmountAdjustment
    void (buttonClearSend `on` G.buttonActivated $ onClearButtonPressed tr)
    void (buttonConfirmSend `on` G.buttonActivated $ onSendButtonPressed st mw)

onClearButtonPressed :: TransactionsTab -> IO ()
onClearButtonPressed TransactionsTab{..} = do
    G.entrySetText entryPayTo ""
    G.entrySetText spinButtonSendAmount ""

onSendButtonPressed :: U.RSCoinUserState -> M.MainWindow -> IO ()
onSendButtonPressed st M.MainWindow{..} = do
    let TransactionsTab{..} = tabTransactions
    userAmount <- runRealMode $ U.getUserTotalAmount st
    sendAddressPre <- G.entryGetText entryPayTo
    sendAmount <- readUnsignedDecimal . T.pack <$> G.entryGetText spinButtonSendAmount
    let sendAddress = C.Address <$> (C.constructPublicKey $ T.pack sendAddressPre)
        reportSimpleError str = void $ do
            dialog <- G.messageDialogNew
                 (Just mainWindow)
                 [G.DialogDestroyWithParent]
                 G.MessageError
                 G.ButtonsClose
                 str
            G.dialogRun dialog
    case (sendAddress, sendAmount) of
        (Nothing,_) ->
            reportSimpleError "Couldn't read address, most probably wrong format."
        (_,Left _) ->
            reportSimpleError "Wrong amount format -- should be positive integer."
        (Just _, Right amount) -> do
            unless (amount > 0) $ reportSimpleError "Amount should be positive."
            unless (amount <= C.getCoin userAmount) $
                reportSimpleError $
                  "Amount exceeds your assets -- you have only " ++
                  show (C.getCoin userAmount) ++ " coins."
            dialog <- G.messageDialogNew
                 (Just mainWindow)
                 [G.DialogDestroyWithParent]
                 G.MessageInfo
                 G.ButtonsOk
                 "Successfully formed and sent transaction."
            void $ G.dialogRun dialog
    return ()
{-
    dialog <- G.dialogNew
    G.set dialog [G.windowTitle := "Enabling all connections"]
    dialogAddButton dialog stockYes ResponseYes
    dialogAddButton dialog stockNo ResponseNo
    upbox <- castToBox <$> dialogGetContentArea dialog
    lbl <- labelNew $ Just disableFirewallWarning
    set lbl [widgetMarginLeft := 10, widgetMarginRight := 10]
    boxPackStart upbox lbl PackGrow 10
    widgetShowAll upbox
    response <-
    widgetDestroy dialog
    if response == ResponseYes
        then do
            labelSetText labelFirewallStatus $ __ "Opening all connections..."
            void $ forkIO $ disconnectAction gui toEnable
        else updateVpnGUI gui =<< getCurrentVpnState
-}
{-
  where disableFirewallWarning =
          -- Yeah, long lines is the only way to fit it into translator
          (__ "This will disable restrictions to create connections only through VPN.\nThat means your connection to the internet could be tracked and somebody may eavesdrop on you.\nAre you sure you
 want to proceed?")
        disconnectAction gui toEnable = do
          cProgressBarPid <- startCProgressBar cProgressBar
          res <- if toEnable then enableVpnFirewall else disableVpnFirewall
          vpnStateRes <- checkVpnConnection
          postGUIAsync $ void $ updateVpnGUI gui $ VpnState vpnStateRes res
          stopCProgressBar cProgressBar cProgressBarPid
          planFirewallResync gui res
-}
