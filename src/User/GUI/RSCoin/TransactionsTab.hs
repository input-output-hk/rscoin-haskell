{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GUI.RSCoin.TransactionsTab
       ( createTransactionsTab
       , initTransactionsTab
       ) where

import           Control.Exception     (SomeException (..), catch)
import           Control.Monad         (void)
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
    sendAmountAdjustment <- G.adjustmentNew 0 0 99999999999 1 1 1
    G.spinButtonSetAdjustment spinButtonSendAmount sendAmountAdjustment
    void (buttonClearSend `on` G.buttonActivated $ onClearButtonPressed tr)
    void (buttonConfirmSend `on` G.buttonActivated $ onSendButtonPressed st mw)

onClearButtonPressed :: TransactionsTab -> IO ()
onClearButtonPressed TransactionsTab{..} = do
    G.entrySetText entryPayTo ""
    G.entrySetText spinButtonSendAmount ""

onSendButtonPressed :: U.RSCoinUserState -> M.MainWindow -> IO ()
onSendButtonPressed st M.MainWindow{..} =
    act `catch` handler
  where
    reportSimpleError str = void $ do
        dialog <- G.messageDialogNew
             (Just mainWindow)
             [G.DialogDestroyWithParent]
             G.MessageError
             G.ButtonsClose
             str
        void (G.dialogRun dialog)
        G.widgetDestroy dialog
    handler (e :: SomeException) =
        reportSimpleError $ "Exception has happened: " ++ show e
    act = do
      let TransactionsTab{..} = tabTransactions
      sendAddressPre <- G.entryGetText entryPayTo
      sendAmount <- readUnsignedDecimal . T.pack <$> G.entryGetText spinButtonSendAmount
      let sendAddress = C.Address <$> C.constructPublicKey (T.pack sendAddressPre)
      constructDialog sendAddress sendAmount
    constructDialog Nothing _ =
        reportSimpleError "Couldn't read address, most probably wrong format."
    constructDialog _ (Left _) =
        reportSimpleError "Wrong amount format -- should be positive integer."
    constructDialog (Just _) (Right amount) | amount <= 0 = do
        reportSimpleError "Amount should be positive."
        G.entrySetText (spinButtonSendAmount tabTransactions) ""
    constructDialog (Just address) (Right amount) = do
        userAmount <- runRealMode $ U.getUserTotalAmount st
        if amount > C.getCoin userAmount
        then do
            reportSimpleError $
                 "Amount exceeds your assets -- you have only " ++
                 show (C.getCoin userAmount) ++ " coins."
            G.entrySetText (spinButtonSendAmount tabTransactions) $ show userAmount
        else do
            runRealMode $ U.formTransactionFromAll st address $ C.Coin amount
            dialog <- G.messageDialogNew
                (Just mainWindow)
                [G.DialogDestroyWithParent]
                G.MessageInfo
                G.ButtonsOk
                "Successfully formed and sent transaction."
            void $ G.dialogRun dialog
            G.widgetDestroy dialog
