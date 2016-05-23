{-# LANGUAGE NoOverloadedStrings #-}

module GUI.RSCoin.TransactionsTab
       ( createTransactionsTab
       , initTransactionsTab
       ) where

import           Control.Monad         (void)
import           Graphics.UI.Gtk       (on)
import qualified Graphics.UI.Gtk       as G

import           GUI.RSCoin.Glade      (GladeMainWindow (..))
import           GUI.RSCoin.MainWindow (TransactionsTab (..))
import qualified GUI.RSCoin.MainWindow as M

createTransactionsTab :: GladeMainWindow -> IO TransactionsTab
createTransactionsTab GladeMainWindow{..} =
    return $
    TransactionsTab
        gEntryPayTo
        gButtonChooseContacts
        gSpinButtonSendAmount
        gButtonConfirmSend
        gButtonClearSend

initTransactionsTab :: M.MainWindow -> IO ()
initTransactionsTab mw = do
    let tr@TransactionsTab{..} = M.tabTransactions mw
    sendAmountAdjustment <- G.adjustmentNew 0 0 1000000000 1 1 1
    G.spinButtonSetAdjustment spinButtonSendAmount sendAmountAdjustment
    void (buttonClearSend `on` G.buttonActivated $ onClearButtonPressed tr)
    void (buttonConfirmSend `on` G.buttonActivated $ onSendButtonPressed tr)

onClearButtonPressed :: TransactionsTab -> IO ()
onClearButtonPressed TransactionsTab{..} = do
    G.entrySetText entryPayTo ""
    G.entrySetText spinButtonSendAmount ""

onSendButtonPressed :: TransactionsTab -> IO ()
onSendButtonPressed = undefined
