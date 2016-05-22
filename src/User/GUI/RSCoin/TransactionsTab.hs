{-# LANGUAGE NoOverloadedStrings #-}

module GUI.RSCoin.TransactionsTab (initTransactionsTab) where

import qualified Graphics.UI.Gtk       as G

import           GUI.RSCoin.Glade      (GladeMainWindow (..))
import qualified GUI.RSCoin.MainWindow as M

initTransactionsTab :: GladeMainWindow -> IO M.TransactionsTab
initTransactionsTab GladeMainWindow{..} = do
    sendAmountAdjustment <- G.adjustmentNew 0 0 1000000000 1 1 1
    G.spinButtonSetAdjustment gSpinButtonSendAmount sendAmountAdjustment
    return $
        M.TransactionsTab
            gEntryPayTo
            gButtonChooseContacts
            gSpinButtonSendAmount
            gButtonConfirmSend
            gButtonClearSend
