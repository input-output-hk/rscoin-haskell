{-# LANGUAGE RecordWildCards #-}

-- | Data structore to store output GUI widgets.

module RSCoin.User.OutputWidgets
    ( OutputWidgets (..)
    , mkOutputWidgets
    ) where

import qualified Graphics.UI.Gtk          as G

import           RSCoin.User.Transactions (VerboseTransaction)

data OutputWidgets = OutputWidgets
    { balanceLabel         :: G.Label
    , unconfirmedLabel     :: G.Label
    , transactionsLabel    :: G.Label
    , recentActivityView   :: G.TreeView
    , transactionsNumLabel :: G.Label
    , transactionsView     :: G.TreeView
    , statusLabel          :: G.Label
    , notificationWindow   :: G.Window
    , messageLabel         :: G.Label
    , transactionsList     :: G.ListStore VerboseTransaction
    }

mkOutputWidgets :: G.Builder -> IO OutputWidgets
mkOutputWidgets builder = do
    let getWidget :: G.GObjectClass c => (G.GObject -> c) -> String -> IO c
        getWidget = G.builderGetObject builder

    balanceLabel         <- getWidget G.castToLabel    "BalanceLabel"
    unconfirmedLabel     <- getWidget G.castToLabel    "UnconfirmedLabel"
    transactionsLabel    <- getWidget G.castToLabel    "TransactionsLabel"
    recentActivityView   <- getWidget G.castToTreeView "RecentActivityView"
    transactionsNumLabel <- getWidget G.castToLabel    "TransactionsNumLabel"
    transactionsView     <- getWidget G.castToTreeView "TransactionsView"
    statusLabel          <- getWidget G.castToLabel    "StatusLabel"
    notificationWindow   <- getWidget G.castToWindow   "NotificationWindow"
    messageLabel         <- getWidget G.castToLabel    "MessageLabel"
    transactionsList     <- G.listStoreNew []
    return OutputWidgets {..}
