{-# LANGUAGE RecordWildCards #-}

module OutputWidgets where

import Graphics.UI.Gtk

data OutputWidgets = OutputWidgets {
    balanceLabel         :: Label,
    unconfirmedLabel     :: Label,
    transactionsLabel    :: Label,
    recentActivityView   :: TreeView,
    myWalletLabel        :: Label,
    transactionsNumLabel :: Label,
    transactionsView     :: TreeView,
    statusLabel          :: Label
}

mkOutputWidgets :: Builder -> IO OutputWidgets
mkOutputWidgets builder = do
    balanceLabel         <- builderGetObject builder castToLabel "BalanceLabel"
    unconfirmedLabel     <- builderGetObject builder castToLabel "UnconfirmedLabel"
    transactionsLabel    <- builderGetObject builder castToLabel "TransactionsLabel"
    recentActivityView   <- builderGetObject builder castToTreeView "RecentActivityView"
    myWalletLabel        <- builderGetObject builder castToLabel "MyWalletLabel"
    transactionsNumLabel <- builderGetObject builder castToLabel "TransactionsNumLabel"
    transactionsView     <- builderGetObject builder castToTreeView "TransactionsView"
    statusLabel          <- builderGetObject builder castToLabel "StatusLabel"
    return OutputWidgets {..}
