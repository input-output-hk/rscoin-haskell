{-# LANGUAGE RecordWildCards #-}

-- | Data structore to store output GUI widgets.

module OutputWidgets
    ( OutputWidgets (..)
    , mkOutputWidgets
    ) where

import qualified Graphics.UI.Gtk as G

data OutputWidgets = OutputWidgets
    { balanceLabel         :: G.Label
    , unconfirmedLabel     :: G.Label
    , transactionsLabel    :: G.Label
    , recentActivityView   :: G.TreeView
    , myWalletLabel        :: G.Label
    , transactionsNumLabel :: G.Label
    , transactionsView     :: G.TreeView
    , statusLabel          :: G.Label
    }

mkOutputWidgets :: G.Builder -> IO OutputWidgets
mkOutputWidgets builder = do
    let getWidget :: G.GObjectClass c => (G.GObject -> c) -> String -> IO c
        getWidget = G.builderGetObject builder

    balanceLabel         <- getWidget G.castToLabel    "BalanceLabel"
    unconfirmedLabel     <- getWidget G.castToLabel    "UnconfirmedLabel"
    transactionsLabel    <- getWidget G.castToLabel    "TransactionsLabel"
    recentActivityView   <- getWidget G.castToTreeView "RecentActivityView"
    myWalletLabel        <- getWidget G.castToLabel    "MyWalletLabel"
    transactionsNumLabel <- getWidget G.castToLabel    "TransactionsNumLabel"
    transactionsView     <- getWidget G.castToTreeView "TransactionsView"
    statusLabel          <- getWidget G.castToLabel    "StatusLabel"
    return OutputWidgets {..}
