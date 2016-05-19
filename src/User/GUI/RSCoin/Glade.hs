module GUI.RSCoin.Glade
       ( MainWindow (..)
       , importGlade
       ) where

import qualified RSCoin.Core     as C

import           Graphics.UI.Gtk (AttrOp ((:=)), on)
import qualified Graphics.UI.Gtk as G
import           Paths_rscoin    (getDataFileName)

data GladeMainWindow = GladeMainWindow
    { window                  :: G.Window
    , notebookMain            :: G.Notebook

    , progressBarUpdate       :: G.ProgressBar
    , treeViewWallet          :: G.TreeView
    , labelCurrentBalance     :: G.Label
    , labelUnconfirmedBalance :: G.Label
    , labelTransactionsNumber :: G.Label
    , labelCurrentAccount     :: G.Label
    }

importGlade :: IO GladeMainWindow
importGlade = do
    C.logDebug C.userLoggerName "Loading Glade layout"
    uiPath <- getDataFileName "resources/RSCoinMain.glade"
    builder <- G.makeBuilder uiPath
    let getWindow      = G.builderGetObject builder G.castToWindow
        getNotebook    = G.builderGetObject builder G.castToNotebook
        getLabel       = G.builderGetObject builder G.castToLabel
        getProgressBar = G.builderGetObject builder G.castToProgressBar
        getButton      = G.builderGetObject builder G.castToButton
        getView        = G.builderGetObject builder G.castToTreeView
    GladeMainWindow
        <$> getWindow          "mainWindow"
        <*> getNotebook        "mainNotebook"
        <*> getProgressBar     "updateProgressBar"
        <*> getView            "walletTreeView"
        <*> getLabel           "currentBalanceLabel"
        <*> getLabel           "unconfirmedBalanceLabel"
        <*> getLabel           "trasactionsNumberLabel"
        <*> getLabel           "currentAccountLabel"
