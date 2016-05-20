{-# LANGUAGE OverloadedStrings #-}

module GUI.RSCoin.Glade
       ( MainWindow (..)
       , importGlade
       ) where

import qualified RSCoin.Core     as C

import qualified Graphics.UI.Gtk as G
import           Paths_rscoin    (getDataFileName)

data MainWindow = MainWindow
    { window                  :: G.Window
    , notebookMain            :: G.Notebook

    , progressBarUpdate       :: G.ProgressBar
    , treeViewWallet          :: G.TreeView
    , labelCurrentBalance     :: G.Label
    , labelUnconfirmedBalance :: G.Label
    , labelTransactionsNumber :: G.Label
    , labelCurrentAccount     :: G.Label
    }

makeBuilder :: FilePath -> IO G.Builder
makeBuilder path =
  do C.logDebug C.userLoggerName "Initializing glade builder"
     builder <- G.builderNew
     G.builderAddFromFile builder path
     return builder

importGlade :: IO MainWindow
importGlade = do
    C.logDebug C.userLoggerName "Loading Glade layout"
    uiPath <- getDataFileName "resources/RSCoinMain.glade"
    builder <- makeBuilder uiPath
    let getWidget :: G.GObjectClass c => (G.GObject -> c) -> String -> IO c
        getWidget      = G.builderGetObject builder
        getWindow      = getWidget G.castToWindow
        getNotebook    = getWidget G.castToNotebook
        getLabel       = getWidget G.castToLabel
        getProgressBar = getWidget G.castToProgressBar
        getButton      = getWidget G.castToButton
        getView        = getWidget G.castToTreeView
    C.logDebug C.userLoggerName "Getting widgets out of GTK"
    MainWindow
        <$> getWindow          "mainWindow"
        <*> getNotebook        "mainNotebook"
        <*> getProgressBar     "updateProgressBar"
        <*> getView            "walletTreeView"
        <*> getLabel           "currentBalanceLabel"
        <*> getLabel           "unconfirmedBalanceLabel"
        <*> getLabel           "transactionsNumberLabel"
        <*> getLabel           "currentAccountLabel"
