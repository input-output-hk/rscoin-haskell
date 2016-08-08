{-# LANGUAGE OverloadedStrings #-}

module GUI.RSCoin.Glade
       ( GladeMainWindow (..)
       , AddContactWindow (..)
       , importGlade
       ) where

import           GUI.RSCoin.MainWindow (AddContactWindow (..))
import qualified RSCoin.Core           as C

import qualified Graphics.UI.Gtk       as G
import           Paths_rscoin          (getDataFileName)

data GladeMainWindow = GladeMainWindow
    { gWindow                  :: G.Window
    , gNotebookMain            :: G.Notebook

    , gProgressBarUpdate       :: G.ProgressBar
    , gLabelSync               :: G.Label

    , gTreeViewWallet          :: G.TreeView
    , gBoxRSCoinLogo           :: G.Box
    , gBoxWalletHeaderWrapper  :: G.Box
    , gBoxWalletHeader         :: G.Box
    , gLabelCurrentBalance     :: G.Label
    , gLabelUnconfirmedBalance :: G.Label
    , gLabelTransactionsNumber :: G.Label
    , gLabelCurrentAccount     :: G.Label

    , gEntryPayTo              :: G.Entry
    , gButtonChooseContacts    :: G.Button
    , gSpinButtonSendAmount    :: G.SpinButton
    , gButtonConfirmSend       :: G.Button
    , gButtonClearSend         :: G.Button

    , gTreeViewContactsView    :: G.TreeView
    , gButtonAddContact        :: G.Button
    , gButtonRemoveContact     :: G.Button
    , gLabelContactsNum        :: G.Label

    , gTreeViewAddressesView   :: G.TreeView
    , gButtonGenerateAddress   :: G.Button
    , gButtonCopyAddress       :: G.Button
    }

makeBuilder :: FilePath -> IO G.Builder
makeBuilder path =
  do C.logDebug "Initializing glade builder"
     builder <- G.builderNew
     G.builderAddFromFile builder path
     return builder

importGlade :: IO (GladeMainWindow, AddContactWindow)
importGlade = do
    C.logDebug "Loading Glade layout"
    uiPath <- getDataFileName "resources/RSCoinMain.glade"
    builder <- makeBuilder uiPath
    let getWidget :: G.GObjectClass c => (G.GObject -> c) -> String -> IO c
        getWidget      = G.builderGetObject builder
        getWindow      = getWidget G.castToWindow
        getNotebook    = getWidget G.castToNotebook
        getLabel       = getWidget G.castToLabel
        getEntry       = getWidget G.castToEntry
        getButton      = getWidget G.castToButton
        getSpinButton  = getWidget G.castToSpinButton
        getBox         = getWidget G.castToBox
        getProgressBar = getWidget G.castToProgressBar
        getView        = getWidget G.castToTreeView
    C.logDebug "Getting widgets out of GTK"
    gmw <- GladeMainWindow
        <$> getWindow      "mainWindow"
        <*> getNotebook    "mainNotebook"
        <*> getProgressBar "updateProgressBar"
        <*> getLabel       "syncLabel"
        <*> getView        "walletTreeView"
        <*> getBox         "rscoinLogo"
        <*> getBox         "walletHeaderWrapper"
        <*> getBox         "walletHeaderBox"
        <*> getLabel       "currentBalanceLabel"
        <*> getLabel       "unconfirmedBalanceLabel"
        <*> getLabel       "transactionsNumberLabel"
        <*> getLabel       "currentAccountLabel"
        <*> getEntry       "payToEntry"
        <*> getButton      "chooseContactsButton"
        <*> getSpinButton  "sendAmountSpinButton"
        <*> getButton      "confirmSendButton"
        <*> getButton      "clearSendButton"
        <*> getView        "contactsView"
        <*> getButton      "addContactButton"
        <*> getButton      "removeContactButton"
        <*> getLabel       "contactsNumLabel"
        <*> getView        "addressesView"
        <*> getButton      "generateAddressButton"
        <*> getButton      "copyAddressButton"
    acw <- AddContactWindow
        <$> getWindow "addContactWindow"
        <*> getEntry  "nameEntry"
        <*> getEntry  "addressEntry"
        <*> getButton "okButton"
        <*> getButton "cancelButton"
    return (gmw, acw)
