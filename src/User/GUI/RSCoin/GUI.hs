{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE RankNTypes          #-}

-- | This module describes main GUI bindings
module GUI.RSCoin.GUI (startGUI, red, green) where

import           Control.Lens               ((^.))
import           Control.Monad              (replicateM_, void)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Acid                  (update, query)
import           Data.Maybe                 (fromJust)
import qualified Data.Text                  as T
import           System.FilePath            (takeBaseName)

import           Graphics.UI.Gtk            (AttrOp ((:=)), on)
import qualified Graphics.UI.Gtk            as G
import           Paths_rscoin               (getDataFileName)

import           GUI.RSCoin.Glade           (AddContactWindow (..)
                                            , GladeMainWindow (..), importGlade)
import qualified GUI.RSCoin.Contacts        as S
import           GUI.RSCoin.MainWindow      (ContactsTab (..), MainWindow (..))
import qualified GUI.RSCoin.MainWindow      as M
import           GUI.RSCoin.TransactionsTab (initTransactionsTab)
import           GUI.RSCoin.WalletTab       (initWalletTab)

green, red:: G.Color
green = G.Color 0 65535 0
red = G.Color 51199 8960 8960

-- ICONS --
loadIcons :: IO ()
loadIcons = mapM_ loadIcon iconList
  where
    iconList = [ "resources/icons/wallet.png"
               , "resources/icons/people.png"
               , "resources/icons/send.png"
               , "resources/icons/options.png"
               ]
    loadIcon path = do
        icon <- G.iconSourceNew
        getDataFileName path >>= G.iconSourceSetFilename icon
        icons <- G.iconSetNew
        G.iconSetAddSource icons icon
        iconf <- G.iconFactoryNew
        G.iconFactoryAdd iconf (T.toLower . T.pack $ takeBaseName path) icons
        G.iconFactoryAddDefault iconf

notebookGetAllPages :: G.NotebookClass self => self -> IO [G.Widget]
notebookGetAllPages nb = do
    npages <- G.notebookGetNPages nb
    mapM (fmap fromJust . G.notebookGetNthPage nb) [0..npages - 1]

notebookGetAllTabLabelText
    :: G.NotebookClass self
    => self
    -> IO [Maybe T.Text]
notebookGetAllTabLabelText nb = notebookGetAllPages nb >>= mapM (G.notebookGetTabLabelText nb)

notebookRemoveAllPages
    :: G.NotebookClass self
    => self
    -> IO ()
notebookRemoveAllPages nb = do
    npages <- G.notebookGetNPages nb
    replicateM_ npages $ G.notebookRemovePage nb 0

setNotebookIcons :: G.NotebookClass self => self -> G.IconSize -> IO ()
setNotebookIcons nb size = do
    pages <- zip <$> notebookGetAllPages nb <*> notebookGetAllTabLabelText nb
    notebookRemoveAllPages nb
    mapM_ addIconPage pages
  where
    addIconPage (widget, Nothing) = do
        noLabel <- G.labelNew $ Just "no label"
        G.notebookAppendPageMenu nb widget noLabel noLabel
    addIconPage (widget, Just name) = do
        -- TODO: if there is no icon in stock, empty image will be rendered
        -- we should put "no icon" label in that case
        image <- G.imageNewFromStock (T.toLower name) size
        G.notebookAppendPageMenu nb widget image image

-- ICONS --

initContacts :: S.ContactsState -> GladeMainWindow -> AddContactWindow -> IO ()
initContacts cs GladeMainWindow {..} AddContactWindow {..} = do
    cls <- query cs S.GetContacts
    G.labelSetText gLabelContactsNum $ "Contacts: " ++ show (length cls)
    cl <- G.listStoreNew cls
    G.treeViewSetModel gTreeViewContactsView cl
    nameCol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle nameCol "Name"
    addressCol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle addressCol "Address"
    renderer <-G.cellRendererTextNew
    G.cellLayoutPackStart nameCol renderer False
    G.cellLayoutPackStart addressCol renderer False
    G.cellLayoutSetAttributes nameCol renderer cl $ \c ->
        [G.cellText := (S.name c)]
    G.cellLayoutSetAttributes addressCol renderer cl $ \c ->
        [G.cellText := (S.address c)]
    void $ G.treeViewAppendColumn gTreeViewContactsView nameCol
    void $ G.treeViewAppendColumn gTreeViewContactsView addressCol
    void $ gButtonAddContact `on` G.buttonActivated $ do
        G.widgetShowAll addContactWindow
        G.entrySetText nameEntry ""
        G.entrySetText addressEntry ""
    void $ okButton `on` G.buttonActivated $ do
        G.widgetHide addContactWindow
        name <- G.entryGetText nameEntry
        address <- G.entryGetText addressEntry
        void $ G.listStorePrepend cl $ S.Contact name address
        update cs $ S.AddContact $ S.Contact name address
        cls' <- query cs S.GetContacts
        G.labelSetText gLabelContactsNum $ "Contacts: " ++ show (length cls')
    void $ cancelButton `on` G.buttonActivated $ do
        G.widgetHide addContactWindow

startGUI :: S.ContactsState -> IO ()
startGUI cs = do
    void G.initGUI
    mw <- initMainWindow cs
    void ((mw ^. M.mainWindow) `on` G.deleteEvent $ liftIO G.mainQuit >> return False)
    G.widgetShowAll $ mw ^. M.mainWindow
    G.mainGUI

initMainWindow :: S.ContactsState -> IO MainWindow
initMainWindow cs = do
    (gmw@GladeMainWindow{..}, acw) <- importGlade
    initContacts cs gmw acw
    _tabWallet <- initWalletTab gmw
    _tabTransactions <- initTransactionsTab gmw
    let _tabContacts =
            ContactsTab
            gTreeViewContactsView
            gButtonAddContact
            gLabelContactsNum
        mw = M.MainWindow
            { _mainWindow = gWindow
            , _notebookMain = gNotebookMain
            , _progressBarUpdate = gProgressBarUpdate
            , .. }
    loadIcons
    setNotebookIcons (mw ^. M.notebookMain) G.IconSizeSmallToolbar
    return mw
