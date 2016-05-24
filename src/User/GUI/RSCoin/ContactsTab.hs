{-# LANGUAGE NoOverloadedStrings #-}

-- | Contacts tab module

module GUI.RSCoin.ContactsTab
       ( createContactsTab
       , initContactsTab
       ) where

import           Control.Monad         (void)
import           Graphics.UI.Gtk       (AttrOp ((:=)), on)
import qualified Graphics.UI.Gtk       as G

import           GUI.RSCoin.Glade      (GladeMainWindow (..))
import           GUI.RSCoin.GUIAcid    (Contact (..), GUIState, addContact,
                                        getContacts)
import           GUI.RSCoin.MainWindow (AddContactWindow (..), ContactsTab (..))
import qualified GUI.RSCoin.MainWindow as M

createContactsTab :: GladeMainWindow -> IO ContactsTab
createContactsTab GladeMainWindow{..} =
    return $
    ContactsTab gTreeViewContactsView gButtonAddContact gLabelContactsNum

initContactsTab :: GUIState -> M.MainWindow -> AddContactWindow -> IO ()
initContactsTab gst M.MainWindow{..} AddContactWindow{..} = do
    let ContactsTab{..} = tabContacts
    contacts <- getContacts gst
    G.labelSetText labelContactsNum $
        "Contacts: " ++ show (length contacts)
    model <- G.listStoreNew contacts
    G.treeViewSetModel treeViewContactsView model
    -- adding columns
    nameCol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle nameCol "Name"
    addressCol <- G.treeViewColumnNew
    G.treeViewColumnSetTitle addressCol "Address"
    renderer <- G.cellRendererTextNew
    G.cellLayoutPackStart nameCol renderer False
    G.cellLayoutPackStart addressCol renderer False
    G.cellLayoutSetAttributes nameCol renderer model $ \c ->
        [G.cellText := contactName c]
    G.cellLayoutSetAttributes addressCol renderer model $ \c ->
        [G.cellText := contactAddress c]
    void $ G.treeViewAppendColumn treeViewContactsView nameCol
    void $ G.treeViewAppendColumn treeViewContactsView addressCol
    -- buttons feedback
    void $ buttonAddContact `on` G.buttonActivated $ do
        G.entrySetText entryContactName ""
        G.entrySetText entryContactAddress ""
        G.widgetShowAll addContactWindow
    void $ buttonContactOk `on` G.buttonActivated $ do
        G.widgetHide addContactWindow
        name <- G.entryGetText entryContactName
        address <- G.entryGetText entryContactAddress
        void $ G.listStorePrepend model $ Contact name address
        addContact gst $ Contact name address
        contacts' <- getContacts gst
        G.labelSetText labelContactsNum $ "Contacts: " ++ show (length contacts')
    void $ buttonContactCancel `on` G.buttonActivated $
        G.widgetHide addContactWindow
