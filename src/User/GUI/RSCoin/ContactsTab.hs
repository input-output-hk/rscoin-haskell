{-# LANGUAGE NoOverloadedStrings #-}

-- | Contacts tab module

module GUI.RSCoin.ContactsTab
       ( createContactsTab
       , initContactsTab
       ) where

import           Control.Monad           (void, when)
import           Data.Maybe              (isJust)
import           Graphics.UI.Gtk         (AttrOp ((:=)), on)
import qualified Graphics.UI.Gtk         as G

import           GUI.RSCoin.ErrorMessage (reportSimpleError)
import           GUI.RSCoin.Glade        (GladeMainWindow (..))
import           GUI.RSCoin.GUIAcid      (Contact (..), GUIState, addContact,
                                          removeContact, getContacts)
import           GUI.RSCoin.MainWindow   (AddContactWindow (..),
                                          ContactsTab (..))
import qualified GUI.RSCoin.MainWindow   as M
import qualified RSCoin.Core             as C

createContactsTab :: GladeMainWindow -> IO ContactsTab
createContactsTab GladeMainWindow{..} =
    return $
    ContactsTab gTreeViewContactsView gButtonAddContact
        gButtonRemoveContact gLabelContactsNum

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
        name <- G.entryGetText entryContactName
        address <- G.entryGetText entryContactAddress
        let ma = C.Address <$> C.constructPublicKey address
        if isJust ma then do
            void $ G.listStorePrepend model $ Contact name address
            addContact gst $ Contact name address
            contacts' <- getContacts gst
            G.labelSetText labelContactsNum $ "Contacts: " ++ show (length contacts')
            G.widgetHide addContactWindow
        else reportSimpleError mainWindow "Bad address format!"
    void $ buttonContactCancel `on` G.buttonActivated $
        G.widgetHide addContactWindow
    void $ buttonRemoveContact `on` G.buttonActivated $ do
        sel <- G.treeViewGetSelection treeViewContactsView
        selNum <- G.treeSelectionCountSelectedRows sel
        rows <- G.treeSelectionGetSelectedRows sel
        when (selNum /= 0) $ do
            dialog <- G.messageDialogNew
                (Just mainWindow)
                [G.DialogDestroyWithParent]
                G.MessageQuestion
                G.ButtonsOkCancel
                "Do you really want to remove the contact?"
            response <- G.dialogRun dialog
            when (response == G.ResponseOk) $ do
                let i = head $ head rows
                removeContact gst i
                G.listStoreRemove model i
                contacts' <- getContacts gst
                G.labelSetText labelContactsNum $
                    "Contacts: " ++ show (length contacts')
            G.widgetDestroy dialog
