-- | Module intializes and runs graphical user interface.

module GUI (runGUI) where

import           Control.Concurrent             (forkIO)
import           Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import           Control.Lens                   ((^.))
import           Control.Monad                  (forM, forM_, unless, void)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.STM              (atomically)
import           Data.Maybe                     (fromJust, isJust)
import           Data.Text                      (pack)

import           Graphics.UI.Gtk                (AttrOp ((:=)), on)
import qualified Graphics.UI.Gtk                as G

import           ActionsExecutor                as A
import           Contacts                       (Contact (..), address, name)
import           OutputWidgets                  as O
import           RSCoin.Core                    as C
import           RSCoin.User                    (commitError)

onExit :: TBQueue A.Action -> IO ()
onExit queue = atomically (writeTBQueue queue A.Exit)

onAddContact :: String -> String -> IO ()
onAddContact _ _ = return ()

onSend :: TBQueue A.Action -> String -> String -> IO ()
onSend queue sendAddress sendAmount = do
    let amount = read sendAmount
        pubKey = C.Address <$> (C.constructPublicKey $ pack sendAddress)
    unless (isJust pubKey) $ commitError $ pack "Invalid key."
    atomically $ writeTBQueue queue $
        A.Send [(1, amount)] (fromJust pubKey) $ C.Coin amount

initializeGUI :: TBQueue A.Action -> IO O.OutputWidgets
initializeGUI queue = do
    let file = "src/User/GUI.glade"

    builder <- G.builderNew
    G.builderAddFromFile builder file

    ow <- O.mkOutputWidgets builder

    let getWidget :: G.GObjectClass c => (G.GObject -> c) -> String -> IO c
        getWidget = G.builderGetObject builder

    window <- getWidget G.castToWindow "Window"
    void $ window `on` G.deleteEvent $ liftIO (onExit queue >> G.mainQuit)
        >> return False

    buttons <- forM tabsNames $ getWidget G.castToButton . flip (++) "Button"
    tabs    <- forM (tabsNames ++ ["Contacts"]) $ getWidget G.castToBox

    forM_ (zip buttons tabs) $ \(b, t) -> b `on` G.buttonActivated $ do
        mapM_ G.widgetHide tabs
        G.widgetShowAll t

    contactsButton <- getWidget G.castToButton    "ContactsButton"
    addContact     <- getWidget G.castToBox       "AddContact"
    selectContact  <- getWidget G.castToAlignment "SelectContact"
    void $ contactsButton `on` G.buttonActivated $ do
        mapM_ G.widgetHide tabs
        G.widgetShowAll $ last tabs
        G.widgetHide addContact
        G.widgetHide selectContact

    addContactButton   <- getWidget G.castToButton    "AddContactButton"
    contactsOperations <- getWidget G.castToAlignment "ContactsOperations"
    newNameEntry       <- getWidget G.castToEntry     "NewNameEntry"
    newAddressEntry    <- getWidget G.castToEntry     "NewAddressEntry"
    void $ addContactButton `on` G.buttonActivated $ do
        G.widgetHide contactsOperations
        G.widgetShowAll addContact
        G.entrySetText newNameEntry ""
        G.entrySetText newAddressEntry ""

    addButton    <- getWidget G.castToButton   "AddButton"
    contactsView <- getWidget G.castToTreeView "ContactsView"
    cl           <- G.listStoreNew []
    G.treeViewSetModel contactsView cl
    nameCol    <- G.treeViewColumnNew
    addressCol <- G.treeViewColumnNew
    renderer   <- G.cellRendererTextNew
    G.cellLayoutPackStart nameCol renderer False
    G.cellLayoutPackStart addressCol renderer False
    G.cellLayoutSetAttributes nameCol renderer cl $ \c ->
        [G.cellText := c ^. name]
    G.cellLayoutSetAttributes addressCol renderer cl $ \c ->
        [G.cellText := c ^. address]
    void $ G.treeViewAppendColumn contactsView nameCol
    void $ G.treeViewAppendColumn contactsView addressCol
    void $ addButton `on` G.buttonActivated $ do
        newName    <- G.entryGetText newNameEntry
        newAddress <- G.entryGetText newAddressEntry
        G.widgetShowAll contactsOperations
        G.widgetHide addContact
        onAddContact newName newAddress
        void $ G.listStoreAppend cl $ Contact (pack newName) (pack newAddress)
        return ()

    cancelButton <- getWidget G.castToButton "CancelButton"
    void $ cancelButton `on` G.buttonActivated $ do
        G.widgetShowAll contactsOperations
        G.widgetHide addContact

    selectButton <- getWidget G.castToButton "SelectButton"
    payToEntry   <- getWidget G.castToEntry  "PayToEntry"
    void $ selectButton `on` G.buttonActivated $ do
        mapM_ G.widgetHide tabs
        G.widgetShowAll $ tabs !! 1
        selection <- G.treeViewGetSelection contactsView
        rows <- G.treeSelectionGetSelectedRows selection
        if length rows > 0 then do
            c <- G.listStoreGetValue cl $ head $ head rows
            G.entrySetText payToEntry $ c ^. address
        else return ()

    selectAddressButton <- getWidget G.castToButton "SelectAddressButton"
    void $ selectAddressButton `on` G.buttonActivated $ do
        mapM_ G.widgetHide tabs
        G.widgetShowAll $ last tabs
        G.widgetHide contactsOperations
        G.widgetHide addContact

    performSendButton <- getWidget G.castToButton "PerformSendButton"
    amountEntry       <- getWidget G.castToEntry  "AmountEntry"
    void $ performSendButton `on` G.buttonActivated $ do
        sendAddress <- G.entryGetText payToEntry
        sendAmount  <- G.entryGetText amountEntry
        G.entrySetText payToEntry ""
        G.entrySetText amountEntry ""
        onSend queue sendAddress sendAmount

    G.widgetShowAll window
    mapM_ G.widgetHide tabs
    G.widgetShow $ head tabs

    return ow
  where
    tabsNames = ["Wallet", "Send", "Transactions", "Options"]
 

-- | Runs the GUI in a separate thread.
runGUI :: TBQueue A.Action -> IO O.OutputWidgets
runGUI queue = do
    void $ G.initGUI
    ow <- initializeGUI queue
    void $ forkIO G.mainGUI
    return ow
