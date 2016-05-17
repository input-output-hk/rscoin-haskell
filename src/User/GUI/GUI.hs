-- | Module intializes and runs graphical user interface.

module GUI.GUI (runGUI) where

import           Control.Concurrent             (forkIO)
import           Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import           Control.Lens                   ((^.))
import           Control.Monad                  (forM, forM_, void, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.STM              (atomically)
import           Data.Acid                      (query, update)
import           Data.Maybe                     (fromJust, isJust)
import           Data.Text                      (pack)

import           Graphics.UI.Gtk                (AttrOp ((:=)), on)
import qualified Graphics.UI.Gtk                as G
import           Serokell.Util.Text             (readUnsignedDecimal)

import           Paths_rscoin                   (getDataFileName)
import qualified RSCoin.Core                    as C
import qualified RSCoin.User.AcidState          as T
import           RSCoin.User.Wallet             (UserAddress, publicAddress)

import qualified GUI.Action                     as A
import qualified GUI.Contacts                   as S
import qualified GUI.OutputWidgets              as O
import           GUI.Transactions               (VerboseTransaction,
                                                 getTransactionAmount,
                                                 showTransaction)

onExit :: TBQueue A.Action -> IO ()
onExit queue = atomically (writeTBQueue queue A.Exit)

onSend :: TBQueue A.Action -> O.OutputWidgets -> String -> String -> IO ()
onSend queue ow sendAddress sendAmount = do
    let amount = readUnsignedDecimal $ pack sendAmount
        pubKey = C.Address <$> (C.constructPublicKey $ pack sendAddress)
    case amount of
        Left _ -> G.postGUIAsync $ do
            G.labelSetText (O.messageLabel ow) "Bad number format."
            G.widgetShowAll (O.notificationWindow ow)
        Right a -> if isJust pubKey
            then atomically $ writeTBQueue queue $ A.Send (fromJust pubKey) a
            else G.postGUIAsync $ do
                G.labelSetText (O.messageLabel  ow) "Bad key."
                G.widgetShowAll (O.notificationWindow ow)

setupTransactionsView
    :: [UserAddress] -> G.TreeView -> G.ListStore VerboseTransaction -> IO ()
setupTransactionsView a t l = do
    G.treeViewSetModel t l
    amountCol  <- G.treeViewColumnNew
    detailsCol <- G.treeViewColumnNew
    tRenderer  <- G.cellRendererTextNew
    G.cellLayoutPackStart amountCol tRenderer False
    G.cellLayoutPackStart detailsCol tRenderer False
    G.cellLayoutSetAttributes amountCol tRenderer l $ \r ->
        [G.cellText := show (getTransactionAmount a r)]
    G.cellLayoutSetAttributes detailsCol tRenderer l $ \r ->
        [G.cellText := showTransaction r]
    void $ G.treeViewAppendColumn t amountCol
    void $ G.treeViewAppendColumn t detailsCol

initializeGUI :: TBQueue A.Action ->
    T.RSCoinUserState -> S.ContactsState -> IO O.OutputWidgets
initializeGUI queue st cs = do
    layoutFile <- getDataFileName "src/User/GUI/GUI.glade"

    builder <- G.builderNew
    G.builderAddFromFile builder layoutFile

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

    addressesView <- getWidget G.castToTreeView "AddressesView"
    addresses     <- query st T.GetAllAddresses
    addressesList <- G.listStoreNew addresses
    G.treeViewSetModel addressesView addressesList
    addressesCol <- G.treeViewColumnNew
    aRenderer    <- G.cellRendererTextNew
    G.cellLayoutPackStart addressesCol aRenderer False
    G.cellLayoutSetAttributes addressesCol aRenderer addressesList $ \a ->
        [G.cellText := C.printPublicKey (a ^. publicAddress)]
    void $ G.treeViewAppendColumn addressesView addressesCol

    transactionsView  <- getWidget G.castToTreeView "TransactionsView"
    setupTransactionsView addresses transactionsView (O.transactionsList ow)

    recentActivityView <- getWidget G.castToTreeView "RecentActivityView"
    setupTransactionsView addresses recentActivityView (O.transactionsList ow)

    addButton    <- getWidget G.castToButton   "AddButton"
    contactsView <- getWidget G.castToTreeView "ContactsView"
    cls          <- query cs $ S.GetContacts
    cl           <- G.listStoreNew cls
    G.treeViewSetModel contactsView cl
    nameCol    <- G.treeViewColumnNew
    addressCol <- G.treeViewColumnNew
    renderer   <- G.cellRendererTextNew
    G.cellLayoutPackStart nameCol renderer False
    G.cellLayoutPackStart addressCol renderer False
    G.cellLayoutSetAttributes nameCol renderer cl $ \c ->
        [G.cellText := (S.name c)]
    G.cellLayoutSetAttributes addressCol renderer cl $ \c ->
        [G.cellText := (S.address c)]
    void $ G.treeViewAppendColumn contactsView nameCol
    void $ G.treeViewAppendColumn contactsView addressCol
    void $ addButton `on` G.buttonActivated $ do
        newName    <- pack <$> G.entryGetText newNameEntry
        newAddress <- pack <$> G.entryGetText newAddressEntry
        G.widgetShowAll contactsOperations
        G.widgetHide addContact
        void $ G.listStorePrepend cl $ S.Contact newName newAddress
        update cs $ S.AddContact $ S.Contact newName newAddress

    cancelButton <- getWidget G.castToButton "CancelButton"
    void $ cancelButton `on` G.buttonActivated $ do
        G.widgetShowAll contactsOperations
        G.widgetHide addContact

    selectButton <- getWidget G.castToButton "SelectButton"
    payToEntry   <- getWidget G.castToEntry  "PayToEntry"
    void $ selectButton `on` G.buttonActivated $ do
        mapM_ G.widgetHide tabs
        G.widgetShowAll $ tabs !! 2
        selection <- G.treeViewGetSelection contactsView
        rows <- G.treeSelectionGetSelectedRows selection
        when (length rows > 0) $ do
            c <- G.listStoreGetValue cl $ head $ head rows
            G.entrySetText payToEntry $ S.address c

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
        onSend queue ow sendAddress sendAmount

    okButton <- getWidget G.castToButton "OKButton"
    void $ okButton `on` G.buttonActivated
        $ G.widgetHide $ O.notificationWindow ow

    G.widgetShowAll window
    mapM_ G.widgetHide tabs
    G.widgetShow $ head tabs

    return ow
  where
    tabsNames = ["Wallet", "Addresses", "Send", "Transactions", "Options"]


-- | Runs the GUI in a separate thread.
runGUI :: TBQueue A.Action ->
    T.RSCoinUserState -> S.ContactsState -> IO O.OutputWidgets
runGUI queue st cs = do
    void $ G.initGUI
    ow <- initializeGUI queue st cs
    void $ forkIO G.mainGUI
    return ow
