module Controller where

import Control.Concurrent.STM.TBQueue
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM

import Data.Maybe
import Data.Text (pack)

import Graphics.UI.Gtk

import AcidExecutor

import RSCoin.Core
import RSCoin.User

import Contacts
import OutputWidgets
import Updater

onExit :: TBQueue Operation -> IO ()
onExit queue = atomically (writeTBQueue queue Exit) >> putStrLn "Exiting..."

onAddContact :: String -> String -> IO ()
onAddContact newName newAddress = do
    putStrLn $ "Adding contact with name = " ++ newName ++ ", address = " ++ newAddress

onSend :: TBQueue Operation -> String -> String -> IO ()
onSend queue sendAddress sendAmount = do
    putStrLn $ "Sending " ++ sendAmount ++ " to " ++ sendAddress
    let amount = read sendAmount
        pubKey = Address <$> (constructPublicKey $ pack sendAddress)
    unless (isJust pubKey) $ commitError $ pack "Invalid key."
    atomically $ writeTBQueue queue $ Send [(1, amount)] (fromJust pubKey) $ Coin amount

initializeController :: FilePath -> IO (OutputWidgets, TBQueue Operation)
initializeController file = do

    queue <- newTBQueueIO 10

    builder <- builderNew
    builderAddFromFile builder file

    ow <- mkOutputWidgets builder

    window <- builderGetObject builder castToWindow "Window"
    _ <- window `on` deleteEvent $ liftIO (onExit queue >> mainQuit) >> return False

    let tabsNames = ["Wallet", "Send", "Transactions", "Options"]
    buttons <- mapM (builderGetObject builder castToButton . flip (++) "Button") tabsNames
    tabs    <- mapM (builderGetObject builder castToBox) $ tabsNames ++ ["Contacts"]

    forM_ (zip buttons tabs) $ \(button, tab) -> button `on` buttonActivated $ do
        mapM_ widgetHide tabs
        widgetShowAll tab

    contactsButton <- builderGetObject builder castToButton "ContactsButton"
    addContact     <- builderGetObject builder castToBox "AddContact"
    selectContact  <- builderGetObject builder castToAlignment "SelectContact"
    _ <- contactsButton `on` buttonActivated $ do
        mapM_ widgetHide tabs
        widgetShowAll $ last tabs
        widgetHide addContact
        widgetHide selectContact

    addContactButton <- builderGetObject builder castToButton "AddContactButton"
    contactsOperations  <- builderGetObject builder castToAlignment "ContactsOperations"
    newNameEntry     <- builderGetObject builder castToEntry "NewNameEntry"
    newAddressEntry  <- builderGetObject builder castToEntry "NewAddressEntry"
    _ <- addContactButton `on` buttonActivated $ do
        widgetHide contactsOperations
        widgetShowAll addContact
        entrySetText newNameEntry ""
        entrySetText newAddressEntry ""

    addButton    <- builderGetObject builder castToButton "AddButton"
    contactsView <- builderGetObject builder castToTreeView "ContactsView"
    cl           <- listStoreNew []
    treeViewSetModel contactsView cl
    nameCol    <- treeViewColumnNew
    addressCol <- treeViewColumnNew
    renderer   <- cellRendererTextNew
    cellLayoutPackStart nameCol renderer False
    cellLayoutPackStart addressCol renderer False
    cellLayoutSetAttributes nameCol renderer cl $ \c -> [cellText := c ^. name]
    cellLayoutSetAttributes addressCol renderer cl $ \c -> [cellText := c ^. address]
    _ <- treeViewAppendColumn contactsView nameCol
    _ <- treeViewAppendColumn contactsView addressCol
    _ <- addButton `on` buttonActivated $ do
        newName    <- entryGetText newNameEntry
        newAddress <- entryGetText newAddressEntry
        widgetShowAll contactsOperations
        widgetHide addContact
        onAddContact newName newAddress
        _ <- listStoreAppend cl $ Contact newName newAddress
        return ()

    cancelButton <- builderGetObject builder castToButton "CancelButton"
    _ <- cancelButton `on` buttonActivated $ do
        widgetShowAll contactsOperations
        widgetHide addContact

    selectButton <- builderGetObject builder castToButton "SelectButton"
    payToEntry   <- builderGetObject builder castToEntry "PayToEntry"
    _ <- selectButton `on` buttonActivated $ do
        mapM_ widgetHide tabs
        widgetShowAll $ tabs !! 1
        selection <- treeViewGetSelection contactsView
        rows <- treeSelectionGetSelectedRows selection
        if length rows > 0 then do
            c <- listStoreGetValue cl $ head $ head rows
            entrySetText payToEntry $ c ^. address
        else return ()

    selectAddressButton <- builderGetObject builder castToButton "SelectAddressButton"
    _ <- selectAddressButton `on` buttonActivated $ do
        mapM_ widgetHide tabs
        widgetShowAll $ last tabs
        widgetHide contactsOperations
        widgetHide addContact

    performSendButton <- builderGetObject builder castToButton "PerformSendButton"
    amountEntry <- builderGetObject builder castToEntry "AmountEntry"
    _ <- performSendButton `on` buttonActivated $ do
        sendAddress <- entryGetText payToEntry
        sendAmount  <- entryGetText amountEntry
        entrySetText payToEntry ""
        entrySetText amountEntry ""
        onSend queue sendAddress sendAmount

    widgetShowAll window
    mapM_ widgetHide tabs
    widgetShow $ head tabs

    return (ow, queue)
