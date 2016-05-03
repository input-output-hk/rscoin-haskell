{-# LANGUAGE RecordWildCards #-}

module Controller where

import Control.Lens

import Control.Monad
import Control.Monad.IO.Class

import Data.IORef

import Graphics.UI.Gtk

import Wallet

data OutputWidgets = OutputWidgets {
    balanceLabel         :: Label,
    unconfirmedLabel     :: Label,
    transactionsLabel    :: Label,
    recentActivityView   :: TreeView,
    myWalletLabel        :: Label,
    transactionsNumLabel :: Label,
    transactionsView     :: TreeView,
    statusLabel          :: Label
}

mkOutputWidgets :: Builder -> IO OutputWidgets
mkOutputWidgets builder = do
    balanceLabel         <- builderGetObject builder castToLabel "BalanceLabel"
    unconfirmedLabel     <- builderGetObject builder castToLabel "UnconfirmedLabel"
    transactionsLabel    <- builderGetObject builder castToLabel "TransactionsLabel"
    recentActivityView   <- builderGetObject builder castToTreeView "RecentActivityView"
    myWalletLabel        <- builderGetObject builder castToLabel "MyWalletLabel"
    transactionsNumLabel <- builderGetObject builder castToLabel "TransactionsNumLabel"
    transactionsView     <- builderGetObject builder castToTreeView "TransactionsView"
    statusLabel          <- builderGetObject builder castToLabel "StatusLabel"
    return OutputWidgets {..}

onExit :: IORef Wallet -> IO ()
onExit _ = putStrLn "Exiting."

onAddContact :: IORef Wallet -> String -> String -> IO ()
onAddContact wr newName newAddress = do
    putStrLn $ "Adding contact with name = " ++ newName ++ ", address = " ++ newAddress
    modifyIORef wr $ contacts %~ ((:) $ Contact newName newAddress)

onSend :: IORef Wallet -> String -> String -> IO ()
onSend wr sendAddress sendAmount = do
    putStrLn $ "Sending " ++ sendAmount ++ " to " ++ sendAddress
    modifyIORef wr $ transactions %~ ((:) $
        Transaction False (Contact "" sendAddress) (read sendAmount))

showWallet :: OutputWidgets -> IORef Wallet -> IO ()
showWallet OutputWidgets {..} wr = do
    wallet <- readIORef wr
    labelSetText balanceLabel $ show (wallet ^. balance)
    labelSetText unconfirmedLabel $ show (wallet ^. unconfirmed)
    let ts = "Transactions: " ++ show (length $ wallet ^. transactions)
    labelSetText transactionsLabel ts
    labelSetText transactionsNumLabel ts
    labelSetText myWalletLabel $ "MY RSCOIN WALLET: " ++ (wallet ^. myAddress)
    labelSetText statusLabel "Synchronized"

initializeController :: FilePath -> Wallet -> IO ()
initializeController file wallet = do

    wr <- newIORef wallet

    builder <- builderNew
    builderAddFromFile builder file

    ow <- mkOutputWidgets builder

    window <- builderGetObject builder castToWindow "Window"
    _ <- window `on` deleteEvent $ liftIO (onExit wr >> mainQuit) >> return False

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
    cl           <- listStoreNew $ wallet ^. contacts
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
        onAddContact wr newName newAddress
        _ <- listStoreAppend cl $ Contact newName newAddress
        showWallet ow wr

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
        onSend wr sendAddress sendAmount
        showWallet ow wr

    widgetShowAll window
    mapM_ widgetHide tabs
    widgetShow $ head tabs

    showWallet ow wr
