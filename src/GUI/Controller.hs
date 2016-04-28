module Controller where

import Control.Monad
import Control.Monad.IO.Class

import Data.IORef

import Graphics.UI.Gtk

data ProgrammState = ProgrammState

onExit :: IORef ProgrammState -> IO ()
onExit psr = putStrLn "Exiting."

onAddContact :: IORef ProgrammState -> String -> String -> IO ()
onAddContact psr name address =
    putStrLn $ "Adding contact with name = " ++ name ++ ", address = " ++ address

onSend :: IORef ProgrammState -> String -> String -> IO ()
onSend psr address amount = putStrLn $ "Sending " ++ amount ++ " to " ++ address

showState :: IORef ProgrammState -> IO ()
showState psr = return ()

initializeController :: FilePath -> ProgrammState -> IO ()
initializeController file state = do

    psr <- newIORef state

    builder <- builderNew
    builderAddFromFile builder file

    window <- builderGetObject builder castToWindow "Window"
    _ <- window `on` deleteEvent $ liftIO (onExit psr >> mainQuit) >> return False

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

    addButton <- builderGetObject builder castToButton "AddButton"
    _ <- addButton `on` buttonActivated $ do
        name    <- entryGetText newNameEntry
        address <- entryGetText newAddressEntry
        widgetShowAll contactsOperations
        widgetHide addContact
        onAddContact psr name address
        showState psr

    cancelButton <- builderGetObject builder castToButton "CancelButton"
    _ <- cancelButton `on` buttonActivated $ do
        widgetShowAll contactsOperations
        widgetHide addContact

    selectButton <- builderGetObject builder castToButton "SelectButton"
    payToEntry   <- builderGetObject builder castToEntry "PayToEntry"
    _ <- selectButton `on` buttonActivated $ do
        mapM_ widgetHide tabs
        widgetShowAll $ tabs !! 1
        entrySetText payToEntry "selected"

    selectAddressButton <- builderGetObject builder castToButton "SelectAddressButton"
    _ <- selectAddressButton `on` buttonActivated $ do
        mapM_ widgetHide tabs
        widgetShowAll $ last tabs
        widgetHide contactsOperations
        widgetHide addContact

    performSendButton <- builderGetObject builder castToButton "PerformSendButton"
    amountEntry <- builderGetObject builder castToEntry "AmountEntry"
    _ <- performSendButton `on` buttonActivated $ do
        address <- entryGetText payToEntry
        amount  <- entryGetText amountEntry
        entrySetText payToEntry ""
        entrySetText amountEntry ""
        onSend psr address amount

    widgetShowAll window
    mapM_ widgetHide tabs
    widgetShow $ head tabs
