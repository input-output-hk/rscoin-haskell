{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- | Database that holds all stuff needed for GUI

module GUI.RSCoin.GUIAcid
    ( Contact (..)
    , GUIState
    , dummyContacts
    , emptyGUIAcid
    , getContacts
    , addContact
    , removeContact
    ) where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)
import           Data.Acid            (AcidState, Query, Update, makeAcidic,
                                       query, update)
import           Data.SafeCopy        (base, deriveSafeCopy)
import           Data.Text            (Text)

-- | Contact information.
data Contact = Contact
    { contactName    :: Text
    , contactAddress :: Text
    } deriving (Show)

dummyContacts :: [Contact]
dummyContacts =
    [Contact "aoeu" "kek", Contact "kek" "ahaha", Contact "memasy" "podkatili"]

-- | List of contacts known to the user.
data ContactsList = ContactsList { list :: [Contact] }

emptyGUIAcid :: ContactsList
emptyGUIAcid = ContactsList []

$(deriveSafeCopy 0 'base ''Contact)
$(deriveSafeCopy 0 'base ''ContactsList)

-- | Adds a contact to the list.
addContact' :: Contact -> Update ContactsList ()
addContact' c = do
    ContactsList l <- get
    put $ ContactsList $ c : l

-- | Removes a contact from the list.
removeContact' :: Int -> Update ContactsList ()
removeContact' i = do
    ContactsList l <- get
    put $ ContactsList $ take i l ++ drop (i + 1) l

-- | Gets the list from the database.
getContacts' :: Query ContactsList [Contact]
getContacts' = list <$> ask

type GUIState = AcidState ContactsList

$(makeAcidic ''ContactsList ['addContact', 'removeContact', 'getContacts'])

getContacts :: GUIState -> IO [Contact]
getContacts st = query st GetContacts'

addContact :: GUIState -> Contact -> IO ()
addContact st c = update st $ AddContact' c

removeContact :: GUIState -> Int -> IO ()
removeContact st i = update st $ RemoveContact' i
