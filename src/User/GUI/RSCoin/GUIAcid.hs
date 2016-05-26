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
    , replaceWithName
    ) where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)
import           Data.Acid            (AcidState, Query, Update, makeAcidic,
                                       query, update)
import           Data.List            (find)
import           Data.SafeCopy        (base, deriveSafeCopy)
import qualified Data.Text            as T

import           Serokell.Util.Text   (formatSingle')

import qualified RSCoin.Core          as C

-- | Contact information.
data Contact = Contact
    { contactName    :: T.Text
    , contactAddress :: T.Text
    } deriving (Show)

dummyContacts :: [Contact]
dummyContacts =
    [Contact "aoeu" "kek", Contact "kek" "ahaha", Contact "memasy" "podkatili"]

-- | List of contacts known to the user.
data ContactsList = ContactsList { unContactsList :: [Contact] }

emptyGUIAcid :: ContactsList
emptyGUIAcid = ContactsList []

$(deriveSafeCopy 0 'base ''Contact)
$(deriveSafeCopy 0 'base ''ContactsList)

-- | Adds a contact to the list.
addContact' :: Contact -> Update ContactsList ()
addContact' c = do
    l <- unContactsList <$> get
    put $ ContactsList $ c : l

-- | Removes a contact from the list.
removeContact' :: Int -> Update ContactsList ()
removeContact' i = do
    l <- unContactsList <$> get
    put $ ContactsList $ take i l ++ drop (i + 1) l

-- | Gets the list from the database.
getContacts' :: Query ContactsList [Contact]
getContacts' = unContactsList <$> ask

replaceWithName' :: C.Address -> Query ContactsList (Maybe T.Text)
replaceWithName' addr = do
    list <- unContactsList <$> ask
    let e = find (\x -> contactAddress x == formatSingle' "{}" addr) list
    return $ contactName <$> e

type GUIState = AcidState ContactsList

$(makeAcidic
      ''ContactsList
      ['addContact', 'removeContact', 'getContacts', 'replaceWithName'])

getContacts :: GUIState -> IO [Contact]
getContacts st = query st GetContacts'

addContact :: GUIState -> Contact -> IO ()
addContact st c = update st $ AddContact' c

removeContact :: GUIState -> Int -> IO ()
removeContact st i = update st $ RemoveContact' i

replaceWithName :: GUIState -> C.Address -> IO (Maybe T.Text)
replaceWithName st addr = query st $ ReplaceWithName' addr
