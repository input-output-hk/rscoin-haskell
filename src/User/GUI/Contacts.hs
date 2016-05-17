{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- | Contacts list.

-- FIXME Contacts acid helpers in GUI.* namespace...
module GUI.Contacts
    ( Contact (..)
    , ContactsList (..)
    , ContactsState
    , AddContact (..)
    , GetContacts (..)
    ) where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)
import           Data.Acid            (AcidState, Query, Update, makeAcidic)
import           Data.SafeCopy        (base, deriveSafeCopy)
import           Data.Text            (Text)

data Contact = Contact
    { name    :: Text
    , address :: Text
    }

data ContactsList = ContactsList { list :: [Contact] }

$(deriveSafeCopy 0 'base ''Contact)
$(deriveSafeCopy 0 'base ''ContactsList)

addContact :: Contact -> Update ContactsList ()
addContact c = do
    ContactsList l <- get
    put $ ContactsList $ c : l

getContacts :: Query ContactsList [Contact]
getContacts = list <$> ask

type ContactsState = AcidState ContactsList

$(makeAcidic ''ContactsList ['addContact, 'getContacts])
