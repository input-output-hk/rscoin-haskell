{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

-- | Database that holds all stuff needed for GUI

module GUI.RSCoin.GUIAcid
    ( Contact (..)
    , GUIState
    , emptyGUIAcid
    , getContacts
    , addContact
    , removeContact
    , replaceWithName
    , addTransaction
    , getTransaction
    ) where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)
import           Data.Acid            (AcidState, Query, Update, makeAcidic,
                                       query, update)
import           Data.List            (find)
import           Data.Map             (Map, empty, insert, lookup)
import           Data.SafeCopy        (base, deriveSafeCopy)
import qualified Data.Text            as T

import           Serokell.Util.Text   (show')

import qualified RSCoin.Core          as C

-- | Contact information.
data Contact = Contact
    { contactName    :: T.Text
    , contactAddress :: T.Text
    } deriving (Show)

-- | List of contacts known to the user.
data GUIDB = GUIDB
    { contactsList      :: [Contact]
    , transactionsCache :: Map C.TransactionId (Maybe C.Transaction)
    }

emptyGUIAcid :: GUIDB
emptyGUIAcid = GUIDB [] empty

$(deriveSafeCopy 0 'base ''Contact)
$(deriveSafeCopy 0 'base ''GUIDB)

-- | Adds a contact to the list.
addContact' :: Contact -> Update GUIDB ()
addContact' c = do
    GUIDB l m <- get
    put $ GUIDB (c : l) m

-- | Removes a contact from the list.
removeContact' :: Int -> Update GUIDB ()
removeContact' i = do
    GUIDB l m <- get
    put $ GUIDB (take i l ++ drop (i + 1) l) m

-- | Gets the list from the database.
getContacts' :: Query GUIDB [Contact]
getContacts' = contactsList <$> ask

replaceWithName' :: C.Address -> Query GUIDB (Maybe T.Text)
replaceWithName' addr = do
    list <- contactsList <$> ask
    let e = find (\x -> contactAddress x == show' addr) list
    return $ contactName <$> e

addTransaction' :: C.TransactionId -> Maybe C.Transaction -> Update GUIDB ()
addTransaction' tId mt = do
    GUIDB l m <- get
    put $ GUIDB l $ insert tId mt m

getTransaction' :: C.TransactionId -> Query GUIDB (Maybe (Maybe C.Transaction))
getTransaction' tId = do
    map' <- transactionsCache <$> ask
    return $ Data.Map.lookup tId map'

type GUIState = AcidState GUIDB

$(makeAcidic
      ''GUIDB
      ['addContact', 'removeContact', 'getContacts'
      , 'replaceWithName', 'addTransaction', 'getTransaction'])

getContacts :: GUIState -> IO [Contact]
getContacts st = query st GetContacts'

addContact :: GUIState -> Contact -> IO ()
addContact st c = update st $ AddContact' c

removeContact :: GUIState -> Int -> IO ()
removeContact st i = update st $ RemoveContact' i

replaceWithName :: GUIState -> C.Address -> IO (Maybe T.Text)
replaceWithName st addr = query st $ ReplaceWithName' addr

addTransaction :: GUIState -> C.TransactionId -> Maybe C.Transaction -> IO ()
addTransaction st tId mt = update st $ AddTransaction' tId mt

getTransaction
    :: GUIState -> C.TransactionId -> IO (Maybe (Maybe C.Transaction))
getTransaction st tId = query st $ GetTransaction' tId
