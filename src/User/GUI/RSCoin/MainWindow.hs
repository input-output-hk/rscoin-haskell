-- | Module that represents gui widgets types.
module GUI.RSCoin.MainWindow
       ( MainWindow (..)
       , WalletModelNode (..)
       , WalletTab (..)
       , TransactionsTab (..)
       , ContactsTab (..)
       , AddContactWindow (..)
       ) where

import qualified Graphics.UI.Gtk as G

-- | Main window, the whole gui -- all elements we're interested in.
data MainWindow = MainWindow
    { mainWindow        :: G.Window
    , notebookMain      :: G.Notebook
    , progressBarUpdate :: G.ProgressBar
    , tabWallet         :: WalletTab
    , tabTransactions   :: TransactionsTab
    , tabContacts       :: ContactsTab
    }

data WalletModelNode = WalletModelNode
    { wIsSend      :: Bool
    , wIsConfirmed :: Bool
    , wTime        :: String
    , wAddress     :: String
    , wAmount      :: Integer
    }

data WalletTab = WalletTab
    { treeViewWallet          :: G.TreeView
    , walletModel             :: G.ListStore WalletModelNode
    , boxWalletHeader         :: G.Box
    , labelCurrentBalance     :: G.Label
    , labelUnconfirmedBalance :: G.Label
    , labelTransactionsNumber :: G.Label
    , labelCurrentAccount     :: G.Label
    }

data TransactionsTab = TransactionsTab
    { entryPayTo           :: G.Entry
    , buttonChooseContacts :: G.Button
    , spinButtonSendAmount :: G.SpinButton
    , buttonConfirmSend    :: G.Button
    , buttonClearSend      :: G.Button
    }

data ContactsTab = ContactsTab
    { treeViewContactsView :: G.TreeView
    , buttonAddContact     :: G.Button
    , labelContactsNum     :: G.Label
    }

data AddContactWindow = AddContactWindow
    { addContactWindow    :: G.Window
    , entryContactName    :: G.Entry
    , entryContactAddress :: G.Entry
    , buttonContactOk     :: G.Button
    , buttonContactCancel :: G.Button
    }
