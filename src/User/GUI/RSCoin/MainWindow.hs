-- | Module that represents gui widgets types.
module GUI.RSCoin.MainWindow
       ( MainWindow (..)
       , WalletModelNode (..)
       , WalletTab (..)
       , TransactionsTab (..)
       , ContactsTab (..)
       , AddressesTab (..)
       , AddContactWindow (..)
       ) where

import           Data.Int             (Int64)
import qualified Data.Text            as T
import qualified Graphics.UI.Gtk      as G

import           GUI.RSCoin.Addresses (VerboseAddress (..))
import           RSCoin.User.Wallet   (TxHStatus (..), TxHistoryRecord (..))

-- | Main window, the whole gui -- all elements we're interested in.
data MainWindow = MainWindow
    { mainWindow        :: G.Window
    , notebookMain      :: G.Notebook
    , progressBarUpdate :: G.ProgressBar
    , labelSync         :: G.Label
    , tabWallet         :: WalletTab
    , tabTransactions   :: TransactionsTab
    , tabContacts       :: ContactsTab
    , tabAddresses      :: AddressesTab
    }

data WalletModelNode = WalletModelNode
    { wIsSend  :: Bool
    , wStatus  :: TxHStatus
    , wHeight  :: Int
    , wAddress :: T.Text
    , wAmount  :: Int64
    , wTxHR    :: TxHistoryRecord
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
    , buttonRemoveContact  :: G.Button
    , labelContactsNum     :: G.Label
    }

data AddressesTab = AddressesTab
    { copyAddressButton     :: G.Button
    , treeViewAddressesView :: G.TreeView
    , addressesModel        :: G.ListStore VerboseAddress
    }

data AddContactWindow = AddContactWindow
    { addContactWindow    :: G.Window
    , entryContactName    :: G.Entry
    , entryContactAddress :: G.Entry
    , buttonContactOk     :: G.Button
    , buttonContactCancel :: G.Button
    }
