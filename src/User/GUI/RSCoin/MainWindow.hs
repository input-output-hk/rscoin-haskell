{-# LANGUAGE TemplateHaskell #-}

-- | Module that represents gui widgets types.
module GUI.RSCoin.MainWindow
       ( MainWindow (..)
       , WalletModelNode (..)
       , WalletTab (..)
       , TransactionsTab (..)
       , ContactsTab (..)

       , mainWindow
       , notebookMain
       , progressBarUpdate
       , tabWallet
       , tabTransactions
       , tabContacts

       , wIsSend
       , wIsConfirmed
       , wTime
       , wAddress
       , wAmount

       , treeViewWallet
       , walletModel
       , boxWalletHeader
       , labelCurrentBalance
       , labelUnconfirmedBalance
       , labelTransactionsNumber
       , labelCurrentAccount

       , entryPayTo
       , buttonChooseContacts
       , spinButtonSendAmount
       , buttonConfirmSend
       , buttonClearSend

       , treeViewContactsView
       , buttonAddContact
       , labelContactsNum
       ) where

import qualified Graphics.UI.Gtk as G

import qualified Control.Lens    as L

-- | Main window, the whole gui -- all elements we're interested in.
data MainWindow = MainWindow
    { _mainWindow        :: G.Window
    , _notebookMain      :: G.Notebook
    , _progressBarUpdate :: G.ProgressBar
    , _tabWallet         :: WalletTab
    , _tabTransactions   :: TransactionsTab
    , _tabContacts       :: ContactsTab
    }

data WalletModelNode = WalletModelNode
    { _wIsSend      :: Bool
    , _wIsConfirmed :: Bool
    , _wTime        :: String
    , _wAddress     :: String
    , _wAmount      :: Integer
    }

data WalletTab = WalletTab
    { _treeViewWallet          :: G.TreeView
    , _walletModel             :: G.ListStore WalletModelNode
    , _boxWalletHeader         :: G.Box
    , _labelCurrentBalance     :: G.Label
    , _labelUnconfirmedBalance :: G.Label
    , _labelTransactionsNumber :: G.Label
    , _labelCurrentAccount     :: G.Label
    }

data TransactionsTab = TransactionsTab
    { _entryPayTo           :: G.Entry
    , _buttonChooseContacts :: G.Button
    , _spinButtonSendAmount :: G.SpinButton
    , _buttonConfirmSend    :: G.Button
    , _buttonClearSend      :: G.Button
    }

data ContactsTab = ContactsTab
    { _treeViewContactsView :: G.TreeView
    , _buttonAddContact     :: G.Button
    , _labelContactsNum     :: G.Label
    }

$(L.makeLenses ''MainWindow)
$(L.makeLenses ''WalletModelNode)
$(L.makeLenses ''WalletTab)
$(L.makeLenses ''TransactionsTab)
$(L.makeLenses ''ContactsTab)
