{-# LANGUAGE TemplateHaskell #-}

module Wallet where

import Control.Lens

data Contact = Contact {
    _name    :: String,
    _address :: String
}

data Transaction = Transaction {
    _input   :: Bool,
    _contact :: Contact,
    _value   :: Double
}

data Wallet = Wallet {
    _myAddress    :: String,
    _balance      :: Double,
    _unconfirmed  :: Double,
    _contacts     :: [Contact],
    _transactions :: [Transaction]
}

makeLenses ''Contact
makeLenses ''Transaction
makeLenses ''Wallet
