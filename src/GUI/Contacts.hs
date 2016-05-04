{-# LANGUAGE TemplateHaskell #-}

module Contacts where

import Control.Lens

data Contact = Contact {
    _name    :: String,
    _address :: String
}

makeLenses ''Contact
