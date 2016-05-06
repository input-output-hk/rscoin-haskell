{-# LANGUAGE TemplateHaskell #-}

-- | Contacts list.

module Contacts
    ( Contact (..)
    , name
    , address
    ) where

import           Control.Lens (makeLenses)
import           Data.Text    (Text)

data Contact = Contact
    { _name    :: Text
    , _address :: Text
    }

makeLenses ''Contact
