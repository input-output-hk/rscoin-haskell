{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Storage for Bank data

module Storage
       ( Storage
       , mkStorage
       , getMintettes
       , addMintette
       ) where

import           Control.Lens        (Getter, makeLenses, (%=))
import           Control.Monad.State (State)
import           Data.Typeable       (Typeable)

import           RSCoin.Core         (Mintette, Mintettes)

data Storage = Storage
    { _mintettes :: Mintettes
    } deriving (Typeable)

$(makeLenses ''Storage)

mkStorage :: Storage
mkStorage = Storage []

type Query a = Getter Storage a

getMintettes :: Query Mintettes
getMintettes = mintettes

type Update = State Storage

addMintette :: Mintette -> Update ()
addMintette m = mintettes %= (m:)
