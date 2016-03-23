{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Storage for Bank data

module Storage
       ( Storage
       , mkStorage
       , getMintettes
       ) where

import           Control.Lens  (Getter, makeLenses)
import           Data.Typeable (Typeable)

import           RSCoin.Core   (Mintettes)

data Storage = Storage
    { _mintettes :: Mintettes
    } deriving (Typeable)

$(makeLenses ''Storage)

mkStorage :: Storage
mkStorage = Storage []

type Query a = Getter Storage a

getMintettes :: Query Mintettes
getMintettes = mintettes
