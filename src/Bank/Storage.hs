{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Storage for Bank data

module Storage
       ( Storage
       , mkStorage
       , getMintettes
       , getPeriodId
       , addMintette
       ) where

import           Control.Lens        (Getter, makeLenses, to, (%=))
import           Control.Monad.State (State)
import qualified Data.Map            as M
import           Data.Typeable       (Typeable)

import           RSCoin.Core         (Mintette, Mintettes, PeriodId, PublicKey)

data Storage = Storage
    { _mintettes :: M.Map Mintette PublicKey
    , _periodId  :: PeriodId
    } deriving (Typeable)

$(makeLenses ''Storage)

mkStorage :: Storage
mkStorage = Storage M.empty 0

type Query a = Getter Storage a

getMintettes :: Query Mintettes
getMintettes = mintettes . to M.keys

getPeriodId :: Query PeriodId
getPeriodId = periodId

type Update = State Storage

-- | Add given mintette to storage and associate given key with it.
-- Overrides existing record if it already exists.
addMintette :: Mintette -> PublicKey -> Update ()
addMintette m k = mintettes %= (M.insert m k)
