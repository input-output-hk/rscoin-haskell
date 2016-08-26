-- | Encapsulate RSCoin.Bank.Storage.*

module RSCoin.Bank.Storage
        ( module Storage
        ) where

import           RSCoin.Bank.Storage.Queries as Storage
import           RSCoin.Bank.Storage.Storage as Storage (Storage, mkStorage)
import           RSCoin.Bank.Storage.Updates as Storage
