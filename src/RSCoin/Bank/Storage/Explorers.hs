{-# LANGUAGE TemplateHaskell #-}

-- | Storage which contains data about explorers.

module RSCoin.Bank.Storage.Explorers
       ( ExplorersStorage
       , mkExplorersStorage
       ) where

import           Data.SafeCopy (base, deriveSafeCopy)

data ExplorersStorage = ExplorersStorage

$(deriveSafeCopy 0 'base ''ExplorersStorage)

mkExplorersStorage :: ExplorersStorage
mkExplorersStorage = ExplorersStorage
