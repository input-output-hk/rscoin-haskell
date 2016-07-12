{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

-- | Storage which contains data about explorers.

module RSCoin.Bank.Storage.Explorers
       ( ExplorersStorage
       , mkExplorersStorage

       , Query
       , getExplorers
       , getExplorersAndPeriods

       , Update
       , addExplorer
       , setExplorerPeriod

       ) where

import           Control.Lens        (Getter, at, makeLenses, to, (.=))
import           Control.Monad.State (State)
import qualified Data.Map            as M
import           Data.SafeCopy       (base, deriveSafeCopy)

import qualified RSCoin.Core         as C

data ExplorersStorage = ExplorersStorage
    {
      -- | All explorers in storage. Explorer is associated with
      -- `PeriodId`. This is id of block which Explorer expects to
      -- receive.
      _esExplorers :: M.Map C.Explorer C.PeriodId
    }

$(makeLenses ''ExplorersStorage)
$(deriveSafeCopy 0 'base ''ExplorersStorage)

mkExplorersStorage :: ExplorersStorage
mkExplorersStorage =
    ExplorersStorage
    { _esExplorers = M.empty
    }

type Query a = Getter ExplorersStorage a

-- | Get list of all explorers in storage.
getExplorers :: Query C.Explorers
getExplorers = esExplorers . to M.keys

-- | Get list of all explorers in storage and ids of periods they
-- expect to receive.
getExplorersAndPeriods :: Query [(C.Explorer, C.PeriodId)]
getExplorersAndPeriods = esExplorers . to M.assocs

type Update a = State ExplorersStorage a

-- | Add given explorer to storage and associate given PeriodId with
-- it. If explorer exists, it is updated.
addExplorer :: C.Explorer -> C.PeriodId -> Update ()
addExplorer e pId = esExplorers . at e .= Just pId

-- | Update expected period id of given explorer. Adds explorer if it
-- doesn't exist.
setExplorerPeriod :: C.Explorer -> C.PeriodId -> Update ()
setExplorerPeriod = addExplorer
