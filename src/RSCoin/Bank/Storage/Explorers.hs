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
       , suspendExplorer
       , restoreExplorers

       ) where

import           Control.Lens        (Getter, at, makeLenses, to, use, (.=),
                                      (<>=))
import           Control.Monad.State (State)
import qualified Data.Map            as M
import           Data.SafeCopy       (base, deriveSafeCopy)

import qualified RSCoin.Core         as C

data ExplorersStorage = ExplorersStorage
    {
      -- | All explorers in storage. Explorer is associated with
      -- `PeriodId`. This is id of block which Explorer expects to
      -- receive.
      _esExplorers          :: M.Map C.Explorer C.PeriodId
    ,
      -- | Some explorers may be suspended if something suspicious is
      -- detected.
      _esSuspendedExplorers :: M.Map C.Explorer C.PeriodId
    }

$(makeLenses ''ExplorersStorage)
$(deriveSafeCopy 0 'base ''ExplorersStorage)

mkExplorersStorage :: ExplorersStorage
mkExplorersStorage =
    ExplorersStorage
    { _esExplorers = M.empty
    , _esSuspendedExplorers = M.empty
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

-- | Temporarily delete explorer from storage until `restoreExplorers` is called.
suspendExplorer :: C.Explorer -> Update ()
suspendExplorer explorer = do
    explorerData <- use $ esExplorers . at explorer
    esExplorers . at explorer .= Nothing
    esSuspendedExplorers . at explorer .= explorerData

-- | Restore all suspended explorers.
restoreExplorers :: Update ()
restoreExplorers = do
    suspended <- use esSuspendedExplorers
    esExplorers <>= suspended
    esSuspendedExplorers .= M.empty
