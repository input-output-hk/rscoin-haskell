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
       , removeExplorer
       , setExplorerPeriod
       , suspendExplorer
       , restoreExplorers

       ) where

import           Control.Lens         (Getter, at, makeLenses, to, use, uses, (%=), (.=),
                                       (<>=))
import           Control.Monad.Except (ExceptT, MonadError (throwError))
import           Control.Monad.State  (State)
import           Data.List            (find)
import qualified Data.Map             as M
import           Data.SafeCopy        (base, deriveSafeCopy)
import           Formatting           (int, sformat, string, (%))

import           RSCoin.Bank.Error    (BankError (BEBadRequest))
import qualified RSCoin.Core          as C

data ExplorersStorage = ExplorersStorage
    { -- | All explorers in storage. Explorer is associated with
      -- `PeriodId`. This is id of block which Explorer expects to
      -- receive.
      _esExplorers          :: !(M.Map C.Explorer C.PeriodId)
      -- | Some explorers may be suspended if something suspicious is
      -- detected.
    , _esSuspendedExplorers :: !(M.Map C.Explorer C.PeriodId)
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

type Update = State ExplorersStorage
type ExceptUpdate = ExceptT BankError Update

-- | Add given explorer to storage and associate given PeriodId with
-- it. If explorer exists, it is updated.
addExplorer :: C.Explorer -> C.PeriodId -> Update ()
addExplorer e pId = esExplorers . at e .= Just pId

-- | Removes a host that matches host/port, fails if not present
removeExplorer :: String -> Int -> ExceptUpdate ()
removeExplorer host port = do
    expls <- uses esExplorers M.keys
    let res = find (\C.Explorer{..} -> explorerHost == host &&
                                       explorerPort == port) expls
    maybe (throwError $ BEBadRequest $
           sformat ("Explorer with host " % string % " and port " % int %
                    " is not in the storage, can't remove") host port)
          (\explorer -> esExplorers %= M.delete explorer)
          res

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
restoreExplorers :: Update [C.Explorer]
restoreExplorers = do
    suspended <- use esSuspendedExplorers
    esExplorers <>= suspended
    esSuspendedExplorers .= mempty
    return $ M.keys suspended
