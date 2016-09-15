{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Storage which contains data about mintettes.

module RSCoin.Bank.Storage.Mintettes
       ( MintettesStorage
       , mkMintettesStorage

       , Query
       , getMintettes
       , getDpk
       , getActionLogs

       , Update
       , addMintette
       , permitMintette
       , removeMintette
       , updateMintettes

       ) where

import           Control.Lens         (Getter, ix, makeLenses, use, uses, (%=),
                                       (&), (.=), (.~))
import           Control.Monad        (unless, when)
import           Control.Monad.Except (ExceptT, MonadError (throwError))
import           Control.Monad.State  (State)
import           Data.List            (delete, find, nub, (\\))
import qualified Data.Map             as M
import qualified Data.Set             as Set
import           Data.SafeCopy        (base, deriveSafeCopy)
import           Formatting           (build, sformat, (%))

import           RSCoin.Bank.Error    (BankError (BEBadRequest))
import qualified RSCoin.Core          as C

-- | DeadMintetteState represents state of mintette which was removed
-- from storage. It's needed to restore data if mintette resurrects.
data DeadMintetteState = DeadMintetteState
    { dmsActionLog :: C.ActionLog
    }

$(deriveSafeCopy 0 'base ''DeadMintetteState)

type DeadMintettesMap = M.Map C.PublicKey DeadMintetteState

data MintettesStorage = MintettesStorage
    {
      -- | List of active mintettes.
      _msMintettes         :: !C.Mintettes
    ,
      -- | List of mintettes which were added in current period and
      -- will become active for the next period.
      -- TODO: should be a set for sake of simplicity
      _msPendingMintettes  :: ![(C.Mintette, C.PublicKey)]
    ,
      -- | Set of permissions for adding mintettes
      _msPermittedMintettes :: !(Set.Set C.PublicKey)
    ,
      -- | Mintettes that should be excluded in the next period
      _msMintettesToRemove :: !C.Mintettes
    ,
      -- | DPK set for the ongoing period. Doesn't mean anything if
      -- there is no active period.
      _msDpk               :: !C.Dpk
    ,
      -- | State of all known dead mintettes.
      _msDeadMintettes     :: !DeadMintettesMap
    ,
      -- | Mintettes' action logs. actionLogs[i] stores action log for
      -- i-th mintette.  Head of action log is the most recent entry.
      _msActionLogs        :: ![C.ActionLog]
    }

$(makeLenses ''MintettesStorage)
$(deriveSafeCopy 0 'base ''MintettesStorage)

mkMintettesStorage :: MintettesStorage
mkMintettesStorage =
    MintettesStorage
    { _msMintettes = []
    , _msPendingMintettes = []
    , _msPermittedMintettes = Set.empty
    , _msMintettesToRemove = []
    , _msDpk = []
    , _msDeadMintettes = M.empty
    , _msActionLogs = []
    }

type Query a = Getter MintettesStorage a

getMintettes :: Query C.Mintettes
getMintettes = msMintettes

getDpk :: Query C.Dpk
getDpk = msDpk

getActionLogs :: Query [C.ActionLog]
getActionLogs = msActionLogs

type Update = State MintettesStorage
type ExceptUpdate = ExceptT BankError Update

-- | Add mintette to the storage
addMintette :: C.Mintette -> C.PublicKey -> ExceptUpdate ()
addMintette m k = do
    isAdded <- (||) <$> uses msDpk (\dpk -> k `elem` map fst dpk)
                    <*> uses msPendingMintettes ((m `elem`) . map fst)
    when isAdded $ throwError $ BEBadRequest $
        sformat ("Mintette " % build % " is already added, won't add.") m
    msMintettesToRemove %= delete m
    msPendingMintettes %= ((m, k) :)

-- | Add mintette public key to the storage
permitMintette :: C.PublicKey -> ExceptUpdate ()
permitMintette k = do
    msPermittedMintettes %= Set.insert k

-- | Unstages a mintette from being in a next period. Is canceled by
-- `addMintette` and vice versa
removeMintette :: C.Mintette -> ExceptUpdate ()
removeMintette m = do
    isAdded <- (||) <$> uses msMintettes (m `elem`)
                    <*> uses msPendingMintettes ((m `elem`) . map fst)
    unless isAdded $ throwError $ BEBadRequest $
        sformat ("Mintette " % build % " is not in the storage, can't remove") m
    e <- uses msPendingMintettes $ find ((== m) . fst)
    maybe (msMintettesToRemove %= nub . (m:))
            (\m' -> msPendingMintettes %= delete m')
              e

-- | Update mintettes state, returning mintette id's that should
-- change their utxo. Performs things as saving actionlogs, checking
-- signatures, adding new mintettes and kicking old ones.
updateMintettes :: C.SecretKey
                -> [(C.MintetteId, C.PeriodResult)]
                -> Update [C.MintetteId]
updateMintettes sk goodMintettes = do
    pendingPairs <- use msPendingMintettes
    toRemove <- use msMintettesToRemove
    toRemoveIds <- uses msMintettes $
        map fst . filter (\(_,m) -> m `elem` toRemove) . ([0..] `zip`)
    let pendingMts = map fst pendingPairs
        pendingDpk = map doSign pendingPairs
        goodMintettesWORemoved = filter ((`notElem` toRemoveIds) . fst) goodMintettes
        goodIndices = map fst goodMintettesWORemoved
        appendNewLogs = mapM_ appendNewLogDo goodMintettesWORemoved
    pendingLogs <- formPendingLogs pendingPairs
    existingMts <- use getMintettes
    existingDpk <- use getDpk
    appendNewLogs
    existingLogs <- use msActionLogs
    let pendingMintetteInfo = zip3 pendingMts pendingDpk pendingLogs
        existingMintetteInfo = zip3 existingMts existingDpk existingLogs
        badIndices = [0 .. length existingMintetteInfo - 1] \\ goodIndices
        (newMintetteInfo,updatedIndices) =
            replaceWithCare badIndices pendingMintetteInfo existingMintetteInfo
        (newMintettes,newDpk,newActionLogs) = unzip3 newMintetteInfo
    storeDeadState badIndices
    msPendingMintettes .= []
    msMintettes .= newMintettes
    msDpk .= newDpk
    msActionLogs .= newActionLogs
    msMintettesToRemove .= []
    -- @TODO introduce a better solution if needed once
    if length existingMts /= length newMintettes
       -- Discard all mitettes' utxo lists in case of list lengths' mismatch
       -- Needed, cause `owners` function relies on the size of mintette list to provide an even distribution
       then return [0 .. length newMintettes - 1]
       else return updatedIndices
  where
    doSign (_,mpk) = (mpk, C.sign sk mpk)
    formPendingLogs pendingPairs = do
        dm <- use msDeadMintettes
        return $
            map (maybe [] dmsActionLog . flip M.lookup dm . snd) pendingPairs
    appendNewLogDo (idx,(_,_,newLog)) = msActionLogs . ix idx %= (newLog ++)

-- | Given the list of bad indices, new list to append and data list,
-- this function returns datalist with appended data and removed bad
-- indices so that number of elements that change their place is
-- minimized. Return value is a pair where second element is exactly
-- elements that changed their indices.
replaceWithCare :: [Int] -> [a] -> [a] -> ([a], [Int])
replaceWithCare bad new old = replaceWithCare' bad new old []

replaceWithCare' :: [Int] -> [a] -> [a] -> [Int] -> ([a], [Int])
replaceWithCare' [] [] list acc = (list, acc)
replaceWithCare' [] pending list acc =
    (list ++ pending, acc ++ [length list .. length list + length pending - 1])
replaceWithCare' (bad:bads) [] list acc =
    replaceWithCare'
        bads
        []
        (take (length list - 1) list & ix bad .~ last list)
        (bad : acc)
replaceWithCare' (bad:bads) (pendh:pends) list acc =
    replaceWithCare'
        bads
        pends
        (list & ix bad .~ pendh)
        (bad : acc)

storeDeadState :: [C.MintetteId] -> Update ()
storeDeadState badIndices = do
    pks <- map fst <$> use msDpk
    logs <- use msActionLogs
    mapM_
        (\i ->
              msDeadMintettes %=
              M.insert
                  (pks !! i)
                  (DeadMintetteState
                   { dmsActionLog = logs !! i
                   }))
        badIndices
