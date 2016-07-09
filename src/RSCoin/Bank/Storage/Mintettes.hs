{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

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
       , updateMintettes

       ) where

import           Control.Lens        (Getter, ix, makeLenses, use, (%=), (&),
                                      (.=), (.~))
import           Control.Monad       (unless)
import           Control.Monad.State (State)
import           Data.List           ((\\))
import qualified Data.Map            as M
import           Data.SafeCopy       (base, deriveSafeCopy)

import qualified RSCoin.Core         as C

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
      _msMintettes        :: !C.Mintettes
    ,
      -- | List of mintettes which were added in current period and
      -- will become active for the next period.
      _msPendingMintettes :: ![(C.Mintette, C.PublicKey)]
    ,
      -- | DPK set for the ongoing period. Doesn't mean anything if
      -- there is no active period.
      _msDpk              :: !C.Dpk
    ,
      -- | State of all known dead mintettes.
      _msDeadMintettes    :: !DeadMintettesMap
      -- | Mintettes' action logs. actionLogs[i] stores action log for
      -- i-th mintette.  Head of action log is the most recent entry.
    , _msActionLogs       :: [C.ActionLog]
    }

$(makeLenses ''MintettesStorage)
$(deriveSafeCopy 0 'base ''MintettesStorage)

mkMintettesStorage :: MintettesStorage
mkMintettesStorage =
    MintettesStorage
    { _msMintettes = []
    , _msPendingMintettes = []
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

type Update a = State MintettesStorage a

addMintette :: C.Mintette -> C.PublicKey -> Update ()
addMintette m k = do
    dpk <- use getDpk
    unless (k `elem` map fst dpk) $ msPendingMintettes %= ((m, k) :)

-- type MintetteInfo = (Mintette, (PublicKey, Signature), ActionLog)
updateMintettes :: C.SecretKey
                -> [(C.MintetteId, C.PeriodResult)]
                -> Update [C.MintetteId]
updateMintettes sk goodMintettes = do
    pendingPairs <- use msPendingMintettes
    let pendingMts = map fst pendingPairs
        pendingDpk = map doSign pendingPairs
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
    -- @TODO introduce a better solution if needed once
    if length existingMts /= length newMintettes
       -- Discard all mitettes' utxo lists in case of list lengths' mismatch
       -- Needed, cause `owners` function relies on the size of mintette list to provide an even distribution
       then return [0 .. length newMintettes - 1]
       else return updatedIndices
  where
    goodIndices = map fst goodMintettes
    doSign (_,mpk) = (mpk, C.sign sk mpk)
    formPendingLogs pendingPairs = do
        dm <- use msDeadMintettes
        return $
            map (maybe [] dmsActionLog . flip M.lookup dm . snd) pendingPairs
    appendNewLogs = mapM_ appendNewLogDo goodMintettes
    appendNewLogDo (idx,(_,_,newLog)) = msActionLogs . ix idx %= (newLog ++)

-- Given the list of bad indices, new list to append and data list,
-- this function returns datalist with appended data and removed bad
-- indices so that number of elements that change their place is
-- minimized.
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
