-- | Functions tightly related to ActionLog

module RSCoin.Core.ActionLog
       ( checkActionLog
       , actionLogNext
       ) where

import           RSCoin.Core.Crypto (Hash, hash)
import           RSCoin.Core.Types  (ActionLog, ActionLogEntry)

initialHash :: Hash
initialHash = hash ("Ivan" :: String)

-- | Check action log integrity using optional preceding element in log.
checkActionLog :: Maybe (ActionLogEntry, Hash) -> ActionLog -> Bool
checkActionLog start entries = all check [0 .. length entries - 1]
  where
    ithEntry i = fst $ entries !! i
    ithHash i = snd $ entries !! i
    prevHash = maybe initialHash snd start
    check i
      | i == length entries - 1 = ithHash i == hash (ithEntry i, prevHash)
      | otherwise = ithHash i == hash (ithEntry i, ithHash (i + 1))

-- | Generate next element in ActionLog using entry itself
-- and optional previous element.
actionLogNext :: Maybe (ActionLogEntry, Hash)
              -> ActionLogEntry
              -> (ActionLogEntry, Hash)
actionLogNext prevHead entry = (entry, hash (entry, prevHash))
  where prevHash = maybe initialHash snd prevHead
