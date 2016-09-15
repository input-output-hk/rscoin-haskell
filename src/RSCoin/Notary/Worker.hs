{-# LANGUAGE ScopedTypeVariables #-}

module RSCoin.Notary.Worker
       ( runFetchWorker
       ) where

import           Control.Monad             (forM_)
import           Data.List.Split           (chunksOf)
import           Formatting                (build, int, sformat, (%))

import           Control.TimeWarp.Timed    (repeatForever, sec, tu)
import           Serokell.Util.Exceptions  ()

import           RSCoin.Core               (PeriodId, WorkMode, logDebug,
                                            logError, logInfo, logWarning)
import qualified RSCoin.Core.Communication as CC

import           RSCoin.Notary.AcidState   (AnnounceNewPeriods (..),
                                            GetPeriodId (..), NotaryState,
                                            SetSynchronization (..), query,
                                            update)

-- | This worker queries Bank each period to see if notary and bank periods matches.
-- If they are not same than Notary stops serving requests until he become updated.
runFetchWorker
    :: WorkMode m
    => NotaryState
    -> m ()
runFetchWorker st =
    repeatForever (tu $ sec 20) handler fetchWorker  -- TODO: use not magic constant
  where
    fetchWorker = do
        logDebug "Fetching HBlocks..."
        notaryPid  <- query st GetPeriodId
        bankHeight <- CC.getBlockchainHeight
        let bankPid = bankHeight - 1

        if notaryPid /= bankPid then do
            update st $ SetSynchronization False
            logWarning $ sformat ("Notary pid " % int % " not equals to bank " % int)
                notaryPid
                bankPid
            runStorageUpdate st (max 0 notaryPid) bankPid
            logInfo "Notary updated his storage!"
        else
            logDebug "Notary storagy is syncronized"

        update st $ SetSynchronization True

    handler e = do
        logError $ sformat
            ("Error was caught by notary worker, restarting in 20 seconds: " % build)
            e
        return $ sec 20

-- | Update Notary storage by fetching HBlocks from Bank.
runStorageUpdate
    :: WorkMode m
    => NotaryState
    -> PeriodId
    -> PeriodId
    -> m ()
runStorageUpdate st from to = do
    -- TODO: optimize into list of pairs
    let fetchPids    = [from .. to]
    let fetchBatches = chunksOf CC.blocksQueryLimit fetchPids
    forM_ fetchBatches $ \pids -> do
        let lastBatchPid = last pids
        newHBlocks <- CC.getBlocksByHeight (head pids) lastBatchPid
        update st $ AnnounceNewPeriods lastBatchPid newHBlocks
