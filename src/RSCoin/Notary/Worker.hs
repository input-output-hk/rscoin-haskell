{-# LANGUAGE ScopedTypeVariables #-}

module RSCoin.Notary.Worker
       ( runFetchWorker
       ) where

import           Control.Monad             (forM_)
import           Data.List.Split           (chunksOf)
import           Formatting                (build, int, sformat, (%))

import           Control.TimeWarp.Timed    (for, ms, repeatForever, sec, tu,
                                            wait)
import           Serokell.Util.Exceptions  ()

import           RSCoin.Core               (PeriodId, WorkMode, logDebug,
                                            logError, logInfo, logWarning)
import qualified RSCoin.Core.Communication as CC

import           RSCoin.Notary.AcidState   (BatchUpdatePeriods (..),
                                            GetPeriodId (..), NotaryState,
                                            SetSynchronization (..), query,
                                            update)

-- | This worker queries Bank each period to see if notary and bank periods match.
-- If they are not same then Notary stops serving requests until he becomes updated.
runFetchWorker
    :: WorkMode m
    => NotaryState
    -> m ()
runFetchWorker st = do
    wait $ for 500 ms  -- Short wait to make rscoin-deploy happy
    repeatForever (tu $ sec 30) handler fetchWorker  -- TODO: use not magic constant
  where
    fetchWorker = do
        logDebug "Fetching HBlocks..."

        -- TODO: atomicity problems? Notary period can change and we will got error =\
        notaryPid <- query st GetPeriodId
        bankPid   <- CC.getBlockchainHeight
        logDebug $ sformat ("Notary's pid " % int % "; bank's " % int)
            notaryPid
            bankPid

        if notaryPid < bankPid then do
            update st $ SetSynchronization False
            logWarning "Notary is not synched!"
            runStorageUpdate st notaryPid (bankPid - 1)
            logInfo "Notary updated his storage"
        else
            logDebug "Notary storage is synchronized"

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
        update st $ BatchUpdatePeriods (lastBatchPid + 1) newHBlocks
