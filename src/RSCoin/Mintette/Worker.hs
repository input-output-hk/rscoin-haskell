{-# LANGUAGE ScopedTypeVariables #-}

-- | Worker that checks for the end of epoch.

module RSCoin.Mintette.Worker
       ( isMEInactive
       , runWorker
       ) where

import           Control.Monad             (unless, void, when)
import           Control.Monad.Extra       (whenJust)
import           Control.Monad.Trans       (liftIO)
import           Data.Acid                 (createArchive, createCheckpoint,
                                            query, update)
import qualified Data.Text                 as T
import           System.FilePath           ((</>))
import qualified Turtle.Prelude            as TURT

import           Serokell.Util.Exceptions  ()
import           Serokell.Util.Text        (formatSingle')

import           RSCoin.Core               (SecretKey, epochDelta, logError,
                                            mintetteLoggerName)

import           RSCoin.Mintette.Acidic    (FinishEpoch (..), GetPeriodId (..))
import           RSCoin.Mintette.AcidState (State)
import           RSCoin.Mintette.Error     (isMEInactive)

import           RSCoin.Timed              (WorkMode, repeatForever, sec, tu)

-- | Start worker which updates state when epoch finishes.
runWorker :: WorkMode m => SecretKey -> State -> Maybe FilePath -> m ()
runWorker sk st storagePath =
    repeatForever (tu epochDelta) handler $
    liftIO $ onEpochFinished sk st storagePath
  where
    handler e = do
        unless (isMEInactive e) $
            logError mintetteLoggerName $
            formatSingle'
                "Error was caught by worker, restarting in 2 seconds: {}"
                e
        return $ sec 2

onEpochFinished :: SecretKey -> State -> Maybe FilePath -> IO ()
onEpochFinished sk st storagePath = do
    update st $ FinishEpoch sk
    createCheckpoint st
    pid <- query st GetPeriodId
    whenJust storagePath $ \stpath -> liftIO $ do
        createArchive st
        void $ TURT.shellStrict
            (T.pack $ "rm -rf " ++ (stpath </> "Archive")) (return "")
