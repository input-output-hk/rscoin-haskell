-- | Convenience functions to launch mintette.

module RSCoin.Mintette.Launcher
       ( ContextArgument (..)

       , dumpStorageStatistics
       , launchMintetteReal
       , mintetteWrapperReal
       ) where

import           Control.Monad.Catch       (bracket)
import           Control.Monad.Trans       (MonadIO (liftIO))
import qualified Data.Text.IO              as TIO
import           Data.Time.Units           (TimeUnit)
import           Formatting                (int, sformat, stext, (%))

import           RSCoin.Core               (SecretKey, mintetteLoggerName)
import           RSCoin.Timed              (ContextArgument (..), MsgPackRpc,
                                            fork_, runRealModeUntrusted)

import           RSCoin.Mintette.Acidic    (GetPeriodId (..), closeState,
                                            getStatistics, openMemState,
                                            openState)
import           RSCoin.Mintette.AcidState (State, query)
import           RSCoin.Mintette.Server    (serve)
import           RSCoin.Mintette.Worker    (runWorkerWithDelta)

mintetteWrapperReal :: Bool
                    -> Maybe FilePath
                    -> ContextArgument
                    -> (State -> MsgPackRpc a)
                    -> IO a
mintetteWrapperReal deleteIfExists dbPath ca action = do
    let openAction = maybe openMemState (openState deleteIfExists) dbPath
    runRealModeUntrusted mintetteLoggerName ca . bracket openAction closeState $
        action

launchMintetteReal
    :: (Show t, Num t, Integral t, TimeUnit t)
    => Bool -> t -> Int -> SecretKey -> Maybe FilePath -> ContextArgument -> IO ()
launchMintetteReal deleteIfExists epochDelta port sk dbPath ctxArg =
    mintetteWrapperReal deleteIfExists dbPath ctxArg $
    \st -> do
        fork_ $ runWorkerWithDelta epochDelta sk st
        serve port st sk

dumpStorageStatistics :: Bool -> FilePath -> ContextArgument -> IO ()
dumpStorageStatistics deleteIfExists dbPath ctxArg =
    mintetteWrapperReal deleteIfExists (Just dbPath) ctxArg impl
  where
    impl st = do
        pId <- query st GetPeriodId
        liftIO .
            TIO.putStrLn .
            sformat
                ("Storage statistics (period id is " % int % "):\n" % stext)
                pId =<<
            getStatistics st
