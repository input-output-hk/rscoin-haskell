-- | Convenience functions to launch mintette.

module RSCoin.Mintette.Launcher
       ( ContextArgument (..)

       , dumpStorageStatistics
       , launchMintetteReal
       , mintetteWrapperReal
       , addToBank
       ) where

import           Control.Monad.Catch       (bracket)
import           Control.Monad.Trans       (MonadIO (liftIO))
import qualified Data.Text.IO              as TIO
import           Data.Time.Units           (TimeUnit)
import           Formatting                (int, sformat, stext, (%))

import           Control.TimeWarp.Timed    (fork_)
import           RSCoin.Core               (ContextArgument (..), RealMode,
                                            mintetteLoggerName,
                                            runRealModeUntrusted, SecretKey)
import qualified RSCoin.Core.Communication as CC

import           RSCoin.Mintette.Acidic    (GetPeriodId (..), closeState,
                                            getStatistics, openMemState,
                                            openState)
import           RSCoin.Mintette.AcidState (State, query)
import           RSCoin.Mintette.Env       (RuntimeEnv)
import           RSCoin.Mintette.Server    (serve)
import           RSCoin.Mintette.Worker    (runWorkerWithDelta)

mintetteWrapperReal :: Bool
                    -> Maybe FilePath
                    -> ContextArgument
                    -> (State -> RealMode a)
                    -> IO a
mintetteWrapperReal deleteIfExists dbPath ca action = do
    let openAction = maybe openMemState (openState deleteIfExists) dbPath
    runRealModeUntrusted mintetteLoggerName ca . bracket openAction closeState $
        action

launchMintetteReal
    :: (Show t, Num t, Integral t, TimeUnit t)
    => Bool -> t -> Int -> RuntimeEnv -> Maybe FilePath -> ContextArgument -> IO ()
launchMintetteReal deleteIfExists epochDelta port env dbPath ctxArg =
    mintetteWrapperReal deleteIfExists dbPath ctxArg $
    \st -> do
        fork_ $ runWorkerWithDelta epochDelta env st
        serve port st env

addToBank :: Bool -> FilePath -> ContextArgument -> SecretKey -> String -> Int -> IO ()
addToBank deleteIfExists dbPath ctxArg mintetteSK host port = do
    mintetteWrapperReal deleteIfExists (Just dbPath) ctxArg impl
      where
        impl st = do
          CC.addMintetteUsingPermission mintetteSK host port

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
