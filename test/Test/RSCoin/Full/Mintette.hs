-- | This module describes possible variants of mintette behavior
-- (normal, with errors, malicious)

module Test.RSCoin.Full.Mintette
       ( defaultMintetteInit
       , malfunctioningMintetteInit
       ) where

import           Control.Concurrent.MVar          (MVar, isEmptyMVar)
import           Control.Lens                     (view)
import           Control.Monad.Trans              (liftIO)

import qualified RSCoin.Mintette                  as M
import           RSCoin.Timed                     (WorkMode, workWhile)

import           Test.RSCoin.Full.Context         (MintetteInfo, port,
                                                   secretKey, state)
import           Test.RSCoin.Full.Mintette.Config (MintetteConfig,
                                                   malfunctioningConfig)
import qualified Test.RSCoin.Full.Mintette.Server as FM

initialization
    :: (WorkMode m)
    => Maybe MintetteConfig -> MVar () -> MintetteInfo -> m ()
initialization conf v m = do
    let runner =
            case conf of
                Nothing -> M.serve
                Just s -> FM.serve s
    workWhileMVarEmpty v $
        runner <$> view port <*> view state <*> view secretKey $ m
    workWhileMVarEmpty v $ M.runWorker <$> view secretKey <*> view state $ m

defaultMintetteInit
    :: (WorkMode m)
    => MVar () -> MintetteInfo -> m ()
defaultMintetteInit = initialization Nothing

malfunctioningMintetteInit
    :: (WorkMode m)
    => MVar () -> MintetteInfo -> m ()
malfunctioningMintetteInit =
    initialization (Just malfunctioningConfig)

workWhileMVarEmpty
    :: WorkMode m
    => MVar a -> m () -> m ()
workWhileMVarEmpty v = workWhile (liftIO . isEmptyMVar $ v)
