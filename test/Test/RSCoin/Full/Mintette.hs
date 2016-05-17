-- | This module describes possible variants of mintette behavior
-- (normal, with errors, malicious)

module Test.RSCoin.Full.Mintette
       ( defaultMintetteInit
       , malfunctioningMintetteInit
       ) where

import           Control.Lens                     (view)

import qualified RSCoin.Mintette                  as M
import           RSCoin.Timed                     (Microsecond, WorkMode, mcs,
                                                   upto, work)

import           Test.RSCoin.Full.Context         (MintetteInfo, TestEnv, port,
                                                   secretKey, state)
import           Test.RSCoin.Full.Mintette.Config (MintetteConfig,
                                                   malfunctioningConfig)
import qualified Test.RSCoin.Full.Mintette.Server as FM

initialization
    :: (WorkMode m)
    => Maybe MintetteConfig -> Microsecond -> MintetteInfo -> TestEnv m ()
initialization conf l m = do
    let runner = case conf of
             Nothing -> M.serve
             Just s  -> FM.serve s
    work (upto l mcs) $
        runner <$> view port <*> view state <*> view secretKey $ m
    work (upto l mcs) $
        M.runWorker <$> view secretKey <*> view state $ m


defaultMintetteInit
    :: (WorkMode m)
    => Microsecond -> MintetteInfo -> TestEnv m ()
defaultMintetteInit = initialization Nothing

malfunctioningMintetteInit
    :: (WorkMode m)
    => Microsecond -> MintetteInfo -> TestEnv m ()
malfunctioningMintetteInit =
    initialization (Just malfunctioningConfig)
