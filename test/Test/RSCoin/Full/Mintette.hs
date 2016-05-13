-- | This module describes possible variants of mintette behavior
-- (normal, with errors, malicious)
module Test.RSCoin.Full.Mintette
       ( defaultMintetteInit
       , malfunctioningMintetteInit
       ) where

import           Control.Lens             (view)

import qualified RSCoin.Mintette          as M
import           RSCoin.Timed             (Microsecond, WorkMode, mcs, upto,
                                           work)

import           Test.RSCoin.Full.Context (MintetteInfo, TestEnv, port,
                                           secretKey, state)

defaultMintetteInit
    :: (WorkMode m)
    => Microsecond -> MintetteInfo -> TestEnv m ()
defaultMintetteInit l m = do
    work (upto l mcs) $
        M.serve <$> view port <*> view state <*> view secretKey $ m
    work (upto l mcs) $
        M.runWorker <$> view secretKey <*> view state $ m

malfunctioningMintetteInit
    :: (WorkMode m)
    => Microsecond -> MintetteInfo -> TestEnv m ()
malfunctioningMintetteInit l m = undefined l m
