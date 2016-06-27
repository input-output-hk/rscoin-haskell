-- | This module describes possible variants of mintette behavior
-- (normal, with errors, malicious)

module Test.RSCoin.Full.Mintette
       ( defaultMintetteInit
       , malfunctioningMintetteInit
       ) where

import           Control.Lens                     (view)

import qualified RSCoin.Mintette                  as M
import           RSCoin.Timed                     (WorkMode, mcs, upto, work)

import           Test.RSCoin.Full.Context         (MintetteInfo, port,
                                                   secretKey, state)
import           Test.RSCoin.Full.Mintette.Config (MintetteConfig,
                                                   malfunctioningConfig)
import qualified Test.RSCoin.Full.Mintette.Server as FM

initialization
    :: (WorkMode m)
    => Maybe MintetteConfig -> MintetteInfo -> m ()
initialization conf m = do
    let runner = case conf of
             Nothing -> M.serve
             Just s  -> FM.serve s
    work (upto 0 mcs) $
        runner <$> view port <*> view state <*> view secretKey $ m
    work (upto 0 mcs) $
        M.runWorker <$> view secretKey <*> view state $ m


defaultMintetteInit
    :: (WorkMode m)
    => MintetteInfo -> m ()
defaultMintetteInit = initialization Nothing

malfunctioningMintetteInit
    :: (WorkMode m)
    => MintetteInfo -> m ()
malfunctioningMintetteInit =
    initialization (Just malfunctioningConfig)
