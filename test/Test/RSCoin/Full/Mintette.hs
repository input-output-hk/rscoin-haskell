-- | This module describes possible variants of mintette behavior
-- (normal, with errors, malicious)

module Test.RSCoin.Full.Mintette
       ( defaultMintetteInit
       , malfunctioningMintetteInit
       ) where

import           Control.Concurrent.MVar          (MVar)
import           Control.Lens                     (view, (^.))

import qualified RSCoin.Core                      as C
import qualified RSCoin.Mintette                  as M

import           Test.RSCoin.Full.Context         (MintetteInfo, port,
                                                   secretKey, state)
import           Test.RSCoin.Full.Mintette.Config (MintetteConfig,
                                                   malfunctioningConfig)
import qualified Test.RSCoin.Full.Mintette.Server as FM

initialization
    :: C.WorkMode m
    => Maybe MintetteConfig -> (m () -> m ()) -> MintetteInfo -> m ()
initialization conf forkTmp m = do
    let runner
            :: C.WorkMode m
            => Int -> M.State -> M.RuntimeEnv -> m ()
        env = M.mkRuntimeEnv 100000 (m ^. secretKey)
        runner =
            case conf of
                Nothing -> M.serve
                Just s  -> FM.serve s
    forkTmp $ runner <$> view port <*> view state <*> pure env $ m
    forkTmp $ M.runWorker env <$> view state $ m

defaultMintetteInit
    :: C.WorkMode m
    => (m () -> m ()) -> MintetteInfo -> m ()
defaultMintetteInit = initialization Nothing

malfunctioningMintetteInit
    :: C.WorkMode m
    => (m () -> m ()) -> MintetteInfo -> m ()
malfunctioningMintetteInit =
    initialization (Just malfunctioningConfig)
