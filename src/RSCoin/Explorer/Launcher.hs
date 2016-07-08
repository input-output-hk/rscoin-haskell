-- | Convenience functions to launch explorer or do high-level operations
-- with it.

module RSCoin.Explorer.Launcher
       ( explorerWrapperReal
       , launchExplorerReal
       , launchExplorer
       ) where

import           Control.Monad.Catch       (bracket)
import           Control.Monad.Trans       (liftIO)

import           RSCoin.Core               (SecretKey)
import           RSCoin.Timed              (MsgPackRpc, WorkMode,
                                            runRealModeLocal)

import           RSCoin.Explorer.AcidState (State, closeState, openState)
import           RSCoin.Explorer.Server    (serve)

explorerWrapperReal :: FilePath -> (State -> MsgPackRpc a) -> IO a
explorerWrapperReal storagePath =
    runRealModeLocal .
    bracket (liftIO $ openState storagePath) (liftIO . closeState)

launchExplorerReal :: Int -> FilePath -> SecretKey -> IO ()
launchExplorerReal port storagePath sk =
    explorerWrapperReal storagePath $ launchExplorer port sk

launchExplorer
    :: WorkMode m
    => Int -> SecretKey -> State -> m ()
launchExplorer port sk st = serve port st sk
