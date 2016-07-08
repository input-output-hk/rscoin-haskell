-- | Convenience functions to launch explorer or do high-level operations
-- with it.

module RSCoin.Explorer.Launcher
       ( explorerWrapperReal
       , launchExplorerReal
       , launchExplorer
       ) where

import           Control.Monad.Catch       (bracket)
import           Control.Monad.Trans       (liftIO)
import           Data.ByteString           (ByteString)

import           RSCoin.Core               (SecretKey)
import           RSCoin.Timed              (MsgPackRpc, WorkMode, runRealMode)

import           RSCoin.Explorer.AcidState (State, closeState, openState)
import           RSCoin.Explorer.Server    (serve)

explorerWrapperReal :: ByteString -> FilePath -> (State -> MsgPackRpc a) -> IO a
explorerWrapperReal bankHost storagePath =
    runRealMode bankHost .
    bracket (liftIO $ openState storagePath) (liftIO . closeState)

launchExplorerReal :: ByteString -> Int -> FilePath -> SecretKey -> IO ()
launchExplorerReal bankHost port storagePath sk =
    explorerWrapperReal bankHost storagePath $ launchExplorer port sk

launchExplorer
    :: WorkMode m
    => Int -> SecretKey -> State -> m ()
launchExplorer port sk st = serve port st sk
