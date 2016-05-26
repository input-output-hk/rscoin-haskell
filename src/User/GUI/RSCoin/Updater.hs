-- | Regularly updates the state of the wallet.

module GUI.RSCoin.Updater (runUpdater) where

import           Control.Concurrent             (threadDelay)
import           Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import           Control.Monad.STM              (atomically)

import           GUI.RSCoin.Action              (Action (Update))

-- | Regularly requests to perform Update action.
runUpdater :: TBQueue Action -> IO ()
runUpdater queue = do
    atomically $ writeTBQueue queue Update
    threadDelay 30000000
    runUpdater queue
