-- | ActionsExecutor performs actions with RSCoinUserState.

module ActionsExecutor
    ( Action (..)
    , runActionsExecutor
    ) where

import           Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue)
import           Control.Lens                   ((^.))
import           Control.Monad                  (forM_, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.STM              (atomically)
import           Data.Acid                      (query)
import           Data.Int                       (Int64)

import           Graphics.UI.Gtk                (labelSetText, postGUIAsync)

import           OutputWidgets                  (OutputWidgets (..))
import           RSCoin.Core                    (Address, Coin, getCoin)
import           RSCoin.Test                    (WorkMode)
import qualified RSCoin.User                    as U

-- | Action to be performed by ActionExecutor
data Action = Exit
            | Send [(Int, Int64)] Address Coin
            | Update

updateUI :: U.RSCoinUserState -> OutputWidgets -> IO ()
updateUI st ow = do
    as <- query st U.GetAllAddresses
    let a = head as
    amount <- U.getAmount st a
    postGUIAsync $ do
        labelSetText (balanceLabel ow) $ show $ getCoin amount
        labelSetText (myWalletLabel ow) $ show $ a ^. U.publicAddress

-- | Runs ActionExecutor
runActionsExecutor ::
    WorkMode m => U.RSCoinUserState -> TBQueue Action -> OutputWidgets -> m ()
runActionsExecutor st queue ow = run
  where
    run :: WorkMode m => m ()
    run = do
        o <- liftIO $ atomically $ readTBQueue queue
        case o of
            Exit       -> return ()
            Send i a c -> U.formTransaction st i a c >> run
            Update     -> do
                walletHeight    <- liftIO $ query st U.GetLastBlockId
                lastBlockHeight <- pred <$> U.getBlockchainHeight
                when (walletHeight < lastBlockHeight) $ do
                    forM_ [walletHeight + 1 .. lastBlockHeight] $
                        U.updateToBlockHeight st
                    liftIO $ updateUI st ow
                run
