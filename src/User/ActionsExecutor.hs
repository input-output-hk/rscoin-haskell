-- | ActionsExecutor performs actions with RSCoinUserState.

module ActionsExecutor (runActionsExecutor) where

import           Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue)
import           Control.Monad                  (forM_, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.STM              (atomically)
import           Data.Acid                      (query)
import           Data.Acid.Advanced             (query')
import           Data.Int                       (Int64)
import           Data.Maybe                     (fromJust, isJust)

import           Graphics.UI.Gtk                (labelSetText, postGUIAsync)
import qualified Graphics.UI.Gtk                as G

import           Action                         (Action (..))
import           Error                          (handled)
import           OutputWidgets                  (OutputWidgets (..))
import           RSCoin.Core                    (Coin (..))
import           RSCoin.Test                    (WorkMode)
import qualified RSCoin.User                    as U

updateUI :: U.RSCoinUserState -> OutputWidgets -> IO ()
updateUI st ow = do
    a <- query st U.GetAllAddresses
    b <- sum <$> mapM (U.getAmount st) a
    postGUIAsync $ do
        labelSetText (balanceLabel ow) $ show $ getCoin b

selectAmounts :: Int64 -> [Int64] -> Maybe [(Int, Int64)]
selectAmounts t a = select t a 1
  where
    select :: Int64 -> [Int64] -> Int -> Maybe [(Int, Int64)]
    select _ [] _ = Nothing
    select n (x:xs) i
        | n <= 0    = Nothing
        | otherwise = if n <= x
                          then Just [(i, n)]
                          else (:) (i, x) <$> select (n - x) xs (i + 1)

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
            Send a c -> do
                handled queue o ow $ do
                    as <- query' st U.GetAllAddresses
                    cs <- liftIO $ mapM ((<$>) getCoin . U.getAmount st) as
                    let is = selectAmounts c cs
                    if isJust is
                        then U.formTransaction st (fromJust is) a $ Coin c
                        else liftIO $ postGUIAsync $ do
                            labelSetText (messageLabel ow) "Invalid amount"
                            G.widgetShowAll (notificationWindow ow)
                run
            Update     -> do
                handled queue o ow $ do
                    walletHeight    <- liftIO $ query st U.GetLastBlockId
                    lastBlockHeight <- pred <$> U.getBlockchainHeight
                    when (walletHeight < lastBlockHeight) $ do
                        forM_ [walletHeight + 1 .. lastBlockHeight] $
                            U.updateToBlockHeight st
                        liftIO $ updateUI st ow
                run
