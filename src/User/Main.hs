{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad.Catch   (MonadCatch, catch, throwM, bracket)
import           Control.Monad.Trans   (MonadIO, liftIO)
import qualified Data.Acid             as ACID
import qualified Data.Text             as T

import           RSCoin.User.Commands  (proceedCommand)
import           RSCoin.Core           (initLogging, logDebug)
import qualified RSCoin.User.AcidState as A
import           RSCoin.Timed          (runRealMode)
import qualified RSCoin.User.Wallet    as W
import qualified UserOptions           as O

main :: IO ()
main = do
    opts@O.UserOptions{..} <- O.getUserOptions
    initLogging logSeverity
    runRealMode $  
        bracket
            (liftIO $ A.openState walletPath)
            (\st -> liftIO $ do
                ACID.createCheckpoint st
                A.closeState st) $
            \st ->
                 do liftIO $ logDebug $
                        mconcat ["Called with options: ", (T.pack . show) opts]
                    handleUninitialized
                        (proceedCommand st userCommand)
                        (A.initState
                            st
                            addressesNum
                            (bankKeyPath isBankMode bankModePath))

  where
    handleUninitialized :: (MonadIO m, MonadCatch m) => m () -> m () -> m ()
    handleUninitialized action initialize =
        action `catch` handler initialize action
    handler i a W.NotInitialized =
        liftIO (putStrLn "Initializing storage..") >> i >> a
    handler _ _ e = throwM e
    bankKeyPath True p = Just p
    bankKeyPath False _ = Nothing
