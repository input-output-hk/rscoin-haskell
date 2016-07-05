{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad.Catch (MonadCatch, bracket, catch, throwM)
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Acid           as ACID
import qualified Data.Text           as T

import qualified RSCoin.Core         as C
import           RSCoin.Timed        (runRealMode)
import qualified RSCoin.User         as U
import qualified RSCoin.User.Wallet  as W

import           Actions             (initializeStorage, processCommand)
import qualified UserOptions         as O

main :: IO ()
main = do
    opts@O.UserOptions{..} <- O.getUserOptions
    C.initLogging logSeverity
    runRealMode bankHost $
        bracket
            (liftIO $ U.openState walletPath)
            (\st -> liftIO $ do
                ACID.createCheckpoint st
                U.closeState st) $
            \st ->
                 do C.logDebug C.userLoggerName $
                        mconcat ["Called with options: ", (T.pack . show) opts]
                    handleUnitialized
                        (processCommand st userCommand opts)
                        (initializeStorage st opts)
  where
    handleUnitialized :: (MonadIO m, MonadCatch m) => m () -> m () -> m ()
    handleUnitialized action initialization =
        action `catch` handler initialization action
      where
        handler i a W.NotInitialized =
            C.logInfo C.userLoggerName "Initalizing storage..." >> i >> a
        handler _ _ e = throwM e
