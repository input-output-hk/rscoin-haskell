{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad          (when)
import           Control.Monad.Catch    (bracket)
import           Control.Monad.Trans    (liftIO)
import qualified Data.Acid              as ACID
import qualified Data.Text              as T

import           RSCoin.User.Commands   (proceedCommand)
import           RSCoin.Core            (initLogging, logDebug)
import qualified RSCoin.User.AcidState  as A
import           RSCoin.User.Operations (walletInitialized)
import           RSCoin.Timed           (runRealMode)
import qualified UserOptions            as O

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
                    i <- liftIO $ walletInitialized st
                    when (not i) $ do
                        A.initState st addressesNum $
                            bankKeyPath isBankMode bankModePath
                    proceedCommand st userCommand contactsPath
  where
    bankKeyPath True  p = Just p
    bankKeyPath False _ = Nothing
