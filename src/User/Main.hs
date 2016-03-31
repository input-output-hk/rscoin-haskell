{-# LANGUAGE FlexibleContexts #-}
import           Control.Exception     (bracket, catch, throwIO)
import qualified Data.Acid             as ACID
import qualified Data.Text             as T

import           Actions               (proceedCommand)
import           RSCoin.Core           (initLogging, logDebug)
import qualified RSCoin.User.AcidState as A
import qualified RSCoin.User.Wallet    as W
import qualified UserOptions           as O

main :: IO ()
main = do
    opts@O.UserOptions{..} <- O.getUserOptions
    initLogging logSeverity
    bracket
        (A.openState walletPath)
        (\st ->
              ACID.createCheckpoint st >> A.closeState st) $
        \st ->
             do logDebug $
                    mconcat ["Called with options: ", (T.pack . show) opts]
                handleUninitialized
                    (proceedCommand st userCommand)
                    (A.initState
                         st
                         addressesNum
                         (bankKeyPath isBankMode bankModePath))
  where
    handleUninitialized :: IO () -> IO () -> IO ()
    handleUninitialized action initialize =
        action `catch` handler initialize action
    handler i a W.NotInitialized =
        putStrLn "Initializing storage.." >> i >> a
    handler _ _ e = throwIO e
    bankKeyPath True p = Just p
    bankKeyPath False _ = Nothing
