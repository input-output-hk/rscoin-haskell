{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Concurrent      (threadDelay)
import           Control.Exception       (SomeException)
import           Control.Monad           (unless, void)
import           Control.Monad.Catch     (MonadCatch, bracket, catch,
                                          throwM)
import           Control.Monad.Trans     (MonadIO, liftIO)
import qualified Data.Acid               as ACID
import           Data.Acid.Advanced      (query')
import qualified Data.Text               as T
import qualified Graphics.UI.Gtk         as G

import qualified RSCoin.Core             as C
import           RSCoin.Timed            (WorkMode, runRealMode)
import qualified RSCoin.User.AcidState   as A
import qualified RSCoin.User.Actions     as UA
import           RSCoin.User.Error       (eWrap)
import qualified RSCoin.User.Wallet      as W

import           GUI.RSCoin.ErrorMessage (reportSimpleErrorNoWindow)
import           GUI.RSCoin.GUI          (startGUI)
import           GUI.RSCoin.GUIAcid      (emptyGUIAcid)
import qualified UserOptions             as O

initializeStorage
    :: forall (m :: * -> *).
       (WorkMode m)
    => A.RSCoinUserState
    -> O.UserOptions
    -> m ()
initializeStorage st O.UserOptions{..} =
    A.initState st addressesNum $ bankKeyPath isBankMode bankModePath
  where
    bankKeyPath True p = Just p
    bankKeyPath False _ = Nothing

-- | Processes command line user command
processCommand
    :: WorkMode m
    => A.RSCoinUserState -> O.UserCommand -> O.UserOptions -> m ()
processCommand st O.ListAddresses _ = UA.processAction st UA.ListAddresses
processCommand st (O.FormTransaction i o) _ = UA.processAction st $ UA.FormTransaction i o
processCommand st O.UpdateBlockchain _ = UA.processAction st UA.UpdateBlockchain
processCommand _ (O.Dump command) _ = eWrap $ dumpCommand command
processCommand st O.StartGUI opts@O.UserOptions{..} = do
    initialized <- query' st A.IsInitialized
    unless initialized $ liftIO G.initGUI >> initLoop
    liftIO $ bracket
        (ACID.openLocalStateFrom guidbPath emptyGUIAcid)
        (\cs -> do ACID.createCheckpoint cs
                   ACID.closeAcidState cs)
        (\cs -> do
          startGUI st cs)
  where
    initLoop =
        initializeStorage st opts `catch`
        (\(e :: SomeException) ->
              do liftIO $
                     reportSimpleErrorNoWindow $
                     "Couldn't initialize rscoin. Check connection, close this " ++
                     "dialog and we'll try again. Error: "
                     ++ show e
                 liftIO $ threadDelay $ 500 * 1000 -- wait for 0.5 sec
                 initLoop)

dumpCommand :: WorkMode m => O.DumpCommand -> m ()
dumpCommand O.DumpMintettes                = void   C.getMintettes
dumpCommand O.DumpPeriod                   = void   C.getBlockchainHeight
dumpCommand (O.DumpHBlocks from to)        = void $ C.getBlocks from to
dumpCommand (O.DumpHBlock pId)             = void $ C.getBlockByHeight pId
dumpCommand (O.DumpLogs mId from to)       = void $ C.getLogs mId from to
dumpCommand (O.DumpMintetteUtxo mId)       = void $ C.getMintetteUtxo mId
dumpCommand (O.DumpMintetteBlocks mId pId) = void $ C.getMintetteBlocks mId pId
dumpCommand (O.DumpMintetteLogs mId pId)   = void $ C.getMintetteLogs mId pId

main :: IO ()
main = do
    opts@O.UserOptions{..} <- O.getUserOptions
    C.initLogging logSeverity
    runRealMode bankHost $
        bracket
            (liftIO $ A.openState walletPath)
            (\st -> liftIO $ do
                ACID.createCheckpoint st
                A.closeState st) $
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
