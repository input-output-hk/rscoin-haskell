{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception       (SomeException)
import           Control.Monad           (unless, void)
import           Control.Monad.Catch     (MonadCatch, bracket, catch, throwM)
import           Control.Monad.Trans     (MonadIO, liftIO)
import qualified Data.Acid               as ACID
import           Data.Acid.Advanced      (query')
import           Data.List               (genericIndex)
import qualified Data.Text               as T
import qualified Data.Text.IO            as TIO
import qualified Graphics.UI.Gtk         as G

import           Serokell.Util           (show')

import qualified RSCoin.Core             as C
import           RSCoin.Timed            (WorkMode, for, ms, runRealMode, wait)
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
processCommand st (O.FormTransaction i o) _ =
    UA.processAction st $ UA.FormTransaction i o Nothing
processCommand st O.UpdateBlockchain _ = UA.processAction st UA.UpdateBlockchain
processCommand st (O.Dump command) _ = eWrap $ dumpCommand st command
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
                 wait $ for 500 ms
                 initLoop)

dumpCommand
    :: WorkMode m
    => A.RSCoinUserState -> O.DumpCommand -> m ()
dumpCommand _ O.DumpMintettes = void C.getMintettes
dumpCommand _ O.DumpPeriod = void C.getBlockchainHeight
dumpCommand _ (O.DumpHBlocks from to) = void $ C.getBlocks from to
dumpCommand _ (O.DumpHBlock pId) = void $ C.getBlockByHeight pId
dumpCommand _ (O.DumpLogs mId from to) = void $ C.getLogs mId from to
dumpCommand _ (O.DumpMintetteUtxo mId) = void $ C.getMintetteUtxo mId
dumpCommand _ (O.DumpMintetteBlocks mId pId) =
    void $ C.getMintetteBlocks mId pId
dumpCommand _ (O.DumpMintetteLogs mId pId) = void $ C.getMintetteLogs mId pId
dumpCommand st (O.DumpAddress idx) =
    liftIO . TIO.putStrLn . show' . (`genericIndex` idx) =<< query' st A.GetPublicAddresses

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
