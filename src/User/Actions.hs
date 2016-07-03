{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Module that provides description of what user node can do and
-- functions that runs chosen action.

module Actions
       ( processCommand
       , initializeStorage
       ) where

import           Control.Exception       (SomeException)
import           Control.Monad           (forM_, unless, void, when)
import           Control.Monad.Catch     (bracket, catch)
import           Control.Monad.Trans     (liftIO)
import qualified Data.Acid               as ACID
import           Data.Acid.Advanced      (query')
import           Data.Function           (on)
import           Data.List               (genericIndex, groupBy, nub)
import           Data.Maybe              (fromJust, fromMaybe, isJust)
import           Data.Monoid             ((<>))
import qualified Data.Text.IO            as TIO
import qualified Graphics.UI.Gtk         as G

import           Serokell.Util.Text      (format', formatSingle', show')

import           RSCoin.Core             as C
import           RSCoin.Timed            (WorkMode, for, ms, wait)
import qualified RSCoin.User.AcidState   as A
import           RSCoin.User.Error       (eWrap)
import           RSCoin.User.Operations  (TransactionData (..), getAmount,
                                          submitTransactionRetry,
                                          updateBlockchain)
import qualified RSCoin.User.Operations  as P

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
processCommand st O.ListAddresses _ =
    eWrap $
    do -- get addresses default strategy first
       addresses <- nub <$> ((++) <$> query' st A.GetOwnedDefaultAddresses
                                  <*> query' st A.GetOwnedAddresses)
       (wallets :: [(C.PublicKey, C.Strategy, [C.Coin])]) <-
           mapM (\addr -> do
                      coins <- C.coinsToList <$> getAmount st addr
                      strategy <- query' st $ A.GetAddressStrategy addr
                      return ( C.getAddress addr
                             , fromMaybe DefaultStrategy strategy
                             , coins))
                addresses
       liftIO $
           do TIO.putStrLn "Here's the list of your accounts:"
              TIO.putStrLn
                  "# | Public ID                                    | Amount"
              mapM_ formatAddressEntry $ ([(1 :: Integer) ..] `zip` wallets)
  where
    spaces = "                                                   "
    formatAddressEntry :: (Integer, (C.PublicKey, C.Strategy, [C.Coin])) -> IO ()
    formatAddressEntry (i, (key, strategy, coins)) = do
       TIO.putStr $ format' "{}.  {} : " (i, key)
       when (null coins) $ putStrLn "empty"
       unless (null coins) $ TIO.putStrLn $ show' $ head coins
       unless (length coins < 2) $
           forM_ (tail coins)
                 (TIO.putStrLn . formatSingle' (spaces <> "{}"))
       case strategy of
           DefaultStrategy -> return ()
           MOfNStrategy m allowed -> do
               TIO.putStrLn $ format'
                    "     This is a multisig address ({}/{}) controlled by keys: "
                    (m, length allowed)
               forM_ allowed $ \allowedAddr -> do
                   addresses <- nub <$> ((++) <$> query' st A.GetOwnedDefaultAddresses
                                              <*> query' st A.GetOwnedAddresses)
                   TIO.putStrLn $ formatSingle'
                           (if allowedAddr `elem` addresses
                            then "     * {} owned by you"
                            else "     * {}")
                           allowedAddr
processCommand st (O.FormTransaction inputs outputAddrStr outputCoins cache) _ =
    eWrap $
    do let outputAddr = C.Address <$> C.constructPublicKey outputAddrStr
           inputs' = map (foldr1 (\(a,b) (_,d) -> (a, b ++ d))) $
                     groupBy ((==) `on` snd) $
                     map (\(idx,o,c) -> (idx - 1, [Coin c (toRational o)]))
                     inputs
           outputs' = map (\(amount,color) -> Coin color (toRational amount))
                          outputCoins
           td = TransactionData
                { tdInputs = inputs'
                , tdOutputAddress = fromJust outputAddr
                , tdOutputCoins = outputs'
                }
       unless (isJust outputAddr) $
           P.commitError $ "Provided key can't be exported: " <> outputAddrStr
       void $ submitTransactionRetry 2 st cache td
processCommand st O.UpdateBlockchain _ =
    eWrap $
    do res <- updateBlockchain st True
       C.logInfo C.userLoggerName $
           if res
               then "Blockchain is updated already."
               else "Successfully updated blockchain."
processCommand st (O.Dump command) _ = eWrap $ dumpCommand st command
processCommand st O.StartGUI opts@O.UserOptions{..} = do
    initialized <- query' st A.IsInitialized
    unless initialized $ liftIO G.initGUI >> initLoop
    liftIO $ bracket
        (ACID.openLocalStateFrom guidbPath emptyGUIAcid)
        (\cs -> do ACID.createCheckpoint cs
                   ACID.closeAcidState cs)
        (\cs -> startGUI st cs)
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
dumpCommand _ O.DumpAddresses = void C.getAddresses
dumpCommand _ O.DumpPeriod = void C.getBlockchainHeight
dumpCommand _ (O.DumpHBlocks from to) = void $ C.getBlocks from to
dumpCommand _ (O.DumpHBlock pId) = void $ C.getBlockByHeight pId
dumpCommand _ (O.DumpLogs mId from to) = void $ C.getLogs mId from to
dumpCommand _ (O.DumpMintetteUtxo mId) = void $ C.getMintetteUtxo mId
dumpCommand _ (O.DumpMintetteBlocks mId pId) =
    void $ C.getMintetteBlocks mId pId
dumpCommand _ (O.DumpMintetteLogs mId pId) = void $ C.getMintetteLogs mId pId
dumpCommand st (O.DumpAddress idx) =
    liftIO . TIO.putStrLn . show' . (`genericIndex` (idx - 1)) =<<
    query' st A.GetOwnedAddresses
