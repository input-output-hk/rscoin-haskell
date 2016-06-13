{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Module that provides description of what user node can do and
-- functions that runs chosen action.

module Actions
       ( processCommand
       , initializeStorage
       ) where

import           Control.Concurrent      (threadDelay)
import           Control.Exception       (SomeException)
import           Control.Lens            ((^.))
import           Control.Monad           (forM_, unless, void, when)
import           Control.Monad.Catch     (bracket, catch)
import           Control.Monad.Trans     (liftIO)
import qualified Data.Acid               as ACID
import           Data.Acid.Advanced      (query')
import           Data.Bifunctor          (bimap)
import qualified Data.Map                as M
import           Data.Maybe              (fromJust, isJust)
import           Data.Monoid             ((<>))
import qualified Data.Text.IO            as TIO
import           Data.Tuple.Select       (sel1)
import qualified Graphics.UI.Gtk         as G

import           Serokell.Util.Text      (format', formatSingle', show')

import           RSCoin.Core             as C
import           RSCoin.Timed            (WorkMode)
import           RSCoin.User.AcidState   (GetAllAddresses (..))
import qualified RSCoin.User.AcidState   as A
import           RSCoin.User.Error       (eWrap)
import           RSCoin.User.Operations  (formTransactionRetry, getAmount,
                                          updateBlockchain)
import qualified RSCoin.User.Operations  as P
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
processCommand st O.ListAddresses _ =
    eWrap $
    do addresses <- query' st GetAllAddresses
       (wallets :: [(C.PublicKey, [C.Coin])]) <-
           mapM (\w -> (w ^. W.publicAddress, ) . C.coinsToList
                       <$> getAmount st w) addresses
       liftIO $
           do TIO.putStrLn "Here's the list of your accounts:"
              TIO.putStrLn
                  "# | Public ID                                    | Amount"
              mapM_ formatAddressEntry $
                  uncurry (zip3 [(1 :: Integer) ..]) $ unzip wallets
  where
    formatAddressEntry :: (Integer, C.PublicKey, [C.Coin]) -> IO ()
    formatAddressEntry (i, key, coins) = do
       TIO.putStr $ format' "{}.  {} : " (i, key)
       when (null coins) $ putStrLn "empty"
       unless (null coins) $ TIO.putStrLn $ show' $ head coins
       unless (length coins < 2) $
           forM_ (tail coins)
                 (TIO.putStrLn . formatSingle' "                    {}")
processCommand st (O.FormTransaction inputs outputAddrStr outputCoins) _ =
    eWrap $
    do let pubKey = C.Address <$> C.constructPublicKey outputAddrStr
           inputs' = map (\(i, o, c) -> (fromIntegral i, fromIntegral o, c)) inputs
           outputs' = map (\(amount, color) -> Coin color (toRational amount)) outputCoins
       unless (isJust pubKey) $
           P.commitError $ "Provided key can't be exported: " <> outputAddrStr
       void $
           formTransactionRetry 2 st True inputs' (fromJust pubKey) $
           fromIntegral . sum $ map sel1 inputs
processCommand st O.UpdateBlockchain _ =
    eWrap $
    do res <- updateBlockchain st True
       C.logInfo C.userLoggerName $
           if res
               then "Blockchain is updated already."
               else "Successfully updated blockchain."
processCommand _ (O.Dump command) _ = eWrap $ dumpCommand command
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
