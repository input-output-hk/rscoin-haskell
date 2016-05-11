{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Module that provides some functions that transform
-- UserOptions.UserCommand s to IO actions.

module RSCoin.User.Commands 
        ( UserCommand (..)
        , DumpCommand (..)  
        , proceedCommand
        ) where

import           Data.Int               (Int64)
import           Data.Text              (Text)

import           Control.Concurrent             (forkIO)
import           Control.Concurrent.STM.TBQueue (newTBQueueIO)
import           Control.Lens                   ((^.))
import           Control.Monad                  (forM_, unless, void, when)
import           Control.Monad.Catch            (bracket, throwM)
import           Control.Monad.Trans            (liftIO)
import qualified Data.Acid                      as S
import           Data.Maybe                     (fromJust, isJust)
import           Data.Monoid                    ((<>))
import qualified Data.Text.IO                   as TIO

import           Serokell.Util.Text             (format', formatSingle')

import           RSCoin.User.ActionsExecutor    (runActionsExecutor)
import           RSCoin.User.Contacts           (ContactsList (..))
import           RSCoin.User.GUI                (runGUI)
import           RSCoin.Core                    as C
import           RSCoin.Timed                   (WorkMode)
import           RSCoin.User.AcidState          (GetAllAddresses (..))
import qualified RSCoin.User.AcidState          as A
import           RSCoin.User.Error              (UserError (..), eWrap)
import qualified RSCoin.User.Operations         as P
import qualified RSCoin.User.Wallet             as W
import           RSCoin.User.Updater            (runUpdater)

-- | Input user command that's contained in every program call
data UserCommand
    = StartGUI                      -- ^ Start graphical user interface
    | ListAddresses                 -- ^ List all addresses in wallet,
                                    -- starting with 1
    | UpdateBlockchain              -- ^ Query bank to update wallet
                                    -- state according to blockchain
                                    -- status
    | FormTransaction [(Int, Int64)]
                      Text          -- ^ First argument represents
                                    -- inputs -- pairs (a,b), where a
                                    -- is index (starting from 1) of
                                    -- address in wallet, b is
                                    -- positive integer representing
                                    -- value to send.  Second argument
                                    -- represents the address to send,
                                    -- and amount
    | Dump DumpCommand
    deriving (Show)

data DumpCommand
    = DumpHBlocks C.PeriodId C.PeriodId
    | DumpHBlock C.PeriodId
    | DumpMintettes
    | DumpPeriod
    | DumpLogs C.MintetteId Int Int
    | DumpMintetteUtxo C.MintetteId
    | DumpMintetteBlocks C.MintetteId C.PeriodId
    | DumpMintetteLogs C.MintetteId C.PeriodId
    deriving (Show)

actionsQueueCapacity :: Int
actionsQueueCapacity = 10

-- | Given the state of program and command, makes correspondent
-- actions.
proceedCommand
    :: WorkMode m => A.RSCoinUserState -> UserCommand -> FilePath -> m ()
proceedCommand st StartGUI contactsPath =
    eWrap $
    do
        bracket
            (liftIO $ S.openLocalStateFrom contactsPath $ ContactsList [])
            (\cs -> liftIO $ do
                S.createCheckpoint cs
                S.closeAcidState cs)
            (\cs -> do
                queue <- liftIO $ newTBQueueIO actionsQueueCapacity
                ow <- liftIO $ runGUI queue st cs
                liftIO $ void $ forkIO $ runUpdater queue
                runActionsExecutor st queue ow)
proceedCommand st ListAddresses _ =
    liftIO $ eWrap $
    do (wallets :: [(C.PublicKey, C.Coin)]) <-
           mapM (\w -> (w ^. W.publicAddress, ) <$> P.getAmount st w) =<<
           S.query st GetAllAddresses
       TIO.putStrLn "Here's the list of your accounts:"
       TIO.putStrLn "# | Public ID                                    | Amount"
       mapM_ (TIO.putStrLn . format' "{}.  {} : {}") $
           uncurry (zip3 [(1 :: Integer) ..]) $ unzip wallets
proceedCommand st (FormTransaction inputs outputAddrStr) _ =
    eWrap $
    do let pubKey = C.Address <$> C.constructPublicKey outputAddrStr
       unless (isJust pubKey) $
           P.commitError $
           "Provided key can't be exported: " <> outputAddrStr
       P.formTransaction st inputs (fromJust pubKey) $
           C.Coin (sum $ map snd inputs)
proceedCommand st UpdateBlockchain _ =
    eWrap $
    do walletHeight <- liftIO $ S.query st A.GetLastBlockId
       liftIO $ TIO.putStrLn $
           formatSingle'
               "Current known blockchain's height (last HBLock's id) is {}."
               walletHeight
       lastBlockHeight <- pred <$> C.getBlockchainHeight
       when (walletHeight > lastBlockHeight) $
           throwM $
           StorageError $
           W.InternalError $
           format'
               ("Last block height in wallet ({}) is greater than last " <>
                "block's height in bank ({}). Critical error.")
               (walletHeight, lastBlockHeight)
       if lastBlockHeight == walletHeight
           then liftIO $ putStrLn "Blockchain is updated already."
           else do
               forM_
                  [walletHeight + 1 .. lastBlockHeight]
                  (\h -> do
                        liftIO $ TIO.putStr $
                            formatSingle' "Updating to height {} ..." h
                        P.updateToBlockHeight st h
                        liftIO $ TIO.putStrLn $
                            formatSingle' "updated to height {}" h)
               liftIO $ TIO.putStrLn "Successfully updated blockchain!"
proceedCommand _ (Dump command) _ = eWrap $ dumpCommand command

dumpCommand :: WorkMode m => DumpCommand -> m ()
dumpCommand (DumpHBlocks from to) =
    void $ C.getBlocks from to
dumpCommand (DumpHBlock pId) =
    void $ C.getBlockByHeight pId
dumpCommand DumpMintettes =
    void $ C.getMintettes
dumpCommand DumpPeriod =
    void $ C.getBlockchainHeight
dumpCommand (DumpLogs mId from to) =
    void $ C.getLogs mId from to
dumpCommand (DumpMintetteUtxo mId) =
    void $ C.getMintetteUtxo mId
dumpCommand (DumpMintetteBlocks mId pId) =
    void $ C.getMintetteBlocks mId pId
dumpCommand (DumpMintetteLogs mId pId) =
    void $ C.getMintetteLogs mId pId
