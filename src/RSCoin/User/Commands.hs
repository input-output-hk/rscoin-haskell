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

import           Control.Lens           ((^.))
import           Control.Monad          (unless, void)
import           Control.Monad.Trans    (liftIO)
import           Data.Acid.Advanced     (query')
import           Data.Bifunctor         (bimap)
import           Data.Maybe             (fromJust, isJust)
import           Data.Monoid            ((<>))
import qualified Data.Text.IO           as TIO

import           Serokell.Util.Text     (format')

import qualified RSCoin.Core            as C
import           RSCoin.Timed           (WorkMode)
import           RSCoin.User.AcidState  (GetAllAddresses (..))
import qualified RSCoin.User.AcidState  as A
import           RSCoin.User.Error      (eWrap)
import           RSCoin.User.Operations (commitError, formTransaction,
                                         getAmount, updateBlockchain)
import qualified RSCoin.User.Wallet     as W


-- | Input user command that's contained in every program call
data UserCommand
    = ListAddresses                 -- ^ List all addresses in wallet,
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

-- | Given the state of program and command, makes correspondent
-- actions.
proceedCommand :: WorkMode m => A.RSCoinUserState -> UserCommand -> m ()
proceedCommand st ListAddresses =
    eWrap $
    do (wallets :: [(C.PublicKey, C.Coin)]) <-
           mapM (\w -> (w ^. W.publicAddress, ) <$> getAmount st w) =<<
           query' st GetAllAddresses
       liftIO $ do
           TIO.putStrLn "Here's the list of your accounts:"
           TIO.putStrLn "# | Public ID                                    | Amount"
           mapM_ (TIO.putStrLn . format' "{}.  {} : {}") $
               uncurry (zip3 [(1 :: Integer) ..]) $ unzip wallets
proceedCommand st (FormTransaction inputs outputAddrStr) =
    eWrap $
    do let pubKey = C.Address <$> C.constructPublicKey outputAddrStr
           inputs' = map (bimap fromIntegral C.Coin) inputs
       unless (isJust pubKey) $
           commitError $
           "Provided key can't be exported: " <> outputAddrStr
       formTransaction st inputs' (fromJust pubKey) $
           C.Coin (sum $ map snd inputs)
proceedCommand st UpdateBlockchain =
    eWrap $
    do res <- updateBlockchain st True
       C.logInfo C.userLoggerName $
           if res
               then "Blockchain is updated already."
               else "Successfully updated blockchain."
proceedCommand _ (Dump command) = eWrap $ dumpCommand command

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
