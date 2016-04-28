{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types#-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | Protocol implements all low level communication between
-- entities (User, Bank, Mintette).

module RSCoin.Core.Protocol
       ( BankMethod (..)
       , MintetteMethod (..)
       , DumpMethod (..)
       , RSCoinMethod (..)
       , WithResult
       , Server
       , T.Client
       , method
       , T.serve
       , call
       , execBank
       , execMintette
       , callBank
       , callMintette
       , unCps
       ) where

import           Control.Monad.IO.Class     (liftIO, MonadIO)

import           Data.IORef                 (newIORef, writeIORef, readIORef)
import qualified Data.ByteString.Char8      as BS
import           Data.Maybe                 (fromJust)

import           Data.MessagePack           (MessagePack)
-- import qualified Network.MessagePack.Server as S
-- import qualified Network.MessagePack.Client as C

import           RSCoin.Core.Constants      (bankHost, bankPort)
import           RSCoin.Core.Types          (Mintette (..))
import           RSCoin.Core.Crypto         ()
import           RSCoin.Core.MessagePack          ()
import qualified RSCoin.Test                as T

-- TODO: this module should provide more safety and expose better api

-- | Requests used in RSCoin transport layer.
data RSCoinMethod
    = RSCBank BankMethod
    | RSCMintette MintetteMethod
    | RSCDump DumpMethod
    deriving (Show)

-- | Requests processed by a Bank.
data BankMethod
    = GetMintettes
    | GetBlockchainHeight
    | GetHBlock
    deriving (Show)

-- | Requests processed by a Mintette.
data MintetteMethod
    = PeriodFinished
    | AnnounceNewPeriod
    | CheckTx
    | CommitTx
    deriving (Show)

-- Requests for dumping state
data DumpMethod
    = GetHBlocks
    | GetLogs
    | GetMintetteUtxo
    | GetMintetteBlocks
    | GetMintetteLogs
    deriving (Show)

type Server a = T.Server a

-- | Create server method.
method :: T.MethodType m f => RSCoinMethod -> f -> T.Method m
method m = T.method (show m)

--call :: RpcType a => RSCoinMethod -> a
-- FIXME: RpcType isn't exported so my idea of using Show RSCoinMethod for method name
-- doesn't hold any more
-- | Call RSCoinMethod.
call m = T.call (show m)

-- TODO: this can be modeled with Cont monad
-- | Continuation passing style transformation.
-- For more see: https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
type WithResult a = forall m . T.WorkMode m => (a -> m ()) -> m ()

-- | Send a request to a Bank using Continuation passing style (CPS).
execBank :: MessagePack a => T.Client a -> WithResult a
execBank = (>>=) . callBank

-- | Send a request to a Mintette using Continuation passing style (CPS).
execMintette :: MessagePack a => Mintette -> T.Client a -> WithResult a
execMintette m = (>>=) . callMintette m
            

-- | Send a request to a Bank.
callBank :: (MessagePack a, T.WorkMode m) => T.Client a -> m a
callBank action = 
    T.execClient (BS.pack bankHost, bankPort) action 

-- | Send a request to a Mintette.
callMintette :: (MessagePack a, T.WorkMode m) 
             => Mintette -> T.Client a -> m a
callMintette Mintette {..} action = 
    T.execClient (BS.pack mintetteHost, mintettePort) action 

-- | Reverse Continuation passing style (CPS) transformation
unCps :: forall a m . MonadIO m => ((a -> m ()) -> m ()) -> m a
unCps withResult = do
    ref <- liftIO $ newIORef Nothing
    withResult $ liftIO . writeIORef ref . Just
    fromJust <$> liftIO (readIORef ref)
