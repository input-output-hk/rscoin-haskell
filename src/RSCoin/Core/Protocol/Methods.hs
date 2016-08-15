{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | This module defines protocol methods to communicate with
-- different node types (User, Bank, Mintette, Explorer).
module RSCoin.Core.Protocol.Methods
       ( WithResult
       , Server
       , T.Client
       , method
       , T.serve
       , call
       , execBank
       , execBankSafe
       , execExplorer
       , execExplorerSafe
       , execMintette
       , execMintetteSafe
       , execNotary
       , callBank
       , callBankSafe
       , callExplorer
       , callExplorerSafe
       , callMintette
       , callMintetteSafe
       , callNotary
       , unCps) where

import           Control.Monad.IO.Class     (MonadIO, liftIO)
import qualified Data.ByteString.Char8      as BS
import           Data.IORef                 (newIORef, readIORef, writeIORef)
import           Data.Maybe                 (fromJust)
import           Data.MessagePack           (MessagePack (..))

import           RSCoin.Core.Constants      (rpcTimeout)
import           RSCoin.Core.MessagePack    ()
import           RSCoin.Core.NodeConfig     (NodeContext (..))
import           RSCoin.Core.Protocol.Types (RSCoinMethod (..))
import           RSCoin.Core.Types          (Explorer (..), Mintette (..))
import qualified RSCoin.Timed               as T

-- TODO: this module should provide more safety and expose better api
-- Note that you can't match arguments in datatype constructors,
-- they are used as 'tags' only.

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

-- | Send a request to Explorer using Continuation passing style (CPS).
execExplorer :: MessagePack a => Explorer -> T.Client a -> WithResult a
execExplorer e = (>>=) . callExplorer e

-- | Send a request to a Bank using Continuation passing style (CPS).
-- Raises an exception if Bank doesn't respond in rpcTimeout time.
execBankSafe :: MessagePack a => T.Client a -> WithResult a
execBankSafe = (>>=) . callBankSafe

-- | Send a request to a Mintette using Continuation passing style (CPS).
-- Raises an exception if Mintette doesn't respond in rpcTimeout time.
execMintetteSafe :: MessagePack a => Mintette -> T.Client a -> WithResult a
execMintetteSafe m = (>>=) . callMintetteSafe m


-- | Send request to Notary.
execNotary :: MessagePack a => T.Client a -> WithResult a
execNotary = (>>=) . callNotary

-- | Send a request to a Explorer using Continuation passing style (CPS).
-- Raises an exception if Explorer doesn't respond in rpcTimeout time.
execExplorerSafe :: MessagePack a => Explorer -> T.Client a -> WithResult a
execExplorerSafe m = (>>=) . callExplorerSafe m

-- | Send a request to a Bank.
callBank
    :: (MessagePack a, T.WorkMode m)
    => T.Client a -> m a
callBank action = do
    bAddr <- _bankAddr <$> T.getNodeContext
    T.execClient bAddr action

-- | Send a request to a Mintette.
callMintette
    :: (MessagePack a, T.WorkMode m)
    => Mintette -> T.Client a -> m a
callMintette Mintette {..} action =
    T.execClient (BS.pack mintetteHost, mintettePort) action

-- | Send a request to a Explorer.
callExplorer
    :: (MessagePack a, T.WorkMode m)
    => Explorer -> T.Client a -> m a
callExplorer Explorer {..} action =
    T.execClient (BS.pack explorerHost, explorerPort) action

-- | Send a request to a Bank.
-- Raises an exception if Bank doesn't respond in rpcTimeout time.
callBankSafe
    :: (MessagePack a, T.WorkMode m)
    => T.Client a -> m a
callBankSafe action = do
    bAddr <- _bankAddr <$> T.getNodeContext
    T.execClientTimeout rpcTimeout bAddr action

-- | Send a request to a Mintette.
-- Raises an exception if Mintette doesn't respond in rpcTimeout time.
callMintetteSafe
    :: (MessagePack a, T.WorkMode m)
    => Mintette -> T.Client a -> m a
callMintetteSafe Mintette {..} action =
    T.execClientTimeout rpcTimeout (BS.pack mintetteHost, mintettePort) action

callNotary :: (MessagePack a, T.WorkMode m) => T.Client a -> m a
callNotary action = do
    nAddr <- _notaryAddr <$> T.getNodeContext
    T.execClientTimeout rpcTimeout nAddr action

-- | Send a request to a Explorer.
-- Raises an exception if Explorer doesn't respond in rpcTimeout time.
callExplorerSafe
    :: (MessagePack a, T.WorkMode m)
    => Explorer -> T.Client a -> m a
callExplorerSafe Explorer{..} action =
    T.execClientTimeout rpcTimeout (BS.pack explorerHost, explorerPort) action

-- | Reverse Continuation passing style (CPS) transformation
unCps
    :: forall a m.
       MonadIO m
    => ((a -> m ()) -> m ()) -> m a
unCps withResult = do
    ref <- liftIO $ newIORef Nothing
    withResult $ liftIO . writeIORef ref . Just
    fromJust <$> liftIO (readIORef ref)
