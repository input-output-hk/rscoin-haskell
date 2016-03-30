{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-- | Protocol implements all low level communication between
-- entities (User, Bank, Mintette).

module RSCoin.Core.Protocol
       ( BankMethod (..)
       , MintetteMethod (..)
       , RSCoinMethod (..)
       , WithResult
       , Server
       , C.Client
       , method
       , S.serve
       , call
       , execBank
       , execMintette
       , callBank
       , callMintette
       , unCps
       ) where

import           Control.Monad.IO.Class     (liftIO)

import           Data.IORef                 (newIORef, writeIORef, readIORef)
import qualified Data.ByteString.Char8      as BS
import           Data.Maybe                 (fromJust)

import qualified Network.MessagePack.Server as S
import qualified Network.MessagePack.Client as C

import           RSCoin.Core.Constants      (bankHost, bankPort)
import           RSCoin.Core.Types          (Mintette (..))
import           RSCoin.Core.Crypto         ()
import           RSCoin.Core.MessagePack          ()

-- TODO: this module should provide more safety and expose better api

-- | Requests used in RSCoin transport layer.
data RSCoinMethod
    = RSCBank BankMethod
    | RSCMintette MintetteMethod
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

type Server a = S.Server a

-- | Create server method.
method :: S.MethodType m f => RSCoinMethod -> f -> S.Method m
method m = S.method (show m)

--call :: RpcType a => RSCoinMethod -> a
-- FIXME: RpcType isn't exported so my idea of using Show RSCoinMethod for method name
-- doesn't hold any more
-- | Call RSCoinMethod.
call m = C.call (show m)

-- TODO: this can be modeled with Cont monad
-- | Continuation passing style transformation.
-- For more see: https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
type WithResult a = (a -> IO ()) -> IO ()

-- | Send a request to a Bank using Continuation passing style (CPS).
execBank :: C.Client a -> WithResult a
execBank action withResult =
    C.execClient (BS.pack bankHost) bankPort $ do
        ret <- action
        liftIO $ withResult ret

-- | Send a request to a Mintette using Continuation passing style (CPS).
execMintette :: Mintette -> C.Client a -> WithResult a
execMintette Mintette {..} action withResult =
    C.execClient (BS.pack mintetteHost) mintettePort $ do
        ret <- action
        liftIO $ withResult ret

-- | Send a request to a Bank.
callBank :: C.Client a -> IO a
callBank = unCps . execBank

-- | Send a request to a Mintette.
callMintette :: Mintette -> C.Client a -> IO a
callMintette m = unCps . execMintette m

-- | Reverse Continuation passing style (CPS) transformation
unCps :: WithResult a -> IO a
unCps withResult = do
    ref <- newIORef Nothing
    withResult $ writeIORef ref . Just
    fromJust <$> readIORef ref
