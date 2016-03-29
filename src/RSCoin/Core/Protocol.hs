{-# LANGUAGE FlexibleContexts #-}
-- | Protocol implements all low level communication between
-- entities (User, Bank, Mintette).

module RSCoin.Core.Protocol
       ( BankMethod (..)
       , MintetteMethod (..)
       , RSCoinMethod (..)
       , WithResult
       , AsMessagePack (..)
       , Server
       , C.Client
       , method
       , S.serve
       , call
       , execBank
       , execMintette
       ) where

import           Control.Monad.Trans     (lift)
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Char8   as BS

import           Data.MessagePack.Object (Object (ObjectBin))
import           Data.MessagePack.Aeson  (AsMessagePack (..))

import qualified Network.MessagePack.Server as S
import qualified Network.MessagePack.Client as C

import           RSCoin.Core.Constants  (bankHost, bankPort)
import           RSCoin.Core.Types      (Mintette (..))
import           RSCoin.Core.Crypto     ()
import           RSCoin.Core.Aeson      ()

data RSCoinMethod
    = RSCBank BankMethod
    | RSCMintette MintetteMethod
    deriving (Show)

data BankMethod
    = GetMintettes
    | GetBlockchainHeight
    | GetHBlock
    deriving (Show)

data MintetteMethod
    = PeriodFinished
    | AnnounceNewPeriod
    | CheckTx
    | CommitTx
    deriving (Show)

type Server a = S.Server (AsMessagePack a)

method :: S.MethodType m f => RSCoinMethod -> f -> S.Method m
method m = S.method (show m)

--call :: RpcType a => RSCoinMethod -> a
-- FIXME: RpcType isn't exported so my idea of using Show RSCoinMethod for method name
-- doesn't hold any more
call m = C.call (show m)

-- TODO: this can be modeled with Cont monad
-- https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style
type WithResult a = (a -> IO ()) -> IO ()

execBank :: C.Client (AsMessagePack a) -> WithResult a
execBank action withResult =
    C.execClient (BS.pack bankHost) bankPort $ do
        ret <- getAsMessagePack <$> action
        liftIO $ withResult ret

execMintette :: Mintette -> C.Client (AsMessagePack a) -> WithResult a
execMintette Mintette {..} action withResult =
    C.execClient (BS.pack mintetteHost) mintettePort $ do
        ret <- getAsMessagePack <$> action
        liftIO $ withResult ret

-- example bellow
 
start' :: IO () 
start' = S.serve 3000 [S.method "add" add]

call' :: IO ()
call' = C.execClient "127.0.0.1" 3000 $ do
    ret <- add' 1 2
    liftIO $ print ret

add :: Int -> Int -> S.Server Int
add x y = return $ x + y

add' :: Int -> Int -> C.Client Int
add' = C.call "add"
