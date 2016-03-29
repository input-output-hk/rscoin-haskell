-- | Protocol implements all low level communication between
-- entities (User, Bank, Mintette).

module RSCoin.Core.Protocol
       ( BankMethod (..)
       , MintetteMethod (..)
       , RSCoinMethod (..)
       , S.Server
       , C.Client
       , method
       , S.serve
       , call
       , execBank
       , execMintette
       , toAeson
       , fromAeson
       ) where

import           Control.Monad.Trans     (lift)
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString.Char8   as BS

import           Data.MessagePack.Object (Object (ObjectBin))
import           Data.MessagePack.Aeson  (toAeson, fromAeson)

import qualified Network.MessagePack.Server as S
import qualified Network.MessagePack.Client as C

import           RSCoin.Core.Constants  (bankHost, bankPort)
import           RSCoin.Core.Types      (Mintette (..))

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

method :: S.MethodType m f => RSCoinMethod -> f -> S.Method m
method m = S.method (show m)

--call :: RpcType a => RSCoinMethod -> a
-- FIXME: RpcType isn't exported so my idea of using Show RSCoinMethod for method name
-- doesn't hold any more
call m = C.call (show m)

execBank :: C.Client a -> (a -> IO a) -> IO ()
execBank action withResult =
    C.execClient (BS.pack bankHost) bankPort $ do
        ret <- action
        liftIO $ withResult ret

execMintette :: Mintette -> C.Client a -> (a -> IO a) -> IO ()
execMintette Mintette {..} action withResult =
    C.execClient (BS.pack mintetteHost) mintettePort $ do
        ret <- action
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
