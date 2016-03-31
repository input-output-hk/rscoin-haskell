{-# LANGUAGE ScopedTypeVariables #-}
-- | This module provides high-abstraction functions to exchange data
-- within user/mintette/bank.

module RSCoin.Core.Communication
       ( CommunicationError (..)
       , getBlockchainHeight
       , getBlockByHeight
       , getGenesisBlock
       , checkNotDoubleSpent
       , commitTx
       , sendPeriodFinished
       , announceNewPeriod
       , getOwnersByAddrid
       , getOwnersByTx
       , P.unCps
       ) where

import           Control.Exception          (Exception, catch, throwIO)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text, pack)
import           Data.Text.Buildable        (Buildable (build))
import           Data.Tuple.Select          (sel1)
import           Data.Typeable              (Typeable)
import qualified Network.MessagePack.Client as MP (RpcError (..))

import           Serokell.Util.Text        (format', formatSingle', show')

import           RSCoin.Core.Crypto         (Signature, hash)
import           RSCoin.Core.Owners         (owners)
import           RSCoin.Core.Primitives     (AddrId, Transaction, TransactionId)
import qualified RSCoin.Core.Protocol       as P
import           RSCoin.Core.Logging        (logInfo, logDebug, logWarning,
                                             logError)
import           RSCoin.Core.Types          (CheckConfirmation,
                                             CheckConfirmations,
                                             CommitConfirmation, HBlock,
                                             Mintette, MintetteId,
                                             NewPeriodData, PeriodId,
                                             PeriodResult)

-- | Errors which may happen during remote call.
data CommunicationError
    = ProtocolError Text  -- ^ Message was encoded incorrectly.
    | MethodError Text    -- ^ Error occured during method execution.
    deriving (Show, Typeable)

instance Exception CommunicationError

instance Buildable CommunicationError where
    build (ProtocolError t) = "internal error: " <> build t
    build (MethodError t) = "method error: " <> build t

rpcErrorHandler :: MP.RpcError -> IO ()
rpcErrorHandler = log . fromError
  where
    log (e :: CommunicationError) = do
        logError $ show' e
        throwIO e
    fromError (MP.ProtocolError s) = ProtocolError $ pack s
    fromError (MP.ResultTypeError s) = ProtocolError $ pack s
    fromError (MP.ServerError obj) = MethodError $ pack $ show obj

execBank :: P.Client a -> P.WithResult a
execBank cl f = P.execBank cl f `catch` rpcErrorHandler

execMintette :: Mintette -> P.Client a -> P.WithResult a
execMintette m cl f = P.execMintette m cl f `catch` rpcErrorHandler

-- | Retrieves blockchainHeight from the server
getBlockchainHeight :: P.WithResult PeriodId
getBlockchainHeight = execBank $ P.call (P.RSCBank P.GetBlockchainHeight)

-- | Given the height/perioud id, retreives block if it's present
getBlockByHeight :: PeriodId -> IO HBlock
getBlockByHeight pId = do
    res <- P.unCps $ execBank $ P.call (P.RSCBank P.GetHBlock) pId
    either (throwIO . MethodError) return res

getGenesisBlock :: IO HBlock
getGenesisBlock = getBlockByHeight 0

getOwnersByHash :: TransactionId -> P.WithResult [(Mintette, MintetteId)]
getOwnersByHash tId = execBank $ toOwners <$> P.call (P.RSCBank P.GetMintettes)
  where
    toOwners mts =
        map
            (\i -> (mts !! i, i)) $
        owners mts tId

-- | Gets owners from Transaction
getOwnersByTx :: Transaction -> P.WithResult [(Mintette, MintetteId)]
getOwnersByTx = getOwnersByHash . hash

-- | Gets owners from Addrid
getOwnersByAddrid :: AddrId -> P.WithResult [(Mintette, MintetteId)]
getOwnersByAddrid = getOwnersByHash . sel1

checkNotDoubleSpent
    :: Mintette
    -> Transaction
    -> AddrId
    -> Signature
    -> P.WithResult (Either Text CheckConfirmation)
checkNotDoubleSpent m tx a s =
    execMintette m $ P.call (P.RSCMintette P.CheckTx) tx a s

commitTx
    :: Mintette
    -> Transaction
    -> PeriodId
    -> CheckConfirmations
    -> P.WithResult (Maybe CommitConfirmation)
commitTx m tx pId cc =
    execMintette m $ P.call (P.RSCMintette P.CommitTx) tx pId cc

sendPeriodFinished :: Mintette -> PeriodId -> P.WithResult PeriodResult
sendPeriodFinished mintette pId =
    execMintette mintette $ P.call (P.RSCMintette P.PeriodFinished) pId

announceNewPeriod :: Mintette -> NewPeriodData -> IO ()
announceNewPeriod mintette npd =
    execMintette
        mintette
        (P.call (P.RSCMintette P.AnnounceNewPeriod) npd)
        return
