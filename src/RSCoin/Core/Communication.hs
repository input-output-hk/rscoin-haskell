-- | This module provides high-abstraction functions to exchange data
-- within user/mintette/bank.

module RSCoin.Core.Communication
       ( getBlockchainHeight
       , getBlockByHeight
       , checkNotDoubleSpent
       , commitTx
       , sendPeriodFinished
       , announceNewPeriod
       , getOwnersByAddrid
       , getOwnersByTx
       ) where

import           Data.Tuple.Select      (sel1)

import           RSCoin.Core.Crypto     (Signature, hash)
import           RSCoin.Core.Owners     (owners)
import           RSCoin.Core.Primitives (AddrId, Transaction, TransactionId)
import qualified RSCoin.Core.Protocol   as P
import           RSCoin.Core.Types      (CheckConfirmation, CheckConfirmations,
                                         CommitConfirmation, HBlock, Mintette,
                                         MintetteId, NewPeriodData, PeriodId,
                                         PeriodResult)

-- | Retrieves blockchainHeight from the server
getBlockchainHeight :: IO PeriodId
getBlockchainHeight =
    fromResponse <$> P.callBank P.ReqGetBlockchainHeight
    where fromResponse (P.ResGetBlockchainHeight h) = h
          fromResponse _ = error "GetBlockchainHeight got unexpected result"

-- | Given the height/perioud id, retreives block if it's present
getBlockByHeight :: PeriodId -> IO (Maybe HBlock)
getBlockByHeight =
    fmap fromResponse . P.callBank . P.ReqGetHBlock
    where fromResponse (P.ResGetHBlock hb) = hb
          fromResponse _ = error "GetBlockByHeight got unexpected result"

getOwnersByHash :: TransactionId -> IO [(Mintette, MintetteId)]
getOwnersByHash tId = do
    mts <- fromResponse <$> P.callBank P.ReqGetMintettes
    return $ map (\i -> (mts !! i, i)) $ owners mts tId
  where
    fromResponse (P.ResGetMintettes m) = m
    fromResponse _ = error "GetMintettes got unexpected result"

-- | Gets owners from Transaction
getOwnersByTx :: Transaction -> IO [(Mintette, MintetteId)]
getOwnersByTx = getOwnersByHash . hash

-- | Gets owners from Addrid
getOwnersByAddrid :: AddrId -> IO [(Mintette, MintetteId)]
getOwnersByAddrid = getOwnersByHash . sel1

checkNotDoubleSpent
    :: Mintette
    -> Transaction
    -> AddrId
    -> Signature
    -> IO (Maybe CheckConfirmation)
checkNotDoubleSpent m tx a s = fromResponse <$> P.callMintette m (P.ReqCheckTx tx a s)
  where
    fromResponse (P.ResCheckTx cc) = cc
    fromResponse _ = error "CheckTx got unexpected result"

commitTx
    :: Mintette
    -> Transaction
    -> PeriodId
    -> CheckConfirmations
    -> IO (Maybe CommitConfirmation)
commitTx m tx pId cc =
    fromResponse <$> P.callMintette m (P.ReqCommitTx tx pId cc)
  where
    fromResponse (P.ResCommitTx res) = res
    fromResponse _ = error "CommitTx got unexpected result"

sendPeriodFinished :: Mintette -> PeriodId -> IO PeriodResult
sendPeriodFinished mintette =
    fmap fromResponse . P.callMintette mintette . P.ReqPeriodFinished
  where
    fromResponse (P.ResPeriodFinished pr) = pr
    fromResponse _ = error "SendPeriodFinished got unexpected result"

announceNewPeriod :: Mintette -> MintetteId -> NewPeriodData -> IO ()
announceNewPeriod mintette mintetteId =
    fmap fromResponse . P.callMintette mintette . P.ReqAnnounceNewPeriod mintetteId
  where
    fromResponse P.ResAnnounceNewPeriod = ()
    fromResponse _ = error "AnnounceNewPeriod got unexpected result"
