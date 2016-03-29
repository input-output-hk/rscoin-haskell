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
       , P.unCps
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
getBlockchainHeight :: P.WithResult PeriodId
getBlockchainHeight =
    P.execBank $
        P.call (P.RSCBank P.GetMintettes)

-- | Given the height/perioud id, retreives block if it's present
getBlockByHeight :: PeriodId -> P.WithResult (Maybe HBlock)
getBlockByHeight pId =
    P.execBank $
        P.call (P.RSCBank P.GetHBlock) pId

getOwnersByHash :: TransactionId -> P.WithResult [(Mintette, MintetteId)]
getOwnersByHash tId =
    P.execBank $
        fmap toOwners
            <$> P.call
                    (P.RSCBank P.GetMintettes)
                    (P.AsMessagePack tId)
    where toOwners mts = map (\i -> (mts !! i, i)) $ owners mts tId

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
    -> P.WithResult (Maybe CheckConfirmation)
checkNotDoubleSpent m tx a s =
    P.execMintette m $
        P.call (P.RSCMintette P.CheckTx)
            (P.AsMessagePack tx)
            (P.AsMessagePack a)
            (P.AsMessagePack s)

commitTx
    :: Mintette
    -> Transaction
    -> PeriodId
    -> CheckConfirmations
    -> P.WithResult (Maybe CommitConfirmation)
commitTx m tx pId cc =
    P.execMintette m $
        P.call (P.RSCMintette P.CommitTx)
            (P.AsMessagePack tx)
            pId
            (P.AsMessagePack cc)

sendPeriodFinished :: Mintette -> PeriodId -> P.WithResult PeriodResult
sendPeriodFinished mintette pId =
    P.execMintette mintette $
        P.call (P.RSCMintette P.PeriodFinished) pId

announceNewPeriod :: Mintette -> MintetteId -> NewPeriodData -> IO ()
announceNewPeriod mintette mId npd =
    P.execMintette mintette
        (P.call (P.RSCMintette P.AnnounceNewPeriod) (P.AsMessagePack (mId, npd)))
        return
