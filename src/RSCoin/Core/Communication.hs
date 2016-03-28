-- | This module provide high-abstraction functions to exchange data
-- within user/mintette/bank.

module RSCoin.Core.Communication
       ( getBlockchainHeight
       , getBlockByHeight
       , getOwners
       , checkTx
       ) where

import           RSCoin.Core.Crypto     (Signature)
import           RSCoin.Core.Owners     (owners)
import qualified RSCoin.Core.Primitives as T
import qualified RSCoin.Core.Protocol   as P
import qualified RSCoin.Core.Types      as T

getBlockchainHeight :: IO Int
getBlockchainHeight =
    fromResponse <$> P.callBank P.ReqGetBlockchainHeight
    where fromResponse (P.ResGetBlockchainHeight h) = h
          fromResponse _ = error "GetBlockchainHeight got unexpected result"

getBlockByHeight :: T.PeriodId -> IO (Maybe T.HBlock)
getBlockByHeight =
    fmap fromResponse . P.callBank . P.ReqGetHBlock
    where fromResponse (P.ResGetHBlock hb) = hb
          fromResponse _ = error "GetBlockByHeight got unexpected result"

getOwners :: T.AddrId -> IO T.Mintettes
getOwners (tId, _, _) = do
    mts <- fromResponse <$> P.callBank P.ReqGetMintettes
    return $ map (mts !!) $ owners mts tId
  where
    fromResponse (P.ResGetMintettes m) = m
    fromResponse _ = error "GetMintettes got unexpected result"

checkTx
    :: T.Mintette
    -> T.Transaction
    -> T.AddrId
    -> Signature
    -> IO (Maybe T.CheckConfirmation)
checkTx m tx a s = fromResponse <$> P.callMintette m (P.ReqCheckTx tx a s)
  where
    fromResponse (P.ResCheckTx cc) = cc
    fromResponse _ = error "CheckTx got unexpected result"
