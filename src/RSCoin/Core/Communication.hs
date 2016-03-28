-- | This module provide high-abstraction functions to exchange data
-- within user/mintette/bank.

module RSCoin.Core.Communication
       ( getBlockchainHeight
       , getBlockByHeight
       , getOwners
       ) where

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
getOwners = undefined -- TODO
