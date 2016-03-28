-- | This module reperents all logic that abstract client should have
-- in its arsenal -- mostly the algorithmic part of user in paper,
-- requests to bank/mintettes and related things.

module RSCoin.User.Logic
       ( getBlockchainHeight
       , getBlockByHeight
       ) where

import           RSCoin.Core as C

-- If they're used not from client only, it makes sense to put them to
-- some other module..
getBlockchainHeight :: IO Int
getBlockchainHeight =
    fromResponse <$> C.callBank C.ReqGetBlockchainHeight
    where fromResponse (C.ResGetBlockchainHeight h) = h
          fromResponse _ = error "GetBlockchainHeight got unexpected result"

getBlockByHeight :: PeriodId -> IO (Maybe C.HBlock)
getBlockByHeight =
    fmap fromResponse . C.callBank . C.ReqGetHBlock
    where fromResponse (C.ResGetHBlock hb) = hb
          fromResponse _ = error "GetBlockByHeight got unexpected result"
