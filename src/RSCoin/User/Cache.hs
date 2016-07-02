{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Information cached by user.

module RSCoin.User.Cache
       ( UserCache
       , mkUserCache
       , invalidateUserCache
       , getOwnersByHash
       , getOwnersByTx
       , getOwnersByAddrid
       ) where

import           Control.Concurrent.STM (TVar, atomically, modifyTVar,
                                         newTVarIO, readTVarIO, writeTVar)
import           Control.Monad.Trans    (liftIO)
import           Data.Tuple.Select      (sel1)

import qualified RSCoin.Core            as C
import           RSCoin.Timed           (WorkMode)

data CacheData = CacheData
    { cdMintettes :: (C.PeriodId, C.Mintettes)
    } deriving (Show)

nullCacheData :: CacheData
nullCacheData =
    CacheData
    { cdMintettes = (-1, [])
    }

type UserCache = TVar CacheData

instance Show UserCache where
    show _ = "UserCache"

mkUserCache :: IO UserCache
mkUserCache =
    newTVarIO nullCacheData

invalidateUserCache :: UserCache -> IO ()
invalidateUserCache = atomically . flip writeTVar nullCacheData

getMintettesList
    :: WorkMode m
    => Maybe UserCache -> C.PeriodId -> m [C.Mintette]
getMintettesList maybeCache p =
    maybe loadAndCache return =<< tryCache maybeCache
  where
    tryCache Nothing = return Nothing
    tryCache (Just c) = do
        (storedPeriodId,storedMintettes) <-
            cdMintettes <$> liftIO (readTVarIO c)
        return $
            if storedPeriodId == p
                then Just storedMintettes
                else Nothing
    loadAndCache = do
        loaded <- C.getMintettes
        case maybeCache of
            Nothing -> return ()
            Just c ->
                liftIO . atomically $
                modifyTVar
                    c
                    (\cd ->
                          cd
                          { cdMintettes = (p, loaded)
                          })
        return loaded

getOwnersByHash
    :: WorkMode m
    => Maybe UserCache -> C.PeriodId -> C.TransactionId -> m [(C.Mintette, C.MintetteId)]
getOwnersByHash maybeCache p tId = toOwners <$> getMintettesList maybeCache p
  where
    toOwners mts =
        map
            (\i ->
                  (mts !! i, i)) $
        C.owners mts tId

getOwnersByTx
    :: WorkMode m
    => Maybe UserCache
    -> C.PeriodId
    -> C.Transaction
    -> m [(C.Mintette, C.MintetteId)]
getOwnersByTx cache p = getOwnersByHash cache p . C.hash

getOwnersByAddrid
    :: WorkMode m
    => Maybe UserCache
    -> C.PeriodId
    -> C.AddrId
    -> m [(C.Mintette, C.MintetteId)]
getOwnersByAddrid cache p = getOwnersByHash cache p . sel1
