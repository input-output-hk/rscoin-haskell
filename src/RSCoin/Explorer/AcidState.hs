{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | AcidState boilerplate.

module RSCoin.Explorer.AcidState
       ( State
       , openState
       , openMemState
       , closeState
       , GetAddressBalance (..)
       , GetLastPeriodId (..)
       , GetTx (..)
       , AddHBlock (..)
       ) where

import           Control.Exception       (throw)
import           Control.Monad.Catch     (MonadThrow (throwM))
import           Data.Acid               (AcidState, Query, Update,
                                          closeAcidState, makeAcidic,
                                          openLocalStateFrom)
import           Data.Acid.Memory        (openMemoryState)
import           Data.SafeCopy           (base, deriveSafeCopy)

import qualified RSCoin.Core             as C

import qualified RSCoin.Explorer.Storage as ES

type State = AcidState ES.Storage

$(deriveSafeCopy 0 'base ''ES.Storage)

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp ES.mkStorage

openMemState :: IO State
openMemState = openMemoryState ES.mkStorage

closeState :: State -> IO ()
closeState = closeAcidState

instance MonadThrow (Update s) where
    throwM = throw

getAddressBalance :: C.Address -> Query ES.Storage (C.PeriodId, C.CoinsMap)
getAddressBalance = ES.getAddressBalance

getLastPeriodId :: Query ES.Storage (Maybe C.PeriodId)
getLastPeriodId = ES.getLastPeriodId

getTx :: C.TransactionId -> Query ES.Storage (Maybe C.Transaction)
getTx = ES.getTx

addHBlock :: C.PeriodId -> C.HBlock -> Update ES.Storage ()
addHBlock = ES.addHBlock

$(makeAcidic ''ES.Storage
             [ 'getAddressBalance
             , 'getLastPeriodId
             , 'getTx
             , 'addHBlock
             ])
