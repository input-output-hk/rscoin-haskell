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
       , GetAddressTxNumber (..)
       , GetAddressTransactions (..)
       , GetLastPeriodId (..)
       , GetTx (..)
       , AddHBlock (..)
       ) where

import           Control.Exception                 (throw)
import           Control.Monad.Catch               (MonadThrow (throwM))
import           Data.Acid                         (AcidState, Query, Update,
                                                    closeAcidState, makeAcidic,
                                                    openLocalStateFrom)
import           Data.Acid.Memory                  (openMemoryState)

import qualified RSCoin.Core                       as C

import qualified RSCoin.Explorer.Storage           as ES
import           RSCoin.Explorer.Web.Sockets.Types (TransactionSummary (..))

type State = AcidState ES.Storage

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

getAddressTxNumber :: C.Address -> Query ES.Storage (C.PeriodId, Word)
getAddressTxNumber = ES.getAddressTxNumber

getAddressTransactions
    :: C.Address
    -> (Word, Word)
    -> Query ES.Storage (C.PeriodId, [(Word, TransactionSummary)])
getAddressTransactions = ES.getAddressTransactions

getLastPeriodId :: Query ES.Storage (Maybe C.PeriodId)
getLastPeriodId = ES.getLastPeriodId

getTx :: C.TransactionId -> Query ES.Storage (Maybe C.Transaction)
getTx = ES.getTx

addHBlock :: C.PeriodId -> C.HBlock -> Update ES.Storage ()
addHBlock = ES.addHBlock

$(makeAcidic ''ES.Storage
             [ 'getAddressBalance
             , 'getAddressTxNumber
             , 'getAddressTransactions
             , 'getLastPeriodId
             , 'getTx
             , 'addHBlock
             ])
