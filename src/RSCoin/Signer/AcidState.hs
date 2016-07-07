{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Wrap Storage into AcidState.

module RSCoin.Signer.AcidState
       ( RSCoinSignerState

         -- * acid-state query and update data types
       , GetSignatures (..)
       , AddSignature       (..)
       , AnnounceNewPeriods (..)
       , GetPeriodId (..)
       , PollTransactions (..)
       , AcquireSignatures (..)
         -- * Bracket functions
       , openState
       , openMemState
       , closeState
       ) where

import           Data.Acid             (AcidState, closeAcidState, makeAcidic,
                                        openLocalStateFrom)
import           Data.Acid.Memory      (openMemoryState)
import           Data.SafeCopy         (base, deriveSafeCopy)

import           RSCoin.Signer.Storage (Storage)
import qualified RSCoin.Signer.Storage as S

type RSCoinSignerState = AcidState Storage

$(deriveSafeCopy 0 'base ''Storage)

openState :: FilePath -> IO RSCoinSignerState
openState fp = openLocalStateFrom fp S.emptySignerStorage

openMemState :: IO RSCoinSignerState
openMemState = openMemoryState S.emptySignerStorage

closeState :: RSCoinSignerState -> IO ()
closeState = closeAcidState

$(makeAcidic ''Storage
             [ 'S.addSignature
             , 'S.getSignatures
             , 'S.announceNewPeriods
             , 'S.getPeriodId
             , 'S.pollTransactions
             , 'S.acquireSignatures
             ])
