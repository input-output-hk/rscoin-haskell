{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Wrap Storage into AcidState.

module RSCoin.Signer.AcidState
        ( RSCoinSignerState

          -- * acid-state query and update data types
        , GetSignedTxs (..)
        , SignTx       (..)

          -- * Bracket functions
        , openState
        , openMemState
        , closeState
        ) where

import           Control.Exception     (throw)
import           Control.Monad.Catch   (MonadThrow (throwM))

import           Data.Acid             (AcidState, Update, closeAcidState,
                                        makeAcidic, openLocalStateFrom)
import           Data.Acid.Memory      (openMemoryState)
import           Data.SafeCopy         (base, deriveSafeCopy)

import           RSCoin.Signer.Storage (Storage, emptySignerStorage, getSignedTxs, signTx)

type RSCoinSignerState = AcidState Storage

$(deriveSafeCopy 0 'base ''Storage)

openState :: FilePath -> IO RSCoinSignerState
openState fp = openLocalStateFrom fp emptySignerStorage

openMemState :: IO RSCoinSignerState
openMemState = openMemoryState emptySignerStorage

closeState :: RSCoinSignerState -> IO ()
closeState = closeAcidState

instance MonadThrow (Update s) where
    throwM = throw

$(makeAcidic ''Storage
             [ 'signTx
             , 'getSignedTxs
             ])
