{-# LANGUAGE TemplateHaskell #-}

-- | Wrap Storage into AcidState.

module RSCoin.Mintette.AcidState
       ( State
       , getSignedTxs
       , signTx
       ) where

import           Control.Exception       (throw)
import           Control.Monad.Catch     (MonadThrow (throwM))
import           Data.Acid               (AcidState, Query, Update)
import           Data.SafeCopy           (base, deriveSafeCopy)

import           RSCoin.Core             (PublicKey, SecretKey,
                                          Signature, Transaction)

import qualified RSCoin.Signer.Storage as SS

type State = AcidState SS.Storage

$(deriveSafeCopy 0 'base ''SS.Storage)

instance MonadThrow (Update s) where
    throwM = throw

getSignedTxs :: Query SS.Storage (S.Set Transaction)
getSignedTxs = SS.getSignedTxs

signTx
    :: SecretKey
    -> Transaction
    -> Update SS.Storage (PublicKey, Signature)
signTx = SS.signTx
