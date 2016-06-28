{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Wrap Storage into AcidState.

module RSCoin.Signer.AcidState
        (
          -- * acid-state query and update data types
          GetSignedTxs (..)
        , SignTx (..)

          -- * Bracket functions
        , openState
        , openMemState
        , closeState
        ) where

import           Control.Exception     (throw)
import           Control.Lens          (view, (%=))
import           Control.Monad.Catch   (MonadThrow (throwM))

import           Data.Acid             (AcidState, Query, Update, closeAcidState,
                                        makeAcidic, openLocalStateFrom)
import           Data.Acid.Memory      (openMemoryState)
import           Data.SafeCopy         (base, deriveSafeCopy)
import           Data.Set              (Set)
import qualified Data.Set              as S

import           RSCoin.Core           (PublicKey, SecretKey, Signature,
                                        Transaction, derivePublicKey, sign)
import           RSCoin.Signer.Storage (Storage, emptySignerStorage, signedTxs)

type State = AcidState Storage

$(deriveSafeCopy 0 'base ''Storage)

openState :: FilePath -> IO State
openState fp = openLocalStateFrom fp emptySignerStorage

openMemState :: IO State
openMemState = openMemoryState emptySignerStorage

closeState :: State -> IO ()
closeState = closeAcidState

instance MonadThrow (Update s) where
    throwM = throw

getSignedTxs :: Query Storage (Set Transaction)
getSignedTxs = view signedTxs

signTx
    :: SecretKey
    -> Transaction
    -> Update Storage (PublicKey, Signature)
signTx sk tx = do
    signedTxs %= S.insert tx
    let pk = derivePublicKey sk
    return (pk, sign sk tx)

$(makeAcidic ''Storage
             [ 'signTx
             , 'getSignedTxs
             ])
