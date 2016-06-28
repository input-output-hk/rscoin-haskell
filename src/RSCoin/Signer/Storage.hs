{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}

-- | Storage for mintette's data.

module RSCoin.Signer.Storage
       ( Storage
       , mkStorage
       , signTx
       , getSignedTxs
       -- | Other helper methods
       , signedTxs
       ) where

import           Control.Applicative        ((<|>))
import           Control.Lens               (Getter, at, makeLenses, to, use,
                                             uses, view, (%=), (+=), (.=),
                                             (<>=), (<~))
import           Control.Monad              (unless, when)
import           Control.Monad.Catch        (MonadThrow (throwM))
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Monad.State.Class  (MonadState)
import           Data.Foldable              (forM_)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust, isJust, isNothing)
import qualified Data.Set                   as S

import           RSCoin.Core                (PublicKey, SecretKey, Signature,
                                             Transaction (..), sign)
import qualified RSCoin.Core                as C
import           RSCoin.Mintette.Error      (MintetteError (..))

data Storage = Storage
    { _signedTxs  :: S.Set Transaction     -- ^ Signed transactions
    }

$(makeLenses ''Storage)

mkStorage :: Storage
mkStorage =
    Storage
    { _signedTxs = M.empty
    }

type Query a = Getter Storage a
type Update a = forall m . MonadState Storage m => m a
type ExceptUpdate a = forall m . (MonadThrow m, MonadState Storage m) => m a

getSignedTxs :: (MonadReader Storage m) => m (S.Set Transaction)
getSignedTxs = view signedTxs

signTx
    :: SecretKey
    -> Transaction
    -> Update (PublicKey, Signature)
signTx sk tx = do
    signedTxs %= S.insert tx
    let pk = derivePublicKey sk
    return (pk, sign sk tx)
