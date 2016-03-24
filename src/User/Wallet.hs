{-# LANGUAGE TemplateHaskell #-}
-- | This module describes user wallet state & actions

module Wallet
       ( UserAddress
       , Wallet
       , emptyWallet
       ) where

import qualified Control.Lens           as L
import           Control.Monad          (replicateM)
import qualified Data.Map               as M

import           RSCoin.Core.Crypto     (PublicKey, SecretKey, keyGen)
import           RSCoin.Core.Primitives (Transaction)

-- | User address as stored and seen by wallet owner.
data UserAddress = UserAddress
    { _publicAddress  :: PublicKey -- ^ Public key aka 'address' as
                                  -- visible the outside
    , _privateAddress :: SecretKey -- ^ Secret key of the address
    }

$(L.makeLenses ''UserAddress)

-- | Wallet, that holdls all information needed for the user to 'own'
-- bitcoins. Plus some meta-information that's crucially needed for
-- this implementation to work.
data Wallet = Wallet
    { _addresses         :: [UserAddress]
    , _inputAddressesTxs :: M.Map UserAddress Transaction
    , _lastBlockId       :: Int
    }

$(L.makeLenses ''Wallet)

_MOCK_getBlockchainLength :: IO Int
_MOCK_getBlockchainLength = 0

emptyWallet :: Int -> IO Wallet
emptyWallet walletsNumber = do
    wallets <- replicateM walletsNumber keyGen
    let _addresses = map (uncurry UserAddress) wallets
    let _inputAddressesTXs = M.empty
    _blockLength <- _MOCK_getBlockchainLength
    return Wallet {..}
