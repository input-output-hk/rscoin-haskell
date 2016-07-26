{-# LANGUAGE TemplateHaskell #-}

-- | This module provides configuration context for running nodes of rscoin.

module RSCoin.Core.NodeConfig
        ( Host
        , NetworkAddress
        , NodeContext (..)
        , Port

          -- * 'NodeContext' lenses
        , bankPublicKey
        , bankSecretKey

          -- * Other lenses
        , bankHost
        , bankPort
        , genesisAddress

          -- * Hardcoded constants for tests and benchmarks
        , defaultNodeContext
        , testBankPublicKey
        , testBankSecretKey
        ) where

import           Control.Lens               (Getter, Lens', makeLenses, to, (^.), _1, _2)
import           Data.Bifunctor             (second)
import           Data.ByteString            (ByteString)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.SafeCopy              (base, deriveSafeCopy)

import           RSCoin.Core.Constants      (defaultPort, localhost)
import           RSCoin.Core.Crypto.Signing (PublicKey, SecretKey, deterministicKeyGen)
import           RSCoin.Core.Primitives     (Address (..))


type Port = Int
type Host = ByteString
type NetworkAddress = (Host, Port)

data NodeContext = NodeContext
    { _bankAddr      :: NetworkAddress
    , _notaryAddr    :: NetworkAddress
    , _bankPublicKey :: PublicKey
    , _bankSecretKey :: Maybe SecretKey  -- @TODO: not type-safe solution, but ok for now
    } deriving (Show)

$(makeLenses ''NodeContext)
$(deriveSafeCopy 0 'base ''NodeContext)

-- | Default node context for local deployment
defaultNodeContext :: NodeContext
defaultNodeContext = NodeContext{..}
  where
    _bankAddr      = (localhost, defaultPort)
    _notaryAddr    = (localhost, 4001)
    (_bankPublicKey, _bankSecretKey) = fromMaybe
        (error "[FATAL] Failed to construct (pk, sk) pair for default context")
        $ second Just <$> deterministicKeyGen "default-node-context-keygen-seed"

bankHost :: Lens' NodeContext Host
bankHost = bankAddr._1

bankPort :: Getter NodeContext Port
bankPort = bankAddr._2

-- | Special address used as output in genesis transaction
genesisAddress :: Getter NodeContext Address
genesisAddress = bankPublicKey.to Address

-- | This Bank public key should be used only for tests and benchmarks.
testBankPublicKey :: PublicKey
testBankPublicKey = defaultNodeContext^.bankPublicKey

-- | This Bank secret key should be used only for tests and benchmarks.
testBankSecretKey :: SecretKey
testBankSecretKey = fromJust $ defaultNodeContext^.bankSecretKey