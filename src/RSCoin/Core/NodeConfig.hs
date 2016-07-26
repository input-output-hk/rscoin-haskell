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
        , genesisAddress
        ) where

import           Control.Lens               (Getter, makeLenses, to)

import           Data.ByteString            (ByteString)
import           Data.SafeCopy              (base, deriveSafeCopy)

import           RSCoin.Core.Primitives     (Address (..))
import           RSCoin.Core.Crypto.Signing (PublicKey, SecretKey)


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

-- | Special address used as output in genesis transaction
genesisAddress :: Getter NodeContext Address
genesisAddress = bankPublicKey.to Address