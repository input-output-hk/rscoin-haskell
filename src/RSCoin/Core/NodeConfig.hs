{-# LANGUAGE TemplateHaskell #-}

-- | This module provides configuration context for running nodes of rscoin.

module RSCoin.Core.NodeConfig
        ( Host
        , NetworkAddress
        , NodeContext (..)
        , Port
        ) where

import           Control.Lens               (makeLenses)

import           Data.ByteString            (ByteString)

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

-- @TODO: all other lenses