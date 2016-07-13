-- | This module contains predefined hardcoded keys. Probably, it is better
-- to keep them somewhere else. But for now it is just temporal solution
-- for testing and implementation ease puproses.

module RSCoin.Core.Trusted
        ( attainPublicKey
        , attainSecretKey
        , bankColdPublic
        , bankColdSecret
        , chainRootPKs
        ) where

import           Data.Maybe         (fromMaybe)

import           RSCoin.Core.Crypto (PublicKey, SecretKey, deterministicKeyGen)

{- Attain keys (for chain of cert solution -}

-- | Attain public key pair. It's needed for multisignature address allocation.
attainKeyPair :: (PublicKey, SecretKey)
attainKeyPair =
    fromMaybe (error "Invalid Attain address seed")
    $ deterministicKeyGen "attain-service-public-addressgen"

-- | Known public key of attain
attainPublicKey :: PublicKey
attainPublicKey = fst attainKeyPair

-- @TODO Move it in proper place so nobody can know about it
attainSecretKey :: SecretKey
attainSecretKey = snd attainKeyPair

{- Some predefined Bank key of cold storage (for new MS allocation strategy -}

bankKeyPair :: (PublicKey, SecretKey)
bankKeyPair =
    fromMaybe (error "Invalid Bank address seed")
    $ deterministicKeyGen "trusted-servicekey-cold-storage0"

-- | Public key of Bank cold storage.
bankColdPublic :: PublicKey
bankColdPublic = fst bankKeyPair

-- | Secret key of Trusted cold storage.
bankColdSecret :: SecretKey
bankColdSecret = snd bankKeyPair

-- | Built-in know root public keys.
chainRootPKs :: [PublicKey]
chainRootPKs = [attainPublicKey, bankColdPublic]