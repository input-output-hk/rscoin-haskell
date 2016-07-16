-- | RSCoin.Core.Crypto specification

module Test.RSCoin.Core.CryptoSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)

import           RSCoin.Core.Crypto    (PublicKey, SecretKey,
                                        checkKeyPair, derivePublicKey, sign,
                                        verify, verifyChain)

spec :: Spec
spec =
    describe "Crypto" $ do
    prop
        "Signature generated using secret key may be verified using derived public key"
        signThenVerify
    prop
        "Signature generated using secret key may be verified only by public key `pk` for which `checkKeyPair (sk, pk)`"
        signThenVerifyArbitraryPK
    prop
        "Signatures generated in chain using secret keys may be verified using derived public keys"
        signThenVerifyArbitraryChain

signThenVerify :: Int -> SecretKey -> Bool
signThenVerify v sk = verify (derivePublicKey sk) (sign sk v) v

signThenVerifyArbitraryPK :: Int -> SecretKey -> PublicKey -> Bool
signThenVerifyArbitraryPK v sk pk =
    verify pk (sign sk v) v == checkKeyPair (sk, pk)

signThenVerifyArbitraryChain :: SecretKey -> [SecretKey] -> Bool
signThenVerifyArbitraryChain sk l = verifyChain pk $ helper sk l
    where pk = derivePublicKey sk
          helper _ [] = []
          helper sKey (newSk:rest) =
              let newPk = (derivePublicKey newSk)
              in  (sign sKey newPk, newPk) : helper newSk rest
