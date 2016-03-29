-- | RSCoin.Core.Crypto specification

module RSCoin.Core.CryptoSpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Test.Hspec.QuickCheck (prop)

import           RSCoin.Core.Crypto    (PublicKey, SecretKey, checkKeyPair,
                                        derivePublicKey, sign, verify)

spec :: Spec
spec = do
    describe "Crypto" $ do
        prop
            "Signature generated using secret key may be verified using derived public key"
            signThenVerify
        prop
            "Signature generated using secret key may be verified only by public key `pk` for which `checkKeyPair (sk, pk)`"
            signThenVerifyArbitraryPK

signThenVerify :: Int -> SecretKey -> Bool
signThenVerify v sk = verify (derivePublicKey sk) (sign sk v) v

signThenVerifyArbitraryPK :: Int -> SecretKey -> PublicKey -> Bool
signThenVerifyArbitraryPK v sk pk =
    verify pk (sign sk v) v == checkKeyPair (sk, pk)
