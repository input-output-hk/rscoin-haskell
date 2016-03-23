-- | RSCoin.Core.Crypto specification

module RSCoin.Core.CryptoSpec
       ( spec
       ) where

import           RSCoin.Core.Crypto (sign, verify, keyGen)

import           Data.ByteString    (ByteString)

import           Test.Hspec         (Spec, describe, it,
                                     shouldSatisfy, shouldNotSatisfy)

spec :: Spec
spec = do
    describe "Crypto" $ do
        let msg1 = "First predefined message"  :: ByteString
            msg2 = "Second predefined message" :: ByteString
        it "Signing and verifying signature" $ do
            (sKey1, pKey1) <- keyGen
            sign sKey1 msg1
                `shouldSatisfy`
                    \sig -> verify pKey1 sig msg1
            sign sKey1 msg1
                `shouldNotSatisfy`
                    \sig -> verify pKey1 sig msg2

            (sKey2, pKey2) <- keyGen
            sign sKey2 msg1
                `shouldNotSatisfy`
                    \sig -> verify pKey1 sig msg1
            sign sKey1 msg1
                `shouldNotSatisfy`
                    \sig -> verify pKey2 sig msg1
