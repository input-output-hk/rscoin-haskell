{-# LANGUAGE ExistentialQuantification #-}

import           Data.Aeson               (ToJSON (toJSON), encode)
import           Data.ByteString.Lazy     (toStrict)
import           Data.Text.Buildable      (Buildable (build))
import           Data.Text.Encoding       (decodeUtf8)
import qualified Data.Text.IO             as TIO

import           Serokell.Util            (show')

import qualified RSCoin.Core              as C
import qualified RSCoin.Explorer.WebTypes as W

data V =
    forall v. (Buildable v, Show v, ToJSON v) => V v

instance Buildable W.ServerError where
    build _ = "build is not defined"

instance Buildable W.ControlMsg where
    build _ = "build is not defined"

instance Buildable W.AddressInfoMsg where
    build _ = "build is not defined"

instance Buildable W.OutcomingMsg where
    build _ = "build is not defined"

main :: IO ()
main = do
    let coin = C.Coin 0 0.3242342
        key  = C.testBankPublicKey
        hash = C.unsafeHash ()
        addr = C.Address key
        tx = C.Transaction [(hash, 0, coin)] [(addr, coin)]
        err = W.ParseError "github" "error"
        introMsg = W.CMSetAddress addr
        aiMsg = W.AIGetTransactions (0, 2)
        outMsg = W.OMTxNumber addr 8 (10, 10)
        values =
            [ V coin
            , V key
            , V hash
            , V addr
            , V tx
            , V err
            , V introMsg
            , V aiMsg
            , V outMsg]
        printAsNeeded (V v) = do
            TIO.putStrLn "Show"
            print v
            TIO.putStrLn "Buildable"
            TIO.putStrLn $ show' v
            TIO.putStrLn "ToJSON"
            print $ toJSON v
            TIO.putStrLn "encode"
            TIO.putStrLn . decodeUtf8 . toStrict . encode . toJSON $ v
            TIO.putStrLn "___"
    mapM_ printAsNeeded values
