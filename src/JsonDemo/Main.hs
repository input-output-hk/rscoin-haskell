{-# LANGUAGE ExistentialQuantification #-}

import           Data.Aeson               (ToJSON (toJSON))
import           Data.Text.Buildable      (Buildable (build))

import           Serokell.Util            (show')

import qualified RSCoin.Core              as C
import qualified RSCoin.Explorer.WebTypes as W

data V =
    forall v. (Buildable v, Show v, ToJSON v) => V v

instance Buildable W.ServerError where
    build _ = "build is not defined"

instance Buildable W.IntroductoryMsg where
    build _ = "build is not defined"

instance Buildable W.AddressInfoMsg where
    build _ = "build is not defined"

instance Buildable W.OutcomingMsg where
    build _ = "build is not defined"

main :: IO ()
main = do
    let coin = C.Coin 0 0.3242342
        key = C.bankPublicKey
        hash = C.hash ()
        addr = C.Address key
        tx = C.Transaction [(hash, 0, coin)] [(addr, coin)]
        err = W.ParseError "ошибка"
        introMsg = W.IMAddressInfo addr
        aiMsg = W.AIGetTransactions (0, 2)
        outMsg = W.OMTxNumber 8 10
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
            print v
            print $ show' v
            print $ toJSON v
            putStrLn "___"
    mapM_ printAsNeeded values
