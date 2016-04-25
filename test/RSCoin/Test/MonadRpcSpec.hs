{-# LANGUAGE ViewPatterns #-}

-- | RSCoin.Test.MonadRpc specification

module RSCoin.Test.MonadRpcSpec
       ( spec
       ) where

import           Control.Concurrent.MVar     (newEmptyMVar, takeMVar, putMVar)
import           Control.Monad.Trans         (liftIO, lift, MonadIO)
import           Data.MessagePack.Object     (Object(..), MessagePack, 
                                              toObject)
import qualified Data.ByteString             as B
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import           Numeric.Natural             (Natural)
import           Test.Hspec                  (Spec, describe)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Arbitrary (arbitrary), 
                                             Property, ioProperty,
                                             counterexample, oneof, elements,
                                             NonEmptyList (..))
import           Test.QuickCheck.Function    (Fun, apply) 
import           Test.QuickCheck.Monadic     (run, assert, PropertyM, monadic,
                                             monitor, pick, forAllM)
import           Test.QuickCheck.Poly        (A)

import           RSCoin.Test.MonadRpc        (MonadRpc (..), Port, Host, Addr,
                                              Method (..), Client (..),
                                              MsgPackRpc (..))
import           RSCoin.Test.MonadTimed      (MonadTimed (..), runTimedIO, for, sec, work, during, ms)

spec :: Spec
spec =
    describe "MonadRpc" $ do
        msgPackRpcSpec "MsgPackRpc" runMsgPackRpcProp

msgPackRpcSpec 
    :: (MonadRpc m, MonadTimed m)
    => String
    -> (PropertyM m () -> Property)
    -> Spec
msgPackRpcSpec description runProp =
    describe description $ do
        describe "server method should execute" $ do
            prop "client should be able to execute server method" $
                runProp . serverMethodShouldExecuteSpec

runMsgPackRpcProp :: PropertyM MsgPackRpc () -> Property
runMsgPackRpcProp = monadic $ ioProperty . runTimedIO . runMsgPackRpc

instance Arbitrary Object where
    arbitrary = oneof [ pure ObjectNil
                      , ObjectBool <$> arbitrary
                      , ObjectInt <$> arbitrary
                      , ObjectFloat <$> arbitrary
                      , ObjectDouble <$> arbitrary
                      , ObjectStr <$> arbitrary
                      , ObjectBin <$> arbitrary
                      , ObjectArray <$> arbitrary
                      , ObjectMap <$> arbitrary
                      , ObjectExt <$> arbitrary <*> arbitrary
                      ]

instance Arbitrary T.Text where
    arbitrary = T.pack <$> arbitrary

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> arbitrary

instance Arbitrary a => Arbitrary (V.Vector a) where
    arbitrary = V.fromList <$> arbitrary

-- TODO: it would be useful to create an instance of Function for Client and Method;
-- see here https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck-Function.html#t:Function

-- | Method should execute if called correctly
serverMethodShouldExecuteSpec
    :: (MonadTimed m, MonadRpc m)
    => NonEmptyList (NonEmptyList Char, [Object])
    -> PropertyM m ()
serverMethodShouldExecuteSpec (getNonEmpty -> methods') = do
    mtds <- createMethods methods
    let methodMap = createMethodMap mtds
    run . work (during $ ms 1000) $ serve 2222 mtds
    run . wait $ for 100 ms
    (name, args) <- pick $ elements methods
    res <- run . execClient ("127.0.0.1", 2222) $ Client name args
    let shouldBe = M.lookup name methodMap <*> pure args
    maybe (return ()) (\k -> assert . (== res) =<< run k) shouldBe
  where methods = map (\(a, b) -> (getNonEmpty a, b)) methods'
        -- TODO: we wouldn't need to do this if Function was defined
        createMethods :: Monad m => [(String, [Object])] -> PropertyM m [Method m]
        createMethods = mapM $ \(name, args) -> do
            res <- pick arbitrary
            return . Method name . const $ return res
        createMethodMap = M.fromList . map (\m -> (methodName m, methodBody m))
