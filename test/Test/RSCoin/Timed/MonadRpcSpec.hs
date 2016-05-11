{-# LANGUAGE ViewPatterns #-}

-- | RSCoin.Test.MonadRpc specification

module Test.RSCoin.Timed.MonadRpcSpec
       ( spec
       ) where

import           Control.Monad.Trans        (MonadIO)
import           Control.Monad.State        (StateT, execStateT, modify)
import           Control.Monad.Catch.Pure   (CatchT, runCatchT)
import           Test.Hspec                 (Spec, describe, runIO)
import           Test.Hspec.QuickCheck      (prop)
import           Test.QuickCheck            (Property, ioProperty,
                                             Arbitrary (..), generate)
import           Test.QuickCheck.Monadic    (PropertyM, assert, monadic,
                                             run)
import           System.Random              (StdGen, mkStdGen)
import           Control.Monad.Random.Class (getRandomR)

import           RSCoin.Timed.MonadRpc      (Addr, Client (..), Host,
                                             MonadRpc (..),
                                             MsgPackRpc (..), Port, call,
                                             method)
import           RSCoin.Timed.MonadTimed    (MonadTimed (..), for, ms)
import           RSCoin.Timed.PureRpc       (PureRpc, runPureRpc,
                                             Delays (..))
import           RSCoin.Timed.TimedIO       (runTimedIO)

import           Network.MessagePack.Server (ServerT)

spec :: Spec
spec =
    describe "MonadRpc" $ do
        msgPackRpcSpec "MsgPackRpc" runMsgPackRpcProp
        -- FIXME: dont generate new radom seed here. Better way would be use the same seed as the one quickcheck/hspec was initialised with. 
        -- Proper way of fixing this is to not use StdGen in PureRpc implementation, but to leave this to quickcheck
        runIO (generate arbitrary) >>= pureRpcSpec "PureRpc" . flip runPureRpcProp delays'

msgPackRpcSpec
    :: (MonadRpc m, MonadTimed m, MonadIO m)
    => String
    -> (PropertyM m () -> Property)
    -> Spec
msgPackRpcSpec description runProp =
    describe description $ do
        describe "server method should execute (simple)" $ do
            prop "client should be able to execute server method" $
                runProp serverMethodShouldExecuteSimpleSpec
        -- describe "server method should execute" $ do
        --     prop "client should be able to execute server method" $
        --         runProp . serverMethodShouldExecuteSpec

pureRpcSpec
    :: String
    -> (PureRpcProp () -> Property)
    -> Spec
pureRpcSpec description runProp =
    describe description $ do
        describe "server method should execute (simple)" $ do
            prop "client should be able to execute server method" $
                runProp serverMethodShouldExecuteSimplePureSpec

-- from pure testing
-- type PureRpcProp = PureRpc (CatchT (State Bool))
type PureRpcProp = PureRpc (StateT Bool IO)

assertPure :: Bool -> PureRpcProp ()
assertPure b = modify (b &&)

runMsgPackRpcProp :: PropertyM MsgPackRpc () -> Property
runMsgPackRpcProp = monadic $ ioProperty . runTimedIO . runMsgPackRpc

runPureRpcProp :: StdGen -> Delays -> PureRpcProp () -> Property
runPureRpcProp gen delays test = 
    ioProperty $ execStateT (runPureRpc gen delays test) True

-- TODO: this is kind of odd
instance Arbitrary StdGen where
    arbitrary = mkStdGen <$> arbitrary

-- FIXME: use arbitrary instead of StdGen
delays' :: Delays
delays' = Delays d
  where
    d _ _ = Just <$> getRandomR (10, 1000)

-- TODO: it would be useful to create an instance of Function for Client and Method;
-- see here https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck-Function.html#t:Function

port :: Port
port = 5000

host :: Host
host = "127.0.0.1"

addr :: Addr
addr = (host, port)

serverMethodShouldExecuteSimpleSpec
    :: (MonadTimed m, MonadRpc m, MonadIO m)
    => PropertyM m ()
serverMethodShouldExecuteSimpleSpec = do
    run $ fork $ server
    client

serverMethodShouldExecuteSimplePureSpec
    :: PureRpcProp ()
serverMethodShouldExecuteSimplePureSpec = do
    fork $ server
    clientPure

server :: MonadRpc m => m ()
server = do
    let respAdd = add
        respEcho = echo
    restrict $ respAdd 0 0
    restrict $ respEcho ""
    serve port
        [ method "add"  respAdd
        , method "echo" respEcho
        ]
  where
    add :: Monad m => Int -> Int -> ServerT m Int
    add x y = return $ x + y

    echo :: Monad m => String -> ServerT m String
    echo s = return $ "***" ++ s ++ "***"

restrict :: Monad m => ServerT m a -> m ()
restrict _  =  return ()

client :: (MonadRpc m, MonadTimed m) => PropertyM m ()
client = do
    run $ wait $ for 50 ms
    r1 <- run $ execClient addr $ addC 123 456
    assert $ r1 == 123 + 456
    r2 <- run $ execClient addr $ echoC "hello"
    assert $ r2 == "***hello***"

clientPure :: PureRpcProp ()
clientPure = do
    wait $ for 50 ms
    r1 <- execClient addr $ addC 123 456
    assertPure $ r1 == 123 + 456
    r2 <- execClient addr $ echoC "hello"
    assertPure $ r2 == "***hello***"

addC :: Int -> Int -> Client Int
addC = call "add"

echoC :: String -> Client String
echoC = call "echo"

-- TODO: this method triggers some msgpack exceptions
-- | Method should execute if called correctly
-- serverMethodShouldExecuteSpec
--     :: (MonadTimed m, MonadRpc m)
--     => NonEmptyList (NonEmptyList Char)
--     -> PropertyM m ()
-- serverMethodShouldExecuteSpec (getNonEmpty -> methodNames') = do
--     mtds <- createMethods methodNames
--     let methodMap = createMethodMap mtds
--     run . fork $ serve port mtds
--     run . wait $ for 500 ms
--     name <- pick $ elements methodNames
--     res <- run . execClient addr $ call name
--     shouldBe <- run $ fromJust $ M.lookup name methodMap <*> pure []
--     assert $ shouldBe == res
--   where methodNames = nub $ map getNonEmpty methodNames'
--         -- TODO: we wouldn't need to do this if Function was defined
--         createMethods :: Monad m => [String] -> PropertyM m [Method m]
--         createMethods = mapM $ \name -> do
--             res <- toObject <$> pick (arbitrary :: Gen Int)
--             return . Method name . const $ return res
--         createMethodMap :: Monad m => [Method m] -> M.Map String ([Object] -> m Object)
--         createMethodMap = M.fromList . map (\m -> (methodName m, methodBody m))
