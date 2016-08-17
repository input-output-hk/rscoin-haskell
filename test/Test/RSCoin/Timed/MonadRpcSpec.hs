{-# LANGUAGE ViewPatterns #-}

-- | RSCoin.Test.MonadRpc specification

module Test.RSCoin.Timed.MonadRpcSpec
       ( spec
       ) where

import           Control.Monad.State         (StateT, execStateT, modify)
import           Network.MessagePack.Server  (ServerT)
import           Test.Hspec                  (Spec, describe, runIO)
import           Test.Hspec.QuickCheck       (prop)
import           Test.QuickCheck             (Property, Testable (property),
                                              ioProperty)
import           Test.QuickCheck.Monadic     (PropertyM, assert, monadic, run)

import           RSCoin.Core                 (testingLoggerName)
import           RSCoin.Core.NodeConfig      (Host, NetworkAddress, Port)
import           RSCoin.Timed                (ContextArgument (CADefaultLocation),
                                              WorkMode, for, fork, fork_,
                                              killThread, ms,
                                              runRealModeUntrusted, wait)
import           RSCoin.Timed.MonadRpc       (Client (..), MonadRpc (..),
                                              MsgPackRpc (..), call, method)
import           RSCoin.Timed.PureRpc        (PureRpc, runPureRpc)

import           Test.RSCoin.Timed.Arbitrary ()

spec :: Spec
spec =
    describe "MonadRpc" $ do
        describe "MsgPackRpc" $
            describe "msgpack-rpc based implementation of RPC layer" $ do
                runIO $ runRealModeUntrusted testingLoggerName CADefaultLocation (fork_ server)
                prop "client should be able to execute server method" $
                    runMsgPackRpcProp serverMethodShouldExecuteSimpleSpec
        describe "PureRpc" $
            describe "pure implementation of RPC layer" $
                prop "client should be able to execute server method" $
                    runPureRpcProp serverMethodShouldExecuteSimplePureSpec

type MsgPackRpcProp = PropertyM MsgPackRpc
type PureRpcProp = PureRpc (StateT Bool IO)

assertPure :: Bool -> PureRpcProp ()
assertPure b = modify (b &&)

runMsgPackRpcProp :: MsgPackRpcProp () -> Property
runMsgPackRpcProp =
    monadic $
    ioProperty . runRealModeUntrusted testingLoggerName CADefaultLocation

runPureRpcProp :: PureRpcProp () -> Property
runPureRpcProp test =
    property $
    \gen delays ->
         ioProperty $ execStateT (runPureRpc gen delays test) True

-- TODO: it would be useful to create an instance of Function for Client and Method;
-- see here https://hackage.haskell.org/package/QuickCheck-2.8.2/docs/Test-QuickCheck-Function.html#t:Function

port :: Port
port = 5000

host :: Host
host = "127.0.0.1"

addr :: NetworkAddress
addr = (host, port)

serverMethodShouldExecuteSimpleSpec
    :: WorkMode m
    => PropertyM m ()
serverMethodShouldExecuteSimpleSpec = client

serverMethodShouldExecuteSimplePureSpec
    :: PureRpcProp ()
serverMethodShouldExecuteSimplePureSpec = do
    serverThread <- fork server
    clientPure
    killThread serverThread

server :: MonadRpc m => m ()
server = do
    let respAdd = add
        respEcho = echo
    restrict $ respAdd 0 0
    restrict $ respEcho ""
    serve port [method "add" respAdd, method "echo" respEcho]
  where
    add
        :: Monad m
        => Int -> Int -> ServerT m Int
    add x y = return $ x + y
    echo
        :: Monad m
        => String -> ServerT m String
    echo s = return $ "***" ++ s ++ "***"

restrict :: Monad m => ServerT m a -> m ()
restrict _  =  return ()

client :: WorkMode m => PropertyM m ()
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
