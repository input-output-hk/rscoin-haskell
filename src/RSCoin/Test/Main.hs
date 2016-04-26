{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad              (forM_)
import           Control.Monad.Random.Class (getRandomR)
import           Control.Monad.Trans        (MonadIO, lift, liftIO)
import           Prelude                    hiding (log)
import           System.Random              (mkStdGen)

import           RSCoin.Test.MonadRpc       (Client, MonadRpc, call, execClient,
                                             method, runMsgPackRpc, serve)
import           RSCoin.Test.MonadTimed     (MonadTimed, after, at, during, for,
                                             invoke, localTime, minute, now,
                                             schedule, sec, sec', till, wait,
                                             work)
import           RSCoin.Test.PureRpc        (Delays (..), runPureRpc)
import           RSCoin.Test.Timed          (runTimedT)
import           RSCoin.Test.TimedIO        (runTimedIO_)

import           Control.Concurrent.MVar    (MVar, newMVar, putMVar, takeMVar)

import           Network.MessagePack.Server (ServerT)

main :: IO ()
main = sayHelloIO

-- * Timed

sayHelloIO :: IO ()
sayHelloIO = runTimedIO_ sayHello

sayHelloPure :: IO ()
sayHelloPure = runTimedT sayHello

sayHello :: (MonadIO m, MonadTimed m) => m ()
sayHello = do
    invoke    now          $ log "Hello"
    invoke   (at    1 sec) $ log "It's 1 second now"

    log                          "Fork point"
    schedule (after 5 sec) $ log "5 more seconds passed"
    invoke   (after 2 sec) $ log "2 more seconds passed"

    wait     (for   5 sec)
    log "Waited for 5 sec, now 8"
    wait     (till 10 sec)
    log "Waited till 10-sec point"

    schedule (at    2 sec 1 minute) $ log "Aha!"

log :: (MonadIO m, MonadTimed m) => String -> m ()
log msg = do
    seconds <- time
    liftIO $ putStrLn $ mconcat $ ["[", show seconds, "s] ", msg]
  where
    time :: MonadTimed m => m Double
    time = ( / 1000000) . fromIntegral <$> localTime

interruptedLol :: (MonadTimed m, MonadIO m) => m ()
interruptedLol = do
    work (during 5000000) tempLol

tempLol :: (MonadIO m, MonadTimed m) => m ()
tempLol = do
    liftIO $ putStrLn "Lol!"
    wait (for 2 sec)
    tempLol


-- * Rpc

rpcIO :: IO ()
rpcIO = runTimedIO_ . runMsgPackRpc $ handshake

rpcPure :: IO ()
rpcPure = rpcSeed 0

rpcSeed :: Int -> IO ()
rpcSeed seed = runPureRpc (mkStdGen seed) delays $ handshake

handshake :: (MonadRpc m, MonadTimed m, MonadIO m) => m ()
handshake = do
    sync <- liftIO $ syncronized <$> newMVar ()

    let resp = response sync
    restrict $ resp 5
    work (during $ sec' 3) $ serve 2222 [method "lol" resp, method "qwe" resp]

    forM_ [1..3] $ \i ->
        schedule (at 1 sec) $ do
            forM_ [1..3] $ \j ->
                schedule (at 2 sec) $ do
                    let a = i * 3 + j
                    let s = if even a then "lol" else "qwe"
                    sync $ log $ "Q" ++ show a
                    res <- execClient ("127.0.0.1", 2222) $ request s a
                    sync $ log $ "A" ++ show res

restrict :: Monad m => ServerT m a -> m ()
restrict _ = return ()

response :: (MonadRpc m, MonadTimed m, MonadIO m) => Sync m -> Int -> ServerT m Int
response sync k = do
    lift $ sync $ liftIO $ putStrLn $ "R" ++ show k
    lift $ wait $ for 0.4 sec'
    return k

request :: String -> Int -> Client Int
request = call

delays :: Delays
delays = Delays d
  where
    d _ _ = Just <$> getRandomR (10, 1000)

type Sync m = m () -> m ()

syncronized :: MonadIO m => MVar () -> Sync m
syncronized lock action = do
    _ <- liftIO $ takeMVar lock
    action
    liftIO $ putMVar lock ()
