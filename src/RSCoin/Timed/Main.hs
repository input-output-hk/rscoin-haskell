{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad              (forM_)
import           Control.Monad.Random.Class (getRandomR)
import           Control.Monad.Trans        (MonadIO, lift, liftIO)
import           Data.Time.Units            (fromMicroseconds)
import           Prelude                    hiding (log)
import           System.Random              (mkStdGen)

import           RSCoin.Timed.MonadRpc      (Client, MonadRpc, call, execClient,
                                             method, runMsgPackRpc, serve)
import           RSCoin.Timed.MonadTimed    (MonadTimed, after, at, during, for,
                                             invoke, localTime, minute, now,
                                             schedule, sec, sec', till, wait,
                                             work)
import           RSCoin.Timed.PureRpc       (Delays (..), runPureRpc)
import           RSCoin.Timed.Timed         (runTimedT)
import           RSCoin.Timed.TimedIO       (runTimedIO_)

import           Control.Concurrent.MVar    (MVar, newMVar, putMVar, takeMVar)

import           Network.MessagePack.Server (ServerT)


main :: IO ()
main = do
    putStrLn "sayHelloIO"
    sayHelloIO
    putStrLn "sayHelloPure"
    sayHelloPure

-- * Timed

sayHelloIO :: IO ()
sayHelloIO = runTimedIO_ sayHello

sayHelloPure :: IO ()
sayHelloPure = runTimedT sayHello

sayHello :: (MonadIO m, MonadTimed m) => m ()
sayHello = do
    log                          "Initiating..."
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

-- actually pure launch of TimedT, now not applicable (hope - temporally)
{-
type TimedState = TimedT (CatchT (State Int))

playWithTimedState :: TimedState ()
playWithTimedState = do
    invoke (at 1 sec) $ put 11
    schedule (after 5 sec) $ put 15
    invoke   (after 2 sec) $ put 12

execPlayWithTimedState :: Int
execPlayWithTimedState = execState (runCatchT $ runTimedT playWithTimedState) undefined
-}

log :: (MonadIO m, MonadTimed m) => String -> m ()
log msg = do
    seconds <- time
    liftIO $ putStrLn $ mconcat $ ["[", show seconds, "s] ", msg]
  where
    time :: MonadTimed m => m Double
    time = ( / 1000000) . fromIntegral <$> localTime

interruptedLol :: (MonadTimed m, MonadIO m) => m ()
interruptedLol = do
    work (during 5 sec) tempLol

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
    work (during 3 sec) $ serve 2222 [method "lol" resp, method "qwe" resp]

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
    d _ _ = Just . fromMicroseconds <$> getRandomR (10, 1000)

type Sync m = m () -> m ()

syncronized :: MonadIO m => MVar () -> Sync m
syncronized lock action = do
    _ <- liftIO $ takeMVar lock
    action
    liftIO $ putMVar lock ()


