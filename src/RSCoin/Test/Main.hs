module Main where

import          Prelude                 hiding (log)
import          Control.Monad           (forM_)
import          Control.Monad.Trans     (liftIO, MonadIO)
import          Data.Default            (def)
import          System.Random           (mkStdGen) 
import          Control.Monad.Random.Class (getRandomR)

import          RSCoin.Test.MonadTimed  (wait, invoke, schedule, now, fork
                                        , at, after, for, till 
                                        , sec, minute, sec'
                                        , MonadTimed, runTimedIO_, localTime)
import          RSCoin.Test.Timed       (runTimedT)
import          RSCoin.Test.MonadRpc 
import          RSCoin.Test.PureRpc
import          RSCoin.Test.MonadRpc    

import          Network.MessagePack.Server (Server)

main :: IO ()
main  =  sayHelloIO

-- * Timed

sayHelloIO :: IO ()
sayHelloIO  =  runTimedIO_ sayHello

sayHelloPure :: IO ()
sayHelloPure  =  runTimedT sayHello

sayHello :: (MonadIO m, MonadTimed m) => m ()
sayHello  =  do
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
log msg  =  do
    seconds <- time
    liftIO $ putStrLn $ mconcat $ ["[", show seconds, "s] ", msg]
  where
    time :: MonadTimed m => m Double
    time  =  ( / 1000000) . fromIntegral <$> localTime 

-- * Rpc

rpcIO :: IO ()
rpcIO  =  runTimedIO_ . runMsgPackRpc $ handshake

rpcPure :: IO ()
rpcPure  =  rpcSeed 0

rpcSeed :: Int -> IO ()
rpcSeed seed  =  runPureRpc (mkStdGen seed) delays $ handshake

handshake :: (MonadRpc m, MonadTimed m, MonadIO m) => m ()
handshake  =  do
    fork $ serve 2222 [method "lol" response]

    forM_ [1..3] $ \i ->
        schedule (at 1 sec) $ do
            forM_ [1..3] $ \j -> 
                schedule (at 2 sec) $ do
                    let a = i * 3 + j
                    log $ "Q" ++ show a
                    res <- execClient ("localhost", 2222) $ request a
                    log $ "A" ++ show a

response :: Int -> Server Int
response k  =  do
    liftIO $ putStrLn $ "R" ++ show k
    return k

request :: Int -> Client Int
request  =  call "lol" 

delays :: Delays
delays  =  Delays d
  where
    d _ _  =  Just <$> getRandomR (10, 1000)
