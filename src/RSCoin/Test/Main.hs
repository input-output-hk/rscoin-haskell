module Main where

import          Prelude                 hiding (log)
import          Control.Monad.Trans     (liftIO, MonadIO)

import          RSCoin.Test.MonadTimed  (wait, invoke, schedule, now, fork
                                        , at, after, for, till 
                                        , sec, minute, sec'
                                        , MonadTimed, runTimedIO_, localTime)
import          RSCoin.Test.Timed       (runTimedT)
import          RSCoin.Test.MonadRpc 
import          RSCoin.Test.PureRpc

import          Network.MessagePack.Server (Server)
import          Network.MessagePack.Client (Client)

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
    seconds <- ( / 1000000) . fromIntegral <$> localTime
    liftIO $ putStrLn $ mconcat $ ["[", show seconds, "s] ", msg]

-- * Rpc

handshake :: IO ()
handshake  =  runTimedIO_ . runMsgPackRpc $ do
    fork $ do
        serve 45678 [method "lol" response]

    wait (for 0.1 sec')
    execClient ("localhost", 45678) (request >>= liftIO . putStrLn)

response :: String -> Server String
response s  =  liftIO (putStrLn s) >> return ("Yes, " ++ s)

request :: Client String
request  =  call "lol" ("It works!" :: String)



