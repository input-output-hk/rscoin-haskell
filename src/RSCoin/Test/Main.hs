module Main where

import          Prelude                 hiding (log)
import          Control.Monad.Trans     (liftIO, MonadIO)

import          RSCoin.Test.MonadTimed  (wait, invoke, schedule, now
                                        , at, after, for, till, sec, minute
                                        , MonadTimed, startTimedIO_, localTime)
import          RSCoin.Test.Timed       (startTimedT)

main :: IO ()
main  =  sayHelloIO

sayHelloIO :: IO ()
sayHelloIO  =  startTimedIO_ sayHello

sayHelloPure :: IO ()
sayHelloPure  =  startTimedT sayHello


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
