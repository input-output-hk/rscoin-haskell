module TimedTest where

import          Control.Monad.Trans     (liftIO, MonadIO)

import          RSCoin.Test.MonadTimed  (wait, invoke, schedule, now
                                        , at, after, for, till, sec, minute
                                        , MonadTimed, startTimedIO_)
import          RSCoin.Test.Timed       (startTimedT)

main :: IO ()
main  =  sayHelloIO

sayHelloIO :: IO ()
sayHelloIO  =  startTimedIO_ sayHello

sayHelloPure :: IO ()
sayHelloPure  =  startTimedT sayHello


sayHello :: (MonadIO m, MonadTimed m) => m ()
sayHello  =  do
    invoke    now          $ liftIO $ putStrLn "Hello"
    invoke   (at    1 sec) $ liftIO $ putStrLn "1 second passed"

    liftIO                          $ putStrLn "Fork point"
    schedule (after 5 sec) $ liftIO $ putStrLn "5 more seconds passed, now 6"
    invoke   (after 2 sec) $ liftIO $ putStrLn "2 more seconds passed, now 3"

    wait     (for   5 sec) 
    liftIO $ putStrLn "Waited for 5 sec, now 8"
    wait     (till 10 sec) 
    liftIO $ putStrLn "Waited till 10-sec point"

    schedule (at    2 sec 1 minute) $ liftIO $ putStrLn "Aha!"
 
