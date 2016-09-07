import           Control.Concurrent.STM (atomically, writeTVar)
import           Spec                   (spec)
import           Test.Hspec             (hspec)
import           TestOptions            as Opts

main :: IO ()
main = do
    testConfig <- Opts.getOptions
    testTVar <- atomically $ writeTVar Opts.testTVar testConfig
    hspec spec
