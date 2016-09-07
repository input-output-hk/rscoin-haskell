import           Data.Default                (Default (def))
import           Control.Concurrent.STM      (atomically, newTVarIO, writeTVar)
import           Spec                        (spec)
import           System.IO.Unsafe            (unsafePerformIO)
import           Test.Hspec                  (hspec)
import           TestOptions                 as Opts

testTVar :: Opts.TestVar
testTVar = unsafePerformIO (newTVarIO def)

main :: IO ()
main = do
    testConfig <- Opts.getOptions
    testTVar <- atomically $ writeTVar testTVar testConfig
    hspec spec
