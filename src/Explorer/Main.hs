import qualified RSCoin.Core     as C

import           ExplorerOptions (Options (..), getOptions)

main :: IO ()
main = do
    Options {..} <- getOptions
    C.initLogging cloLogSeverity
    sk <- C.readSecretKey cloSecretKeyPath
    putStrLn "Blockchain = DEAD"
    undefined sk
