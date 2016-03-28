import           RSCoin.Core     (readSecretKey)
import qualified RSCoin.Mintette as M

import qualified Options         as Opts

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    sk <- readSecretKey cloSecretKeyPath
    M.serve cloPort cloPath sk
