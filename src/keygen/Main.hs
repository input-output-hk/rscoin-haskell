import           Control.Monad             (replicateM)

import           Data.Aeson                (encode)
import qualified Data.ByteString.Lazy as B (writeFile)

import           KeygenOptions             as Opts

import           RSCoin.Core               (initLogging, keyGen,
                                            readSecretKey, sign)

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    case cloCommand of
        Opts.GenerateSingle -> do
            sk <- readSecretKey cloSKPath
            pairPKSig <- generator sk
            let generatedKey = encode pairPKSig
            B.writeFile cloKeysPath generatedKey
        Opts.GenerateBatch genNum -> do
            sk <- readSecretKey cloSKPath
            keys <- replicateM genNum (generator sk)
            let generatedKeys = encode keys
            B.writeFile cloKeysPath generatedKeys
  where generator topSK = do
            (_, pk) <- keyGen
            let sig = sign topSK pk
            return (pk, sig)
