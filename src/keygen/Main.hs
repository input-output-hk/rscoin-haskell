import           Control.Monad             (replicateM)

import           Data.Aeson                (encode)
import qualified Data.ByteString.Lazy as B (intercalate, writeFile)

import           KeygenOptions             as Opts

import           RSCoin.Core               (derivePublicKey,initLogging,
                                            keyGen, readSecretKey, sign)

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    case cloCommand of
        Opts.Single skPath genPath -> do
            helper skPath genPath 1
        Opts.Batch genNum skPath genPath -> do
            helper skPath genPath genNum
  where
    generator masterSK = do
        (sk, pk) <- keyGen
        let sig      = sign masterSK pk
            masterPK = derivePublicKey masterSK
        return (masterPK, (pk, sk), sig)
    helper keyPath gPath num = do
        masterSK <- readSecretKey keyPath
        keys <- replicateM num (generator masterSK)
        let generatedKeys = B.intercalate "\n" $ map encode keys
        B.writeFile gPath generatedKeys
