import           Control.Monad             (replicateM)

import           Data.Aeson                (encode)
import qualified Data.ByteString.Lazy as B (append, intercalate, writeFile)

import           KeygenOptions             as Opts

import           RSCoin.Core               (initLogging, keyGen,
                                            readSecretKey, sign)

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    case cloCommand of
        Opts.Single skPath genPath -> do
            masterSK <- readSecretKey skPath
            tupleKeysSig <- generator masterSK
            let generatedKey = encode tupleKeysSig `B.append` "\n"
            B.writeFile genPath generatedKey
        Opts.Batch genNum skPath genPath -> do
            masterSK <- readSecretKey skPath
            keys <- replicateM genNum (generator masterSK)
            let generatedKeys = B.intercalate "\n" $ map encode keys
            B.writeFile genPath generatedKeys
  where
    generator masterSK = do
        (_, pk) <- keyGen
        let sig = sign masterSK pk
        return (pk, sig)
