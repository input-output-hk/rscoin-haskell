import           Control.Monad       (replicateM)
import           Data.Maybe          (fromMaybe)
import           Options.Applicative ((<>))

import           KeygenOptions       as Opts
import           RSCoin.Core         (derivePublicKey, keyGen, readPublicKey,
                                      readSecretKey, sign, writePublicKey,
                                      writeSecretKey, writeSignature)

main :: IO ()
main = do
    command <- Opts.getOptions
    case command of
        Opts.Single keyName -> do
            let fpSecret = keyName
                fpPublic = keyName <> ".pub"
            (sk,pk) <- keyGen
            writeSecretKey fpSecret sk
            writePublicKey fpPublic pk
        Opts.Batch genNum genPath skPath -> do
            masterSK <- readSecretKey skPath
            keys <- replicateM genNum (generator masterSK)
            let generatedKeys = unlines $ map show keys
            writeFile genPath generatedKeys
        Opts.Derive skPath output -> do
            secretKey <- readSecretKey skPath
            let publicKey = derivePublicKey secretKey
            flip writePublicKey publicKey $
                fromMaybe (skPath <> ".pub") output
        Opts.Sign skPath pkPath output -> do
            secretKey <- readSecretKey skPath
            publicKey <- readPublicKey pkPath
            let signature = sign secretKey publicKey
            flip writeSignature signature $
                fromMaybe (pkPath <> ".sig") output
  where
    generator masterSK = do
        (sk, pk) <- keyGen
        let sig      = sign masterSK pk
            masterPK = derivePublicKey masterSK
        return (masterPK, (pk, sk), sig)
