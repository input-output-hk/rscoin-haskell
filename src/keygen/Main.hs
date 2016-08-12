import           Control.Monad                      (replicateM)

import           Data.Aeson                         (encode)
import qualified Data.ByteString.Lazy       as B    (writeFile)
import qualified Data.ByteString.Lazy.Char8 as B    (unlines)
import           KeygenOptions              as Opts

import           Options.Applicative                ((<>))

import           RSCoin.Core                        (derivePublicKey, keyGen,
                                                     readSecretKey, sign,
                                                     writePublicKey, writeSecretKey)

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    case cloCommand of
        Opts.Single fpName -> do
            let fpSecret = fpName <> ".sec"
                fpPublic = fpName <> ".pub"
            (sk,pk) <- keyGen
            writePublicKey fpPublic pk
            writeSecretKey fpSecret sk
        Opts.Batch genNum genPath skPath -> do
            masterSK <- readSecretKey skPath
            keys <- replicateM genNum (generator masterSK)
            let generatedKeys = B.unlines $ map encode keys
            B.writeFile genPath generatedKeys
  where
    generator masterSK = do
        (sk, pk) <- keyGen
        let sig      = sign masterSK pk
            masterPK = derivePublicKey masterSK
        return (masterPK, (pk, sk), sig)
