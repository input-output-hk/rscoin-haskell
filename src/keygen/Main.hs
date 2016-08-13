import           Control.Monad                      (replicateM)

import           KeygenOptions              as Opts

import           Options.Applicative                ((<>))

import           RSCoin.Core                        (derivePublicKey, keyGen,
                                                     readSecretKey, sign,)

import           System.IO                          (IOMode (WriteMode),
                                                     hClose, openFile)

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    case cloCommand of
        Opts.Single keyName -> do
            let fpSecret = keyName <> ".sec"
                fpPublic = keyName
            (sk,pk) <- keyGen
            pub <- openFile fpPublic WriteMode
            sec <- openFile fpSecret WriteMode
            writeFile fpSecret $ show sk
            writeFile fpPublic $ show pk
            hClose pub
            hClose sec
        Opts.Batch genNum genPath skPath -> do
            masterSK <- readSecretKey skPath
            keys <- replicateM genNum (generator masterSK)
            let generatedKeys = unlines $ map show keys
            keysFile <- openFile genPath WriteMode
            writeFile genPath generatedKeys
            hClose keysFile
  where
    generator masterSK = do
        (sk, pk) <- keyGen
        let sig      = sign masterSK pk
            masterPK = derivePublicKey masterSK
        return (masterPK, (pk, sk), sig)
