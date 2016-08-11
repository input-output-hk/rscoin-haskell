import           Data.Aeson          (encode)
import           Data.ByteString     (writeFile)

import           Options.Applicative (Parser, execParser, fullDesc, help,
                                      helper, info, metavar, progDesc,
                                      showDefault, strArgument, value, (<>))

import           KeygenOptions       as Opts
import           RSCoin.Core         (defaultSecretKeyPath, keyGen,
                                      readSecretKey,
                                      writePublicKey, writeSecretKey)

main :: IO ()
main = do
    Opts.Options{..} <- Opts.getOptions
    initLogging cloLogSeverity
    case cloCommand of
        Opts.GenerateSingle -> do
            sk <- readSecretKey cloSKPath
            pairPKSig <- generator sk
            let generatedKey = encode pairPKSig
            writePublicKey cloKeysPath generatedKey
        Opts.GenerateBatch genNum -> do
            sk <- readSecretKey cloSKPath
            keys <- replicateM genNum (generator sk)
            let generatedKeys = encode keys
            writeFile cloKeysPath generatedKeys
  where generator topSK = do
            (_, pk) <- keyGen
            let sig = sign topSK pk
            return (pk, sig)
