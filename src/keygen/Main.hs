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
        Opts.Generate -> do
            topSecretKey <- readSecretKey
            let genNum = fromInteger cloPubKeyNum
            keys <- replicateM genNum generator
            let generatedKeys = encode keys
            writeFile cloKeysPath generatedKeys
  where generator = generatePKs cloKeysPath cloSkPath topSecretKey
        generatePKs keyspath skpath topSK = do
            (_, pk) <- keyGen
            let sig = sign topSK pk
            return (pk, sig)
