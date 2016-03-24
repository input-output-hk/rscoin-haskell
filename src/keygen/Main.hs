import           Options.Applicative (Parser, execParser, fullDesc, help,
                                      helper, info, metavar, progDesc,
                                      strArgument, (<>))

import           RSCoin.Core.Crypto  (keyGen, writePublicKey, writeSecretKey)

parser :: Parser FilePath
parser = strArgument (metavar "PATH" <> help "Path to store private key")

main :: IO ()
main = do
    fpSecret <-
        execParser $
        info (helper <*> parser) (fullDesc <> progDesc "RSCoin's Bank")
    let fpPublic = fpSecret <> ".pub"
    (sk,pk) <- keyGen
    writePublicKey fpPublic pk
    writeSecretKey fpSecret sk
