import           Options.Applicative (Parser, execParser, fullDesc, help,
                                      helper, info, metavar, progDesc,
                                      showDefault, strArgument, value, (<>))

import           RSCoin.Core         (defaultSecretKeyPath, keyGen,
                                      writePublicKey, writeSecretKey)

parser :: Parser FilePath
parser =
    strArgument
        (metavar "PATH" <> help "Path to store private key" <>
         value defaultSecretKeyPath <> showDefault)

main :: IO ()
main = do
    fpSecret <-
        execParser $
        info (helper <*> parser) (fullDesc <> progDesc "RSCoin's Bank")
    let fpPublic = fpSecret <> ".pub"
    (sk,pk) <- keyGen
    writePublicKey fpPublic pk
    writeSecretKey fpSecret sk
