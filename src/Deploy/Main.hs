{-# LANGUAGE TupleSections #-}

import           Control.Concurrent       (ThreadId, forkIO, killThread,
                                           threadDelay)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception        (finally)
import           Control.Monad            (forM_)
import           Data.Maybe               (fromMaybe)
import           Data.String.Conversions  (cs)
import           Formatting               (build, int, sformat, shown, stext,
                                           string, (%))
import qualified Options.Applicative      as Opts
import           Serokell.Util.OptParse   (strArgument)
import           System.FilePath          ((</>))
import           System.IO.Temp           (withTempDirectory)
import qualified Turtle                   as Cherepakha

import qualified RSCoin.Core              as C

import           Config                   (BankData (..), DeployConfig (..),
                                           MintetteData (..), readDeployConfig)

optionsParser :: Opts.Parser FilePath
optionsParser =
    strArgument $ mconcat [Opts.value "local.yaml", Opts.showDefault]

getConfigPath :: IO FilePath
getConfigPath =
    Opts.execParser $
    Opts.info
        (Opts.helper <*> optionsParser)
        (mconcat
             [ Opts.fullDesc
             , Opts.progDesc "Wrapper tool to deploy rscoin locally"])

data CommonParams = CommonParams
    { cpExec    :: Cherepakha.Text
    , cpBaseDir :: FilePath
    , cpPeriod  :: Word
    } deriving (Show)

waitSec :: Word -> IO ()
waitSec = threadDelay . (* 1000000) . fromIntegral

startMintette :: CommonParams
              -> (Word, MintetteData)
              -> IO (ThreadId, C.PublicKey)
startMintette CommonParams{..} (idx,MintetteData{..}) = do
    let workingDir = cpBaseDir </> mconcat ["mintette-workspace-", show idx]
        workingDirModern = Cherepakha.fromText $ cs workingDir
        skPath = workingDir </> "key"
        pkPath = workingDir </> "key.pub"
        keyGenCommand = mconcat [cpExec, "rscoin-keygen"]
        fullKeyGenCommand = mconcat [keyGenCommand, " ", cs skPath]
        runCommand = mconcat [cpExec, "rscoin-mintette"]
        severityArg =
            maybe "" (sformat (" --log-severity " % shown)) mdSeverity
        port = fromMaybe C.defaultPort mdPort
        dbDir = workingDir </> "mintette-db"
        fullRunCommand =
            sformat
                ("cd " % string % "; " % stext % " --sk " % string % " --port " % int % stext %
                 " --bank-host 127.0.0.1 " %
                 " --path " %
                 string)
                workingDir
                runCommand
                skPath
                port
                severityArg
                dbDir
    Cherepakha.mkdir workingDirModern
    (Cherepakha.ExitSuccess,_) <-
        Cherepakha.shellStrict fullKeyGenCommand mempty
    key <-
        fromMaybe (error "FATAL: constructPulicKey failed") .
        C.constructPublicKey <$>
        (Cherepakha.readTextFile $ Cherepakha.fromText $ cs pkPath)
    waitSec 1
    (, key) <$> forkIO (() <$ Cherepakha.shellStrict fullRunCommand mempty)

startBank :: CommonParams -> [(Int, C.PublicKey)] -> BankData -> IO ()
startBank CommonParams{..} mintettePortsAndKeys BankData{..} = do
    let workingDir = cpBaseDir </> "bank-workspace"
        workingDirModern = Cherepakha.fromText $ cs workingDir
        dbDir = workingDir </> "bank-db"
        bankCommand = mconcat [cpExec, "rscoin-bank"]
        addMintetteCommand =
            sformat
                (stext % " --path " % string % " add-mintette " %
                 " --host 127.0.0.1 " %
                 " --port " %
                 int %
                 " --key " %
                 build)
                bankCommand
                dbDir
        severityArg =
            maybe "" (sformat (" --log-severity " % shown)) bdSeverity
        serveCommand =
            sformat
                ("cd " % string % "; " % stext % " --path " % string %
                 " --period-delta " %
                 int %
                 stext %
                 " serve " %
                 " --secret-key " %
                 string)
                workingDir
                bankCommand
                dbDir
                cpPeriod
                severityArg
                bdSecret
    Cherepakha.mkdir workingDirModern
    forM_
            mintettePortsAndKeys
            (\(port,key) ->
                  Cherepakha.shellStrict (addMintetteCommand port key) mempty)
    waitSec 1
    Cherepakha.echo "Deployed successfully!"
    () <$ Cherepakha.shellStrict serveCommand mempty

main :: IO ()
main = do
    DeployConfig{..} <- readDeployConfig =<< getConfigPath
    absoluteDir <-
        ((</> dcDirectory) . cs . either (error . show) id . Cherepakha.toText) <$>
        Cherepakha.pwd
    absoluteSecret <-
        ((</> bdSecret dcBank) .
         cs . either (error . show) id . Cherepakha.toText) <$>
        Cherepakha.pwd
    withTempDirectory absoluteDir "rscoin-deploy" $
        \tmpDir ->
             do let cp =
                        CommonParams
                        { cpExec = dcExec
                        , cpBaseDir = tmpDir
                        , cpPeriod = dcPeriod
                        }
                    bd =
                        dcBank
                        { bdSecret = absoluteSecret
                        }
                    mintettePorts =
                        map (fromMaybe C.defaultPort . mdPort) dcMintettes
                (mintetteThreads,mintetteKeys) <-
                    unzip <$>
                    mapConcurrently (startMintette cp) (zip [0 ..] dcMintettes)
                waitSec 2
                startBank cp (zip mintettePorts mintetteKeys) bd `finally`
                    mapM_ killThread mintetteThreads
