{-# LANGUAGE TupleSections #-}

import           Control.Concurrent      (ThreadId, forkIO, threadDelay)
import           Data.Maybe              (fromMaybe)
import           Data.String.Conversions (cs)
import           Formatting              (int, sformat, shown, stext, string,
                                          (%))
import qualified Options.Applicative     as Opts
import           Serokell.Util.OptParse  (strArgument)
import           System.FilePath         ((</>))
import           System.IO.Temp          (withTempDirectory)
import qualified Turtle                  as Cherepakha

import qualified RSCoin.Core             as C

import           Config                  (DeployConfig (..), MintetteData (..),
                                          readDeployConfig)

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
    } deriving (Show)

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
    (, key) <$> forkIO (() <$ Cherepakha.shellStrict fullRunCommand mempty)

main :: IO ()
main = do
    DeployConfig{..} <- readDeployConfig =<< getConfigPath
    absoluteDir <-
        ((</> dcDirectory) . cs . either (error . show) id . Cherepakha.toText) <$>
        Cherepakha.pwd
    withTempDirectory absoluteDir "rscoin-deploy" $
        \tmpDir ->
             do let cp =
                        CommonParams
                        { cpExec = dcExec
                        , cpBaseDir = tmpDir
                        }
                (mintetteThreads,mintetteKeys) <-
                    unzip <$> mapM (startMintette cp) (zip [0 ..] dcMintettes)
                threadDelay 10000000000
