{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

import           Control.Concurrent      (ThreadId, forkIO, killThread)
import           Control.Exception       (finally)

import           Control.Monad           (forM_)
import           Control.Monad.Catch     (bracket)
import           Control.Monad.Extra     (whenJust)
import           Control.Monad.Trans     (MonadIO (liftIO))

import qualified Data.Acid               as ACID
import           Data.Maybe              (fromMaybe)
import           Data.String.Conversions (cs)
import           Formatting              (sformat, stext, string, (%))
import qualified Options.Applicative     as Opts
import           Serokell.Util.OptParse  (strArgument)
import           System.FilePath         ((</>))
import           System.IO.Temp          (withTempDirectory)
import qualified Turtle                  as Cherepakha

import qualified RSCoin.Bank             as B
import qualified RSCoin.Core             as C
import qualified RSCoin.Explorer         as E
import qualified RSCoin.Mintette         as M
import qualified RSCoin.Notary           as N
import           RSCoin.Timed            (ContextArgument (CADefault),
                                          Millisecond, runRealModeUntrusted)
import qualified RSCoin.User             as U

import           Config                  (DeployConfig (..), readDeployConfig)

optionsParser :: Opts.Parser FilePath
optionsParser =
    strArgument $
    mconcat
        [Opts.value "local.yaml", Opts.showDefault, Opts.metavar "FILEPATH"]

getConfigPath :: IO FilePath
getConfigPath =
    Opts.execParser $
    Opts.info
        (Opts.helper <*> optionsParser)
        (mconcat
             [ Opts.fullDesc
             , Opts.progDesc "Wrapper tool to deploy rscoin locally"])

data CommonParams = CommonParams
    { cpBaseDir :: !FilePath
    , cpPeriod  :: !Millisecond
    , cpEpoch   :: !Millisecond
    } deriving (Show)

contextArgument :: ContextArgument
contextArgument = CADefault

bankSecretKey :: C.SecretKey
bankSecretKey = C.testBankSecretKey

toDeprecatedFilePath :: FilePath -> Cherepakha.FilePath
toDeprecatedFilePath = Cherepakha.fromText . cs

startMintette :: CommonParams -> Word -> IO (ThreadId, C.PublicKey)
startMintette CommonParams{..} idx = do
    let workingDir = cpBaseDir </> mconcat ["mintette-workspace-", show idx]
        workingDirDeprecated = toDeprecatedFilePath workingDir
        port = mintettePort idx
        dbDir = workingDir </> "mintette-db"
    Cherepakha.mkdir workingDirDeprecated
    (sk,pk) <- C.keyGen
    let start =
            M.launchMintetteReal cpEpoch port sk (Just dbDir) contextArgument
    (, pk) <$> forkIO start

startExplorer
    :: Maybe C.Severity
    -> CommonParams
    -> Word
    -> IO (ThreadId, C.PublicKey)
startExplorer severity CommonParams{..} idx = do
    let workingDir = cpBaseDir </> mconcat ["explorer-workspace-", show idx]
        workingDirDeprecated = toDeprecatedFilePath workingDir
        portRpc = explorerPort idx
        portWeb = explorerWebPort idx
        dbDir = workingDir </> "explorer-db"
    Cherepakha.mkdir workingDirDeprecated
    (sk,pk) <- C.keyGen
    let start =
            E.launchExplorerReal
                portRpc
                portWeb
                (fromMaybe C.Warning severity)
                dbDir
                contextArgument
                sk
    (, pk) <$> forkIO start

startNotary :: Maybe C.Severity
            -> CommonParams
            -> IO ThreadId
startNotary severity CommonParams{..} = do
    let workingDir = cpBaseDir </> "notary-workspace"
        workingDirDeprecated = toDeprecatedFilePath workingDir
        dbDir = workingDir </> "notary-db"
        start =
            N.launchNotaryReal
                (fromMaybe C.Warning severity)
                (Just dbDir)
                contextArgument
                8090
    Cherepakha.mkdir workingDirDeprecated
    forkIO start

type PortsAndKeys = [(Int, C.PublicKey)]

startBank
    :: CommonParams
    -> PortsAndKeys
    -> PortsAndKeys
    -> IO ThreadId
startBank CommonParams{..} mintettes explorers = do
    let workingDir = cpBaseDir </> "bank-workspace"
        workingDirDeprecated = toDeprecatedFilePath workingDir
        dbDir = workingDir </> "bank-db"
    Cherepakha.mkdir workingDirDeprecated
    forM_
        explorers
        (\(port,key) ->
              B.addExplorerInPlace
                  contextArgument
                  bankSecretKey
                  dbDir
                  (C.Explorer C.localhost port key)
                  0)
    forM_
        mintettes
        (\(port,key) ->
              B.addMintetteInPlace
                  contextArgument
                  bankSecretKey
                  dbDir
                  (C.Mintette C.localhost port)
                  key)
    forkIO $ B.launchBankReal cpPeriod dbDir contextArgument bankSecretKey

-- TODO: we can setup other users similar way
setupBankUser :: CommonParams -> IO ()
setupBankUser CommonParams{..} = do
    let workingDir = cpBaseDir </> "user-workspace-bank"
        workingDirDeprecated = toDeprecatedFilePath workingDir
        dbDir = workingDir </> "wallet-db"
        addressesNum = 5
        walletPathArg = sformat (" --wallet-path " % string % " ") dbDir
        skPath = workingDir </> "secret-key"
    Cherepakha.mkdir workingDirDeprecated
    C.writeSecretKey skPath bankSecretKey
    runRealModeUntrusted C.userLoggerName contextArgument $
        bracket
            (liftIO $ U.openState dbDir)
            (\st ->
                  liftIO $
                  do ACID.createCheckpoint st
                     U.closeState st) $
        \st ->
             U.initState st addressesNum $ Just skPath
    Cherepakha.echo $
        sformat ("Initialized bank user, db is stored in " % string) dbDir
    -- doesn't work, invent something better
    -- Cherepakha.export "BU_ARG" walletPathArg
    Cherepakha.echo
        "Use command below to do smth as bank user"
    Cherepakha.echo $
        sformat ("stack $NIX_STACK exec -- rscoin-user --local " % stext) walletPathArg

mintettePort :: Integral a => a -> Int
mintettePort = (C.defaultPort + 1 +) . fromIntegral

explorerPort :: Integral a => a -> Int
explorerPort = (C.defaultPort + 3000 +) . fromIntegral

explorerWebPort :: Integral a => a -> Int
explorerWebPort = (C.defaultPort + 5000 +) . fromIntegral

main :: IO ()
main = do
    DeployConfig{..} <- readDeployConfig =<< getConfigPath
    let makeAbsolute path =
            ((</> path) . cs . either (error . show) id . Cherepakha.toText) <$>
            Cherepakha.pwd
        maybeInitLogger mSev name =
            whenJust mSev $ flip C.initLoggerByName name
    C.initLogging dcGlobalSeverity
    maybeInitLogger dcBankSeverity C.bankLoggerName
    maybeInitLogger dcNotarySeverity C.notaryLoggerName
    maybeInitLogger dcMintetteSeverity C.mintetteLoggerName
    maybeInitLogger dcExplorerSeverity C.explorerLoggerName
    absoluteDir <- makeAbsolute dcDirectory
    withTempDirectory absoluteDir "rscoin-deploy" $
        \tmpDir -> do
            let cp =
                    CommonParams
                    { cpBaseDir = tmpDir
                    , cpPeriod = fromIntegral dcPeriod
                    , cpEpoch = fromIntegral dcEpoch
                    }
                mintetteIndices = [1 .. dcMintettes]
                explorerIndices = [1 .. dcExplorers]
                mintettePorts = map mintettePort mintetteIndices
                explorerPorts = map explorerPort explorerIndices
            (mintetteThreads,mintetteKeys) <-
                unzip <$> mapM (startMintette cp) mintetteIndices
            (explorerThreads,explorerKeys) <-
                unzip <$>
                mapM (startExplorer dcExplorerSeverity cp) explorerIndices
            let mintettes = zip mintettePorts mintetteKeys
                explorers = zip explorerPorts explorerKeys
            notaryThread <- startNotary dcNotarySeverity cp
            bankThread <- startBank cp mintettes explorers
            setupBankUser cp
            Cherepakha.echo "Deployed successfully!"
            Cherepakha.sleep 100500 `finally`
                (mapM_ killThread $
                 bankThread : notaryThread : mintetteThreads ++ explorerThreads)
