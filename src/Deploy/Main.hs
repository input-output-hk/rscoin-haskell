{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

import           Control.Concurrent      (ThreadId, forkIO, killThread)
import           Control.Exception       (finally)

import           Control.Monad           (forM_)
import           Control.Monad.Catch     (bracket)
import           Control.Monad.Extra     (whenJust)

import           Data.Maybe              (fromMaybe)
import           Data.Optional           (Optional (Default))
import           Data.String.Conversions (cs)
import           Formatting              (sformat, stext, string, (%))
import qualified Options.Applicative     as Opts
import           Serokell.Util.OptParse  (strArgument)
import           System.FilePath         ((</>))
import           System.IO.Temp          (withTempDirectory)
import qualified Turtle                  as Cherepakha

import           Control.TimeWarp.Timed  (Millisecond)
import qualified RSCoin.Bank             as B
import qualified RSCoin.Core             as C
import qualified RSCoin.Explorer         as E
import qualified RSCoin.Mintette         as M
import qualified RSCoin.Notary           as N
import qualified RSCoin.User             as U

import           Config                  (DeployConfig (..), readDeployConfig)

optionsParser :: Opts.Parser FilePath
optionsParser =
    strArgument $
    mconcat
        [ Opts.value "local.yaml"
        , Opts.showDefault
        , Opts.metavar "CONFIG"
        , Opts.help "Path to deployment config" ]
{-    Opts.switch $
    mconcat
        [ Opts.short 'r', Opts.long "rebuild-db",
          Opts.help "Erase databse if it already exists"]-}

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
    , cpRebuild :: !Bool
    } deriving (Show)

contextArgument :: C.ContextArgument
contextArgument = C.CADefault

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
    let env = M.mkRuntimeEnv 1000000000 sk
        start =
            M.launchMintetteReal cpRebuild cpEpoch port env (Just dbDir) contextArgument
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
                cpRebuild
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
startNotary severity CommonParams {..} = do
    let workingDir = cpBaseDir </> "notary-workspace"
        workingDirDeprecated = toDeprecatedFilePath workingDir
        dbDir = workingDir </> "notary-db"
        start =
            N.launchNotaryReal
                (fromMaybe C.Warning severity)
                cpRebuild
                C.testNotarySecretKey
                (Just dbDir)
                contextArgument
                8090
                []
                Default
                Default
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
    forkIO $ B.launchBankReal cpRebuild  cpPeriod dbDir contextArgument bankSecretKey

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
    C.runRealModeUntrusted C.userLoggerName contextArgument $
        bracket (U.openState cpRebuild dbDir) U.closeState $
        \st ->
             U.initState st addressesNum $ Just skPath
    Cherepakha.echo $
        sformat ("Initialized bank user, db is stored in " % string) dbDir
    -- doesn't work, invent something better
    -- Cherepakha.export "BU_ARG" walletPathArg
    Cherepakha.echo
        "Use command below to do smth as bank user"
    Cherepakha.echo $
        sformat
            ("stack $NIX_STACK exec -- rscoin-user --default-context " % stext)
            walletPathArg

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
                    , cpRebuild = False
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
