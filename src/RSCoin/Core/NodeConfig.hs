{-# LANGUAGE TemplateHaskell #-}

-- | This module provides configuration context for running nodes of rscoin.

module RSCoin.Core.NodeConfig
        ( Host
        , NetworkAddress
        , NodeContext (..)
        , Port

          -- * 'NodeContext' lenses
        , bankAddr
        , bankPublicKey
        , bankSecretKey
        , ctxLoggerName
        , notaryAddr

          -- * Other lenses
        , bankHost
        , bankPort
        , genesisAddress
        , notaryPort

          -- * Hardcoded constants for tests and benchmarks
        , defaultNodeContext
        , testBankPublicKey
        , testBankSecretKey

          -- * Functions to read context from configuration file
        , readDeployNodeContext
        ) where

import           Control.Exception          (Exception, throwIO)
import           Control.Lens               (Getter, Lens', makeLenses, to,
                                             (^.), _1, _2)
import           Control.Monad              (when)

import           Data.Bifunctor             (second)
import           Data.ByteString            (ByteString)
import qualified Data.Configurator          as Config
import qualified Data.Configurator.Types    as Config
import           Data.Maybe                 (fromJust, fromMaybe, isNothing)
import           Data.String                (IsString)
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)

import           Formatting                 (build, sformat, stext, (%))

import           RSCoin.Core.Constants      (defaultConfigurationPath,
                                             defaultPort, localhost)
import           RSCoin.Core.Crypto.Signing (PublicKey, SecretKey,
                                             constructPublicKey,
                                             derivePublicKey,
                                             deterministicKeyGen)
import           RSCoin.Core.Logging        (LoggerName, nakedLoggerName)
import           RSCoin.Core.Primitives     (Address (..))


type Port = Int
type Host = ByteString
type NetworkAddress = (Host, Port)

data NodeContext = NodeContext
    { _bankAddr      :: NetworkAddress
    , _notaryAddr    :: NetworkAddress
    , _bankPublicKey :: PublicKey
    , _bankSecretKey :: Maybe SecretKey  -- @TODO: not type-safe solution, but ok for now
    , _ctxLoggerName :: LoggerName
    } deriving (Show)

$(makeLenses ''NodeContext)

-- | Default node context for local deployment
defaultNodeContext :: NodeContext
defaultNodeContext = NodeContext{..}
  where
    _bankAddr      = (localhost, defaultPort)
    _notaryAddr    = (localhost, 4001)
    (_bankPublicKey, _bankSecretKey) = fromMaybe
        (error "[FATAL] Failed to construct (pk, sk) pair for default context")
        $ second Just <$> deterministicKeyGen "default-node-context-keygen-seed"
    _ctxLoggerName    = nakedLoggerName

bankHost :: Lens' NodeContext Host
bankHost = bankAddr . _1

bankPort :: Getter NodeContext Port
bankPort = bankAddr . _2

notaryPort :: Getter NodeContext Port
notaryPort = notaryAddr . _2

-- | Special address used as output in genesis transaction
genesisAddress :: Getter NodeContext Address
genesisAddress = bankPublicKey . to Address

-- | This Bank public key should be used only for tests and benchmarks.
testBankPublicKey :: PublicKey
testBankPublicKey = defaultNodeContext ^. bankPublicKey

-- | This Bank secret key should be used only for tests and benchmarks.
testBankSecretKey :: SecretKey
testBankSecretKey = fromJust $ defaultNodeContext ^. bankSecretKey

bankPublicKeyPropertyName :: IsString s => s
bankPublicKeyPropertyName = "bank.publicKey"

readRequiredDeployContext :: Maybe FilePath -> IO (Config.Config, NodeContext)
readRequiredDeployContext configPath = do
    confFile <- defaultConfigurationPath
    deployConfig <-
        Config.load [Config.Required (fromMaybe confFile configPath)]

    cfgBankHost <- Config.require deployConfig "bank.host"
    cfgBankPort <- Config.require deployConfig "bank.port"
    cfgNotaryHost <- Config.require deployConfig "notary.host"
    cfgNotaryPort <- Config.require deployConfig "notary.port"

    let obtainedContext =
            defaultNodeContext
            { _bankAddr = (cfgBankHost, cfgBankPort)
            , _notaryAddr = (cfgNotaryHost, cfgNotaryPort)
            }
    return (deployConfig, obtainedContext)

data ConfigurationReadException
    = ConfigurationReadException T.Text
    deriving (Show, Typeable)

instance Exception ConfigurationReadException

-- | Read config from 'defaultConfigurationPath' and converts into 'NodeContext'.
-- Tries to read also bank public key if it is not provided. If provied, then rewrites
-- configuration file.
readDeployNodeContext :: Maybe SecretKey -> Maybe FilePath -> IO NodeContext
readDeployNodeContext (Just newBankSecretKey) confPath = do
    (deployConfig, obtainedContext) <- readRequiredDeployContext confPath

    cfgBankPublicKey <- Config.lookup deployConfig bankPublicKeyPropertyName
    when (isNothing cfgBankPublicKey)
        $ throwIO $ ConfigurationReadException
        $ sformat ("Configuration file doesn't have property: " % stext) bankPublicKeyPropertyName

    let Just cfgReadPublicKey = cfgBankPublicKey
    let newBankPublicKey      = derivePublicKey newBankSecretKey
    let pkConfigValue         = Config.String $ sformat build newBankPublicKey
    when (pkConfigValue /= cfgReadPublicKey)
        $ throwIO $ ConfigurationReadException
        $ sformat ("Bank's derived PK " % build % " doesn't match PK in cfg file") newBankPublicKey

    return obtainedContext
        { _bankPublicKey = newBankPublicKey
        , _bankSecretKey = Just newBankSecretKey }
readDeployNodeContext Nothing confPath = do
    (deployConfig, obtainedContext) <- readRequiredDeployContext confPath
    cfgBankPublicKey  <- Config.require deployConfig bankPublicKeyPropertyName
    case constructPublicKey cfgBankPublicKey of
        Nothing -> throwIO
            $ ConfigurationReadException
            $ sformat (stext % " is not a valid public key in config file") cfgBankPublicKey
        Just pk ->
            return obtainedContext { _bankPublicKey = pk }
