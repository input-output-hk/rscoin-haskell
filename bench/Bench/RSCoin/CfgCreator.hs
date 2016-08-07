-- | This modules provides benchmarks with functions to create temporary configuration.

module Bench.RSCoin.CfgCreator
        ( createDeployConfiguration
        ) where

import           Data.Configurator.Export (renderHashMap)
import           Data.Configurator.Types  (Name, Value (Number, String))
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as HM hiding (HashMap)

import           Formatting               (build, sformat, string, (%))

import           RSCoin.Core.Constants    (defaultConfigurationFileName,
                                           defaultPort, localhost)
import           RSCoin.Core.NodeConfig   (testBankPublicKey)

import           Bench.RSCoin.Logging     (logDebug)

cfgHashMap :: HashMap Name Value
cfgHashMap = HM.fromList
    [ ("bank.host"     , String localhost)
    , ("bank.port"     , Number defaultPort)
    , ("bank.publicKey", String $ sformat build testBankPublicKey)

    , ("notary.host", String localhost)
    , ("notary.port", Number 9000)
    ]

-- | This function creates temporary configuration file for benchmarks.
-- Also this function uses test bank public key
createDeployConfiguration :: FilePath -> IO ()
createDeployConfiguration benchDir = do
    let benchConfig = renderHashMap cfgHashMap
    logDebug $ sformat ("Generated configuration:\n" % string) benchConfig
    writeFile (benchDir </> defaultConfigurationFileName) benchConfig