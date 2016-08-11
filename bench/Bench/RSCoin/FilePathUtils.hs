module Bench.RSCoin.FilePathUtils
        ( benchConfPath
        , dbFormatPath
        , tempBenchDirectory
        , walletPathPrefix
        ) where

import           Data.String           (IsString)
import           Data.Text             (Text, unpack)
import           Formatting            (int, sformat, stext, (%))
import           System.FilePath       ((</>))

import           RSCoin.Core.Constants (defaultConfigurationFileName)

tempBenchDirectory :: FilePath
tempBenchDirectory = ".bench-local"

walletPathPrefix :: IsString s => s
walletPathPrefix = "wallet-db"

dbFormatPath :: Integral i => Text -> i -> FilePath
dbFormatPath dbPath num = unpack $ sformat (stext % int) dbPath num

benchConfPath :: FilePath -> FilePath
benchConfPath benchDir = benchDir </> defaultConfigurationFileName
