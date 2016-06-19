module Bench.RSCoin.FilePathUtils
        ( dbFormatPath
        , tempBenchDirectory
        , walletPathPrefix
        ) where

import           Data.String (IsString)
import           Data.Text   (Text, unpack)
import           Formatting  (int, sformat, stext, (%))

tempBenchDirectory :: FilePath
tempBenchDirectory = ".bench-local"

walletPathPrefix :: IsString s => s
walletPathPrefix = "wallet-db"

dbFormatPath :: Integral i => Text -> i -> FilePath
dbFormatPath dbPath num = unpack $ sformat (stext % int) dbPath num