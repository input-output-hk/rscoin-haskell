module Bench.RSCoin.FilePathUtils
        ( dbFormatPath
        , defaultBankKey
        , tempBenchDirectory
        ) where

import           Data.Text.Lazy (Text, unpack)
import           Formatting     (format, int, text, (%))

tempBenchDirectory :: FilePath
tempBenchDirectory = ".bench-local"

dbFormatPath :: Integral i => Text -> i -> String
dbFormatPath dbPath num = unpack $ format (text % int) dbPath num

defaultBankKey :: String
defaultBankKey = "SecKey \"448d85e1261c2ce919bdbdf1b3830653e91380f4f22ef6d5b0edfb6537dd0772\""