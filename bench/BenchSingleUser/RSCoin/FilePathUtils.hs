module BenchSingleUser.RSCoin.FilePathUtils
        ( dbFormatPath
        , tempBenchDirectory
        ) where

import           Data.Text.Lazy (Text, unpack)
import           Formatting     (format, int, text, (%))

tempBenchDirectory :: FilePath
tempBenchDirectory = ".bench-local"

dbFormatPath :: Integral i => Text -> i -> String
dbFormatPath dbPath num = unpack $ format (text % int) dbPath num
