module Serokell.Data.DateTime where

import Prelude       ((<>), (<<<), ($), show)

import Data.DateTime (DateTime (..))
import Data.Date 	 (day, month, year)
import Data.Time 	 (hour, minute, second, millisecond)
import Data.Enum     (fromEnum)

prettyDate :: DateTime -> String
prettyDate = formatDate <<< toRecord
  where
    formatDate dt = dt.year <> "-" <> dt.month <> "-" <> dt.day <> " " <> dt.hour <> ":" <> dt.minute <> ":" <> dt.second

type DateRec =
  { year :: String
  , month :: String
  , day :: String
  , hour :: String
  , minute :: String
  , second :: String
  , millisecond :: String
}

toRecord :: DateTime -> DateRec
toRecord (DateTime d t) =
  { year: show $ fromEnum (year d)
  , month: show $ fromEnum (month d)
  , day: show $ fromEnum (day d)
  , hour: show $ fromEnum (hour t)
  , minute: show $ fromEnum (minute t)
  , second: show $ fromEnum (second t)
  , millisecond: show $ fromEnum (millisecond t)
}
