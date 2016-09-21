module Serokell.Data.DateTime where

import Prelude       ((<>), (<<<), ($), show, (<=))

import Data.DateTime (DateTime (..))
import Data.Date 	 (day, month, year)
import Data.Time 	 (hour, minute, second, millisecond)
import Data.Enum     (fromEnum)
import Data.String   (length)

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
    , month: paddZero $ show $ fromEnum (month d)
    , day: paddZero $ show $ fromEnum (day d)
    , hour: paddZero $ show $ fromEnum (hour t)
    , minute: paddZero $ show $ fromEnum (minute t)
    , second: paddZero $ show $ fromEnum (second t)
    , millisecond: paddZero $ show $ fromEnum (millisecond t)
    }
  where
    -- NOTE: this is assuming `length s <= 2`
    paddZero s =
        if length s <= 1
            then "0" <> s
            else s
