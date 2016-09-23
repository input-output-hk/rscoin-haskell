module Serokell.Data.DateTime where

import Prelude            ((<>), (<<<), ($), show, (<=), (<), otherwise, (==), map, sub)

import Data.DateTime      (DateTime (..))
import Data.Date          (day, month, year)
import Data.Time          (hour, minute, second, millisecond)
import Data.Time.Duration (class Duration, Minutes (..), Hours (..), Days (..),
                           unMinutes, unHours, unDays, convertDuration)
import Data.Enum          (fromEnum)
import Data.String        (length, joinWith, trim)
import Data.Int           (floor, toNumber)
import Data.Tuple         (uncurry, Tuple (..))

prettyDate :: DateTime -> String
prettyDate = formatDate <<< toRecord
  where
    formatDate dt = dt.year <> "-" <> dt.month <> "-" <> dt.day <> " " <> dt.hour <> ":" <> dt.minute <> ":" <> dt.second

prettyDuration :: forall a. Duration a => a -> String
prettyDuration dur | convertDuration dur < Minutes 1.0 = "< 1 minute"
                   | otherwise = trim $ joinWith " " $ map (uncurry showIfNonZero)
                        [ Tuple d "days"
                        , Tuple h "hours"
                        , Tuple m "minutes"
                        ]
  where
    m = floor <<< unMinutes $ convertDuration dur `sub` convertDuration (Days $ toNumber d) `sub` convertDuration (Hours $ toNumber h)
    h = floor <<< unHours $ convertDuration dur `sub` convertDuration (Days $ toNumber d)
    d = floor <<< unDays $ convertDuration dur
    showIfNonZero nu st =
        if nu == 0
            then ""
            else show nu <> " " <> st

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
