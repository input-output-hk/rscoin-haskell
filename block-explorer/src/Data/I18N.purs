module Data.I18N where

import Prelude (class Show, show, class Eq)

import Data.Maybe (Maybe (..))

data Language
    = English
    | Japanese
    | Russian


instance showLanguage :: Show Language where
    show = languageNativeName

derive instance eqLanguage :: Eq Language

readBool :: String -> Maybe Boolean
readBool "true" = Just true
readBool "false" = Just false
readBool _ = Nothing

-- | ISO 639 https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
readLanguage :: String -> Maybe Language
readLanguage "en" = Just English
readLanguage "eng" = Just English
readLanguage "engs" = Just English
readLanguage "ja" = Just Japanese
readLanguage "jpn" = Just Japanese
readLanguage "ru" = Just Russian
readLanguage "rus" = Just Russian
readLanguage _ = Nothing

-- | ISO 639-1 https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
languageCode :: Language -> String
languageCode English = "en"
languageCode Japanese = "ja"
languageCode Russian = "ru"

-- | ISO 639 https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
languageNativeName :: Language -> String
languageNativeName English = "English"
languageNativeName Japanese = "日本語"
languageNativeName Russian = "Русский"
