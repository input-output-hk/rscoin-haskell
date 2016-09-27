module Data.I18N where

import Prelude (class Show, show, class Eq, ($), (<<<))

import Data.Maybe (Maybe (..))
import Data.Functor ((<$>))
import Data.String (take)

import DOM (DOM)
import Control.Monad.Eff (Eff)

import LanguagePack.Translation (english, japanese, russian)

foreign import detectLanguage_ :: forall e. Eff (dom :: DOM | e) String

detectLanguage :: forall e. Eff (dom :: DOM | e) (Maybe Language)
detectLanguage = readLanguage <<< take 2 <$> detectLanguage_

type Translation =
    { aboutUs          :: String
    , contacts         :: String
    , privacyPolicy    :: String
    , termsOfService   :: String
    , address          :: String
    , transaction      :: String
    , transactions     :: String
    , finalBalance     :: String
    , colorBalance     :: String
    , qrCode           :: String
    , to               :: String
    , notFound         :: String
    , receivedTime     :: String
    , totalInput       :: String
    , totalOutput      :: String
    , includedInBlocks :: String
    , summary          :: String
    , currentBlocks    :: String
    , transactionsFeed :: String
    , height           :: String
    , age              :: String
    , totalSent        :: String
    , expand           :: String
    , emission         :: String
    , _qrCodeMsg       :: String
    , minute           :: String
    , minutes          :: String
    , hours            :: String
    , days             :: String
    }

data Language
    = English
    | Japanese
    | Russian

instance showLanguage :: Show Language where
    show = languageNativeName

derive instance eqLanguage :: Eq Language
-- derive instance enumLanguage :: Enum Language


getTranslation :: Language -> Translation
getTranslation lang@English = english
getTranslation lang@Japanese = japanese
getTranslation lang@Russian = russian

allLanguages :: Array Language
allLanguages = [English, Japanese, Russian]

readBool :: String -> Maybe Boolean
readBool "true" = Just true
readBool "false" = Just false
readBool _ = Nothing

-- | ISO 639-1 https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
readLanguage :: String -> Maybe Language
readLanguage "en" = Just English
readLanguage "ja" = Just Japanese
readLanguage "ru" = Just Russian
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
