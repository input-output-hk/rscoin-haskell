module Data.I18N where

import Prelude (class Show, show, class Eq, ($), (<<<))

import Data.Maybe (Maybe (..))
import Data.Functor ((<$>))
import Data.String (take)

import DOM (DOM)
import Control.Monad.Eff (Eff)

import LanguagePack.Translation (english, japanese, russian, chinese, korean)

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
    | Chinese
    | Korean
    | Russian

instance showLanguage :: Show Language where
    show = languageNativeName

derive instance eqLanguage :: Eq Language
-- derive instance enumLanguage :: Enum Language

getTranslation :: Language -> Translation
getTranslation lang@English = english
getTranslation lang@Japanese = japanese
getTranslation lang@Chinese = chinese
getTranslation lang@Korean = korean
getTranslation lang@Russian = russian

allLanguages :: Array Language
allLanguages = [English, Japanese, Chinese, Korean, Russian]

-- refer to this for links to other languages https://input-output-rnd.slack.com/archives/project-avm/p1475563773001831
footerAboutUs :: Language -> String
footerAboutUs Japanese = "http://attaincorp.co.jp/company/"
footerAboutUs English = "http://attaincorp.co.jp/en/company/"
footerAboutUs Korean = "http://attaincorp.co.jp/korea/company/"
footerAboutUs Chinese = "http://attaincorp.co.jp/ch_kan/company/"
footerAboutUs Russian = "#"

footerContactUs :: Language -> String
footerContactUs Japanese = "http://attaincorp.co.jp/inq/"
footerContactUs English = "http://attaincorp.co.jp/en/inq/"
footerContactUs Korean = "http://attaincorp.co.jp/korea/inq/"
footerContactUs Chinese = "http://attaincorp.co.jp/ch_kan/inq/"
footerContactUs Russian = "#"

footerPrivacyPolicy :: Language -> String
footerPrivacyPolicy Japanese = "http://attaincorp.co.jp/rule/policy.html"
footerPrivacyPolicy English = "http://attaincorp.co.jp/en/rule/policy.html"
footerPrivacyPolicy Korean = "http://attaincorp.co.jp/korea/rule/policy.html"
footerPrivacyPolicy Chinese = "http://attaincorp.co.jp/ch_kan/rule/policy.html"
footerPrivacyPolicy Russian = "#"

footerTermsOfService :: Language -> String
footerTermsOfService Japanese = "http://attaincorp.co.jp/rule/rule.html"
footerTermsOfService English = "http://attaincorp.co.jp/en/rule/rule.html"
footerTermsOfService Korean = "http://attaincorp.co.jp/korea/rule/rule.html"
footerTermsOfService Chinese = "http://attaincorp.co.jp/ch_kan/rule/rule.html"
footerTermsOfService Russian = "#"

readBool :: String -> Maybe Boolean
readBool "true" = Just true
readBool "false" = Just false
readBool _ = Nothing

-- | ISO 639-1 https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
readLanguage :: String -> Maybe Language
readLanguage "en" = Just English
readLanguage "ja" = Just Japanese
readLanguage "kr" = Just Korean
readLanguage "ch" = Just Chinese
readLanguage "ru" = Just Russian
readLanguage _ = Nothing

-- | ISO 639-1 https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
languageCode :: Language -> String
languageCode English = "en"
languageCode Japanese = "ja"
languageCode Korean = "kr"
languageCode Chinese = "ch"
languageCode Russian = "ru"

-- | ISO 639 https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes
languageNativeName :: Language -> String
languageNativeName English = "English"
languageNativeName Japanese = "日本語"
languageNativeName Korean = "한국의"
languageNativeName Chinese = "中國"
languageNativeName Russian = "Русский"
