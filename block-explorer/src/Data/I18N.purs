module Data.I18N where

import Prelude (class Show, show, class Eq, ($))

import Data.Maybe (Maybe (..))

-- import Data.Enum (class Enum, enumFromTo)

type Translation =
    { _nativeName      :: String
    , _code            :: String
    , aboutUs          :: String
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
getTranslation lang@English =
    { _nativeName: languageNativeName lang
    , _code: languageCode lang
    , aboutUs: "About us"
    , contacts: "Contacts"
    , privacyPolicy: "Privacy Policy"
    , termsOfService: "Terms of Service"
    , address: "Address"
    , transaction: "Transaction"
    , transactions: "Transactions"
    , finalBalance: "Final balance"
    , colorBalance: "Color balance"
    , qrCode: "QR code"
    , to: "to"
    , notFound: "Not Found"
    , receivedTime: "Received Time"
    , totalInput: "Total Input"
    , totalOutput: "Total Output"
    , includedInBlocks: "Included In Blocks"
    , summary: "Summary"
    , currentBlocks: "Current Blocks"
    , transactionsFeed: "Transactions Feed"
    , height: "Height"
    , age: "Age"
    , totalSent: "Total Sent"
    , expand: "Expand"
    , emission: "Emission"
    , _qrCodeMsg: "Scan this QR Code to copy address to clipboard"
    , minute: "minute"
    , minutes: "minutes"
    , hours: "hours"
    , days: "days"
    }
getTranslation lang@Japanese =
    { _nativeName: languageNativeName lang
    , _code: languageCode lang
    , aboutUs: "私たちに関しては"
    , contacts: "コンタクト"
    , privacyPolicy: "個人情報保護方針"
    , termsOfService: "利用規約"
    , address: "アドレス"
    , transaction: "トランザクション"
    , transactions: "トランザクション"
    , finalBalance: "最終残高"
    , colorBalance: "各色の残高"
    , qrCode: "QRコード"
    , to: "に"
    , notFound: "見つかりません"
    , receivedTime: "受信時間"
    , totalInput: "インプット合計"
    , totalOutput: "アウトプット合計"
    , includedInBlocks: "ブロックに含む"
    , summary: "要約"
    , currentBlocks: "?"
    , transactionsFeed: "?"
    , height: "?"
    , age: "?"
    , totalSent: "?"
    , expand: "?"
    , emission: "?"
    , _qrCodeMsg: "?"
    , minute: "?"
    , minutes: "?"
    , hours: "?"
    , days: "?"
    }
getTranslation lang@Russian =
    { _nativeName: languageNativeName lang
    , _code: languageCode lang
    , aboutUs: "О нас"
    , contacts: "контакты"
    , privacyPolicy: "политика конфиденциальности"
    , termsOfService: "Условия использования"
    , address: "Адрес"
    , transaction: "?"
    , transactions: "операции"
    , finalBalance: "Окончательный баланс"
    , colorBalance: "Цветовой баланс"
    , qrCode: "QR код"
    , to: "к"
    , notFound: "не обнаружена"
    , receivedTime: "?"
    , totalInput: "?"
    , totalOutput: "?"
    , includedInBlocks: "?"
    , summary: "?"
    , currentBlocks: "?"
    , transactionsFeed: "?"
    , height: "?"
    , age: "?"
    , totalSent: "?"
    , expand: "?"
    , emission: "?"
    , _qrCodeMsg: "?"
    , minute: "?"
    , minutes: "?"
    , hours: "?"
    , days: "?"
    }

allLanguages :: Array Language
allLanguages = [English, Japanese, Russian]

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
