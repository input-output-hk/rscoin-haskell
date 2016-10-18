module App.CSS where

import Prelude                        (($), (<>))

import Pux.CSS                        (rgb, Color, border, solid, nil,
                                       white, height, px, (?), body,
                                       backgroundColor, td)

import CSS.String                     (fromString)
import CSS.Stylesheet                 (CSS, key)

darkRed :: Color
darkRed = rgb 126 0 0

veryLightGrey :: Color
veryLightGrey = rgb 251 251 251

lightGrey :: Color
lightGrey = rgb 242 242 242

imagePath :: String -> String
imagePath i = "/image/" <> i

logoPath :: String
logoPath = imagePath "logo.svg"

switchOnPath :: String
switchOnPath = imagePath "switch-on.svg"

switchOffPath :: String
switchOffPath = imagePath "switch-off.svg"

logoSmallPath :: String
logoSmallPath = imagePath "logo-small.png"

headerBitmapPath :: String
headerBitmapPath = imagePath "header-bitmap.png"

adaSymbolDarkPath :: String
adaSymbolDarkPath = imagePath "ada-currency-symbol-dark.svg"

adaSymbolPath :: String
adaSymbolPath = imagePath "ada-currency-symbol.svg"

transactionArrowIncomePath :: String
transactionArrowIncomePath = imagePath "transaction-arrow-income.svg"

transactionArrowOutcomePath :: String
transactionArrowOutcomePath = imagePath "transaction-arrow-outcome.svg"

transactionArrowNeutralPath :: String
transactionArrowNeutralPath = imagePath "transaction-arrow-neutral.svg"

opacity :: Number -> CSS
opacity = key $ fromString "opacity"

noBorder :: CSS
noBorder = border solid nil white

headFootHeight :: CSS
headFootHeight = height $ px 72.0

styleSheet :: CSS
styleSheet =
    td ? noBorder
