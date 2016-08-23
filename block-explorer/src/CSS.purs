module App.CSS where

import Prelude                        (($), (<>))

import Pux.CSS                        (rgb, Color, border, solid, nil, white, height, px)

import CSS.String                     (fromString)
import CSS.Stylesheet                 (CSS, key)

darkRed :: Color
darkRed = rgb 126 0 0

veryLightGrey :: Color
veryLightGrey = rgb 251 251 251

lightGrey :: Color
lightGrey = rgb 242 242 242

imagePath :: String -> String
imagePath i = "image/" <> i

logoPath :: String
logoPath = imagePath "logo-copy.png"

logoSmallPath :: String
logoSmallPath = imagePath "logo-small.png"

headerBitmapPath :: String
headerBitmapPath = imagePath "header-bitmap-copy.png"

adaSymbolPath :: String
adaSymbolPath = imagePath "ada-currency-symbol-dark.png"

opacity :: Number -> CSS
opacity = key $ fromString "opacity"

noBorder :: CSS
noBorder = border solid nil white

headFootHeight :: CSS
headFootHeight = height $ px 50.0 -- TODO: this should be 72 px
