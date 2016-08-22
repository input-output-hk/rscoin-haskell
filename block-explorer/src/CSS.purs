module App.CSS where

import Prelude                        (($), (<>))

import Pux.CSS                        (rgb, Color)

import CSS.String                     (fromString)
import CSS.Stylesheet                 (CSS, key)

darkRed :: Color
darkRed = rgb 126 0 0

imagePath :: String -> String
imagePath i = "image/" <> i

logoPath :: String
logoPath = imagePath "logo-copy.png"

headerBitmapPath :: String
headerBitmapPath = imagePath "header-bitmap-copy.png"

opacity :: Number -> CSS
opacity = key $ fromString "opacity"


