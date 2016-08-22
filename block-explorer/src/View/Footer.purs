module App.View.Footer where

import Prelude                        (const, ($), (==), (<<<), bind, (<>), (#))

import App.Routes                     (homeUrl) as R
import App.Types                      (State, Action (..))
import App.CSS                        (darkRed, opacity, logoPath,
                                       headerBitmapPath, headFootHeight)


import Pux.Html                       (Html, div, text, span, button,
                                       input, a, li, ul, nav, img, footer)
import Pux.Router                     (link) as R
import Pux.Html.Attributes            (aria, data_, type_,
                                       placeholder, value, src, alt)
import Pux.Html.Events                (onChange, onClick, onKeyDown)

import Pux.CSS                        (style, backgroundColor, height, px,
                                       color, white, backgroundImage, url)

import Serokell.Pux.Html              (classNames, className)
import Serokell.Pux.Themes.Bootstrap3 as B

view :: State -> Html Action
view state =
    footer
        [ style do
            backgroundImage $ url headerBitmapPath
            headFootHeight
        ]
        [ div
            [ style do
                backgroundColor darkRed
                opacity 0.8
                headFootHeight
            ]
            [
            ]
        ]
