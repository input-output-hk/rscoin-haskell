module App.View.Header where

import Prelude                        (const, ($), (==), (<<<), bind, (<>))

import App.Routes                     (homeUrl) as R
import App.Types                      (State, Action (..))


import Pux.Html                       (Html, div, text, span, button,
                                       input, a, li, ul, nav, img)
import Pux.Router                     (link) as R
import Pux.Html.Attributes            (aria, data_, type_,
                                       placeholder, value, src, alt)
import Pux.Html.Events                (onChange, onClick, onKeyDown)

import Pux.CSS                        (style, backgroundColor, rgb, Color,
                                       color, white, backgroundImage, url)
import CSS.String                     (fromString)
import CSS.Stylesheet                 (CSS, key)

import Serokell.Pux.Html              (classNames, className)
import Serokell.Pux.Themes.Bootstrap3 as B

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

view :: State -> Html Action
view state =
    nav
        [ classNames [B.navbar, B.navbarDefault]
        , style do
            backgroundImage $ url headerBitmapPath
        ]
        [ div
            [ className B.containerFluid
            , style do
                backgroundColor darkRed
                opacity 0.8
            ]
            [ div
                [ className B.navbarHeader ]
                [ R.link R.homeUrl
                    [ className B.navbarBrand
                    , style do
                        color white
                    ]
                    [ img
                        [ alt "Brand"
                        , src logoPath
                        ]
                        []
                    ]
                ]
            , ul
                [ classNames [B.nav, B.navbarNav, B.navbarRight] ]
                [ li
                    [ className B.dropdown ]
                    [ a
                        [ className B.dropdownToggle
                        , data_ "target" "#"
                        , data_ "toggle" "dropdown"
                        , aria "haspopup" "true"
                        , aria "expanded" "false"
                        , style $ color white
                        ]
                        [ text "ADA"
                        , span
                            [ className B.caret ]
                            []
                        ]
                    , ul
                        [ className B.dropdownMenu ]
                        [ li
                            []
                            [ a
                                [ data_ "target" "#" ]
                                [ text "English" ]
                            ]
                        ]
                    ]
                ]
            , div
                [ classNames [B.colXs6, B.navbarForm, B.navbarRight] ]
                [ div
                    [ className B.inputGroup ]
                    [ input
                        [ type_ "text"
                        , value state.searchQuery
                        , onChange $ SearchQueryChange <<< _.value <<< _.target
                        , onKeyDown $ \e -> if e.keyCode == 13 then SearchButton else Nop
                        , className B.formControl
                        , placeholder "Address / Transaction"
                        ] []
                    , span
                        [ className B.inputGroupBtn ]
                        [ button
                            [ onClick $ const SearchButton
                            , classNames [B.btn, B.btnDefault]
                            ]
                            [ span
                                [ classNames [B.glyphicon, B.glyphiconSearch]
                                , aria "hidden" "true"
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            ]
        ]
