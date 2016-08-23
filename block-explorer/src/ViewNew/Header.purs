module App.ViewNew.Header where

import Prelude                        (const, ($), (==), (<<<), bind, (<>))

import App.Routes                     (homeUrl) as R
import App.Types                      (State, Action (..))
import App.CSS                        (darkRed, opacity, logoPath,
                                       headerBitmapPath, noBorder)


import Pux.Html                       (Html, div, text, span, button,
                                       input, a, li, ul, nav, img)
import Pux.Router                     (link) as R
import Pux.Html.Attributes            (aria, data_, type_,
                                       placeholder, value, src, alt)

import Pux.CSS                        (style, backgroundColor, padding, px,
                                       color, white, backgroundImage, url)

import Serokell.Pux.Html              (classNames, className)
import Serokell.Pux.Themes.Bootstrap3 as B

view :: State -> Html Action
view state =
    nav
        [ classNames [B.navbar, B.navbarDefault]
        , style do
            backgroundImage $ url headerBitmapPath
            noBorder
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
                        let p = px 10.0
                        padding p p p p
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
                        , className B.formControl
                        , placeholder "Address / Transaction"
                        ] []
                    , span
                        [ className B.inputGroupBtn ]
                        [ button
                            [ classNames [B.btn, B.btnDefault]
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
