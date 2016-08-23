module App.ViewNew.Footer where

import Prelude                        (const, ($), (==), (<<<), bind, (<>), (#))

import App.Routes                     (homeUrl) as R
import App.Types                      (State, Action (..))
import App.CSS                        (darkRed, opacity, logoSmallPath,
                                       headerBitmapPath, headFootHeight)


import Pux.Html                       (Html, div, text, span, button,
                                       input, a, li, ul, nav, img, footer)
import Pux.Router                     (link) as R
import Pux.Html.Attributes            (aria, data_, type_,
                                       placeholder, value, src, alt)
import Pux.Html.Events                (onChange, onClick, onKeyDown)

import Pux.CSS                        (style, backgroundColor, height, px,
                                       color, white, backgroundImage, url,
                                       padding, width, pct, textAlign, center,
                                       nil)

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
            [ div
                [ className B.containerFluid ]
                [ div
                    [ className B.navbarHeader ]
                    [ R.link R.homeUrl
                        [ className B.navbarBrand
                        , style do
                            color white
                            let p = px 18.0
                            padding p p p p
                        ]
                        [ img
                            [ alt "Brand"
                            , src logoSmallPath
                            ]
                            []
                        ]
                    ]
                , div
                    [ style do
                        width $ pct 100.0
                        textAlign center
                        color white
                        let p = px 15.0
                        padding p nil p nil
                    ]
                    [ text "About us | Contacts | Privacy Policy | Terms of Service" ]
--                , ul
--                    [ classNames [B.nav, B.navbarNav, B.navbarRight] ]
--                    [ li
--                        [ className B.dropdown ]
--                        [ a
--                            [ className B.dropdownToggle
--                            , data_ "target" "#"
--                            , data_ "toggle" "dropdown"
--                            , aria "haspopup" "true"
--                            , aria "expanded" "false"
--                            , style $ color white
--                            ]
--                            [ text "English"
--                            , span
--                                [ className B.caret ]
--                                []
--                            ]
--                        , ul
--                            [ className B.dropdownMenu ]
--                            [ li
--                                []
--                                [ a
--                                    [ data_ "target" "#" ]
--                                    [ text "English" ]
--                                ]
--                            ]
--                        ]
--                    ]
                ]
            ]
        ]
