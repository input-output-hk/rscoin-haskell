module App.ViewNew.Footer where

import Prelude                        (const, ($), (==), (<<<), bind, (<>), (#))

import App.Routes                     (homeUrl) as R
import App.Types                      (State, Action (..))
import App.CSS                        (darkRed, opacity, logoPath,
                                       headerBitmapPath, headFootHeight)


import Pux.Html                       (Html, div, text, span, button,
                                       input, a, li, ul, nav, img, footer)
import Pux.Router                     (link) as R
import Pux.Html.Attributes            (aria, data_, type_, className, id_,
                                       placeholder, value, src, alt)
import Pux.Html.Events                (onChange, onClick, onKeyDown)

import Pux.CSS                        (style, backgroundColor, height, px,
                                       color, white, backgroundImage, url,
                                       padding, width, pct, textAlign, center,
                                       nil)

view :: State -> Html Action
view state =
    footer
        [ id_ "footer-image" ]
        [ div
            [ className "container-fluid"
            , id_ "header-container" ]
            [ div
                [ className "navbar-header"
                , id_ "logo-link"]
                [ R.link R.homeUrl
                    []
                    [ img
                        [ id_ "logo"
                        , src logoPath
                        ]
                        []
                    ]
                ]
            , ul
                [ className "nav navbar-nav navbar-right"
                , id_ "currency"]
                [ li
                    [ className "dropup" ]
                    [ a
                        [ className "dropdown-toggle"
                        , data_ "target" "#"
                        , data_ "toggle" "dropdown"
                        , aria "haspopup" "true"
                        , aria "expanded" "false"
                        , id_ "toggle"
                        ]
                        [ text "English"
                        , span
                            [ className "caret"
                            , id_ "caret"]
                            []
                        ]
                    , ul
                        [ className "dropdown-menu" ]
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
                [ id_ "footer-links" ]
                [ a
                    [ data_ "target" "#" ]
                    [ text "About us" ]
                , text "|"
                , a
                    [ data_ "target" "#" ]
                    [ text "Contacts" ]
                , text "|"
                , a
                    [ data_ "target" "#" ]
                    [ text "Privacy Policy" ]
                , text "|"
                , a
                    [ data_ "target" "#" ]
                    [ text "Terms of Service" ]
                ]
            ]
        ]
