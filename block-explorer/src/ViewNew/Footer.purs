module App.ViewNew.Footer where

import Prelude                        (const, ($), (==), (<<<), bind, (<>), (#),
                                       show, map)

import App.Routes                     (homeUrl) as R
import App.Types                      (State, Action (..))
import App.CSS                        (darkRed, opacity, logoPath,
                                       headerBitmapPath, headFootHeight)
import App.Common.Html                (ttext)

import Data.I18N                      (allLanguages)

import Pux.Html                       (Html, div, text, span, button,
                                       input, a, li, ul, nav, img, footer)
import Pux.Router                     (link) as R
import Pux.Html.Attributes            (aria, data_, type_, className, id_,
                                       placeholder, value, src, alt)
import Pux.Html.Events                (onClick)

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
                    , span
                        [ id_ "upper-version" ]
                        [ text "VO.123" ]
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
                        [ ttext state.language _._nativeName
                        , span
                            [ className "caret"
                            , id_ "caret"]
                            []
                        ]
                    , ul
                        [ className "dropdown-menu"
                        , id_ "dropdown"
                        ]
                        $ map languageItem allLanguages
                    ]
                ]
            , div
                [ id_ "footer-links" ]
                [ a
                    [ data_ "target" "#" ]
                    [ ttext state.language _.aboutUs ]
                , text "|"
                , a
                    [ data_ "target" "#" ]
                    [ ttext state.language _.contacts ]
                , text "|"
                , a
                    [ data_ "target" "#" ]
                    [ ttext state.language _.privacyPolicy ]
                , text "|"
                , a
                    [ data_ "target" "#" ]
                    [ ttext state.language _.termsOfService ]
                ]
            ]
        ]
  where
    languageItem lang =
        li
            []
            [ a
                [ data_ "target" "#"
                , onClick $ const $ LanguageSet lang
                ]
                [ ttext lang _._nativeName ]
            ]
