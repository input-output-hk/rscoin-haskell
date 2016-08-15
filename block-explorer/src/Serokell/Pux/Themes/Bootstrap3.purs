module Serokell.Pux.Themes.Bootstrap3
       ( bootstrap
       , bootstrapCss
--       , container_fluid
       ) where

import Prelude             (($), (<>))

import Pux.Html            (Html, Attribute, div, link, script)
import Pux.Html.Attributes (src, rel , type_, href)
import Pux.Html.Attributes (className) as A

bootstrap :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
bootstrap attr html = bootstrapCss attr $ bootstrapScripts <> html
  where
    bootstrapScripts =
        [ script
            [ src "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js" ]
            []
        , script
            [ src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" ]
            []
        ]

bootstrapCss :: forall a. Array (Attribute a) -> Array (Html a) -> Html a
bootstrapCss attr html = div attr $ [ bootstrapStyle ] <> html
  where
    bootstrapStyle =
        link
            [ rel "stylesheet"
            , type_ "text/css"
            , href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
            ]
            []

-- | A wrapper for strings which are used as CSS classes.
-- newtype ClassName = ClassName String
--
-- className :: forall a. ClassName -> Attribute a
-- className (ClassName s) = A.className $ runClassName s
--
-- container_fluid :: ClassName
-- container_fluid = className "container-fluid"
