module App.Common.Html where

import Prelude (($), (<>))

import Pux.Html (Html, text, span)
import Pux.Html.Attributes (className)

import Data.I18N (languageCode, Language, getTranslation, Translation)

ttext :: forall a. Language -> (Translation -> String) -> Html a
ttext = ttextClass ""

ttextUpper :: forall a. Language -> (Translation -> String) -> Html a
ttextUpper = ttextClass "text-uppercase"

ttextClass :: forall a. String -> Language -> (Translation -> String) -> Html a
ttextClass _class lang label =
    span
        [ className $ _class <> " language-" <> languageCode lang ]
        [ text $ label $ getTranslation lang ]
