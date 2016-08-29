module App.Common.Html where

import Prelude (($), (<>))

import Pux.Html (Html, text, span)
import Pux.Html.Attributes (className)

import Data.I18N (languageCode, Language, getTranslation, Translation)

-- TODO: we should move this function to helpers
ttext :: forall a. Language -> (Translation -> String) -> Html a
ttext lang label =
    span
        [ className $ "language-" <> languageCode lang ]
        [ text $ label $ getTranslation lang ]
