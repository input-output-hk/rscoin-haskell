module Serokell.Pux.Html
       ( className
       , classNames
       , ClassName (..)
       ) where

import Prelude             ((<<<), map)

import Data.String         (joinWith)
import Data.Array          (singleton)

import Pux.Html            (Attribute)
import Pux.Html.Attributes (className) as A

-- | A wrapper for strings which are used as CSS classes.
newtype ClassName = ClassName String

runClassName :: forall a. ClassName -> String
runClassName (ClassName s) = s

className :: forall a. ClassName -> Attribute a
className = classNames <<< singleton

classNames :: forall a. Array ClassName -> Attribute a
classNames = A.className <<< joinWith " " <<< map runClassName
