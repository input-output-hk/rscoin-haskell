module Serokell.Data.Maybe where

import Prelude (($))

import Data.Maybe (fromJust, Maybe)
import Partial.Unsafe (unsafePartial)

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = unsafePartial $ fromJust
