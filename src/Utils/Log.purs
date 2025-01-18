module Utils.Log where

import Prelude
import Effect

foreign import debug :: forall a. a -> Effect Unit
