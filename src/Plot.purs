module Plot where

import Prelude

import BF.Data.ArrayView (ArrayView, Float32Array, Uint32Array)
import Effect (Effect)

foreign import createLineChart
  :: { title :: String, elemId :: String }
  -> Effect
       ( { noOps :: { interactions :: Uint32Array, noOps :: Uint32Array }
         , compression :: { interactions :: Uint32Array, compression :: Uint32Array }
         }
         -> Effect Unit
       )
