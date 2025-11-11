module BF.Compression where

import Prelude

import BF.Data.ArrayView (Uint8Array)
import Effect (Effect)

foreign import fflateGZipSync :: Uint8Array -> Effect Int
