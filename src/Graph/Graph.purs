module Graph.Graph where

import Effect
import Prelude

import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Aff
import Web.DOM.Element (Element)
import Promise.Aff

foreign import data VegaView :: Type

foreign import _embedChart :: Element -> Effect (Promise VegaView)

embedChart :: Element -> Aff VegaView
embedChart = _embedChart >>> toAffE

foreign import addDataPoint :: VegaView -> Number -> Number -> Effect Unit

foreign import addNewRecord :: VegaView -> Number -> Effect Unit
