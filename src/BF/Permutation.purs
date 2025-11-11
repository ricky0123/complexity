module BF.Permutation where

import Prelude

import BF.Data.ArrayView
  ( ArrayView
  , SharedArrayBuffer
  , Uint32Array
  , fromSharedBuffer
  , writeAt
  )
import BF.Data.Utils (zerosSharedArrayBuffer)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Utils (rangeLoop)

newPermutationBuffer :: Int -> Effect SharedArrayBuffer
newPermutationBuffer nPrograms = do
  buf <- zerosSharedArrayBuffer (nPrograms * 4)
  let
    arr = fromSharedBuffer buf :: Uint32Array
  _ <- rangeLoop nPrograms \i _ -> do
    writeAt arr i i
  pure buf

getPermutationArray :: SharedArrayBuffer -> Uint32Array
getPermutationArray = fromSharedBuffer

fastShuffle :: forall a. ArrayView a -> Effect Unit
fastShuffle = runEffectFn1 _fastShuffle

foreign import _fastShuffle :: forall a. EffectFn1 (ArrayView a) Unit
