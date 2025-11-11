module BF.Data.Utils where

import Prelude

import BF.Data.ArrayView
  ( ArrayView
  , Int32Array
  , SharedArrayBuffer
  , Uint32Array
  , Uint8Array
  )
import Data.Foldable (class Foldable)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Monoid (mempty, (<>))
import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import JS.BigInt (BigInt)

-- would have to change types to make a real instance of Foldable
foreign import _reduceUint8Array :: forall a. a -> (Fn2 a Int a) -> Uint8Array -> a

foldMapUint8Array :: forall m. Monoid m => (Int -> m) -> Uint8Array -> m
foldMapUint8Array f = _reduceUint8Array mempty $ mkFn2 \acc i -> acc <> (f i)

-- shared array buffer
foreign import randomSharedArrayBuffer :: Int -> Effect SharedArrayBuffer
foreign import zerosSharedArrayBuffer :: Int -> Effect SharedArrayBuffer

foreign import bufferOf :: forall a. ArrayView a -> SharedArrayBuffer

-- atomics-related
foreign import _waitForDifferentValue :: EffectFn3 Int32Array Int Int Unit

waitForDifferentValue :: Int32Array -> Int -> Int -> Effect Unit
waitForDifferentValue = runEffectFn3 _waitForDifferentValue
