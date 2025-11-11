module BF.Data.ArrayView where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2, EffectFn3, runEffectFn2, runEffectFn3)
import JS.BigInt (BigInt)

foreign import data ArrayBuffer :: Type
foreign import data SharedArrayBuffer :: Type
foreign import data ArrayView :: ArrayViewType -> Type

data ArrayViewType

foreign import data Uint8 :: ArrayViewType
foreign import data Uint32 :: ArrayViewType
foreign import data Int32 :: ArrayViewType
foreign import data BigInt64 :: ArrayViewType
foreign import data Float32 :: ArrayViewType

type Uint8Array = ArrayView Uint8
type Uint32Array = ArrayView Uint32
type Int32Array = ArrayView Int32
type BigInt64Array = ArrayView BigInt64
type Float32Array = ArrayView Float32

-- instances for existing classes
instance sharedArrayBufferShow :: Show SharedArrayBuffer where
  show _ = "SharedArrayBuffer"

instance sharedArrayBufferEq :: Eq SharedArrayBuffer where
  eq _ _ = true

instance arrayViewShow :: Show (ArrayView a) where
  show = _showArray

instance arrayViewEq :: Eq (ArrayView a) where
  eq = _eqArray

-- new classes
class ArrayViewCommon a b | a -> b where
  -- a = specific array type like Uint8Array, b = element type like Int
  index :: a -> Int -> b
  writeAt :: a -> Int -> b -> Effect Unit
  fromArray :: Array b -> a
  toArray :: a -> Array b
  fromSharedBuffer :: SharedArrayBuffer -> a
  fromSharedBufferSubset :: SharedArrayBuffer -> Int -> Int -> a
  slice :: a -> Int -> Int -> a
  length :: a -> Int

infix 6 index as !!

class IntArray a where
  incAtI :: a -> Int -> Effect Unit
  decAtI :: a -> Int -> Effect Unit

class AtomicsCommon a b | a -> b where
  atomicsAdd :: a -> Int -> b -> Effect Unit
  atomicsLoad :: a -> Int -> Effect b
  atomicsExchange :: a -> Int -> b -> Effect b

class AtomicsNotify a b | a -> b where
  atomicsWait :: a -> Int -> b -> Effect String
  atomicsNotify :: a -> Int -> Effect Unit

-- instances for ArrayViewCommon
instance uint8ArrayArrayViewCommon :: ArrayViewCommon (ArrayView Uint8) Int where
  index = _indexUint8Array
  writeAt = runEffectFn3 _writeAtUint8Array
  fromArray = _fromArrayUint8Array
  toArray = _toArrayUint8Array
  fromSharedBuffer = _fromSharedBufferUint8Array
  fromSharedBufferSubset = _fromSharedBufferSubsetUint8Array
  slice = _sliceUint8Array
  length = _lengthUint8Array

instance uint32ArrayArrayViewCommon :: ArrayViewCommon (ArrayView Uint32) Int where
  index = _indexUint32Array
  writeAt = runEffectFn3 _writeAtUint32Array
  fromArray = _fromArrayUint32Array
  toArray = _toArrayUint32Array
  fromSharedBuffer = _fromSharedBufferUint32Array
  fromSharedBufferSubset = _fromSharedBufferSubsetUint32Array
  slice = _sliceUint32Array
  length = _lengthUint32Array

instance int32ArrayArrayViewCommon :: ArrayViewCommon (ArrayView Int32) Int where
  index = _indexInt32Array
  writeAt = runEffectFn3 _writeAtInt32Array
  fromArray = _fromArrayInt32Array
  toArray = _toArrayInt32Array
  fromSharedBuffer = _fromSharedBufferInt32Array
  fromSharedBufferSubset = _fromSharedBufferSubsetInt32Array
  slice = _sliceInt32Array
  length = _lengthInt32Array

instance bigInt64ArrayArrayViewCommon :: ArrayViewCommon (ArrayView BigInt64) BigInt where
  index = _indexBigInt64Array
  writeAt = runEffectFn3 _writeAtBigInt64Array
  fromArray = _fromArrayBigInt64Array
  toArray = _toArrayBigInt64Array
  fromSharedBuffer = _fromSharedBufferBigInt64Array
  fromSharedBufferSubset = _fromSharedBufferSubsetBigInt64Array
  slice = _sliceBigInt64Array
  length = _lengthBigInt64Array

instance float32ArrayArrayViewCommon :: ArrayViewCommon (ArrayView Float32) Number where
  index = _indexFloat32Array
  writeAt = runEffectFn3 _writeAtFloat32Array
  fromArray = _fromArrayFloat32Array
  toArray = _toArrayFloat32Array
  fromSharedBuffer = _fromSharedBufferFloat32Array
  fromSharedBufferSubset = _fromSharedBufferSubsetFloat32Array
  slice = _sliceFloat32Array
  length = _lengthFloat32Array

-- instances for IntArray
instance uint8ArrayIntArray :: IntArray Uint8Array where
  incAtI = runEffectFn2 _incAtIUint8Array
  decAtI = runEffectFn2 _decAtIUint8Array

-- instances for AtomicsCommon
instance uint8Atomics :: AtomicsCommon Uint8Array Int where
  atomicsAdd = runEffectFn3 _uint8AtomicsAdd
  atomicsLoad = runEffectFn2 _uint8AtomicsLoad
  atomicsExchange = runEffectFn3 _uint8AtomicsExchange

instance uint32Atomics :: AtomicsCommon Uint32Array Int where
  atomicsAdd = runEffectFn3 _uint32AtomicsAdd
  atomicsLoad = runEffectFn2 _uint32AtomicsLoad
  atomicsExchange = runEffectFn3 _uint32AtomicsExchange

instance int32Atomics :: AtomicsCommon Int32Array Int where
  atomicsAdd = runEffectFn3 _int32AtomicsAdd
  atomicsLoad = runEffectFn2 _int32AtomicsLoad
  atomicsExchange = runEffectFn3 _int32AtomicsExchange

instance bigIntAtomics :: AtomicsCommon BigInt64Array BigInt where
  atomicsAdd = runEffectFn3 _bigInt64AtomicsAdd
  atomicsLoad = runEffectFn2 _bigInt64AtomicsLoad
  atomicsExchange = runEffectFn3 _bigInt64AtomicsExchange

-- instances for AtomicsNotify
instance int32AtomicsNotify :: AtomicsNotify Int32Array Int where
  atomicsWait = runEffectFn3 _int32ArrayAtomicsWait
  atomicsNotify = runEffectFn2 _int32ArrayAtomicsNotify

instance bigInt64AtomicsNotify :: AtomicsNotify BigInt64Array BigInt where
  atomicsWait = runEffectFn3 _bigInt64ArrayAtomicsWait
  atomicsNotify = runEffectFn2 _bigInt64ArrayAtomicsNotify

-- foreign imports for existing classes
foreign import _showArray :: forall a. ArrayView a -> String
foreign import _eqArray :: forall a. ArrayView a -> ArrayView a -> Boolean

-- foreign imports for ArrayViewCommon -> index
foreign import _indexUint8Array :: Uint8Array -> Int -> Int
foreign import _indexUint32Array :: Uint32Array -> Int -> Int
foreign import _indexInt32Array :: Int32Array -> Int -> Int
foreign import _indexBigInt64Array :: BigInt64Array -> Int -> BigInt
foreign import _indexFloat32Array :: Float32Array -> Int -> Number

-- foreign imports for ArrayViewCommon -> writeAt
foreign import _writeAtUint8Array :: EffectFn3 Uint8Array Int Int Unit
foreign import _writeAtUint32Array :: EffectFn3 Uint32Array Int Int Unit
foreign import _writeAtInt32Array :: EffectFn3 Int32Array Int Int Unit
foreign import _writeAtBigInt64Array :: EffectFn3 BigInt64Array Int BigInt Unit
foreign import _writeAtFloat32Array :: EffectFn3 Float32Array Int Number Unit

-- foreign imports for ArrayViewCommon -> fromArray
foreign import _fromArrayUint8Array :: Array Int -> Uint8Array
foreign import _fromArrayUint32Array :: Array Int -> Uint32Array
foreign import _fromArrayInt32Array :: Array Int -> Int32Array
foreign import _fromArrayBigInt64Array :: Array BigInt -> BigInt64Array
foreign import _fromArrayFloat32Array :: Array Number -> Float32Array

-- foreign imports for ArrayViewCommon -> toArray
foreign import _toArrayUint8Array :: Uint8Array -> Array Int
foreign import _toArrayUint32Array :: Uint32Array -> Array Int
foreign import _toArrayInt32Array :: Int32Array -> Array Int
foreign import _toArrayBigInt64Array :: BigInt64Array -> Array BigInt
foreign import _toArrayFloat32Array :: Float32Array -> Array Number

-- foreign imports for ArrayViewCommon -> fromSharedBuffer
foreign import _fromSharedBufferUint8Array :: SharedArrayBuffer -> Uint8Array
foreign import _fromSharedBufferUint32Array :: SharedArrayBuffer -> Uint32Array
foreign import _fromSharedBufferInt32Array :: SharedArrayBuffer -> Int32Array
foreign import _fromSharedBufferBigInt64Array :: SharedArrayBuffer -> BigInt64Array
foreign import _fromSharedBufferFloat32Array :: SharedArrayBuffer -> Float32Array

foreign import _fromSharedBufferSubsetUint8Array
  :: SharedArrayBuffer -> Int -> Int -> Uint8Array

foreign import _fromSharedBufferSubsetUint32Array
  :: SharedArrayBuffer -> Int -> Int -> Uint32Array

foreign import _fromSharedBufferSubsetInt32Array
  :: SharedArrayBuffer -> Int -> Int -> Int32Array

foreign import _fromSharedBufferSubsetBigInt64Array
  :: SharedArrayBuffer -> Int -> Int -> BigInt64Array

foreign import _fromSharedBufferSubsetFloat32Array
  :: SharedArrayBuffer -> Int -> Int -> Float32Array

-- imports for ArrayViewCommon -> slice
foreign import _sliceUint8Array :: Uint8Array -> Int -> Int -> Uint8Array
foreign import _sliceUint32Array :: Uint32Array -> Int -> Int -> Uint32Array
foreign import _sliceInt32Array :: Int32Array -> Int -> Int -> Int32Array
foreign import _sliceBigInt64Array :: BigInt64Array -> Int -> Int -> BigInt64Array
foreign import _sliceFloat32Array :: Float32Array -> Int -> Int -> Float32Array

-- foreign imports for ArrayViewCommon -> length
foreign import _lengthUint8Array :: Uint8Array -> Int
foreign import _lengthUint32Array :: Uint32Array -> Int
foreign import _lengthInt32Array :: Int32Array -> Int
foreign import _lengthBigInt64Array :: BigInt64Array -> Int
foreign import _lengthFloat32Array :: Float32Array -> Int

-- foreign imports for IntArray methods
foreign import _incAtIUint8Array :: EffectFn2 Uint8Array Int Unit
foreign import _decAtIUint8Array :: EffectFn2 Uint8Array Int Unit

-- foreign imports for AtomicsCommon methods
foreign import _uint8AtomicsAdd :: EffectFn3 Uint8Array Int Int Unit
foreign import _uint8AtomicsLoad :: EffectFn2 Uint8Array Int Int
foreign import _uint8AtomicsExchange :: EffectFn3 Uint8Array Int Int Int

foreign import _uint32AtomicsAdd :: EffectFn3 Uint32Array Int Int Unit
foreign import _uint32AtomicsLoad :: EffectFn2 Uint32Array Int Int
foreign import _uint32AtomicsExchange :: EffectFn3 Uint32Array Int Int Int

foreign import _int32AtomicsAdd :: EffectFn3 Int32Array Int Int Unit
foreign import _int32AtomicsLoad :: EffectFn2 Int32Array Int Int
foreign import _int32AtomicsExchange :: EffectFn3 Int32Array Int Int Int

foreign import _bigInt64AtomicsAdd :: EffectFn3 BigInt64Array Int BigInt Unit
foreign import _bigInt64AtomicsLoad :: EffectFn2 BigInt64Array Int BigInt
foreign import _bigInt64AtomicsExchange :: EffectFn3 BigInt64Array Int BigInt BigInt

-- foreign import for AtomicsNotify methods
foreign import _int32ArrayAtomicsWait :: EffectFn3 Int32Array Int Int String
foreign import _int32ArrayAtomicsNotify :: EffectFn2 Int32Array Int Unit

foreign import _bigInt64ArrayAtomicsWait :: EffectFn3 BigInt64Array Int BigInt String
foreign import _bigInt64ArrayAtomicsNotify :: EffectFn2 BigInt64Array Int Unit
