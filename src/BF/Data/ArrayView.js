export const _showArray = x => x.toString()

export const _eqArray = a => b => {
  if (a.length != b.length) {
    return false
  }
  for (let i = 0; i < a.length; i++) {
    if (a[i] != b[i]) {
      return false
    }
  }
  return true
}

const _indexArray = arr => i => arr[i]

export const _indexUint8Array = _indexArray
export const _indexUint32Array = _indexArray
export const _indexInt32Array = _indexArray
export const _indexBigInt64Array = _indexArray
export const _indexFloat32Array = _indexArray

export const _incAtIUint8Array = (arr, i) => {
  arr[i] += 1
}

export const _decAtIUint8Array = (arr, i) => {
  arr[i] -= 1
}

const _writeAt = (arr, i, val) => {
  arr[i] = val
}

export const _writeAtUint8Array = _writeAt
export const _writeAtUint32Array = _writeAt
export const _writeAtInt32Array = _writeAt
export const _writeAtBigInt64Array = _writeAt
export const _writeAtFloat32Array = _writeAt

export const _fromArrayUint8Array = arr => {
  return new Uint8Array(arr)
}

export const _fromArrayUint32Array = arr => {
  return new Uint32Array(arr)
}

export const _fromArrayInt32Array = arr => {
  return new Int32Array(arr)
}

export const _fromArrayBigInt64Array = arr => {
  return new BigInt64Array(arr)
}

export const _fromArrayFloat32Array = arr => {
  return new Float32Array(arr)
}

const _toArray = arr => {
  return Array.from(arr)
}

export const _toArrayUint8Array = _toArray
export const _toArrayUint32Array = _toArray
export const _toArrayInt32Array = _toArray
export const _toArrayBigInt64Array = _toArray
export const _toArrayFloat32Array = _toArray

const _atomicsAdd = (arr, i, val) => {
  Atomics.add(arr, i, val)
}

const _atomicsLoad = (arr, i) => {
  return Atomics.load(arr, i)
}

const _atomicsExchange = (arr, i, val) => {
  return Atomics.exchange(arr, i, val)
}

export const _uint8AtomicsAdd = _atomicsAdd
export const _uint8AtomicsLoad = _atomicsLoad
export const _uint8AtomicsExchange = _atomicsExchange

export const _uint32AtomicsAdd = _atomicsAdd
export const _uint32AtomicsLoad = _atomicsLoad
export const _uint32AtomicsExchange = _atomicsExchange

export const _int32AtomicsAdd = _atomicsAdd
export const _int32AtomicsLoad = _atomicsLoad
export const _int32AtomicsExchange = _atomicsExchange

export const _bigInt64AtomicsAdd = _atomicsAdd
export const _bigInt64AtomicsLoad = _atomicsLoad
export const _bigInt64AtomicsExchange = _atomicsExchange


export const _fromSharedBufferUint8Array = buf => {
  return new Uint8Array(buf)
}

export const _fromSharedBufferUint32Array = buf => {
  return new Uint32Array(buf)
}

export const _fromSharedBufferInt32Array = buf => {
  return new Int32Array(buf)
}

export const _fromSharedBufferBigInt64Array = buf => {
  return new BigInt64Array(buf)
}

export const _fromSharedBufferFloat32Array = buf => {
  return new Float32Array(buf)
}

export const _fromSharedBufferSubsetUint8Array = buf => start => len => {
  return new Uint8Array(buf, start, len)
}

export const _fromSharedBufferSubsetUint32Array = buf => start => len => {
  return new Uint32Array(buf, start, len)
}

export const _fromSharedBufferSubsetInt32Array = buf => start => len => {
  return new Int32Array(buf, start, len)
}

export const _fromSharedBufferSubsetBigInt64Array = buf => start => len => {
  return new BigInt64Array(buf, start, len)
}

export const _fromSharedBufferSubsetFloat32Array = buf => start => len => {
  return new Float32Array(buf, start, len)
}

export const _int32ArrayAtomicsWait = Atomics.wait
export const _bigInt64ArrayAtomicsWait = Atomics.wait

export const _int32ArrayAtomicsNotify = Atomics.notify
export const _bigInt64ArrayAtomicsNotify = Atomics.notify

const _slice = x => start => end => x.slice(start, end)

export const _sliceUint8Array = _slice
export const _sliceUint32Array = _slice
export const _sliceInt32Array = _slice
export const _sliceBigInt64Array = _slice
export const _sliceFloat32Array = _slice

const _lengthArray = arr => arr.length

export const _lengthUint8Array = _lengthArray
export const _lengthUint32Array = _lengthArray
export const _lengthInt32Array = _lengthArray
export const _lengthBigInt64Array = _lengthArray
export const _lengthFloat32Array = _lengthArray
