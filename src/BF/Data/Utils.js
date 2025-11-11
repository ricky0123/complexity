
let getRandomValues
try {
  if (window !== undefined) {
    console.log("Browser environment detected")
    getRandomValues = arr => window.crypto.getRandomValues(arr)
  }
} catch (e) {
  console.log("Test environment detected")
  getRandomValues = (arr) => {
    for (let i = 0; i < arr.length; i++) {
      arr[i] = Math.floor(Math.random() * 255)
    }
  }
}

export const _waitForDifferentValue = (arr, i, val) => {
  while (Atomics.load(arr, i) == val) {
    Atomics.wait(arr, i, val)
  }
}

export const _reduceUint8Array = i => f => arr => {
  return arr.reduce(f, i)
}

export const randomSharedArrayBuffer = size => () => {
  let soup = new SharedArrayBuffer(size)
  let soupView = new Uint8Array(soup)

  let offset = 0;
  while (offset < soupView.byteLength) {
    const len = Math.min(65536, soupView.byteLength - offset);
    const tmp = new Uint8Array(len);
    getRandomValues(tmp);
    new Uint8Array(soupView.buffer, soupView.byteOffset + offset, len).set(tmp);
    offset += len;
  }

  return soup
}

export const zerosSharedArrayBuffer = size => () => {
  return new SharedArrayBuffer(size)
}

export const bufferOf = arr => {
  return arr.buffer
}
