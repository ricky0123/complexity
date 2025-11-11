import { gzipSync } from 'fflate'

const overhead = 20 // gzipSync(new Uint8Array(0)).byteLength;

export const fflateGZipSync = (data) => () => {
  return gzipSync(data).byteLength - overhead
}
