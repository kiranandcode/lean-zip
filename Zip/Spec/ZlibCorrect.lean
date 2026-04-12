import Zip.Native.Gzip
import Zip.Spec.DeflateRoundtrip
import ZipCommon.Spec.BinaryCorrect
import Zip.Spec.DeflateSuffix
import Zip.Spec.InflateComplete
import Zip.Spec.GzipCorrect

namespace Zip.Native

namespace ZlibDecode

/-- Pure zlib decompressor for single-stream data (no preset dictionary).
    Proof-friendly: no for/while/mut. -/
def decompressSingle (data : ByteArray)
    (maxOutputSize : Nat := 1024 * 1024 * 1024) :
    Except String ByteArray := do
  if data.size < 6 then throw "Zlib: input too short"
  let cmf := data[0]!
  let flg := data[1]!
  let check := cmf.toUInt16 * 256 + flg.toUInt16
  unless check % 31 == 0 do throw "Zlib: header check failed"
  unless cmf &&& 0x0F == 8 do throw "Zlib: unsupported compression method"
  unless cmf >>> 4 ≤ 7 do throw "Zlib: invalid window size"
  unless flg &&& 0x20 == 0 do throw "Zlib: preset dictionaries not supported"
  let (decompressed, endPos) ← Inflate.inflateRaw data 2 maxOutputSize
  if endPos + 4 > data.size then throw "Zlib: truncated trailer"
  let b0 := data[endPos]!.toUInt32
  let b1 := data[endPos + 1]!.toUInt32
  let b2 := data[endPos + 2]!.toUInt32
  let b3 := data[endPos + 3]!.toUInt32
  let expectedAdler := (b0 <<< 24) ||| (b1 <<< 16) ||| (b2 <<< 8) ||| b3
  let actualAdler := Adler32.Native.adler32 1 decompressed
  unless actualAdler == expectedAdler do throw "Zlib: Adler32 mismatch"
  return decompressed

end ZlibDecode

namespace ZlibEncode


end ZlibEncode


end Zip.Native
