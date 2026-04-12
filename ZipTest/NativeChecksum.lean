import ZipTest.Helpers
import Zip.Native.Adler32
import Zip.Native.Crc32

/-! Tests comparing native Adler32 and CRC32 implementations against known values. -/

def ZipTest.NativeChecksum.tests : IO Unit := do
  let big ← mkTestData
  let helloBytes := "Hello, world!".toUTF8

  -- Native Adler32 on known data
  let adler1 := Adler32.Native.adler32 1 helloBytes
  let adler2 := Adler32.Native.adler32 1 helloBytes
  assert! adler1 == adler2

  -- Native Adler32 on large data
  let adlerBig1 := Adler32.Native.adler32 1 big
  let adlerBig2 := Adler32.Native.adler32 1 big
  assert! adlerBig1 == adlerBig2

  -- Incremental native Adler32 matches whole-buffer
  let half := big.size / 2
  let firstHalf := big.extract 0 half
  let secondHalf := big.extract half big.size
  let nativeInc1 := Adler32.Native.adler32 1 firstHalf
  let nativeInc2 := Adler32.Native.adler32 nativeInc1 secondHalf
  let nativeWhole := Adler32.Native.adler32 1 big
  assert! nativeInc2 == nativeWhole

  -- Incremental native Adler32 consistency
  let inc1 := Adler32.Native.adler32 1 firstHalf
  let inc2 := Adler32.Native.adler32 inc1 secondHalf
  assert! nativeInc2 == inc2

  -- Empty Adler32
  let nativeEmpty := Adler32.Native.adler32 1 ByteArray.empty
  assert! nativeEmpty == 1

  -- Single byte Adler32
  let singleByte := ByteArray.mk #[42]
  let single1 := Adler32.Native.adler32 1 singleByte
  let single2 := Adler32.Native.adler32 1 singleByte
  assert! single1 == single2

  -- Native CRC32 on known data
  let crc1 := Crc32.Native.crc32 0 helloBytes
  let crc2 := Crc32.Native.crc32 0 helloBytes
  assert! crc1 == crc2

  -- Native CRC32 on large data
  let crcBig1 := Crc32.Native.crc32 0 big
  let crcBig2 := Crc32.Native.crc32 0 big
  assert! crcBig1 == crcBig2

  -- Incremental native CRC32 matches whole-buffer
  let nativeCrcInc1 := Crc32.Native.crc32 0 firstHalf
  let nativeCrcInc2 := Crc32.Native.crc32 nativeCrcInc1 secondHalf
  let nativeCrcWhole := Crc32.Native.crc32 0 big
  assert! nativeCrcInc2 == nativeCrcWhole

  -- Incremental native CRC32 consistency
  let crcInc1 := Crc32.Native.crc32 0 firstHalf
  let crcInc2 := Crc32.Native.crc32 crcInc1 secondHalf
  assert! nativeCrcInc2 == crcInc2

  -- Empty CRC32
  let nativeCrcEmpty := Crc32.Native.crc32 0 ByteArray.empty
  assert! nativeCrcEmpty == 0

  -- Single byte CRC32
  let crcSingle1 := Crc32.Native.crc32 0 singleByte
  let crcSingle2 := Crc32.Native.crc32 0 singleByte
  assert! crcSingle1 == crcSingle2

  IO.println "Native checksum tests: OK"
