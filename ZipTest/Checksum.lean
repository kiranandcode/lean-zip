import ZipTest.Helpers
import Zip.Native.Crc32
import Zip.Native.Adler32

/-! Tests for native CRC32 and Adler32 checksums, including incremental updates and edge cases. -/

def ZipTest.Checksum.tests : IO Unit := do
  let big ← mkTestData
  let helloBytes := "Hello, world!".toUTF8

  -- CRC32 of known data (precomputed: CRC32 of "Hello, world!" = 0xebe6c6e6)
  let crc := Crc32.Native.crc32 0 helloBytes
  assert! crc == 3957769958

  -- Incremental CRC32 matches whole-buffer
  let crc1 := Crc32.Native.crc32 0 (big.extract 0 3000)
  let crc2 := Crc32.Native.crc32 crc1 (big.extract 3000 big.size)
  let crcWhole := Crc32.Native.crc32 0 big
  assert! crc2 == crcWhole

  -- Adler32 of known data
  let adler := Adler32.Native.adler32 1 helloBytes
  assert! adler == 543032458

  -- Incremental Adler32 matches whole-buffer
  let adler1 := Adler32.Native.adler32 1 (big.extract 0 3000)
  let adler2 := Adler32.Native.adler32 adler1 (big.extract 3000 big.size)
  let adlerWhole := Adler32.Native.adler32 1 big
  assert! adler2 == adlerWhole

  -- Empty input checksums
  let crcEmpty := Crc32.Native.crc32 0 ByteArray.empty
  assert! crcEmpty == 0
  let adlerEmpty := Adler32.Native.adler32 1 ByteArray.empty
  assert! adlerEmpty == 1
  IO.println "Checksum tests: OK"
