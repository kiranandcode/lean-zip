import ZipTest.Helpers
import Zip.Native.Inflate
import Zip.Native.Deflate
import Zip.Native.DeflateDynamic

/-! Tests for native inflate (raw DEFLATE decompression) against native-compressed data
    across compression levels and block types. -/

def ZipTest.NativeInflate.tests : IO Unit := do
  IO.println "  NativeInflate tests..."
  let big ← mkTestData
  let helloBytes := "Hello, world!".toUTF8

  -- Native compress (raw deflate) → native inflate: small data
  let compressed := Zip.Native.Deflate.deflateRaw helloBytes
  match Zip.Native.Inflate.inflate compressed with
  | .ok result => assert! result == helloBytes
  | .error e => throw (IO.userError s!"native inflate failed on hello: {e}")

  -- Native compress → native inflate: larger repetitive data
  let compressedBig := Zip.Native.Deflate.deflateRaw big
  match Zip.Native.Inflate.inflate compressedBig with
  | .ok result => assert! result == big
  | .error e => throw (IO.userError s!"native inflate failed on big data: {e}")

  -- Native compress level 1 → native inflate
  let compressed1 := Zip.Native.Deflate.deflateRaw helloBytes 1
  match Zip.Native.Inflate.inflate compressed1 with
  | .ok result => assert! result == helloBytes
  | .error e => throw (IO.userError s!"native inflate (level 1) failed: {e}")

  -- Native compress level 9 → native inflate
  let compressed9 := Zip.Native.Deflate.deflateRaw big 9
  match Zip.Native.Inflate.inflate compressed9 with
  | .ok result => assert! result == big
  | .error e => throw (IO.userError s!"native inflate (level 9) failed: {e}")

  -- Empty data
  let compressedEmpty := Zip.Native.Deflate.deflateRaw ByteArray.empty
  match Zip.Native.Inflate.inflate compressedEmpty with
  | .ok result => assert! result == ByteArray.empty
  | .error e => throw (IO.userError s!"native inflate (empty) failed: {e}")

  -- Single byte
  let singleByte := ByteArray.mk #[42]
  let compressedSingle := Zip.Native.Deflate.deflateRaw singleByte
  match Zip.Native.Inflate.inflate compressedSingle with
  | .ok result => assert! result == singleByte
  | .error e => throw (IO.userError s!"native inflate (single byte) failed: {e}")

  -- Stored block (level 0 = no compression)
  let stored := Zip.Native.Deflate.deflateRaw helloBytes 0
  match Zip.Native.Inflate.inflate stored with
  | .ok result => assert! result == helloBytes
  | .error e => throw (IO.userError s!"native inflate (stored) failed: {e}")

  -- Large data with stored blocks (level 0)
  let storedBig := Zip.Native.Deflate.deflateRaw big 0
  match Zip.Native.Inflate.inflate storedBig with
  | .ok result => assert! result == big
  | .error e => throw (IO.userError s!"native inflate (stored big) failed: {e}")

  -- Large data to exercise dynamic Huffman and long back-references
  let large ← mkLargeData
  let compressedLarge := Zip.Native.Deflate.deflateRaw large
  match Zip.Native.Inflate.inflate compressedLarge with
  | .ok result => assert! result == large
  | .error e => throw (IO.userError s!"native inflate (large) failed: {e}")

  -- Incompressible data (random-ish bytes)
  let random := mkRandomData
  let compressedRandom := Zip.Native.Deflate.deflateRaw random
  match Zip.Native.Inflate.inflate compressedRandom with
  | .ok result => assert! result == random
  | .error e => throw (IO.userError s!"native inflate (random) failed: {e}")

  IO.println "  NativeInflate tests passed."
