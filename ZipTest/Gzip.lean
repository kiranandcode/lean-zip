import ZipTest.Helpers
import Zip.Native.Gzip

/-! Tests for native gzip compression/decompression: roundtrip, compression levels,
    empty input, and concatenated streams. -/

def ZipTest.Gzip.tests : IO Unit := do
  let big ← mkTestData

  -- Gzip roundtrip
  let gzipped := Zip.Native.GzipEncode.compress big
  let gunzipped ← match Zip.Native.GzipDecode.decompress gzipped with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! gunzipped.beq big

  -- Compression levels
  let fast := Zip.Native.GzipEncode.compress big (level := 1)
  let best := Zip.Native.GzipEncode.compress big (level := 9)
  assert! best.size ≤ fast.size

  -- Empty input
  let ge := Zip.Native.GzipEncode.compress ByteArray.empty
  let gde ← match Zip.Native.GzipDecode.decompress ge with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! gde.beq ByteArray.empty

  -- Concatenated gzip streams
  let part1 := "First gzip member. ".toUTF8
  let part2 := "Second gzip member. ".toUTF8
  let gz1 := Zip.Native.GzipEncode.compress part1
  let gz2 := Zip.Native.GzipEncode.compress part2
  let concatenated := gz1 ++ gz2
  let decoded ← match Zip.Native.GzipDecode.decompress concatenated with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! decoded.beq (part1 ++ part2)

  IO.println "Gzip tests: OK"
