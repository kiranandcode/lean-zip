import ZipTest.Helpers
import Zip.Native.Deflate
import Zip.Native.DeflateDynamic
import Zip.Native.Inflate

/-! Tests for native raw DEFLATE compression/decompression with roundtrip verification. -/

def ZipTest.RawDeflate.tests : IO Unit := do
  let big ← mkTestData

  -- Whole-buffer roundtrip
  let rawCompressed := Zip.Native.Deflate.deflateRaw big
  let rawDecompressed ← match Zip.Native.Inflate.inflate rawCompressed with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! rawDecompressed.beq big

  -- Empty raw deflate
  let rawCE := Zip.Native.Deflate.deflateRaw ByteArray.empty
  let rawDE ← match Zip.Native.Inflate.inflate rawCE with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! rawDE.beq ByteArray.empty
  IO.println "RawDeflate tests: OK"
