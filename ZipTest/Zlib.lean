import ZipTest.Helpers
import Zip.Native.Gzip

/-! Tests for native zlib compression/decompression with roundtrip verification. -/

def ZipTest.Zlib.tests : IO Unit := do
  let big ← mkTestData

  -- Zlib compress/decompress roundtrip
  let compressed := Zip.Native.ZlibEncode.compress big
  let decompressed ← match Zip.Native.ZlibDecode.decompress compressed with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! decompressed.beq big

  -- Decompression limit
  match Zip.Native.ZlibDecode.decompress compressed (maxOutputSize := 10) with
  | .ok _ => throw (IO.userError "decompress limit should have been rejected")
  | .error _ => pure ()

  -- Empty input roundtrip
  let empty := ByteArray.empty
  let ce := Zip.Native.ZlibEncode.compress empty
  let de ← match Zip.Native.ZlibDecode.decompress ce with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! de.beq empty
  IO.println "Zlib tests: OK"
