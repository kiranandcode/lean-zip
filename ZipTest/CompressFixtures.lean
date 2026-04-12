import ZipTest.Helpers
import Zip.Native.Gzip

/-! Tests for gzip decompression with real-world interop fixtures and malformed data. -/

def ZipTest.CompressFixtures.tests : IO Unit := do
  -- system-hello.gz: decompress gzip from system gzip tool
  let sysGzData ← readFixture "gzip/interop/system-hello.gz"
  let sysGzDecomp ← match Zip.Native.GzipDecode.decompress sysGzData with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  unless String.fromUTF8! sysGzDecomp == "Hello from system gzip\n" do
    throw (IO.userError s!"system-hello.gz: content mismatch: {repr (String.fromUTF8! sysGzDecomp)}")

  -- two-members.gz: concatenated gzip from Haskell zlib
  let twoMembersData ← readFixture "gzip/interop/two-members.gz"
  let twoMembersDecomp ← match Zip.Native.GzipDecode.decompress twoMembersData with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  unless String.fromUTF8! twoMembersDecomp == "Test 1Test 2" do
    throw (IO.userError s!"two-members.gz: content mismatch: {repr (String.fromUTF8! twoMembersDecomp)}")

  -- bad-crc.gz: gzip with wrong CRC
  let badCrcGzData ← readFixture "gzip/malformed/bad-crc.gz"
  match Zip.Native.GzipDecode.decompress badCrcGzData with
  | .error e =>
    unless e.contains "CRC" do
      throw (IO.userError s!"Gzip malformed (bad-crc.gz): wrong error: {e}")
  | .ok _ => throw (IO.userError "Gzip malformed (bad-crc.gz): expected error")

  -- truncated.gz: truncated deflate stream
  let truncGzData ← readFixture "gzip/malformed/truncated.gz"
  match Zip.Native.GzipDecode.decompress truncGzData with
  | .error _ => pure ()
  | .ok _ => throw (IO.userError "Gzip malformed (truncated.gz): expected error")

  IO.println "Gzip fixture tests: OK"
