import ZipTest.Helpers
import ZipTest.BenchHelpers
import Zip.Native.Inflate
import Zip.Native.Gzip
import Zip.Native.DeflateDynamic
import Zip.Native.Crc32
import Zip.Native.Adler32

/-! Performance and conformance tests for native inflate/deflate, gzip, zlib, and
    checksums across varying data sizes and compression patterns. All tests use
    pure native Lean implementations (no FFI). -/

namespace ZipTest.NativeScale

inductive Pattern where | constant | cyclic | prng | text

def Pattern.name : Pattern → String
  | .constant => "constant"
  | .cyclic   => "cyclic"
  | .prng     => "prng"
  | .text     => "text"

def Pattern.generate : Pattern → Nat → ByteArray
  | .constant => mkConstantData
  | .cyclic   => mkCyclicData
  | .prng     => mkPrngData
  | .text     => mkTextData

def patterns : Array Pattern := #[.constant, .cyclic, .prng, .text]
def levels : Array UInt8 := #[0, 1, 6]
def matrixSizes : Array Nat := #[1024, 16384, 131072, 1048576]
def sweepSizes : Array Nat := #[1024, 2048, 4096, 8192, 16384, 32768,
                                  65536, 131072, 262144, 524288, 1048576]

structure TimingEntry where
  op : String
  size : Nat
  pat : String
  ns : Nat

-- Inflate (raw deflate) conformance
private def testInflateMatrix (timings : IO.Ref (Array TimingEntry)) : IO Unit := do
  IO.println "    --- inflate (raw deflate) ---"
  for size in matrixSizes do
    for pat in patterns do
      let data := pat.generate size
      for level in levels do
        let compressed := Zip.Native.Deflate.deflateRaw data level
        let start ← IO.monoNanosNow
        let result ← match Zip.Native.Inflate.inflate compressed with
          | .ok r => pure r
          | .error e => throw (IO.userError e)
        let stop ← IO.monoNanosNow
        let ns := stop - start
        unless result == data do
          throw (IO.userError s!"inflate mismatch: {sizeName size} {pat.name} lvl={level}")
        IO.println s!"      {pad (sizeName size) 6} {pad pat.name 9} lvl={level}  OK  {fmtMs ns}ms"
        if size == 1048576 then
          timings.modify (·.push { op := "inflate", size, pat := pat.name, ns })

-- Gzip conformance
private def testGzipMatrix (timings : IO.Ref (Array TimingEntry)) : IO Unit := do
  IO.println "    --- gzip ---"
  for size in matrixSizes do
    for pat in patterns do
      let data := pat.generate size
      for level in levels do
        let compressed := Zip.Native.GzipEncode.compress data level
        let start ← IO.monoNanosNow
        let result ← match Zip.Native.GzipDecode.decompress compressed with
          | .ok r => pure r
          | .error e => throw (IO.userError e)
        let stop ← IO.monoNanosNow
        let ns := stop - start
        unless result == data do
          throw (IO.userError s!"gzip mismatch: {sizeName size} {pat.name} lvl={level}")
        IO.println s!"      {pad (sizeName size) 6} {pad pat.name 9} lvl={level}  OK  {fmtMs ns}ms"
        if size == 1048576 then
          timings.modify (·.push { op := "gzip", size, pat := pat.name, ns })

-- Zlib conformance
private def testZlibMatrix (timings : IO.Ref (Array TimingEntry)) : IO Unit := do
  IO.println "    --- zlib ---"
  for size in matrixSizes do
    for pat in patterns do
      let data := pat.generate size
      for level in levels do
        let compressed := Zip.Native.ZlibEncode.compress data level
        let start ← IO.monoNanosNow
        let result ← match Zip.Native.ZlibDecode.decompress compressed with
          | .ok r => pure r
          | .error e => throw (IO.userError e)
        let stop ← IO.monoNanosNow
        let ns := stop - start
        unless result == data do
          throw (IO.userError s!"zlib mismatch: {sizeName size} {pat.name} lvl={level}")
        IO.println s!"      {pad (sizeName size) 6} {pad pat.name 9} lvl={level}  OK  {fmtMs ns}ms"
        if size == 1048576 then
          timings.modify (·.push { op := "zlib", size, pat := pat.name, ns })

-- Size sweep: all 11 powers of two, prng data, level 6, all three formats
private def testSizeSweep (timings : IO.Ref (Array TimingEntry)) : IO Unit := do
  IO.println "    --- size sweep (prng, lvl=6) ---"
  for size in sweepSizes do
    let data := Pattern.prng.generate size
    let rawComp := Zip.Native.Deflate.deflateRaw data 6
    let s1 ← IO.monoNanosNow
    let r1 ← match Zip.Native.Inflate.inflate rawComp with
      | .ok r => pure r
      | .error e => throw (IO.userError e)
    let e1 ← IO.monoNanosNow
    let ns1 := e1 - s1
    unless r1 == data do
      throw (IO.userError s!"sweep inflate mismatch: {sizeName size}")
    let gzComp := Zip.Native.GzipEncode.compress data 6
    let s2 ← IO.monoNanosNow
    let r2 ← match Zip.Native.GzipDecode.decompress gzComp with
      | .ok r => pure r
      | .error e => throw (IO.userError e)
    let e2 ← IO.monoNanosNow
    let ns2 := e2 - s2
    unless r2 == data do
      throw (IO.userError s!"sweep gzip mismatch: {sizeName size}")
    let zlComp := Zip.Native.ZlibEncode.compress data 6
    let s3 ← IO.monoNanosNow
    let r3 ← match Zip.Native.ZlibDecode.decompress zlComp with
      | .ok r => pure r
      | .error e => throw (IO.userError e)
    let e3 ← IO.monoNanosNow
    let ns3 := e3 - s3
    unless r3 == data do
      throw (IO.userError s!"sweep zlib mismatch: {sizeName size}")
    IO.println s!"      {pad (sizeName size) 6} inflate={pad (fmtMs ns1 ++ "ms") 10} gzip={pad (fmtMs ns2 ++ "ms") 10} zlib={fmtMs ns3}ms"
    if size == 1048576 then
      timings.modify (·.push { op := "sweep-inflate", size, pat := "prng", ns := ns1 })
      timings.modify (·.push { op := "sweep-gzip", size, pat := "prng", ns := ns2 })
      timings.modify (·.push { op := "sweep-zlib", size, pat := "prng", ns := ns3 })

-- Checksum conformance: CRC-32 and Adler-32
private def testChecksums (timings : IO.Ref (Array TimingEntry)) : IO Unit := do
  IO.println "    --- crc32 ---"
  for size in sweepSizes do
    for pat in patterns do
      let data := pat.generate size
      let start ← IO.monoNanosNow
      let nativeCrc := Crc32.Native.crc32 0 data
      let stop ← IO.monoNanosNow
      let ns := stop - start
      let _ := nativeCrc
      IO.println s!"      {pad (sizeName size) 6} {pad pat.name 9} OK  {fmtMs ns}ms"
      if size == 1048576 then
        timings.modify (·.push { op := "crc32", size, pat := pat.name, ns })
  IO.println "    --- adler32 ---"
  for size in sweepSizes do
    for pat in patterns do
      let data := pat.generate size
      let start ← IO.monoNanosNow
      let nativeAdler := Adler32.Native.adler32 1 data
      let stop ← IO.monoNanosNow
      let ns := stop - start
      let _ := nativeAdler
      IO.println s!"      {pad (sizeName size) 6} {pad pat.name 9} OK  {fmtMs ns}ms"
      if size == 1048576 then
        timings.modify (·.push { op := "adler32", size, pat := pat.name, ns })

-- Incremental checksum conformance
private def testIncrementalChecksums : IO Unit := do
  IO.println "    --- incremental checksums ---"
  for size in matrixSizes do
    for pat in patterns do
      let data := pat.generate size
      let half := size / 2
      let firstHalf := data.extract 0 half
      let secondHalf := data.extract half data.size
      -- CRC-32 incremental
      let nativeWhole := Crc32.Native.crc32 0 data
      let nativeInc1 := Crc32.Native.crc32 0 firstHalf
      let nativeInc2 := Crc32.Native.crc32 nativeInc1 secondHalf
      unless nativeInc2 == nativeWhole do
        throw (IO.userError s!"crc32 incremental != whole: {sizeName size} {pat.name}")
      -- Adler-32 incremental
      let nativeAdlerWhole := Adler32.Native.adler32 1 data
      let nativeAdlerInc1 := Adler32.Native.adler32 1 firstHalf
      let nativeAdlerInc2 := Adler32.Native.adler32 nativeAdlerInc1 secondHalf
      unless nativeAdlerInc2 == nativeAdlerWhole do
        throw (IO.userError s!"adler32 incremental != whole: {sizeName size} {pat.name}")
      IO.println s!"      {pad (sizeName size) 6} {pad pat.name 9} crc32+adler32 OK"

-- Compression throughput: native across patterns, sizes, and levels
private def testCompressMatrix (timings : IO.Ref (Array TimingEntry)) : IO Unit := do
  IO.println "    --- raw deflate compression (native) ---"
  for size in matrixSizes do
    for pat in patterns do
      let data := pat.generate size
      for level in levels do
        let s1 ← IO.monoNanosNow
        let nc ← forceEval (Zip.Native.Deflate.deflateRaw data level)
        let e1 ← IO.monoNanosNow
        -- Verify roundtrip: decompress native output with native inflate
        let rd ← match Zip.Native.Inflate.inflate nc with
          | .ok r => pure r
          | .error e => throw (IO.userError e)
        unless rd == data do
          throw (IO.userError s!"compress roundtrip: {sizeName size} {pat.name} lvl={level}")
        let nNs := e1 - s1
        IO.println s!"      {pad (sizeName size) 6} {pad pat.name 9} lvl={level}  native={pad (fmtMs nNs ++ "ms") 10} ({fmtMBps size nNs} MB/s)"
        if size == 1048576 then
          timings.modify (·.push { op := s!"compress-l{level}", size, pat := pat.name, ns := nNs })

-- Compression quality: native output sizes at 1MB across all 10 levels
private def testCompressQuality : IO Unit := do
  IO.println "    --- compression quality at 1MB (native, levels 0-9) ---"
  IO.println s!"      {pad "Pattern" 9} {pad "Level" 6} {pad "Native" 10} {"Ratio%"}"
  for pat in #[Pattern.prng, Pattern.text] do
    let data := pat.generate 1048576
    for level in #[(0 : UInt8), 1, 2, 3, 4, 5, 6, 7, 8, 9] do
      let nc ← forceEval (Zip.Native.Deflate.deflateRaw data level)
      -- Verify native output decompresses correctly
      let rd ← match Zip.Native.Inflate.inflate nc with
        | .ok r => pure r
        | .error e => throw (IO.userError e)
      unless rd == data do
        throw (IO.userError s!"quality roundtrip: {pat.name} lvl={level}")
      let ratio := if data.size == 0 then 0.0 else nc.size.toFloat / data.size.toFloat
      let sr := let s := s!"{ratio}"; if s.length > 6 then s.take 6 else s
      IO.println s!"      {pad pat.name 9} {pad s!"lvl={level}" 6} {pad (toString nc.size) 10} {sr}"

def tests : IO Unit := do
  IO.println "  NativeScale tests..."
  let timings ← IO.mkRef #[]
  testInflateMatrix timings
  testGzipMatrix timings
  testZlibMatrix timings
  testCompressMatrix timings
  testSizeSweep timings
  testChecksums timings
  testIncrementalChecksums
  testCompressQuality
  -- Timing summary (1MB results)
  let entries ← timings.get
  if entries.size > 0 then
    IO.println "    --- 1MB timing summary ---"
    for e in entries do
      IO.println s!"      {pad e.op 16} {pad e.pat 9} {fmtMs e.ns}ms"
  IO.println "  NativeScale tests passed."

end ZipTest.NativeScale
