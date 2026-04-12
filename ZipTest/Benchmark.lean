import ZipTest.Helpers
import ZipTest.BenchHelpers
import Zip.Native.Inflate
import Zip.Native.DeflateDynamic

/-! Decompression throughput benchmark: native Lean DEFLATE.

    This is the Track D1 baseline benchmark. It compresses representative
    inputs using the native Lean compressor, then measures wall-clock
    decompression time for native Lean inflate over multiple iterations.
    Reports throughput in MB/s. -/

namespace ZipTest.Benchmark

/-- Run decompression `iters` times, return total elapsed nanoseconds. -/
private def benchNative (compressed : ByteArray) (iters : Nat) : IO Nat := do
  let start ← IO.monoNanosNow
  for _ in [:iters] do
    let _ ← forceEval (match Zip.Native.Inflate.inflate compressed with
      | .ok r => r | .error _ => ByteArray.empty)
  let stop ← IO.monoNanosNow
  return stop - start

def tests : IO Unit := do
  IO.println "  Benchmark: DEFLATE decompression throughput (Track D1)..."

  -- Data patterns: text (compressible) and prng (less compressible)
  let pats := #[("text", mkTextData), ("prng", mkPrngData)]
  -- Sizes: 16KB, 64KB, 256KB
  let sizes := #[16384, 65536, 262144]
  -- Compression levels
  let levels : Array UInt8 := #[1, 6]
  -- Iterations for stable timing
  let iters := 5

  IO.println s!"    Iterations per measurement: {iters}"
  IO.println s!"    {pad "Size" 6} {pad "Pattern" 9} {pad "Level" 6} {pad "Ratio" 8} {pad "Native" 20}"

  for size in sizes do
    for (pname, pgen) in pats do
      let data := pgen size
      for level in levels do
        -- Compress with native
        let compressed := Zip.Native.Deflate.deflateRaw data level
        let ratio := if data.size == 0 then "N/A"
          else
            let r10 := compressed.size * 1000 / data.size
            let whole := r10 / 10
            let frac := r10 % 10
            s!"{whole}.{frac}%"

        -- Verify correctness before benchmarking
        match Zip.Native.Inflate.inflate compressed with
        | .ok r => unless r == data do
            throw (IO.userError s!"native inflate mismatch: {sizeName size} {pname} lvl={level}")
        | .error e => throw (IO.userError s!"native inflate error: {e}")

        -- Benchmark: multiple iterations
        let nElapsed ← benchNative compressed iters

        -- Per-iteration averages
        let nAvg := nElapsed / iters

        IO.println s!"    {pad (sizeName size) 6} {pad pname 9} {pad s!"lvl={level}" 6} {pad ratio 8} native={pad (fmtMs nAvg ++ "ms") 10} ({fmtMBps size nAvg} MB/s)"

  IO.println "  Benchmark tests passed."

end ZipTest.Benchmark
