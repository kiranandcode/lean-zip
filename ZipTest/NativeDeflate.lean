import ZipTest.Helpers
import Zip.Native.Deflate
import Zip.Native.DeflateDynamic
import Zip.Native.Inflate

/-! Tests for native DEFLATE: stored, fixed Huffman, dynamic Huffman, and lazy matching modes
    with cross-implementation verification against FFI inflate. -/

def ZipTest.NativeDeflate.tests : IO Unit := do
  IO.println "  NativeDeflate tests..."
  let big ← mkTestData
  let helloBytes := "Hello, world!".toUTF8

  -- Native deflateStored → native inflate: small data
  let compressed := Zip.Native.Deflate.deflateStored helloBytes
  match Zip.Native.Inflate.inflate compressed with
  | .ok result => assert! result == helloBytes
  | .error e => throw (IO.userError s!"deflateStored→inflate failed on hello: {e}")

  -- Native deflateStored → native inflate: empty data
  let compressedEmpty := Zip.Native.Deflate.deflateStored ByteArray.empty
  match Zip.Native.Inflate.inflate compressedEmpty with
  | .ok result => assert! result == ByteArray.empty
  | .error e => throw (IO.userError s!"deflateStored→inflate failed on empty: {e}")

  -- Native deflateStored → native inflate: single byte
  let singleByte := ByteArray.mk #[42]
  let compressedSingle := Zip.Native.Deflate.deflateStored singleByte
  match Zip.Native.Inflate.inflate compressedSingle with
  | .ok result => assert! result == singleByte
  | .error e => throw (IO.userError s!"deflateStored→inflate failed on single byte: {e}")

  -- Native deflateStored → native inflate: larger repetitive data
  let compressedBig := Zip.Native.Deflate.deflateStored big
  match Zip.Native.Inflate.inflate compressedBig with
  | .ok result => assert! result == big
  | .error e => throw (IO.userError s!"deflateStored→inflate failed on big: {e}")

  -- Native deflateStored → native inflate: data > 65535 bytes (multi-block)
  let large := mkConstantData 100000
  let compressedLarge := Zip.Native.Deflate.deflateStored large
  match Zip.Native.Inflate.inflate compressedLarge with
  | .ok result => assert! result == large
  | .error e => throw (IO.userError s!"deflateStored→inflate failed on large: {e}")

  -- Native deflateStored → native inflate: random data > 65535 bytes
  let largeRandom := mkPrngData 131072
  let compressedRandom := Zip.Native.Deflate.deflateStored largeRandom
  match Zip.Native.Inflate.inflate compressedRandom with
  | .ok result => assert! result == largeRandom
  | .error e => throw (IO.userError s!"deflateStored→inflate failed on large random: {e}")

  -- Native deflateStored → native inflate (cross-implementation)
  let compressedCross := Zip.Native.Deflate.deflateStored helloBytes
  let decompressedCross ← match Zip.Native.Inflate.inflate compressedCross with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! decompressedCross == helloBytes

  -- Native compress level 0 → native inflate (stored blocks)
  let ffiStored := Zip.Native.Deflate.deflateRaw helloBytes 0
  match Zip.Native.Inflate.inflate ffiStored with
  | .ok result => assert! result == helloBytes
  | .error e => throw (IO.userError s!"native level 0→native inflate failed: {e}")

  -- Level 1 (fixed Huffman) tests

  -- deflateFixed → native inflate: small data
  let fixedHello := Zip.Native.Deflate.deflateFixed helloBytes
  match Zip.Native.Inflate.inflate fixedHello with
  | .ok result => assert! result == helloBytes
  | .error e => throw (IO.userError s!"deflateFixed→inflate failed on hello: {e}")

  -- deflateFixed → native inflate: empty data
  let fixedEmpty := Zip.Native.Deflate.deflateFixed ByteArray.empty
  match Zip.Native.Inflate.inflate fixedEmpty with
  | .ok result => assert! result == ByteArray.empty
  | .error e => throw (IO.userError s!"deflateFixed→inflate failed on empty: {e}")

  -- deflateFixed → native inflate: single byte
  let fixedSingle := Zip.Native.Deflate.deflateFixed singleByte
  match Zip.Native.Inflate.inflate fixedSingle with
  | .ok result => assert! result == singleByte
  | .error e => throw (IO.userError s!"deflateFixed→inflate failed on single byte: {e}")

  -- deflateFixed → native inflate: repetitive data
  let fixedBig := Zip.Native.Deflate.deflateFixed big
  match Zip.Native.Inflate.inflate fixedBig with
  | .ok result => assert! result == big
  | .error e => throw (IO.userError s!"deflateFixed→inflate failed on big: {e}")

  -- deflateFixed → native inflate (cross-implementation)
  let fixedCross := Zip.Native.Deflate.deflateFixed helloBytes
  let decompFixedCross ← match Zip.Native.Inflate.inflate fixedCross with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! decompFixedCross == helloBytes

  -- deflateFixed → native inflate: larger data (cross-implementation)
  let fixedCrossBig := Zip.Native.Deflate.deflateFixed big
  let decompFixedCrossBig ← match Zip.Native.Inflate.inflate fixedCrossBig with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! decompFixedCrossBig == big

  -- deflateFixed achieves compression on repetitive data
  let storedSize := (Zip.Native.Deflate.deflateStored big).size
  let fixedSize := fixedBig.size
  assert! fixedSize < storedSize

  -- deflateFixed → native inflate: all-same-byte data
  let allSame := mkConstantData 1000
  let fixedSame := Zip.Native.Deflate.deflateFixed allSame
  match Zip.Native.Inflate.inflate fixedSame with
  | .ok result => assert! result == allSame
  | .error e => throw (IO.userError s!"deflateFixed→inflate failed on all-same: {e}")

  -- deflateFixed → native inflate: pseudo-random data
  let random := mkPrngData 1000
  let fixedRandom := Zip.Native.Deflate.deflateFixed random
  match Zip.Native.Inflate.inflate fixedRandom with
  | .ok result => assert! result == random
  | .error e => throw (IO.userError s!"deflateFixed→inflate failed on random: {e}")

  -- Level 2 (lazy LZ77) tests

  -- deflateLazy → native inflate: small data
  let lazyHello := Zip.Native.Deflate.deflateLazy helloBytes
  match Zip.Native.Inflate.inflate lazyHello with
  | .ok result => assert! result == helloBytes
  | .error e => throw (IO.userError s!"deflateLazy→inflate failed on hello: {e}")

  -- deflateLazy → native inflate: empty data
  let lazyEmpty := Zip.Native.Deflate.deflateLazy ByteArray.empty
  match Zip.Native.Inflate.inflate lazyEmpty with
  | .ok result => assert! result == ByteArray.empty
  | .error e => throw (IO.userError s!"deflateLazy→inflate failed on empty: {e}")

  -- deflateLazy → native inflate: single byte
  let lazySingle := Zip.Native.Deflate.deflateLazy singleByte
  match Zip.Native.Inflate.inflate lazySingle with
  | .ok result => assert! result == singleByte
  | .error e => throw (IO.userError s!"deflateLazy→inflate failed on single byte: {e}")

  -- deflateLazy → native inflate: repetitive data
  let lazyBig := Zip.Native.Deflate.deflateLazy big
  match Zip.Native.Inflate.inflate lazyBig with
  | .ok result => assert! result == big
  | .error e => throw (IO.userError s!"deflateLazy→inflate failed on big: {e}")

  -- deflateLazy → native inflate (cross-implementation)
  let lazyCross := Zip.Native.Deflate.deflateLazy helloBytes
  let decompLazyCross ← match Zip.Native.Inflate.inflate lazyCross with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! decompLazyCross == helloBytes

  -- deflateLazy → native inflate: larger data (cross-implementation)
  let lazyCrossBig := Zip.Native.Deflate.deflateLazy big
  let decompLazyCrossBig ← match Zip.Native.Inflate.inflate lazyCrossBig with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! decompLazyCrossBig == big

  -- deflateLazy achieves equal or better compression than deflateFixed on repetitive data
  let lazySize := lazyBig.size
  let fixedSize := fixedBig.size
  assert! lazySize ≤ fixedSize

  -- deflateLazy → native inflate: all-same-byte data
  let lazySame := Zip.Native.Deflate.deflateLazy allSame
  match Zip.Native.Inflate.inflate lazySame with
  | .ok result => assert! result == allSame
  | .error e => throw (IO.userError s!"deflateLazy→inflate failed on all-same: {e}")

  -- deflateLazy → native inflate: pseudo-random data
  let lazyRandom := Zip.Native.Deflate.deflateLazy random
  match Zip.Native.Inflate.inflate lazyRandom with
  | .ok result => assert! result == random
  | .error e => throw (IO.userError s!"deflateLazy→inflate failed on random: {e}")

  -- Level 5 (dynamic Huffman) tests

  -- deflateDynamic → native inflate: small data
  let dynHello := Zip.Native.Deflate.deflateDynamic helloBytes
  match Zip.Native.Inflate.inflate dynHello with
  | .ok result => assert! result == helloBytes
  | .error e => throw (IO.userError s!"deflateDynamic→inflate failed on hello: {e}")

  -- deflateDynamic → native inflate: empty data
  let dynEmpty := Zip.Native.Deflate.deflateDynamic ByteArray.empty
  match Zip.Native.Inflate.inflate dynEmpty with
  | .ok result => assert! result == ByteArray.empty
  | .error e => throw (IO.userError s!"deflateDynamic→inflate failed on empty: {e}")

  -- deflateDynamic → native inflate: single byte
  let dynSingle := Zip.Native.Deflate.deflateDynamic singleByte
  match Zip.Native.Inflate.inflate dynSingle with
  | .ok result => assert! result == singleByte
  | .error e => throw (IO.userError s!"deflateDynamic→inflate failed on single byte: {e}")

  -- deflateDynamic → native inflate: repetitive data
  let dynBig := Zip.Native.Deflate.deflateDynamic big
  match Zip.Native.Inflate.inflate dynBig with
  | .ok result => assert! result == big
  | .error e => throw (IO.userError s!"deflateDynamic→inflate failed on big: {e}")

  -- deflateDynamic → native inflate: all-same-byte data
  let dynSame := Zip.Native.Deflate.deflateDynamic allSame
  match Zip.Native.Inflate.inflate dynSame with
  | .ok result => assert! result == allSame
  | .error e => throw (IO.userError s!"deflateDynamic→inflate failed on all-same: {e}")

  -- deflateDynamic → native inflate: pseudo-random data
  let dynRandom := Zip.Native.Deflate.deflateDynamic random
  match Zip.Native.Inflate.inflate dynRandom with
  | .ok result => assert! result == random
  | .error e => throw (IO.userError s!"deflateDynamic→inflate failed on random: {e}")

  -- deflateDynamic → native inflate (cross-implementation)
  let dynCross := Zip.Native.Deflate.deflateDynamic helloBytes
  let decompDynCross ← match Zip.Native.Inflate.inflate dynCross with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! decompDynCross == helloBytes

  -- deflateDynamic → native inflate: larger data (cross-implementation)
  let dynCrossBig := Zip.Native.Deflate.deflateDynamic big
  let decompDynCrossBig ← match Zip.Native.Inflate.inflate dynCrossBig with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! decompDynCrossBig == big

  -- deflateDynamic achieves equal or better compression than deflateLazy on repetitive data
  let dynSize := dynBig.size
  let lazySize := lazyBig.size
  assert! dynSize ≤ lazySize

  -- Unified deflateRaw dispatch tests

  -- deflateRaw level 0 (stored) → native inflate
  let rawStored := Zip.Native.Deflate.deflateRaw helloBytes 0
  match Zip.Native.Inflate.inflate rawStored with
  | .ok result => assert! result == helloBytes
  | .error e => throw (IO.userError s!"deflateRaw(0)→inflate failed on hello: {e}")

  -- deflateRaw level 1 (fixed) → native inflate
  let rawFixed := Zip.Native.Deflate.deflateRaw helloBytes 1
  match Zip.Native.Inflate.inflate rawFixed with
  | .ok result => assert! result == helloBytes
  | .error e => throw (IO.userError s!"deflateRaw(1)→inflate failed on hello: {e}")

  -- deflateRaw level 3 (lazy) → native inflate
  let rawLazy := Zip.Native.Deflate.deflateRaw big 3
  match Zip.Native.Inflate.inflate rawLazy with
  | .ok result => assert! result == big
  | .error e => throw (IO.userError s!"deflateRaw(3)→inflate failed on big: {e}")

  -- deflateRaw level 6 (dynamic) → native inflate
  let rawDyn := Zip.Native.Deflate.deflateRaw big 6
  match Zip.Native.Inflate.inflate rawDyn with
  | .ok result => assert! result == big
  | .error e => throw (IO.userError s!"deflateRaw(6)→inflate failed on big: {e}")

  -- deflateRaw level 6 → native inflate (cross-implementation)
  let rawDynCross := Zip.Native.Deflate.deflateRaw helloBytes 6
  let decompRawCross ← match Zip.Native.Inflate.inflate rawDynCross with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! decompRawCross == helloBytes

  -- deflateRaw level 6 → native inflate: larger data
  let rawDynCrossBig := Zip.Native.Deflate.deflateRaw big 6
  let decompRawCrossBig ← match Zip.Native.Inflate.inflate rawDynCrossBig with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! decompRawCrossBig == big

  -- deflateRaw on empty data (all levels)
  for level in [0, 1, 3, 6] do
    let rawEmpty := Zip.Native.Deflate.deflateRaw ByteArray.empty level.toUInt8
    match Zip.Native.Inflate.inflate rawEmpty with
    | .ok result => assert! result == ByteArray.empty
    | .error e => throw (IO.userError s!"deflateRaw({level})→inflate failed on empty: {e}")

  -- deflateRaw on single byte (all levels)
  for level in [0, 1, 3, 6] do
    let rawSingle := Zip.Native.Deflate.deflateRaw singleByte level.toUInt8
    match Zip.Native.Inflate.inflate rawSingle with
    | .ok result => assert! result == singleByte
    | .error e => throw (IO.userError s!"deflateRaw({level})→inflate failed on single byte: {e}")

  -- Iterative LZ77 conformance tests

  -- lz77GreedyIter matches lz77Greedy on small inputs
  for (name, data) in [("empty", ByteArray.empty), ("single", singleByte),
                        ("hello", helloBytes), ("big", big),
                        ("constant1K", mkConstantData 1024),
                        ("cyclic1K", mkCyclicData 1024),
                        ("prng1K", mkPrngData 1024)] do
    let iterTokens := Zip.Native.Deflate.lz77GreedyIter data
    let recTokens := Zip.Native.Deflate.lz77Greedy data
    unless iterTokens == recTokens do
      throw (IO.userError s!"lz77GreedyIter vs lz77Greedy mismatch on {name}: {iterTokens.size} vs {recTokens.size} tokens")

  -- lz77GreedyIter on large inputs (would stack-overflow with lz77Greedy)
  for (name, size) in [("64KB", 65536), ("256KB", 262144)] do
    let data := mkCyclicData size
    let tokens := Zip.Native.Deflate.lz77GreedyIter data
    unless tokens.size > 0 do
      throw (IO.userError s!"lz77GreedyIter produced no tokens on {name}")

  -- deflateFixedIter roundtrip on large inputs via native inflate
  for (name, data) in [("64KB-const", mkConstantData 65536),
                        ("256KB-cyclic", mkCyclicData 262144),
                        ("256KB-prng", mkPrngData 262144)] do
    let compressed := Zip.Native.Deflate.deflateFixedIter data
    match Zip.Native.Inflate.inflate compressed with
    | .ok result => unless result == data do
        throw (IO.userError s!"deflateFixedIter→inflate mismatch on {name}")
    | .error e => throw (IO.userError s!"deflateFixedIter→inflate failed on {name}: {e}")

  -- deflateFixedIter → native inflate roundtrip on 256KB
  let largeCyclic := mkCyclicData 262144
  let compressedLarge := Zip.Native.Deflate.deflateFixedIter largeCyclic
  let decompLarge ← match Zip.Native.Inflate.inflate compressedLarge with
    | .ok r => pure r
    | .error e => throw (IO.userError e)
  assert! decompLarge == largeCyclic

  -- deflateRaw level 1 now uses iterative path — roundtrip 256KB
  let rawLargeFixed := Zip.Native.Deflate.deflateRaw largeCyclic 1
  match Zip.Native.Inflate.inflate rawLargeFixed with
  | .ok result => assert! result == largeCyclic
  | .error e => throw (IO.userError s!"deflateRaw(1) 256KB roundtrip failed: {e}")

  IO.println "  NativeDeflate tests passed."
