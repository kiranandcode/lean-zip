import Zip.Spec.DeflateEncode
import Zip.Spec.BitstreamWriteCorrect

namespace Deflate.Spec

/-- A code-length entry for DEFLATE dynamic blocks (RFC 1951 §3.2.7).
    Each entry is `(clCode, extraValue)` where:
    - clCode 0–15: literal code length, extraValue = 0
    - clCode 16: repeat previous, extraValue = count − 3 (0–3, 2 bits)
    - clCode 17: repeat zero, extraValue = count − 3 (0–7, 3 bits)
    - clCode 18: repeat zero long, extraValue = count − 11 (0–127, 7 bits) -/
abbrev CLEntry := Nat × Nat

/-- Count consecutive occurrences of `val` at the front of `xs`. -/
def countRun (val : Nat) : List Nat → Nat
  | x :: xs => if x == val then 1 + countRun val xs else 0
  | [] => 0

/-- RLE-encode a list of code lengths into CL entries.
    Greedy strategy: use the longest possible run at each position.
    For runs of zeros: use codes 17 or 18.
    For runs of the same non-zero value: emit literal then code 16. -/
def rlEncodeLengths (lengths : List Nat) : List CLEntry :=
  go lengths
where
  go : List Nat → List CLEntry
    | [] => []
    | x :: xs =>
      if x == 0 then
        let runLen := 1 + countRun 0 xs
        if runLen >= 11 then
          let take := min runLen 138
          (18, take - 11) :: go (xs.drop (take - 1))
        else if runLen >= 3 then
          (17, runLen - 3) :: go (xs.drop (runLen - 1))
        else
          -- 1 or 2 zeros: emit as literals
          (0, 0) :: go xs
      else
        let runLen := countRun x xs
        if runLen >= 3 then
          let take := min runLen 6
          (x, 0) :: (16, take - 3) :: go (xs.drop take)
        else
          (x, 0) :: go xs
  termination_by xs => xs.length
  decreasing_by all_goals sorry

/-- Decode a list of CL entries back into code lengths.
    This is the pure (non-Huffman) inverse of `rlEncodeLengths`.
    Code 16 requires a previous value, so we use an accumulator. -/
def rlDecodeLengths (entries : List CLEntry) : Option (List Nat) :=
  go entries []
where
  go : List CLEntry → List Nat → Option (List Nat)
    | [], acc => some acc
    | (code, extra) :: rest, acc =>
      if code ≤ 15 then
        go rest (acc ++ [code])
      else if code == 16 then do
        guard (acc.length > 0)
        let prev := acc.getLast!
        go rest (acc ++ List.replicate (extra + 3) prev)
      else if code == 17 then
        go rest (acc ++ List.replicate (extra + 3) 0)
      else if code == 18 then
        go rest (acc ++ List.replicate (extra + 11) 0)
      else none


/-- CL code length alphabet order for DEFLATE dynamic block headers
    (RFC 1951 §3.2.7). This is the same permutation as `codeLengthOrder`
    on the decode side. -/
protected def clPermutation : List Nat :=
  [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]

/-- Count frequencies of CL symbols (0–18) in a list of `CLEntry` pairs.
    Returns a list of 19 frequency counts indexed by symbol. -/
def clSymbolFreqs (entries : List CLEntry) : List Nat :=
  entries.foldl (fun acc (code, _) =>
    if code < acc.length then acc.set code (acc.getD code 0 + 1) else acc)
    (List.replicate 19 0)

/-- Encode the extra bits for a CL entry.
    - Code 16: 2 extra bits (repeat count − 3)
    - Code 17: 3 extra bits (repeat count − 3)
    - Code 18: 7 extra bits (repeat count − 11)
    - Codes 0–15: no extra bits -/
def encodeCLExtra (code extra : Nat) : List Bool :=
  if code == 16 then writeBitsLSB 2 extra
  else if code == 17 then writeBitsLSB 3 extra
  else if code == 18 then writeBitsLSB 7 extra
  else []

/-- Write the CL code lengths in permuted order as 3-bit values.
    This is the inverse of `readCLLengths`. -/
def writeCLLengths (clLens : List Nat) (numCodeLen : Nat) : List Bool :=
  (Deflate.Spec.clPermutation.take numCodeLen).flatMap fun pos =>
    writeBitsLSB 3 (clLens.getD pos 0)

/-- Encode CL entries using the CL Huffman table.
    Each entry is encoded as: Huffman codeword + extra bits.
    This is the inverse of `decodeCLSymbols`. -/
def encodeCLEntries (clTable : List (Huffman.Spec.Codeword × Nat))
    (entries : List CLEntry) : Option (List Bool) :=
  match entries with
  | [] => some []
  | (code, extra) :: rest => do
    let cw ← encodeSymbol clTable code
    let restBits ← encodeCLEntries clTable rest
    return cw ++ encodeCLExtra code extra ++ restBits

/-- Determine the number of CL code lengths to transmit (HCLEN + 4).
    We need at least 4 and find the last non-zero entry in permuted order. -/
def computeHCLEN (clLens : List Nat) : Nat :=
  let permutedLens := Deflate.Spec.clPermutation.map fun pos => clLens.getD pos 0
  let lastNonZero := permutedLens.length -
    (permutedLens.reverse.takeWhile (· == 0)).length
  max 4 lastNonZero

/-- Encode the dynamic Huffman tree header for a DEFLATE dynamic block.
    Takes lit/len code lengths and distance code lengths, returns the
    header bit sequence that `decodeDynamicTables` can decode.
    Returns `none` if the code lengths cannot be validly encoded. -/
def encodeDynamicTrees (litLens : List Nat) (distLens : List Nat) :
    Option (List Bool) := do
  -- Validate input sizes
  guard (litLens.length ≥ 257 ∧ litLens.length ≤ 288)
  guard (distLens.length ≥ 1 ∧ distLens.length ≤ 32)
  let hlit := litLens.length - 257
  let hdist := distLens.length - 1

  -- Step 1: RLE-encode the concatenated code lengths
  let allLens := litLens ++ distLens
  let clEntries := rlEncodeLengths allLens

  -- Step 2: Compute CL code lengths from symbol frequencies
  let clFreqs := clSymbolFreqs clEntries
  let clFreqPairs := (List.range clFreqs.length).map fun i => (i, clFreqs.getD i 0)
  let clLens := Huffman.Spec.computeCodeLengths clFreqPairs 19 7

  -- Step 3: Build CL Huffman codes
  let clCodes := Huffman.Spec.allCodes clLens 7
  let clTable := clCodes.map fun (sym, cw) => (cw, sym)

  -- Step 4: Determine HCLEN
  let numCodeLen := computeHCLEN clLens
  let hclen := numCodeLen - 4

  -- Step 5: Encode CL entries using the CL Huffman table
  let symbolBits ← encodeCLEntries clTable clEntries

  -- Step 6: Assemble header bits
  let headerBits := writeBitsLSB 5 hlit ++
    writeBitsLSB 5 hdist ++
    writeBitsLSB 4 hclen
  let clLenBits := writeCLLengths clLens numCodeLen

  return headerBits ++ clLenBits ++ symbolBits

end Deflate.Spec
