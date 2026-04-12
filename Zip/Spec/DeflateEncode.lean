import Zip.Spec.Deflate
import Zip.Spec.DeflateSuffix
import Zip.Spec.HuffmanEncode

namespace Deflate.Spec

/-- Find the length code index for a given length (3–258).
    Returns `(index, extraBitsCount, extraBitsValue)` where
    the length code symbol is `257 + index`. -/
def findLengthCode (length : Nat) : Option (Nat × Nat × Nat) :=
  go 0
where
  go (i : Nat) : Option (Nat × Nat × Nat) :=
    if h : i ≥ lengthBase.size then none
    else
      let base := lengthBase[i]
      let nextBase := lengthBase[i + 1]?.getD 259
      if base ≤ length && length < nextBase then
        some (i, lengthExtra[i]!, length - base)
      else go (i + 1)
  termination_by lengthBase.size - i

/-- Find the distance code for a given distance (1–32768).
    Returns `(code, extraBitsCount, extraBitsValue)`. -/
def findDistCode (distance : Nat) : Option (Nat × Nat × Nat) :=
  go 0
where
  go (i : Nat) : Option (Nat × Nat × Nat) :=
    if h : i ≥ distBase.size then none
    else
      let base := distBase[i]
      let nextBase := distBase[i + 1]?.getD 32769
      if base ≤ distance && distance < nextBase then
        some (i, distExtra[i]!, distance - base)
      else go (i + 1)
  termination_by distBase.size - i

/-- Look up the Huffman codeword for a symbol in the code table.
    Returns the codeword or `none` if the symbol is not in the table. -/
def encodeSymbol (table : List (Huffman.Spec.Codeword × Nat))
    (sym : Nat) : Option (List Bool) :=
  match table with
  | [] => none
  | (cw, s) :: rest => if s == sym then some cw else encodeSymbol rest sym

/-- Encode one LZ77 symbol as Huffman-coded bits.
    Inverse of `decodeLitLen`. -/
def encodeLitLen (litLengths distLengths : List Nat)
    (sym : LZ77Symbol) : Option (List Bool) := do
  let litCodes := Huffman.Spec.allCodes litLengths
  let litTable := litCodes.map fun (s, cw) => (cw, s)
  match sym with
  | .literal b => encodeSymbol litTable b.toNat
  | .endOfBlock => encodeSymbol litTable 256
  | .reference len dist => do
    let (idx, extraN, extraV) ← findLengthCode len
    let lenBits ← encodeSymbol litTable (257 + idx)
    let distCodes := Huffman.Spec.allCodes distLengths
    let distTable := distCodes.map fun (s, cw) => (cw, s)
    let (dCode, dExtraN, dExtraV) ← findDistCode dist
    let distBits ← encodeSymbol distTable dCode
    return lenBits ++ writeBitsLSB extraN extraV ++
           distBits ++ writeBitsLSB dExtraN dExtraV

/-- Encode a list of LZ77 symbols as Huffman-coded bits. -/
def encodeSymbols (litLengths distLengths : List Nat)
    (syms : List LZ77Symbol) : Option (List Bool) :=
  match syms with
  | [] => some []
  | s :: rest => do
    let bits ← encodeLitLen litLengths distLengths s
    let restBits ← encodeSymbols litLengths distLengths rest
    return bits ++ restBits


/-- A symbol list is valid for decoding: ends with `.endOfBlock` and
    no `.endOfBlock` appears earlier. -/
def ValidSymbolList : List LZ77Symbol → Prop
  | [] => False
  | [.endOfBlock] => True
  | .endOfBlock :: _ => False
  | _ :: rest => ValidSymbolList rest

/-- Encode a list of LZ77 symbols as a single fixed-Huffman DEFLATE block.
    Produces BFINAL=1 + BTYPE=01 header followed by Huffman-coded symbols.
    Returns `none` if any symbol cannot be encoded. -/
def encodeFixed (syms : List LZ77Symbol) : Option (List Bool) := do
  let bits ← encodeSymbols fixedLitLengths fixedDistLengths syms
  return [true, true, false] ++ bits


end Deflate.Spec
