import Zip.Spec.Huffman
import ZipForStd.Array

namespace Huffman.Spec

/-- Simple recursive definition of the nextCodes recurrence:
    `ncRec blCount 0 = 0`, `ncRec blCount (b+1) = (ncRec blCount b + blCount[b]!) * 2`.
    This matches what `nextCodes.go` computes at each step. -/
protected def ncRec (blCount : Array Nat) : Nat → Nat
  | 0 => 0
  | b + 1 => (Huffman.Spec.ncRec blCount b + blCount[b]!) * 2

/-- Partial Kraft sum from position `start` to `maxBits`:
    `∑_{i=start}^{maxBits} blCount[i]! * 2^(maxBits - i)`. -/
private def kraftSumFrom (blCount : Array Nat) (maxBits b : Nat) : Nat :=
  if b > maxBits then 0
  else blCount[b]! * 2 ^ (maxBits - b) + kraftSumFrom blCount maxBits (b + 1)
termination_by maxBits + 1 - b


end Huffman.Spec
