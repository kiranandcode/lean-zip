import Zip.Spec.BitstreamCorrect
import Zip.Spec.BitstreamComplete


namespace Deflate.Correctness

/-- Pure tree decode on a bit list. Follows the same logic as
    `HuffTree.decode.go` but operates on `List Bool` instead of `BitReader`. -/
def decodeBits : Zip.Native.HuffTree → List Bool → Option (UInt16 × List Bool)
  | .leaf s, bits => some (s, bits)
  | .empty, _ => none
  | .node _ _, [] => none
  | .node _ o, true :: bits => decodeBits o bits
  | .node z _, false :: bits => decodeBits z bits


/-- Predicate: tree `t` has a leaf with symbol `sym` at path `cw`,
    where `false` means "go left (zero)" and `true` means "go right (one)". -/
inductive TreeHasLeaf : Zip.Native.HuffTree → List Bool → UInt16 → Prop
  | leaf : TreeHasLeaf (.leaf s) [] s
  | left : TreeHasLeaf z cw s → TreeHasLeaf (.node z o) (false :: cw) s
  | right : TreeHasLeaf o cw s → TreeHasLeaf (.node z o) (true :: cw) s

/-- Predicate: tree `t` has no leaf at an intermediate position along `path`.
    This ensures `insert.go` can traverse the path without hitting a collision. -/
def NoLeafOnPath : Zip.Native.HuffTree → List Bool → Prop
  | .leaf _, _ :: _ => False
  | .node z _, false :: rest => NoLeafOnPath z rest
  | .node _ o, true :: rest => NoLeafOnPath o rest
  | _, _ => True

end Deflate.Correctness
