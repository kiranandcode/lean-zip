import Zip.Native.Inflate
import ZipForStd.ByteArray
import Std.Tactic.BVDecide

namespace Zip.Spec.DeflateStoredCorrect

open Zip.Native
open ZipCommon (BitReader)

/-- Pure recursive version of deflateStored for proof purposes.
    Encodes data as stored DEFLATE blocks starting from position `pos`.
    Each block has at most 65535 data bytes. The last block has BFINAL=1.
    When `pos ≥ data.size`, produces a single final empty block. -/
def deflateStoredPure (data : ByteArray) (pos : Nat := 0) : ByteArray :=
  let blockSize := min (data.size - pos) 65535
  if _ : pos + blockSize ≥ data.size then
    let len := blockSize.toUInt16
    let nlen := len ^^^ 0xFFFF
    ByteArray.mk #[0x01, (len &&& 0xFF).toUInt8,
      ((len >>> 8) &&& 0xFF).toUInt8, (nlen &&& 0xFF).toUInt8,
      ((nlen >>> 8) &&& 0xFF).toUInt8] ++ data.extract pos (pos + blockSize)
  else
    let len := blockSize.toUInt16
    let nlen := len ^^^ 0xFFFF
    let hdr := ByteArray.mk #[0x00, (len &&& 0xFF).toUInt8,
      ((len >>> 8) &&& 0xFF).toUInt8, (nlen &&& 0xFF).toUInt8,
      ((nlen >>> 8) &&& 0xFF).toUInt8]
    (hdr ++ data.extract pos (pos + blockSize)) ++
      deflateStoredPure data (pos + blockSize)
termination_by data.size - pos
decreasing_by omega

/-- Number of stored blocks: ⌈max(n,1) / 65535⌉, which equals (n-1)/65535 + 1 in Nat. -/
private def numStoredBlocks (n : Nat) : Nat := (n - 1) / 65535 + 1

end Zip.Spec.DeflateStoredCorrect
