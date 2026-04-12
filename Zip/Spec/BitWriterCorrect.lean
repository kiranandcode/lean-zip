import Zip.Native.BitWriter
import Zip.Spec.Deflate
import Zip.Spec.Huffman

namespace Zip.Native.BitWriter

/-- The logical bit sequence represented by a BitWriter. -/
def toBits (bw : BitWriter) : List Bool :=
  bw.data.data.toList.flatMap Deflate.Spec.bytesToBits.byteToBits ++
  (List.range bw.bitCount.toNat).map (fun i => bw.bitBuf.toNat.testBit i)

/-- Well-formedness: bitCount < 8, no stale bits above bitCount. -/
def wf (bw : BitWriter) : Prop :=
  bw.bitCount.toNat < 8 ∧ bw.bitBuf.toNat < 2 ^ bw.bitCount.toNat

theorem empty_wf : empty.wf := by
  constructor <;> simp only [empty, UInt8.toNat_zero, Nat.zero_lt_succ, Nat.pow_zero, Nat.lt_add_one]

theorem empty_toBits : empty.toBits = [] := by
  simp only [toBits, empty, ByteArray.data_empty, Array.toList_empty, List.flatMap_nil,
    UInt8.toNat_zero, Nat.zero_testBit, List.range_zero, List.map_nil, List.append_nil]


end Zip.Native.BitWriter
