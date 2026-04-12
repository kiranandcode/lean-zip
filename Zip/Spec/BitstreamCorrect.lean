import Zip.Spec.Deflate
import Zip.Native.Inflate
import ZipForStd.List
import ZipForStd.Nat

/-- The spec-level bit list corresponding to a `BitReader`'s current position. -/
def ZipCommon.BitReader.toBits (br : ZipCommon.BitReader) : List Bool :=
  (Deflate.Spec.bytesToBits br.data).drop (br.pos * 8 + br.bitOff)

namespace Deflate.Correctness


end Deflate.Correctness
