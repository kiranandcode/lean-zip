import Zip.Spec.Adler32

namespace Adler32.Native

/-- Process a `ByteArray`, updating the Adler-32 state. -/
def updateBytes (s : Spec.State) (data : ByteArray) : Spec.State :=
  data.data.foldl Spec.updateByte s

/-- Compute Adler-32 of an entire `ByteArray`. -/
def adler32 (init : UInt32 := 1) (data : ByteArray) : UInt32 :=
  let s := Spec.unpack init
  let s' := updateBytes s data
  Spec.pack s'

end Adler32.Native
