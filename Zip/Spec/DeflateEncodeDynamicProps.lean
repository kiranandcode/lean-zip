import Zip.Spec.DeflateEncodeDynamic
import Zip.Spec.DeflateEncodeProps

namespace Deflate.Spec

/-! ## encodeCLEntries success -/

private abbrev clFreqFoldl := fun (acc : List Nat) (p : CLEntry) =>
  if p.1 < acc.length then acc.set p.1 (acc.getD p.1 0 + 1) else acc


end Deflate.Spec
