import ZipCommon.Spec.BitReaderInvariant
import Zip.Native.Inflate

/-!
# DEFLATE-specific BitReader invariant preservation

Generic BitReader properties (readBit/readBits data preservation, hpos
invariants, bitPos advancement, pos_le_size bounds) are provided by
`ZipCommon.Spec.BitReaderInvariant`.

This file contains DEFLATE-specific invariant preservation lemmas for
operations defined in `Zip.Native.Inflate`: HuffTree decoding, stored
block decoding, Huffman block decoding, and dynamic tree decoding.
-/

namespace Zip.Native

open ZipCommon


end Zip.Native
