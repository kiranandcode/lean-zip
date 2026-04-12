import Zip.Spec.Crc32

namespace Crc32.Native

/-- Precomputed CRC-32 lookup table (256 entries). -/
def table : Array UInt32 := Spec.mkTable

/-- Process a `ByteArray` using the table-driven algorithm. -/
def updateBytes (crc : UInt32) (data : ByteArray) : UInt32 :=
  data.data.foldl (Spec.crcByteTable table) crc

/-- Compute CRC-32 of a `ByteArray`.
    Matches the zlib API: `init = 0` starts a fresh checksum. -/
def crc32 (init : UInt32 := 0) (data : ByteArray) : UInt32 :=
  let raw := if init == 0 then 0xFFFFFFFF else init ^^^ 0xFFFFFFFF
  let result := updateBytes raw data
  result ^^^ 0xFFFFFFFF

end Crc32.Native
