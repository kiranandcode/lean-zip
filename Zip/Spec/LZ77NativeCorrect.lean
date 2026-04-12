import Zip.Native.Deflate
import Zip.Spec.LZ77
import ZipForStd.ByteArray

namespace Zip.Native.Deflate

/-- Convert a native LZ77Token to a spec LZ77Symbol. -/
def LZ77Token.toLZ77Symbol : LZ77Token → Deflate.Spec.LZ77Symbol
  | .literal b => .literal b
  | .reference len dist => .reference len dist

/-- Convert native LZ77 token array to spec symbol list with end-of-block. -/
def tokensToSymbols (tokens : Array LZ77Token) : List Deflate.Spec.LZ77Symbol :=
  tokens.toList.map LZ77Token.toLZ77Symbol ++ [.endOfBlock]

/-- A token list is a valid decomposition of `data` starting at position `pos`.
    Each literal has the correct byte, each reference has matching bytes in the
    lookback window, and tokens cover `data[pos..]` contiguously. -/
inductive ValidDecomp (data : ByteArray) : Nat → List LZ77Token → Prop where
  | done (h : pos ≥ data.size) : ValidDecomp data pos []
  | literal {b : UInt8} {tokens : List LZ77Token}
      (hpos : pos < data.size)
      (hb : data[pos]! = b)
      (rest : ValidDecomp data (pos + 1) tokens) :
      ValidDecomp data pos (.literal b :: tokens)
  | reference {len dist : Nat} {tokens : List LZ77Token}
      (hlen : len ≥ 3) (hdist_pos : dist ≥ 1) (hdist_le : dist ≤ pos)
      (hlen_le : pos + len ≤ data.size)
      (hmatch : ∀ i, i < len → data[pos + i]! = data[pos - dist + i]!)
      (rest : ValidDecomp data (pos + len) tokens) :
      ValidDecomp data pos (.reference len dist :: tokens)

private def Encodable (t : LZ77Token) : Prop :=
  match t with
  | .literal _ => True
  | .reference len dist => 3 ≤ len ∧ len ≤ 258 ∧ 1 ≤ dist ∧ dist ≤ 32768


end Zip.Native.Deflate
