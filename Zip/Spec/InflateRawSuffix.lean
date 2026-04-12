import Zip.Native.Inflate
import ZipCommon.Spec.BinaryCorrect
import Zip.Spec.InflateComplete

namespace Zip.Native

open ZipCommon (BitReader)

private abbrev brAppend (br : BitReader) (suffix : ByteArray) : BitReader :=
  ⟨br.data ++ suffix, br.pos, br.bitOff⟩


-- Tactic macro for the bfinal dispatch in inflateLoop_append_suffix.
-- Handles both bfinal=1 (alignToByte) and bfinal≠1 (WF guards + recursive call).
set_option hygiene false in
local macro "bfinal_suffix_dispatch" : tactic =>
  `(tactic| (
    by_cases hbf1 : (bfinal == 1) = true
    next =>
      rw [if_pos hbf1] at h ⊢; simp only [pure, Except.pure] at h ⊢
      rw [alignToByte_append]; exact h
    next =>
      rw [if_neg hbf1] at h ⊢
      split at h
      next => exact nomatch h
      next h_progress =>
        split at h
        next => exact nomatch h
        next =>
          split
          next h₁' => exact absurd h₁' h_progress
          next =>
            split
            next h₂' => exact absurd h₂' (by assumption)
            next => exact ih _ (by omega) br' out' result endPos Nat.le.refl h))

end Zip.Native
