namespace Deflate.Spec

/-- The symbols produced by DEFLATE Huffman decoding, before LZ77
    back-reference resolution. -/
inductive LZ77Symbol where
  /-- A literal byte (codes 0–255). -/
  | literal (byte : UInt8)
  /-- A length-distance back-reference (length codes 257–285 + distance). -/
  | reference (length : Nat) (distance : Nat)
  /-- End of block marker (code 256). -/
  | endOfBlock
  deriving Repr, BEq

/-- Resolve a sequence of LZ77 symbols to produce output bytes.
    Returns `none` if any back-reference is invalid (distance exceeds
    current output size). -/
def resolveLZ77 : List LZ77Symbol → List UInt8 → Option (List UInt8)
  | [], acc => some acc
  | .literal b :: rest, acc => resolveLZ77 rest (acc ++ [b])
  | .endOfBlock :: _, acc => some acc
  | .reference len dist :: rest, acc =>
    if dist == 0 || dist > acc.length then none
    else
      let start := acc.length - dist
      let copied := List.ofFn fun (i : Fin len) =>
        acc[start + (i.val % dist)]!
      resolveLZ77 rest (acc ++ copied)

/-! ## resolveLZ77 properties -/

/-- Empty symbol list returns the accumulator unchanged. -/
@[simp] theorem resolveLZ77_nil (acc : List UInt8) :
    resolveLZ77 [] acc = some acc := rfl

/-- End-of-block marker returns the accumulator, ignoring remaining symbols. -/
@[simp] theorem resolveLZ77_endOfBlock (rest : List LZ77Symbol) (acc : List UInt8) :
    resolveLZ77 (.endOfBlock :: rest) acc = some acc := rfl

/-- A literal symbol appends the byte and continues resolving. -/
@[simp] theorem resolveLZ77_literal (b : UInt8) (rest : List LZ77Symbol) (acc : List UInt8) :
    resolveLZ77 (.literal b :: rest) acc = resolveLZ77 rest (acc ++ [b]) := rfl

/-! ## LZ77 matching (greedy encoder) -/
/-- Count consecutive matching bytes at position `pos` with source at
    distance `dist` back, using DEFLATE's overlapping-copy semantics.
    Returns 0 if `dist > pos` or `dist = 0`. -/
def matchLength (data : List UInt8) (pos dist : Nat)
    (maxLen : Nat := 258) : Nat :=
  if dist == 0 || dist > pos then 0
  else matchLength.go data pos dist 0 maxLen
where
  go (data : List UInt8) (pos dist count maxLen : Nat) : Nat :=
    if count ≥ maxLen then count
    else
      match data[pos + count]?, data[pos - dist + (count % dist)]? with
      | some a, some b =>
        if a == b then go data pos dist (count + 1) maxLen else count
      | _, _ => count
  termination_by maxLen - count

/-- Find the longest match at position `pos`, scanning distances
    1 to `min pos windowSize`. Returns `(length, distance)` if a
    match of length ≥ 3 is found. -/
def findLongestMatch (data : List UInt8) (pos : Nat)
    (windowSize : Nat := 32768) : Option (Nat × Nat) :=
  go data pos 1 (min pos windowSize) none
where
  go (data : List UInt8) (pos d maxDist : Nat)
      (best : Option (Nat × Nat)) : Option (Nat × Nat) :=
    if d > maxDist then best
    else
      let len := matchLength data pos d
      let best' := match best with
        | some (bestLen, _) => if len > bestLen then some (len, d) else best
        | none => if len ≥ 3 then some (len, d) else none
      go data pos (d + 1) maxDist best'
  termination_by maxDist + 1 - d

/-- Greedy LZ77 matching: at each position, emit the longest match
    or a literal. Terminates with endOfBlock. -/
def matchLZ77 (data : List UInt8) (windowSize : Nat := 32768) :
    List LZ77Symbol :=
  go data 0 windowSize
where
  go (data : List UInt8) (pos windowSize : Nat) : List LZ77Symbol :=
    if pos ≥ data.length then [.endOfBlock]
    else
      match findLongestMatch data pos windowSize with
      | some (len, dist) =>
        if len ≥ 3 then
          .reference len dist :: go data (pos + len) windowSize
        else
          .literal data[pos]! :: go data (pos + 1) windowSize
      | none =>
        .literal data[pos]! :: go data (pos + 1) windowSize
  termination_by data.length - pos

/-! ## LZ77 matching properties -/

/-- Lazy LZ77 matching (Level 2): at each position, find the longest match,
    then check if a longer match exists at the next position. If so,
    emit a literal and use the longer match instead. -/
def matchLZ77Lazy (data : List UInt8) (windowSize : Nat := 32768) :
    List LZ77Symbol :=
  go data 0 windowSize
where
  go (data : List UInt8) (pos windowSize : Nat) : List LZ77Symbol :=
    if pos ≥ data.length then [.endOfBlock]
    else
      match findLongestMatch data pos windowSize with
      | some (len1, dist1) =>
        if len1 < 3 then
          .literal data[pos]! :: go data (pos + 1) windowSize
        else if pos + 1 < data.length then
          match findLongestMatch data (pos + 1) windowSize with
          | some (len2, dist2) =>
            if len2 > len1 then
              .literal data[pos]! ::
                .reference len2 dist2 :: go data (pos + 1 + len2) windowSize
            else
              .reference len1 dist1 :: go data (pos + len1) windowSize
          | none =>
            .reference len1 dist1 :: go data (pos + len1) windowSize
        else
          .reference len1 dist1 :: go data (pos + len1) windowSize
      | none =>
        .literal data[pos]! :: go data (pos + 1) windowSize
  termination_by data.length - pos


end Deflate.Spec
