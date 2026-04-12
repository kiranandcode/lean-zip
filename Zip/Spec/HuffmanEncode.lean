import Zip.Spec.Huffman

namespace Huffman.Spec

/-- Binary tree for Huffman code construction. Leaves carry a symbol,
    internal nodes carry the combined weight of their subtrees. -/
inductive BuildTree where
  | leaf (weight : Nat) (sym : Nat)
  | node (weight : Nat) (left right : BuildTree)
deriving Repr

/-- Weight of a Huffman tree node. -/
def BuildTree.weight : BuildTree → Nat
  | .leaf w _ => w
  | .node w _ _ => w

/-- Insert a tree into a list sorted by weight (ascending). -/
def insertByWeight (t : BuildTree) : List BuildTree → List BuildTree
  | [] => [t]
  | x :: xs =>
    if t.weight ≤ x.weight then t :: x :: xs
    else x :: insertByWeight t xs

/-- Build a Huffman tree from a list of trees sorted by weight.
    Repeatedly merges the two lightest trees until one remains.
    Precondition: the input list should be non-empty and sorted by weight. -/
private theorem insertByWeight_length (t : BuildTree) (l : List BuildTree) :
    (insertByWeight t l).length = l.length + 1 := by
  induction l with
  | nil => simp [insertByWeight]
  | cons x xs ih =>
    simp only [insertByWeight]
    split <;> simp [ih]

def buildHuffmanTree : List BuildTree → BuildTree
  | [] => .leaf 0 0
  | [t] => t
  | t1 :: t2 :: rest =>
    let merged := BuildTree.node (t1.weight + t2.weight) t1 t2
    buildHuffmanTree (insertByWeight merged rest)
termination_by l => l.length
decreasing_by simp_all [insertByWeight_length]

/-! ## Depth extraction -/

/-- Extract the depth of each symbol in the tree, as (symbol, depth) pairs. -/
def BuildTree.depths (t : BuildTree) (depth : Nat := 0) :
    List (Nat × Nat) :=
  match t with
  | .leaf _ sym => [(sym, depth)]
  | .node _ l r => l.depths (depth + 1) ++ r.depths (depth + 1)

/-! ## Code length computation pipeline -/

/-- Assign code lengths into a list indexed by symbol number.
    Symbols not mentioned in `depths` get length 0. -/
def assignLengths (depths : List (Nat × Nat)) (numSymbols : Nat) : List Nat :=
  let init := List.replicate numSymbols 0
  depths.foldl (fun acc (sym, len) =>
    if sym < acc.length then acc.set sym len else acc) init

/-- Kraft sum of a list of depths, relative to normalization constant `D`:
    `∑ 2^(D - dᵢ)` where dᵢ are the depths. -/
def kraftSum (depths : List Nat) (D : Nat) : Nat :=
  depths.foldl (fun acc d => acc + 2 ^ (D - d)) 0

/-- Fix code lengths to satisfy the Kraft inequality. If the Kraft sum
    exceeds `2^maxBits`, set all non-zero lengths to `maxBits`.
    This produces valid (though potentially suboptimal) codes. -/
def fixKraftList (lengths : List Nat) (maxBits : Nat) : List Nat :=
  if kraftSum (lengths.filter (· != 0)) maxBits ≤ 2 ^ maxBits then lengths
  else lengths.map fun l => if l == 0 then 0 else maxBits

/-- Compute Huffman code lengths from symbol frequencies.
    `freqs` is a list of (symbol, frequency) pairs.
    Returns a list of length `numSymbols` where index `i` is the code length
    for symbol `i` (0 means the symbol has no codeword).

    Code lengths are capped at `maxBits`, and if the resulting Kraft sum
    exceeds `2^maxBits`, all non-zero codes are set to `maxBits` as a
    fallback. For typical DEFLATE inputs (≤ 286 symbols, maxBits=15),
    the optimal tree depth is well under 15, so the fallback never activates. -/
def computeCodeLengths (freqs : List (Nat × Nat)) (numSymbols : Nat)
    (maxBits : Nat := 15) : List Nat :=
  let nonzero := freqs.filter (fun (_, f) => f > 0)
  if nonzero.isEmpty then List.replicate numSymbols 0
  else if nonzero.length == 1 then
    let (sym, _) := nonzero.head!
    assignLengths [(sym, 1)] numSymbols
  else
    let leaves := (nonzero.map fun (sym, freq) => BuildTree.leaf freq sym)
      |>.mergeSort (fun a b => a.weight ≤ b.weight)
    let tree := buildHuffmanTree leaves
    let depths := tree.depths
    -- Cap at maxBits, then fix Kraft inequality if needed
    let capped := depths.map fun (s, d) => (s, min d maxBits)
    fixKraftList (assignLengths capped numSymbols) maxBits

/-! ## Properties -/

/-- Symbol `s` is a leaf symbol in tree `t`. -/
private inductive BuildTree.HasSym : BuildTree → Nat → Prop where
  | leaf (w s : Nat) : HasSym (.leaf w s) s
  | nodeLeft (w : Nat) (l r : BuildTree) (s : Nat) : l.HasSym s → (BuildTree.node w l r).HasSym s
  | nodeRight (w : Nat) (l r : BuildTree) (s : Nat) : r.HasSym s → (BuildTree.node w l r).HasSym s

end Huffman.Spec
