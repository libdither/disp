# Sort — from type to synthesizer target

A worked example showing how dependent types capture a full spec, and
how a spec becomes a synthesizer target. Meant as a teaching example;
not implemented.

## Supporting predicates

```disp
-- Sortedness: every adjacent pair is ordered.
let Sorted : {A : Type} -> (A -> A -> Bool) -> List A -> Type
  = fix ({self, A, le, xs} ->
      case xs of
        nil         -> Unit
        cons x rest -> case rest of
          nil        -> Unit
          cons y _   -> { step : Eq Bool (le x y) TT
                        , tail : self A le rest })

-- Permutation: inductive relation; definition omitted.
let Perm : {A : Type} -> List A -> List A -> Type = ...
```

## SortType — the type of correct sorting functions

```disp
let SortType : {A : Type} -> (A -> A -> Bool) -> Type
  = {A, le} -> (xs : List A) ->
      { ys       : List A
      , sorted   : Sorted le ys
      , permuted : Perm xs ys }
```

`SortType le` is the type of functions from `List A` to a record
carrying the output list plus proofs that it's sorted and a permutation
of the input.

A correct implementation inhabits this type:

```disp
let sort : SortType le = ...      -- any term of this type is a correct sort
```

Wrong implementations fail to type-check:

- Returning `[]` regardless of input — `Perm xs []` is inhabited only
  when `xs = []`; proof can't be constructed.
- Returning input unchanged — `Sorted` proof fails unless input was
  already sorted.
- Returning a sorted but unrelated list — `Perm` proof fails.

## Why `Val -> Bool` isn't enough for synthesis

`SortType` is a predicate: `Val -> Bool`. A candidate either inhabits
the type (TT) or doesn't (FF). Binary.

A synthesizer searches the space of candidate terms. Binary feedback
offers no gradient — the synthesizer can't tell whether a failing
candidate was "almost right" or "wildly wrong." It needs a scoring
function:

```
score : Val -> Nat       -- 0 = perfect; larger = further from correct
```

Smaller scores indicate "closer to correct." The synthesizer's job is
to drive this toward 0.

## Modifying SortType into a synthesizer target

A synthesizer target is a `Val -> Nat` scoring function built from the
spec plus auxiliary metrics. Three modifications:

### 1. Specialize the implicits

Pin `A` and `le` to concrete types so the synthesizer searches a fixed
space:

```disp
let NatSort : Type = SortType NatLe
```

where `NatLe : Nat -> Nat -> Bool` is concrete Nat comparison.

### 2. Build the score from verification + examples + size

Three ingredients combine into a score:

```disp
-- Verification: 0 if candidate has type NatSort, else 1.
let verify : Val -> Nat
  = {v} -> if check v NatSort then zero else one

-- Example mismatches: count inputs where the candidate's output
-- disagrees with expected.
let examples : List { input : List Nat, expected : List Nat }
  = [ { input := [3, 1, 2],    expected := [1, 2, 3] }
    , { input := [],            expected := []         }
    , { input := [5, 5, 5],     expected := [5, 5, 5]  }
    , { input := [4, 4, 1, 1],  expected := [1, 1, 4, 4] } ]

let example_mismatches : Val -> Nat
  = {v} -> count ({ex} ->
      not (eq_list_nat (project_ys (v ex.input)) ex.expected)) examples

-- Size excess: amount by which candidate exceeds the budget.
let budget : Nat = 200

let size_excess : Val -> Nat
  = {v} -> max zero (sub (size v) budget)
```

### 3. Combine into a single score

```disp
let NatSortScore : Val -> Nat
  = {v} -> add (mul (verify v) 1000)         -- spec violation: heavy weight
           (add (example_mismatches v)        -- each example miss: 1 point
                (size_excess v))              -- size overrun: linear penalty
```

`NatSortScore v = 0` iff `v` type-checks against `NatSort`, matches
every example, and fits within `budget`. Any failure contributes
positive weight; the synthesizer searches for terms that drive the
total to 0.

Weights are tunable — in practice the spec violation weight is very
large so that type-incorrect terms never "win" just by happening to
pass examples.

## What the synthesizer sees

A concrete scoring function:

```disp
let target : Val -> Nat = NatSortScore
```

The synthesizer's job: find a term `t` minimizing `target t`. When
`target t = 0`, the candidate:

- type-checks (so it's a correct sort by the full spec),
- matches every listed example, and
- fits within the size budget.

During search, the synthesizer uses the score's gradient: candidates
with lower scores are "more promising" than higher-scoring ones, even
before hitting 0.

## The broader pattern

Turning any dependently-typed spec into a synthesizer target:

1. **Specialize** — pin polymorphic parameters to concrete types.
2. **Score, not verify** — construct `Val -> Nat` from the type plus
   auxiliary metrics. `0 = correct`; positive values indicate
   distance.
3. **Combine** — weight the components so spec violations dominate
   example mismatches dominate size overruns (or whatever ordering
   fits the domain).

The type IS the specification; the score IS what makes search
tractable. A good score's zero-set is exactly the spec's inhabitants —
both components are needed, and they serve different roles
(verification vs. guidance).

## Under the hood

`Val -> Nat` is a function in the disp object language like any other.
The auxiliary pieces (`check`, `size`, `count`) are either kernel
primitives or library-level disp functions. The target is
fully-reified — first-class, manipulable, composable — so the
synthesizer itself can be a disp program that consumes targets and
produces candidates.

## Notes

- `Eq Bool (le x y) TT` says "`le x y` evaluates to `TT`." In practice
  an `IsTrue : Bool -> Type` predicate is more ergonomic.
- `Perm` is non-trivial; it's an inductive relation with constructors
  for "empty permutation of empty" and "insertion preserves
  permutation." Elided here.
- `case` syntax used here is informal; the actual surface grammar may
  use different pattern-matching forms once inductive types are in
  scope.
- `size` of a term is its tree-calculus node count; assumed as a kernel
  primitive or a simple disp function over the tree structure.
- `check v T` runs the type-checker on candidate `v` against type `T`
  and returns Bool. Assumed available as a library function.
