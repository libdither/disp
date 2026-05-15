# Records and Refinement-Typed Projection — Proposal

> STATUS: Proposed and design-frozen (not yet implemented). Drafted 2026-05-14. All eight design decisions are resolved (see §11). Implementation can begin with `Refinement` (step 1 of the §10.1 landing order). Audit (see §10.2) confirmed hard cutover is safe — zero current uses of `{}` in `.disp` source and zero non-`open use` value bindings.

## 1. Motivation

The current record machinery (`SYNTAX.typ` §"Programs, record bodies, and items", `COMPILATION.typ` §"Record encoding") works but has two costs:

1. **Bespoke compile-time tracking.** `src/compile.ts:413–426` (`proj` case) walks back to construction sites via `resolveExprRecord` to resolve field names. The elaborator maintains parallel data structures (`record.fields`, `record.fieldTrees`, `record.fieldInnerFields`) that exist only for projection support. This logic doesn't compose with the rest of the kernel — it's a special case in elaboration.

2. **No tree-calculus error reporting for projection mistakes.** `r.bogus` raises a host-level `Error("projection '.bogus': field not found")` in `compile.ts`. The error doesn't come from the type system; it comes from a TypeScript check that has no analogue in the object language.

This proposal reformulates records so that:

- A record is an iterated `Sigma`, with names tracked in the type's metadata.
- Projection is a tree-native function whose argument refinement type rejects invalid field names. **Compile-time errors for `r.bogus` come from kernel predicate evaluation**, identical to how `Bool TT = TT` is decided.
- The current bespoke elaborator machinery is replaced by a thin syntactic desugaring layer (`r.x ⟶ proj r "x"`) plus an optional inlining pass that reduces literal-name projections to direct chains for speed.
- Dependent records work automatically because iterated `Sigma` already handles dependence.

The win is a smaller elaborator, a tree-calculus-native error model, and dependent records for free.

## 2. Design principles

1. **Object language is the spec.** Records compile to existing kernel-validated structures (`Sigma`, `Refinement`). No new kernel primitives.
2. **Errors are predicate evaluations.** Invalid projections fail because a Refinement type's predicate reduces to `FF`. Not because the host code raised.
3. **Speed is optimization, not semantics.** A "slow path" using runtime name-walk is the reference; an elaborator inlining pass produces direct projection chains. The two are provably equal by reduction.
4. **Names live in types, not values.** A `Record fields` value is a bare nested pair. The field names live in `fields`, which is the type's `params` metadata.
5. **Hash-cons does the heavy lifting.** Distinct field-name sets produce distinct types; identical schemas share trees. Memo makes repeated work free.

## 3. Prerequisites — library types to land first

### 3.1 `Refinement`

Refinement is sketched in `TYPE_THEORY.typ` §5.8 but not yet implemented.

```disp
// lib/types/refinement.disp
//
// Refinement A P : Type
//   inhabited by v iff (A v = TT) AND (P v = TT)
//
// Standard predicate_frame wrapping; sentinel codomain_fn (closed dispatch —
// Refinement is a value type, no function-application hypothesis behavior).

open use "../kernel/utils.disp"
open use "../kernel/handlers.disp"

Refinement_pf := predicate_frame
  ({params, v} ->
    let A = pair_fst params
    let P = pair_snd params
    and (A v) (P v))
  t

Refinement := {A, P} -> wait Refinement_pf (pair A P)

is_refinement := {v} -> has_sig Refinement_pf v
refinement_base := {T} -> pair_fst (type_meta T)         // extracts A
refinement_pred := {T} -> pair_snd (type_meta T)         // extracts P
```

**Tests** (`lib/tests/refinement.test.disp`):

```disp
// Define a local predicate for the test (is_zero is not currently in lib/std)
let is_zero = {n} -> tree_eq n zero

// positive: Nat ∧ is_zero accepts 0, rejects succ 0
test (Refinement Nat is_zero) zero = TT
test (Refinement Nat is_zero) (succ zero) = FF

// negative: malformed value (not a Nat at all)
test (Refinement Nat is_zero) TT = FF

// is_refinement / accessors
test is_refinement (Refinement Nat is_zero) = TT
test refinement_base (Refinement Nat is_zero) = Nat
test refinement_pred (Refinement Nat is_zero) = is_zero
```

Note: `is_zero` should eventually move into `lib/std/nat/ops.disp` as part of standard predicate exports — currently only defined inline in `lib/tests/match.test.disp`.

### 3.2 `Unit`

The terminator for the Sigma chain encoding records.

```disp
// lib/types/unit.disp
//
// Unit : Type
//   inhabited by a single canonical witness `unit_witness`.

open use "../kernel/utils.disp"
open use "../kernel/handlers.disp"

unit_witness := pair t t   // canonical leaf-pair; distinct from common compiled forms

Unit_pf := predicate_frame
  ({_params, v} -> tree_eq v unit_witness)
  t

Unit := wait Unit_pf t   // no parameters; closed singleton

is_unit_witness := {v} -> tree_eq v unit_witness
```

**Tests** (`lib/tests/unit.test.disp`):

```disp
test Unit unit_witness = TT
test Unit t = FF                  // bare leaf isn't unit
test Unit (pair t (pair t t)) = FF
```

Note on the choice of `unit_witness`: `pair t t` is `K` (the constant combinator) in tree-calc. It is already established disp convention — `lib/types/false.disp:30` uses `pair t t` as the "no-params" sentinel in metadata layout. Bool's `TT` is `K K = pair (pair t t) (pair t t)`, not `K` itself, so there's no collision with Bool. Using `pair t t` as `unit_witness` is consistent with existing kernel patterns.

### 3.3 `String` (or just a permissive base type for v1)

Identifiers from disp source are interned to canonical hash-consed trees at parse time. `tree_eq` decides equality in O(1).

**For the records design to work, we don't strictly need a structural `String` recognizer** — we only need:
1. A base type permissive enough to accept any name tree (so `Refinement` can layer constraints on top).
2. The parser to intern identifiers deterministically.

Two equivalent options:

**Option A — permissive String (v1):**
```disp
// String accepts any tree. The Name s refinement is what enforces canonical-ness.
String_pf := predicate_frame
  ({_params, _v} -> TT)
  t

String := wait String_pf t
```

**Option B — proper Scott-byte-list String (future):**
```disp
// String accepts only well-formed Scott byte lists. Recursive predicate.
// is_scott_byte_list is a prelude function defined alongside is_scott_nat.
String_pf := predicate_frame
  ({_params, v} -> is_scott_byte_list v)
  t

String := wait String_pf t
```

**Recommendation: Option A for v1.** The Refinement layer (`Name s`, `ValidField fields`) does the real type-level work — String only exists as a base type for refinement to refine over. The structural check in Option B becomes useful when we want to reject non-string trees passed as names, but for the records use case the parser only emits canonical trees so the structural check is redundant.

**Parser convention** (independent of A or B): the parser maintains an atom table mapping source identifiers to their canonical Scott-byte-list trees. First reference to `"codomain_fn"` builds the Scott list; subsequent references reuse the same tree via hash-cons. Identifiers are interned deterministically (same source identifier → same tree, across sessions).

```disp
// Helpers (parser-emitted; live in prelude):
empty_string := {n, c} -> n
string_cons  := {byte, rest} -> {n, c} -> c byte rest
string_eq    := tree_eq    // hash-cons identity
```

## 4. The `Record` type

### 4.1 Field lists

A field list is a `List (Pair String Type_fn)` where `Type_fn` is either a constant type or a function from prior field values to a type (for dependent records).

```disp
// FieldList shape:
//   nil   = empty record
//   cons (name, type_fn) rest = head field + remaining fields
//
// For non-dependent records, type_fn ignores its argument:
//   ("x", {_} -> Nat)
//
// For dependent records, type_fn binds the predecessor's value:
//   ("x", {A} -> A)   // in a record where A : Type is a previous field
```

The parser canonicalizes field lists by sorting alphabetically on names. This makes `{x := 1, y := 2}` and `{y := 2, x := 1}` produce identical types and identical values.

### 4.2 `Record` as a thin predicate_frame wrapper

```disp
// lib/types/record.disp

open use "../kernel/utils.disp"
open use "../kernel/handlers.disp"
open use "./sigma.disp"
open use "./unit.disp"
open use "./refinement.disp"

// Build the sigma chain corresponding to a field list. Each field's
// type_fn takes the tuple of all preceding values, so dependent fields
// can reference earlier values. For non-dependent fields, type_fn
// simply ignores its argument.
//
//   [(n1, T1), (n2, T2)] with non-dependent T2:
//     ⟶ Sigma (T1 ()) ({v1} -> Sigma (T2 (v1, ())) ({v2} -> Unit))
//
//   [(A, Type), (x, {prev} -> pair_fst prev)] — x depends on A:
//     ⟶ Sigma Type ({A} -> Sigma A ({_} -> Unit))
//
// `sigma_chain` is a metadata-construction helper; the resulting tree
// IS a Sigma chain, and the Record predicate_frame wraps it so the
// type carries the field list as params.

sigma_chain := fix({self, fields, prev_tuple} ->
  match (list_is_nil fields) {
    TT => (Unit)
    FF => (let head = list_head fields
           let rest = list_tail fields
           let ty_fn = pair_snd head
           Sigma (ty_fn prev_tuple)
                 ({v} -> self rest (pair v prev_tuple)))
  })

// Top-level entry: starts with the empty predecessor tuple.
sigma_chain_top := {fields} -> sigma_chain fields unit_witness

// Record itself: predicate_frame wrapping the Sigma chain's recognizer,
// with `fields` carried in params so accessors can read field names.

Record_pf := predicate_frame
  ({params, v} ->
    let sigma_form = sigma_chain_top params
    sigma_form v)
  t   // closed dispatch — Records aren't applied like Pi

Record := {fields} -> wait Record_pf fields

is_record := {v} -> has_sig Record_pf v
record_fields := {T} -> type_meta T   // the FieldList, with names

// Field name list (just the names, no types):
record_names := {T} -> list_map ({nt} -> pair_fst nt) (record_fields T)

// Field type at a given name:
record_type_at := {T, n} -> 
  let fields = record_fields T
  pair_snd (list_find ({nt} -> tree_eq (pair_fst nt) n) fields)
```

**Why a wrapper rather than just exposing iterated Sigma?**

- **Type identity tracks field names.** `Record [("x", Nat)]` and `Record [("a", Nat)]` are different types because their `params` differ — even though their inhabitants are structurally identical (both are `pair v unit_witness`). Without the wrapper, both would be the same `Sigma Nat ({_} -> Unit)` type and `r.x` vs `r.a` would be indistinguishable to the type system.
- **Validation hook.** The Record library type is where we'll attach record-specific laws (extensionality, projection laws) as the strictness work proceeds.

### 4.3 Recognizer behavior

`Record fields` accepts exactly the same value-shapes as the corresponding Sigma chain. Specifically:

- `Record []` (empty record) accepts `unit_witness` and rejects everything else.
- `Record [("x", Nat)]` accepts `pair v unit_witness` iff `Nat v = TT`.
- `Record [("x", Nat), ("y", Bool)]` accepts `pair v1 (pair v2 unit_witness)` iff `Nat v1 = TT` and `Bool v2 = TT`.
- Dependent: `Record [("A", Type), ("x", {A} -> A)]` accepts `pair Nat (pair 0 unit_witness)` (because `0 : Nat`).

## 5. Projection

### 5.1 `ValidField` refinement

The key construct: a refinement type that accepts exactly the strings that name a field in some `fields` list.

```disp
// "Is name n in field list fields?"
in_fields := fix({self, fields, n} ->
  match (list_is_nil fields) {
    TT => FF
    FF => match (tree_eq (pair_fst (list_head fields)) n) {
      TT => TT
      FF => self (list_tail fields) n
    }
  })

// Refinement type: "n is a valid field name in fields"
ValidField := {fields} -> Refinement String ({n} -> in_fields fields n)
```

`ValidField [("x", Nat), ("y", Bool)]` is inhabited by exactly `"x"` and `"y"` (the canonical String trees for those identifiers) — and nothing else.

### 5.2 The projector

```disp
// Walk the Sigma chain to a given index, returning the value at that position:
sigma_walk := fix({self, r, i} ->
  match (is_zero i) {
    TT => pair_fst r
    FF => self (pair_snd r) (pred i)
  })

// Find the index of a name in a field list (caller guarantees presence):
find_index := fix({self, fields, n} ->
  match (tree_eq (pair_fst (list_head fields)) n) {
    TT => zero
    FF => succ (self (list_tail fields) n)
  })

// The tree-native projector:
//   proj : {fields} -> Record fields -> (n : ValidField fields) -> field_type
//
// The Refinement on `n` is what enforces validity at typecheck time.
proj := {fields, r, n} ->
  sigma_walk r (find_index fields n)
```

### 5.3 Compile-time error mechanism

When you call `proj r "bogus"` and `"bogus"` isn't in `r`'s fields, the kernel:

1. Checks the call's type. The argument's expected type is `ValidField fields`.
2. `ValidField fields = Refinement String ({n} -> in_fields fields n)`.
3. Refinement's recognizer evaluates `(String "bogus") ∧ (in_fields fields "bogus")`.
4. `in_fields fields "bogus"` reduces by walking `fields` and comparing with `tree_eq`. None match. Result: `FF`.
5. The conjunction returns `FF`. The Refinement's recognizer returns `FF`.
6. The kernel concludes `"bogus"` is not of type `ValidField fields`. **Type-check fails.**

The error is structurally identical to what happens when you write `(succ zero) : Bool`: a predicate evaluates to `FF` and the kernel rejects. No bespoke error path; no elaborator special case.

### 5.4 Speed: elaborator inlining

The slow path `proj r n` walks the chain in O(k) reductions for the k-th field. Hash-cons memo makes repeated access O(1) after the first call, so closed records are effectively free.

For *open* records (where `r` is a function parameter), reduction stalls on `r` and we'd pay O(k) per call site. Elaborator inlining recovers O(1):

```
proj r "x"
  // Elaborator sees: literal name "x", knows r's type, so knows fields
  // Computes: find_index fields "x" = 0  (at parse time)
  // Rewrites to: sigma_walk r 0
  // Further reduces: pair_fst r
```

The inlined form is provably equal to the slow path by reduction, so substituting is sound. The kernel typechecks the *original* `proj r "x"` form; the inlined form is the compiled result.

If the name is dynamic (`proj r computed_name`), inlining can't fire; the slow path runs. Both produce identical results.

This is the "speed and name-usability" payoff: kernel-validated correctness with no per-access cost in the common literal-name case.

## 6. Surface syntax

### 6.1 Construction

```
{ x := e1, y := e2 }
```

**Desugars to:** `pair (eval e1) (pair (eval e2) unit_witness)`, after the parser canonicalizes the field order alphabetically.

If both `x` and `y` are explicitly typed via context (`let r : Record [("x", Nat), ("y", Bool)] = …`), the type's recognizer validates the value at typecheck time.

If no type annotation is present, the type is inferred:
- For each field, infer the value's type.
- Build the FieldList from declared names + inferred types.
- The result is `Record fields`.

### 6.2 Type form

```
{ x : T1, y : T2 }
```

**Desugars to:** `Record [("x", T1), ("y", T2)]` after canonicalization. (For dependent records, the type expressions may reference earlier-named fields; see §7.)

### 6.3 Projection

```
r.x
```

**Desugars to:** `proj r "x"`.

The parser interns `"x"` as a canonical String tree (Scott byte list). The kernel typechecks the call, validating `"x" : ValidField (fields_of r)`. If the check passes, the elaborator inlining pass (§5.4) tries to reduce to a direct projection chain; otherwise the slow path runs at evaluation.

### 6.4 Examples

**Non-dependent record:**

```disp
let pt := { x := 1, y := 2 }
// type: Record [("x", Nat), ("y", Nat)]
// value: pair 1 (pair 2 unit_witness)

test pt.x = 1
test pt.y = 2
// pt.z would fail typecheck — Refinement rejects "z"
```

**Dependent record:**

```disp
let pair_with_type := { T := Nat, val := 0 }
// Source-level type: Record [("T", Type), ("val", T)]
// Parser-emitted FieldList:
//   [("T", {_prev} -> Type),
//    ("val", {prev} -> pair_fst prev)]    -- T is at tuple position 0
// Sigma-chain form:
//   Sigma Type ({T_val} -> Sigma T_val ({_val_val} -> Unit))
// Value: pair Nat (pair 0 unit_witness)
// Typecheck:
//   - first slot: Nat satisfies Type
//   - second slot: 0 satisfies (the function applied to Nat = Nat)

test pair_with_type.T = Nat
test pair_with_type.val = 0
```

**Kernel metadata, rewritten:**

```disp
// Today (lib/types/pi.disp):
//   Pi_pf := predicate_frame ({params, v} -> ...) ({ks, raw, meta, v} -> ...)
//   metadata is bare pairs, accessed via pair_fst/pair_snd chains
//
// After Records land:
let pi_meta := { recognizer := pi_recognizer, dom := A, cod_fn := B }
// .recognizer, .dom, .cod_fn are typed projections, validated by ValidField.
```

## 7. Dependent records

Dependence is automatic because `sigma_chain` builds an iterated `Sigma`, and `Sigma A ({a} -> B a)` is dependent by construction. Each entry in the FieldList has a `type_fn` that takes a tuple of all preceding values, so any field's type can reference any earlier field.

**Encoding convention.** Each `type_fn` is a single-argument function whose argument is the tuple `(v_{i-1}, (v_{i-2}, (..., unit_witness)))` of all preceding field values (most recent first). The parser translates source-level identifier references into the corresponding tuple-accessor expressions:

```
Source:        { A : Type, x : A, y : List A }

Parser emits FieldList:
  [ ("A", {_prev} -> Type)
  , ("x", {prev} -> pair_fst prev)                              // A is at position 0
  , ("y", {prev} -> List (pair_fst (pair_snd prev)))            // A is at position 1
  ]

sigma_chain_top produces:
  Sigma Type ({A_val} ->
    Sigma A_val ({_x_val} ->
      Sigma (List A_val) ({_y_val} ->
        Unit)))
```

A value `pair Nat (pair 0 (pair nil unit_witness))` typechecks because:
- First slot: `Nat` satisfies `Type`.
- Second slot: applying `({A_val} -> Sigma A_val ...)` to `Nat` gives `Sigma Nat ...`, and `0` satisfies `Nat`.
- Third slot: continues similarly.

**Parser responsibility.** The parser must:
1. Parse field-type expressions in a scope where preceding field names are bound.
2. Translate each identifier reference (e.g., `A`) into the appropriate tuple-accessor (`pair_fst prev`, `pair_fst (pair_snd prev)`, …).
3. Emit FieldList entries whose `type_fn` is the resulting closure.

This is a mechanical desugaring — no inference required. Identifier-to-tuple-position is determined entirely by the position-from-end of the referenced field.

**Non-dependent records** are the trivial case where every `type_fn` ignores `prev`: the parser emits `{_prev} -> SomeType` for each field. No source-level identifier references means no tuple-accessor translation.

## 8. Coherence laws

These should be provable in-language once Records land. Adding them to the test suite locks the semantics:

```disp
// (1) Projection of constructed record returns the right value
test ({ x := 1, y := 2 }).x = 1
test ({ x := 1, y := 2 }).y = 2

// (2) Two records with the same fields in the same order are tree_eq
test tree_eq { x := 1, y := 2 } { x := 1, y := 2 } = TT

// (3) Field order is canonicalized — different source orders give same value
test tree_eq { y := 2, x := 1 } { x := 1, y := 2 } = TT

// (4) Different field names → different types (even with same value shape)
test tree_eq (Record [("x", Nat)]) (Record [("a", Nat)]) = FF

// (5) Extensionality: records with same projections at every field are equal
//     (proven via the recognizer's structural recursion)
```

The extensionality property is what justifies the cubical-fast encoding optimization eventually: any program that observes only field projections is invariant under records that agree on every field.

## 9. Enums (sketch)

The dual construction. An enum is a tagged union, where the tag is a refinement-singleton name. The encoding mirrors records:

```disp
// VariantList = List (Pair String Type)
//   [("Extend", Tree), ("Return", Tree)]

// Either A B : Type — disjoint sum, needs to land alongside Refinement
Either_pf := predicate_frame
  ({params, v} ->
    let A = pair_fst params
    let B = pair_snd params
    or (and (is_inl v) (A (un_inl v)))
       (and (is_inr v) (B (un_inr v))))
  t

Either := {A, B} -> wait Either_pf (pair A B)

// Enum encoding:
//   data Action = Extend(Tree) | Return(Tree)
//     ⟶  Either (Sigma (Name "Extend") ({_} -> Tree))
//               (Sigma (Name "Return") ({_} -> Tree))
//
// where Name s := Refinement String ({x} -> tree_eq x s)
//
// Constructor:
//   Extend(t) ⟶ inl ("Extend", t)
//
// Match:
//   match a { Extend(x) => f x ; Return(y) => g y }
//     ⟶  either ({et} -> let x = pair_snd et in f x)
//                ({rt} -> let y = pair_snd rt in g y)
//                a
```

For more than two variants, iterated `Either`. Parser canonicalizes variant order (alphabetical) for type stability.

Exhaustiveness checking: the parser inspects the match arms against the enum's variant list (accessible via `params` on the Enum type), and emits a compile error if any variant is missing.

This is a sketch; full enum design is a follow-up doc once records land.

## 10. Migration plan

### 10.1 Landing order

1. **Refinement** (`lib/types/refinement.disp` + tests) — unblocking. ~50 lines including tests.
2. **Unit** (`lib/types/unit.disp` + tests) — ~30 lines.
3. **String** formalized as a library type if not already — clarify the existing convention, add a recognizer. ~50 lines.
4. **Record** (`lib/types/record.disp` + tests) — depends on Sigma (already landed via W1), Refinement, Unit. ~80 lines.
5. **ValidField + proj** (in `lib/types/record.disp` or `lib/std/record_ops.disp`) — ~40 lines.
6. **Parser desugaring** updates to `src/parse.ts` and `src/compile.ts`:
   - **Intern identifiers as canonical Scott byte lists.** New parser pass: each source identifier (in `r.x` projections, in `:=` field labels, in `:` field-type labels) gets a deterministic Scott-encoded byte-list tree via an atom table. Same identifier → same tree, across sessions. This is the load-bearing prerequisite for `Name s` refinements to work.
   - `r.x` → `proj r "x"` (replaces current `proj` case in compile.ts:413).
   - `{ x := e }` → nested pair (replaces Church encoding from compile.ts:402–407).
   - `{ x : T }` → `Record [...]` (replaces current recType compile-time metadata).
   - Canonicalize field order alphabetically at parse time.
   - For dependent records: when parsing each field-type expression, track the predecessor field names in scope and translate identifier references to tuple-accessor functions (per §7).
7. **Elaborator inlining pass** (optional, follows after correctness) — detects literal-name `proj` calls and rewrites to direct chains.
8. **Spec updates to TYPE_THEORY.typ**:
   - §5.X: Refinement (move from §5.8 sketch to its own section).
   - §5.Y: Unit.
   - §5.Z: Record (the predicate_frame wrapper + the projection refinement pattern).
   - Update §"Compilation" to describe the new desugaring rules.
9. **Migrate kernel metadata** to use Record syntax (`pi_meta.cod_fn` instead of `pair_snd (pair_snd ...)`), retiring hand-written `*_meta_*` accessors. Touches every `lib/types/*.disp` and `lib/kernel/handlers.disp`.
10. **Migrate file bodies** (`use "..."` imports) to return Record values rather than Church-encoded records. Coordinate with all `use` consumers.

Steps 1–6 are the core implementation. 7 is performance. 8–10 are downstream consolidation that can happen incrementally.

### 10.2 Backward compatibility

Steps 1–5 are purely additive (new library types, no kernel changes). Step 6 changes the underlying *tree encoding* of existing surface syntax — the syntax itself is unchanged.

**What stays the same (syntax):**
- `use "file.disp"` is still a valid expression. It still loads a file and yields a value of its exports.
- `open use "file.disp"` is unchanged. Field names are still brought into scope.
- `{x := e}` and `r.x` are still the construction / projection forms.

**What changes (encoding):**
- `use "file.disp"` previously evaluated to a Church-encoded record `λA. λk. k field1 field2 …`. After migration, it evaluates to a Record-typed value `pair field1 (pair field2 unit_witness)`.
- `{x := e}` previously compiled to `λsel. sel e` (Church encoding). After migration, it compiles to `pair e unit_witness`.
- `r.x` previously compiled to a Church-record selector application. After migration, it compiles to `proj r "x"` (or its inlined `pair_fst (pair_snd …)` form).

**Why "hard cutover" is safe:**

The encoding change is only observable to code that inspects the underlying tree shape directly. Audit for the two patterns that could break:

```
grep -rn "= use \""  lib/ src/        → 0 hits
grep -rn "let.*= use "  lib/ src/      → 0 hits
grep -rn "{}"  in .disp source         → 0 hits
```

Specifically:
- **No code applies a `use` result as a Church record.** Hypothetical pattern: `(use "f.disp") SomeType (λa b → a + b)` — applying the result as a function. Not found anywhere.
- **No code binds `use` to a name for later projection.** `let mod = use "x.disp"` — not found. (All uses are `open use`, which is encoding-invariant.)
- **No `.disp` file uses `{}` in expression position.** The Church-unit identity encoding of the empty record is dead code.

So "hard cutover" means: change the desugaring once, delete the old code path, no compatibility shim. The alternative — side-by-side mode (e.g., `use!` for new, plain `use` for old) — would add complexity for no observable benefit since the audit showed nobody relies on the old encoding.

**What changes in the codebase:**
- `lib/tests/**/*.test.disp` and `lib/**/*.disp` using `{x := e}` or `r.x` get the new desugaring transparently. Surface syntax is identical, so source files don't need editing.
- `src/compile.ts`'s `recValue` and `proj` cases are rewritten; the bespoke `resolveExprRecord` / `fieldInnerFields` infrastructure is deleted.
- Add a few regression tests covering the new desugaring before deleting the old code path.

### 10.3 Spec updates

Each prerequisite type gets a §5.X entry in TYPE_THEORY.typ describing its recognizer (one-line code) and laws (a few sentences). Record gets a longer entry explaining the Sigma-chain desugaring rule and the ValidField refinement pattern. The "elaborator handles record shape tracking" note in COMPILATION.typ becomes "elaborator desugars syntax to library types," and the projection-mismatch error row in the error table moves from "elab" to "kernel: Refinement predicate FF on field name."

## 11. Decisions (formerly open questions)

All design points have a resolution. Listed here for the implementation agent and for future readers who want the rationale.

1. **`unit_witness` tree: `pair t t`.** Matches the "no-params" sentinel already used in `lib/types/false.disp:30`. Does not collide with Bool (TT is `K K = pair (pair t t) (pair t t)`, not `K` alone). Established disp convention.

2. **String encoding: Scott byte list.** Stable, reflective, simple to spec. Performance is fine because hash-cons makes equality O(1) regardless of size. Alternatives (Church list, boot-set atoms, bit-pair tree) offered no compelling advantage for the cost of being non-standard.

3. **Atom-table interning: pure deterministic.** Every disp session produces the same canonical String tree for a given identifier. Cost is one Scott-list construction per unique identifier, paid once and cached. Stability across sessions enables hash-cons sharing in separate compilation and makes spec comparison reproducible.

4. **Empty record `{}`: hard cutover.** Audit (`grep -rn '{}'` in `.disp` source) finds zero current uses of `{}`. The polymorphic-identity Church-unit encoding is dead code. After migration, `{}` desugars to `unit_witness`.

5. **`use "file.disp"` semantics: encoding changes, syntax doesn't.** `use` remains a valid expression that loads a file and yields its exports. The underlying tree changes from Church-encoded record to Record-typed value (nested Sigma). Audit finds zero current uses that depend on the underlying encoding (zero `= use "..."` bindings, zero `let foo = use "..."` patterns) — every existing `use` is `open use`, which is encoding-invariant. No compatibility shim needed.

6. **Inlining pass correctness: by reduction.** When the inlining pass rewrites `proj r "x"` to a direct projection chain, the result must be `tree_eq` to the original after kernel reduction. Add a regression test that compiles both forms and compares hash-cons identity. Not a design question — a test obligation.

7. **Dependent record scoping rule.** Field-type expressions are parsed in a scope where all preceding field names are bound. Mirrors `let`-binding shadowing. `{ A : Type, x : A }` parses with `A` in scope for `x`'s type expression. This is the only rule that makes sense for dependent records.

8. **Enum design: deferred.** §9 sketches the construction. The full design (encoding choice, exhaustiveness implementation, multi-arity variants) lives in a separate `ENUMS_PROPOSAL.md` to be drafted once records land.

## 12. What this proposal does NOT cover

- **Record subtyping / row polymorphism.** Two records with overlapping fields are not interchangeable; subtyping coercions are not provided. Out of scope.
- **Mutable records / update operators.** A `record { r | x := new_val }` update form would require additional library functions. Sketch only.
- **Record types as kernel types.** Records are library types, not kernel primitives. No new kernel handler. The kernel's seven primitives don't grow.
- **First-class field names.** Names are tree values, but the parser still translates source identifiers; users can't write `"x"` directly in a projection (only via `r.x`). A separate `proj_dyn r expr` for computed names is a follow-up.

## Summary

After this proposal lands, the entire record story in disp is:

- A record is a thin `Record fields` wrapper over an iterated `Sigma` chain.
- Projection `r.x` desugars to `proj r "x"` where `proj`'s name parameter has a `ValidField fields` refinement type.
- Compile-time errors for unknown fields come from the Refinement's predicate evaluating to `FF` — the kernel rejects, identical to any other type mismatch.
- Speed is recovered by an elaborator inlining pass that reduces literal-name projections to direct chains.
- Dependent records work for free because iterated Sigma handles dependence.
- The current bespoke `resolveExprRecord` / `fieldInnerFields` machinery in `src/compile.ts` is replaced by a small desugaring layer.

Records become library code. The kernel stays the same. The elaborator shrinks. Errors come from tree calculus. That is the elegant landing zone we've been working toward.
