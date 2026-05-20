# Coproduct types and unified pattern matching

A proposal for an iteration of `TYPE_THEORY_NEXT.typ` adding categorical-coproduct sum types with a unified `match` surface syntax. Builds on the framework already established in `TYPE_THEORY_NEXT.typ` (manifest contracts, TypeFormer records, kernel primitives). The kernel doesn't change; this is library + elaborator work.

## Status

- **Position**: proposed iteration on `TYPE_THEORY_NEXT.typ`. Adds one or two new sections (likely a new §14 between Cubical and Soundness). Reformulates `Bool`, `Nat`, `Eq`, `Ord` in §12 to use the new mechanism.
- **Kernel impact**: zero. All work is in the library and elaborator.
- **Dependency on other proposals**: requires records (`RECORDS_PROPOSAL.md`) to land first, since the per-case dispatch table is a record.
- **Status flagged as**: design proposal, not landed.

## Motivation

Today's disp has scattered eliminator mechanisms:

| Mechanism | Used for |
|---|---|
| `select`, `select_lazy` | Bool dispatch |
| `bool_rec`, `nat_rec`, `ord_rec`, `eq_J` | Per-type eliminators |
| `triage` | Raw tree decomposition |
| `match { TT => ...; FF => ... }` | Binary on TT/FF only |

Each is a special case of "dispatch on a value's constructor." There's no unified surface for users to pattern-match on user-defined sum types, no way to declare a new sum type with constructors and get matching for free.

A **Coproduct** library type would unify these. Categorically, coproducts are sum types (the dual of records as products):

- **Record (Product)**: a value contains ALL of these fields; eliminator picks one via projection.
- **Coproduct (Sum)**: a value is ONE of these tagged variants; eliminator picks the matching case via dispatch.

In a language with records (`RECORDS_PROPOSAL.md`), coproducts are the natural counterpart. Once they exist, every inductive type — `Bool`, `Nat`, `Maybe`, `List`, `Either`, and user-defined types — is uniformly a coproduct, and `match` desugars uniformly to the coproduct's eliminator.

## The design

### Coproduct as a TypeFormer

A coproduct is parameterized by a list of constructors. Each constructor is a `(name, arg_type)` pair: the name is the constructor's identifier (interned by the parser), the arg_type is the type of the payload (use `Unit` for nullary constructors).

```disp
Constructor := { name : String, arg_type : Type }

Coproduct := make_type_former
  (List Constructor)                // params
  coproduct_recognizer
  none                               // not applicable (data, not function)
  coproduct_functor                  // per-constructor recursive transport
  refl_identity_law
  refl_composition_law
```

The recognizer for `Coproduct ctors` on a candidate `v`:
1. `v` must be a fork `pair tag arg`.
2. `tag` must hash-cons-equal one of the constructor names in `ctors`.
3. `arg` must inhabit the corresponding constructor's `arg_type`.

```disp
let coproduct_recognizer = {ctors, v} -> {
  match (is_fork v) {
    FF => FF
    TT => {
      let tag = pair_fst v
      let arg = pair_snd v
      // Look up the constructor with this tag; return its arg_type.
      let ctor_opt = find_ctor_by_name ctors tag
      match (is_some ctor_opt) {
        FF => FF                                   // unknown constructor
        TT => {
          let arg_type = ctor_arg_type (unwrap ctor_opt)
          // Check arg inhabits arg_type.
          param_apply arg_type arg
        }
      }
    }
  }
}
```

Hash-cons identity makes the tag comparison O(1). Constructor lookup is O(N) on the constructor list (which is typically short — Bool has 2, Nat has 2, most user types have 2–8).

### Constructor values and the `ctor` smart constructor

A constructor application produces a tagged-pair value. The smart `ctor` constructor builds these correctly:

```disp
// ctor T name arg: build a value of coproduct type T with constructor
// `name` and payload `arg`. Validates that `name` is one of T's
// declared constructors and `arg` inhabits its arg_type.
ctor := {T, name, arg} -> {
  let name_tree = intern_name name
  let ctors     = coproduct_ctors T
  let ctor_opt  = find_ctor_by_name ctors name_tree
  match (is_some ctor_opt) {
    FF => Fail   // not a valid constructor for T
    TT => {
      let expected_arg_type = ctor_arg_type (unwrap ctor_opt)
      // Validate arg against arg_type.
      must_ok_tt (param_apply expected_arg_type arg) ({_} ->
        Ok (pair name_tree arg))
    }
  }
}

// Standard constructors:
TT   := ctor Bool "TT" unit_witness         // produces (pair "TT" unit)
FF   := ctor Bool "FF" unit_witness
zero := ctor Nat "zero" unit_witness
succ := {n} -> ctor Nat "succ" n
some := {A, x} -> ctor (Maybe A) "some" x
none := {A} -> ctor (Maybe A) "none" unit_witness
```

The smart `ctor` returns `Result`; for closed constructors the result reduces to `Ok value` at compile time, so callers get a bare value.

### The generic eliminator: `coproduct_rec`

The eliminator dispatches on a value's tag and runs the corresponding case-handler. Cases are bundled in a record indexed by constructor name.

```disp
// coproduct_rec : given a target value of coproduct type T, a motive
// (T -> Type) giving the result type per target, and a case record
// (one handler per constructor), apply the appropriate handler.
coproduct_rec := {T, motive, cases, target} -> {
  let tag = pair_fst target
  let arg = pair_snd target
  // Project the field of `cases` matching `tag`.
  let handler = lookup_field_by_name cases tag
  // Each handler has type `arg_type_i -> motive (ctor i arg)`.
  handler arg
}
```

For hypothesis-typed targets, the eliminator must stuck-mint (since the dispatch can't proceed). This is exactly what the kernel's `eliminator_frame` primitive (§6.3 of TYPE_THEORY_NEXT) does:

```disp
coproduct_rec_safe := {T, motive, cases, target} ->
  match (is_neutral target) {
    TT => StuckElim (motive target) target
    FF => coproduct_rec T motive cases target
  }
```

This wrapping is done via `eliminator_frame_form` so the kernel's eliminator_frame handler routes correctly under the walker. The `make_coproduct` smart constructor handles this wrapping automatically.

### `lookup_field_by_name`

The dispatcher's heart is record-field-projection by name. Under the records proposal (`RECORDS_PROPOSAL.md`), records have a `FieldList` with `(name, type_fn)` entries; projection by name walks this list and extracts the value at the matching position.

```disp
lookup_field_by_name := {record, name} -> {
  let chain  = record I_canonical    // extract the chain (Option B wrapping)
  let fields = record_fields (type_of record)
  let idx    = find_index fields name
  sigma_walk chain idx
}
```

Hash-cons identity on `name` makes the lookup O(N) on field count, with O(1) tag comparisons per step. For typical coproducts (2–8 constructors), this is fast.

## `match` as desugaring

The elaborator desugars surface `match e { c1 args1 => body1 ; c2 args2 => body2 ; ... }` to a `coproduct_rec` call.

### Algorithm

For `match e { c1 p1 => body1 ; c2 p2 => body2 ; ... }`:

1. **Resolve `e`'s type T** from scope. Must be a Coproduct (or have a Coproduct field in its TypeFormer); else compile error.

2. **Read T's constructors** via `coproduct_ctors T`. This is a list of `(name, arg_type)` pairs.

3. **Check exhaustiveness**: every constructor in T's list must have exactly one match arm. Missing or duplicate arms are compile-time errors.

4. **Check arm order**: arms must appear in the constructor declaration order. (This is the simplest rule; allowing any order is a future relaxation.)

5. **Infer the motive**:
   - Non-dependent case: motive is `{_} -> T_result` where T_result is the expected type at the match expression's position.
   - Dependent case: user provides via `match e returning (motive_expr) { ... }`.

6. **Compile each arm**: for arm `c_i p_i => body_i`, the pattern `p_i` binds the constructor's argument(s). Compile `body_i` with the binding, checking against `motive (ctor i p_i)`.

7. **Build the cases record**:
   ```disp
   { c1 := ({p1} -> body1) ; c2 := ({p2} -> body2) ; ... }
   ```
   Each field is a function from the constructor's argument type to the motive at that point.

8. **Emit** `coproduct_rec T motive cases_record e`.

### Example: Bool

```disp
// Source:
let result : Nat = match b {
  TT => zero
  FF => succ zero
}

// Desugars to:
let result : Nat = coproduct_rec
  Bool
  ({_} -> Nat)                                // non-dependent motive
  { TT := ({_} -> zero); FF := ({_} -> succ zero) }
  b
```

### Example: Maybe

```disp
// Source:
let n : Nat = match maybe_n {
  none      => zero
  some x    => x
}

// Desugars to:
let n : Nat = coproduct_rec
  (Maybe Nat)
  ({_} -> Nat)
  { none := ({_} -> zero); some := ({x} -> x) }
  maybe_n
```

### Example: dependent

```disp
// Source:
let result : (b : Bool) -> if_then_else b Nat Bool = {b} ->
  match b returning ({b} -> if_then_else b Nat Bool) {
    TT => zero          // expected type Nat (when b = TT)
    FF => TT            // expected type Bool (when b = FF)
  }
```

The `returning` clause provides the motive. Each arm's body checks against the motive at the corresponding constructor value.

## Recursive coproducts

For Nat, List, and other inductive types, constructors reference the type being defined:

```disp
// Naïve:
Nat := Coproduct [
  { name := "zero", arg_type := Unit },
  { name := "succ", arg_type := Nat }     // ← Nat refers to itself
]
```

This is the standard inductive-type recursion. Resolved via `fix` at the type level:

```disp
Nat := fix ({Self} -> Coproduct [
  { name := "zero", arg_type := Unit },
  { name := "succ", arg_type := Self }
])
```

`List`:
```disp
List := {A} -> fix ({Self} -> Coproduct [
  { name := "nil",  arg_type := Unit },
  { name := "cons", arg_type := Pair A Self }
])
```

### Positivity

For the fix-point to be well-defined, the recursive references to `Self` must occur in **strictly positive positions** within constructor arg-types (i.e., `Self` doesn't appear on the left of a function arrow within an arg_type). Otherwise the fix-point can encode contradictions.

Two options for handling this:

**Option A — explicit positivity check.** A library function `is_strictly_positive Self ctors` validates the constructor list. `make_coproduct` runs this check; non-positive coproducts fail to construct. Standard Lean/Coq approach.

**Option B — divergence-as-rejection.** Don't check positivity statically; let ill-founded recursion diverge at use time. Matches disp's existing approach to Type-in-Type (§11.4 of TYPE_THEORY_NEXT).

**Recommendation**: Option A. The positivity check is a library function, runs at coproduct construction, and gives clear errors. Disp's "divergence as rejection" works for Russell-paradox-style attacks at the universe level, but for typed inductive types, explicit positivity is the standard discipline and gives better diagnostics.

The check itself is straightforward: walk each constructor's arg_type; reject if `Self` appears on the left of any function arrow. Implement as a library function and call it from `make_coproduct`.

## Dependent matching

The basic match handles non-dependent cases — every arm's result has the same type. For dependent cases (where each arm's type varies based on the constructor), the elaborator needs the motive explicitly.

### The `returning` clause

```disp
match target returning motive {
  c1 args1 => body1
  c2 args2 => body2
  ...
}
```

The `returning` clause provides the motive as an explicit Pi-typed function from target values to types. Each body is type-checked against `motive (ctor i args_i)`.

Surface syntax extension to add to parser:
- After `match expr`, optionally `returning motive_expr`.
- If absent, motive is `{_} -> expected_type_from_context`.

### Example: dependent Eq elimination

```disp
let cong : Pi A ({A} ->
           Pi B ({B} ->
           Pi (A -> B) ({f} ->
           Pi A ({x} ->
           Pi A ({y} ->
           Pi (Eq A x y) ({p} ->
           Eq B (f x) (f y)))))))
= {A, B, f, x, y, p} ->
  match p returning ({y} -> Eq B (f x) (f y)) {
    refl => refl
  }
```

The motive `{y} -> Eq B (f x) (f y)` captures the dependence on the equality's right-hand side. When matching `refl`, `y` is unified with `x`, so the body needs `Eq B (f x) (f x)`, which is `refl`.

This is exactly Coq/Lean's dependent `match ... in ... return ...`. Disp inherits the discipline.

## What this unifies

Once Coproduct + match land, several mechanisms collapse:

| Today | After Coproduct |
|---|---|
| `select then else cond` | `match cond { TT => then; FF => else }` (desugars to coproduct_rec) |
| `select_lazy thunk_then thunk_else cond` | Same, with the elaborator emitting thunks on demand |
| `bool_rec motive ct cf b` | `match b { TT => ct; FF => cf }` (with motive inferred or annotated) |
| `nat_rec motive base step n` | `match n { zero => base; succ m => step m }` |
| `ord_rec ...` | `match o { zero_ord => ...; omega_plus ... => ... }` |
| `eq_J motive refl_case eq_proof` | `match eq_proof { refl => refl_case }` |
| `triage on_leaf on_stem on_fork tree` | Stays — for raw tree decomposition, not typed dispatch |

Library types `Bool`, `Nat`, `Eq`, `Ord` are reformulated to use `Coproduct` internally. Their existing eliminators (`bool_rec`, etc.) become aliases for `coproduct_rec` at specific coproducts, kept for backward compatibility.

## Open questions

### Order of constructors in match arms

The simplest rule is **declaration order**: match arms must appear in the same order as the coproduct's constructor list. This avoids ambiguity in the cases-record construction (hash-cons matters).

Allowing any order is feasible but requires extra elaborator work to canonicalize. Probably not worth it for v1.

### Nested patterns

`match (b1, b2) { (TT, TT) => x ; (TT, FF) => y ; ... }`. Standard compilation: nested case-splits. Adds elaborator complexity. **Recommendation**: defer to a future extension. v1 supports only top-level constructor patterns; nested patterns desugar to nested `match` expressions in user code.

### Wildcards

`match e { TT => x ; _ => y }`. The wildcard catches all remaining constructors. Adds elaborator work (expand wildcards into specific case branches). **Recommendation**: defer. Users write out cases explicitly in v1.

### Pattern guards

`match e { TT when foo => x ; ... }`. Substantial elaborator complexity. **Recommendation**: punt.

### Exhaustiveness as hard error vs warning

Match arms must cover all constructors. **Recommendation**: hard error at compile time. Non-exhaustive matches are almost always bugs; turning them into runtime stuck values just delays the error.

### Constructor name collisions

If two coproducts have constructors with the same name (e.g., both `Maybe` and `Option` have `some`), how does disambiguation work in match arms?

Two options:
- **Qualified names**: `match e { Maybe.some x => ... ; Maybe.none => ... }`. Verbose but unambiguous.
- **Inferred from target type**: the elaborator already knows `e`'s type; resolve the constructor name in that type's scope. Concise but requires extra elaborator work.

**Recommendation**: inferred from target type (option 2). The elaborator has the type info needed.

### Polymorphic constructors

For `Maybe : Type -> Type`, `some : Pi Type ({A} -> A -> Maybe A)`. The `some` constructor takes the type parameter `A` plus the value. In `match e { some x => ... }` over `e : Maybe Nat`, the `x` is bound at type `Nat` (the type parameter is implicit/inferred).

**Recommendation**: type parameters to coproducts are explicit at type construction (`Maybe Nat`), implicit at constructor use (the parser/elaborator infers from context). Standard ML/Haskell discipline.

### Recursive constructor signatures

For `List`, the `cons` constructor takes `(A, List A)`. Pattern: `match xs { cons (h, t) => ... }` binds `h : A` and `t : List A`.

The "pair" of arguments is a Sigma (or Pair) — i.e., `cons` has arg_type `Pair A (List A)`, and matching destructures the pair.

**Recommendation**: keep this explicit. `cons (h, t)` desugars to "bind `cons`'s arg to a fresh variable, destructure it as a pair." If pair-destructuring isn't itself supported, users write `match xs { cons p => let h = fst p in let t = snd p in ... }`.

For pair-destructuring to work in patterns, we'd need pair patterns supported in match arms — another extension.

### Empty coproducts

`Coproduct []` (no constructors) — the empty type, sometimes called `Void` or `Empty`. It has no inhabitants. `match e { }` (with `e : Coproduct []`) is vacuously well-typed: there are no arms to check, the motive is whatever, and the body never runs (because no value of type `Coproduct []` exists).

**Recommendation**: support this. `Empty := Coproduct []`. Useful for proof-by-contradiction (`Eq A x y -> Empty` for distinguishable values).

## Implementation roadmap

### Phase 1 — Coproduct library type

- `lib/types/coproduct.disp` — `Coproduct` type-former, recognizer, smart constructor.
- `lib/std/coproduct.disp` — `ctor`, `coproduct_rec`, `lookup_field_by_name`.
- Tests in `lib/tests/coproduct.test.disp`.

Dependencies: requires records (`RECORDS_PROPOSAL.md` landed) for the cases-record.

Estimated effort: 1–2 weeks. ~300 lines of disp, ~50 tests.

### Phase 2 — Reformulate existing inductive types

Bool, Nat, Eq, Ord, Unit, False all defined via Coproduct:

```disp
Bool := Coproduct [
  { name := "TT", arg_type := Unit },
  { name := "FF", arg_type := Unit }
]

Nat := fix ({Self} -> Coproduct [
  { name := "zero", arg_type := Unit },
  { name := "succ", arg_type := Self }
])
```

Keep `bool_rec`, `nat_rec`, etc. as aliases for backward compatibility. Existing tests should pass unchanged.

Estimated effort: 1 week. Disrupts a lot but the new code is cleaner.

### Phase 3 — Surface `match` syntax

Extend the parser to support general match arms with constructor patterns. Extend the elaborator to:
1. Look up the target's type's Coproduct.
2. Check exhaustiveness and ordering.
3. Build the cases record.
4. Emit `coproduct_rec` call.

Estimated effort: 1 week. Mostly in `src/parse.ts` and `src/compile.ts`.

### Phase 4 — User-defined sum types

Allow user code to declare coproducts:

```disp
data Color = Red | Green | Blue
// Sugar for:
Color := Coproduct [
  { name := "Red",   arg_type := Unit },
  { name := "Green", arg_type := Unit },
  { name := "Blue",  arg_type := Unit }
]
```

This requires `data` (or `enum`, `coproduct`) as a parser keyword and elaborator support for sugar.

Estimated effort: 1 week, layered on Phase 3.

### Phase 5 — Extensions (optional)

- Positivity check (Option A above).
- Nested patterns.
- Wildcards.
- Pattern guards.

Each is its own substantial work item.

## How this fits in TYPE_THEORY_NEXT.typ

Once landed, this proposal adds a new section to TYPE_THEORY_NEXT.typ. Probable structure:

**New §14 — Coproduct types and unified pattern matching** (between current §13 Cubical and §14 Soundness; existing §14+ renumber):

- §14.1 Motivation: unify scattered eliminators.
- §14.2 The Coproduct library type as a TypeFormer.
- §14.3 Constructor values and the smart `ctor` constructor.
- §14.4 The generic eliminator `coproduct_rec`.
- §14.5 `match` as desugaring to coproduct_rec.
- §14.6 Recursive coproducts and positivity.
- §14.7 Dependent matching via `returning`.
- §14.8 Open questions and extensions.

Existing §12 (Library types) gets reformulated: `Bool`, `Nat` defined via `Coproduct`. The §6.3 eliminator_frame primitive's description gets updated to note that coproduct_rec is its primary user.

The §14 Soundness theorem (renumber) gets a clause covering coproduct types: a value `v` passes `typecheck (Coproduct ctors) v` iff `v` is `pair tag arg` where `tag` is one of `ctors`' names and `arg` inhabits the corresponding arg_type.

## Connection to records (categorical duality)

Records and Coproducts are categorical duals:

- **Record (Product)** `{x : A, y : B}` — values contain BOTH x and y. Eliminator: projection (`.x`, `.y`).
- **Coproduct (Sum)** `Coproduct [{name: "x", arg: A}, {name: "y", arg: B}]` — values contain EITHER x or y. Eliminator: case dispatch (`match`).

In disp, both can use the same FieldList machinery:
- Records: FieldList = `[(name, type)]` — fields with names and types.
- Coproducts: FieldList = `[(name, arg_type)]` — constructors with names and arg types.

The case-record passed to `coproduct_rec` is a record indexed by the coproduct's constructor names. So coproduct elimination is "build a record of handlers, indexed by the same names as the coproduct's constructors."

This duality is **categorically standard**. A coproduct A + B has eliminator `(A → C) × (B → C) → (A + B → C)`. The case-record IS the pair of handlers, generalized to N constructors.

Surface symmetry:

```disp
// Record construction:
let r = { x := 5 ; y := 3 }
// Record elimination:
let n = r.x

// Coproduct construction:
let v = ctor C "x" 5
// Coproduct elimination:
let n = match v { x p => p ; y q => q + 10 }
```

The two are dual at every level — types, values, eliminators.

## Why this matters for the rest of disp

Three concrete payoffs:

**1. User-defined sum types.** Most non-trivial programs need them. Today disp can encode them via tagged pairs, but the discipline is manual. With Coproduct + match, declaring a new sum type is one line and pattern matching works automatically.

**2. Cleaner standard library.** `Bool`, `Nat`, `Maybe`, `Either`, `List` all become instances of the same machinery. The library shrinks because per-type eliminators (`bool_rec`, `nat_rec`, ...) collapse into `coproduct_rec`.

**3. Categorical foundations stays honest.** The `CATEGORY_THEORY_FOUNDATIONS_PROPOSAL.typ` framework treats every type-former as having a `Functor` field (for morphism action). Coproducts give us the categorical dual to records — both fit naturally in the same framework. Without coproducts, the framework is half-built.

## What this proposal does NOT cover

Things deliberately out of scope:

- **GADTs** (generalized algebraic data types where each constructor refines type parameters). Coq's `Vector n` style. Doable in dependent types via fancier motive inference but adds substantial elaborator complexity.
- **Recursive principle** (induction principles vs eliminators). Coproduct gives non-dependent recursion via the motive; full induction-recursion is more.
- **Coinduction.** Cofix and corecursive types are a different beast.
- **Higher inductive types (HITs).** Cubical's HITs have path-constructors; need separate machinery beyond plain Coproduct.
- **Anonymous variants** (Haskell-style `data Either a b = Left a | Right b` without naming the type ahead of time). Likely not useful in disp; users declare named types.
- **Pattern guards, wildcards, nested patterns** — listed as future extensions.

## Summary

A `Coproduct` library type unifies sum types under one mechanism. Constructors are named with arg-types; the eliminator dispatches on tag and runs the matching handler. `match` becomes a uniform surface for pattern-matching across all inductive types. The kernel doesn't change.

Bool, Nat, Maybe, List, Either, user-defined types all use the same machinery. The scattered eliminators (`bool_rec`, `nat_rec`, `select`, etc.) collapse into `coproduct_rec`. Pattern matching becomes ergonomic.

The categorical duality with records — products and coproducts as the two sides of the same coin — is explicit. The framework in `CATEGORY_THEORY_FOUNDATIONS_PROPOSAL.typ` already handles both directions; this proposal makes them symmetric in the library.

Dependencies: records must land first. Estimated total effort: 4–6 weeks across phases 1–4. Phase 5 extensions are optional.

This proposal is structured so iteration is independent: phases 1–4 can land sequentially; extensions in phase 5 are individually addable. The proposal can be revised in pieces without invalidating the framework.
