# Records and Refinement-Typed Projection — Proposal

> **STATUS (2026-05-15):** Library-types phase complete (steps 1–6 from §11.1 landed; 148 tests passing). Encoding-migration phase paused after investigation revealed the original "hard cutover" plan was incompatible with `recq`'s Church-encoding dependency. **Sections 7–10 below describe the revised design (Option B: smart-wrapped chains + `rec {…}` recursive records) that supersedes the original `{x := e} → t e1 (t e2 unit_witness)` plan in the earlier draft of this document.** Pending: implementation of §8 encoding flip + §9 `rec` form + kernel rewrite per §10.

## 1. Motivation

The current record machinery (`SYNTAX.typ` §"Programs, record bodies, and items", `COMPILATION.typ` §"Record encoding") works but has two costs:

1. **Bespoke compile-time tracking.** `src/compile.ts:413–426` (`proj` case) walks back to construction sites via `resolveExprRecord` to resolve field names. The elaborator maintains parallel data structures (`record.fields`, `record.fieldTrees`, `record.fieldInnerFields`) that exist only for projection support. This logic doesn't compose with the rest of the kernel — it's a special case in elaboration.

2. **No tree-calculus error reporting for projection mistakes.** `r.bogus` raises a host-level `Error("projection '.bogus': field not found")` in `compile.ts`. The error doesn't come from the type system; it comes from a TypeScript check that has no analogue in the object language.

This proposal reformulates records so that:

- A record is an iterated `Sigma`, with names tracked in the type's metadata.
- Projection is a tree-native function whose argument refinement type rejects invalid field names. **Compile-time errors for `r.bogus` come from kernel predicate evaluation**, identical to how `Bool TT = TT` is decided.
- Mutual recursion in records becomes a first-class user feature (`rec {…}`), with the kernel's handler-record as one instance of the general form.
- Dependent records work automatically because iterated `Sigma` already handles dependence.

The win is a smaller elaborator, a tree-calculus-native error model, dependent records for free, and a generalized recursion form that retires the kernel's bespoke `recq` combinator.

## 2. Design principles

1. **Object language is the spec.** Records compile to existing kernel-validated structures (`Sigma`, `Refinement`). No new kernel primitives.
2. **Errors are predicate evaluations.** Invalid projections fail because a Refinement type's predicate reduces to `FF`. Not because the host code raised.
3. **Speed is optimization, not semantics.** A "slow path" using runtime name-walk is the reference; an elaborator inlining pass produces direct projection chains. The two are provably equal by reduction.
4. **Names live in types, not values.** A `Record fields` value is a chain of field values. The field names live in `fields`, which is the type's `params` metadata.
5. **Hash-cons does the heavy lifting.** Distinct field-name sets produce distinct types; identical schemas share trees. Memo makes repeated work free.
6. **Unify, don't bifurcate.** Records and the kernel handler record must respond to the SAME projection desugaring rule. Two parallel mechanisms would split the elaborator and double the test surface.

## 3. Library types — landed (steps 1–6)

The library-types phase is **complete** and verified by 148 passing tests. These are real and usable today; the encoding migration in §§7–10 builds on top.

### 3.1 `Refinement` — `lib/types/refinement.disp`

```disp
Refinement_pf := {A, P} -> guard (predicate_frame_form
  (t refinement_pf_recognizer
     (t (t (unguard_checked A) P) t)))
Refinement := Refinement_pf
```

`Refinement A P` is inhabited by `v` iff `A v = TT` AND `P v = TT`. Predicate-frame wrapping; closed dispatch (no bind_hyp). Reflection: `is_refinement`, `refinement_base`, `refinement_pred`.

### 3.2 `Unit` — `lib/types/unit.disp`

```disp
unit_witness := t t t   // = fork(LEAF, LEAF), the K combinator's tree
Unit := guard (predicate_frame_form (t unit_pf_recognizer (t t t)))
```

Singleton type. The recognizer accepts only `unit_witness`. Reflection: `is_unit_witness`.

### 3.3 `String` — `lib/types/string.disp` (Option A: permissive)

```disp
String_pf := predicate_frame ({_, _} -> TT) t   // accepts any tree
empty_string := {n, _c} -> n
string_cons  := {byte, rest} -> {_n, c} -> c byte rest
string_eq    := tree_eq
```

Identifiers compile to canonical Scott byte-list trees via the host-side atom table `internName` in `src/compile.ts`. Hash-consing makes equality O(1). Option B (a structural `is_scott_byte_list` recognizer) is reserved for if/when we need to reject non-string trees passed as names — currently the parser only emits canonical trees so the structural check is redundant.

### 3.4 List helpers added to `lib/std/list.disp`

```disp
list_is_nil := {xs} -> is_leaf xs
list_head   := {xs} -> pair_fst xs
list_tail   := {xs} -> pair_snd xs
list_find   := {pred, xs} -> ...   // first element where pred returns TT, else nil
```

`is_zero` / `pred` / `succ` were already in `lib/std/nat/ops.disp`.

### 3.5 `Record` — `lib/types/record.disp`

```disp
sigma_chain_top := {fields} -> sigma_chain fields unit_witness
Record_pf := {fields} -> guard (predicate_frame_form
  (t record_pf_recognizer (t fields t)))
Record := Record_pf
record_fields := {T} -> pair_fst (pair_snd (type_meta (unguard_or_self T)))
```

`Record fields` wraps an iterated `Sigma` chain in a predicate_frame whose params slot carries the FieldList. Value shape is `t v1 (t v2 ... unit_witness)` — the same nested-pair chain the §8 surface encoding will produce. Type identity tracks field names because they live in `params`.

Note: types/record.disp uses triage directly on the field-list constructor shape rather than calling std/list helpers, because std/ imports kernel/prelude.disp which re-exports this file (import cycle).

### 3.6 `ValidField` + `proj` — `lib/std/record.disp`

```disp
in_fields := fix ({self, fields, n} ->
  triage FF (...) ({head, tail} ->
    match (tree_eq (pair_fst head) n) {
      TT => TT
      FF => self tail n
    }) fields)

ValidField := {fields} -> Refinement String ({n} -> in_fields fields n)
proj       := {fields, r, n} -> sigma_walk r (find_index fields n)

record_names   := {T} -> list_map ({nt} -> pair_fst nt) (record_fields T)
record_type_at := {T, n} -> pair_snd (list_find ({nt} -> tree_eq (pair_fst nt) n) (record_fields T))
```

`ValidField fields` is the refinement type that accepts exactly the names in `fields`. `proj` is the tree-native projector. Both work end-to-end against in-language constructed `Record` values.

## 4. The "compile-time error from kernel predicate" mechanism

This is the load-bearing claim — and the analysis below confirms it works as a clean rewrite of today's elaborator, not as a special case.

`src/compile.ts:846-938` already orchestrates type checking the same way the proposal needs: `check(e, expected, ctx)` compiles `e`, runs `applyTree(expected, tree, APPLY_BUDGET)`, and rejects if the result isn't `TT`. The host orchestrates; the kernel decides via reduction.

Under the new encoding (§8), the parser desugars `r.x` to `proj r ("x" : ValidField (record_fields_of r))`. The annotation `: T` already routes through `check()` (compile.ts:973-988). The elaborator computes `ValidField (record_fields_of r)` from r's known type, the existing `check` path runs `applyTree`, and an `FF` result throws via the existing "type check failed" branch.

**Net code change at projection sites:** the bespoke `record.fields`/`fieldTrees`/`fieldInnerFields` side-tables and `resolveExprRecord` walking go away. Replaced by `record_fields_of (type_of r)` — a kernel-evaluated query. Roughly the same line count, but the truth-condition is decided by kernel reduction rather than a TypeScript string-table lookup.

The "available fields" error-message UX is preserved as a small special case in the elaborator for literal-name projections; otherwise users get the generic "Refinement returned FF" path.

## 5. Why the original "hard cutover" plan failed

The earlier draft of this document (audit in §10.2 of the original) claimed nested-pair encoding could replace Church-encoded records via a hard cutover. The audit greps came up clean:

```
grep -rn "= use \""  lib/ src/        → 0 hits
grep -rn "let.*= use "  lib/ src/      → 0 hits
grep -rn "{}"  in .disp source         → 0 hits
```

But the audit missed the load-bearing consumer of Church-encoded records: **`recq` itself**.

```disp
recq := {components} -> fix ({self, query} -> components query ({q} -> wait self q) self query)
```

This combinator (in `lib/prelude.disp:76`) and its consumer `kernel := recq {hyp_reduce := …, guard := …, …}` (in `lib/kernel/handlers.disp:340`) treat the record literal `{…}` as a function — `components query` is Church dispatch. Under a naive nested-pair encoding, `components query` becomes "fork applied to walker" which triages on the walker's tree and produces garbage.

Verified by attempting the hard cutover: 33/38 test files broke immediately because every kernel handler dispatch failed. Reverted.

## 6. The encoding question revisited

After the failed hard cutover, the design question became: how do projection desugaring + recq dispatch + Record library type coexist?

Two ways to think about it:

**Option A — make recq dispatch via chain accessors.** Components becomes a chain; recq's `components query` flips to `walker components`. Internal handlers receive `(self, walker)` instead of `(ks, raw, query)`. But the recq output (`kernel`) is still a function. External call sites `kernel.field` need `target walker`; chain literals `{x := e1}.x` need `walker target`. **Two desugaring rules, distinguished by an elaborator `isChain` flag.**

**Option B — make every record a function that hands its inner chain to whatever inspector you supply.** A `{x := e1, y := e2}` literal compiles to:

```disp
λi. i (t e1 (t e2 unit_witness))
```

The outer wrapper is a one-arg lambda; the chain sits inside. Apply the record to a walker and β-reduction produces `walker (the chain)`, which walks the chain. **One desugaring rule (`target walker`) works for kernel, chain literals, ks proxies, raw selves — all four shapes — because all four are functions that hand back chain content.**

**Decision: Option B.** Unified desugaring, no elaborator complexity flag, recq stays structurally similar to today (`components walker self walker` — just rename `query` to `walker`). Cost is one extra β-reduction per access, fully memoized by hash-cons.

What Option B gives up: records are no longer pure data you can walk directly. To inspect a record's chain without using a known walker, apply to identity: `record (λx. x) = chain`. That's a tiny ceremony, and the chain you get back IS pure data that library code can introspect.

For the `Record fields` library type's recognizer: extract chain via `v (λx. x)`, then check against the FieldList. One extra step beyond the pure-chain version implemented in §3.5; otherwise identical.

## 7. Recursive records as a general feature

Once Option B is in place, the kernel's `recq` pattern generalizes naturally. Every record that wants self-reference can opt in via `rec {…}`:

```disp
let counter = rec {
  count := 0
  inc   := {n} -> add count n        // bare `count` refers to self.count
  reset := {} -> 0
  next  := {} -> inc count           // bare `inc` and `count` refer to self.inc / self.count
}
```

The parser detects that field bodies reference sibling names and rewrites those references to `self.<field>` projections. The compiler wraps the whole construction in `fix ({self} -> …)`, producing a wrapped chain whose fields can refer back to `self` via the standard projection mechanism.

Compiled form (roughly):

```disp
fix ({self} -> wrap_chain (
  t 0                                                                          // count
    (t ({n} -> add (self walker_count) n)                                      // inc
       (t ({} -> 0)                                                            // reset
          (t ({} -> (self walker_inc) (self walker_count)) unit_witness)))))   // next
```

Mutual recursion is bounded by `wait` deferral, same mechanism the kernel uses today. The user typically doesn't write `wait` explicitly because call sites (`self.inc x`) supply all args; deferral only matters when constructing partial applications inside a method body — and there the user writes `wait self.field` explicitly. **Be liberal with `wait` for any cross-field call site that produces a value capturing self.**

### 7.1 The kernel as a `rec {…}` instance

Under §6 + §7, `recq` becomes redundant:

```disp
// lib/kernel/handlers.disp — after migration
kernel = rec {
  hyp_reduce       := {query, ...rest} -> ... (wait self.guard ...) ...
  guard            := {query, ...rest} -> ... (wait self.predicate_frame ...) ...
  unguard          := ...
  checked_apply    := ...
  predicate_frame  := ...
  eliminator_frame := ...
  bind_hyp         := ...
}
```

The bespoke `recq` combinator in `lib/prelude.disp:76` retires. The `ks`/`raw`/`query` 3-arg dispatch contract collapses to plain `self.field` projections (lazy by default in cross-field call sites; the `wait` is implied by the partial-application context). External code (`Hyp := wait kernel.hyp_reduce …`) keeps the same syntactic shape.

Importantly: **the kernel stops being a special case in the language**. It becomes one instance of the general `rec {…}` form available to any user.

### 7.2 The two surface forms

Form 1 (recommended for ergonomics) — the implicit-`self` shape shown above. The parser does letrec-style name resolution.

Form 2 (falls out as a special case when there's no recursion) — non-recursive records use the plain `{…}` form. No `rec` keyword, no auto-`self`-passing, no fix-construction. Compatible with §3–§6 Record library type recognition.

## 8. Projection desugaring under Option B

Single rule: `r.field` desugars to `r W_field`, where `W_field` is the chain walker for field's position.

The walker is a closed term built from prelude primitives:
- `W_0 = pair_fst`
- `W_1 = {r} -> pair_fst (pair_snd r)` → after bracket abstraction: `S (K pair_fst) pair_snd`
- `W_i = pair_fst (pair_snd^i r)` → `S (K pair_fst) (S (K pair_snd) … pair_snd)` (i nested pair_snds)

These are deterministic per index, hash-cons identical across call sites.

Resolution flow at a `r.field` site:
1. Elaborator queries r's type → finds Record-typed annotation or recType → extracts FieldList.
2. Looks up `field` in FieldList → gets position `idx`.
3. Builds (or memoizes) walker tree W_idx.
4. Optionally: emits `check(internName(field), ValidField fieldList_tree, ctx)` for kernel-validated rejection of typos. The check runs `applyTree(ValidField_tree, name_tree, APPLY_BUDGET)`; on `FF`, throws with the available-fields list.
5. Emits CIR: `cap(target_cir, { tag: "lit", t: W_idx })`.

For a Record-typed (Option B wrapped) value at runtime:
- `r W_idx = (λi. i chain) W_idx = W_idx chain = pair_fst (pair_snd^idx chain)` = the field value.

For the kernel (recq output, which is itself a wrapped function via §7.1):
- `kernel W_idx` fires the rec's fix, dispatches via `(W_idx components) self W_idx` = the handler partial-applied to (self, W_idx).

For ks-style lazy proxies (if any survive after the kernel rewrite):
- `ks W_idx = (λw. wait self w) W_idx = wait self W_idx`.

**All four shapes resolve through the same desugaring.**

## 9. H-rule identity check under Option B

Today's H-rule sites compare `wait (ks query) meta` against reconstructed types. Hash-cons identity makes this O(1) because `self`, `query`, and `meta` are stable trees.

Under Option B:
- `ks query` (today) ↔ `wait self W_field` (tomorrow).
- `wait (ks query) meta` (today) ↔ `wait (wait self W_field) meta` (tomorrow).
- Both `self` and `W_field` and `meta` are stable trees (walker functions hash-cons via bracket-abstraction determinism).
- Hash-cons identity preserved → H-rule O(1) preserved.

**Verified by reasoning, not yet by code.** The first migration step (§10) is to confirm this empirically: build a test that compares wait-of-wait-of-self-of-walker against an independently-built reconstruction, expecting hash-cons match.

## 10. Migration plan (revised)

Each step is contained and reversible. Run tests after every step.

### Step 1 — Smart-wrapper encoding for `{x := e}` literals
Update `src/compile.ts:333` (`case "recValue"`) to emit `λi. i (t e1 (t e2 ... unit_witness))` instead of the current Church `λsel. sel e1 e2`.

Update file-level export (`compile.ts:1169-1175`) similarly: file's exports become `λi. i (t v0 (t v1 ... unit_witness))`.

Update projection (`compile.ts:413-426`): emit `cap(target_cir, walker_tree)` where walker_tree is built per-index from `pair_fst`/`pair_snd` primitives. The walker construction is the same per-position math used for the §6 sigma_walk; hoist that into a host helper.

Update `open` extraction sites (`compile.ts:382, 506, 1294`): instead of `applyTree(target, selectorTree(n, i), APPLY_BUDGET)`, use `applyTree(target, walker_for_index(i), APPLY_BUDGET)`. Same arity, different walker.

Test expectation: all 148 current tests pass unchanged. The Option B wrapper preserves Church-shaped projection semantics — `record selector` and `wrapped record walker` both produce the field value via β-reduction.

### Step 2 — Empirical H-rule check
Add a test in `lib/tests/walker.test.disp` (or new file) that constructs two independent `wait (wait self W_X) meta` forms and verifies `tree_eq` returns TT. If this fails, walkers aren't hash-cons-stable and the migration is blocked pending walker normalization.

### Step 3 — Parser: `rec {…}` form
Add `rec` keyword to the parser. Parsing produces a new AST node `recValueRec` with the same field list as `recValue`, plus a flag.

In compile.ts, handle `recValueRec`:
1. Compile the chain construction with each field body evaluated in a scope where sibling field names resolve to `self.<field>` projections.
2. Wrap the result in `fix ({self} -> chain_construction)`.

Implementation detail: the chain construction returns a wrapped record (§8 form). The fix produces a deferred form; applying it to an inspector fires the fix and returns `inspector chain`. For external `kernel.field` calls, this gives the right shape via §8 desugaring.

### Step 4 — Kernel rewrite as `rec {…}`
Rewrite `lib/kernel/handlers.disp:340` from `kernel := recq {…}` to `kernel = rec {…}`. Each handler body:
- Old `{ks, raw, query} -> body` becomes `{query, …rest} -> body_with_self`.
- Old `ks.field` becomes `wait self.field` (explicit defer at partial-app contexts).
- Old `raw.field` becomes `self.field` (eager dispatch).

Retire `recq` and the bespoke `ks` proxy in `lib/prelude.disp:76`. `rec` is the public combinator now (or rather, the keyword that desugars to fix+wrap).

### Step 5 — kernel_ref consolidation
`kernel_ref` (`handlers.disp:353`) exists today to provide a wait-form view of kernel for external type construction. Under §6 + §7, this is `{q} -> wait kernel q` — still expressible. Either keep it as a one-line definition, or inline its uses at the ~3 sites that need it. The latter is cleaner; do it during the kernel rewrite.

### Step 6 — External `kernel.field` sites
There's one external `kernel.field` use today: `lib/types/type.disp:86` (`wait kernel.hyp_reduce neutral_meta`). After §8 desugaring this becomes `wait (kernel walker_hyp_reduce) neutral_meta`. Should "just work" given §8 routing; verify in a regression test.

### Step 7 — Test surface cleanup
- `lib/tests/rec.test.disp` and `lib/tests/walker.test.disp` reference `rec`/`recq` directly. Rewrite to the new `rec {…}` form.
- The `kernel.hyp_reduce` constant pinning test (`lib/tests/sig_pinning.test.disp`) verifies a known tree shape; update the expected tree if it changes under the new encoding (it shouldn't, by §8 unification, but verify).

### Step 8 — Delete bespoke record machinery
- `resolveExprRecord` (`compile.ts:462`): replace usages with `record_fields_of (type_of r)` queries. Delete once unused.
- `ScopeEntry.fields`/`fieldTrees`/`fieldInnerFields`: delete side-tables.
- `selectorTree`/`buildSelector` (`compile.ts:134, 143`): delete once walker-based emission is the only path.

### Step 9 — Spec doc updates
- `TYPE_THEORY.typ` §5.X: add Refinement (move from §5.8 sketch).
- `TYPE_THEORY.typ` §5.Y: add Unit.
- `TYPE_THEORY.typ` §5.Z: add Record (predicate_frame wrapper + ValidField refinement pattern).
- `COMPILATION.typ` §"Record encoding": rewrite for Option B + `rec {…}`.
- `SYNTAX.typ` §"Programs, record bodies, and items": add `rec` keyword.

## 11. Decisions

1. **`unit_witness` tree: `pair t t`** ✅ landed. Matches the "no-params" sentinel already used in `lib/types/false.disp:30`. Does not collide with Bool (TT is `K K = pair (pair t t) (pair t t)`, not `K` alone).

2. **String encoding: Scott byte list (permissive Option A)** ✅ landed. Stable, reflective, simple to spec. Performance fine because hash-cons makes equality O(1). Option B (structural recognizer) reserved for future strictness work.

3. **Atom-table interning: pure deterministic** ✅ landed in `src/compile.ts:internName`. Every disp session produces the same canonical String tree for a given identifier. Cost is one Scott-list construction per unique identifier, paid once and cached.

4. **Encoding: Option B (smart-wrapped chains)** — NEW resolution superseding the original "nested-pair hard cutover." A `{x := e}` literal compiles to `λi. i (t e1 (t e2 unit_witness))`. All records are functions that hand back chain content. One desugaring rule for projection.

5. **Recursive records: `rec {…}` keyword** — NEW. Generalizes the kernel's `recq` pattern as a first-class user feature. The kernel becomes a normal user record under this form.

6. **`recq` and `ks`/`raw` distinction retired** — under §10.4, kernel handlers receive only `(query, …rest)` and reference siblings via `wait self.field` (lazy) or `self.field` (eager). The proxy mechanism dissolves into explicit per-call-site `wait`.

7. **Empty record `{}`: hard cutover** ✅ Audit confirms zero current uses. Under Option B, `{}` compiles to `λi. i unit_witness`.

8. **Field order canonicalization: alphabetical at parse time** ✅ Original decision stands. Makes `{x := 1, y := 2}` and `{y := 2, x := 1}` produce identical types and identical values.

9. **Dependent record scoping rule** ✅ Original decision stands. Field-type expressions parse in a scope where preceding field names are bound. Mirrors `let`-binding shadowing.

10. **H-rule O(1) identity check preservation** — to be verified empirically in §10.2 before committing to encoding flip. Reasoning suggests walker functions hash-cons stably; needs test.

## 12. What this proposal does NOT cover

- **Record subtyping / row polymorphism.** Two records with overlapping fields are not interchangeable; subtyping coercions are not provided.
- **Mutable records / update operators.** A `record { r | x := new_val }` update form would require additional library functions.
- **Record types as kernel primitives.** Records are library types. The seven kernel primitives don't grow.
- **First-class field names.** Names are tree values, but the parser still translates source identifiers; users can't write `"x"` directly in a projection (only via `r.x`). A separate `proj_dyn r expr` for computed names is a follow-up.
- **Enum desugaring** (sketched in original §9). Moved to a separate `ENUMS_PROPOSAL.md` to be drafted once records land.

## 13. Summary

After §10 lands, the entire record story in disp is:

- A record is a function `λi. i chain` where `chain` is a nested-pair of field values terminating in `unit_witness`.
- Projection `r.x` desugars to `r W_x` where `W_x` is the walker for x's position in the FieldList.
- Compile-time errors for unknown fields come from `ValidField`'s predicate evaluating to `FF` — kernel reduction, no host check.
- Speed is recovered by hash-cons memoization; the wrapper layer adds one β-reduction per access.
- Dependent records work because the chain is recognized via iterated `Sigma`.
- Mutual recursion is available via `rec {…}` — a general user feature that subsumes the kernel's `recq` combinator.
- The kernel's handler record becomes one instance of `rec {…}`; the bespoke `ks`/`raw`/`query` 3-arg dispatch retires.
- The `resolveExprRecord` machinery in `src/compile.ts` retires.

Records become library code with a single elaborator desugaring rule. The kernel stops being a special case in the language. Errors come from tree calculus. That is the landing zone.
