# Records and Refinement-Typed Projection — Proposal

> **STATUS (2026-05-16):** Library-types phase complete (steps 1–6 from §11.1 landed; 148 tests passing). Encoding-migration phase paused after investigation revealed the original "hard cutover" plan was incompatible with `recq`'s Church-encoding dependency. Sections 7–10 below describe the revised design: Option B (smart-wrapped chains) for the encoding flip, plus retirement of `recq` in favor of the existing `rec` combinator (`lib/prelude.disp:66`). **No `rec` keyword** — recursive records are a value-level construction over `fix`, not a parser/elaborator feature. Pending: implementation of §8 encoding flip + kernel rewrite per §10. Future work on typed recursion (Design D) tracked in §11.

## 1. Motivation

The current record machinery (`SYNTAX.typ` §"Programs, record bodies, and items", `COMPILATION.typ` §"Record encoding") works but has two costs:

1. **Bespoke compile-time tracking.** `src/compile.ts:413–426` (`proj` case) walks back to construction sites via `resolveExprRecord` to resolve field names. The elaborator maintains parallel data structures (`record.fields`, `record.fieldTrees`, `record.fieldInnerFields`) that exist only for projection support. This logic doesn't compose with the rest of the kernel — it's a special case in elaboration.

2. **No tree-calculus error reporting for projection mistakes.** `r.bogus` raises a host-level `Error("projection '.bogus': field not found")` in `compile.ts`. The error doesn't come from the type system; it comes from a TypeScript check that has no analogue in the object language.

This proposal reformulates records so that:

- A record is an iterated `Sigma`, with names tracked in the type's metadata.
- Projection is a tree-native function whose argument refinement type rejects invalid field names. **Compile-time errors for `r.bogus` come from kernel predicate evaluation**, identical to how `Bool TT = TT` is decided.
- Mutual recursion in records is expressed via the existing `rec` combinator (`lib/prelude.disp:66`) — `rec components = fix({self, sel} -> components sel self)`. The kernel's handler-record collapses to one user of this combinator, retiring `recq` and the bespoke `ks`/`raw`/`query` triple.
- Dependent records work automatically because iterated `Sigma` already handles dependence.

The win is a smaller elaborator, a tree-calculus-native error model, dependent records for free, and retirement of the kernel's bespoke `recq` combinator in favor of the already-public `rec`.

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

## 7. Recursive records via the existing `rec` combinator

Once Option B is in place, the kernel's `recq` pattern collapses into the existing `rec` combinator at `lib/prelude.disp:66`:

```disp
rec := {components} -> fix ({self, sel} -> components sel self)
```

This is already a pure tree-calculus function — no parser sugar, no special AST node. A user-defined recursive record is a record-of-self-taking-functions wrapped by `rec`:

```disp
let counter = rec {
  count := {self, _} -> 0
  inc   := {self, n} -> add (self.count t) n
  reset := {self} -> 0
  next  := {self}  -> (self.inc t) (self.count t)
}
```

Every field takes `self` as its first parameter, including value-shaped fields (those wrap their result in a unary thunk: `{self, _} -> 0` — applied to a dummy `t` at use). Cross-field references go through `self.field` projections. The combinator's `fix` ties the knot.

Under Option B's smart-wrapper encoding (§8), the reduction trace for `counter.inc 3` is:
- `counter.inc` = `counter walker_inc` = `walker_inc chain_of_field_bodies` = `({self, n} -> add (self.count t) n) counter`.
- `... 3` reduces the inner add against `(counter.count t) = (({self, _} -> 0) counter t) = 0`, yielding `add 0 3`.

Mutual recursion is bounded by `wait` deferral, same mechanism the kernel uses today. `wait self.field` defers; bare `self.field` is eager. The user reaches for `wait` only when constructing partial applications inside a method body that capture self.

**Static check for non-functional sibling refs.** A field body that references a sibling without going through a binder will diverge at `fix`-construction time (tree calculus is strict; `wait` is the only deferral). The elaborator should detect this and emit a clear error rather than producing a record that hangs on first use. Concretely: walk each field's AST; if the body's outermost form is not a binder and it mentions any sibling field name, reject with "non-functional rec field cannot reference siblings; thunk it with `{_} -> …`". Cheap to implement; bad UX without it.

### 7.1 The kernel as a `rec` instance

Under §6 + §7, `recq` becomes redundant:

```disp
// lib/kernel/handlers.disp — after migration
kernel = rec {
  hyp_reduce       := {self, meta, v} -> ... (wait self.guard ...) ...
  guard            := {self, core, v} -> ... (wait self.predicate_frame ...) ...
  unguard          := ...
  checked_apply    := ...
  predicate_frame  := ...
  eliminator_frame := ...
  bind_hyp         := ...
}
```

`self` is the first parameter of each field; the `rec` combinator's fix-wrap passes the rec-record back through this slot. §8 spells out the reduction: `kernel W_idx = (W_idx components) self W_idx = handler at W_idx, partial-applied to self`. The bespoke `recq` combinator in `lib/prelude.disp:76` retires entirely. The `ks`/`raw`/`query` 3-arg dispatch contract collapses to `self` plus walker-as-arg: eager dispatch is `self.field` (= `self walker_field`), lazy dispatch is `wait self.field`. There's no longer a separate `query` parameter — each handler is structurally specialized to its slot, and the H-rule reconstruction `wait (ks query) meta` becomes `wait self.this_handler meta` with `this_handler` hard-coded per body. External code (`Hyp := wait kernel.hyp_reduce …`) keeps the same syntactic shape; only the underlying tree shape shifts.

Importantly: **the kernel stops being a special case in the language**. It becomes one instance of the existing `rec` combinator available to any user.

### 7.2 Non-recursive records

Non-recursive records keep using the plain `{…}` literal form (already implemented). No `rec` involvement, no auto-`self`-passing, no fix-construction. Compatible with §3–§6 Record library type recognition. The §8 emission flip applies uniformly to both forms.

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

Today's H-rule sites inside recq handlers compare `wait (ks query) meta` against a reconstructed type tree. Hash-cons identity makes this O(1) because the recq form binds stable values for `ks` and `query`, and `meta` is constructed deterministically.

Under the new combinator-based form (post-§7.1):
- Old `ks query` ↔ new `wait self.this_handler` = `wait (self walker_this_handler)`.
- Old `wait (ks query) meta` ↔ new `wait (wait (self walker_this_handler)) meta`.
- `self` is the fix-point of the `rec` combinator over the kernel record — a stable closed value once compiled. `walker_this_handler` is the bracket-abstraction-deterministic walker for that field's slot. `meta` is the same deterministic construction as today.
- Hash-cons identity is preserved → H-rule O(1) preserved.

**Verified by reasoning, not yet by code.** Step 2 of §10 is to confirm empirically: build a test that compares `wait (wait (kernel walker_X)) meta` constructed inside a handler body against the same form constructed externally, expecting `tree_eq = TT`. If walkers aren't hash-cons-stable, the migration is blocked pending walker normalization.

## 10. Migration plan (revised)

Each step is contained and reversible. Run tests after every step.

### Step 0 — Extend `infer` to recognize Records

Prerequisite for §10.8 (deleting `resolveExprRecord`). The elaborator currently returns `type: null` for `recValue` and `proj` (`compile.ts:1062-1070`), so the projection desugar can't route through `check()` to get kernel-evaluated rejection — there's no `type_of r` to query.

Extend `KernelHelpers` (`compile.ts:632`) mirroring the existing Pi/Type pattern:
- Sample `Record empty_fields` at startup; record its recognizer ID.
- Add `isRecord(t)`, `recordFields(t)`, `recordTypeAt(t, nameTree)`.
- Add `buildRecord(fieldListTree)` — `applyTree(Record, fieldListTree)`.
- Add a host helper that builds a FieldList tree from a list of `(nameTree, typeFnTree)` pairs (mirror the `string_cons` byte-list pattern).

Extend `infer`:
- `case "recValue"`: for each field, recursively `infer` its value's type. If all field types are known, build the FieldList and call `buildRecord`. Return `{tree, type: Record_typed_value}`. If any field's type is null, return `type: null` (fall back to current behavior).
- `case "proj"`: recursively `infer` the target. If its type is a Record, call `recordTypeAt(type_of_target, internName(e.field))`. Return that as the field's type.

Field-dependency handling: a later field's type may depend on earlier fields' values (the `type_fn` accepts a `prev_tuple`). For `infer(recValue)`, thread a scope where preceding field names bind to their compiled trees as the loop walks the field list, mirroring the existing `recValue.members` processing at `compile.ts:340-389`.

Why this is Step 0 (not Step 8): Step 1's encoding flip is reversible per-file. The `infer` extension is independent — it adds capability without altering tree shapes. Landing it first means Step 1 onward can immediately route projection through the kernel-validated path where types are known, instead of relying on `resolveExprRecord` static fallback.

Cost estimate: ~150 LOC in `compile.ts`. No library changes. No tree-shape changes. All current tests pass unchanged because the new paths only fire when type info propagates further than today.

### Step 1 — Smart-wrapper encoding for `{x := e}` literals
Update `src/compile.ts:333` (`case "recValue"`) to emit `λi. i (t e1 (t e2 ... unit_witness))` instead of the current Church `λsel. sel e1 e2`.

Update file-level export (`compile.ts:1169-1175`) similarly: file's exports become `λi. i (t v0 (t v1 ... unit_witness))`.

Update projection (`compile.ts:413-426`): emit `cap(target_cir, walker_tree)` where walker_tree is built per-index from `pair_fst`/`pair_snd` primitives. The walker construction is the same per-position math used for the §6 sigma_walk; hoist that into a host helper.

Update `open` extraction sites (`compile.ts:382, 506, 1294`): instead of `applyTree(target, selectorTree(n, i), APPLY_BUDGET)`, use `applyTree(target, walker_for_index(i), APPLY_BUDGET)`. Same arity, different walker.

Test expectation: all 148 current tests pass unchanged. The Option B wrapper preserves Church-shaped projection semantics — `record selector` and `wrapped record walker` both produce the field value via β-reduction.

Before flipping the encoding, add an integration test that the existing 148 tests don't cover: feed a parser-built `{x := zero}` to the library `Record fields_x_nat` recognizer and assert acceptance. Today's record tests in `lib/tests/record.test.disp` all hand-construct chains as `t v unit_witness`; none exercise the path from surface `{x := …}` syntax into the library `Record` type. Without this test the "all 148 pass" claim is true but vacuous — it doesn't certify that the new encoding actually flows into the recognizer that's supposed to consume it. The recognizer in `lib/types/record.disp:64-67` currently applies `sigma_chain_top params v` directly to `v`; under Option B it must apply to `v I_canonical` (extract chain by applying identity). Update the recognizer in lockstep with the emission flip.

### Step 2 — Empirical H-rule check
Add a test in `lib/tests/walker.test.disp` (or new file) that constructs two independent `wait (wait self W_X) meta` forms and verifies `tree_eq` returns TT. If this fails, walkers aren't hash-cons-stable and the migration is blocked pending walker normalization.

### Step 3 — Non-functional sibling-ref static check (parser/elaborator)
Add a check (in `parse.ts` or early in `compile.ts`) that for each `rec`-application's argument record literal, no field's body references a sibling field by name unless the body is a binder. Reject with the message "non-functional rec field cannot reference siblings; thunk it with `{_} -> …`". This is a pre-flight check on the argument to `rec` (a regular function call), not a special AST node — `rec` itself remains an ordinary library combinator.

The detection target: `e.tag === "app"` where `e.f` resolves to the `rec` binding, `e.x.tag === "recValue"`, and any `field.value.tag !== "binder"` mentions another field's name. The mentions check reuses `exprMentions` (`compile.ts:176`).

This check is the only static behavior associated with `rec`. No keyword, no AST node, no parser-side rewriting.

### Step 4 — Kernel rewrite via the `rec` combinator
Rewrite `lib/kernel/handlers.disp:340` from `kernel := recq {…}` to `kernel := rec {…}`. Each handler body:
- Old `{ks, raw, query} -> body` becomes `{self, …rest} -> body` — one parameter, supplied by `rec`'s fix-wrap.
- Old `ks.field` becomes `wait self.field` (lazy / deferred — preserves recq's lazy-proxy semantics).
- Old `raw.field` becomes `self.field` (eager projection).
- The old `query` parameter (the atom selector identifying the dispatched handler) is gone. Each handler is structurally specialized to its slot; the H-rule reconstruction `wait (ks query) meta` becomes `wait self.this_handler meta` with `this_handler` hard-coded per body (e.g., `wait self.hyp_reduce meta` inside `hyp_reduce_fn` itself).

Retire `recq` and the bespoke `ks` proxy in `lib/prelude.disp:76`. The existing public `rec` combinator (already at `lib/prelude.disp:66`) is the only recursion combinator going forward.

### Step 5 — kernel_ref consolidation
`kernel_ref` (`handlers.disp:353`) exists today to provide a wait-form view of kernel for external type construction. Under §6 + §7, this is `{q} -> wait kernel q` — still expressible. Either keep it as a one-line definition, or inline its uses at the ~3 sites that need it. The latter is cleaner; do it during the kernel rewrite.

### Step 6 — `kernel.field` / `kernel_ref.field` sites
After Step 4, every `kernel.X` (eager projection) and `kernel_ref.X` (lazy proxy) call site needs auditing. The eager / lazy distinction is load-bearing: `kernel.hyp_reduce` and `kernel_ref.hyp_reduce` produce structurally different wait-form trees, and `lib/types/type.disp:81-86` explicitly documents that `Hyp`'s codomain check relies on the *eager* shape. Under §8 desugaring `kernel.X` becomes `kernel walker_X` (eager) and `kernel_ref.X` becomes `wait kernel walker_X`-style (lazy); both shapes must be preserved exactly.

Sites (verified by grep against today's tree):
- Eager (`kernel.X`):
  - `lib/kernel/handlers.disp:361,388,389,438,444,478` — `is_neutral`, `Hyp`, `StuckElim`, `checked_apply`, `checked_check`, `kernel_hyp_reduce_sig`.
  - `lib/types/type.disp:86` — `type_pf_codomain_fn` (the one external site).
- Lazy (`kernel_ref.X`):
  - `lib/kernel/handlers.disp:395,408,432,433,479-483` — `guard`, `unguard_checked`, `predicate_frame_form`, `eliminator_frame_form`, and the four `kernel_*_sig` constants for guard / unguard / predicate_frame / eliminator_frame / bind_hyp.
  - `lib/types/pi.disp:21` — `bind_hyp`.
  - `lib/types/type.disp:72-73` — `type_pf_recognizer`.

The internal handler-body uses inside the recq record (`ks.guard`, `ks.predicate_frame`, `ks.checked_apply`, `raw.hyp_reduce`) all migrate as part of Step 4's handler rewrite — they're not enumerated above because their site is the kernel rewrite itself.

### Step 7 — Test surface cleanup
- `lib/tests/rec.test.disp` already exercises `rec` and `recq`. Drop the `recq` tests after Step 4 retires the combinator. The `rec`-using tests remain unchanged in shape — only the encoding under the hood shifts.
- `lib/tests/walker.test.disp:292-301` builds a `bool_pkg` via `recq`. Rewrite to `rec` with explicit `self` parameter.
- The signature-pinning test (`lib/tests/sig_pinning.test.disp`) compares `kernel_X_sig` against `pair_fst (constructor …)`. Both sides recompute consistently under the new encoding, so the test stays green by construction. No expected-tree updates needed.

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

5. **Recursive records via existing `rec` combinator** — Recursive records are constructed with `lib/prelude.disp:66`'s `rec := {components} -> fix({self, sel} -> components sel self)`. **No new keyword, no new AST node, no parser sugar.** Field bodies take `self` as first parameter; cross-field references go through `self.field`. The static well-formedness check for non-functional sibling refs lives in the elaborator as a pre-flight check on `rec`'s argument literal.

6. **`recq` and `ks`/`raw` distinction retired** — under §10.4, kernel handlers receive only `(self, …rest)` and reference siblings via `wait self.field` (lazy) or `self.field` (eager). The proxy mechanism dissolves into ordinary self-reference. Each handler is structurally specialized to its slot.

7. **Empty record `{}`: hard cutover** ✅ Audit confirms zero current uses. Under Option B, `{}` compiles to `λi. i unit_witness`.

8. **Field order canonicalization: alphabetical at parse time** ✅ Original decision stands. Makes `{x := 1, y := 2}` and `{y := 2, x := 1}` produce identical types and identical values.

9. **Dependent record scoping rule** ✅ Original decision stands. Field-type expressions parse in a scope where preceding field names are bound. Mirrors `let`-binding shadowing.

10. **H-rule O(1) identity check preservation** — to be verified empirically in §10.2 before committing to encoding flip. Reasoning suggests walker functions hash-cons stably; needs test.

## 12. Future work: typed recursion

The current plan tracks recursion at the *value* level (`rec` is `fix`-wrapped value construction) but not at the *type* level — the type of a `rec`-built record is plain `Record fields`. Two limitations follow:

1. **Divergence can't be checked at type-check time.** Ill-formed recursive definitions (e.g., a non-functional sibling cycle) diverge at use. The proposal's Step 3 static check catches the common case but doesn't have kernel-level backing.
2. **The type system can't distinguish recursion-bearing records from plain ones.** A function that requires its argument to be a fixed-point produced by `rec` has no way to express that constraint.

Both gaps require kernel-side totality machinery (`wf_fix` / `Total` / `TotalWith` per `TYPE_THEORY.typ`). Once those land, a `RecRecord fields` type-former becomes viable:

```disp
RecRecord fields := Refinement (Record fields) ({v} -> well_formed_fix v fields)
```

The recognizer checks the value's recursive structure is productive. `rec` would then be typed:

```disp
rec : Pi (Record (lift_self fields)) ({components} -> RecRecord fields)
```

where `lift_self` maps each `(name, T)` to `(name, Pi RecRecord_self ({_} -> T))`. This requires a way to express the self-reference without circular type definition — likely via a Mu/Nu type-former or via parametricity over a Self placeholder.

Until that's tractable, recursive records work via the value-level `rec` combinator with the static well-formedness check as a near-term safety net. No callers need to change when `RecRecord` lands later — the function-signature shift can be backward-compatible by aliasing `RecRecord fields := Record fields` as a transition step.

## 13. What this proposal does NOT cover

- **Record subtyping / row polymorphism.** Two records with overlapping fields are not interchangeable; subtyping coercions are not provided.
- **Mutable records / update operators.** A `record { r | x := new_val }` update form would require additional library functions.
- **Record types as kernel primitives.** Records are library types. The seven kernel primitives don't grow.
- **First-class field names.** Names are tree values, but the parser still translates source identifiers; users can't write `"x"` directly in a projection (only via `r.x`). A separate `proj_dyn r expr` for computed names is a follow-up.
- **Typed recursion** (see §12) — deferred to post-`wf_fix`.
- **Enum desugaring** (sketched in original §9). Moved to a separate `ENUMS_PROPOSAL.md` to be drafted once records land.

## 14. Summary

After §10 lands, the entire record story in disp is:

- A record is a function `λi. i chain` where `chain` is a nested-pair of field values terminating in `unit_witness`.
- Projection `r.x` desugars to `r W_x` where `W_x` is the walker for x's position in the FieldList.
- Compile-time errors for unknown fields come from `ValidField`'s predicate evaluating to `FF` — kernel reduction, no host check.
- Speed is recovered by hash-cons memoization; the wrapper layer adds one β-reduction per access.
- Dependent records work because the chain is recognized via iterated `Sigma`.
- Mutual recursion is available via the existing `rec` combinator, with `self` passed as each field's first parameter.
- The kernel's handler record becomes one instance of `rec`; `recq` and the `ks`/`raw`/`query` triple retire.
- The `resolveExprRecord` machinery in `src/compile.ts` retires (gated on Step 0's `infer` extension).
- Typed recursion (`RecRecord`) is deferred to post-`wf_fix` per §12.

Records become library code with a single elaborator desugaring rule. The kernel stops being a special case in the language. Recursive records are pure tree-calculus — no parser keyword, no AST node. Errors come from tree calculus. That is the landing zone.
