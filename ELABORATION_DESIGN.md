# Elaboration Design

The implementation plan that sits between `TREE_NATIVE_TYPE_THEORY.md` (types are predicates) and `BIND_TREE_NBE_IDEA.md` (bind-trees as the binder substrate). This doc tracks the current state of the implementation and the next three phases planned.

**Status**: Phase 1 complete (`examples/predicates.disp`); Phase 2 surface elaborator landed (`src/elaborate.ts` + parser extensions + `examples/elab.disp`). Phase 3 next.

**Companion docs**:
- [`TREE_NATIVE_TYPE_THEORY.md`](TREE_NATIVE_TYPE_THEORY.md) — types are executable predicates; the kernel call is `apply(type, value)`.
- [`BIND_TREE_NBE_IDEA.md`](BIND_TREE_NBE_IDEA.md) — bind-trees, canonical hypothesis tokens, structural splice. Implemented; see substrate inventory below.
- [`DEVELOPMENT_PHILOSOPHY.md`](DEVELOPMENT_PHILOSOPHY.md) — load-bearing. Every component participating in well-typedness must have a tree-calculus encoding before host shortcuts.

---

## Current state (as of 2026-04-13)

### Substrate (built, in `examples/*.disp`)

| Layer | Status | File(s) |
|---|---|---|
| Tree-calc runtime: hash-cons, eager `apply`, budgets, `FAST_EQ` | Done (host) | `src/tree.ts` |
| Surface parser: `def`, `\x.`, juxtaposed app, `t` for leaf, `test = ` | Done (host) | `src/parse.ts` |
| Recursion via lambada's `wait`+`fix` | Tree-native | `recursion.disp` |
| Tagged forms: V, H, App, Lam, Pi (kind tags KV/KH/KA/KL/KP) | Tree-native | `tagged.disp`, `normalize.disp`, `picheck.disp` |
| Bind-trees: BE, BN, BApp, BLam, BPi (KBA/KBL/KBP) | Tree-native | `splice_full.disp` |
| `splice` (term × bind-tree × value → term), full dispatch | Tree-native | `splice_full.disp` |
| `normalize` (full β to fixed point, reduces under binders) | Tree-native | `normalize.disp` |
| `infer` (H → annotation; App → splice piCodom of recursively-inferred head) | Tree-native | `checking.disp` |
| `check` (Lam vs Pi descent + App via infer + H lookup + conversion fallback) | Tree-native | `checking.disp` |
| `pred_of` (tagged type → callable predicate; types-as-predicates kernel) | Tree-native | `predicates.disp` |
| `mkAtom` (atom in already-predicate form, for synthesis) | Tree-native | `predicates.disp` |

139 tree-native test cases pass across 12 example files. The host-side TS only handles the runtime, the parser, and the test driver — every checker-relevant operation is a tree program.

### Sharp lessons earned (must respect when extending)

1. **Recursive-call arg order under `wait`/`fix`.** `rec X Y Z` is safe iff `X` is bound by a lambda *inside* the recursive function. If `X` derives from an outer binder, fix-expansion at compile time eagerly fires `wait` and diverges. Workaround: reorder so the inner-derived arg comes first. See `splice.disp` header.

2. **Strict branch evaluation.** `triage` evaluates every branch — including the unselected one. To prevent eager `rec` in unselected branches, wrap each branch as `\u. body` and apply *after* dispatch picks one. See `normalize.disp` and `checking.disp`.

3. **`H` wraps its type, not its binder.** `BIND_TREE_NBE_IDEA.md` §3.4 has H wrap a binder with `type_of_H` digging four layers; we changed to `mkH ty := tagged(KH, ty)` and `type_of_H := payload`. App-of-H typing needs the type directly; this is the cleaner contract.

4. **Bind-trees are load-bearing.** They aren't decoration; `splice` trusts them. A bind-tree that lies about V positions silently corrupts results (we hit this writing test fixtures). The elaborator must compute them from term structure.

5. **H comparisons need both direct and unwrap paths.** Phase 1's `pred_of` initially failed Lam-vs-Pi recursion: after splice, both `cand` and `ty` become the *same* `mkH(A)` token, and direct `fast_eq` is the right comparison. App-of-H, by contrast, has `cand = mkH(A)` against a bare `ty = A`, which needs H-unwrap. The H-case branch must try direct first and fall back to unwrap.

6. **Canonical H tokens collide on type alone.** A two-level dependent type like `(x:A) → (y:A) → x` requires distinguishing the outer `x` from the inner `y` even though both have type `A`. With single-arg `mkH ty`, both descents introduce identical hash-cons-equal tokens, so KI-shape `\x.\y.y` is wrongly accepted. Fix: `mkH ty marker` carries a freshness marker; the descent threads a fork-shaped level (`lvl_start = t t t`, `lvl_next = \l. t l t`) so depth-N tokens are pairwise distinct. Hand-constructed free hypotheses default to leaf-marker; two free Hs of the same type need distinct markers to be distinguishable. **Latent in `checking.disp`'s `check` too** — only `predicates.disp` is fixed, since the existing example tests don't exercise the failing case. Real elaboration (Phase 2) must mint fresh markers.

### What's deliberately not yet built

- **Surface elaborator.** No `\(x : T). body` syntax, no `(x : T) → R`, no auto-computed bind-trees. Programs are hand-constructed via `mkLam`/`mkPi`/`mkApp`. Phase 2 builds the elaborator.
- **Metas / unification.** No `_` holes, no implicit args, no inference of omitted types. Phase 3.
- **Type universe.** No `Type` kind. We're Type:Type and using stand-in atoms (`TyA`, `TyB`); a Type predicate is a one-line addition once we have the kernel form.

---

## Plan: three phases

### Phase 1 — Types-as-predicates kernel ✅ DONE

**Status**: implemented in `examples/predicates.disp` (26 tests, all green; 131 total tree-native tests pass). `pred_of` derives a closed predicate from a tagged type; `check term ty := pred_of ty term`. `mkAtom id` produces an already-callable atom predicate (skips the tagged round-trip; useful for synthesis).

The Phase 1 plan below is preserved as the spec. One adjustment landed during implementation: see lesson 5 in "Sharp lessons earned" — the H-case branch must try direct `fast_eq` before falling back to `type_of_H`-unwrap, otherwise Lam-vs-Pi recursion fails on the canonical hypothesis tokens introduced by splice.

**Goal**: `check term ty` becomes (semantically) `apply(ty, term)`. A type is a callable predicate that returns TT (`leaf`) or FF (`stem(leaf)`).

**Why first**: the elaborator's *output target* depends on this. If we elaborate first against tagged-Pi-as-data and then refactor, every emit-site changes. Refactoring the kernel first locks the contract.

**Two representations, one source of truth**

The bind-tree substrate stays useful — `splice`, structural `fastEq`, accessors. We don't throw it away. We *add* a predicate form derived from it.

```
mkPi A B codom            — tagged form: tagged(KP, fork(fork(A, B), codom)). For splice, fastEq, structural read.
pred_of (mkPi A B codom)  — closed predicate: \candidate. piCheck A B codom candidate.
```

`pred_of` is one-way. The tagged form is canonical (it's what fastEq compares). The predicate form is derived for invocation. Synthesis targets the predicate form (per `GOALS.md`); the elaborator manipulates the tagged form.

**`piCheck` shape** (as implemented; differs from the original spec on H-construction — see lesson 6)

```
piCheck A B codom candidate level :=
  if (is_lam candidate):
    and (fastEq A (lamA candidate))
        (let H = mkH A level in                  -- level threaded through descent
         let body' = splice (lamBody candidate) (lamB candidate) H in
         let codom' = splice codom B H in
         pred_of_lvl codom' body' (lvl_next level))
  else if (is_app candidate):
    -- can't dispatch on App as a "canonical inhabitant" of Pi; defer to infer
    and (pred_of_lvl (piA (infer (appF candidate))) (appX candidate) level)
        (fastEq (normalize (infer candidate)) (normalize (mkPi A B codom)))
  else if (is_h candidate):
    -- direct fastEq first (canonical-hypothesis case from descent), then unwrap
    or (fastEq (normalize candidate) (normalize (mkPi A B codom)))
       (fastEq (normalize (type_of_H candidate)) (normalize (mkPi A B codom)))
  else: FF
```

`pred_of ty cand := pred_of_lvl ty cand lvl_start` where `lvl_start = t t t` (fork-rooted) and `lvl_next = \l. t l t`. Hand-constructed Hs use leaf/stem markers (`Hx = mkH TyA t`); the two namespaces are disjoint so descent and free hypotheses never alias.

Once Pi is callable, `check term ty := apply (pred_of ty) term`. The external `check` shrinks to a one-line dispatcher (or disappears).

**Atomic types as predicates**

For each atomic type T (currently `TyA`, `TyB`, `TyC`):
```
mkAtom id := \candidate. (is_h candidate) && fastEq id (type_of_H candidate)
```
i.e., an atom's predicate accepts H tokens whose annotation is itself. (`type_of_H` extracts only the type, not the freshness marker, so `mkAtom TyA` accepts `mkH TyA t`, `mkH TyA (t t)`, etc. equally.)

**Synthesis hook (forward-looking, not Phase 1)**: post-erase, the predicate form is what neural search optimizes against. Phase 1 produces the predicate; Phase 2's elaborator emits both forms; an `erase` pass (not Phase 1) strips the tagged scaffolding to leave only the predicate.

**Deliverables**:
- `examples/predicates.disp`: `pred_of_pi`, `pred_of_atom`, tests covering TT/FF cases for Lam/App/H candidates against various Pis.
- Refactor `check` to invoke `pred_of`. Keep the structural accessors for splicing/normalization.
- All 105 existing tests still pass through the new path.

**Open question, deferred**: how does the predicate form handle dependent types where the codom is an arbitrary computation? For now, assume codom is a tagged form spliced with the candidate's relevant value; this is what bind-trees + splice already give us. If we hit a case that needs higher-order codom-functions, revisit.

---

### Phase 2 — Surface elaborator ✅ DONE (subset)

**Status**: shipped in `src/elaborate.ts` + `src/parse.ts` + `examples/elab.disp`. The elaborator handles annotated lambdas, non-dep arrows, and dependent Pis. Typed defs `def NAME : T = EXPR` elaborate both sides and run the user-defined `check` from globals at parse time; an FF result throws a parse-time error.

What's working:
- Surface syntax: `\(x : T). body`, `(x : T) -> R`, `A -> B`, plus `elab name = SURFACE_EXPR` and `def NAME : T = EXPR` top-level forms.
- Bind-tree computation: each annotated lambda introduces a fresh `mkH T marker` token in the body; after recursive elaboration, `computeBind` walks the result looking for that token and produces the bind-tree; `replaceMarker` swaps the token for V.
- Encoding compatibility: `src/elaborate.ts` mirrors `examples/predicates.disp`'s tagged-form layout exactly (same TAG_ROOT, kind tags, payload structure), so elaborator output is structurally equal to hand-constructed forms (modulo H freshness markers minted from a counter).
- Free hypotheses (`Hf = mkH (Pi A BN A) t`) defined via `def` work inside `elab` bodies because globals resolve as their literal trees.
- Typed defs run the disp-side `check` (which must be in scope when the `def` parses) and bind the elaborated tagged tree on success. Both positive examples (`def id_AA_typed : A -> A = \(x : A). x`) and negative cases (`def WRONG : A -> A = \(x : B). x` throws) are exercised.

What's deferred to keep Phase 2 minimal:
- A real `Type` universe with consistency. Today there's a `Type` sentinel H token and Type:Type works via hash-cons short-circuit; atoms inhabiting Type are built with plain `def` as `mkH Type marker` (markers must be hand-managed until Phase 3 supplies a fresh-name source).
- Metas (Phase 3).

Closed in this iteration:
- Tree-program port of `compute_bind` and `replace_marker` (`examples/elab.disp`). The host versions in `src/elaborate.ts` are now formally optimizations of the disp-side functions; per philosophy rule 2 the tree version is canonical.

The Phase 2 plan below is preserved as the spec.

**Goal**: write programs in surface syntax with explicit type annotations; the elaborator emits tagged forms with computed bind-trees and runs the kernel from Phase 1 to check.

**Surface additions** (extends `src/parse.ts`):
- `\(x : T). body` — annotated lambda.
- `(x : T) → R` — Pi type with named binder.
- `A -> B` — non-dependent arrow (sugar for `(_ : A) → B`).
- `def name : T = e` — definition with declared type. Triggers a check.
- `Type` — universe (placeholder; for now `Type := mkAtom Type_id` and `Type : Type`).

**Elaboration pass** (new `src/elaborate.ts`, mirroring a future tree-program version):
```
elab : SExpr × Env → (TaggedTerm, TaggedType)

elab(\(x:T). body, env):
  T'           = elab T (expecting Type)
  body', body_type = elab body (env extended with x:T')
  bindtree     = compute_bind body' x
  bindtree_T   = compute_bind body_type x          -- for the Pi codom
  ( mkLam T' bindtree body',
    mkPi  T' bindtree_T body_type )

elab((x:T) → R, env):
  T' = elab T
  R', _ = elab R (env extended)
  bindtree = compute_bind R' x
  ( mkPi T' bindtree R', Type )

elab(f x, env):
  f', f_type = elab f
  x', x_type = elab x
  -- check f_type is a Pi; check x_type matches piA; result type = splice piCodom
  ...

elab(name, env):
  lookup name in env (locals first, then globals)
```

**Bind-tree computation**:
```
compute_bind term varName :=
  case term of:
    V matching varName       → BE
    no occurrence of varName → BN
    App(f, x)                → if either has var, mkBApp (compute_bind f) (compute_bind x); else BN
    Lam(A, _, body)          → similarly mkBLam (compute_bind A) (compute_bind body); else BN
    Pi(A, _, codom)          → mkBPi (compute_bind A) (compute_bind codom); else BN
    other                    → BN
```

Implemented host-side initially per philosophy rule 2 (mirror, with planned tree-program port).

**Top-level pipeline** (extends current `runFile`):
1. Parse `def name : T = e`.
2. `elab T` against `Type` → `T_tagged`.
3. `elab e` in env → `(e_tagged, e_type)`.
4. `check e_tagged T_tagged` (which now invokes `pred_of T_tagged` per Phase 1).
5. If pass: register `name → (e_tagged, T_tagged)` in globals.
6. If fail: report mismatch.

**Deliverables**:
- `src/elaborate.ts` with `elab`, `compute_bind`, `extend_env`.
- Parser extensions (`src/parse.ts`) for the new surface forms.
- `examples/programs.disp` rewriting `id`, K, KI, simple App-of-H programs in surface syntax. Each program gets `def name : T = e` form; elaboration produces tagged forms; kernel checks.
- Cross-validation: the tagged forms produced by elaboration must structurally match what `examples/typing.disp`/`checking.disp` hand-construct.

**Limit**: with no metas, every binder needs an explicit type. `\x. body` with no annotation is rejected (Phase 3 lifts this).

---

### Phase 3 — Metavariables and unification

**Goal**: support `_` holes in surface syntax, implicit args, and bidirectional inference of omitted types via Miller-pattern unification.

This is the genuinely hard phase. The conceptual obstacle (called out earlier): tree calculus has no fresh-name source. Two viable encodings:

**Option A — Positional canonical metas** (recommended)
- A meta is identified by a tree-encoded *path* into the elaboration state.
- The elaboration state mirrors the program structure: at every `_` in surface, a meta entry exists at the corresponding position.
- `mkMeta path := tagged(KM, path)`.
- `lookup_meta state path → Solved t | Unsolved`.
- `solve_meta state path t → state'` (structural rebuild; hash-consing makes this cheap).
- No counter, no fresh-name source. Two metas at the same position would collide; in practice they can't (the program tree has unique positions).

**Option B — Counter via threaded state**
- Threads an integer (Nat-encoded tree) through every elaboration call.
- More flexible (metas can be relocated, duplicated) but the state-passing burden is on every operation.

Recommend A. Less power but kernel stays simple, no metatheory creep.

**Components**:
1. `KM` kind tag; `mkMeta`, `lookup_meta`, `solve_meta` tree-side.
2. Extend `normalize`: solved meta → expand and re-normalize; unsolved → stuck.
3. Extend `infer`: `infer (Meta α) := lookup_meta state α → if Solved t then infer t else type-of-meta-entry`.
4. Extend `check`: at a hole `_`, allocate a meta with the expected type.
5. `unify t1 t2`: try `fastEq` after normalize; on miss, try Miller pattern fragment (one side is a meta-applied-to-distinct-vars, with occurs check + scope check).
6. Elaborator hook: `_` in surface → `mkMeta (current_position)`; bidirectional check fills it.

**Trace example to design against**:
```
def id : (A : _) → A → A = \(A : _). \(x : A). x
```
The `_`s become metas. Elaborator allocates `?1` and `?2` (positions). Checking the body `x` against the codom `A` succeeds via H-lookup; checking the surface `\(x : A). x` against the inferred Pi shape constrains `?1 = Type`, `?2 = Type`. Final elaborated form is the same as hand-constructed `id`.

**Deliverables**:
- Tree-side: meta encoding, lookup/solve, normalize extension, unify with pattern fragment.
- Host-side: elaborator extensions for `_`, implicit-arg insertion.
- `examples/metas.disp` exercising solved/unsolved metas, occurs check, scope check, the polymorphic `id` example end-to-end.

---

## Implementation order and gates

Phase 1 → Phase 2 → Phase 3, sequentially. Within each phase:

1. **Tree-program first** (per philosophy rule 1). New operations land in `examples/*.disp`.
2. **Host mirror** in TS for speed (per rule 2). Each TS function has a tree-program counterpart.
3. **Cross-validation** in tests (per rule 4). The tree-program version is canonical; if host disagrees, host is wrong.

### Gates

- **End of Phase 1**: `(pred_of (mkPi A B C)) (mkLam A B body)` returns TT iff the structural check would have. All current 105 tests still pass through the predicate form. `pred_of_atom` works on atomic types.
- **End of Phase 2**: hand-written examples in `typing.disp`/`checking.disp` are reproducible from surface syntax in `examples/programs.disp`. Elaborator output equals hand-constructed reference (hash-cons identity).
- **End of Phase 3**: polymorphic `id` typeable from surface; one-meta and two-meta unification cases pass; the dependent `(x : A) → x` example works with `_` for `A`.

### Stopping rules

- Any phase that requires a host feature with no declared tree-encoding triggers a redesign or a memo to `DEVELOPMENT_PHILOSOPHY.md` (not silent acceptance).
- Any phase whose tree-program version exceeds the host version's expressive power signals the host has drifted; reconcile before proceeding.
- The kernel size (tree representing the predicate kernel) should stabilize after Phase 1 and not grow through Phases 2-3. If it does, something belongs in the elaborator instead.

---

## Open questions (for later)

1. **η-equivalence**. Currently α/β; no η. May surface in Phase 1 if `pred_of_pi` accepts a non-Lam that's η-equivalent to one. Defer until a concrete example needs it.
2. **Definitional unfolding (δ)** of top-level defs during normalize. Phase 2 has to decide: always unfold (safe, slow), never unfold (fast, breaks conversion), heuristic.
3. **Universe hierarchy**. We're Type:Type. If we ever want consistency, add a level field to Pi. Not on the critical path.
4. **`erase` to runtime SKI tree**. Once predicates and metas are in, we want to compile elaborated trees down to closed runtime trees (per `GOALS.md` and `TREE_NATIVE_TYPE_THEORY.md` synthesis target). Happens after Phase 3.
5. **Self-hosting the kernel**. The eventual target: the kernel from Phase 1, currently a tree program tested via host harness, should bootstrap — i.e., be parseable by an elaborator written using the kernel that checks itself. Not a Phase 1-3 deliverable but the discipline shouldn't drift away from it.
