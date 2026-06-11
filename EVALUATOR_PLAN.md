# Evaluator Backends Plan

Make the evaluator a **fully portable dependency of the elaborator**: the
TypeScript elaboration pipeline (itself slated to move to native disp
eventually) consumes an evaluator through a small session ABI — a state object
the engine passes around, `apply`, term construction/inspection, and budgets —
so that *the entire pipeline* (elaboration, verification, tests) runs on any
backend: the current eager TS evaluator, a Rust/WASM TC-Net runtime, or
anything else implementing the ABI. Out-of-process evaluators speaking the
lambada ternary ABI remain supported as a batch tier for benchmarking.

Status: PLAN — for review. Nothing here is implemented.

---

## 1. Current state (audit)

Verified against the code 2026-06-11:

- **The elaborator's entire evaluator coupling is one import list.**
  `compile.ts` imports `{ Tree, LEAF, stem, fork, applyTree, treeEqual, force,
  getApplyStats, setTreeEqId, getTreeEqId }` from `tree.js`; `run.ts` adds
  `prettyTree` and the cache/stats reset functions; tests add the same resets.
  Nothing else inspects trees. This *is* the ABI surface, discovered rather
  than designed.
- **Evaluation happens during elaboration** (~12 `applyTree` sites in
  `compile.ts`, including typed-binding verification `param_apply typ record`
  at `compile.ts:745`). There is no separate runtime phase; `run.ts` compares
  already-normalized test sides.
- **Evaluator state is module-global** (`applyMemo`, `applyStats`,
  `cacheStats`, the trace buffer, `TREE_EQ_ID`), which is why consumers must
  call `clearApplyCache()`/`resetApplyStats()` defensively — the
  non-transparency this plan eliminates by making state a session object.
- **`tree_eq`'s O(1)-ness is an implementation property, not a contract.**
  The kernel's conversion checking needs *decidable structural equality of
  normal forms* — which tree calculus provides natively via the in-language
  `tree_eq` program. Hash-consing + the native fast-path make it O(1) in the
  reference backend; a backend without them runs the in-language `tree_eq` at
  O(min size) or implements `equal` however it likes. Correctness is
  unaffected; only constants change.
- **Rule-set compatibility with the lambada ecosystem is confirmed** (same
  5-rule triage calculus), and their CLI contract (stdin ternary terms,
  left-fold apply, print ternary NF) is the de-facto batch interchange format.
- `susp`/`force` exist in `tree.ts` — the reference backend is already
  internally lazy-capable; under the ABI, suspension becomes a backend-internal
  concern.

## 2. Goals and non-goals

**Goals**
1. A `Session` ABI (§3) such that `parseProgram`/`runFile` take a session and
   never touch a concrete tree representation. Selecting
   `--evaluator=<backend>` runs **elaboration + verification + tests** on that
   backend.
2. Backend-owned state: sessions encapsulate memo tables, arenas, stats,
   native-recognition tables. No module-global evaluator state anywhere.
3. An FFI story (§4) for in-process native backends (WASM first, N-API if
   needed) with bulk operations so boundary chattiness doesn't dominate.
4. Ternary serialization + `emit` CLI for interchange with the batch tier and
   the benchmark harness.
5. A conformance suite that runs the full `lib/tests` corpus on every session
   backend, plus randomized differential terms across batch evaluators.
6. `bench/` harness comparing all backends and external repos on shared
   workloads (lambada's five + disp-authored sharing/laziness/kernel-checker
   benchmarks).

**Non-goals**
- Building the TC-Net evaluator itself (this plan defines its socket).
- Subprocess backends serving *elaboration* (latency-prohibitive; see §4.4 —
  they remain batch-tier).
- Self-hosting the elaborator (but the ABI is chosen to survive it: once the
  elaborator is a disp program, it runs *on* the backend and the host shrinks
  to backend + driver, per GOALS.md).

## 3. The Session ABI

```ts
// A backend is a factory; a session owns all evaluator state.
interface EvalBackend {
  readonly name: string
  createSession(opts?: SessionOpts): Session
}

type H = unknown   // opaque handle, owned by its session

interface Session {
  // ── term algebra ──
  leaf(): H
  stem(child: H): H
  fork(left: H, right: H): H

  // ── computation ──
  // May return a suspended handle under lazy backends; dump/equality force.
  apply(f: H, x: H, budget?: Budget): H

  // ── bulk ops (required; the anti-chattiness path and the interchange path) ──
  loadTernary(s: string): H
  dumpTernary(h: H): string          // forces full normal form

  // ── standard natives (recognition — see §3.1) ──
  // Backends ship knowing the standard-natives registry (name -> canonical
  // tree, pinned by content hash; today only "tree_eq") and MUST intercept
  // saturated applications of any recognized tree with a native
  // implementation bit-identical to running the definition; the in-language
  // tree remains the spec. Equality semantics: structural equality of normal
  // forms in the backend's own representation (id check, recursive walk,
  // normalize-and-compare — backend's choice). Recognition is free under
  // hash-consing (canonicalization makes the prelude-compiled def
  // pointer-equal to the baked-in tree); one bottom-up hash at construction
  // otherwise. Interception hooks APPLICATION ONLY, at saturation: a
  // recognized definition examined as data (triaged over by reflective code)
  // presents its real structure — intensionality is never bypassed.

  // Static build-time report: native name -> content hashes (of the
  // canonical ternary encoding) this backend recognizes. Input to the
  // engine-side compatibility assertion (§3.1); never consulted during
  // reduction, and independent of anything the elaborator compiles.
  natives(): ReadonlyMap<string, readonly string[]>

  // ── capabilities (optional) ──
  // Native equality shortcut. DERIVABLE: the engine applies the standard
  // tree_eq definition — recognized and intercepted natively by obligation,
  // so even the derived path runs native equality — and dumps the tiny
  // constant TT/FF result: equal(a,b) ≡ dump(apply(apply(treeEqDef,a),b))
  // === dump(TT). 3 boundary crossings + two partial-application nodes vs 1.
  equal?(a: H, b: H, budget?: Budget): boolean
  // Native weak-head inspection. DERIVABLE in-language: classify is one
  // triage step — tag = apply(△(△ tagL (K tagS)) (K(K tagF)), h), the tiny
  // constant tag tree read back via dump (or derived equal); children
  // projected by triage{_,I,_} / triage{_,_,K} / triage{_,_,K∘I}. The engine
  // ships that derivation as a session-generic helper (~3-5 boundary calls +
  // garbage terms per node); backends override natively for performance.
  // Same spec/optimization split as tree_eq: the derivation is the spec,
  // conformance checks native-vs-derived agreement. Forces WHNF under lazy
  // backends either way (applying triage IS the demand).
  classify?(h: H): { tag: "leaf" } | { tag: "stem", child: H }
                | { tag: "fork", left: H, right: H }
  stats?(): EvalStats                // rule/interaction counts, steps, peaks
  // True iff handles are canonical (a === b implies equal(a,b)); engines may
  // use this only as an optimization gate (e.g. run.ts name registry).
  readonly canonicalHandles: boolean

  // End-of-life: free everything the session owns (WASM instance / native
  // arena / threads). Near no-op on the TS reference backend (GC suffices);
  // load-bearing for native backends, whose arena memory the JS GC cannot
  // see. Implement [Symbol.dispose] so call sites can `using session = ...`.
  dispose(): void
}

interface Budget { remaining: number }   // steps (sequential) or interactions
                                         // (nets); BudgetExhausted on zero
```

Notes, mapped to the audit:

- `leaf/stem/fork` replace `LEAF/stem/fork`; `classify` (derived or native)
  replaces `isLeaf/isStem/isFork` + child access and subsumes `force`; `apply`
  replaces `applyTree` (budget now explicit); `equal` replaces `treeEqual`;
  standard-natives recognition (§3.1) replaces `setTreeEqId/getTreeEqId`; `stats` replaces
  `getApplyStats`; session creation/disposal replaces the
  `clearApplyCache/resetApplyStats/resetCacheStats` sprinkles; `equal`
  (derived or native) replaces `treeEqual`. `prettyTree` becomes an
  engine-side helper over `classify`.
- **`dumpTernary` is the sole irreducible observation primitive.** Handles
  are opaque, so at least one operation must bridge handle-space to
  JS-value-space for the engine to branch on results; that bridge is dump's
  string. Everything else is derivable from
  `{leaf, stem, fork, apply, dump}`: engine-side equality
  routes through the standard `tree_eq` tree — recognized and intercepted
  natively by obligation, so even the derived path runs native equality,
  paying only boundary overhead — with the tiny TT/FF result dumped and compared
  host-side; `classify` via a triage step whose constant tag trees are
  likewise dumped. The required core is therefore exactly: build trees,
  apply trees, serialize trees, register natives. Everything observational
  beyond dump (`equal`, `classify`, `stats`) is an optional native override
  of an engine-side derivation, conformance-checked against it.
- **Budgets are per-call with a shared mutable budget object** (current
  `apply` already does exactly this); the unit is backend-declared
  (steps vs interactions) and reported in stats, never compared across
  backends.
- **Laziness is admitted by contract**: `apply` may be O(1) and suspended
  (TC-Net builds a `P` node); `classify`/`dumpTernary`/`equal` force. The
  eager reference backend simply always returns WHNF+ trees. Divergence
  differences between eager and lazy backends surface as budget/timeout
  differences on non-total terms — the conformance corpus is total.
- **Handle identity is not semantics.** Only `canonicalHandles` backends
  permit `===` shortcuts; the engine treats it as an optimization gate
  (today's only use: the `run.ts` tree-id → name registry for pretty errors,
  which degrades to numbered placeholders on non-canonical backends).
- **Determinism**: results are deterministic on all backends (confluence;
  strong confluence for nets). Stats and scheduling need not be (parallel
  backends), except interaction *counts*, which strong confluence fixes.
- **The session is the unit of memory reclamation — there is deliberately no
  per-handle `free()`.** The engine never tracks liveness of intermediate
  trees; backends arena-allocate and `dispose()` drops everything wholesale.
  Cost: a session's high-water mark is its total allocation — acceptable for
  per-file elaboration sessions, and exactly how the current evaluator
  behaves anyway (the module-global memo grows until `clearApplyCache()`;
  dispose is that, made explicit and scoped). Keeps refcounting out of the
  FFI entirely.

### 3.1 Standard natives: recognition

Backends know the standard native definitions statically, rather than being
told per session. (The alternative — a runtime call where the engine hands
each session its compiled `tree_eq` handle — is worse on every axis that
matters here: it adds a per-session protocol step for information that is
static, it excludes evaluators the engine never gets to call — the batch
tier — which would force an export-substitution step and fork the blob
format, and it ties interception to a handshake instead of to the committed
convention that already pins these trees.)

The convention:

- A committed registry (e.g. `conventions/natives.json`): native name →
  canonical ternary encoding(s) + content hash(es) + pointer to the
  in-language spec (`prelude.disp`'s `tree_eq`). The canonical tree is a
  compilation artifact, but the repo already treats bracket abstraction as
  part of definitional equality (`lib/elab/bracket.disp` is the spec,
  bit-identically validated) — pinning the tree makes existing policy
  explicit. The registry may carry multiple canonical encodings per native
  during encoding transitions.
- Backends bake the registry in at build time and intercept recognized trees
  at saturated application (recognition cost: zero under hash-consing, one
  bottom-up hash at node construction otherwise).

**How the compatibility assertion works** (who provides what): matching is by
content hash of the canonical ternary encoding — representation-independent,
so no handles ever cross the boundary for this.

1. The *registry* pins, per native name, the blessed hashes.
2. The *backend* reports via `natives()` which registry hashes it was built
   recognizing. This is a static fact about the backend binary; it does not
   consult the elaborator and cannot be wrong about itself.
3. The *engine*, after compiling the prelude, dumps its own compiled
   `tree_eq` (a small tree) and hashes the ternary string — this is "what has
   been set as tree_eq" in the current build. It then asserts:
   (a) the hash is in the registry — guards against prelude or
   bracket-abstraction drift relative to the convention;
   (b) the hash is in `session.natives().get("tree_eq")` — guards against a
   stale or mismatched backend.
   Either failure is a loud configuration error before any reduction runs —
   never a silent performance cliff.

The handshake is deliberately indirect: engine and backend each conform to
the committed registry independently, and the engine verifies both
conformances by hash. Nothing about "which tree is tree_eq" flows between
them at runtime.

What recognition buys beyond a smaller ABI: **exported blobs are
self-contained and fast-pathable everywhere.** The definition is inline (it
always was the spec), and any evaluator that recognizes it intercepts —
including batch-tier CLIs. The native-substitution machinery that
`format/export.ts` would have needed is deleted; there is no difference
between a blob for a session backend and a blob for the batch tier.

Is anything else registration-shaped needed? `tree_eq` is the only live
native. Plausible future natives — nat arithmetic on the binary encoding, a
revived native `param_apply` dispatcher (the legacy kernel had one; per
CLAUDE.md it returns only with an equivalence test) — fit the same convention
once their definitions stabilize. A fast-churning definition would suffer
registry churn, which is an argument for stabilizing it before pinning, not
for dynamic registration.

## 4. FFI per backend class

The crucial sizing fact: the ABI boundary is **per term-operation, not
per reduction step**. One `apply` call may run millions of internal rule
firings; FFI overhead amortizes over them. The chatty direction is term
*construction* (one `mk` per node), which `loadTernary` batches.

### 4.1 TypeScript in-process (reference: `eval/eager.ts`)
Handle = `Tree` object pointer. Zero-cost calls. Work: move module-global
memo/stats/trace/`TREE_EQ_ID` into an `EagerSession` instance (pure hygiene,
fixes the transparency mess regardless of the rest of this plan).
`canonicalHandles: true` (hash-consing retained as this backend's internal
optimization — exactly the demotion requested).

### 4.2 WASM (Rust/C compiled; the intended TC-Net path)
- Handle = `u32` index into instance-owned arena; session = WASM instance +
  handle table; `dispose()` drops the instance.
- Calls are synchronous, ~10–50 ns overhead each — negligible against `apply`
  payloads; `loadTernary`/`dumpTernary` cross the boundary once per term as
  UTF-8 in linear memory.
- Same artifact runs in node and browser; no platform build matrix; sandboxed
  memory makes arena bugs non-fatal to the host.
- Threads: WASM threads + SharedArrayBuffer exist but are operationally
  painful; a parallel TC-Net backend may prefer §4.3. A *sequential* TC-Net
  WASM backend is still a perfectly good first conformance target.

### 4.3 N-API native addon (escape hatch for parallel)
Same handle scheme over `napi-rs`; calls ~100 ns; full OS threads inside the
addon (rayon work-stealing reducer runs freely; `apply`/`equal` block the JS
thread or expose async variants — engine is synchronous today, so blocking is
acceptable). Cost: per-platform builds. Choose only if/when the parallel
TC-Net backend exists and WASM threading proves inadequate.

### 4.4 Subprocess session (rejected for elaboration)
A line protocol (`MK`/`APPLY`/`EQ`/`CLASSIFY` with integer handles over stdio)
can implement the ABI, but elaboration performs ~10⁴–10⁶ boundary calls per
file at ~µs–ms per round trip — orders of magnitude over budget. Kept out of
scope; revisit only as a debugging curiosity.

### 4.5 Batch tier (lambada ABI peers)
External CLIs (`lambada-tc` implementations, `tc-evaluator`, any future
standalone) are `BatchRunner`s: one-shot stdin ternary terms → left-fold →
ternary NF, declared in `bench/evaluators.json`. They serve **benchmarks and
differential conformance only**, never elaboration. `bench/adapters/disp-eager.ts`
(~40 lines over the reference session: `loadTernary` each line, fold `apply`,
`dumpTernary`) makes disp a contestant in *their* harness too.

## 5. Repo re-org

```
src/
  core/cir.ts        # (if split from parse) bracket abstraction IR — no tree dep
  eval/types.ts      # EvalBackend / Session / Budget / EvalStats / errors
  eval/eager.ts      # the reference backend (current tree.ts evaluator, state-ized)
  eval/registry.ts   # name -> EvalBackend; --evaluator resolution
  format/ternary.ts  # engine-side encode/decode helpers (over classify/mk)
  format/export.ts   # closure of a binding into one self-contained blob
                     # (defs inline by construction; no substitution — §3.1)
  parse.ts           # unchanged
  compile.ts         # takes a Session; all tree ops via ABI
  run.ts             # takes/creates a Session; --evaluator flag
bench/
  evaluators.json    # batch-tier peers (external repos, by path)
  programs/          # disp-authored benchmarks + frozen lambada blobs + expected
  artifacts/         # generated ternary (gitignored)
  harness.ts         # sessions + batch runners x benchmarks; wall, counts,
                     # PASS/FAIL, thread sweep; absorbs bench/eval-steps.ts
  adapters/disp-eager.ts
test/
  eval-conformance.test.ts
```

`src/core/tree.ts` (hash-consed `Tree`, `susp`/`force`) survives as the
**private internals of `eval/eager.ts`** — no longer exported to the engine.
That is the concrete meaning of "tree_eq O(1) is just this backend's
optimization."

## 6. Phases

**Phase 0 — pin behavior (½ day).** Golden snapshot of outputs + `applyStats`
for the eval-steps corpus. Grep audit of remaining tree-shape couplings in
`compile.ts` (per the elaborator-consumes-names memory) so `equal`-based
replacements are enumerated up front.

**Phase 1 — state-ize the evaluator (1 day).** Module globals →
`EagerSession`; `clearApplyCache`/reset sprinkles deleted; consumers create
sessions. Behavior byte-identical against phase 0. (Standalone value even if
the plan stops here.)

**Phase 2 — ABI extraction (2–3 days).** `eval/types.ts`; `compile.ts` and
`run.ts` rewritten against `Session` (mechanical: the import list in §1 maps
1:1); `setTreeEqId` → `registerNative("tree_eq", def)`; `core/tree.ts`
de-exported from the engine. Tests green via the reference backend.

**Phase 3 — honesty backend + conformance (1–2 days).** A deliberately naive
second TS backend (`eval/naive.ts`: no hash-consing, no memo, structural
`equal`, `canonicalHandles: false`). Its native `equal` (per decision 2)
is a plain recursive walk. Run the full `lib/tests` suite on it. This is the
cheap proof that the ABI is real, that nothing secretly depends on canonical
handles or O(1) equality, and it prices hash-consing directly (the existing
`treeEqRules` counter gives the equality-call volume the O(n) walk must
absorb). Becomes the permanent conformance fixture.

**Phase 4 — ternary + export + batch tier (1–2 days).** `format/`,
`emit` CLI, binding-closure export, `conventions/natives.json` registry + the
engine-side hash assertion (§3.1). Export-conformance tests: exported blob
normalized by the reference backend equals the in-repo result, and a
recognizing backend intercepts `tree_eq` inside the blob. `BatchRunner` +
`evaluators.json` + disp-eager adapter; randomized differential test across
batch peers.

**Phase 5 — bench harness (2–3 days).** Matrix runner, per-benchmark
timeouts, median+min wall, counts column (per-backend units, labeled), thread
sweep, dated logs; programs = five frozen lambada blobs + disp-authored ports
+ sharing/laziness benchmarks (the δⁿ experiments from
`research/interaction-combinator/tc-net.typ`) + the kernel-checker flagship
(`verify` of a real module via `emit`). Delete `bench/eval-steps.ts` after
absorption.

**Phase 6 — first native backend (separate effort).** Sequential TC-Net in
Rust→WASM implementing `Session` (§4.2); parallel version later via WASM
threads or N-API (§4.3). The conformance suite from phase 3 is its
acceptance gate: full `lib/tests` green ⇒ disp elaborates on an interaction-net
substrate.

Total for phases 0–5: ~1.5–2 weeks. Each phase lands green and is
independently revertable.

## 7. Decisions requiring sign-off

1. **Handles are opaque; hash-consing is demoted** to a private optimization of
   the reference backend; the `tree_eq` interception is the equality contract
   (decidable structural equality of NFs), `canonicalHandles` the optimization
   gate.
2. **`tree_eq` interception is a backend obligation via recognition, not
   registration (§3.1).** Every session bakes in the standard-natives registry
   and intercepts recognized trees at saturated application; the engine
   asserts registry agreement at prelude-compile time (loud error, never a
   silent perf cliff). The in-language `tree_eq` stays the spec: conformance
   validates each backend's native against the reference backend with
   recognition disabled (`SessionOpts.noNativeIntercept` exists for exactly
   this). Note for lazy/parallel backends: native equality is a strictness
   point and a parallelism barrier (both sides forced to full NF) —
   semantically identical to the in-language program, but it will show in
   profiles. Export substitution is deleted: blobs carry definitions inline
   and any recognizing evaluator (batch tier included) fast-paths them.
3. **The whole pipeline runs on the selected backend** — including
   typed-binding verification. There is no longer a privileged
   elaboration evaluator. (The alternative — pinning elaboration to the host
   evaluator and making only test execution pluggable — would need a special
   deferred-compilation seam for test spines and would leave verification
   untestable on other backends; running everything on the session avoids
   both, with the conformance suite carrying the correctness burden.)
4. **`loadTernary`/`dumpTernary` are required, not optional** — they are both
   the FFI anti-chattiness path and the interchange format; making them
   optional would fork the engine into two code paths.
5. **WASM before N-API** for native backends.
6. **Batch tier stays one-shot** (no subprocess session protocol).

## 8. Risks

- **FFI chattiness in construction-heavy elaboration** (one boundary call per
  AST node before reduction starts). Mitigated by `loadTernary` bulk paths and
  by measuring phase-3 call counts before building any native backend; if
  construction dominates, add a bulk `loadExprBatch`.
- **Budget-unit drift**: 10M compile steps was tuned for the eager backend
  with native tree_eq; other backends need per-backend budget configuration,
  not a shared constant (put it in `SessionOpts`).
- **Hidden canonical-handle assumptions** beyond the known `run.ts` name
  registry — exactly what phase 3's naive backend exists to flush out.
- **Lazy-backend forcing discipline**: `classify` in a loop can accidentally
  deep-force; engine helpers (`prettyTree`, ternary encode) must be written
  once, carefully, in `format/`.
- **Performance regression of the demotion itself**: none expected for the
  default path — the reference backend keeps hash-consing internally; only the
  engine's *access* to it changes.
- **Standard-native drift**: the canonical `tree_eq` tree is a compilation
  artifact — evolving bracket abstraction or the prelude changes it, and a
  stale backend stops recognizing. Correctness is unaffected (the in-language
  definition runs), but conversion checking falls off a performance cliff.
  Mitigated by the engine-side hash assertion + `natives()` report (loud
  mismatch, never silent) and by multi-version registry entries during
  transitions.
- **CLAUDE.md drift**: phases 1, 2, 5 each end with a layout/discipline
  update (`src/tree.ts` is documented prominently today; the "native
  fast-path" discipline section gains the standard-natives recognition
  framing).
