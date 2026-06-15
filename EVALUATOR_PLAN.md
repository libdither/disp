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
  Beyond the import list, two helpers read tree *shape* directly
  (`treePairFst`/`isUniverseTree`, `compile.ts:570` — fork pattern-match +
  `force` + signature `treeEqual`); they map cleanly to `classify`+`equal`,
  but they are why phase 0's shape-coupling grep is load-bearing, not a
  formality. This *is* the ABI surface, discovered rather than designed.
- **Evaluation happens during elaboration** (~12 `applyTree` sites in
  `compile.ts`, including typed-binding verification `param_apply typ record`
  at `compile.ts:745`). There is no separate runtime phase; `run.ts` compares
  already-normalized test sides.
- **Evaluator state is module-global** (`applyMemo`, `applyStats`,
  `cacheStats`, the trace buffer, `TREE_EQ_ID`), which is why consumers must
  call `clearApplyCache()`/`resetApplyStats()` defensively — the
  non-transparency this plan eliminates by making state a session object.
  One *elaborator* global deliberately survives sessions: `verifiedModules`
  (`compile.ts:31`), the path-keyed Set that skips re-verifying a module's
  typed exports across `parseProgram` calls. It stores paths, never handles,
  so it stays engine-global — without it, per-file sessions would re-verify
  every kernel module on every file. **Its one bug under `--evaluator` is the
  missing backend dimension**: a module verified on backend A would be skipped
  when re-elaborated on backend B in the same process (exactly the
  conformance-suite scenario). Fix is to key by `(backend.name, path)`, *not*
  to move it onto the session — session-scoping would regress the cross-file
  survival just described. Backend-*name* granularity is correct because
  verification is deterministic per backend name. (More-correct-but-deferred:
  key by content hash of the module record instead of path — closes a latent
  stale-file-mid-process skip and is the shape this takes when `use` goes
  in-language in elaborator Stage 5, a memo over the path→content oracle.)
- **The elaborator-validation tests are a third shape-coupling surface,
  discovered post-audit (Stages 0–3 landed 2026-06-12).** `test/compile.test.ts`,
  `test/desugar.test.ts`, `test/bracket.test.ts` (and `test/apply.bench.ts`)
  import `fork/stem/LEAF/applyTree/force` directly and validate the in-language
  elaborator against the host by **hash-cons `.id` equality**
  (`compile.test.ts:270,274`) plus **id-keyed decode maps**
  (`desugar.test.ts:152,159`). So the `run.ts` name registry is *not* the only
  canonical-handle consumer; these are. They port to the ABI (§5, decision 8):
  `.id ===` → `equal()`, id-keyed maps → `dumpTernary`-string-keyed maps. Phase
  0's grep must cover `test/`, not just `compile.ts`.
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
  Audited `lambada-llc/tree-calculus` directly (2026-06-12): the benchmark
  contract is `run-one.sh <expected> <ternary>...` (left-fold from identity,
  assert NF); the five benchmarks are size / recursive-fib / silly-exp /
  exercise-rules / merge-sort; ternary = preorder arity encoding per their
  `conventions/`. **No evaluator in the ecosystem implements native
  recognition** — their memoizing TS evaluators carry the exact TODO
  ("proactively encode some known trees, so they end up with IDs. Then in
  [apply] one can add fast paths"), sketched, never built. §3.1's convention
  is therefore greenfield, and their `conventions/` directory is the natural
  upstream home to propose it in.
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
- Self-hosting the elaborator (Stages 0–3 landed 2026-06-12 — the **compile
  pipeline** `compile_expr`/`compile_type` (AST→tree: bracket abstraction +
  every desugar + scope resolution) is *already* in-language, host
  `compileExpr` demoted to a validated fast path; only Stage 4 parser + Stage 5
  driver-boundary remain, and the driver/IO stays host forever — see
  `ELABORATOR_PLAN.md`). Three consequences this plan is built around:
  - **"A smaller host that calls into the evaluator" is already the shape, not
    a future one.** Because the compile pipeline is a *tree*, running it on a
    backend is `session.apply(compile_expr_tree, encoded_ast)` — and decision 8
    already ports the `compile`/`desugar`/`bracket` validation tests to do
    exactly that on every session backend. So "disp elaborates on backend X"
    (the phase-6 gate) is, *for the compile step*, **reachable now** — not gated
    on finishing self-hosting. What stays host is the parser (until Stage 4) and
    the driver/IO (TYPE_THEORY §15's one impure boundary — host forever). The
    two plans co-design the same `src/` seams; `ELABORATOR_PLAN.md` §7 sequences
    its Stage 5 against this plan's substrate/evaluator split.
  - **The ABI's elaborate-tier coupling is transitional; its durable core is
    not.** The ~12 host `applyTree` sites are scaffolding for the still-host
    parser/driver; as self-hosting completes they collapse toward a single
    "feed AST → apply the in-language elaborator" crossing. The *durable* ABI —
    what survives to serve the run / bench / native tiers forever — is exactly
    `{leaf, stem, fork, apply, dumpTernary}` + recognition + budget. Polish the
    elaborate-specific surface accordingly (lightly; it dissolves).
  - **Construction chattiness *peaks then collapses* (a transitional risk, §8),
    it does not monotonically worsen.** Mid-migration, host and in-language
    passes interleave and the AST itself crosses as a constructed tree, so
    one-`mk`-per-node construction is most acute *during* the transition; once
    the parser+elaborator are one tree the host feeds a source string to, the
    crossing count collapses to ~one. `loadTernary` batching is the mitigation
    throughout; phase 3 measures where the peak actually lands.

## 3. The Session ABI

```ts
// A backend is a factory; a session owns all evaluator state.
interface EvalBackend {
  readonly name: string
  // Static build-time report: native name -> content hashes (sha-256 over the
  // UTF-8 canonical ternary string) this backend recognizes. A fact about the
  // backend binary — hence here, not on Session. Input to the engine-side
  // compatibility assertion (§3.1); never consulted during reduction.
  natives(): ReadonlyMap<string, readonly string[]>
  createSession(opts?: SessionOpts): Session
}

interface SessionOpts {
  defaultBudget?: number       // in the backend's own unit (steps vs
                               // interactions) — never a shared constant (§8)
  noNativeIntercept?: boolean  // conformance mode: run natives in-language
  trace?: (event: unknown) => void   // replaces the module-global trace buffer
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
  // Forces full normal form — so it takes a budget: under lazy backends THIS
  // is where the deferred reduction actually runs. Every forcing operation
  // (dump, classify, equal) is boundable; otherwise a lazy backend can
  // diverge inside a call that is O(size) on the eager one.
  dumpTernary(h: H, budget?: Budget): string

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
  // (The build-time natives() report lives on EvalBackend.)

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
  classify?(h: H, budget?: Budget): { tag: "leaf" } | { tag: "stem", child: H }
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
// Across an FFI boundary the shared-object mutation doesn't exist: the
// binding passes `remaining` in, gets consumed-back out, and the JS wrapper
// mutates the object — same observable semantics, one extra return value.
```

Notes, mapped to the audit:

- `leaf/stem/fork` replace `LEAF/stem/fork`; `classify` (derived or native)
  replaces `isLeaf/isStem/isFork` + child access and subsumes `force`; `apply`
  replaces `applyTree` (budget now explicit); `equal` replaces `treeEqual`;
  standard-natives recognition (§3.1) replaces `setTreeEqId/getTreeEqId`; `stats` replaces
  `getApplyStats`; session creation/disposal replaces the
  `clearApplyCache/resetApplyStats/resetCacheStats` sprinkles. `prettyTree`
  becomes an engine-side helper over `classify`.
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
  apply trees, serialize trees, recognize natives. Everything observational
  beyond dump (`equal`, `classify`, `stats`) is an optional native override
  of an engine-side derivation, conformance-checked against it.
- **`equal`/`classify` are deliberately *not* promoted to standard natives**
  (unlike `tree_eq`). They are ordinary tree-calculus programs: `classify` is a
  single `triage`, which *is* a primitive reduction rule on every backend, so
  the derivation is one rule-firing + a few boundary crossings — not interpreted
  evaluation, and not slow for the reason it might look. Keeping them in-language
  holds the discipline that only genuine conversion-checking (`tree_eq`) earns
  native recognition; their cost in hot loops is recovered by the optional
  native *override* (`equal?`/`classify?`), never by registry promotion. (This
  closes an earlier review suggestion to register a `classify` tag-extractor: it
  bought little over the override and cost the recognition-purity invariant.)
- **Budgets are per-call with a shared mutable budget object** (current
  `apply` already does exactly this; `applyTree` allocates a fresh 10M-step
  budget per top-level call at `tree.ts:504`, so a budget bounds *one
  reduction*, not cumulative elaboration); the unit is backend-declared
  (steps vs interactions) and reported in stats, never compared across
  backends.
- **A budget is a divergence bound, not a semantic gate — so budget
  *consumption* is an explicitly non-portable observation.** Memo warmth and
  laziness change how many steps a given reduction *spends* (cached
  sub-reductions don't re-charge), never *whether* a total term reaches NF.
  The whole conformance corpus is total, so a generous budget admits every
  valid program on every backend. The only contract: the budget must be set
  loose enough that exhaustion fires on genuine divergence/blow-up, never as a
  tight accept/reject boundary — otherwise acceptance would become
  cache-warmth-relative, violating decision §7. This is *why* session
  lifecycle can be left to the caller (decision 9): the apply memo is a
  per-session perf artifact with no bearing on which programs are accepted.
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
- **Derivation garbage is a backend concern, by construction — which is the
  reason there is no per-handle `free()`.** Derived `equal` allocates two
  partial-application nodes per call (`apply(apply(treeEqDef,a),b)`), and
  conversion checking is the hottest loop in the checker, so on a backend that
  uses derived equality *and* never reclaims, session high-water grows linearly
  in equality-call count, not just term size. The ABI does not solve this; it
  gives backends the two tools to: (1) native `equal?()` allocates zero
  intermediate nodes — the garbage never exists; (2) intra-session GC reclaims
  it wholesale (TC-Net needs reachability GC for parked δⁿ duplicators anyway —
  the same machinery absorbs equal-garbage). The eager backend hash-conses the
  two nodes to ~nothing; only the naive backend (phase 3) accumulates it,
  *intentionally* — pricing the unmanaged case is its job, and phase 3 measures
  arena high-water alongside wall-clock to validate this a-priori reasoning
  rather than assume it.

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
  **arity** + canonical ternary encoding(s) + content hash(es) (sha-256 over
  the UTF-8 ternary string — the algorithm and hashed bytes are pinned in the
  file format itself) + pointer to the in-language spec (`prelude.disp`'s
  `tree_eq`). The canonical tree is a
  compilation artifact, but the repo already treats bracket abstraction as
  part of definitional equality (`lib/elab/bracket.disp` is the spec,
  bit-identically validated) — pinning the tree makes existing policy
  explicit. The registry may carry multiple canonical encodings per native
  during encoding transitions.
- Backends bake the registry in at build time and intercept recognized trees
  at saturated application. **The obligation is extensional**: applying (a
  tree equal to) a registry tree to its arity's worth of arguments yields the
  same NF as running the definition — partials included; *how* a backend
  recognizes is its own business. Saturation is not one event for an n-ary
  native: the reference backend is two-stage — `apply(tree_eq, a)` returns the
  hash-consed suspension `susp(tree_eq, a)` (`tree.ts:414`), a first-class
  value that intercepts on meeting its second operand (`tree.ts:404`) and
  forces to the genuine reduct if examined as data (`tree.ts:445`), so
  intensionality survives partials too. Conformance pins the corners: a
  `tree_eq a` partial stored/passed as a value, triage over a partial, triage
  over the definition itself.
- **The registry doubles as the engine's bootstrap source.** At session
  creation the engine `loadTernary`s each registry encoding, so `treeEqDef`
  exists as a handle *before any source compiles* — resolving the ordering
  hazard where derived `equal` (which routes through the tree_eq tree) is
  needed while elaborating the very prelude that defines `tree_eq`
  (`isUniverseTree`, module auto-verify). When the prelude binding named
  `tree_eq` compiles, the engine asserts its hash (below) and thereafter
  **substitutes the session's registry handle for the binding**, so every
  downstream reference embeds the handle the backend already knows. Under
  hash-consing the substitution is the identity (same canonical node —
  today's behavior exactly); for a non-hash-consing backend it makes
  session-path recognition O(1) with zero per-node hashing (tag one handle
  at load). Blob loading recognizes during `loadTernary` (substring match or
  bottom-up hash) — backend's choice.

**`noNativeIntercept` (conformance mode) splits the machinery by purpose:**

- *Session-creation `loadTernary` of the registry: unconditional.* Derived
  `equal` needs `treeEqDef` to *exist* during prelude bootstrap regardless of
  the flag; under the flag it then runs that definition for real instead of
  intercepting it — which is exactly the behavior conformance validates.
- *Hash assertion: always runs, both clauses.* Clause (a) — compiled `tree_eq`
  hash ∈ registry — catches prelude/bracket-abstraction drift, independent of
  whether interception fires. Clause (b) — hash ∈ `backend.natives()` — is a
  static capability fact that stays true under the flag. One dump+hash at
  prelude-compile; never gated.
- *Binding substitution: gated with interception* (`noNativeIntercept` ⇒ no
  substitution). Conformance mode means "run the actual compiled definition,
  unmodified." Substitution is provably identity under a passing hash
  assertion, so gating it changes nothing observable on a conformant build —
  but it makes conformance exercise the *real* compiled artifact rather than
  the registry stand-in, which is the entire reason the flag exists.

Net invariant: `noNativeIntercept` turns off *all* native-recognition machinery
(substitution + interception), leaving only the always-on configuration
assertion and the unconditional bootstrap load.

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
   (b) the hash is in `backend.natives().get("tree_eq")` — guards against a
   stale or mismatched backend.
   Either failure is a loud configuration error before any reduction runs —
   never a silent performance cliff.

The handshake is deliberately indirect: engine and backend each conform to
the committed registry independently, and the engine verifies both
conformances by hash. Nothing about "which tree is tree_eq" flows between
them at runtime.

What recognition buys beyond a smaller ABI: **exported blobs are
self-contained and fast-pathable everywhere.** The definition is inline (it
always was the spec), and any evaluator that adopts the convention
intercepts — including batch-tier CLIs. The native-substitution machinery
that `format/export.ts` would have needed is deleted; there is no difference
between a blob for a session backend and a blob for the batch tier.
(Ecosystem reality, audited 2026-06-12: **no external evaluator implements
any native recognition today** — lambada's memoizing evaluators carry the
exact TODO, "proactively encode some known trees, so they end up with IDs.
Then in [apply] one can add fast paths" — so the fast path initially exists
only on disp backends; elsewhere blobs run the inline definition, correct
and merely slower. The convention is ours to define;
`lambada-llc/tree-calculus`'s `conventions/` directory is the natural place
to propose it upstream.)

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
  payloads. **What actually crosses (the FFI is numbers-only):** WASM exports
  take/return i32/i64 only, so `leaf/stem/fork/apply` are `u32 → u32` exports
  over arena indices — numbers cross *free*, no marshalling. The only byte-copy
  is `loadTernary(ptr,len)` / `dumpTernary → (ptr,len)`: JS encodes the string,
  calls an exported allocator, `memcpy`s into linear memory, and calls in (dump
  is the mirror — read `memory.buffer.subarray(ptr,ptr+len)`, `TextDecoder`,
  free). That is **once per term, never per node** — which is the entire sizing
  argument. `dispose()` drops the `WebAssembly.Instance`, and the JS GC reclaims
  its backing `ArrayBuffer` wholesale: the session-arena / no-per-handle-`free()`
  model is not a convention here, it is *literally how WASM memory works*.
- Same artifact runs in node and browser; no platform build matrix; sandboxed
  memory makes arena bugs non-fatal to the host.
- Threads: WASM threads + SharedArrayBuffer exist but are operationally
  painful; a parallel TC-Net backend may prefer §4.3. A *sequential* TC-Net
  WASM backend is still a perfectly good first conformance target.
- **Laziness is TC-Net's core feature, not an optimization — the inverse of the
  reference backend.** Its headline result (Theorem 6 at-most-once reduction =
  family-optimal sharing *with no oracle*, `tc-net.typ` §Work-Sharing) comes
  *entirely* from `δⁿ` call-by-need; run TC-Net eagerly (fire-everything + `δˢ`)
  and you keep correctness but discard the only reason to build it (`δˢ` shares
  structure, never work). So where the reference backend treats laziness as an
  optional susp/`wait` layer, TC-Net treats demand-driven scheduling as
  load-bearing — which is why its `wait`-dissolving and `tree_eq`-O(1)-losing
  properties (decisions 1, 7) are intrinsic to it, not incidental.
- **TC-Net status, and how it meets the registry.** The calculus-level design
  is complete in `research/interaction-combinator/tc-net.typ` (rooted
  construction; `P` suspension agents; δˢ/δⁿ duplicator species; strong
  confluence; Theorem 6 at-most-once reduction — need-sharing is
  family-optimal with no oracle). What remains are *implementation*
  decisions, not design ones: scheduler (δⁿ requires demand-driven
  scheduling for completeness — fire-everything speculates on shared
  arguments), reachability GC for parked δⁿ duplicators (the erasure leak;
  the ABI's session-arena + `dispose()` model absorbs it — garbage
  accumulates per session and is reclaimed wholesale), and arena layout.
  The §3.1 recognition story transfers *unchanged*, because the reference
  backend's two-stage susp scheme was modeled on TC-Net's `P` node in the
  first place: stage 1 leaves the inert hash-consed partial `P(tree_eq, a)`
  exactly as `susp` does, and both stages are O(1) canonical-id checks
  inside the A-dispatch rule against the node built by `loadTernary` of the
  registry encoding at session init (binding substitution makes every
  compiled reference share that node). tc-net.typ's hash-consing note
  already covers `P(f,a)` nodes and already describes native equality as a
  *normalizer consumer* + pointer equality of canonical NFs — that consumer
  is precisely decision 2's strictness-point/parallelism-barrier caveat.
  Intensional transparency is free: interception hooks only the A-dispatch
  rule, and a partial demanded by triage flows through ordinary `P` demand,
  exposing the genuine reduct.

### 4.3 N-API native addon (escape hatch for parallel)
Same handle scheme over `napi-rs`; calls ~100 ns; full OS threads inside the
addon (rayon work-stealing reducer runs freely; `apply`/`equal` block the JS
thread or expose async variants — engine is synchronous today, so blocking is
acceptable). Cost: per-platform builds. Choose only if/when the parallel
TC-Net backend exists and WASM threading proves inadequate.

### 4.4 Subprocess session (rejected for *per-op* elaboration)
A line protocol (`MK`/`APPLY`/`EQ`/`CLASSIFY` with integer handles over stdio)
can implement the ABI, but elaboration performs ~10⁴–10⁶ boundary calls per
file at ~µs–ms per round trip — orders of magnitude over budget. Kept out of
scope; revisit only as a debugging curiosity. **The exception is a
*fat-message* subprocess** — whole job in, result out — which sidesteps per-op
chatter entirely, but only becomes possible once the parser+elaborator are
self-hosted (Stage 4): then the subprocess runs the in-language pipeline on a
source string and returns one result tree. That is the convergence point where
out-of-process elaboration stops being latency-prohibitive, and where the
in-process and subprocess tiers collapse to the *same* fat-message shape (§4.6).
Until then it is the batch tier only (§4.5).

### 4.5 Batch tier (lambada ABI peers)
External CLIs (`lambada-tc` implementations, `tc-evaluator`, any future
standalone) are `BatchRunner`s: one-shot stdin ternary terms → left-fold →
ternary NF, declared in `bench/evaluators.json`. They serve **benchmarks and
differential conformance only**, never elaboration. `bench/adapters/disp-eager.ts`
(~40 lines over the reference session: `loadTernary` each line, fold `apply`,
`dumpTernary`) makes disp a contestant in *their* harness too.

### 4.6 Which backends are actually any good (candid)

Mapped to what each must *implement* and what it buys. The ABI was shaped for
linear-memory backends on purpose (opaque handle ⇒ `u32` arena index; no
per-handle `free()` ⇒ `dispose()` = instance/arena drop; `dumpTernary` as the
sole observation ⇒ the one thing that crosses as *bytes*), so the native rows
are glue, not rewrites.

| Backend | FFI surface to write | Marshalling cost | Verdict |
|---|---|---|---|
| **TS eager (reference)** | none — handle = `Tree` pointer | zero | Mandatory baseline; phase-1 state-izing is the only work. |
| **TS naive (phase 3)** | none | zero | Not a "real" backend — a conformance/honesty fixture, deliberately slow. Good at its one job: flushing canonical-handle assumptions, pricing derived-`equal` garbage. |
| **Rust→WASM, sequential** | `leaf/stem/fork/apply` as `u32→u32` exports + `loadTernary(ptr,len)`/`dumpTernary→(ptr,len)` over linear memory + an exported allocator | numbers free (~10–50 ns/call); strings = 1 alloc + 1 memcpy + 1 free *per term* | **The real first native target.** One artifact (node+browser), sandboxed, synchronous (matches the engine), `dispose()` = drop the instance (= the session-arena model, for free). Recommended. |
| **Rust→WASM, threaded** | above + SharedArrayBuffer + Atomics + the threads proposal | same + cross-origin isolation (COOP/COEP) in browsers | Good for parallel, operationally painful. Defer until the sequential backend is green and parallelism is the proven bottleneck. |
| **Rust N-API (rayon)** | same handle scheme over `napi_value`; real OS threads inside the addon | ~100–200 ns/call (napi type-tagging); strings copy through napi | **Narrow.** Justified *only* by parallelism WASM threads can't deliver. Cost: per-platform prebuilt matrix, node-only. "If/when," per decision 5. |
| **Subprocess, per-op** | a line protocol (`MK`/`APPLY`/`EQ`/`LOAD`/`DUMP`, int handles over stdio) | µs–ms *per call* | **Bad for elaboration** (10⁴–10⁶ calls/file → seconds–hours of pure IPC). Correctly rejected (§4.4). |
| **Subprocess, fat-message** | one-shot: whole job (source) in, result tree out | one round trip | **Becomes good for elaboration once self-hosted** (Stage 4): per-node chatter that kills §4.4 disappears. This is where in-process and out-of-process tiers *converge*. Today it is the batch tier (§4.5). |
| **GPU (CUDA/WebGPU)** | marshal the net to device memory, run, marshal back | PCIe transfer per job | **Aspirational / inherently batch.** TC-Net's GPU-parallel endgame (shared interaction-combinator substrate with HVM2), but per-op `apply` across the device boundary is a non-starter — GPU backends are fat-message only. Not a near-term Session backend; the long-term *reason* TC-Net exists. |

Cross-cutting calls:

- **Only strings marshal; numbers are free.** Per-`apply` is one int→int call
  wrapping millions of internal firings; the only byte-copy is one
  `loadTernary`/`dumpTernary` per *term*. Construction (one `mk` per AST node) is
  the lone chatty exception, and `loadTernary` exists to fold it into a single
  string crossing — the §8 risk to measure, not assume.
- **Language: Rust.** It hits WASM *and* N-API from one codebase, gives arena
  memory safety, and the `tc-net.typ` design is already Rust-flavored. Zig is a
  fine WASM-only alternative; C/C++ only if a dependency forces it. **Avoid GC'd
  languages** (Go/Java/C#) for the arena backend — their GC fights the
  session-arena model and bloats the WASM artifact. AssemblyScript (TS→WASM)
  tempts with one-language-everywhere but its GC/perf doesn't earn its keep.
- **The honest near-term ladder** is exactly three rungs: state-ized TS eager
  (phase 1) → naive TS (phase 3, the ABI-is-real proof) → sequential Rust→WASM
  TC-Net (phase 6). Threaded WASM, N-API, and GPU are all *conditional on the
  sequential one paying off first* — none is on the critical path.

## 5. Repo re-org

```
src/
  core/cir.ts        # (if split from parse) bracket abstraction IR — no tree dep
  eval/types.ts      # EvalBackend / Session / Budget / EvalStats / errors
  eval/eager.ts      # the reference backend (current tree.ts evaluator, state-ized)
  eval/naive.ts      # phase-3 honesty backend (no consing, no memo, canonicalHandles:false)
  eval/registry.ts   # name -> EvalBackend; --evaluator resolution
  eval/derived.ts    # engine-side equal/classify/prettyTree derivations over
                     # {leaf,stem,fork,apply,dump} — the careful forcing code
                     # the lazy-backend risk (§8) demands live in ONE place
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
  eval-conformance.test.ts   # full lib/tests x every session backend;
                             # native-vs-derived agreement; recognition corners
  compile.test.ts   ┐        # PORT to Session (decision 8): .id === -> equal(),
  desugar.test.ts   ├─         id-keyed decode maps -> dumpTernary-string-keyed.
  bracket.test.ts   ┘          Host-expected crosses backends via the ternary
                             # bridge (reference dump -> target loadTernary ->
                             # equal); they then join the conformance matrix.
  apply.bench.ts             # rehome alongside bench/ (it benches the eager
                             # backend's apply) — §5 previously omitted it
  tree.test.ts               # stays on core/tree.ts (tests one backend's internals)
```

`src/core/tree.ts` (hash-consed `Tree`, `susp`/`force`) survives as the
**private internals of `eval/eager.ts`** — no longer exported to the engine.
That is the concrete meaning of "tree_eq O(1) is just this backend's
optimization."

## 6. Phases

**Phase 0 — pin behavior (½ day).** Golden snapshot of outputs + `applyStats`
for the eval-steps corpus. Grep audit of remaining tree-shape couplings —
**`compile.ts` *and* `test/`** (the elaborator-validation tests `.id`/id-map
couplings are the post-audit surface, §1) — so `equal`-based replacements and
the `dumpTernary`-keyed map rewrites are enumerated up front.

**Phase 1 — state-ize the evaluator (1 day).** Module globals →
`EagerSession`; `clearApplyCache`/reset sprinkles deleted; consumers create
sessions. Outputs and verdicts byte-identical against phase 0; stats and
node ids shift in known ways (session-fresh arenas vs today's never-cleared
globals) and are re-baselined. (Standalone value even if the plan stops
here.)

**Phase 2 — ABI extraction (2–3 days).** `eval/types.ts`; `compile.ts` and
`run.ts` rewritten against `Session` (mechanical: the import list in §1 maps
1:1); `setTreeEqId` → recognition per §3.1 — `conventions/natives.json`, the
engine-side hash assertion, and binding substitution land **here**, not in
phase 4 (recognition is load-bearing for elaboration and the registry file
is small; phase 4 keeps only the export/batch tooling); `core/tree.ts`
de-exported from the engine. `verifiedModules` re-keyed to `(backend.name,
path)` (§1, decision 9). The elaborator-validation tests port to the ABI
(decision 8): `.id ===` → `equal()`, id-keyed decode maps →
`dumpTernary`-string-keyed. Tests green via the reference backend.

**Phase 3 — honesty backend + conformance (1–2 days).** A deliberately naive
second TS backend (`eval/naive.ts`: no hash-consing, no memo, structural
`equal`, `canonicalHandles: false`). Its native `equal` (per decision 2)
is a plain recursive walk. Run the full `lib/tests` suite on it. This is the
cheap proof that the ABI is real, that nothing secretly depends on canonical
handles or O(1) equality, and it prices hash-consing directly (the existing
`treeEqRules` counter gives the equality-call volume the O(n) walk must
absorb). Measure reference-backend memo hit rates first: if no-memo is
wall-clock infeasible (the checker re-walks types heavily), fall back to a
memoized-naive variant — memoization is not a canonical-handle assumption —
and/or a corpus subset; the point is flushing out hidden assumptions, not
asceticism. **Measure arena high-water, not just wall-clock** (decision/§3
reclamation bullet): the naive backend uses derived `equal`, so its garbage
growth prices the no-native-`equal`, no-GC corner directly and validates that
no *required* ABI path forces unbounded growth on a backend that cannot help
it. Becomes the permanent conformance fixture.

**Phase 4 — ternary + export + batch tier (1–2 days).** `format/`,
`emit` CLI, binding-closure export (the registry + hash assertion landed in
phase 2). Export-conformance tests: exported blob
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
   gate. *Note the precise trade a demand-driven backend makes* (per TC-Net's
   own impl note, `tc-net.typ` §Hash-Consing): it **keeps** hash-consing —
   structure, including `P(f,a)` suspensions, stays shared — but loses hash-cons
   id *as the conversion check*: with live suspensions, `id ≠ id` no longer
   implies `≠`, so equality becomes demand-then-compare (O(min size)), not an
   O(1) id check. The reference backend keeps the O(1) path by staying eager.
   The ABI is exactly what lets eager-fast-conversion and demand-driven
   work-sharing coexist without forking the language.
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
   both, with the conformance suite carrying the correctness burden.) *This is
   closer than it looks:* the compile pipeline is **already in-language**
   (Stages 0–3), so "run elaboration on backend X" is "apply the in-language
   `compile_expr` tree" — host `compileExpr` stays the reference-backend fast
   path under the same spec/fast-path discipline as `tree_eq`, and decision 8's
   ported tests already exercise this on every backend (§2, non-goal 3).
4. **`loadTernary`/`dumpTernary` are required, not optional** — they are both
   the FFI anti-chattiness path and the interchange format; making them
   optional would fork the engine into two code paths.
5. **WASM before N-API** for native backends.
6. **Batch tier stays one-shot** (no subprocess session protocol).
7. **The reference backend is normative for program acceptance — *temporarily*,
   and as a strategy choice, not a semantic one.** There is *one* confluent
   semantics (a unique normal form); eager and demand-driven are *strategies*
   over it. The reference backend's eager compile-time normalization
   (`cirToTree` fully reduces closed redexes) is what makes *acceptance*
   strategy-dependent: a demand-driven backend defers redexes the eager one
   force-reduces, so it accepts strictly more programs — precisely the
   `select_lazy`-hazard class, the η/saturation rule, and the Appendix-A
   S-duplication OOM, **all of which are eager-strategy taxes, not properties of
   disp.** (`wait`/`fix` are the same: idioms that build an *unfired* redex
   because the reference strategy is eager; under a demand-driven backend
   deferral is the default and they are redundant no-ops — though they cannot be
   deleted from the sources while the eager backend is normative, since it still
   needs them.) So "valid disp" is held to the eager strategy *for now* because
   that is what exists and its acceptance is decidable-cheap — **not** because
   eagerness is fundamental. The crux that would retire this clause is
   `ELABORATOR_PLAN.md` Appendix A.3.2: *can compile-time normalization be made
   demand-driven (WHNF/lazy) without breaking the canonical-normal-form
   invariant that `tree_eq` conversion depends on?* If yes, the workaround
   classes dissolve and acceptance stops being strategy-relative; until then, a
   program that elaborates only on a demand-driven backend is out of contract.
   Conformance ("lib/tests green on backend X") tests one direction only.
8. **The elaborator-validation tests port to the ABI** (`compile`/`desugar`/
   `bracket`), trading `.id` equality for `equal()` and id-keyed decode maps
   for `dumpTernary`-string-keyed maps. They are not pinned to the reference
   backend: porting makes them run on every session backend through the ternary
   bridge, so the in-language elaborator — the component most wanted on TC-Net —
   is conformance-validated there too. (The alternative, leaving them on raw
   `tree.ts`, would keep a hidden canonical-handle dependency the plan claims to
   have removed and would never exercise the in-language elaborator off the
   reference backend.)
9. **Session lifecycle is the caller's; budgets are divergence bounds, not
   semantic gates.** No "one session per X" is baked into the ABI. `run.ts`
   defaults to one session per `runFile` (matching today's `clearApplyCache`
   cadence), but callers may hold longer-lived per-backend sessions for memo
   warmth. This is safe *because* budget consumption is a non-portable
   observation (§3): a generously-budgeted total program is accepted on every
   backend regardless of cache state, so acceptance never becomes
   lifecycle-relative. `verifiedModules` is the one cross-session fact, and it
   stays engine-global keyed by `(backend.name, path)` (§1) — not session-scoped,
   which would regress cross-file survival.

## 8. Risks

- **FFI chattiness in construction-heavy elaboration** (one boundary call per
  AST node before reduction starts). Mitigated by `loadTernary` bulk paths and
  by measuring phase-3 call counts before building any native backend; if
  construction dominates, add a bulk `loadExprBatch`.
- **Budget-unit drift**: 10M compile steps was tuned for the eager backend
  with native tree_eq; other backends need per-backend budget configuration,
  not a shared constant (put it in `SessionOpts`). Decision 9 contains the
  blast radius: budgets bound divergence, not acceptance, so the risk is a
  total program *spuriously rejected* by a too-tight per-backend budget — a
  configuration error, not a semantic one. Set budgets loose; never compare
  consumption across backends.
- **Hidden canonical-handle assumptions** beyond the known `run.ts` name
  registry — exactly what phase 3's naive backend exists to flush out.
- **Naive-backend wall-clock *and arena***: losing the memo *and* hash-consing
  together may push the full suite from seconds to hours, making phase 3's
  "1–2 days" optimistic — hence the measure-first / memoized-naive fallback
  written into phase 3. The same backend's derived-`equal` garbage (two
  partial-app nodes per conversion check, never reclaimed) makes session
  high-water grow in equality-call count; phase 3 measures it to confirm it is
  a backend-owned concern (native `equal` or intra-session GC erase it — §3
  reclamation bullet) rather than an ABI defect.
- **Ternary blowup on shared blobs**: ternary dump expands every shared
  subtree at every occurrence, and compiled modules are heavily shared DAGs
  (hash-consing is what makes them small in memory) — a kernel-checker blob
  (the phase-5 flagship) may be orders of magnitude larger than its DAG.
  The lambada `conventions/` already define a let-style DAG text format for
  exactly this. Measure blob sizes in phase 4; if they explode, add optional
  `loadDag`/`dumpDag` bulk ops as the sharing-preserving interchange —
  ternary stays the required lowest common denominator (it is what the
  batch tier speaks; their evaluator binaries read ternary only, DAG lives
  in their converter tool). DAG cannot replace ternary: it is not canonical
  (one tree, many encodings — orderings, sharing choices), and canonicality
  is load-bearing for the registry hash and the `dump`-comparison
  observation bridge. If hashable DAGs are ever needed, pin canonical DAG =
  first-visit preorder numbering of the maximally-shared graph (for a
  hash-consing backend, that is literally a dump of the reachable
  hash-cons table).
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
