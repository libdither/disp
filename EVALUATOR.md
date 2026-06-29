# The evaluator subsystem — architecture & backend map

disp's elaborator, verifier, and tests run over a **reduction backend** through one small
`Session` ABI, so the *entire* pipeline is portable: swap the backend, the pipeline is
unchanged. This doc is the map of that subsystem — the contract, the backends, and how they
relate. It's the entry point; the detailed plan, on-disk layout, and per-backend design live
in the linked docs.

> Status is the code, not this doc. For *what's landed*, read `git log`, `npm test`, and each
> backend's source. This file is durable architecture; the milestone history is
> [`EVALUATOR_PLAN.md`](EVALUATOR_PLAN.md) §6.

## The contract: the `Session` ABI

Authoritative in [`src/eval/types.ts`](src/eval/types.ts); rationale in `EVALUATOR_PLAN.md`
§3. A `Session<H>` over an opaque handle type `H`:

- **Term algebra:** `leaf()`, `stem(child)`, `fork(left, right)` — build tree-calculus nodes.
- **Reduction:** `apply(f, x, budget?)` — may return a *suspended* handle on lazy backends;
  the forcing observations below drive it.
- **Interchange:** `loadTernary(s)` / `dumpTernary(h, budget?)` — the one string copy per term
  (preorder `0`/`1`/`2`), byte-identical across backends.
- **Observations (optional):** `equal?`, `classify?` (weak-head inspect), `stats?`,
  `recognizeNative?` (push a native fast-path id, e.g. `tree_eq`).
- **Properties:** `canonicalHandles` (is handle identity O(1) conversion?), `dispose()`,
  a `Budget` that fires `BudgetExhausted` at zero (unit is backend-declared — eager: steps;
  a net: interactions — never compared across backends).

A backend is an `EvalBackend` (`name`, `natives()`, `createSession()`); the **registry**
(`src/eval/registry.ts`) always has `eager`+`naive` and adds a foreign backend only if its
build artifact exists, so `npm test` never needs a toolchain.

## The backends

| Backend | Class | Role | Substrate / strategy | Lives in | Validated by |
|---|---|---|---|---|---|
| **eager** (disp-eager) | TS in-process | the reference **oracle** | hash-consed eager tree reducer | `src/eval/eager.ts` + `src/core/tree.ts` | *is* the spec (canonical) |
| **naive** | TS in-process | honesty backend (flushes canonical-handle assumptions) | structural, no hash-cons identity (`canonicalHandles=false`) | `src/eval/naive.ts` | differential vs eager |
| **rust-eager** | Rust→wasm in-process | the **fast checker backend** (the default when built) | **strategy 1**: hash-consed reducer | `evaluators/rust-eager/` ↔ `src/eval/rust-eager.ts` | differential vs disp-eager |
| **rust-ic-net** | Rust→wasm in-process (+ native parallel) | the **optimizer substrate** — *not* a checker | **strategy 2**: materialized interaction net | `evaluators/rust-ic-net/` ↔ `src/eval/ic-net.ts` | differential vs rust-eager (= a race detector under threads) |
| **lambada peers** (~11) | out-of-process **batch tier** | benchmark contestants + an *external* differential oracle | various lazy/memoizing λ-reducers | `evaluators/lambada/` ↔ `src/eval/lambada.ts` | ternary-ABI differential |

## How they relate — three organizing axes

- **Strategy 1 vs 2** (the deep split, [`RUST_IC_NET_DESIGN.md`](research/interaction-combinator/RUST_IC_NET_DESIGN.md) §0):
  *hash-consing* collapses the interaction net into a tree reducer — O(1) conversion +
  cross-occurrence sharing, wins sequential reduction (disp-eager, rust-eager). *Materializing*
  the net (rust-ic-net) gives up both, on purpose, to buy the two things hash-consing
  structurally cannot: **parallelism** and **per-candidate provenance**.
- **Checker vs optimizer vs benchmark.** The checker wants O(1) conversion ⇒ a hash-consed
  backend (disp-eager / rust-eager). The optimizer wants distinct-candidate provenance ⇒
  rust-ic-net (and it literally *cannot* be a checker backend — its lazy net can't serve the
  elaborator's `classify` protocol). Benchmarking wants many independent reducers ⇒ the
  lambada batch tier.
- **The differential-oracle discipline.** disp-eager is the canonical reference; every other
  backend is validated by reducing the same terms and comparing normal forms. Strong
  confluence (tc-net.typ Theorem 2) makes rust-ic-net's NF *schedule-invariant*, so its
  differential against rust-eager doubles as a parallel **race detector**.

## The calculus underneath

All backends realize the same semantics, specced once in
[`research/interaction-combinator/tc-net.typ`](research/interaction-combinator/tc-net.typ) —
the Rooted TC-Net interaction-net calculus (agents, rules, the `δⁿ` need-duplicator, strong
confluence). It is the evaluator's authoritative spec, the way `TYPE_THEORY.typ` is the type
system's. The hash-consed backends are "just" the standard graph-reduction realization of it;
rust-ic-net is the literal materialization.

## Layout & adding a backend

- **A *project* is the build/vendoring unit; an *evaluator* is the registration unit.** One
  foreign project can ship several evaluators (lambada's `lambada-lazy`, `lambada-memo`, …).
- **On disk:** `src/eval/` is the TS engine layer — `types.ts` (the ABI), `registry.ts`, and
  one thin wrapper per backend (`eager.ts` / `naive.ts` / `rust-eager.ts` / `ic-net.ts` /
  `lambada.ts`). `evaluators/` holds one self-contained folder per *foreign project*
  (`lambada/`, `rust-eager/`, `rust-ic-net/`) — each just a `build.sh` + gitignored
  `artifacts/` (+ a `vendor/` only for multi-file upstreams), **excluded from the TS build**.
  *(Deferred: an optional `src/eval/impl/` reshuffle grouping each evaluator's internals — a
  pure file-move, no functional value.)*
- **Graceful degradation:** the registry registers `eager`+`naive` unconditionally and each
  foreign backend only if its build artifact exists (skip-if-unbuilt), so `npm test` and the
  default loop never need cargo/curl.
- **Adding a backend:** (1) if foreign, `evaluators/<project>/build.sh` pins its upstream →
  `artifacts/`; (2) `src/eval/<name>.ts` exports an `EvalBackend` (implement `Session`, or
  wrap the artifact); (3) one line in `registry.ts`. No shared base to subclass.
- **Shared vs not:** shared is *only* `types.ts` (the ABI) + `registry.ts`; each backend owns
  its own reducer, ternary codec, and FFI mechanism — that's correct, not duplication to fix.

## Where to read what

| You want… | Read |
|---|---|
| this overview + the backend map | **this doc** |
| the ABI in detail / FFI classes / phase history / decisions | `EVALUATOR_PLAN.md` (§3 ABI, §4 FFI classes, §6 phases, §7 decisions) |
| the on-disk layout / how to add a backend | this doc, **§ Layout & adding a backend** (above) |
| the batch-tier (lambada) integration | [`EVALUATOR_LAMBADA_PLAN.md`](EVALUATOR_LAMBADA_PLAN.md) |
| the calculus spec | `research/interaction-combinator/tc-net.typ` |
| the materialized net (strategy 2) design | `research/interaction-combinator/RUST_IC_NET_DESIGN.md` |
| a native backend's internals | its crate `crate/src/lib.rs` module-map doc-comment |
| why this exists (the optimizer it feeds) | [`OPTIMIZER.typ`](OPTIMIZER.typ) |
