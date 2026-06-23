# TC-Net evaluator — implementation plan

A second disp evaluator backend: **Rooted TC-Net**, a sequential (then parallel)
interaction-net runtime in Rust→WASM, implementing the `Session` ABI so the whole
pipeline (elaboration + verification + tests) can run on an interaction-net
substrate. This is `EVALUATOR_PLAN.md` Phase 6, made concrete.

Status: **PLAN + M0 SCAFFOLD landed.** The crate skeleton
(`evaluators/tc-net/crate/`), `build.sh`, and the Session wrapper
(`src/eval/tcnet.ts`) exist with the full FFI surface pinned and every engine
body `todo!()`. The backend registers only once `build.sh` produces the artifact,
so the default loop needs no Rust toolchain. Milestones below fill it in.

## Source docs (authoritative, read first)

- `research/interaction-combinator/tc-net.typ` (845 lines) — **the calculus
  design.** Agents, the demand/application/dispatch/sharing rules + summary
  matrix, the `δⁿ` demand-before-copy variant, the hash-consing/equality note,
  the correctness/strong-confluence/work-sharing theorems, and §Open Questions.
  `tc-net.pdf` renders it. Section references below are to this file.
- `EVALUATOR_PLAN.md` §4.2 (WASM FFI sizing), §4.6 (why Rust→WASM is the real
  first native target), **decision 1** (hash-consing kept, O(1) conversion lost),
  **decision 7** (eager strategy normative — the acceptance-divergence risk),
  Phase 6 (conformance gate).
- `ELABORATOR_PLAN.md` Appendix A.3.2 — the open crux decision 7 hinges on
  (can compile-time normalization be demand-driven without breaking the
  canonical-NF invariant `tree_eq` depends on?).
- `src/eval/types.ts` — the `Session`/`EvalBackend` ABI being implemented.

## Why TC-Net (the payoff)

Tree-calculus terms are trees; interaction nets duplicate trees while preserving
constructor identity; triage dispatches on constructor identity — so
self-reflection composes with explicit parallel sharing **without a binding
oracle**. The headline (Theorem 6, §work-sharing): with the call-by-need
duplicator species `δⁿ`, **every application is dispatched at most once** —
family-optimal sharing, no labels/brackets/oracle — because binderlessness means
no redex acquires two residuals. That is the sharing λ-calculus needs Lamping's
full apparatus for, and it is the reason to build a second backend rather than
only optimize the eager one. Downstream it is the parallel/GPU substrate the
`GOALS.md` optimizer needs (`research/interaction-combinator/DISP_BACKPROP.typ`).

## What already exists (the socket TC-Net drops into)

1. **The ABI** (`src/eval/types.ts`): `leaf/stem/fork/apply/loadTernary/dumpTernary`
   (+ optional `equal/classify/stats/recognizeNative`, `canonicalHandles`,
   `dispose`). That is the entire contract.
2. **The acceptance gate**: the conformance corpus. `test/eval-naive-elaborate.test.ts`
   already proves a *non-canonical, structural-equality* backend can drive real
   elaboration to a verdict — TC-Net inherits that de-risked path (the naive
   backend exists precisely to flush canonical-handle assumptions).
3. **The benchmark**: `bench/bench-evaluators.ts` — TC-Net becomes a contestant
   the moment its CLI exists; cold-subprocess `reduce_ms`, oracle validation, and
   `STACK`/`TIMEOUT`/`DNF` classification are already in place.
4. **Recognition is not a blocker** (§3.1 re-grade): TC-Net rides the
   `recognizeNative` hook like every backend; recognition-by-hash is a later knob.
5. **The §3.1 susp scheme was modeled on TC-Net's `P` node** (EVALUATOR_PLAN
   §4.2) — so the eager backend's two-stage suspension already mirrors the
   construction TC-Net implements natively.

## Architecture

### Agents & arena (§Agents, §Encoding)

Producers are stored nodes (`evaluators/tc-net/crate/src/lib.rs::Node`):
`L` (leaf), `S(x)` (stem), `F(a,b)` (fork), and `P(f,a)` — a **suspended
application**, the rooted-construction root, reduced on demand. Consumers are
reduction steps, not stored kinds: `A` (apply), `T₁`/`T₂` (the two-level triage
dispatch), `δˢ`/`δⁿ` (structural / need duplicators), `ε` (erase). A WASM instance
owns exactly one arena (`Vec<Node>` + side tables); handles are `u32` indices.

### Reduction rules (§Demand/Application, §Dispatch, §Sharing)

- **Demand & application**: demanding `P(f,a)` makes `f` active against an `A`
  carrying `a`; `A ⊗ L/S/F` enters dispatch.
- **Two-level dispatch** (§Dispatch + §Summary Matrix): the tree-calculus 5 rules
  as `A`→`T₁`→`T₂` interactions on the operator's constructor — this is where
  binderless directness beats λ-bookkeeping.
- **Sharing** (§Sharing Rules): `δ` propagates through producers preserving
  constructor identity (a duplicated fork is a fork…). This invariant is what
  makes self-reflection compatible with sharing.
- **Erase**: `ε` deletes a producer, recursing into children (`P` erased without
  evaluating — laziness of discard).

### Hash-consing & the three equalities (§Implementation Note)

Nodes are interned by `(tag, child ids)`, so structural `δ` copy returns a shared
ref instead of deep-copying (copy realized lazily through sharing). Three
equalities: **raw** (canonical syntax incl. suspended `P`), **weak-head** (demand
each side to WH form, compare exposed constructor), **normal-form** (a recursive
normalizer consumer, then pointer-compare canonical NFs). `Session.equal` maps to
NF-equality. **Crucially (decision 1):** with live suspensions, `id ≠ id` no
longer implies `≠`, so equality is *demand-then-compare* (O(min size)), **not**
the eager backend's O(1) id check. Hence `canonicalHandles = false`.

### Call-by-need: `δⁿ`, scheduler, GC (§demand-before-copy, §Costs of δⁿ)

The structural `δˢ` shares *structure*, never *work* (Prop 5: residuals of one
redex reduce twice). The one-rule repair `δⁿ` **demands** a duplicated `P` and
parks itself on the result wire → call-by-need as an interaction net (Theorem 6:
at-most-once). The S-rule spawns `δⁿ`. Two species never meet principal-to-
principal, so **no coherence oracle**. Costs `δⁿ` imposes (the M1 obligations):

- **Demand-driven scheduler** — completeness requires it once `δⁿ` is live (a
  fire-everything scheduler speculates on shared args). The scheduler choice now
  affects the *work profile*, not just copy-wave timing.
- **Reachability GC** — a parked `δⁿ` whose two copies are both erased is
  unreachable by `ε` (it sits on auxiliary wires) → a lazy interaction-net
  evaluator needs reachability GC. The session-arena + `dispose()` absorbs the
  *per-session* leak; long sessions need intra-session GC.
- **Full normalizer** — the core is weak-head; `dumpTernary`/`equal` need full NF,
  so add a recursive consumer that demands constructor children (§Open Q #2).

## ABI mapping (Session method → WASM export)

The FFI is **numbers-only except one string copy per term** (EVALUATOR_PLAN §4.2).
Exports are pinned in `crate/src/lib.rs`; the wrapper is `src/eval/tcnet.ts`.

| Session | export | notes |
|---|---|---|
| `leaf/stem/fork` | `tc_leaf/tc_stem/tc_fork` | `u32 → u32`, numbers cross free |
| `apply` | `tc_apply(f,x,budget) -> u32` | `u32::MAX` (−1 in JS) ⇒ budget exhausted → host throws |
| `loadTernary(s)` | `tc_alloc(len)` + memcpy + `tc_load_ternary(ptr,len)` + `tc_free` | the one byte copy in; parses preorder 0/1/2 (byte-identical to disp) |
| `dumpTernary(h)` | `tc_dump_ternary(h,budget) -> u64` | returns `(ptr<<32)\|len`; host reads `memory`, decodes, `tc_free`. Forces full NF |
| `equal(a,b)` | `tc_equal(a,b,budget) -> 0/1` | demand-then-compare |
| `classify(h)` | `tc_classify(h,budget)` + `tc_child0/1` | 0/1/2 tags; forces WHNF |
| `dispose()` | drop the instance | arena's `ArrayBuffer` GC'd wholesale — the no-per-handle-free model is literally how WASM memory works |

Budget unit is **interactions** (backend-declared; non-portable — never compared
to the eager backend's steps, decision 9). The exact JS sign of the `u32::MAX`
exhaustion sentinel (`tc_apply` returns i32 → JS Number is signed ⇒ −1) is pinned
in M0.

## The acceptance risk to settle first (decision 7)

disp's **eager backend is normative for program acceptance**, and acceptance is
currently *strategy-relative*: eager compile-time normalization fully reduces
closed redexes, so a demand-driven backend accepts strictly more programs (the
`select_lazy`/η-saturation/S-duplication hazard classes are **eager-strategy
taxes, not properties of disp**). Therefore:

- **Target conformance parity first.** "Full `lib/tests` green via
  `--evaluator=tcnet`" tests one direction (everything eager accepts, TC-Net
  accepts) — the well-defined M0/M1 gate.
- The deeper question (can compile-time normalization itself become demand-driven
  without breaking the canonical-NF invariant `tree_eq` needs — `ELABORATOR_PLAN`
  A.3.2) is **out of scope** for these milestones; it is what would *retire* the
  eager-normative clause, not a TC-Net deliverable.

Secondary, expected: the **distribution-cost gap** (§What Is Still Not Shared) —
bracket-abstracted binders still expand to Θ(|body|) dispatch interactions per β;
`δⁿ` removes re-reduction, not distribution. So design the sharing benchmarks to
exhibit work-sharing wins, not binder-heavy workloads.

## Milestones

### M0 — sequential, `δˢ`-only, get to conformance parity

Implement the arena + hash-consing + the 5-rule two-level dispatch + structural
`δˢ`/`ε` + the full normalizer for `dumpTernary` + ternary parse/serialize +
`tc_alloc/free`. Eager-ish scheduling is fine here (no `δⁿ` yet). Wire `cli`
parity so it joins the benchmark + a subprocess differential.
- **Gate:** `--evaluator=tcnet` runs full `lib/tests` green (the
  `eval-naive-elaborate` template, generalized); differential vs disp-eager +
  the 11 lambada evaluators in `bench/bench-evaluators.ts` (add `tcnet` as a
  contestant); a `test/eval-tcnet.test.ts` mirroring `eval-lambada` (skipIf the
  artifact is unbuilt).
- **Outcome:** disp elaborates on an interaction-net substrate. Correct, not yet
  work-sharing.

### M1 — `δⁿ` call-by-need + demand-driven scheduler + reachability GC

Add the second duplicator species (S-rule spawns `δⁿ`), the demand-driven
scheduler completeness requires, and reachability GC for parked duplicators.
- **Gate:** conformance still green; the sharing benchmarks (the `δⁿ` experiments
  — encode a shared suspended computation duplicated into two strict positions,
  per Prop 5) show `δⁿ` performing the shared work **once** vs `δˢ` twice, and
  TC-Net beating disp-eager on sharing-heavy inputs. Report interaction counts in
  `stats()`.

### M2 — parallel

Parallelize the reducer (strong confluence (Theorem 2) makes interaction *counts*
deterministic, so conformance is unaffected). Two routes (EVALUATOR_PLAN §4.3):
WASM threads + SharedArrayBuffer + Atomics (one artifact, browser needs
COOP/COEP), or an N-API addon (`napi-rs`, rayon work-stealing — per-platform
builds, node-only). Choose WASM-threads first; N-API only if threading proves
inadequate.
- **Gate:** conformance green under the parallel scheduler; a thread-sweep column
  in the benchmark shows speedup on parallelizable workloads.

### (M3 — GPU, deferred)

The HVM2-substrate endgame (§Open Q #4, EVALUATOR_PLAN §4.6 GPU row) — inherently
fat-message/batch, not a near-term Session backend. Out of scope for now.

## Testing & benchmarking

- **Conformance** is the gate at every milestone: full `lib/tests` through the
  ABI on `--evaluator=tcnet`. Reuse the `eval-naive-elaborate` pattern; the naive
  backend already proved nothing depends on canonical handles / O(1) equality.
- **Differential**: `test/eval-tcnet.test.ts` (skipIf-gated) cross-checks TC-Net
  against disp-eager on the lambada benchmark programs + randomized small terms —
  the same shape as `test/eval-lambada.test.ts`.
- **Benchmark**: add `tcnet` to `bench/bench-evaluators.ts` (a cold-subprocess
  `cli` mirroring `bench/disp-cli.ts`); the δⁿ sharing programs become new entries.

## Layout

```
evaluators/tc-net/            foreign project (own toolchain; not in the TS build)
  crate/                        the Rust evaluator
    Cargo.toml                    cdylib ⇒ wasm32
    src/lib.rs                    arena + agents + C-ABI exports (M0 scaffold = todo!())
  build.sh                      rustup target add wasm32 + cargo build → artifacts/tcnet.wasm
  artifacts/tcnet.wasm          gitignored build output
src/eval/tcnet.ts             Session<number> wrapper (WASM instance, u32 handles); registry skips if unbuilt
```

(Deferred tidy: the `src/eval/impl/` reshuffle in `EVALUATOR_LAYOUT.md` — pure
file-moves with broad import churn, no functional value for TC-Net, so done when
convenient, not as part of this work.)

## Open questions (from tc-net.typ §Open Questions, scoped to this effort)

- **Scheduler design** — the central M1 runtime decision (demand-driven firing).
- **Reachability-GC strategy** — the known cost of lazy interaction-net evaluators.
- **Lévy-optimality formalization** — theory only; not on the impl path.
- **Typed/linear TC-Net** — linear values need no `δ`; a later optimization.
- **HVM2 extension vs from-scratch** — we chose from-scratch sequential Rust→WASM;
  HVM2-extension stays the GPU-parallel (M3) option.
