# TC-Net evaluator — implementation plan

A second disp evaluator backend: **Rooted TC-Net**, a sequential (then parallel)
interaction-net runtime in Rust→WASM, implementing the `Session` ABI so the whole
pipeline (elaboration + verification + tests) can run on an interaction-net
substrate. This is `EVALUATOR_PLAN.md` Phase 6, made concrete.

Status: **M0 LANDED — FULL `lib/tests` elaboration GREEN on the interaction-net
backend, and ~2× FASTER than the eager V8 backend.** Running the whole corpus
through `--evaluator=tcnet`: **all 816 tests pass, in ~58 s vs eager's ~176 s**
(~3× — initially 85 s, brought to 58 s by a perf pass: an inline FxHash-style
hasher on the intern/memo tables + a sparse `forced` map). disp's elaborator +
verifier + tests run on a Rust→WASM interaction-net substrate, and the Rust-vs-V8
constant factor beats eager outright — sequential, no parallelism. (Exact ratios
drift with the live std/kernel edit churn; eager and tcnet agree on identical
source.) `tc_apply` ships **eager** (the
elaboration-conformance mode — EVALUATOR_PLAN decision 7: the elaborator is
eager-normative); the iterative explicit-stack reducer + the `tree_eq` two-stage
fast-path (`recognizeNative`) make it competitive. The **M1 lazy** core
(`tc_apply_lazy` = `Susp`; `force`/`nf` = `δⁿ` at-most-once via the `forced` memo)
is kept and validated by the reduction differential (`test/eval-tcnet.test.ts`: 5
lambada programs + 50 random terms) + the direct laziness check (`K a (silly-exp
6)` = 2 interactions vs 13 304 to force the discarded arg).

**Five backend-compat fixes were needed for full elaboration** — none a reduction
bug (the engine matches eager on 4000-term differentials); all from tcnet being the
first **number-handle + deep-reduction** Session backend (the naive backend uses
Tree *objects*, so it never flushed these):
1. **Falsy handle** — compile.ts tests handles with `entry?.tree ?` (truthiness);
   a `0` LEAF handle reads as "absent" → spurious "unresolved free variable". Fix:
   reserve handle 0 as null, `LEAF = 1` (no valid handle is JS-falsy).
2. **Stack overflow** — native recursion dies on the kernel's deep reduction spines.
   Fix: ported `src/core/tree.ts`'s **iterative explicit-stack** `apply` machine.
3. **No `tree_eq` fast-path** — in-language `tree_eq` (O(size)) made kernel verify
   heavy. Fix: `recognizeNative` + the two-stage susp interception (O(1) compare).
4. **Budget over-charge** — tcnet charged budget for leaf/stem; eager charges only
   fork-dispatches. Fix: aligned.
5. **Per-call budget** — even aligned, a kernel verification call exceeds eager's
   40 M (`APPLY_BUDGET`, eager-tuned); budget is a non-portable per-backend bound
   (decision 9 / §8). Fix: tcnet floors its per-call budget at 4 G interactions.

The backend registers only once `build.sh` produces the (gitignored) artifact, so
the default loop needs no Rust toolchain. **Remaining: M2** (parallelism +
materialized agents + the modal-typed scheduler).

Toolchain note: `build.sh` works with rustup *or* a standalone/Nix toolchain that
ships the `wasm32-unknown-unknown` std (it discovers a `wasm-ld` when `rust-lld`
is absent). `.cargo/config.toml` sets a 256 MiB wasm stack (the iterative reducer
no longer recurses along the spine; only `nf`/`equal` recurse, on tree depth).

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

### Implementation strategy: hash-consed reducer (M0/M1) vs materialized graph (M2)

tc-net.typ specifies the *semantics* (agents, ports, active pairs) — it does not
mandate the data structures. Two strategies realize it, and the choice decides
which of the §Costs-of-`δⁿ` obligations are even incurred:

1. **Hash-consed tree reducer** — the eager backend's lineage (`src/core/tree.ts`).
   Producers are hash-consed nodes (`L`/`S`/`F`/`P`=`susp`); the consumers
   (`A`/`T₁`/`T₂`/`δ`/`ε`) are the *control flow* of a reduction function, not
   stored agents. Then `δˢ` = returning a shared reference = **free** (it *is*
   hash-consing), and `δⁿ` at-most-once = a `forced` memo cell on the shared `P`
   node (force once, memoize, every reference reads it). Sequentially this yields
   the full work-sharing of `δⁿ` with **no materialized duplicators, no parked-
   agent reachability-GC, and no active-pair queue** — they collapse into
   hash-consing + memoized forcing + ordinary heap reachability. This is precisely
   what the eager backend already is (`susp` *is* `P`; the apply loop *is* the
   `A`/`T₁`/`T₂` dispatch; `tree.ts:30`).
2. **Materialized graph machine** — HVM-style. Agents and ports are real arena
   nodes; active pairs sit in a work queue; `δⁿ` is a genuine parked agent on an
   auxiliary wire. This is what M2 (parallel) **requires** — reduction can't be
   distributed across threads without explicit, schedulable active pairs — and it
   is what *creates* the reachability-GC and scheduler obligations the plan lists.

**Decision: M0 and M1 take strategy (1); M2 switches to (2).** Consequence: the
demand-driven scheduler and reachability GC that §Costs-of-`δⁿ` attaches to "M1"
are *strategy-(2)* costs. Under strategy (1), M0 is a faithful **port** of
`src/core/tree.ts` (apply/force/treeEqual/encode/decode) + arena + budget + the
FFI seam (already pinned in `tcnet.ts`/`lib.rs`), and M1 is a **bounded** change —
make `apply` lazy (WHNF, standard call-by-need forcing) + add the recursive
normalizer — with GC deferred to grow-until-`dispose()` (sound for per-file
sessions). The materialized machine and its open *parallel* scheduler are
correctly deferred to M2, where parallelism forces them and the modal-type lever
(M2 note) makes the scheduling optimal rather than heuristic.

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

### M0 — sequential, `δˢ`-only, get to conformance parity  ✅ LANDED

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

### M1 — `δⁿ` call-by-need  ✅ LANDED (hash-cons forced-memo, not materialized agents)

**As built:** under the hash-consed reducer strategy (§Implementation strategy),
`δⁿ` call-by-need is realized *without* the materialized parked-duplicator,
demand-driven *parallel* scheduler, or reachability GC the formal net would need —
those are M2 costs. `tc_apply` returns `Susp(f,x)` (O(1)); `force` drives WHNF on
demand and memoizes every visited `Susp` in its `forced` cell (`δⁿ` at-most-once,
Theorem 6); hash-consing makes structurally-identical `Susp`s share one cell
(cross-occurrence global memoization, the eager backend's `applyMemo` equivalent).
Sequential lazy forcing is the "scheduler"; grow-until-`dispose()` is the GC.
- **Gate:** ✅ conformance green (`test/eval-tcnet.test.ts`); laziness verified —
  a discarded suspended computation is never forced (`K a (silly-exp 6)` = 2
  interactions vs 13 304); `stats()` reports interaction counts via
  `tc_interactions`. Deferred: the Prop-5 dual-strict-position sharing benchmark +
  a bench contestant (`disp-cli` wrapper) — measurement, not capability.

### M2 — parallel

Parallelize the reducer (strong confluence (Theorem 2) makes interaction *counts*
deterministic, so conformance is unaffected). Two routes (EVALUATOR_PLAN §4.3):
WASM threads + SharedArrayBuffer + Atomics (one artifact, browser needs
COOP/COEP), or an N-API addon (`napi-rs`, rayon work-stealing — per-platform
builds, node-only). Choose WASM-threads first; N-API only if threading proves
inadequate.
- **Gate:** conformance green under the parallel scheduler; a thread-sweep column
  in the benchmark shows speedup on parallelizable workloads.

#### The work/span trade-off — why *optimal* M2 scheduling depends on modal types

The M2 scheduler must resolve a real tension, not a tuning knob. Call-by-need
(`δⁿ`) is **work-optimal** (every application dispatched at most once, Theorem 6)
but its demand chain *serializes* — it throttles available parallelism (high
span). Fire-everything (`δˢ`, HVM2-style) is **low-span** but *wastes work*:
it speculatively reduces discarded subterms and diverges on
discardable-but-divergent ones (§Costs of `δⁿ`, eager-safety). This is the
classic **work-vs-span (work-depth) trade-off** of parallel evaluation.

It has a *principled* resolution, not a heuristic one: speculate (fire eagerly /
in parallel) **exactly on subterms that are definitely demanded**. A strict
position wastes zero work when forced early — it would be forced anyway — so it
is free to parallelize (work-efficient in Brent's sense); a non-strict position
stays call-by-need. The two enabling analyses are sound, not guesses:

- *strictness / demand analysis* → **where** eager/parallel firing is
  semantically safe (a sound static analysis; GHC ships it in production);
- *cardinality / usage analysis* → the **sharing policy** per duplication site:
  `no-δ` for linear / used-once values (the §Open "Typed TC-Net" point), `δⁿ`
  for used-many, `δˢ` only where copying *syntax* is the intent.

The only genuinely heuristic residue is **granularity** (is a task big enough to
amortize the spark?) — and even that has an adaptive answer (below).

**Why disp can do this better than HVM, and where the information lives.** disp is
*typed*, so strictness and usage are **type-level facts** — M2 can do
*type-directed scheduling* instead of HVM's untyped fire-everything-and-hope. The
formal home for "how a value is used" is **coeffects / graded modal types /
quantitative type theory**, which disp already has research on
(`research/effects-and-coeffects.typ`, `research/MODAL_TYPES_INVESTIGATION.md`);
the natural carrier is a **graded `Pi`** — usage/occurrence info belongs on the
function type, not the term (the bindtree-is-a-`Pi`-property insight). So the
principled lever for optimal M2 scheduling is *already inside disp's research
program*: it is the bridge that lets "valid disp" carry its own parallelization
policy. The self-improving cost model (`ApplyStats`-style interaction counts,
`research/VERIFIED_OPTIMIZER_IMPLEMENTATION.md`) is the substrate for the adaptive
granularity policy (Optimistic-Evaluation-style: speculate, measure, back off),
which fits the GOALS.md self-optimizing north star.

**Architectural consequence for M1.** The trade-off is *purely an M2 concern* —
M0's `δˢ` fire-everything is speculation-safe (`P` is inert), and M1's `δⁿ`
demand-driven is sequential and work-optimal. So build M1's reducer with a
**pluggable demand policy**: M2 then starts at the sound floor (speculate only on
type-certified strict positions → zero wasted work) and grows toward
graded-type-directed and adaptive scheduling, without reworking M1.

*Literature before designing the M2 scheduler:* GHC cardinality/demand analysis
(Sergey–Vytiniotis–Peyton Jones, POPL 2014); quantitative/graded types (Atkey
QTT; McBride "I Got Plenty o' Nuttin'"; Petricek–Orchard–Mycroft coeffects); the
parallel optimal-reduction implementations that hit this wall directly (Asperti
**BOHM**, Pedicini–Quaglia **PELCR**, **Lambdascope**); lenient/dataflow
(Arvind–Nikhil Id/pH); adaptive speculation (Ennals–Peyton Jones, *Optimistic
Evaluation*, ICFP 2003); work-span cost semantics (Blelloch–Greiner).

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

- **Scheduler design** — sequential demand-driven firing (M1) is *standard lazy
  evaluation* (STG/Sinot), not research; the **parallel** scheduler (M2) is the
  open problem — the work/span trade-off resolved by type-directed speculation
  (see the M2 modal-types note). Don't conflate the two.
- **Reachability-GC strategy** — the known cost of lazy interaction-net evaluators
  *under the materialized-agent strategy*. A hash-consed reducer (the M0/M1
  route — see §Architecture / Implementation strategy) replaces parked-duplicator
  reachability with ordinary heap reachability + grow-until-`dispose()`, deferring
  this past the M1 gate; it returns only if/when M2 materializes active pairs.
- **Lévy-optimality formalization** — theory only; not on the impl path.
- **Typed/linear TC-Net** — NOT merely a later optimization: graded/usage types
  are the *principled lever* for optimal M2 scheduling (strict ⇒ safe to fire
  early/parallel; linear ⇒ no `δ`). See the M2 note; the formal home is disp's
  coeffect/modal-type research.
- **HVM2 extension vs from-scratch** — we chose from-scratch sequential Rust→WASM;
  HVM2-extension stays the GPU-parallel (M3) option.
