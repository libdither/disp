# Evaluator Backends Plan

Re-organize the repo so disp programs can be run by **arbitrary evaluators** — the
current eager hash-consed host evaluator, a future parallel TC-Net runtime, and
out-of-process evaluators speaking the lambada-llc ternary ABI (their ASM/C++/etc.
implementations, spikeyarmaku's interaction-net evaluator) — selectable at the CLI,
with a benchmark harness that compares them on identical workloads.

Status: PLAN — for review. Nothing here is implemented.

---

## 1. Current state (audit)

Facts that constrain the design, verified against the code 2026-06-11:

- **`src/tree.ts` (582 lines) has two roles fused**: the term substrate
  (hash-consed `Tree`, `stem`/`fork`, `susp`/`force`, `treeEqual`, `prettyTree`)
  and the evaluator (`apply` with memo + budget + stats, `applyTree`, the
  `tree_eq` native fast-path registry `setTreeEqId`).
- **Evaluation happens during elaboration, not after it.** `src/compile.ts`
  calls `applyTree` at ~12 sites (e.g. CIR reduction at `compile.ts:133`, nat
  literals at `:277`, record building at `:742`, and typed-binding verification
  `param_apply typ record` at `:745`). By the time `run.ts` sees a test, both
  sides are already normalized; `runFile` only compares hash-cons ids
  (`run.ts:33`). There is **no separate runtime phase today**.
- **Hash-consing is load-bearing** (CLAUDE.md): conversion is O(1) id equality;
  deterministic elaboration means same type → same tree. Any design must keep
  this for the kernel's `tree_eq`.
- **`susp`/`force` already exist** (`tree.ts:67`, `:550`) — deferred-application
  infrastructure is partially built.
- **Rule-set compatibility is confirmed**: disp implements the same 5-rule
  "triage calculus" the lambada repo standardizes on (K, S, triage-leaf/stem/fork
  — see their `reduction-rules/README.md`), so cross-evaluator comparison is
  semantically sound.
- **The lambada ABI** (their `benchmark/run-one.sh`): an evaluator is a CLI that
  reads newline-separated ternary terms (`0`=leaf, `1`=stem+child,
  `2`=fork+left+right, prefix notation) on stdin, left-fold-applies them,
  prints the ternary normal form. Output is asserted against an expected string.
- **`bench/` already exists** (`eval-steps.ts`: step-count corpus benchmark;
  `micro_coproduct.disp`). It is host-evaluator-only and will be absorbed.
- **One native fast-path is live**: `tree_eq`, registered by tree-id from
  `compile.ts`. Exported programs must substitute the in-language `tree_eq`
  definition (which exists by the metacircular discipline) or they are not
  portable.

## 2. Goals and non-goals

**Goals**
1. `npm run disp -- --evaluator=<name> file.disp` runs a file's tests under any
   registered evaluator.
2. Two evaluator tiers with one registry:
   - **in-process** (TypeScript, implements the full `Evaluator` interface),
   - **external** (subprocess speaking the ternary ABI; coarse-grained).
3. A `disp emit` path that serializes any compiled binding to a portable ternary
   artifact (native fast-paths substituted with in-language definitions).
4. A benchmark harness in `bench/` that runs {disp-eager, external repos, future
   TC-Net} × {lambada's 5 benchmarks, disp-authored benchmarks} and reports
   wall-clock, rule/interaction counts, and PASS/FAIL assertions.
5. A conformance suite: every evaluator must agree with the reference on a
   corpus (lib tests + randomized terms), in the spirit of the existing
   `test/bracket.test.ts` differential discipline.

**Non-goals (this plan)**
- Swapping the **elaboration-time** evaluator. Compile-time reduction and
  typed-binding verification stay pinned to the host eager evaluator: it is
  part of definitional equality (deterministic tree ids) and runs mid-compile
  at per-`apply` granularity, which external evaluators cannot serve. The
  interface seam is built (§4 phase 2) so this can change later behind a
  bit-identical conformance gate.
- Building the TC-Net Rust evaluator itself (separate effort; this plan gives
  it a socket to plug into).
- Changing surface syntax, the kernel, or any `.disp` semantics.

## 3. Target architecture

```
src/
  core/
    tree.ts        # term substrate ONLY: Tree, hash-consing, stem/fork/susp,
                   # treeEqual (id check + force), prettyTree, K/I/SCOTT_* consts
  eval/
    types.ts       # Evaluator interface + EvalStats + capability flags
    eager.ts       # current apply/applyTree/memo/budget/stats, unchanged behavior
                   # (the REFERENCE evaluator; default everywhere)
    external.ts    # subprocess adapter: ternary ABI, batch normalize() only
    registry.ts    # name -> evaluator factory; reads evaluators.json
  format/
    ternary.ts     # encode/decode Tree <-> ternary string; minbin optional later
    export.ts      # export mode: native-fast-path substitution (tree_eq id ->
                   # in-language tree), closure of a binding into one blob
  parse.ts         # unchanged
  compile.ts       # imports eval/eager directly (pinned, documented); gains
                   # "defer" compilation of test/expression spines (susp nodes)
  run.ts           # gains --evaluator; routes test execution through registry
bench/
  evaluators.json  # registered external evaluators: name, cmd, cwd, encoding
  programs/        # benchmarks AUTHORED IN DISP (*.disp) + frozen lambada blobs
  artifacts/       # generated .ternary + expected outputs (gitignored, rebuilt)
  harness.ts       # runner: evaluators x benchmarks, wall + counts + assert,
                   # thread sweep for parallel evaluators; absorbs eval-steps.ts
  adapters/
    disp-eager.ts  # ~40-line CLI giving the host evaluator the lambada ABI
test/
  eval-conformance.test.ts  # every evaluator vs reference: lib corpus + random terms
```

### 3.1 The `Evaluator` interface (in-process tier)

```ts
interface Evaluator {
  readonly name: string
  // Core: both must return hash-consed trees (re-intern at the boundary if the
  // evaluator uses its own representation internally).
  apply(f: Tree, x: Tree, budget?: Budget): Tree
  normalize(t: Tree, budget?: Budget): Tree     // full NF (forces susp spines)
  // Instrumentation (the currency of the bench harness):
  stats(): EvalStats                            // rule counts, steps, memo, peak
  reset(): void
  // Capabilities:
  readonly natives: ReadonlySet<string>         // e.g. {"tree_eq"} — must mirror
                                                // in-language defs bit-identically
  readonly deterministic: boolean               // parallel evaluators: result yes,
                                                // stats no
}
```

Decisions embedded here, for review:
- **Hash-consing stays the shared term substrate.** In-process evaluators
  receive and return interned `Tree`s so `treeEqual` stays O(1) and the kernel's
  conversion checking is untouched. An evaluator with its own representation
  (e.g. a future in-process TC-Net via N-API) pays an intern/extract cost at
  the call boundary. External evaluators avoid this entirely: they round-trip
  through ternary text and results are re-interned on parse.
- **`BudgetExhausted` is part of the contract** (the compiler's 10M-step budget
  and `run` budgets must behave identically under any in-process evaluator).
- **Stats are best-effort for parallel evaluators** (interaction counts are
  deterministic under strong confluence; wall-clock and scheduling are not).

### 3.2 The external tier

An external evaluator is *not* an `Evaluator` — it is a `BatchRunner`:

```ts
interface BatchRunner {
  readonly name: string
  // One shot: terms are ternary strings; returns the ternary normal form.
  run(terms: string[], opts: { timeoutMs: number, threads?: number }): Promise<string>
}
```

Registered declaratively in `bench/evaluators.json`:

```json
{ "name": "lambada-asm-x64",
  "cmd": ["/path/to/lambada-tc/implementation/asm/bin/x64"],
  "stdin": "ternary-lines", "encoding": "ternary" }
```

External evaluators can run **test executions and benchmarks** (closed-term
normalization), never elaboration. A persistent server protocol (one process,
term-per-line REPL) is a future extension noted in §6 — not in scope.

### 3.3 Execution-model change: deferred test spines

Today a test's `lhs`/`rhs` are normalized during compilation, so there is no
work left for a selected evaluator. Fix: compile test (and top-level
expression) bodies in **defer mode** — build the application spine with `susp`
nodes instead of calling `applyTree`. Then:

- default path (`--evaluator=eager`): `runFile` normalizes via the reference
  evaluator — *behaviorally identical to today* (eager evaluation of the same
  spine, same budget), only the evaluation site moves from compile-time to
  run-time for test bodies;
- external path: the susp spine is exported via `format/export.ts` (function
  position and argument as separate ternary terms — exactly the lambada
  left-fold convention) and shipped to the `BatchRunner`; the returned ternary
  NF is re-interned and compared with `treeEqual` against the (host-normalized)
  `rhs`.

Scope guard: defer mode applies **only to test/expression items**. `let`
bindings, annotations, and verification still elaborate eagerly (they feed
subsequent scope and must produce deterministic ids). This is the minimal seam
that makes "running disp" pluggable without touching definitional equality.

Audit task before implementing (phase 0): confirm no `lib/tests` test depends
on evaluation order or budget effects in a way that distinguishes
compile-time from run-time normalization (expected: none — same evaluator,
same spine).

### 3.4 Export mode and native substitution

`format/export.ts` produces a self-contained blob for a binding:
1. compile normally;
2. replace any subtree whose id is a registered native fast-path id
   (today: `tree_eq`) with its **in-language definition's tree** (compiled with
   registration disabled);
3. serialize to ternary.

CLI: `npm run disp -- emit --ternary <file.disp> <binding>` and
`emit --test <n>` (emits `{ lhs-spine terms..., expected }` for harness use).
Conformance: an exported blob normalized by the reference evaluator must equal
the in-repo result (this pins substitution correctness, and is a test).

## 4. Phases

Each phase lands green (`npm test`) and is independently revertable.

**Phase 0 — pin behavior (½ day).**
Snapshot current `applyStats` + outputs for the `bench/eval-steps.ts` corpus
into a committed golden file. Audit defer-mode safety (§3.3). Grep `src/` for
kernel-name/tree-shape couplings before any move (per
`reference_elaborator_consumes_disp_names` memory).

**Phase 1 — mechanical split (1 day).**
`src/tree.ts` → `src/core/tree.ts` (substrate) + `src/eval/eager.ts`
(evaluator). Pure code motion: no signature or behavior changes; update
imports (`compile.ts`, `run.ts`, `bench/eval-steps.ts`, `test/*`). Update
CLAUDE.md "Code layout". Golden file from phase 0 must be byte-identical.

**Phase 2 — interface + registry (1–2 days).**
Add `eval/types.ts`, wrap `eager.ts` as the reference `Evaluator`, add
`eval/registry.ts`, thread `--evaluator` through `run.ts`. `compile.ts` keeps a
direct import of the eager evaluator with a comment documenting *why* it is
pinned (definitional equality). Default behavior unchanged.

**Phase 3 — ternary format + export (1–2 days).**
`format/ternary.ts` (encode/decode + property test: roundtrip identity on
random trees), `format/export.ts` (native substitution), `emit` CLI. Add the
export-conformance test (§3.4).

**Phase 4 — deferred test spines + external tier (2–3 days).**
Defer-mode compilation for test items; `eval/external.ts` `BatchRunner`;
`bench/evaluators.json`; `bench/adapters/disp-eager.ts` (lambada-ABI CLI around
the reference evaluator — this also makes disp a contestant in *their*
harness). Conformance test grows: run a subset of `lib/tests` through the
external path against the disp-eager adapter (round-trip sanity), and random
closed terms through every registered external evaluator.

**Phase 5 — bench harness (2–3 days).**
`bench/harness.ts`: benchmarks × evaluators matrix; per-benchmark timeout;
median + min wall-clock; steps/interactions column (from `EvalStats` for
in-process, from evaluator-reported counters for external ones that provide
them); thread sweep (`threads: 1/2/4/8`) for parallel evaluators; PASS/FAIL
assertion à la lambada; dated logs. Seed `bench/programs/` with: the five
frozen lambada blobs (verbatim), disp-authored ports of fib/sort
(cross-validation), one sharing benchmark and one laziness benchmark (the δⁿ
experiments from `research/interaction-combinator/tc-net.typ`), and the
flagship **kernel-checker workload** (`verify` of a real module, exported via
`emit`). Absorb and delete `bench/eval-steps.ts` (its corpus becomes harness
entries; its step-count reporting becomes the steps column).

**Phase 6 — futures (out of scope, sockets ready).**
Persistent server protocol for external evaluators; in-process TC-Net via
N-API (implements `Evaluator`, pays intern boundary); swapping the elaboration
evaluator behind a bit-identical conformance gate; minbin encoding.

Total: ~1.5–2 weeks. Phases 1–3 are low-risk and individually useful; phase 4
is the only one touching compile semantics (behind the defer-mode scope guard).

## 5. Decisions requiring sign-off

1. **Hash-consed `Tree` remains the universal in-process term type** (§3.1).
   Alternative rejected: abstract term handles per evaluator — would force the
   kernel's O(1) `tree_eq` through an indirection and contaminate `compile.ts`.
2. **Elaboration stays pinned to the eager reference evaluator** (§2 non-goals).
3. **Test semantics under external evaluators** = normalize exported spine,
   re-intern, `treeEqual` against host-normalized `rhs`. (Implication: the host
   still elaborates and normalizes `rhs`; external evaluators are checked
   against the host, not trusted independently.)
4. **`bench/artifacts/` is generated and gitignored**; `bench/programs/` (disp
   sources + frozen lambada blobs + expected outputs) is committed.
5. **External repos are referenced by path in `evaluators.json`**, not vendored
   or submoduled (they are benchmarking peers, not dependencies).

## 6. Risks

- **Defer mode changes *where* evaluation happens for tests.** Mitigated by
  phase 0 audit + golden snapshots; same evaluator, same spine, same budget.
- **Kernel-export blob size** (whole kernel closure in one ternary string,
  plausibly MBs). The lambada "parsing is negligible" assumption must be
  re-verified at that scale before the flagship benchmark is trusted.
- **Eager/lazy semantic divergence**: a lazy external evaluator can normalize
  terms the eager reference cannot (and vice versa for budget exhaustion).
  Harness reports these as explicit `DIVERGES`/`TIMEOUT` rows, never silent
  failures; conformance tests use total terms only.
- **Stats comparability**: disp rule-counts and interaction-net interaction
  counts are different currencies. Report both, label clearly, never sum.
- **CLAUDE.md drift**: phases 1, 4, 5 each end with a CLAUDE.md layout update
  (it documents `src/tree.ts` prominently today).
