# Lambada batch peer + ternary reconciliation

The concrete next step after the Session ABI (phases 1–4). Wires the first
**external** evaluator — a lambada (`lambada-llc/tree-calculus`) reducer — as a
batch-tier peer, and proves disp's ternary interchange against it by differential
conformance. This implements `EVALUATOR_PLAN.md` §4.5 and expands
`EVALUATOR_LAYOUT.md`'s migration step 3 with the reconciliation homework done.

Status: **LANDED 2026-06-22.** All pieces built and green; full suite 243 (was
164, +79 lambada conformance). **All 11 lambada evaluators wired** (not just
lazy-stacks) + a reproducible cross-evaluator benchmark. Outcome below; the
section bodies are the as-built record.

## Outcome (landed 2026-06-22)

The first **external** validation of disp's evaluator ABI + ternary interchange —
against ALL ELEVEN lambada reducers, plus a reproducible benchmark.

- **Reconciliation held exactly** (§2): lambada's ternary is byte-identical to
  disp's — no codec shim. The reference CLI uses **argv mode** (`-ternary t0 t1 …`,
  folds from identity), confirmed against the vendored source.
- **All 11 evaluators wired, not one.** The stock `main.mts` hardcodes lazy-stacks;
  `evaluators/lambada/cli.mts` generalizes it to a selectable `-e <name>` CLI over
  every evaluator (`eager-{func,node-app,stacks,value-adt,value-memory,value-
  memoizing,value-memoizing-alt}`, `lazy-{func,stacks,value-adt,value-memory}`),
  reusing lambada's own generic ternary formatter + `id`/`marshal`. It also prints
  startup-free `reduce_ms` on stderr for the benchmark.
- **Built** (`evaluators/lambada/build.sh`): pinned shallow fetch of
  `lambada-llc/tree-calculus` @ `b30ce83` → `npm install` + `tsc` → esbuild-bundle
  `cli.mts` → one `artifacts/cli.cjs` (.cjs: bundle is CommonJS, disp is
  "type":"module"). Reproduces from clean in ~3s; verifies the pin (loud fail on
  drift). vendor/ + artifacts/ gitignored.
- **Adapter** (`src/eval/lambada.ts`): `lambadaRunner(evaluator)` — a `BatchRunner`
  (out-of-process, one-shot — §4.4) per evaluator; `LAMBADA_EVALUATORS` lists all
  11; `lambadaAvailable()`. Peers declared in `bench/evaluators.json`.
- **Fixture** (`bench/programs/lambada-benchmarks.json`): the five PROGRAMS
  extracted verbatim from the pinned `run.sh` and parse-verified (hand-copying the
  ~700–1800-char strings was error-prone — a truncated `fib` caught it).
- **Differential test** (`test/eval-lambada.test.ts`, `skipIf(!built)`): **all 11**
  evaluators ⟷ disp-eager agree on size / fib / silly-exp / exercise-rules /
  merge-sort AND disp hits independently-derived values, + a randomized
  small-total-term sweep vs a lazy and an eager evaluator. **79/79** with the
  artifact; all SKIP without it (the everyday loop pulls no foreign build).
- **Benchmark** (`bench/bench-evaluators.ts` + `bench/disp-cli.ts`,
  `npm run bench:evaluators`): disp-eager + disp-naive + all 11 lambada evaluators
  × the five programs, **best-of-N, cold-subprocess, startup-free `reduce_ms`** so
  disp and lambada are measured identically (no warm-JIT/arena bias). Validates
  every result against the disp-eager oracle; classifies STACK (recursion-based
  evaluators overflow the host stack on deep workloads) / TIMEOUT / DNF / FAIL.
  Dated log under `bench/logs/`. Reproducible (pinned evaluators + programs,
  deterministic args, size knobs via env).
- **Two real findings:** (1) esbuild bundles CommonJS but disp is "type":"module"
  → artifact must be `.cjs`; (2) lambada's `*-func`/`*-value-adt`/`*-value-memory`/
  `*-value-memoizing` evaluators reduce on the **host call stack**, so they STACK on
  deep workloads — the explicit-stack/iterative evaluators (`*-stacks`, node-app,
  lazy-value-memory) and **both disp backends** complete everything. (Directly
  relevant to disp's iterative `apply` and TC-Net's explicit scheduling.)

**Acceptance criteria (§7): all met** (and exceeded — all 11 + benchmark). Not
committed (left for review).

Deliberately **not** done (low value now): the reverse adapter (disp as a
contestant in lambada's harness — defer until cross-running their bench is wanted).
Next phase is TC-Net (phase 6).

---

Status (original): PLAN — for review.

## 1. Why this is the next step

All conformance today is **eager-vs-naive** — two reducers written by the same
author, sharing one ternary codec lineage and one mental model. That is a weak
differential. lambada is the first *foreign* evaluator: a separate implementation
of the same 5-rule calculus, with its own codec, its own reducers. Running
disp's interchange against it is where a latent ABI / ternary / NF-equality flaw
would actually surface — the highest-information, lowest-cost validation
available right now.

It is also strictly additive and **does not require the on-disk layout
migration** (`EVALUATOR_LAYOUT.md` steps 1–2, the `src/eval/impl/` reshuffle):
a batch peer is out-of-process (`EVALUATOR_PLAN.md` §4.4/§4.5 — a `BatchRunner`,
never a `Session`), so it needs only a subprocess adapter + a peer entry + a
test. The one layout piece it does touch — a new `evaluators/lambada/`
foreign-project folder — is a *new directory*, not a move of existing files, so
it carries none of the reshuffle's churn (and competes with no live kernel work).

Advances `EVALUATOR_PLAN.md` goals 5 (differential conformance) and 6 (bench vs
external repos), and `GOALS.md`'s "outsource execution to a faster external
language" substrate.

## 2. Reconciliation (homework done 2026-06-22 against the lambada repo)

**CONFIRMED — the ternary character scheme is byte-identical; no shim.**
- lambada `conventions/README.md`: preorder arity encoding, `0` = leaf,
  `1…` = stem (one child), `2…` = fork (two children), characters with **no
  separators or whitespace** between subterms. Worked example: "the identity
  program would be `21100`."
- disp `encodeTernary` (`src/core/tree.ts:660`) emits exactly this (`force` then
  push `0`/`1`/`2` preorder); `decodeTernary` (`:675`) parses exactly this and
  rejects trailing input. Cross-check: `21100` decodes to
  `fork(stem(stem(leaf)), leaf)` under disp's parser — a valid single term. So a
  disp ternary string **is** a lambada ternary term and vice-versa.
- Both treat ternary as the encoding of fully-reduced **values** (lambada:
  "can only represent fully reduced values"; disp: `encodeTernary` forces). Bench
  inputs are values; the *fold* builds the redex that gets reduced. So disp's
  force-on-encode is correct here, not a problem.

**CONFIRMED — the batch contract (`benchmark/run-one.sh`).**
- Stdin mode (most evaluators): terms are fed **one ternary term per line**
  (`printf '%s\n' "$bench_stdin" | "$@"`); the binary left-folds the lines by
  application and prints the ternary NF.
- Argv mode (JS reference only): `node main.js -ternary t1 t2 …` (space-separated).
- Expected NF is `run-one.sh`'s first positional arg; the check is an **exact
  string compare** of the binary's stdout against it. Exit 0 required.
- The fold-from-identity vs fold-from-`t0` distinction is already reconciled in
  `src/eval/batch.ts` (`apply(I,t0)≡t0`). For an external peer the fold happens
  **inside** the binary; our adapter only pipes lines and reads the NF — it does
  *not* apply in-process (that is what `sessionBatchRunner` does for the
  in-process backends). Both satisfy the one `BatchRunner.fold` contract.

**RESIDUAL RISKS — cannot confirm from docs; the test exists to catch them.**
- *I/O hygiene*: the binary may emit a trailing newline / whitespace; the adapter
  must `trim()` stdout before comparing.
- *NF-string identity*: both are the same 5-rule calculus, so NFs *should* be
  byte-identical ternary — but proving it is the entire point. A mismatch reveals
  a genuine rule/encoding divergence. (This is the value, not a setup detail.)
- *Which binary + exact CLI*: pick the reducer and confirm stdin vs `-ternary`.
  The layout doc's intended peers are lambada's **memoizing** and **lazy-stacks**
  reducers (`implementation/typescript/src/evaluator/eager-value-memoizing.mts`,
  `lazy-stacks.mts`) — registered as `lambada-memo` / `lambada-lazy`.

## 3. Scope

**Needs:** a pinned vendor of one lambada reducer; a subprocess `BatchRunner`
adapter; a peer entry in `bench/evaluators.json`; a differential/benchmark test.

**Does NOT need:** the `src/eval/impl/` layout migration; §3.1
recognition-by-hash (a non-recognizing peer runs the inline `tree_eq`, correct
and merely slower — `EVALUATOR_PLAN.md` §3.1 re-grade); any change to the
elaborator or the kernel.

## 4. Pieces to build

1. **`evaluators/lambada/`** — the foreign-project folder (`EVALUATOR_LAYOUT.md`
   conventions), created fresh:
   - `build.sh` — pins lambada (commit SHA + a content hash it verifies),
     fetches `implementation/typescript/`, runs its build (node + `tsc`/bundle)
     into `artifacts/`, yielding the runnable reducer entry point(s). Records the
     pin so a clean clone reproduces it.
   - `artifacts/` — gitignored build output.
   - First peer is node-based (no cargo/wasm toolchain) → lowest friction; matches
     "the everyday loop needs zero foreign builds" (skipped if `artifacts/` absent).

2. **The subprocess adapter** — `src/eval/lambada.ts` (flat for now, beside
   `eager.ts`/`naive.ts`/`batch.ts`; moves to `src/eval/impl/` with everything
   else when the layout migration runs). Implements `BatchRunner`:
   - `fold(terms)`: spawn the pinned binary, write `terms.join("\n") + "\n"` to
     stdin (or `-ternary …` argv for the JS entry — confirm in §2), read stdout,
     `trim()`, return the ternary NF. One spawn per fold (the one-shot contract;
     fine for bench + tests).
   - Registers the project's several reducers (`lambada-memo`, `lambada-lazy`) as
     distinct `BatchRunner`s configured off the same vendored entry — "a project
     ships several evaluators" (`EVALUATOR_LAYOUT.md`).
   - Budget is honored as a wall-clock **timeout** only (the peer has its own
     internal limits); the `Budget` step/interaction unit does not cross.

3. **`bench/evaluators.json`** — fill the empty `peers: []` with the lambada
   entries (the schema/doc string is already there: `{ name, cmd, args }`,
   stdin-ternary-lines → ternary-NF). Point `cmd` at the `artifacts/` entry.

4. **`test/eval-lambada.test.ts`** — `describe.skipIf(!artifactBuilt)` so
   `npm test` stays green with no foreign build. Two tiers:
   - **Benchmark conformance (core gate):** lift lambada's five committed
     benchmarks (size / recursive-fib / silly-exp / exercise-rules / merge-sort)
     and their expected NFs from `benchmark/run.sh`; assert disp-eager's
     `sessionBatchRunner` fold **and** the lambada peer's fold both equal the
     committed expected. This validates the interchange end-to-end on real,
     non-trivial programs against an oracle disp didn't author.
   - **Randomized differential (bonus):** generate random disp **values**, dump
     to ternary, assemble small folds, assert disp-eager NF === lambada-peer NF.

5. **(Optional, §4.5) reverse adapter `bench/adapters/disp-eager.ts`** — makes
   disp a contestant in *lambada's* harness: read stdin lines, decode each, fold
   via `sessionBatchRunner`, print NF. ~40 lines; lets `benchmark/run-one.sh`
   call disp directly. Defer unless cross-running their harness is wanted.

## 5. Phasing

1. **Vendor + smoke (½ day).** `evaluators/lambada/build.sh`; build one reducer;
   by hand: pipe disp's `encodeTernary` of `I` (`21100`) applied to a value,
   confirm the peer returns the expected NF. Settles the CLI (stdin vs argv) and
   I/O trimming before any test code.
2. **Adapter + peer entry (½ day).** `src/eval/lambada.ts` `BatchRunner`;
   `evaluators.json` entries; register `lambada-memo`/`lambada-lazy`.
3. **Benchmark conformance test (½ day).** The five committed benchmarks through
   disp-eager and the peer vs expected. **This is the acceptance gate.**
4. **Randomized differential + (optional) reverse adapter (½ day).**

~1.5–2 days. Each step lands green and is independently revertable;
`skipIf`-gated so the default suite never pulls the foreign build.

## 6. Risks

- **NF mismatch on a real benchmark** — the outcome that justifies the work.
  Triage: is it an I/O/whitespace artifact (trim), a fold-direction artifact
  (re-check `apply(I,t0)` reconciliation), or a genuine reduction/encoding
  divergence (a real bug in one side — escalate, do not paper over)?
- **CLI drift between lambada reducers** (stdin vs `-ternary`, flag spellings) —
  settled in phase 1 against the actual binary; encode it in `evaluators.json`.
- **Build friction** (node version, bundling the `.mts` evaluators) — pin node +
  deps in `build.sh`; if the TS entry is awkward, fall back to the Python
  reducer (`implementation/python/tree-calculus.py`, stdin mode) as the first
  peer instead — same contract, simpler build.
- **Pin staleness** — `build.sh` verifies a content hash; a drifted upstream
  fails loudly at build, never silently runs a different evaluator.

## 7. Acceptance criteria

- `evaluators/lambada/build.sh` produces a runnable reducer from a clean clone.
- `npm test` green unchanged when the artifact is absent (skip), and with it
  built, `test/eval-lambada.test.ts` passes: all five lambada benchmarks reach
  their committed expected NF via **both** disp-eager and the lambada peer.
- Randomized folds agree disp-eager vs peer.
- No change to elaborator/kernel; no layout reshuffle required.

## 8. Open questions

- Vendor lambada's TS source and build it, or fetch a prebuilt/bundled entry?
  (Source+build is reproducible from the pin; prebuilt is less friction. Lean
  source+build to keep the pin honest.)
- First peer: TS memoizing reducer (the layout doc's intended `lambada-memo`) or
  the Python reducer (simplest build)? Either satisfies the contract; the test is
  identical. Start with whichever builds in one command.
- Propose the standard-natives convention (`EVALUATOR_PLAN.md` §3.1) upstream in
  lambada's `conventions/` once disp's recognition lands? Out of scope here;
  noted because this is the natural moment the two ecosystems first touch.
