# OPTIMIZER_DESIGN.md — building the first optimizer (M0)

The concrete build plan for **M0**, the empirical superoptimizer — the buildable on-ramp of
[`OPTIMIZER.typ`](OPTIMIZER.typ) (which is the *conceptual* unified design; read its §1–§2 and §7
first). M0 is **zero kernel change**, **untrusted**, and **translation-validated**. Everything
exotic in `OPTIMIZER.typ` — superposition, `rust-ic-net`, reverse-mode, `φ`/certificates, proofs,
self-hosting — is a *later* milestone (`OPTIMIZER.typ` §10). M0 needs *none* of it, and *none* of
the §11 open questions resolved.

> **Status.** Draft. Not built. Paths verified against the tree on 2026-06-29.

---

## 0. Scope & the honest soundness floor

M0 is one function: `optimize(e : T) → e'`, an **equality-saturation superoptimizer** that returns a
cheaper, behaviourally-equivalent `e'` (or `e` unchanged). Its goal is to **prove the pipeline + the
cost model end-to-end** on real `.disp` programs.

Its soundness is **translation validation only**: an output is accepted iff it (a) re-runs the
program's own `test`s and (b) re-type-checks at `T`. That is sound *with respect to the test suite
and the types* — **not** with respect to observational equivalence. A wrong rewrite that happens to
pass incomplete tests can slip through. This is M0's known, deliberate weakness; M1 fixes it by
replacing "trust the tests" with "check a certificate" (`OPTIMIZER.typ` §7, §10).

---

## 1. Language: **TypeScript** (decision + rationale)

**Build M0 in TypeScript**, in a new `src/opt/`. The deciding facts:

- **The optimizer manipulates disp *trees*, and the trees live in TS.** The `Tree` runtime,
  hash-consing, `apply`, and `tree_eq` are `src/core/tree.ts`; the elaborator is `src/compile.ts`;
  the test harness is `src/run.ts`; the evaluator `Session` ABI is `src/eval/`. *Everything M0
  touches is already TypeScript.* The Rust evaluators (`rust-eager`, `rust-ic-net`) sit **behind**
  the `Session` ABI (called via WASM), so a TS optimizer runs candidates on the fast Rust backend
  *without itself being Rust*.
- **It reuses the cost model, the validator, and the harness with no FFI.** Cost = `Session.stats()`
  (`src/eval/types.ts`) / `ApplyStats` (`src/core/tree.ts`); re-type-check = `param_apply`
  (`src/compile.ts`); re-run tests = the `src/run.ts` driver. In Rust, each of these would have to be
  re-exposed or marshalled across the FFI boundary.
- **M0 is an untrusted, exploratory, transitional seed** — three reasons that all point at TS:
  *untrusted* ⇒ the language can't affect soundness (the checker does, M1), so optimise for nothing
  but iteration; *exploratory* ⇒ you will churn the rule set and search strategy, where TS iterates
  fastest; *transitional* ⇒ M0 is the host *seed* that the self-hosted, natively-compiled optimizer
  eventually replaces (`OPTIMIZER.typ` §10 "The bootstrap"), so do not over-invest.

**When Rust/`egg` wins (later, not now).** The one real pull toward Rust is **`egg`** (the mature
e-graph library). Bind it only if/when the *e-graph itself* becomes the runtime bottleneck — but for
M0 the hot path is **reducing candidates** to score them, which is *already* Rust (behind the
`Session`). The e-graph bookkeeping is not the bottleneck, so a small hand-rolled e-graph in TS
(§3) avoids a heavy tree↔`egg` marshalling boundary. Revisit `egg` as a drop-in behind a stable
interface if profiling later says the e-graph dominates.

*(This matches the prior plan, now folded into `OPTIMIZER.typ`: "write in TypeScript … or bind real
`egg`.")*

---

## 2. The pipeline

```
optimize(e: Tree, T: Tree): Tree
  1. seed     — build an e-graph from e (its hash-consed subtrees are e-nodes)
  2. saturate — apply RuleBook rules by e-matching until fixpoint (or a budget)
  3. extract  — pick the lowest measured-cost member of e's e-class
  4. validate — re-run the program's tests with e' + re-type-check param_apply T e'
  5. return   — e' if it validated AND cost(e') < cost(e); else e
```

---

## 3. The e-graph (reuse the hash-cons)

disp already hash-conses every subtree to a unique sequential id (`src/core/tree.ts`), with O(1)
structural equality (`a.id === b.id`) and a deterministic, memoised `apply`. So the e-graph is
nearly free:

- **e-node** = `(tag, child-class-ids)`; **e-class** = a union-find set of equivalent tree ids.
- **Seed**: insert `e` and its subtrees — each existing hash-cons id is its own initial e-class.
- **Union** on rule application (lhs-occurrence class ∪ rhs class), recording the explanation step
  (for M1's certificate; M0 can keep it for debugging).

Hand-roll this in `src/opt/egraph.ts` (union-find over `Tree.id` + a hashcons of e-nodes → class).
~150 lines. Defer `egg` per §1.

---

## 4. Rules (parametric disp programs)

A rule is **a function from its metavariables to its `(lhs, rhs)` pair** (`OPTIMIZER.typ` §7) — no
separate pattern language:

```disp
map_fusion := {f, g, xs} -> pair (map f (map g xs)) (map (compose f g) xs)
```

- **Matching is `tree_eq`, not unification**: deterministic elaboration means `rule a1 .. ak`
  rebuilds the *exact* tree of an occurrence, so the checker only verifies a *given* instantiation.
  The optimizer's job is to *find* the instantiation (e-matching) — its untrusted problem.
- **Start first-order.** Arithmetic identities, `id`-elimination, K/S/β/η (first-order forms),
  `fst`/`snd` projection, simple list laws — these need only first-order matching. Higher-order
  rules (`map_fusion`'s `f, g`) need Miller-pattern matching; defer or hand-special-case a few.
- **Where**: `lib/opt/rulebook.disp` — disp programs, each shipping `test` cases (Tier-1). Putting
  rules in-language from the start means they *carry over to M1* (where the kernel certifies them);
  the TS optimizer loads + instantiates them. (A faster-still prototype is hard-coded TS rewrite
  functions, but they don't carry to M1.)

---

## 5. Cost (reuse `ApplyStats` via the `Session` ABI)

`cost(e)` = run `e` on a small set of representative inputs and read the backend's step counter:

```
for x in representativeInputs:  acc += session.apply(e, x) ; force ; read session.stats().steps
```

- The metric is **per-backend** (`src/eval/types.ts`: `EvalStats.steps`; rust-eager counts
  fork-reductions, ic-net counts interactions) — read it from the session, **never** a global
  constant, and never compare across backends. Pick one backend (rust-eager) and be consistent.
- For M0, the representative input set is **hand-picked per program**. (Choosing it well is
  `OPTIMIZER.typ` §11's cost-model/online-corner question; M0 does not need it solved.)
- Cost here is **guidance only** (untrusted) — it just ranks candidates; soundness is §7.

---

## 6. Extraction

Pick the lowest-cost member of `e`'s root e-class. Start with **greedy bottom-up** extraction (each
class's best = cheapest e-node over its children's bests); add ILP/global extraction only if greedy
demonstrably leaves wins on the table.

---

## 7. Validation (M0's soundness)

Two independent checks, both reusing existing machinery:

1. **Re-run the program's `test`s** with `e'` substituted — the `src/run.ts` driver already decides a
   test by `session.equal(lhs, rhs)`.
2. **Re-type-check** `e'` at `T` — the elaborator's module-verification path (`param_apply T e' ==
   Ok TT`, `src/compile.ts`).

Accept `e'` only if both pass *and* `cost(e') < cost(e)`. As stated in §0, this is translation
validation; it is the floor M1's certificate checker raises to a proof.

---

## 8. File layout (all TypeScript, plus one disp file)

| file | role |
|---|---|
| `src/opt/egraph.ts` | union-find e-graph over `Tree.id` + e-node hashcons |
| `src/opt/rules.ts` | load `lib/opt/rulebook.disp`; instantiate + e-match |
| `src/opt/cost.ts` | `cost(e)` via a `Session` (§5) |
| `src/opt/extract.ts` | cheapest-member extraction |
| `src/opt/optimize.ts` | the pipeline (§2) + validation (§7); the public entry |
| `lib/opt/rulebook.disp` | the rule programs + their `test`s |

**Reuses, no new FFI:** `src/core/tree.ts` (trees, hash-cons, `ApplyStats`), `src/eval/` (the
`Session` ABI + backends), `src/compile.ts` (`param_apply`), `src/run.ts` (the test harness).

---

## 9. Done-when, and the M0 → M1 seam

**M0 is done** when it takes a real naive `.disp` program (e.g. `map f (map g xs)`, or a non-tail
`reverse`), returns a validated `e'` with **strictly lower measured cost**, and the program's own
tests still pass. That proves the cost model, the e-graph, and the validate loop end to end.

**M1 next** (`OPTIMIZER.typ` §10): add the in-language **certificate checker** (`lib/opt/checker.disp`,
~30 lines) and the `Cert` format; rewrites now carry a certificate the kernel validates — replacing
"the tests passed" with "the rewrite chain checks." The rule programs from §4 carry over unchanged;
only the *acceptance* gate strengthens.

---

## 10. Open decisions for M0 (all local — none block starting)

- **Starter rule set** — which laws first; hand-write, or mine the standard library's
  `behavioral_specs` meta-slot (the per-type law field, currently inert).
- **Cost inputs** — the representative input set per program (links to `OPTIMIZER.typ` §11 #10).
- **Extraction** — greedy vs global; start greedy.
- **e-graph** — hand-rolled (default) vs `egg` (only if the e-graph, not the evaluator, dominates).
- **Rule form** — in-language `rulebook.disp` (carries to M1; recommended) vs hard-coded TS (faster
  one-off prototype).
