# Implementing the Verified Optimizer in Practice

> *Part of disp's verified-optimizer stack — [`OPTIMIZER.md`](OPTIMIZER.md) is the reading map (this is layer 3, "how to build it").*

How to actually build the operational-equivalence-licensing optimizer stack on
disp's *current* codebase. Companion to the theory in
[`OPERATIONAL_EQUIVALENCE_LICENSING.md`](OPERATIONAL_EQUIVALENCE_LICENSING.md)
(why the relation is a walker-defined logical relation + Sands improvement) and
[`EQUALITY_FOR_VERIFIED_OPTIMIZATION.md`](EQUALITY_FOR_VERIFIED_OPTIMIZATION.md)
(the OTT/φ equality layer it consumes).

The headline: **disp already has most of the substrate.** The cost model, the
e-graph node identity, the certificate-checking harness, and a clean trust
boundary all exist or are one wiring step away. What's missing is the glue (a
rule library, a certificate format, a `φ` cast) and one genuine theory port
(Sands' Improvement Theorem for eager reduction).

> **Status.** Implementation design, grounded in code read on 2026-06-13/14.
> File:line references are to the current tree. Sketches are illustrative.

---

## 0. What already exists (the happy surprises)

Verified against `src/tree.ts`, `src/compile.ts`, `src/run.ts`, `lib/kernel/`,
and `EVALUATOR_PLAN.md`:

| Component the optimizer needs | Already in the codebase? | Where |
|---|---|---|
| **A reduction-step cost metric** | **Yes, built.** `apply` decrements a budget and increments `ApplyStats.steps` *only on fork-case reductions* (S-rule + triage) — exactly a Sands-style "real work" metric that skips O(1) leaf/stem/K/memo-hit steps. Per-rule-kind counters too (`sRules`, `triageForkRules`, …). | `src/tree.ts:335` (`apply`), `:436-438` (step++), `:186-203` (`ApplyStats`), `:263` (`getApplyStats`) |
| **Bounded divergence detection** | **Yes.** Run with a budget; `BudgetExhausted` ⇒ "diverges within N steps" (sound conservative observation). | `src/tree.ts:286-290, 391, 438` |
| **An e-graph node algebra** | **Yes, effectively.** Every subtree is hash-consed to a unique sequential `id`; structural equality is O(1) `a.id === b.id`; `apply` is deterministic + memoized on `(f.id, x.id)`. Canonical ids ≈ e-classes for free. | `src/tree.ts:31-80` (caches), `:94-101` (`treeEqual`), `:120` (`applyMemo`) |
| **An elaboration-time fact-checker** | **Yes.** `test lhs = rhs` compiles both sides and asserts `treeEqual(lhs, rhs)`. The natural host for running certificate checks and rule-soundness tests. | `src/run.ts:33`, `src/compile.ts:1007-1015` |
| **A type-re-verification path** | **Yes.** Module exports are checked by `param_apply typ record == Ok TT`. Reusable to re-type-check an optimized term. | `src/compile.ts:889` |
| **A free, designed-in slot for a rule library** | **Yes — and currently inert.** `behavioral_specs` is in every type's meta (`make_meta`) and the `MetaShape` type, but is set to `t` and *read nowhere*. | `lib/kernel/cut.disp:62,77-78`, `lib/kernel/types.disp:373` |
| **A backend ABI + native-recognition convention** | **Yes (planned/landing).** The Session ABI owns per-session stats and a `natives()` registry (name → canonical tree by content hash; today only `tree_eq`). New natives/observations slot here; the optimizer is a "batch-tier" consumer using `loadTernary`/`dumpTernary` + session stats. | `EVALUATOR_PLAN.md` §3, §3.1 |
| **`strip` / full erasure** | **No.** Only `ann` (type-annotation) erasure exists; §10 strip is spec-only. Matters for `φ` residue (but `φ` can emit a bare tree, sidestepping it — §4). | `src/compile.ts:302` |
| **`φ` / `checked` host primitives** | **No.** `checked` is the kernel's `param_apply` routing; no host `φ`. To add. | — |

The implication: a *working empirical superoptimizer* is buildable now with **zero
kernel changes** (Milestone 0). The *verified* version adds a small certificate
checker, a `φ` cast, and — for cost soundness — the improvement-theory port.

---

## 1. Architecture: untrusted optimizer, tiny trusted checker

```
  ┌──────────────────────────────────────────────────────────────────────────┐
  │  OPTIMIZER  (host, UNTRUSTED — may be buggy / neural / wrong)             │
  │   e-graph over tree ids · apply rule library · saturate · extract cheapest │
  │   cost = measured session stats (guidance only)                           │
  │            input: e : T          output: (e', certificate)                 │
  └───────────────────────────────────┬──────────────────────────────────────┘
                                       │ e', cert
  ┌────────────────────────────────────▼─────────────────────────────────────┐
  │  CHECKER  (TRUSTED, small)                                                 │
  │   (1) check_cert e cert  ==tree_eq==  e'      (each step matches a rule)   │
  │   (2) re-type-check       param_apply T e' == Ok TT     (belt & suspenders)│
  │   on success: accept e' (φ emits the bare e' tree — zero runtime cost)     │
  └────────────────────────────────────────────────────────────────────────── ┘

  Trusted base (TCB):  tree.ts `apply` (defines reduction = ~/cost) ·
                       check_cert (small, in-language) · the rule book's soundness
                       (Tier-0 axioms / Tier-2 proofs) · Howe congruence (meta-thm) ·
                       param_apply + recognizers (existing TCB).
```

The optimizer can be arbitrarily clever or broken; **a bad rewrite either fails
`check_cert` or fails the re-type-check.** This is the egg/lean-egg trust model
(optimizer outside the kernel, emits a checkable certificate) specialized to
disp — and it maps onto `GOALS.md`'s "external optimizer + scoring function": the
score is `(cost from session stats) × (type-check pass)`.

---

## 2. The cost model (already there; one decision to make)

`ApplyStats.steps` is incremented at `src/tree.ts:437` only on fork-case
reductions — so it already counts "the work that isn't O(1) plumbing," which is
the right shape for an improvement metric (Sands counts function-calls; disp
counts S/triage firings). To use it:

```ts
resetApplyStats()
const nf = applyTree(program, input, BIG_BUDGET)   // or run a battery of inputs
const cost = getApplyStats().steps                 // the measured cost
```

**The one decision (sign-off in `EVALUATOR_PLAN.md` §7 territory):** what *is* the
cost metric? Options, cheapest-to-implement first:
- **`steps` as-is** — total fork-case reductions. Defensible default.
- **Weighted** — e.g. `sRules + triageForkRules`, or weights per rule kind to
  model real hardware (the eventual `GOALS.md` "deterministic model of the base
  hardware"). The per-kind counters already exist (`src/tree.ts:189-196`).
- **Backend-relative** — `EVALUATOR_PLAN.md` §3 warns budgets are "in the
  backend's own unit (steps vs interactions)." For a TC-Net/interaction-net
  backend the metric is *interactions*, not S-rules. So the cost metric must be a
  **per-backend function**, read from the session, not a global constant. This is
  the honest version and it aligns with the planned ABI.

Two distinct uses, kept separate:
- **Guidance (untrusted):** measured cost drives e-graph extraction and search.
  Wrong measurements only make the optimizer pick a worse-but-still-correct term.
- **Soundness (trusted):** that a rewrite *doesn't regress* comes from each rule's
  **strong-improvement direction** (Milestone 4), not from measurement.

---

## 3. The certified rule library

### 3.1 Representing a rule (disp-native: a rule is a parametric program)

The cleanest encoding avoids a separate "pattern with holes" language: **a rule is
a function from its metavariables to its `(lhs, rhs)` pair.** Schematicity rides
bracket abstraction; instantiation is application; the two sides are projections.

```disp
// A rule over k metavariables. Applying it to k arguments yields the pair
// (lhs_instance, rhs_instance). The rule is DIRECTIONAL: lhs ⤳ rhs.
// e.g. map fusion (first-order in the list, higher-order in f,g):
map_fusion := {f, g, xs} -> pair (map f (map g xs)) (map (compose f g) xs)

// A RuleBook entry: name + the rule program + arity + kind + soundness witness.
RuleEntry := { name : String, rule : Tree, arity : Nat, kind : RuleKind, witness : Tree }
RuleKind  := CostEq | StrongImpr        // CostEq is reversible; StrongImpr is one-way
```

Recovering the two sides: `let s = rule a1 .. ak in pair_fst s` (lhs), `pair_snd s`
(rhs). Under hash-consing, `map_fusion f g xs` reconstructs the *exact* tree of an
occurrence of `map f (map g xs)` (deterministic elaboration → identical tree), so
**matching is `tree_eq`, not unification**, for the instantiated rule. The
optimizer still has to *find* the instantiation `(f,g,xs)` (e-matching), but that
is its problem (untrusted); the checker only verifies a *given* instantiation.

> *Higher-order caveat.* `map_fusion` quantifies over functions `f,g`. First-order
> rules (arithmetic identities, `id`-elimination, record/projection laws) need only
> first-order matching — easy. Higher-order rules need the optimizer to guess
> function instantiations (higher-order matching, undecidable in general, tractable
> with patterns à la Miller). **But many higher-order rules arrive free as
> parametricity free theorems** (the walker already guarantees them) — those need
> no search, just instantiation. Stage the optimizer: first-order + free-theorem
> rules first.

### 3.2 Where the rule book lives

- **Per-type laws → `behavioral_specs`.** `List`'s meta carries the list laws,
  `Nat`'s the arithmetic ones. This activates the currently-inert slot
  (`cut.disp:77`) for exactly the role the spec imagines ("runnable coherence
  laws", `TYPE_THEORY.typ` §12.1) — now realized as rewrite rules.
- **Cross-type laws (β/η, `id`, `compose`) → a global `RuleBook`** in a new
  `lib/opt/rulebook.disp`.
- Both are ordinary disp values consumed by the same in-language checker.

### 3.3 The soundness witness — three tiers (a staging device)

`witness` says *why* `lhs ⤳ rhs` is sound. Tier it so the machinery boots before
the proofs exist:

- **Tier 0 — reduction laws (sound by the operational semantics).** β, η, `K`/`S`
  computation, `id x ⤳ x`: these *are* the reduction relation the runtime
  implements, so they're already captured by `tree_eq`-after-reduction and need no
  rule at all — list them only if the optimizer wants them as explicit e-graph
  moves. `witness = reduction`.
- **Tier 1 — test-certified (a strong filter, not a proof).** The rule ships a
  battery of `test` cases (`run.ts:33` checks them at elaboration). Catches most
  bugs; **in the TCB until promoted.** `witness = <test suite ref>`. This is how
  Milestone 0/1 ship real rules fast.
- **Tier 2 — proof-certified (the real thing).** `witness : Eq_obs T lhs rhs` —
  an in-language observational-equality proof (funext etc., Milestone 3), checked
  by the existing type-checker. For cost, additionally `witness_cost :
  Improves T lhs rhs` (Milestone 4). Now the rule is *not* in the TCB; the proof
  is machine-checked.

The "total fragment" of the theory docs is concretely **the consistent sub-logic
where Tier-2 witnesses are proved** — and it can be small: many rules are
free theorems (walker), and the rest are local `Eq_obs` proofs.

---

## 4. The certificate + checker + `φ`

### 4.1 Certificate format (egg-style flat explanation)

```disp
// Navigate to a redex, apply a rule instance, repeat.
Dir   := DL | DR | DStem          // fork-left, fork-right, into-stem-child
Path  := List Dir
Step  := { rule : String, path : Path, subst : List Tree }
Cert  := List Step
```

### 4.2 The checker (in-language, ~30 lines, reuses triage/pair/tree_eq)

```disp
// fetch subtree at a path; replace subtree at a path (both fold over Dir via triage)
at      := fix ({self, p, t} -> if (is_nil p) then t else (self (tail p) (step_into (head p) t)))
replace := fix ({self, p, new, t} -> if (is_nil p) then new else (rebuild (head p) t (self (tail p) new ...)))

// one step: instantiate the rule, check lhs matches the targeted subtree, splice rhs.
check_step := {book, t, s} -> {
  let pair_lr = apply_all (lookup book s.rule).rule s.subst    // rule m1..mk -> (lhs,rhs)
  let l = pair_fst pair_lr
  let r = pair_snd pair_lr
  if (tree_eq (at s.path t) l) then (Ok (replace s.path r t)) else Err
}

check_cert := {book, e, cert} -> foldM check_step (Ok e) cert    // -> Ok e' | Err

// the licensing check: cert validates that e rewrites to the claimed e'.
verify_rewrite := {book, e, e', cert} ->
  match (check_cert book e cert) { Ok t => tree_eq t e'; Err => FF }
```

It is **decidable, total (under budget), and small enough to audit** — and being a
disp program, it is itself type-checkable and (eventually) verifiable, satisfying
the metacircular discipline. Soundness argument: each step replaces a subtree by a
rule-related one; each rule is in `~_T` (Tier-0 axiom / Tier-2 proof); `~_T` is a
congruence (Howe — the one external meta-theorem, *easy here because tree calculus
is binder-free*, see `OPERATIONAL_EQUIVALENCE_LICENSING.md` §5); so `e ~_T e'`.

### 4.3 `φ` — the zero-cost swap (an elaborator construct)

`φ` is wired like `Pi`/`select_lazy`/`tree_eq` — a kernel name the elaborator
special-cases (`compile.ts` already hardcodes ~15 such names):

```
elaborate( phi T e e' cert ):
   1.  assert verify_rewrite RuleBook e e' cert  == TT     // the certificate holds
   2.  assert param_apply T e' == Ok TT                    // re-type-check the result (reuses compile.ts:889)
   3.  emit  e'                                            // the RUNTIME VALUE is just e' — zero residue
```

Because disp erases (terms are bare trees) and step 3 emits `e'` directly, **there
is no `φ` residue at runtime** — the "zero-cost coercion" is literal. No `strip`
pass is needed for `φ` itself (it never leaves a wrapper); `strip` (§10) remains
worth doing for record/coproduct descriptors but is orthogonal to this.

Step 2 is belt-and-suspenders: even if `check_cert` or the rule book is buggy, the
result must still independently type-check at `T`. It's cheap (`e'` is the small
tree) and reuses the existing module-verification call verbatim.

For the *optimizer* pipeline, `φ` need not appear in source at all: the optimizer
rewrites the program's tree and hands `(e', cert)` to the build; the build runs the
same two checks. `φ` is the *user-facing* surface (manual "I claim these are
interchangeable, here's the proof") sharing one checker.

---

## 5. The optimizer (host, untrusted)

An e-graph over disp trees, reusing the runtime's canonical ids as e-node keys:

- **Seed** the e-graph with the target term (its hash-consed subtrees are e-nodes).
- **Saturate** by applying `RuleBook` rules (e-matching: find instantiations whose
  `lhs` occurs; add the `rhs`; union the classes; record the explanation step).
- **Extract** the lowest-cost member of the program's class — cost from measured
  session stats (run candidates on representative inputs via the Session ABI) or a
  static size proxy.
- **Emit** `(e', cert)` where `cert` is the explanation chain for `e ⤳* e'`.

Engineering notes:
- The e-graph and search are **pure host code, untrusted** — write in TypeScript
  (the `eval/eager.ts` reference backend), or bind real `egg`. Soundness doesn't
  depend on it.
- Run cost evaluations on whatever backend is fast (`EVALUATOR_PLAN.md` §4: WASM,
  or the TC-Net path — this is `GOALS.md`'s "outsource execution to a faster
  language" used for *scoring*, not for trust).
- **Neural guidance** (`GOALS.md`) = the rule-selection / extraction policy. It
  changes *which* certificate is found, never *whether it checks*.
- **Asymptotic vs constant-factor:** measured cost on a few inputs is a heuristic;
  the *guarantee* of no-regression rides the rule directions (Milestone 4). For
  genuine asymptotic wins (the deforestation-style transforms), the optimizer needs
  the asymmetric improvement preorder + improvement induction — see §6, M4, and the
  Moran–Sands constant-factor result in the theory doc.

---

## 6. Milestones (each shippable, increasing trust)

**M0 — Empirical superoptimizer. *Zero kernel change; buildable now.***
Host e-graph + a hand rule set (Tier-1, tested via `test`); extract by measured
cost; **validate output by re-running the program's existing tests + `param_apply`
type-check.** This is translation validation: sound *w.r.t. the test suite and
types*, not yet w.r.t. `~`. Proves the pipeline and the cost model end-to-end.
Touches only `src/` (a new `src/opt/` host module) + a rule file. *Risk: low.*

**M1 — Certificate checker + rule book (in-language).** Add `lib/opt/{rulebook,
checker}.disp` (§3–4.2). Rewrites now carry certs the kernel validates; add a
`test`-style elaboration check. Soundness now rests on rule-book soundness
(Tier-0/1) + checker correctness + Howe congruence (assumed). *Risk: low-medium
(e-matching, path navigation).*

**M2 — `φ` cast (elaborator).** Wire `phi` in `compile.ts` (§4.3): verify cert +
re-type-check + emit `e'`. User-facing zero-cost swaps; the optimizer pipeline gets
its kernel-blessed accept step. *Risk: low (mirrors existing hardcoded-name
handling).*

**M3 — Observational `Eq` (funext) + Tier-2 rule proofs.** Land the `eq` meta-field
(`EQUALITY_FOR_VERIFIED_OPTIMIZATION.md` §3); re-prove the rule book as `Eq_obs`
terms. Rule soundness becomes machine-checked in-language; rules leave the TCB.
*Risk: medium (the equality-layer change; conservative for discrete types).*

**M4 — Cost-aware improvement (Sands).** Port the Improvement Theorem to eager
tree-calculus reduction (choose the metric, re-derive a context lemma); tag rules
`CostEq`/`StrongImpr`; add improvement induction for asymptotic transforms. *Risk:
high — the one genuine theory port (feasible: a call-by-value improvement theory
exists, Sands 1997).*

**M5 — Research.** Formalize Howe congruence for tree calculus (binder-free ⇒
easy); if univalence/HITs ever wanted, the bridge-modality dimensional layer
(`OPERATIONAL_EQUIVALENCE_LICENSING.md` §6) — *separate from the optimizer*.

---

## 7. Worked example: map fusion, end to end (M1–M2)

```
program  :  λxs. map inc (map dbl xs)           // tree e : List Nat -> List Nat
rule     :  map_fusion = {f,g,xs} -> pair (map f (map g xs)) (map (compose f g) xs)
optimizer:  e-matches f:=inc, g:=dbl, xs:=<the bound var>; emits
            e'   = λxs. map (compose inc dbl) xs
            cert = [ { rule:"map_fusion", path:[<into the body>], subst:[inc, dbl, xs] } ]
checker  :  check_cert RuleBook e cert
              → at path = `map inc (map dbl xs)`
              → map_fusion inc dbl xs = pair (map inc (map dbl xs)) (map (compose inc dbl) xs)
              → tree_eq subterm (pair_fst …)  ✓  → splice (pair_snd …)
              → result tree_eq e'  ✓
            param_apply (List Nat -> List Nat) e' == Ok TT   ✓
accept   :  φ emits e'.  Runtime now does one traversal instead of two — and the
            proof obligation discharged to a single `tree_eq` + a type-check.
```

Why this is cheap in disp specifically: the *swap* costs nothing (erased tree
replacement), the *proof* is a `tree_eq` against an instantiated rule, and (at M3)
`map_fusion`'s soundness is one `Eq_obs` lemma proved once — funext makes it
statable, the walker makes the `f,g` parametricity free.

---

## 8. The trust boundary, precisely

**In the TCB:**
1. `src/tree.ts apply` — *defines* reduction, hence `~` and the cost metric. (Already trusted: it's the runtime.)
2. `check_cert` — small, in-language, auditable, eventually self-verified.
3. The rule book's soundness: Tier-0 axioms (= reduction, free) + Tier-2 proofs (machine-checked) — Tier-1 *tested* rules are in the TCB only until promoted.
4. **Howe congruence for tree calculus** — the one external meta-theorem (`~`/improvement is a congruence so step-wise rewriting composes). Binder-free ⇒ the hard clause vanishes.
5. The cost-metric definition (per backend) — for M4's no-regression guarantee.
6. Existing TCB: `param_apply`, the recognizers, the walker.

**Not in the TCB:** the entire optimizer (e-graph, e-matching, search, neural
policy, extraction), all cost *measurements* used for guidance, the parser, the
pretty-printer.

---

## 9. Hard parts & open questions

1. **The eager Improvement Theorem (M4).** Sands is call-by-name/need with a
   function-call metric; disp is eager. Pick the metric (likely `sRules +
   triageForkRules`), re-derive the context lemma + improvement induction for the
   eager strategy. The single biggest theory task; known-feasible (Sands 1997 CBV).
2. **Higher-order e-matching.** First-order + free-theorem rules first; higher-order
   rules (map fusion's `f,g`) need pattern-style higher-order matching in the
   optimizer (untrusted, so incompleteness is fine — missed rules = missed
   speedups, not unsoundness).
3. **`~_T` vs the walker's escape check.** The in-language soundness *proofs*
   (Tier 2) must themselves stay walker-clean (an `Eq_obs`/improvement proof must
   not leak hyp structure). Likely the same escape-check the walker already runs;
   confirm `check_cert` and the proofs route through `param_apply`, not raw apply.
4. **Cost metric as a backend function.** Bake into the Session ABI (`SessionOpts`
   already carries `defaultBudget` "in the backend's own unit"); a TC-Net backend
   scores *interactions*, not S-rules. Don't hardcode a global cost.
5. **Where does the rule book bootstrap?** `behavioral_specs` is per-type and loads
   with the kernel; the global book is a `lib/opt/` module. Ordering: the checker
   and book must elaborate after the kernel but before any optimized module —
   mirror the prelude-barrel ordering (`lib/kernel/prelude.disp`).
6. **Promotion path Tier-1 → Tier-2.** A workflow: a tested rule is flagged
   "trusted"; landing its `Eq_obs` proof flips it to "verified" and removes it from
   the TCB. Track which rules are still trusted (a lint, like `soundness.test.disp`
   pins the carve-outs today).
7. **Does the optimizer ever need a *refinement* (not equivalence) relation?**
   E.g. eliminating a diverging unused subterm changes termination — not an `~`,
   needs an improvement/approximation preorder. Catalogue which transforms need
   which relation (ties to M4).

---

## 10. Summary

- A **working empirical superoptimizer is a Milestone-0 away** — the cost model,
  e-graph ids, and validation harnesses already exist; it needs only host glue and
  a tested rule set, with **no kernel change.**
- The **verified** version is a small, well-separated addition: an in-language
  certificate checker (~30 lines), a `φ` elaborator construct (mirrors existing
  hardcoded-name handling), and the `behavioral_specs` slot repurposed as the rule
  library. The optimizer stays fully untrusted; a bad rewrite fails the cert-check
  or the re-type-check.
- The **trust boundary is tiny and mostly pre-existing**: the runtime, a small
  checker, the rule-book soundness (proved at M3/M4), and one binder-free Howe
  congruence theorem.
- The **one real theory port is Sands' Improvement Theorem for eager reduction**
  (M4) — needed only for *asymptotic* (not constant-factor) and *no-regression*
  guarantees; everything up to M3 delivers correctness-preserving (not yet
  provably-faster) rewrites.
- It composes exactly with `GOALS.md`: the optimizer is the external scorer (cost ×
  type-check), neural guidance is the rule policy, and the faster evaluator
  backends (`EVALUATOR_PLAN.md`) are the scoring substrate.

### References
Implementation anchors: `src/tree.ts` (apply/stats/hash-cons/tree_eq),
`src/compile.ts` (elaboration, hardcoded kernel names, module verification),
`src/run.ts` (test harness), `lib/kernel/cut.disp` (`make_meta`,
`behavioral_specs`), `EVALUATOR_PLAN.md` (Session ABI §3, native convention §3.1,
backends §4). Theory: `OPERATIONAL_EQUIVALENCE_LICENSING.md` (the relation),
`EQUALITY_FOR_VERIFIED_OPTIMIZATION.md` (the φ/OTT layer). External: egg (Willsey
et al., POPL 2021) + lean-egg (Rossel–Goens 2024) for the certificate model; Sands
(TOPLAS 1996, HOOTS 1998) + Moran–Sands (POPL 1999) for improvement; Pitts (2000)
for the logical relation; Howe (1996) for congruence.
