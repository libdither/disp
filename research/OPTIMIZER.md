# The verified self-improving optimizer — reading map

disp's north star ([`GOALS.md`](../GOALS.md)) is a **universal self-improving optimizer**:
write a specification as a type, turn the type checker into a 0/1 scoring function, combine
it with cost/size scores, and have an optimizer search the host calculus for a program that
is **both formally correct and efficient**. Four research docs work out that program. They
form one stack, and this is the order to read them in.

**None of it is landed yet.** The *substrate* mostly exists (see "Status"); the optimizer
itself is research.

## The stack (read top-to-bottom)

| # | Doc | Layer | One-line takeaway |
|---|-----|-------|-------------------|
| 1 | [`EQUALITY_FOR_VERIFIED_OPTIMIZATION.md`](EQUALITY_FOR_VERIFIED_OPTIMIZATION.md) | **what equality** | A rewrite must be *licensed by an equality*. Disp inverts the usual cost: terms are erased and conversion is O(1) hash-cons, so transport is *free* — the bottleneck is "is the equation provable, and is the fragment sound?" Recommends a stack: **(A)** observational/OTT for funext, **(B)** a Cedille `φ` zero-cost cast as the rewrite primitive, **(C)** operational-equivalence licensing for soundness, **(D)** cubical mostly *not* needed (representation-independence is parametricity, which the walker already enforces). |
| 2 | [`OPERATIONAL_EQUIVALENCE_LICENSING.md`](OPERATIONAL_EQUIVALENCE_LICENSING.md) | **soundness (load-bearing)** | The deep dive sharpening EQUALITY's §7 / option-C. In a *reflective* calculus, contextual equivalence collapses to `tree_eq` (a `tree_eq [·] P` context distinguishes any two distinct trees), so it is the *floor*, not the ceiling. Licensing must be a deliberately coarser **logical relation over the walker's observer class** (applicative uses only), and specifically **Sands' strong-improvement** preorder — cost-aware, so it can certify O(n²)→O(n), not just constant factors. Set-level by design; univalence is a separate upper deck whose base case is this. |
| 3 | [`VERIFIED_OPTIMIZER_IMPLEMENTATION.md`](VERIFIED_OPTIMIZER_IMPLEMENTATION.md) | **how to build it** | The concrete realization: an **untrusted** optimizer emits `(rewritten term, certificate)`; a **tiny trusted** in-language checker re-checks it. Most substrate already exists — the cost model is `ApplyStats.steps` on fork-reductions, the e-graph is hash-cons ids, rule rulebooks are `behavioral_specs`, the harness is `test`/`param_apply`. M0 (an empirical superoptimizer) needs *zero* kernel change. Milestones M0–M5, each shippable. |
| 4 | [`interaction-combinator/DISP_BACKPROP.typ`](interaction-combinator/DISP_BACKPROP.typ) | **the search strategy** | Why hole-filling / type-directed synthesis *is* reverse-mode evaluation — unifying backprop, CDCL, and Neo-style learned clauses under semiring-parameterized **composition DAGs** with a backward functor (the chain rule). disp already has the *forward* DAG (type propagation along spines); this proposes the *backward* slot, with a superposition agent `sup_λ` as its engine. |

## Where it meets the evaluator subsystem

Doc 4 is the bridge to the reduction backends: `sup_λ` superposition runs on
**`rust-ic-net`** — the materialized parallel net, which abandons hash-consing for
per-candidate **provenance** (a *prerequisite* for reverse-mode blame attribution, not a
regression). That net is specced by
[`interaction-combinator/tc-net.typ`](interaction-combinator/tc-net.typ) and designed in
[`interaction-combinator/RUST_IC_NET_DESIGN.md`](interaction-combinator/RUST_IC_NET_DESIGN.md)
(see its §9 optimizer-interface tiers). So the dependency runs **optimizer cluster (docs 1–4)
→ rust-ic-net → tc-net calculus**. The equality stack (docs 1–3) is substrate-agnostic; the
synthesis strategy (doc 4) is what consumes the net.

## Status

- **Substrate mostly exists** (VERIFIED_OPTIMIZER §0): the cost model, e-graph-as-hash-cons,
  rulebooks, and test/verify harnesses are all already in the codebase — the M0 superoptimizer
  is a zero-kernel-change starting point.
- **The equality enlargements are research** (EQUALITY): observational `Eq`, the `φ` cast, and
  the totality gate (`Total` / `wf_fix`) are all unbuilt (deferred in `TYPE_THEORY.typ`).
- **The synthesis strategy is the riskiest** (DISP_BACKPROP): `sup_λ` soundness —
  triage-must-distribute-over-superposition, Conjectures 1–2 — is unproven, and is gated
  behind rust-ic-net's tier-1 (`foldMany`, validatable against rust-eager *now*).

## Caveats for readers

- `DISP_BACKPROP.typ` predates the kernel cutover: some code pointers (e.g.
  `lib/kernel/handlers.disp`) and a doc reference (`CATEGORY_THEORY_FOUNDATIONS_PROPOSAL.typ`,
  which does not exist) are stale — read it for the architecture, not the line numbers.
- The full higher-dimensional equality theory (cubical / HOTT) is explicitly **out of scope**
  at this layer (OPERATIONAL_EQUIVALENCE §6) — the optimizer wants a deliberate set-level floor.
