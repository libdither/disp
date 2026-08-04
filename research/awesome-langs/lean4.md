# Lean 4 — Lean FRO / Leonardo de Moura

**Repo:** https://github.com/leanprover/lean4 (8,686★, 269 contributors, v4.33.0-rc2 released 2026-08-03)
**Verified activity:** 100+ commits in the last 3 months; the single most-resourced project in this survey.

## What it is

A dependently-typed language and proof assistant that is also a real programming
language — self-hosted (Lean's elaborator, tactic framework, and much of the
compiler are written in Lean), compiling via C. It is the gravitational center of
the 2025–2026 AI-proving wave: Harmonic, Math Inc, Axiom Math, DeepMind's
AlphaProof, AWS's Cedar, Verity, and Velvet/Loom all target Lean.

## The parts that matter to disp

**Metacircularity that shipped.** Lean's elaborator is written in Lean; `Expr` is a
first-class inductive type; `MetaM`/`TacticM` let ordinary Lean programs construct
and inspect terms. This is disp's §3 discipline, realized in a mainstream system —
and it is *the* existence proof that "the checker is a program in the language" is
compatible with being fast and widely used. **Lean4Lean** (Carneiro) is a Lean-4
kernel written in Lean 4, pushed 2026-08-03, the direct analogue of disp's
self-verification goal (Q5).

**Reflection is quotation-based, not intensional.** This is the key divergence.
Lean's `Expr` is a *deep embedding*: you inspect a data structure that represents
a term, not the term itself. disp's tree calculus gives programs-as-data natively —
`shape_of` pattern-matches on any program with no quotation layer. Lean pays
Gödel-numbering-style overhead (quote/unquote, `Syntax` vs `Expr` vs the actual
function) that disp's substrate avoids by construction. Conversely, Lean's
approach keeps parametricity and extensionality intact, which disp's
intensionality deliberately breaks (FOUNDATIONS §1, "simultaneously its selling
point and a soundness hazard").

**The equality answer disp cites.** Lean is intensional MLTT with definitional
unfolding, `Quot`, and propositional extensionality as axioms. It does **not**
solve A4 the way disp needs — mathlib works because humans supply the rewrite
chains, and `grind`/`simp` automate the easy ones. The Lean FRO Y3 roadmap
(Aug 2025–Jul 2026) explicitly targets software verification: `grind`/`simp`
scaling, counterexample generation, VC generation from `do`-notation.

**The AI proposer exists here.** Harmonic's Aristotle, Math Inc's Gauss, DeepMind's
AlphaProof Nexus all emit Lean and are refined against the Lean kernel — i.e.,
FOUNDATIONS §15's "verifier as hard filter, neural proposer" is operational in
Lean *today*. Raw-LLM program-proof rates are still poor (Verina: 4.9% single-shot
for o3; vericoding benchmark: 26.8% for Lean vs 82.2% for Dafny), but agentic
scaffolding changes that dramatically (see `velvet-loom-wybecoder.md`).

## Scorecard

| Axis | Lean 4 | Note |
|---|---|---|
| A1 Reflection | ◐ | Full metaprogramming, but via quotation/`Expr` deep embedding, not native intensionality. |
| A2 Spec power | **✅** | Full dependent types + universes + mathlib. Far more spec power than disp has today. |
| A3 Kernel | ✅ | Small trusted kernel, external checkers exist (lean4lean, lean4export). Larger than MM0's, much smaller than the elaborator. |
| A4 Equality | ◐ | Intensional MLTT + axioms. Rewriting is human/tactic-driven, not a decidable licensing relation. Same wall disp faces, answered socially (mathlib) rather than structurally. |
| A5 Perf | ◐ | Compiles via C, reference-counted; fine for tooling, **not** C/Rust-class for systems code. No cost-as-resource. No hardware model. |
| A6 Search | **◐→✅** | No built-in synthesis, but the richest external ecosystem of proof-search agents anywhere. |

## What disp could steal

- **`grind`-style automation as a target for the optimizer.** Lean's Y3 roadmap is
  effectively an admission that A4 must be attacked with engineering, not just
  theory. Whatever Lean lands there is directly relevant to disp's Q1.
- **The AI-proposer plumbing.** disp's §15 is "barely sketched even in design."
  Lean has the only mature interface (`sorry`-holes, tactic state as a serializable
  goal, REPL) for letting a model propose and a kernel dispose. disp needs an
  equivalent hole/goal protocol before any proposer work is meaningful.
- **lean4lean as a template for Q5** — a self-hosted kernel that is honest about
  its external anchor.

## Where disp differs

Three ways, all deliberate:

1. **Types are not predicates.** Lean has an intrinsic typed term language; disp
   has one untyped universe where a type is a function returning yes/no
   (NuPRL tradition). disp's `test Nat 3 = Ok true` has no Lean analogue.
2. **Conversion cost.** disp's conversion is O(1) pointer identity via hash-consed
   deterministic elaboration. Lean's is definitional unfolding — the thing that
   makes big mathlib proofs slow.
3. **The optimizer.** Lean has no cost model, no superoptimization, no
   self-application-for-speed story. Its self-hosting buys assurance and
   extensibility, exactly the "buys assurance, not speed or self-improvement"
   outcome FOUNDATIONS §3 identifies as the graveyard reason.

## Verdict

**disp's most serious competitor for A2, and the place where the neural half of
A6 already works.** If disp's Q1 (a decidable rewrite-licensing fragment) fails,
the honest fallback for the whole project is "write specs in Lean and let
Aristotle/Gauss-class provers close obligations" — which is what Runtime
Verification's production zkEVM pipeline actually does. disp's claim to exist is
A1 + A4 + A5 + A6 as a *combination*; Lean beats it on A2 and A3 individually and
loses on A5 badly.

**Distance from disp's goals: same destination on verification, opposite substrate;
no systems-performance or cost story at all.**
