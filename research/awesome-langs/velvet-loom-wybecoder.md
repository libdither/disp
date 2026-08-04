# Velvet / Loom / WybeCoder — VERSE Lab (NUS) + Meta AI Research

**Repos:**
- https://github.com/verse-lab/loom (156★, pushed 2026-08-03) — the framework
- https://github.com/verse-lab/velvet (71★, pushed 2026-07-06) — the verifier
- https://github.com/facebookresearch/wybecoder (36★, pushed 2026-05-06) — the agent
**People:** Ilya Sergey's VERSE Lab; Velvet paper at CAV 2026.
**Clone inspected:** yes — Velvet README, `Velvet/Syntax.lean`, `Velvet/VelvetTheory.lean`; Loom README.

## What it is: a three-layer stack

**Loom** — a framework for generating *foundational multi-modal verifiers*, based on
a monadic shallow embedding of executable program semantics into Lean 4. It does
**automated weakest-precondition generation** using monad transformer algebras, plus
executable semantics and non-determinism semantics.

**Velvet** — a Dafny-style auto-active verifier for imperative programs, built on
Loom. Specifications via `method`/`requires`/`ensures`/`invariant`/`decreasing`
macros; obligations discharged by cvc5 and z3; **when SMT fails, you finish
interactively with Lean tactics** (`aesop`, `grind`) and you have all of mathlib
available. Also: property-based testing of extracted executables, separate partial
and total correctness, angelic/demonic non-determinism.

**WybeCoder** (Meta) — an agentic *prove-as-you-generate* framework where "code,
invariants, and proofs co-evolve," running Claude Opus 4.5 agents over Velvet.
Results: **74.1% on Verina** (128 proved + 12 disproved, 32 turns × 16 agents) and
**62.1% on Clever-Loom**. Strategies: a sequential refinement loop with pass@k, and
**subgoal decomposition** — extract verification subgoals, dispatch parallel provers,
reconstruct the full proof, with conflict-driven method modification across
iterations.

## Why it matters to disp

**1. It is the clearest quantitative evidence for the §15 paradigm.** Verina's
single-shot Lean proof success for o3 was **4.9%**. WybeCoder on the same benchmark
gets **74.1%** with agentic scaffolding. That ~15× gap is the strongest available
answer to disp's Q3 ("can a proposer find certifiable improvements faster than
checking junk costs?") — for *proofs*, on *benchmark-scale* problems, the answer is
now clearly yes, and the improvement came from search structure, not model scale.

**2. Multi-modal verification is a design disp should copy.** Velvet's key
ergonomic move is the *fallback ladder*: SMT first, interactive tactics when SMT
fails, property-based testing when you just want evidence. disp's checker is
currently binary (`Ok true`/`Ok false`/`Err`). An optimizer that can only ever get
"yes/no" wastes the enormous amount of information in *how* a check failed and
*how close* it got. Velvet institutionalizes the gradient.

**3. Subgoal decomposition is a search architecture, not a prompt.** WybeCoder
extracts subgoals, proves them in parallel, and reconstructs — with
*conflict-driven method modification*, i.e. failures feed back into the
decomposition. This is the closest existing thing to disp's reverse-mode credit
assignment (§13): blame flows from a failed whole back to the choices that made it.

## Scorecard

| Axis | Velvet/Loom/WybeCoder | Note |
|---|---|---|
| A1 Reflection | ◐ | Inherits Lean metaprogramming; Loom generates verifiers *as* Lean developments, which is a real reflective move. |
| A2 Spec power | ✅ | Full Lean + mathlib behind a Dafny-style surface. |
| A3 Kernel | ✅ | Lean's kernel; Loom verifiers are *foundational* (proofs bottom out in Lean, not in a trusted VC generator). |
| A4 Equality | ◐ | Lean's, plus SMT within its fragment. Not a rewrite-licensing relation. |
| A5 Perf | ✗ | Imperative programs extracted for testing; no native systems backend, no cost model. |
| A6 Search | **✅** | Best-documented agentic verified-synthesis loop with published benchmark numbers. |

## What disp could steal

- **The fallback ladder** (automated → interactive → testing) as the checker's
  external interface. Even inside disp's own architecture, "the check failed and
  here is the stuck neutral term" is far more useful to a proposer than `Ok false`.
- **Subgoal decomposition + conflict-driven revision** as the concrete algorithm to
  prototype before any neural work. It needs no model — it is a search strategy.
- **Loom's WP-generation-via-monad-transformer-algebras** as the mechanism for
  turning disp's effect/cost ledger (§8–9) into verification conditions
  automatically, which disp currently has designed but unbuilt.
- **Benchmark discipline.** Verina/Clever-Loom exist; disp has no analogue. Without
  one, "the optimizer works" is unfalsifiable — which FOUNDATIONS Part V already
  identifies as the project's evaluation problem.

## Where disp differs

This stack has **no performance axis at all**. It verifies imperative programs
embedded in Lean; nothing compiles to fast native code, nothing models hardware,
nothing searches for a *cheaper* program — only a *correct* one. disp's scoring
function is `correctness × cost`; WybeCoder's is `correctness`.

Also, the proposer is a frontier LLM at 32 turns × 16 agents per problem — the
"compute-heavy, verifier-does-the-real-work" pattern FOUNDATIONS §15 names as the
graveyard reason. It works, expensively, on benchmark-sized problems.

## Verdict

**The state of the art for the neural half of disp's A6, with the numbers to prove
it, and zero overlap with disp's A5.** The single most useful external result for
calibrating what disp's proposer must beat.

**Distance from disp's goals: solves the proof-search half on benchmarks; ignores
cost, hardware, and self-application entirely.**
