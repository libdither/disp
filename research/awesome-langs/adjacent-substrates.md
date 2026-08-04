# Adjacent substrates — ideas disp's design already cites

Not competitors; components. Each of these owns one piece of disp's design, and in
several cases owns it better than disp currently does.

## egg / equality saturation — **the closest thing to disp's `~_T` machinery**

**Repo:** egraphs-good/egg · Tate et al. 2009; Willsey et al. 2021 · ROVER (arXiv:2406.12421)

E-graphs represent *exponentially many equivalent programs* compactly and apply
rewrites non-destructively until saturation, then extract the cheapest member by a
cost function. That is — almost exactly — disp's optimizer: a space of equivalent
programs, a cost metric, extraction of the best one.

FOUNDATIONS §12 cites it, and the crucial recent development is **certificate
emission**: ROVER and "Small Proofs from Congruence Closure" make e-graph rewriting
emit proofs that a **Lean kernel replays**. That is disp's untrusted-optimizer +
trusted-re-checker architecture, working, today, for rewrite systems.

**Why this is the most actionable item in this file:** disp's Q1 asks for a
decidable, locally-composable fragment of `~_T` rich enough to license real
rewrites, and disp's chosen first milestone is *one* certified rewrite (map fusion)
re-checked by the trusted kernel. E-graphs are the standard, well-tooled way to
manage exactly that, and the certificate work removes the "but can I trust it"
objection. disp is building a bespoke version of a solved subproblem.

The catch, which disp should be clear-eyed about: e-graphs handle *equational*
rewriting over first-order terms well and higher-order/dependent settings poorly,
and extraction under sharing is NP-hard in general. But as a starting point for Q1,
it beats starting from nothing.

## Unison — content-addressing as identity

Cited in FOUNDATIONS §1. Definitions are identified by the hash of their AST, so
renaming is free, dependency conflicts largely vanish, and code is
distribution-ready. disp uses the same trick (hash-consed trees, deterministic
elaboration → *same type, same tree*, O(1) conversion).

The limitation FOUNDATIONS already names is the important one: content-addressing
gives *syntactic* identity, not *semantic* equality — two behaviorally identical
definitions still hash differently. So Unison validates disp's substrate choice and
simultaneously demonstrates that it does not solve A4.

## Granule — coeffects, the multi-axis version

Orchard et al. Granule ships graded modal types with *multiple* grades
(linearity, security levels, intervals). This is the research vehicle closest to
disp's §9 plan of "cost × usage × sharing × staging as axes of one semiring."
Where Idris 2 ships one axis in a practical language, Granule explores several in a
research one. If disp is going to attempt the four-axis ledger, Granule's published
experience is the only prior art on whether the metatheory composes.

## Koka / Effekt / Neut — effects and memory without GC

- **Koka** (Daan Leijen, 3,997★, v3.2.3): algebraic effect handlers + **Perceus**
  reference counting with reuse analysis — in-place mutation of functional code
  when refcount is 1. Relevant to disp's §8 (effects as values) and to any
  future story about making disp's pure substrate fast.
- **Effekt** (453★, v0.75.0 released 2026-08-03): lexical effect handlers with
  lightweight effect polymorphism — the ergonomic frontier of effect typing.
- **Neut** (vekatze, 1,010★): static memory management via a box modality, no GC,
  no borrow checker.

None does verification. They matter because disp's §8 bet is that effects fall out
of purity, and its measurement primitive (run external code, return cost) is an
effect. These three are where the effect-system engineering knowledge lives.

## Cedille / CDLE — the φ trick

Stump, 2018–2020. λ-encodings *with* induction and **definitionally zero-cost
coercions** — cited in FOUNDATIONS §7 as one of three responses to the equality
problem. Largely quiescent since ~2021, which is itself the datum: the most elegant
of the three answers is the one that stopped.

## Wuffs (Google, 4,797★) — proofs at C speed, shallow but shipped

A special-purpose language for parsing untrusted formats where the programmer
states *facts* and the compiler proves bounds/overflow safety at compile time, then
transpiles to C with zero runtime cost. The specs are shallow (safety, not
functional correctness) — but it is a real, widely-deployed instance of
"programmer writes assertions, compiler discharges them, output is C-fast," which
is disp's A2+A5 at 5% of the ambition and 100% of the delivery.

## What disp should do with this file

One concrete recommendation: **evaluate egg/e-graphs seriously before building more
bespoke rewrite machinery for Q1.** The rest are reference material for §8/§9 when
those get built.
