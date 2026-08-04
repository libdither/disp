# Idris 2 — Edwin Brady

**Repo:** https://github.com/idris-lang/Idris2 (3,030★, v0.8.0 released 2025-10-31, pushed 2026-07-29)
**Relevance:** the shipping implementation of **Quantitative Type Theory** — the
formalism disp's §9 cost-as-graded-coeffect design is built on.

## Why disp should care: QTT is disp's cost ledger, one axis at a time

FOUNDATIONS §9 states disp's plan: treat **cost, usage, sharing, and staging as
axes of one semiring**, so that memoization, partial evaluation, JIT, and AOT
become the *same* graded rewrite at different points of "how much input is fixed."
The named lineage is Petricek/Orchard/Mycroft coeffects, Granule, and **QTT
(Atkey 2018; McBride 2016), shipping in Idris 2 (Brady 2021)**.

Idris 2 is what that looks like when it actually ships. Every binder carries a
quantity — `0` (erased at runtime, present only for typing), `1` (used exactly
once), or `ω` (unrestricted):

```idris
0 n : Nat        -- erased: no runtime representation
1 x : Int        -- linear: consumed exactly once
```

The `0` quantity is the load-bearing one for disp's purposes: it makes
**erasure a typing-level fact** rather than a compiler heuristic. Idris 2 knows
statically that a proof term contributes nothing at runtime, so dependent types
stop costing performance. That is precisely the property disp needs if a *dependent
specification* is going to sit on top of code that must run at C speed — the spec
must be provably free.

And FOUNDATIONS' honest caveat is confirmed by Idris 2's experience: **most
implementations expose one grade (linearity/usage) and stop.** Idris 2 ships the
usage axis. It does not ship cost, sharing, or staging as additional grades. disp's
plan to unify four axes in one semiring with usable ergonomics *and* tractable
metatheory is, as §9 says, unsolved — and Idris 2 is the evidence for how much work
one axis is.

## Other things Idris 2 does that disp's design touches

- **Elaborator reflection**: Idris 2 exposes its elaborator so users can write
  type-directed metaprograms — the same A1-by-quotation approach as Agda/Lean.
- **Multiple backends** (Chez Scheme default, RefC, JS): a plurality discipline
  closer to disp's §10 than most languages, though without byte-identity demands.
- **Totality checking** as a separate, queryable judgement — relevant because disp
  keeps evaluation total and eager as its decidability strategy (§2).

## Scorecard

| Axis | Idris 2 | Note |
|---|---|---|
| A1 Reflection | ◐ | Elaborator reflection, quotation-based. |
| A2 Spec power | ✅ | Full dependent types + QTT. |
| A3 Kernel | ✗ | No LCF kernel; typechecker is the TCB. |
| A4 Equality | ✗ | Intensional MLTT; no extensional story. Same wall as Lean, without mathlib's social solution. |
| A5 Perf | ◐ | Chez backend is respectable but not C/Rust-class. **Erasure-by-quantity is genuinely relevant** to making specs free. No cost model. |
| A6 Search | ✗ | Proof search (`auto`) is basic. No synthesis, no optimizer. |

## What disp could steal

- **`0`-quantity erasure as the mechanism that makes dependent specs
  performance-free.** disp intends specs to sit over fast code; Idris 2 has the
  shipped design for proving the spec costs nothing. This is the most directly
  reusable idea.
- **The realistic scoping lesson for §9.** Before attempting a four-axis semiring
  ledger, implement *one* axis end-to-end (usage or cost) and see what it costs in
  ergonomics and metatheory. Idris 2 took years on one axis with a strong theorist
  driving it.
- **Backend plurality without byte-identity** as a cheaper intermediate discipline
  than disp's current five-way agreement requirement.

## Where disp differs

Idris 2 is a conventional dependently-typed language with a good grading story and
no ambitions on A4 or A6. Its type system is built into the compiler, not written
as library code over a two-op kernel; there is no reflective substrate, no rewrite
licensing, no optimizer.

The interesting asymmetry: disp wants QTT-style grading *for the optimizer's
benefit* (cost as a searchable objective), whereas Idris 2 uses it *for the
programmer's benefit* (linearity, erasure). Same formalism, different consumer —
and disp's consumer is far more demanding, because a search procedure needs the
grades to be *compositional and cheap to compute*, not just checkable.

## Verdict

**The shipping precedent for disp's grading design, and the realistic estimate of
what one axis costs.** Also the cleanest answer to "how do dependent specs avoid
slowing down the generated code."

**Distance from disp's goals: shares A2, provides the A5-adjacent erasure
mechanism, nothing on A1's substrate question, A4, or A6.**
