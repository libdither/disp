# Nova — Russoul (solo)

**Repo:** https://github.com/Russoul/Nova (4★, 473 files, pushed 2026-08-03 — committing daily)
**Written in:** Idris 2. Docs at russoul.github.io/Nova
**Clone inspected:** yes — `README.md`, `docs/` (NovaFoundation, NovaPipeline, NovaElaboration, NovaKernel)

## What it is

A mechanised formal type theory based on **extensional Martin-Löf Type Theory**,
with an architecture the README states precisely:

> surface files elaborate to certificate-carrying artifacts that a small trusted
> kernel re-checks

That is disp's §12 architecture — untrusted clever producer, tiny trusted
re-checker — applied to *elaboration* rather than optimization. Four separate
specs: Foundation (the theory), Pipeline (architecture), Elaboration (surface
syntax), Kernel (the rules).

## Why it matters to disp: the extensional choice

Nova is the one project here that takes the **opposite** side of disp's crux (A4).

- **disp** chose an *intensional* substrate: `tree_eq` is O(1) structural identity,
  the finest possible equality. FOUNDATIONS §7 concedes this "reopens the exact
  wound the field is healing" and calls it the spine of the project.
- **Nova** chose *extensional* type theory: equality reflection collapses
  propositional and definitional equality, so anything you can *prove* equal is
  *treated* as equal by the checker. That is exactly the rewrite-licensing power
  disp's optimizer needs — you get behavioral equivalence for free in the theory.

The catch is the classical one, and it is why ETT is a minority position:
**type checking becomes undecidable**. You cannot re-derive which equality proof
justified a step, so checking requires the proof terms to carry it. Nova's answer
is the certificate pipeline: the elaborator (untrusted, does search) emits
artifacts that record enough evidence for the small kernel to re-check
mechanically. Undecidable-in-general becomes decidable-given-the-certificate.

**This is directly applicable to disp's Q1.** disp is trying to find a *decidable
fragment* of `~_T` rich enough to license rewrites. Nova suggests a different
framing: don't restrict the relation, make the *optimizer* emit the evidence and
keep the checker dumb. disp's §12 already says this for rewrites; Nova shows what
it looks like when applied to the whole type theory.

## Scorecard

| Axis | Nova | Note |
|---|---|---|
| A1 Reflection | ✗ | Standard elaborator/kernel pipeline; no programs-as-data. |
| A2 Spec power | **✅** | Full extensional MLTT — strictly stronger equality story than disp has. |
| A3 Kernel | ✅ | Explicit small trusted kernel with its own spec document; certificate-carrying artifacts. Same shape as disp's. |
| A4 Equality | **✅** | Extensional — the thing disp needs, obtained by paying undecidability and recovering it with certificates. |
| A5 Perf | ✗ | Written in Idris 2, research-scale. No native codegen, no cost model, no systems ambition. |
| A6 Search | ✗ | No synthesis. |

## What disp could steal

- **The four-document discipline** (Foundation / Pipeline / Elaboration / Kernel as
  separate specs). disp's docs table shows TYPE_THEORY.typ at 5/10 quality and
  several kernel files at 5/10; Nova's split is a model for what "the kernel is
  source code written to be read" wants to become.
- **The certificate-carrying-artifact shape as a general answer to A4.** Worth
  reading `docs/NovaKernel.txt` specifically for what the kernel demands from the
  elaborator — that interface is the analogue of what disp's optimizer must emit.
- **Evidence that ETT is buildable by one person.** Nova is a solo project with a
  working elaborator/kernel split, which is a useful existence proof given disp is
  also solo-plus-AI.

## Where disp differs

disp is trying to get extensional *power* on an intensional *substrate* by
building a licensing relation (`~_T`) upward from cheap structural equality.
Nova starts extensional and pays with certificates. disp's route buys O(1)
conversion and a fast checker; Nova's buys the equality theory outright but has no
performance story at all.

The honest read: **Nova has already solved the half of A4 that disp calls its
make-or-break question — by choosing a theory where it isn't a question.** What
Nova cannot do is anything on A5/A6, which is most of why disp exists.

## Verdict

**The most theoretically instructive small project in this survey, and the one
whose architecture most resembles disp's intended optimizer/checker split.**
Tiny (4 stars) and unknown, but the author is an Idris 2 contributor committing
daily and writing real specs.

**Distance from disp's goals: solves A4 differently and better; absent on A1, A5,
A6.**
