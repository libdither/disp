# Acorn — Kevin Lacker

**Repos:** https://github.com/acornprover/acorn (48★, ~4,600 commits since Oct 2024, last commit 2026-07-15) + `acornlib` (the library) + VS Code extension
**Site:** acornprover.org (blog active through 2026-05-26)
**Author:** Parse co-founder (acquired by Facebook), ex-Google, two-time Putnam Fellow.
**Clone inspected:** yes — `src/`, `python/` (model training), README, AGENTS.md

## What it is

A theorem-proving language written in Rust with an **AI proof assistant built into
the prover itself**. The workflow: a human states the theorem and sketches the
structure; the built-in model fills in the details; everything is verified by
Acorn's own checker. The intended loop is self-reinforcing — as `acornlib` grows,
it becomes training data for the next model.

Critically, the model is **local and embedded**: `python/` trains with PyTorch and
exports **ONNX**, which the Rust prover loads at runtime. This is not an API call to
a frontier lab; it is a learned proposer shipped inside the binary.

## Why disp should care

**This is FOUNDATIONS §15's neural proposer, built by one person, running locally,
inside the checker.** Every other AI-verification result in this survey (WybeCoder,
AutoVerus, AlphaVerus, Aristotle) calls out to a large frontier model. Acorn
demonstrates the alternative disp will eventually need: a *small, domain-trained,
locally-runnable* proposer, tightly coupled to the verifier, with the verified
library as the training corpus.

For disp specifically, the "library becomes training data" flywheel is the
mechanism that makes GOALS' **self-play** requirement ("continually improve by
generating score functions itself to then satisfy") concrete. Acorn is the smallest
existing worked example of that loop.

## The disqualifying caveat

**Acorn is math-only.** Nothing on the site, blog, or the Zero Knowledge podcast
(ep. 382, Oct 2025) indicates a software-verification orientation. There is no
systems language, no native codegen, no cost model, no programs-as-data. Its
`acornlib` targets Freek's Top 100 theorem list.

So Acorn is a *methodology* reference for disp, not a design competitor. The
transferable question is: does the "small local model + verifier + growing library"
flywheel work when the corpus is *programs with cost* rather than *theorems*?
Nobody has tried, and disp's substrate (cheap checker, dense cost signal) is
arguably a better fit for it than Lean's.

## Scorecard

| Axis | Acorn | Note |
|---|---|---|
| A1 Reflection | ✗ | None. |
| A2 Spec power | ◐ | A real theorem-proving language, but aimed at mathematics; not dependent-type-strength program specification. |
| A3 Kernel | ◐ | Has its own checker; TCB not characterized as an LCF kernel. |
| A4 Equality | ✗ | Not addressed for program rewriting. |
| A5 Perf | ✗ | Not a systems language at all. |
| A6 Search | **✅** | The only project here with a *locally-trained, embedded* neural proposer in the verification loop. |

## What disp could steal

- **The embedded-ONNX-proposer architecture.** When disp reaches §15, the design
  question is "frontier API or local model?" Acorn is the existence proof that
  local works, that the training pipeline is a person-sized project, and that
  PyTorch→ONNX→Rust is a viable path (disp already has Rust backends).
- **Library-as-training-corpus flywheel** as the concrete reading of GOALS'
  self-play clause.
- **The interaction model**: human states the goal and the *structure*, machine
  fills details. disp's optimizer is envisioned as fully automatic; Acorn suggests
  the intermediate product (human sketches, machine completes) is both more
  achievable and independently useful — a plausible first milestone that is not
  "solve superoptimization."

## Verdict

**Not a competitor — the best available template for the neural half of disp's
endgame, built at solo scale by a highly credible engineer, and aimed at the wrong
domain.**

**Distance from disp's goals: zero overlap on A1–A5; the closest methodological
model for A6's proposer.**
