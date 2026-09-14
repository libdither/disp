# The comparison axes

Every file in this directory scores a project against the same six axes, which are
lifted straight out of disp's `GOALS.md` and `FOUNDATIONS.md`. They are the things
disp actually needs; they are *not* a general "is this a good language" scale. A
project can be excellent and score near-zero here.

| # | Axis | What disp requires | Where it comes from |
|---|------|--------------------|---------------------|
| **A1** | **Reflection / programs-as-data** | Programs can inspect programs without Gödel numbering or quotation, so the type checker is an ordinary program in the language | GOALS "programs are data"; FOUNDATIONS §1 (tree calculus) |
| **A2** | **Spec power** | Dependent types or stronger (HoTT); the type system is *library code* over a tiny kernel, not built into the compiler | GOALS "dependently typed or strictly more powerful"; FOUNDATIONS §2 (types as predicates), §6 |
| **A3** | **Kernel / trust** | LCF-style: a tiny trusted core mints unforgeable evidence, everything clever is untrusted and re-checked | FOUNDATIONS §4 (2-op kernel: `bind_hyp`, `hyp_reduce`), §12 |
| **A4** | **Equality** | A decidable, composable notion of "different program, same behavior" rich enough to *license rewrites* — disp's crux (Q1) | FOUNDATIONS §7, Part V |
| **A5** | **Performance + cost model** | C/Rust-class native execution, plus a measurement primitive that returns *cost* (time/memory) alongside results, and cost as a typing-level resource | GOALS bullet 2; FOUNDATIONS §9 (graded coeffects), §11 |
| **A6** | **Search / self-application** | Spec → implementation automatically: combinatorial search over programs scored by a checker (0/1) × cost, and eventually the optimizer aimed at itself | GOALS "external optimizer"; FOUNDATIONS §12–15 |

## Rating key

A cell reads `◐ 50% (quotation)`: the percentage of disp's requirement met (three clauses,
see the grading section below), the symbol it derives, and a how-tag.

- ✅ — 80% or more: does this at the strength disp wants
- ◐ — 25 to 79%: a weaker or narrower version
- ✗ — under 25%: does not do this, or actively went the other way (a design counts as 0)
- **bold** — in the master table: a higher percentage than disp's, so *ahead of disp* on this axis
- **(tag)** — one or two words naming *how* the level is reached, so two projects with the same symbol still read differently; the website shows it under the symbol

## The one-sentence version of disp

Write a specification as a dependent type, turn the checker into a 0/1 score,
multiply by a hardware-faithful cost score, and search a reflective low-level
calculus for a program that is both provably correct and fast — then turn that
search on itself.

The foundation (A1–A3) is largely built. The endgame (A6) is not. Per
FOUNDATIONS Part V, A4 is the spine: disp picked an **intensional** substrate
(`tree_eq` = O(1) pointer identity) for speed, but the optimizer's whole job is
**extensional** (swap a program for a different, faster, equivalent one). Nearly
every comparison in this directory ends up being about how the other project
handles that same gap.

## Where disp stands

The same scale applied to disp itself, so the website's comparison can draw
disp on its own axes. Update it when the status changes; the site rebuilds
from this table.

| Axis | disp | Note | Clauses |
|---|:--:|---|---|
| A1 Reflection | ✅ 100% (native) | Native intensionality with no quotation layer: `shape_of` triages any program, and the checker is an ordinary tree. | 1 · 1 · 1 — `shape_of` triages any program; the checker is an ordinary tree you apply |
| A2 Spec power | ◐ 67% (dependent, library) | Dependent types as library code over the kernel (Pi, telescopes, coproducts, a universe that checks itself), but far less spec power than Lean, Agda, or F*. | 1 · ½ · ½ — Pi, telescopes, coproducts and proofs that run; a small library (~1,300 pins), no universe hierarchy yet |
| A3 Kernel | ✅ 83% (two-op core) | A two-op trusted core (`bind_hyp`, `hyp_reduce`) in the archived kernel; the promoted kernel keeps legitimacy in provenance. MM0's core is smaller and externally verified. | 1 · 1 · ½ — a two-op core mints hypotheses; elaborator and library untrusted; five evaluators agree but nothing independent re-checks the kernel's verdicts |
| A4 Equality | ◐ 50% (witnesses) | The substrate is intensional (`tree_eq` is pointer identity). One end-to-end slice of witness-licensed rewrites (map fusion) has landed; whether a decidable fragment licenses enough rewrites is open question Q1. | ½ · ½ · ½ — witness-licensed rewrites landed as one slice (map fusion); whether a decidable fragment is rich enough is open (Q1) |
| A5 Perf | ✗ 17% (interpreted) | Interpreted tree-walkers (the TypeScript oracle, Rust eager at about 2×, the ic-net at a measured 4,000–67,000× penalty). Cost as a typing-level resource is designed, not built. | 0 · ½ · 0 — interpreted tree-walkers; `--stats` steps are a deterministic meter outside the language; cost as a grade is designed |
| A6 Search | ✗ 0% (designed) | The optimizer is designed (`research/OPTIMIZER.typ`) and unbuilt; the licensing machinery it needs exists as one slice. | 0 · 0 · 0 — the optimizer is designed (`research/OPTIMIZER.typ`), not built; no search exists yet |

## Grading: the maximum and the clauses

The three symbols are coarse. [`_SCORES.md`](_SCORES.md) grades every cell as a percentage
of disp's own requirement: the "What disp requires" column above, split into three clauses.
Each clause scores 0, ½ or 1 and the axis is their mean, so the scale is 0, 17, 33, 50, 67,
83, 100. The maximum is what disp needs, not the best project in the survey, so a project can
be excellent and score low; "or stronger (HoTT)" is graded under A4, where it does the work, and
the "library code over a tiny kernel" half of A2 is an architecture property the how-tag carries
(`library types`, `dependent, library`) rather than a graded clause.

| Axis | (a) | (b) | (c) |
|---|---|---|---|
| **A1** | programs can inspect other programs | one representation is both run and inspected, no quotation layer | the checker is callable from programs as an ordinary function |
| **A2** | specifications mention values (½ contracts or refinements over runtime values, 1 dependent types) | propositions and proofs are first-class objects (½ obligations discharged by a solver, no proof objects) | breadth in use: universes, inductive families and a proof library people build on (½ a small or young library) |
| **A3** | a trusted core small enough to audit | it mints unforgeable evidence: theorems, proof objects, certificates (½ proof objects checked only by a large checker) | the clever layers are untrusted and re-checked by the core, and an independent checker exists (½ re-checked by the one core only) |
| **A4** | a behavioral equality beyond syntactic identity (½ a decidable fragment or a fixed proved relation) | it is mechanically checkable and composes (½ tactic- or human-driven) | it licenses rewrites: something replaces programs by equivalent ones on its authority (½ one slice or a fixed set of passes) |
| **A5** | C/Rust-class native execution (½ compiled but not systems class) | a primitive returns cost with results (½ a meter or report outside the language) | cost is a typing-level resource (½ a usage grade, erasure, or a static bound that is not a type) |
| **A6** | spec → implementation automatically (½ an external LLM loop, harness generation, basic proof search) | scored by the checker and by cost (½ one of the two) | the search is aimed at itself (½ a flywheel started) |

Rules: a design gets no credit, the how-tag carries it. The symbol is derived: ✗ below 25,
◐ from 25 to 79, ✅ from 80. "Ahead of disp" is derived: a higher percentage than disp's on
that axis. `_SCORES.md` is the source; `scripts/awesome-scores.py --write` recomputes the
percentages and copies them into every write-up's scorecard (score, clause values, why), the
master table, and the table below, and its report ranks each column.
