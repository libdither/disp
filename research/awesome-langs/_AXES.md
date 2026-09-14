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

- ✅ — does this, in production, at the strength disp wants
- ◐ — partial: does a weaker or narrower version, or has it designed but unbuilt
- ✗ — does not do this / actively went the other way
- **bold** — this project is *ahead of disp* on this axis and is worth stealing from
- **(tag)** — one or two words after the symbol naming *how* the level is reached (`◐ (quotation)`, `✅ (native)`), so two projects with the same symbol still read differently; the website shows it under the symbol

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

| Axis | disp | Note |
|---|:--:|---|
| A1 Reflection | ✅ (native) | Native intensionality with no quotation layer: `shape_of` triages any program, and the checker is an ordinary tree. |
| A2 Spec power | ◐ (dependent, library) | Dependent types as library code over the kernel (Pi, telescopes, coproducts, a universe that checks itself), but far less spec power than Lean, Agda, or F*. |
| A3 Kernel | ✅ (two-op core) | A two-op trusted core (`bind_hyp`, `hyp_reduce`) in the archived kernel; the promoted kernel keeps legitimacy in provenance. MM0's core is smaller and externally verified. |
| A4 Equality | ◐ (witnesses) | The substrate is intensional (`tree_eq` is pointer identity). One end-to-end slice of witness-licensed rewrites (map fusion) has landed; whether a decidable fragment licenses enough rewrites is open question Q1. |
| A5 Perf | ✗ (interpreted) | Interpreted tree-walkers (the TypeScript oracle, Rust eager at about 2×, the ic-net at a measured 4,000–67,000× penalty). Cost as a typing-level resource is designed, not built. |
| A6 Search | ◐ (designed) | The optimizer is designed (`research/OPTIMIZER.typ`) and unbuilt; the licensing machinery it needs exists as one slice. |

## Grading: the maximum and the clauses

The three symbols are coarse. [`_SCORES.md`](_SCORES.md) grades every cell as a percentage
of disp's own requirement: the "What disp requires" column above, split into three clauses.
Each clause scores 0, ½ or 1 and the axis is their mean, so the scale is 0, 17, 33, 50, 67,
83, 100. The maximum is what disp needs, not the best project in the survey, so a project can
be excellent and score low; "or stronger (HoTT)" is graded under A4, where it does the work.

| Axis | (a) | (b) | (c) |
|---|---|---|---|
| **A1** | programs can inspect other programs | one representation is both run and inspected, no quotation layer | the checker is callable from programs as an ordinary function |
| **A2** | specifications mention values (½ contracts or refinements over runtime values, 1 dependent types) | propositions and proofs are first-class objects (½ obligations discharged by a solver, no proof objects) | the type system is library code over a kernel, not built into the compiler |
| **A3** | a trusted core small enough to audit | it mints unforgeable evidence: theorems, proof objects, certificates (½ proof objects checked only by a large checker) | the clever layers are untrusted and re-checked by the core, and an independent checker exists (½ re-checked by the one core only) |
| **A4** | a behavioral equality beyond syntactic identity (½ a decidable fragment or a fixed proved relation) | it is mechanically checkable and composes (½ tactic- or human-driven) | it licenses rewrites: something replaces programs by equivalent ones on its authority (½ one slice or a fixed set of passes) |
| **A5** | C/Rust-class native execution (½ compiled but not systems class) | a primitive returns cost with results (½ a meter or report outside the language) | cost is a typing-level resource (½ a usage grade, erasure, or a static bound that is not a type) |
| **A6** | spec → implementation automatically (½ an external LLM loop, harness generation, basic proof search) | scored by the checker and by cost (½ one of the two) | the search is aimed at itself (½ a flywheel started) |

Rules: a design gets no credit, the how-tag carries it. The derived symbol is ✗ below 25,
◐ from 25 to 79, ✅ from 80. "Ahead of disp" is derived: a higher percentage than disp's on
that axis. `scripts/awesome-scores.py` recomputes the percentages, lists every derived
symbol that differs from the master table, and ranks each column.
