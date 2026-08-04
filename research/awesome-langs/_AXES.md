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
