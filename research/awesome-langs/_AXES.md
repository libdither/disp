# The comparison axes

Every file in this directory scores a project against the same five axes, which are
lifted straight out of disp's `GOALS.md` and `FOUNDATIONS.md`. They are the things
disp actually needs; they are *not* a general "is this a good language" scale. A
project can be excellent and score near-zero here.

The five follow the loop disp is built around: what programs are made of, what you
can say about them, why a verdict can be believed, how fast and how accountably they
run, and how an implementation gets found. The first and last close the loop, since
self-application needs the substrate.

| # | Axis | What disp requires | Where it comes from |
|---|------|--------------------|---------------------|
| **G1** | **Substrate** | Programs can take other programs apart and run them directly — no encoding step, no separate quote/eval layer — so the type checker is just an ordinary program in the language | GOALS "programs are data"; FOUNDATIONS §1 (tree calculus) |
| **G2** | **Specification** | A spec can say everything that matters: types that mention values (dependent types or stronger), proofs you can run, resource and cost budgets, and a workable answer to when two different programs count as the same — with the whole type system built as library code, not baked into the core | GOALS "dependently typed or strictly more powerful"; FOUNDATIONS §2 (types as predicates), §6, §7 (equality), §9 (graded coeffects) |
| **G3** | **Trust** | A verdict is only as believable as the code you must audit to accept it, so that code is kept tiny: every clever layer — elaborator, solver, search — is untrusted and must hand a small core replayable evidence for its claims, and only what the core re-checks counts (the tradition LCF started) | FOUNDATIONS §4 (2-op kernel: `bind_hyp`, `hyp_reduce`), §12 |
| **G4** | **Execution** | Runs at C/Rust speed with a faithful model of what the hardware will do, a cost account that is exact and replayable rather than a benchmark, and slow definitions swappable for fast ones only when a checked certificate says they are equal | GOALS bullet 2 and the hardware bullet; FOUNDATIONS §11 (cost), §12 (licensed rewrites), Part V |
| **G5** | **Search** | From spec to implementation automatically: a search over candidate programs scored by the checker (pass/fail) times measured cost — and eventually that optimizer aimed at its own code | GOALS "external optimizer"; FOUNDATIONS §12–15 |

## Rating key

A cell reads `◐ 50% (quotation)`: the percentage of disp's requirement met (three or four
clauses, see the grading section below), the symbol it derives, and a how-tag.

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

The substrate and the trust story (G1, G3) are largely built. The endgame (G5) is
not. Per FOUNDATIONS Part V the spine is equality: disp picked an **intensional**
substrate (`tree_eq` = O(1) pointer identity) for speed, but the optimizer's whole
job is **extensional** (swap a program for a different, faster, equivalent one). In
this grading that is Specification's equality clause and Execution's licensed-rewrite
clause, and nearly every comparison in this directory ends up being about how the
other project handles that same gap.

## Where disp stands

The same scale applied to disp itself, so the website's comparison can draw
disp on its own axes. Update it when the status changes; the site rebuilds
from this table.

| Axis | disp | Note | Clauses |
|---|:--:|---|---|
| G1 Substrate | ✅ 100% (native) | Native intensionality with no quotation layer: `shape_of` triages any program, and the checker is an ordinary tree. | 1 · 1 · 1 — `shape_of` triages any program; the checker is an ordinary tree you apply |
| G2 Specification | ◐ 50% (dependent, library) | Dependent types as library code over the kernel (Pi, telescopes, coproducts, a universe that checks itself); proofs run but the library is small; cost as a grade is designed. Equality: the substrate is intensional (`tree_eq` is pointer identity), one slice of witness-licensed rewrites (map fusion) has landed, and whether a decidable fragment licenses enough rewrites is open question Q1. | 1 · ½(small library) · 0 · ½(witness slices) — Pi, telescopes, coproducts and proofs that run, over a small library; cost as a grade is designed; equality is intensional with witness-licensed slices and the decidable fragment open (Q1) |
| G3 Trust | ✅ 83% (two-op core) | A two-op trusted core (`bind_hyp`, `hyp_reduce`) in the archived kernel; the promoted kernel keeps legitimacy in provenance. MM0's core is smaller and externally verified. | 1 · 1 · ½(one core) — a two-op core mints hypotheses; elaborator and library untrusted; five evaluators agree but nothing independent re-checks the kernel's verdicts |
| G4 Execution | ◐ 38% (interpreted) | Interpreted tree-walkers (the TypeScript oracle, Rust eager at about 2×, the ic-net at a measured 4,000–67,000× penalty); `--stats` steps and `cold_equiv` are a deterministic, replayable cost model; no hardware model; `.opt.disp` overlays replace definitions under a license, landed for one slice. | 0 · 0 · 1 · ½(one slice) — interpreted tree-walkers; `--stats` steps and `cold_equiv` are a deterministic, replayable cost model; no hardware model; `.opt.disp` overlays replace definitions under a license, one slice landed |
| G5 Search | ✗ 0% (designed) | The optimizer is designed (`research/OPTIMIZER.typ`) and unbuilt; the licensing machinery it needs exists as one slice. | 0 · 0 · 0 — the optimizer is designed (`research/OPTIMIZER.typ`), not built; no search exists yet |

## Grading: the maximum and the clauses

The three symbols are coarse. [`_SCORES.md`](_SCORES.md) grades every cell as a percentage
of disp's own requirement: the "What disp requires" column above, split into three or four
clauses. Each clause scores 0, ½ or 1 and the axis is their mean. The maximum is what disp
needs, not the best project in the survey, so a project can be excellent and score low. The
"library code over a tiny kernel" half of G2 is an architecture property the how-tag carries
(`library types`, `dependent, library`) rather than a graded clause.

| Axis | Clauses |
|---|---|
| **G1** | (a) **program inspection** — programs can inspect other programs | (b) **one representation** — the same form is both run and inspected, no quotation layer | (c) **checker as a function** — the checker is callable from programs as an ordinary function (½ from metaprograms only) |
| **G2** | (a) **dependent types** — specifications mention values (½ contracts or refinements over runtime values) | (b) **first-class proofs** — propositions and proofs are objects in the language, with a library in use (½ solver-discharged obligations, or proofs without a library) | (c) **cost in types** — resources and cost as types (½ a usage grade, erasure by quantity, or a static bound that is not a type) | (d) **equality theory** — an equality you can state and check: extensional, cubical, observational (½ propositional or SMT-fragment equality, tactic-driven or a decidable fragment) |
| **G3** | (a) **auditable core** — a trusted core small enough to audit | (b) **evidence objects** — it mints unforgeable evidence: theorems, proof objects, certificates (½ proof objects checked only by a large checker) | (c) **independent re-checking** — the clever layers are untrusted and re-checked by the core, and an independent checker exists (½ re-checked by the one core only) |
| **G4** | (a) **native speed** — C/Rust-class execution (½ compiled but not systems class) | (b) **path to the machine** — a verified compiler or a hardware model in the logic (½ a verified backend for a fragment, or a modelled VM) | (c) **deterministic cost** — the runtime accounts for its own cost: steps, interaction counts, a replayable model (½ timing hints or a report outside the model) | (d) **licensed rewrites** — definitions replaced by faster equivalents on a checked license (½ asserted, like jets or unverified compiler passes, or a fixed set of proved passes) |
| **G5** | (a) **spec → implementation** — implementations found automatically (½ an external LLM loop, harness generation, basic proof search) | (b) **checker × cost** — candidates scored by the checker and by measured cost, both in the loop (½ one of the two) | (c) **self-application** — the search is aimed at itself (½ a flywheel started) |

Rules: a design gets no credit, the how-tag carries it. A ½ must name its route in the
cell — `½(contracts)`, `½(metaprograms)` — the specific half-credit condition it takes,
so no half score exists without a stated reason; the script rejects a bare ½. 1 and 0
stay bare. The symbol is derived: ✗ below 25,
◐ from 25 to 79, ✅ from 80. "Ahead of disp" is derived: a higher percentage than disp's on
that axis. `_SCORES.md` is the source; `scripts/awesome-scores.py --write` recomputes the
percentages and copies them into every write-up's scorecard (score, clause values, why), the
master table, and the table above, and its report ranks each column.
