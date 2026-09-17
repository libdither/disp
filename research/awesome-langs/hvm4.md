# HVM4 / Interaction Calculus — Victor Taelin, Higher Order Co

**Repo:** https://github.com/HigherOrderCO/HVM4 (C, 121★; README still says "you're here before launch"; last pushed 2026-05-30; verified 2026-09-17)
**Lineage:** HVM1 (2022, Rust) → HVM2 (2024, Rust/CUDA, the runtime under Bend 1) → HVM3 (Haskell, last pushed 2026-01-29) → HVM4 (this repo) → `hvm5.c` (private; a port of it ships as a demo inside Bend 2). **Bend 2 runs on none of them** — it dropped interaction nets for compiled C; see [`bend2.md`](bend2.md).
**Clone inspected:** yes — `docs/primer.md`, `docs/hvm/collapser.md`, `docs/theory/interaction_calculus.md`, `src/hvm.c`

## What it is

A C runtime (single file, `src/hvm.c`) for the **Interaction Calculus** — Lafont
interaction nets with explicit duplication (`DUP`) and superposition (`SUP`) nodes.
Variables are **affine** (used at most once); using one twice requires an explicit
cloned binder `λ&x`, which inserts duplication nodes. It is untyped. The typed
language the company built on top, Bend 2, does not use it: the runtime line and
the language line split in 2026, and this is the runtime line, dormant since May.

## The mechanism that matters to disp

Two things, and they are exactly disp's §11 and §13.

**1. Label-coordinated duplication is real and shipped.** HVM4's `SUP`/`DUP` nodes
carry labels, and the interaction rules are:

- **same label** → DUP and SUP *annihilate pairwise* (extraction: `!x&A = &A{1,2}; [x₀,x₁]` → `[1,2]`)
- **different labels** → they *commute*, producing a cross product (`[&A{1,2}, &B{10,20}]` → 4 results)

This is precisely the "label-coordinated duplication" disp names as the open half
of make-or-break **Q2** ("can sound duplication recover enough sharing to make
superposition search affordable?"). HVM4 does not answer disp's version of the
question — it is untyped, so it never has to make *type checking* distribute over
duplication, which is where disp measured the conjecture failing (affine-only,
provably false for any recognizer that projects twice). But the label discipline
itself is a working artifact, not a proposal.

**2. Search is a native runtime feature, not a tool on top.** From the primer:

```hvm
@X = &N{0n, 1n+@X}                          -- an infinite superposition of ALL naturals
@main = @if(@eq(@add(@X, 2n), 4n), @X, &{})  -- solve X + 2 = 4
//2n
```

An infinite superposed candidate space, collapsed against a predicate, with shared
work across candidates. That is disp's optimizer loop (`GOALS.md`: "combinatorial
search over programs") expressed in three lines at the substrate level. The
**collapser** (CNF readback) is the enumeration engine: breadth-first with a
priority queue, `↑` to control ordering, `-C10` to limit output.

What is shipped is search for a *value*. The program-hole filler built on the same
idea — announced as SupGen, later NeoGen, claimed to find recursive functions from
tests or theorems and to be enumerative rather than an LLM — has never been
published. The company's paid proving agent, Bender, lists SupGen as a future
update, and Bend 2's own README says it has no proof search.

## Scorecard

| Axis | HVM4 | Note | Clauses |
|---|---|---|---|
| G1 Substrate | ◐ 33% (graph nodes) | Terms are runtime graph nodes; no in-language checker-as-program discipline. Untyped core. | ½(runtime graphs) · ½(graphs only) · 0 — terms are runtime graph nodes with no in-language inspection; untyped, no checker |
| G2 Specification | ✗ 0% (untyped) | Untyped. The typed language became Bend 2, which does not run on this runtime. | 0 · 0 · 0 · 0 — untyped; the typed language became Bend 2, which does not run on this runtime |
| G3 Trust | ✗ 0% (trust hvm.c) | No kernel, no evidence discipline. Trust = trust `hvm.c`. | 0 · 0 · 0 · 0 — trust is `hvm.c` |
| G4 Execution | ◐ 70% (native, GPU) | Native C, AOT compilation of superposition-bearing functions to machine code, claimed 10–100× over interpretation; GPU lineage from HVM2; `-s` reports interaction counts. | 1 · 0 · 1† — native C with GPU lineage; the runtime reports interaction counts; no verified path |
| G5 Search | ◐ 40% (value enumeration) | The only runtime anywhere with superposition-based search built in; what ships enumerates values against an equation. The program synthesizer (SupGen/NeoGen) is unpublished. | ½(value enumeration) · ½(tests only) · 0 — the primer solves X + 2 = 4 by collapsing an infinite superposition of naturals; the program-hole filler (SupGen, NeoGen) was never published; equations rather than a checker, no cost objective |

## What disp could steal

- **The label algebra as implemented.** disp's Q2 is HVM4's shipped feature set.
  Reading `src/hvm.c`'s DUP/SUP interaction rules and the collapser's priority
  queue is the cheapest available experiment for disp's `sup_λ` prototype.
- **The collapse-as-readback framing.** disp needs to enumerate candidates *out* of
  a superposed net; HVM4's CNF (quote to kill DUPs, lift to kill SUPs) is a worked
  algorithm for exactly that.
- **`↑` priority as a search-ordering knob** — a cheap way to steer enumeration
  before any learned proposer exists.

## Where disp differs

disp deliberately **dropped hash-consing** in `rust-ic-net` to keep cost
attributable per candidate (provenance-preserving credit assignment for the
reverse-mode optimizer). HVM4 keeps optimal sharing and pays for it with no cost
attribution — it can tell you *an* answer, not *which candidate cost what*. These
are opposite choices on the same tradeoff, and disp has *measured* its side
(4,000–67,000× work inflation, 600× raw speed loss). HVM4 has the faster substrate;
disp has the one that can score candidates.

The deeper difference: HVM4 has no types, so its search is filtered by equations,
not by a dependent-type oracle. disp's entire bet (FOUNDATIONS §13) is that a
dependent spec is a far stronger oracle than test cases. HVM4 is the strongest
existing evidence that the *search substrate* can work; it says nothing about
whether the *verification oracle* scales. Its own company answered that question
by walking away: Bend 2 kept the types and left the nets.

## Verdict

**The only shipped label algebra and the only runtime with superposition search
built in, and it is dormant.** Untouched since May 2026, "before launch", while
the company's product moved to a runtime without nets. The code is still the
cheapest experiment disp can run on Q2. Risk: the restart pattern (HVM2→3→4→5) is
still running — `hvm5.c` exists privately — so any dependence on this repo's
exact rules should expect them to move.

**Distance from disp's goals: closest on the G4/G5 substrate, absent on G1–G3.**
