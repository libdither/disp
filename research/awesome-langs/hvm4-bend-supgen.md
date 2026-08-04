# HVM4 / Bend2 / SupGen — Victor Taelin, Higher Order Co

**Repo:** https://github.com/HigherOrderCO/HVM4 (334 files, C; pre-launch, pushed 2026-05-30)
**Frozen predecessor:** https://github.com/HigherOrderCO/Bend (19,777★, HVM2-era, "chore: bump repo activity" 2026-07-07). Bend2 is not public yet.
**Clone inspected:** yes — `docs/primer.md`, `docs/hvm/collapser.md`, `docs/theory/interaction_calculus.md`, `src/hvm.c`

## What it is

A C runtime (single file, `src/hvm.c`) for the **Interaction Calculus** — Lafont
interaction nets with explicit duplication (`DUP`) and superposition (`SUP`) nodes.
Variables are **affine** (used at most once); using one twice requires an explicit
cloned binder `λ&x`, which inserts duplication nodes. On top of this Taelin is
building Bend2 (the surface language, planned to carry "a complete proof system
like Lean and Kind") and **SupGen/NeoGen**, a program synthesizer.

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

SupGen is the same idea aimed at program holes: put a hole anywhere, the runtime
enumerates superposed candidates until tests/proofs pass. Taelin claims a
previously-intractable O(n log n) sort found this way, and — importantly — it is
**not an LLM**; it is enumerative search exploiting optimal sharing.

## Scorecard

| Axis | HVM4/Bend2 | Note |
|---|---|---|
| A1 Reflection | ◐ | Terms are runtime graph nodes; no in-language checker-as-program discipline. Untyped core. |
| A2 Spec power | ◐ | Bend2 *plans* dependent types + proofs; HVM4 itself is untyped. Not shipped. |
| A3 Kernel | ✗ | No LCF kernel, no evidence discipline. Trust = trust `hvm.c`. |
| A4 Equality | ✗ | Affine/optimal-sharing semantics, no equivalence-licensing story at all. |
| A5 Perf | **✅** | Native C, AOT compilation of superposition-bearing functions to machine code, claimed 10–100× over interpretation; GPU lineage from HVM2. |
| A6 Search | **✅** | The only project anywhere with superposition-based program search *in the runtime*. |

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

The deeper difference: HVM4 has no types, so its search is filtered by tests and
equations, not by a dependent-type oracle. disp's entire bet (FOUNDATIONS §13) is
that a dependent spec is a far stronger oracle than test cases. HVM4 is the
strongest existing evidence that the *search substrate* can work; it says nothing
about whether the *verification oracle* scales.

## Verdict

**The closest living relative to disp's endgame, and the only project that has
built the search half.** It is missing everything disp built (types, kernel,
metacircularity, equality) and disp is missing what it built (a fast net with
working superposition search). Risk: Taelin's restart pattern (HVM2→3→4,
Kind→Kind2→Bend2) means the proof system has been "planned" for years.

**Distance from disp's goals: closest on A5/A6, absent on A1–A4.**
