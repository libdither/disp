# Soma — Gabriel Minatel (solo)

**Repo:** https://github.com/SrGaabriel/soma (73★, 563 files, pushed 2026-06-03)
**Written in:** Lean 4 (compiler) + Rust (runtime/svm) + LLVM backend
**Clone inspected:** yes — README, `compiler/`, `runtime/`, `backend/`

## What it is

A dependently-typed, pure functional language that **compiles to native code via
LLVM**, using **interaction nets as a compilation model** to get deterministic,
GC-free memory management with no annotations, no reference counting, and no
`free` calls. Quantities (`0`/`1`/`ω`) from **Quantitative Type Theory** appear in
binders; the compiler uses them to place duplication and erasure operations.

Feature list: dependent types, row polymorphism, type classes, first-class linear
types, open variants.

## Why it matters to disp: same three ingredients, opposite bet

Soma combines *dependent types + QTT grading + interaction nets + native codegen* —
which is, ingredient for ingredient, disp's A2 + A5 + §9 + §11. It is the closest
technical cousin in this entire survey. And it makes the opposite call on the
central question:

> **Soma is not Lévy-optimal.** It does not share the reduction of duplicated
> redexes the way HVM does. The bet is that native code with good cache behavior
> wins over graph rewriting for most programs and that QTT eliminates most of the
> cases where sharing would help anyway.

disp made the *other* opposite call (drop hash-consing to keep cost attributable),
and HVM4 made the third (keep optimal sharing, lose attribution). Three projects,
three different points on the same tradeoff, and **disp has measured its own**:
dropping memoization inflates work 4,000–67,000× (600× raw speed loss).

Soma's tiering is the pragmatic engineering answer disp doesn't currently have:

- **Flat types** (ints, bools, floats, all-flat structs): duplication is a register
  copy, erasure is a no-op — *zero overhead, same as C*.
- **Heap types** (closures, tagged unions, recursive data): duplication creates a
  SUP node deferring the copy; if only one side is ever accessed, SUP and ERA
  **annihilate and no copy happens**.

So "defensive duplication costs nothing when only one branch executes" — the
interaction-net machinery is used as a *compile-time memory-management discipline*
that lowers to ordinary native code, rather than as a runtime graph-rewriting
engine. That is a genuinely different way to cash in the same theory.

## Scorecard

| Axis | Soma | Note |
|---|---|---|
| A1 Reflection | ✗ | No programs-as-data; conventional compiler pipeline. |
| A2 Spec power | ✅ | Dependent types + QTT + row polymorphism. Real, though not a proof assistant. |
| A3 Kernel | ✗ | No LCF kernel; trust the compiler. |
| A4 Equality | ✗ | Not addressed. |
| A5 Perf | **✅** | LLVM native, GC-free, flat types at C cost. Best A5 among the dependently-typed indies. |
| A6 Search | ✗ | None. |

## What disp could steal

- **The flat/heap tiering.** disp's ic-net pays the no-memo tax uniformly. Soma's
  observation — that most values are flat and duplication of flats is *free* — is a
  cheap way to recover a large constant factor without touching the sharing
  semantics for the cases that matter. disp's measured 4,000–67,000× inflation is
  exactly the kind of number a tier split attacks.
- **SUP/ERA annihilation as the memory story.** disp currently frames interaction
  nets as the *search* substrate (superposition = candidate exploration). Soma
  shows the same nodes doing *memory management*. If disp's nets serve both roles,
  the cost model gets a second consumer, which strengthens §9.
- **QTT quantities in binders as the grading carrier.** disp wants cost as one axis
  of a graded semiring alongside usage/sharing/staging (§9). Soma ships the usage
  axis in a real compiler with a native backend — the closest working precedent for
  the ledger disp designed.

## Where disp differs

Soma is a *language*, not a *system that optimizes itself*. It has no reflection,
no kernel, no equality theory, no synthesis. Its dependent types serve program
correctness in the ordinary way; nothing turns a type into a search objective.

And its central bet — "native code beats graph rewriting" — is a bet **against**
the substrate disp is building the optimizer on. If Soma is right for general
programs, disp's ic-net only pays off *for search*, which sharpens rather than
weakens disp's Q2: the nets must earn their keep by making superposition
affordable, not by being a good way to run ordinary code.

## Verdict

**The technical cousin: same four ingredients, assembled for speed instead of
search.** Small and young, but a real compiler with a real backend by a competent
solo developer. The flat/heap tiering is the single most directly stealable idea
in this directory.

**Distance from disp's goals: shares A2 and beats disp on A5; nothing on A1, A3,
A4, A6.**
