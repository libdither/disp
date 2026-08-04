# LogosLang — Thobias Knudsen (solo)

**Repo:** https://github.com/ThobiasKnudsen/LogosLang (9★, 120 files, created 2026-06-17, pushed 2026-07-26)
**Site:** logoslang.dev · **Clone inspected:** yes — README, `DESIGN.md`, `language_sketch.logos`, `seed/`

## What it is

> *"A self-hosting systems language where the compiler, types, proofs, and syntax
> all live in one structure the language can read and rewrite."*

That single sentence is the closest anyone in the indie tier comes to disp's
thesis. The structure is called the **Logic Graph**: source parses into it, the
interpreter walks it, and — per the design — the language can inspect and rewrite
it, with the borrow checker, proof layer, rewriting engine, and standard library
all specified as things that live *in* the graph rather than in the compiler.

## Honest status

The README is unusually forthright: this is "the bootstrap seed," and the
**borrow checker, proof layer, rewriting engine, and standard library are
specified and not yet built.** What ships today is a parser, a Logic Graph
interpreter, and `.compile()` — an explicit, source-directed call that lowers a
function to machine code via **Cranelift**.

So: A1 partially real (the graph is walkable and reflectable), A2 and A4 are
design documents, A5 is measured and real.

## The performance table (measured, one core, Ryzen 7 5800U, July 2026)

| Runner | ns/iter | vs C |
|---|---|---|
| C, `gcc -O2` (auto-vectorized) | 0.47 | 1.0× |
| Rust, `rustc -O` (scalar) | 0.95 | 2.0× |
| **Logos, after `.compile()`** | **1.4** | **3.0×** |
| **Logos, interpreted** | **140** | **~300×** |
| Python 3.13 | ~170 | ~360× |

This is a genuinely useful data point for disp, because it measures the exact
tradeoff disp lives on: **a fully reflectable graph walk costs ~300× native**, and
a JIT call to Cranelift recovers to within 3× of vectorized C. disp's interpreted
tree-walking is in the same architectural class, and disp's own numbers (2× generic
walker tax, 8 GB test heaps, ic-net's 4,000–67,000× inflation) suggest it is
currently paying more than Logos does.

The author's framing is also the right one: *"interpreted Logos sits in CPython's
class while staying a graph walk over fully reflectable structure, and one
`.compile()` call puts the same function within about 1.5× of Rust's scalar code."*
Reflection is affordable **if you can drop out of it on demand**.

## Scorecard

| Axis | LogosLang | Note |
|---|---|---|
| A1 Reflection | ◐ | The Logic Graph is the design's centerpiece and is walkable today; self-rewriting is specified, not built. |
| A2 Spec power | ◐ (design only) | A "proof layer" is specified. No type theory exists yet. |
| A3 Kernel | ✗ | No kernel discipline described. |
| A4 Equality | ✗ | The "rewriting engine" is named but unspecified. |
| A5 Perf | ✅ | Cranelift JIT, measured, 3× vectorized C. Ships binaries for three platforms. |
| A6 Search | ✗ | None. |

## What disp could steal

- **Source-directed `.compile()` as the reflection escape hatch.** disp's cost
  problem is that everything runs through the generic walker. Logos makes
  compilation an *explicit act in the source* ("compilation is directed in source,
  never by compiler flags"), which is a clean way to have a fully reflective
  substrate and native speed in the same program without a sufficiently-smart
  compiler. For disp this maps onto GOALS bullet 2 (outsource execution to a faster
  language and return the cost) — Logos shows the ergonomics of the user-facing
  half.
- **Publishing a measured comparison table.** disp has `perf_logs/` and benchmarks
  but no public "here is what reflection costs and what we recover" statement.
  Logos's table is a model for how to make Q6 legible.

## Where disp differs

disp has actually built the parts LogosLang has only specified: a real type system
(A2), a kernel (A3), an equality story (A4, even if unfinished), and ~1,200 tests.
LogosLang has built the part disp has not: a JIT that makes the reflective
representation fast on demand.

The honest comparison is that LogosLang is at the stage disp was several years ago,
with a better performance escape hatch and a much weaker foundation. Its trademark
policy and one-way-door release discipline suggest a builder thinking about
longevity, but the proof layer being entirely unbuilt means the hard parts are all
ahead of it.

## Verdict

**The nearest indie statement of disp's thesis — reflective structure the language
can rewrite, at systems speed — with the foundation still missing and the
performance escape hatch already working.** Worth watching, and worth stealing the
`.compile()` idea from immediately.

**Distance from disp's goals: same ambition on A1+A5, nothing yet on A2–A4, A6.**
