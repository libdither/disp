# Mojo (Modular, now Qualcomm)

**Repo:** https://github.com/modular/modular (27,025★, ~875 MB monorepo, pushed 2026-08-18)
**Written in:** C++ (KGEN, the MLIR-based compiler) + Mojo (stdlib, GPU kernels) + Python (tooling)
**Clone inspected:** no; inspected via the GitHub API: `KGEN/lib/` layout, `std/reflection` sources, `roadmap.mdx`, release changelogs

## What it is

Chris Lattner's Python-syntax systems language on MLIR, targeting CPUs, GPUs, and
AI accelerators from one source language. Three weeks in August 2026 changed its
status: Qualcomm's acquisition of Modular closed (July 29, with a stated
commitment to continue Mojo and MAX), Mojo 1.0 shipped with source-stability
guarantees (August 11), and the full compiler and toolchain went open source
under Apache 2.0 with LLVM exceptions (August 18, with docs split out to
mojolang.org). The compiler is in-tree: `KGEN/lib/` holds `MojoParser`,
`Elaborator`, `Interpreter`, five MLIR dialects, and the LLVM lowering, all C++.
Compiler contributions are not yet accepted (their target: end of 2026).

The 1.0 feature set: a parameter system with `comptime if`/`for`, traits with
compositions and default methods, `where` clauses, parametric raises, Rust-style
ownership, explicitly-destroyed linear types, GPU programming abstractions, and a
compile-time `std/reflection` module (`reflect[T].field_names()`, function name
introspection). Hundreds of thousands of lines of production GPU kernels are
written in it (the MAX kernel library, in the same repo).

## Why it matters to disp

**1. The staging half of A1, shipped at industrial scale.** Compile-time and
runtime Mojo are the same language: the compiler embeds an interpreter
(`KGEN/lib/Interpreter/`) that evaluates ordinary Mojo during elaboration, types
are first-class compile-time values, and there is no quotation layer. This is the
nearest any mainstream-audience language comes to disp's position that the
metaprogram should be a program in the language. The boundary is just as
informative: programs never become data (there is no way to inspect a function
body), and the checker itself is C++, so the reflective loop disp needs for A6
cannot close here.

**2. Where "dependent types" land when you refuse propositions.** Types indexed
by compile-time values (`SIMD[dtype, width]`), `where` clauses, conditional
conformance, and `comptime assert` for instantiation-time predicate checks. The
roadmap checks off "predictable dependent types" and means exactly this much:
dependence on values the compile-time interpreter can fully evaluate, nothing
over runtime values, no proof objects anywhere. One real fragment of "types as
library code" exists (`Int`, `SIMD`, and friends are stdlib structs wrapping MLIR
primitives), though the checker is compiler-internal. This is useful calibration:
the ceiling of spec power an industrial audience will absorb.

**3. The autotune retreat.** Mojo launched with `autotune()` and `search()` as
language primitives: fork a function over a parameter space, benchmark, keep the
winner. v0.7.0 (January 2024) removed them for a redesign that never returned;
production autotuning today is offline benchmark-sweep scripts over kernel
parameter grids (`max/kernels/benchmarks/autotune/`), cost-only, correctness
assumed. A well-funded team tried cost-in-the-loop search inside the language and
moved it out to tooling. disp's A6 differs on exactly the dimension Mojo dropped:
a checker multiplying the cost score.

## Scorecard

| Axis | Mojo | Note |
|---|---|---|
| A1 Reflection | ◐ | Same language at compile time, no quotation layer; `std/reflection` reads type structure. Terms are never data; the checker is C++. |
| A2 Spec power | ◐ | Compile-time-value indexing, `where` clauses, conditional conformance, linear types. No propositions, no proofs, no runtime-value dependency. |
| A3 Kernel | ✗ | Trust is the whole MLIR/LLVM C++ stack. Now visible, never small. |
| A4 Equality | ✗ | MLIR rewrites are unverified compiler transforms; no semantic licensing, no certificates. |
| A5 Perf | **✅** | The strongest hardware story in this survey: MLIR codegen for CPU/GPU/accelerators, vendor-class kernels, zero-cost flat structs. |
| A6 Search | ◐ᶠ | Offline benchmark sweeps over kernel parameter grids, cost only, no checker in the loop; the in-language `autotune` was removed in v0.7.0. |

## What disp could steal

- **The comptime interpreter as readable engineering.** `KGEN/lib/Interpreter/`
  is an industrial implementation of the job disp's elaborator does when it
  reduces during checking: evaluate the object language at elaboration time,
  fail compilation when a value will not resolve to a constant. Newly readable
  as of this week.
- **The reflection API surface.** `reflect[T]` in the prelude with `.name()`,
  `.field_count()`, `.field_names()`, and `comptime assert` building good error
  messages from it (their `constrained.mojo` does exactly this). A model for the
  friendly face disp's programs-as-data could expose, minimal enough that stdlib
  authors actually use it.
- **The autotune retreat as A6 calibration.** Measured-cost search lasted under a
  year as a language primitive before moving to offline tooling. disp's design
  (external optimizer, checker in the loop) is consistent with that lesson: the
  search loop wants to live outside the core language.
- **Linear types with a mainstream audience.** Explicitly-destroyed types landed
  in 1.0, precedent that usage discipline of the kind disp grades with QTT-style
  ledgers is sellable outside proof-assistant circles.

## Where disp differs

Verification is structurally out of scope: no propositions, no proofs, no
kernel, no equality story, and the trusted base is the entire MLIR/LLVM C++
stack. Mojo is the performance substrate of the two-layer world with no proof
layer yet built on top; if one appears it will be another Rust-cluster-shaped
system, with the optimizer living outside the language it optimizes. Nothing
here competes with disp's A1+A6 claim. The competition is for the audience:
people who want one language for hosts and accelerators in the AI era.

## Verdict

**The industrial proof that one-language-at-every-stage sells, and the
competition for the AI-hardware audience.** Mojo shows what A1+A5 look like with
the other four axes dropped: it stages without reflection, indexes types without
propositions, and rewrites without licenses. Now that the compiler is readable,
the comptime interpreter is the part worth an afternoon.

**Distance from disp's goals: minimal on A5 and the staging half of A1; maximal
on A3, A4, and A6.**
