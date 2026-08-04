# ATS3 / Xanadu — Hongwei Xi (Boston University)

**Repo:** https://github.com/githwxi/ATS-Xanadu (254★, 7,309 commits, 2,172 files, pushed 2026-08-03 — committing today)
**Contributors:** 9 listed, but effectively a one-person effort spanning 25 years.
**Site:** https://www.cs.bu.edu/~hwxi/atslangweb/ · First successful ATS3 bootstrap: 2025-03-29.

## What it is

ATS is the longest-running serious attempt at exactly disp's A2 + A5 combination:
**dependent types + linear types at C-level performance**. ATS2 compiles to C,
runs with no runtime overhead, and has been used for embedded and kernel-adjacent
work; its type system supports proving memory-safety and functional-correctness
properties of low-level pointer code.

**ATS3/Xanadu** is a ground-up redesign whose stated purpose is fixing the reason
ATS never spread: *"ML-like type-checking first, dependent type-checking second."*
ATS2's notorious learning curve came from making programmers satisfy the dependent
layer immediately; ATS3 stages it.

## Why disp should care

**1. It is the closest thing to disp's A2+A5 intersection that actually exists and
runs.** Not F* (which extracts to C through KaRaMeL), not Verus (which is Rust +
SMT, no dependent types) — ATS is *natively* a dependently-typed language whose
compilation model is C with no GC and no runtime. If disp's claim is "dependent
specs over C-speed code," ATS got there first, decades ago.

**2. Its failure mode is the most important cautionary tale in this directory.**
ATS is technically successful and socially unsuccessful. It has no ecosystem, tiny
adoption, and documentation that people bounce off. The lesson FOUNDATIONS §6
already states — dependent types win only in *bounded, high-assurance domains* —
is embodied here: ATS made everyday systems programming possible with dependent
types, and almost nobody came. The blocker was never capability; it was ergonomics
and the proof burden.

For disp this is a direct warning about A2. disp's answer (types are predicates,
checking is running, the type system is library code) is genuinely more ergonomic
in principle. But ATS3 is the reminder that "more ergonomic in principle" is what
every dependently-typed systems language has claimed, and the field's verdict is
40 years of niche use.

**3. Xanadu's staging insight is directly applicable.** "ML-like checking first,
dependent second" is a usable strategy for disp: most code should typecheck with
cheap structural checks, and the expensive predicate/hypothesis machinery should
engage only where the programmer actually states a strong property. disp's
`param_apply` dispatcher already draws a line like this (raw application never
reaches the kernel; only dispatched checks mint hypotheses) — Xanadu suggests
making that line a deliberate *user-facing staging* rather than an implementation
detail.

## Scorecard

| Axis | ATS3 | Note |
|---|---|---|
| A1 Reflection | ✗ | No programs-as-data. |
| A2 Spec power | ✅ | Dependent + linear types, natively, for low-level code. |
| A3 Kernel | ✗ | No LCF kernel; the typechecker is the TCB. |
| A4 Equality | ✗ | Not addressed as a rewrite-licensing problem. |
| A5 Perf | **✅** | Compiles to C, no GC, no runtime. Genuine systems performance with dependent types — rare. |
| A6 Search | ✗ | None. |

## What disp could steal

- **Staged checking (cheap first, dependent second)** as an explicit user-facing
  mode, and as the answer to disp's Q6 (does the whole edifice run fast enough to
  be a tool?). disp's generic-walker tax (~2×) and 8 GB test heaps are the same
  class of problem ATS3 was rebuilt to solve.
- **25 years of accumulated evidence about which dependent-type ergonomics
  actually deter users.** Xi has written extensively about why ATS2 was too hard;
  that is free user research for anyone building in this space.

## Where disp differs

ATS is a language for *humans* to write verified systems code in. disp is a
substrate for a *machine* to synthesize it. That difference reframes ATS's
ergonomic failure: if the primary author of disp programs is an optimizer, the
learning-curve problem that killed ATS's adoption matters far less — which is
arguably disp's strongest reply to the "dependent types never went mainstream"
objection.

Conversely, ATS has none of A1/A3/A4/A6 and no ambition toward them.

## Verdict

**The 25-year prior attempt at disp's A2+A5, still actively maintained by its
author, and the field's best evidence about why that combination alone is not
enough.** Worth reading precisely because it shows what disp would be if it
stopped after the foundation and never built the optimizer.

**Distance from disp's goals: shares A2+A5, absent on everything that makes disp
disp.**
