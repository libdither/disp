# Salt — "bneb" (anonymous)

**Repo:** https://github.com/bneb/salt (4★) · **Site:** salt-lang.dev · Show HN playground 2026-03-02
**Clone inspected:** yes — README, `salt-front/` (Rust), `salt-opt/` (C++ MLIR), `docs/`, `tests/z3_contracts/`

## ⚠️ Provenance caution

Read this first. The repo contains ~4 MB of real Rust/C++ across 1,210 files and a
blog active through June 2026 — but **the entire git history is one squashed commit
(2026-07-12)**, the author is anonymous, and the README claims an implausibly broad
ecosystem (microkernel, database server, Llama-2 inference, 70+ stdlib modules) for
a project with 4 stars. Every claim below is *as stated by the project*; none is
independently verified. Treat as a well-constructed design document that may or may
not be a working system.

## What it is (as claimed)

A systems language with **Z3-powered compile-time verification** and an
**MLIR → LLVM** backend claiming `clang -O3` parity.

```salt
pub fn safe_div(a: i32, b: i32) -> i32
    requires(b != 0)            // Z3 proves this at compile time
{ return a / b; }
```

The three design points that are actually interesting to disp:

**1. Zero-cost contracts with a reported proof ratio.** `requires`/`ensures` are
checked at compile time; **proven checks are elided from the binary**, unproven
ones become runtime assertions, and every build prints the ratio:

```
Z3: 8/8 checks proven (100%), 0 deferred to runtime
```

`--deny-deferred` turns any unproven check into a hard CI error.

**2. Type-bound proofs are discharged automatically.** `requires(x < 256)` on a
`u8` parameter is elided because the compiler knows `u8 ∈ [0, 255]`.

**3. Arena-based memory regions checked by the compiler** — no GC, no borrow
checker, claimed as a third point in the memory-management design space.

## Why disp should care: the proof-coverage ratio

Setting aside provenance, one idea here is genuinely worth stealing and nobody else
in this survey does it: **verification is reported as a continuous coverage number,
not a binary pass/fail, and the gap degrades gracefully to runtime checks.**

This matters to disp for two reasons:

- **It is a gradient for the optimizer.** disp's scoring function is
  `typecheck(0/1) × cost`. A 0/1 signal gives a search procedure almost nothing to
  climb. "8/8 proven, 0 deferred" is a *partial credit* signal — the fraction of
  obligations discharged is exactly the kind of smooth objective FOUNDATIONS §15
  says the checker should provide alongside the hard filter. disp has the cost axis
  as its smooth signal; proof coverage would be a second one.
- **Graceful degradation is a shipping strategy.** disp's checker either verifies
  or it doesn't. Salt's "unproven → runtime assertion" means a program is always
  runnable, and verification is a dial. For a language whose programs are
  machine-generated, being able to *ship the candidate while recording what remains
  unproven* is operationally valuable.

## Scorecard

| Axis | Salt (claimed) | Note |
|---|---|---|
| A1 Reflection | ✗ | None. |
| A2 Spec power | ◐ | Z3 contracts: bounds, postconditions, quantifiers, loop invariants, bitvectors. SMT-shaped, not dependent. |
| A3 Kernel | ✗ | TCB = saltc + Z3 + MLIR. |
| A4 Equality | ✗ | Not addressed. |
| A5 Perf | ◐ (unverified) | MLIR→LLVM, claims `-O3` parity, compiler-checked arena regions. Plausible design; unverified claim. |
| A6 Search | ✗ | Not agent-oriented; no synthesis. |

## What disp could steal

- **Proof-coverage percentage as a first-class build output** — the single most
  transferable idea, and cheap to add to disp's test harness.
- **Proven-checks-elided-from-the-binary** as the concrete statement of "verification
  should cost nothing at runtime," which is the same goal Idris 2's `0` quantity
  achieves by a different route (see `idris2-qtt.md`).
- **`--deny-deferred`** as the CI discipline: make the *absence* of proof a
  build-breaking condition on demand.

## Verdict

**One genuinely good idea (continuous proof coverage with graceful degradation)
wrapped in claims that cannot currently be trusted.** Take the idea; do not depend
on the artifact until the provenance improves.

**Distance from disp's goals: shallow on every axis except the reporting idea.**
