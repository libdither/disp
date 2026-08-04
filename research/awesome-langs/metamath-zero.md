# Metamath Zero (MM0 / MM1 / MMC) — Mario Carneiro

**Repo:** https://github.com/digama0/mm0 (408★, 393 files, pushed 2026-08-02)
**Recent:** x86 spec test suite (2026-08-01), "x86: correct the machine model against the Intel x86-64 SDM" (2026-07-28)
**Sibling:** https://github.com/digama0/thinking-sand (created 2026-08-01) — "how to verify a computer down to physics"
**Clone inspected:** yes — `README.md`, `mm0.md`, `examples/` (peano, x86, compiler, verifier)

## What it is

A specification-and-proof language built to be **verified all the way down to the
hardware**. Stated project goal: *"build a formally verified (in MM0) verifier for
MM0, down to the hardware, to build a strong trust base."*

The architecture is a clean two-layer split that disp will recognize immediately:

- **MM0** — specification-only, deliberately tiny. Proofs are fully explicit and
  verbose; nothing is inferred. The `mm0-c` verifier is a bare-bones C program.
- **MM1** — the *untrusted* authoring language on top: elaboration, unification, a
  Scheme-like metaprogramming environment for tactics. Compiling an MM1 file emits
  an MM0 spec + a binary proof file (MMB). **Because the process is proof-producing,
  it need not be trusted.**

This is the LCF discipline with the trust boundary drawn even more aggressively
than usual, plus a deliberate rejection of Lean's monolith ("there is only one
program that can read `.lean` files… huge and full of bugs").

## The parts that matter to disp

**Self-verification without hand-waving.** `examples/mm0.mm0` + `mm0.mm1` is a
complete formal specification of the MM0 format and its verification — input
strings, parser, proof checking — *written in MM0*. `verifier.mm0` states the
implementation-correctness theorem of an MM0 verifier as the project's main goal.
This is disp's §3 metacircular discipline and Q5 (self-verification without the
Löbian wall mattering), carried further than any dependently-typed system has.
Carneiro is explicit about the anchor: you get relative consistency, and MM0's
answer to keeping the anchor small is to make the checked artifact tiny and the
authoring layer untrusted.

**A hardware model in the same logic.** `x86.mm0`/`x86.mm1` formalize x86-64 (and
are being corrected against the Intel SDM *this week*), and **MMC** is a compiler
targeting that model. This is disp's GOALS bullet — "deterministic models of the
base hardware encoded into the type system" — actually existing. `thinking-sand`
extends the ambition below the ISA: eight layers from field equations to
instruction set, with an explicit *axiom ledger* recording every claim that cannot
be a theorem.

**The differential-oracle pattern, socially.** Because the MM0 spec is small,
third parties wrote independent verifiers (`second_opinion` in Rust,
`trivial-rs`, `mm0kt` in Kotlin). disp's §10 evaluator plurality is achieved here
by *specification minimalism* rather than by an in-repo harness.

## Scorecard

| Axis | MM0 | Note |
|---|---|---|
| A1 Reflection | ◐ | MM0 formalizes MM0, but via explicit deep embedding, not native programs-as-data. No tree-calculus-style intensionality. |
| A2 Spec power | ◐ | Interpretable as a subset of HOL — deliberately *weaker* than dependent types. Expressiveness was traded away for verifier simplicity. |
| A3 Kernel | **✅** | The strongest trusted-core story in existence: a C verifier small enough to formalize, with the whole authoring layer untrusted and proof-producing. |
| A4 Equality | ✗ | Not addressed — MM0 is a proof format, not an optimizing calculus. No rewrite-licensing notion. |
| A5 Perf | **◐** | Checking is Metamath-fast (seconds for huge libraries). MMC compiles to verified x86. But MM0 is not a general-purpose systems language you'd write applications in. |
| A6 Search | ✗ | No synthesis. MM1 tactics are human-written metaprogramming. |

## What disp could steal

- **The "spec small enough that others reimplement it" strategy.** disp gets
  evaluator plurality by running five backends in-repo; MM0 gets it for free
  because the spec fits in a person's head. If disp's kernel surface (2 Σ-ops +
  dispatcher) is genuinely that small, publishing it as a checkable spec would buy
  the same external oracle.
- **The axiom ledger.** `thinking-sand`'s discipline — enumerate every claim that
  *cannot* be a theorem, classify it, price it — is exactly the honesty disp's
  FOUNDATIONS Part V is reaching for, but as a maintained artifact rather than
  prose.
- **Verbose-explicit proof format + untrusted elaborator.** disp's certificate
  story for the optimizer (§12) wants exactly this shape: clever untrusted
  producer, dumb fast checker.

## Where disp differs

MM0 bought its extraordinary kernel story by **giving up expressiveness**
(HOL-subset, not dependent types) and **giving up computation** (it is a proof
format; programs are not the objects being optimized). disp wants dependent types
*and* to run the programs *and* to optimize them. MM0 is the proof that the
minimal-kernel end of the design space works when you take nothing else with you.

The other difference is the target: MM0's endgame is *trust* (a verified verifier
on verified hardware). disp's endgame is *automation* (synthesize the program).
MM0 has no A6 at all and doesn't want one.

## Verdict

**The reference implementation of disp's A3, and the only project that has taken
A5's "hardware model in the logic" seriously.** Carneiro is also the single most
credible individual in this entire survey. Read `mm0.md` and the `verifier.mm0`
goal theorem before making any further decisions about disp's kernel/certificate
boundary.

**Distance from disp's goals: far on A2/A4/A6, ahead of disp on A3 and on
hardware modeling.**
