# NanoLang — Jordan Hubbard

**Repo:** https://github.com/jordanhubbard/nanolang (613★, 1,985 files, created ~2025-09-30, pushed 2026-08-02)
**Author:** FreeBSD co-founder, ex-Apple Director of UNIX Technology, now NVIDIA.
**Clone inspected:** yes — README, `docs/SPECIFICATION.md`, `docs/NANOISA.md`, `formal/`

## What it is

> *"I am a minimal programming language designed for machines to write and humans
> to read. I require tests, I use unambiguous syntax, and my core is formally
> proved."*

(The docs are written in first person, from the language's point of view — itself
an agent-facing design choice.)

Three legs, all real:

1. **Transpiles to C** for native performance; also ships **NanoISA**, its own VM
   that isolates dangerous external calls in a separate process.
2. **Core semantics mechanically proved in Coq** — type soundness, progress,
   determinism, and big-step ↔ small-step equivalence, all complete and
   **`Admitted`-free** (193 theorems, zero axioms).
3. **Designed as an LLM target**: mandatory "shadow" test blocks, unambiguous
   syntax, one canonical form. Self-hosting bootstrap in progress.

## Why it matters to disp: the verification is one level off

This is the sharpest distinction in the whole directory, and it is worth being
precise about because the project is easy to over-credit.

NanoLang's Coq development verifies **the language**, not **your program**. The
theorems say the semantics are deterministic and the type system is sound. They do
*not* let a programmer write `∀ n, sort n is sorted` and get a machine-checked
proof about their own code. There is no spec language for user-level properties,
no dependent types, no proof obligations in user code.

disp needs both levels: a proved-sound kernel (A3) **and** user-written
specifications strong enough to serve as the optimizer's scoring oracle (A2).
NanoLang has an exemplary version of the first and none of the second.

That said, the first level is done to a standard almost nobody meets. `Admitted`-free
mechanized metatheory for a language with a real C backend, by one person, in
under a year, is genuinely rare — most languages in this survey have *no*
mechanized metatheory at all, disp included (disp's self-verification is in-language
and Q5 explicitly flags open gaps).

## Scorecard

| Axis | NanoLang | Note |
|---|---|---|
| A1 Reflection | ✗ | No programs-as-data. |
| A2 Spec power | ✗ | No user-level spec language. The proofs are *about* the language, not written *in* it. |
| A3 Kernel | ◐ | No LCF kernel, but mechanized `Admitted`-free metatheory in Coq — a different and complementary kind of trust. |
| A4 Equality | ✗ | Not addressed. |
| A5 Perf | ✅ | Transpiles to C; NanoISA sandbox for FFI. |
| A6 Search | ◐ | Designed as an LLM target (canonical syntax, mandatory tests) but no synthesis engine or optimizer of its own. |

## What disp could steal

- **Mechanized metatheory as a deliverable.** disp's Q5 (self-verification without
  a growing external anchor) is currently argued in prose and partially in-language.
  A Coq/Lean development proving disp's *kernel* rules sound — even just the two
  Σ-ops and the walker's escape check — would be the external anchor FOUNDATIONS
  says the metacircular story needs, and NanoLang shows a solo dev can produce one.
- **Mandatory tests as a synthesis gate.** NanoLang refuses to compile without test
  blocks. For a language whose programs are machine-written, making the
  specification artifact *non-optional* is a cheap structural defense against the
  spec-gaming failure mode disp's 0/1 scoring invites.
- **The sandboxed-FFI VM (NanoISA)** as a model for disp's GOALS bullet 2 — the
  "outsource execution to an external faster language and return the cost"
  primitive needs exactly this kind of isolation boundary to be measurable and safe.

## Where disp differs

disp's entire architecture is about making user-level specification cheap and
machine-checkable; NanoLang's is about making the *language* trustworthy and easy
for models to emit. They are complementary halves that don't overlap: NanoLang
would need a whole type theory to reach disp's A2, and disp would need a Coq
development to match NanoLang's A3-adjacent guarantee.

## Verdict

**The most credible developer in the indie AI-native cluster, executing a real but
narrower thesis.** Useful to disp as (a) proof that mechanized metatheory is
achievable solo, (b) a model for agent-facing language constraints, and (c) a
caution about the phrase "formally verified language" — always ask *which level*.

**Distance from disp's goals: shares A5 and the AI-target motivation; absent on
A1, A2, A4, A6.**
