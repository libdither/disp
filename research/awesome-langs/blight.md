# Blight — sjqtentacles (solo)

**Repo:** https://github.com/sjqtentacles/blight (0★ at time of check, 1,957 files, created 2026-07-06, last commit 2026-07-09)
**Written in:** Rust (kernel + tower crates) + `.bl` standard library
**Clone inspected:** yes — README is unusually well-written for a two-month-old project.

## What it is

*"Scheme's soul, a proof assistant's spine, grown from one spore."*

A dependently-typed language that is also a proof assistant, in s-expression
syntax. The type theory is **cubical + quantitative (0/1/ω) grading + algebraic
effects** — i.e. it deliberately combines Cubical Agda's equality story, Idris 2's
QTT, and effect handlers in one kernel.

## Why it matters to disp: the architecture is disp's, stated in disp's words

Blight's README independently arrives at disp's §4 and §10:

**The kernel/tower split.** The kernel — the *spore* — is "the only code that can
mint a `Proof`, and it is deliberately microscopic." Everything else (data types,
pattern matching, traits, functors, effect handlers, tactics, stdlib, package
manager) is *tower* code: untrusted, explicitly re-checked. A tower bug "at worst
*fails to produce* a `Proof` — it is caught — and can never mint a false one."

That is `bind_hyp`/`hyp_reduce` and disp's "clever-and-untrusted around
tiny-and-trusted" (FOUNDATIONS §4), described in different vocabulary.

**The differential oracle, as a soundness argument.** Blight ships a *second,
independently-written re-checker* that can re-verify any accepted proof, so
soundness rests on "two small checkers agree, or the second honestly *declines* —
never silently disagree." disp's §10 runs five backends behind one Session ABI and
demands byte-identical agreement. Blight applies the same discipline to *proof
checking* rather than *reduction*, and — notably — designs for the "declines"
case, which is a distinction disp's harness doesn't currently make (agree vs.
disagree vs. abstain).

**Explicit trust framing.** "'Trusted' here means *implicitly* trusted: relied on
with no external check, so a bug in it could silently certify something false.
That is a liability, not a badge." This is the correct framing and better prose
than most academic kernels manage.

## Scorecard

| Axis | Blight | Note |
|---|---|---|
| A1 Reflection | ◐ | S-expression syntax and a tower/kernel split invite metaprogramming, but no native programs-as-data. |
| A2 Spec power | **✅** | Cubical + QTT + effects is a *more* ambitious type theory than disp currently implements. |
| A3 Kernel | ✅ | Same architecture as disp, plus an independent second checker. |
| A4 Equality | **✅** | Cubical: computational univalence and higher inductive types — the heavyweight-but-real answer disp cites in §7 but hasn't integrated. |
| A5 Perf | ✗ | No native backend, no cost model. Rust host, research-scale. |
| A6 Search | ✗ | Tactics are human-written. No synthesis. |

## What disp could steal

- **The "second checker may decline" protocol.** disp's differential oracle treats
  disagreement as a bug; a three-valued (agree / decline / disagree) contract is
  strictly more useful when backends have different completeness, which disp's
  do (rust-ic-net vs eager).
- **Cubical + QTT in one kernel.** disp wants cubical path types (listed under
  "designed but not built") *and* graded coeffects for cost (§9). Blight is
  attempting the combination now; whether the metatheory composes is exactly
  disp's open question, and watching someone else hit the wall is cheap.
- **The README itself** as a model for explaining a kernel/tower split to readers
  who don't already know LCF.

## Where disp differs

Blight has no systems story whatsoever — no native codegen, no cost, no optimizer.
It is a proof assistant with good architecture. disp shares the architecture and
adds A5/A6, which is where all of disp's risk lives.

Also: Blight is **two months old with one contributor and no stars**, and its
commit history shows heavy AI assistance (mutation-testing gates, watchdogs). The
design is genuinely good; the durability is unproven. Treat it as a well-argued
design document with code attached rather than as a system to depend on.

## Verdict

**Independent convergence on disp's kernel architecture, with a more ambitious
type theory and no performance ambitions.** Most valuable as evidence that the
kernel/tower + independent-re-checker design is the natural answer, and as a live
experiment in whether cubical + QTT + effects can share one kernel.

**Distance from disp's goals: same A3, stronger A2/A4, nothing on A5/A6.**
