# AWESOME-LANGS

Languages and systems surveyed against **disp**'s goals, August 2026.
Scored on the six axes defined in [`_AXES.md`](_AXES.md); one file per project with
the full argument. All activity dates verified via the GitHub API on 2026-08-03
(Mojo: 2026-08-18, the day its compiler went open source; Narya: 2026-08-19; Telomare: 2026-08-26).

**disp in one sentence:** write a spec as a dependent type, turn the checker into a
0/1 score, multiply by a hardware-faithful cost score, search a reflective low-level
calculus for a program that is both provably correct and fast — then turn that
search on itself.

| Axis | Short name |
|---|---|
| **A1** | Reflection / programs-as-data |
| **A2** | Spec power (dependent types, type system as library code) |
| **A3** | Tiny trusted kernel, everything clever untrusted |
| **A4** | Equality — licensing "different program, same behavior" |
| **A5** | Native performance + cost model |
| **A6** | Search: spec → implementation, and self-application |

---

## The master table

✅ has it · ◐ partial · ✗ absent · **bold = ahead of disp on that axis**

| Project | A1 | A2 | A3 | A4 | A5 | A6 | Closest to disp on |
|---|:--:|:--:|:--:|:--:|:--:|:--:|---|
| [**HVM4 / Bend2 / SupGen**](hvm4-bend-supgen.md) | ◐ | ◐ | ✗ | ✗ | **✅** | **✅** | the *search substrate* — superposition search in the runtime |
| [**Metamath Zero**](metamath-zero.md) | ◐ | ◐ | **✅** | ✗ | **◐** | ✗ | the *kernel* + hardware model + self-verification |
| [**Verus**](verus.md) | ✗ | ◐ | ✗ | ◐ | **✅** | **✅** | *delivered outcomes* — verified fast systems code from specs |
| [**F\* / Low\* / Pulse**](fstar-lowstar-pulse.md) | ◐ | **✅** | ◐ | ◐ | **✅** | ✗ | the *shipping baseline*: dependent spec → verified fast C |
| [**Lean 4**](lean4.md) | ◐ | **✅** | ✅ | ◐ | ✗ | ◐→✅ | *spec power* + where the AI provers live |
| [**Nova**](nova.md) | ✗ | **✅** | ✅ | **✅** | ✗ | ✗ | the *equality* answer + certificate/kernel split |
| [**Blight**](blight.md) | ◐ | **✅** | ✅ | **✅** | ✗ | ✗ | the *kernel architecture*, independently reinvented |
| [**Soma**](soma.md) | ✗ | ✅ | ✗ | ✗ | **✅** | ✗ | same four ingredients, assembled for speed |
| [**fiat-crypto + CryptOpt + Jasmin**](fiat-crypto-cryptopt-jasmin.md) | ◐ | ✅ | ✅ | ◐ | **✅** | **✅** | the *whole endgame*, at 1/1000th scope |
| [**Velvet / Loom / WybeCoder**](velvet-loom-wybecoder.md) | ◐ | ✅ | ✅ | ◐ | ✗ | **✅** | the *agentic proof loop*, with numbers |
| [**Agda / Cubical**](agda-cubical.md) | ◐ | **✅** | ◐ | **✅** | ✗ | ✗ | cubical equality; reflection-by-quotation contrast |
| [**Narya**](narya.md) | ✗ | **✅** | ✗ | **◐** | ✗ | ✗ | the *interval-free* equality answer, shaped like disp's per-type metadata |
| [**Idris 2 / QTT**](idris2-qtt.md) | ◐ | ✅ | ✗ | ✗ | ◐ | ✗ | the *grading* formalism disp's cost ledger uses |
| [**CakeML + Pancake**](cakeml-pancake.md) | ✗ | ◐ | ✅ | ◐ | **✅** | ✗ | verified *all the way to the binary* |
| [**ATS3 / Xanadu**](ats-xanadu.md) | ✗ | ✅ | ✗ | ✗ | **✅** | ✗ | the 25-year prior attempt at A2+A5 |
| [**Vow**](vow.md) | ✗ | ◐ | ✗ | ✗ | ✅ | ◐ | the *agent-facing interface* |
| [**Dafny**](dafny.md) | ✗ | ◐ | ✗ | ◐ | ✗ | **✅** | calibration: what AI writes best |
| [**NanoLang**](nanolang.md) | ✗ | ✗ | ◐ | ✗ | ✅ | ◐ | mechanized metatheory, solo |
| [**Rust cluster**](rust-verification-cluster.md) | ✗ | ◐–✅ | ◐ | ◐ | **✅** | ◐ | the competition, and the fallback |
| [**Mojo**](mojo.md) | ◐ | ◐ | ✗ | ✗ | **✅** | ◐ᶠ | one language at every stage, at industrial scale |
| [**LogosLang**](logoslang.md) | ◐ | ◐ᵈ | ✗ | ✗ | ✅ | ✗ | nearest indie statement of disp's thesis |
| [**Acorn**](acorn.md) | ✗ | ◐ | ◐ | ✗ | ✗ | **✅** | *local* neural proposer in the loop |
| [**Salt**](salt.md) | ✗ | ◐ | ✗ | ✗ | ◐ᶜ | ✗ | proof-coverage as a continuous signal |
| [**Telomare** (Stand-In Language)](telomare.md) | ✗ | ◐ | ✗ | ✗ | ◐ᵗ | ✗ | the *static cost bound*: totality by inferred iteration counts, `--certificate`/`--meter` |
| [**Indie AI-first cluster**](indie-ai-first-cluster.md) | ✗ | ◐ | ✗ | ✗ | mostly ✗ | ◐ | LSTS shares the library-not-kernel thesis |
| [**Adjacent substrates**](adjacent-substrates.md) | — | — | — | **✅**ᵉ | — | — | egg/e-graphs ≈ disp's `~_T` machinery |
| [**Graveyard**](graveyard.md) | — | — | — | — | — | — | what happens when this is attempted |

ᵈ designed, not built · ᶜ claimed, unverified provenance · ᵉ egg specifically · ᶠ cost-only parameter sweeps, not program search · ᵗ static iteration bounds, no native speed

---

## How close is anyone to disp's actual goal?

**Nobody occupies disp's square.** Every project here has at most four of the six
axes, and the missing ones are always the same pair: **A1 + A6 together**. That
combination — a reflective substrate where the optimizer is a program in the
language it optimizes — is what makes self-application possible, and it is
structurally unavailable to the entire two-layer world (Rust+SMT, Lean+extraction,
C+separation logic), because there the optimizer is written in a *different*
language from the programs it transforms.

That is disp's real differentiator. Not dependent types (Lean, F*, Agda, Idris,
ATS have more), not performance (Verus, Rust cluster, CakeML have more), not the
kernel (MM0 has a better one). **It is that disp's optimizer can be aimed at
itself, and no one else's can.**

### Closest by axis

- **A1 (reflection):** disp leads. Native intensionality with no quotation layer is
  unique here; everyone else quotes (Lean `Expr`, Agda `Term`, Meta-F*) or, like
  Mojo, stages the same language without ever making terms data.
- **A2 (spec power):** disp trails badly. Lean, Agda, F*, Nova, Blight all have more.
- **A3 (kernel):** MM0 ahead; Nova and Blight match the architecture.
- **A4 (equality):** **disp is last.** Nova (extensional + certificates), Agda
  (cubical), Narya (observational, partially computing), and egg (e-graphs with
  replayable certificates) all have answers.
  This is disp's own Q1 and the survey confirms it as the weakest point.
- **A5 (performance):** Verus/Rust ≈ Mojo ≈ ATS ≈ Soma ≈ CakeML ahead; disp is an
  interpreted tree-walker with a measured 4,000–67,000× ic-net penalty. Telomare is the
  only entry with a *static* cost bound (inferred per-site iteration counts): the coeffect
  half of §9, which disp has not built.
- **A6 (search):** HVM4 (enumerative), Verus+IDS (neural, with cost in the loop),
  and CryptOpt (randomized, certified) are all ahead of disp, which has this
  designed but unbuilt.

### The three results that most change the picture

1. **Inductive Deductive Synthesis** (Berkeley+Google, May 2026): autonomously
   produced verified distributed KV stores for **7/7 consistency specs**, some
   **3× faster than published human-verified systems**, with performance
   benchmarks *inside* the verification loop. disp's stated endgame, achieved for
   a real workload — using Verus + an LLM, not a reflective calculus. → `verus.md`
2. **WybeCoder** (Meta, 2026): Lean/Velvet proof success went from **4.9%
   single-shot to 74.1% agentic** on the same benchmark. The gain came from search
   structure (subgoal decomposition, conflict-driven revision), not model scale.
   → `velvet-loom-wybecoder.md`
3. **CryptOpt is unmaintained since June 2024.** The one project whose architecture
   matched disp's endgame exactly — untrusted randomized search + verified
   equivalence checker + measured on-CPU cost — stopped. → `fiat-crypto-cryptopt-jasmin.md`

---

## What to steal, ranked by value per unit of effort

1. **Evaluate egg/e-graphs before building more bespoke rewrite machinery** for Q1.
   ROVER-style certificate emission (replayed by a kernel) is disp's §12
   architecture, already working. → `adjacent-substrates.md`
2. **Soma's flat/heap tiering.** Duplication of flat values is a free register copy;
   only heap types pay for SUP/ERA. Directly attacks disp's measured no-memo tax.
   → `soma.md`
3. **HVM4's label algebra, as shipped code.** disp's open Q2 is HVM4's feature set;
   `src/hvm.c`'s DUP/SUP rules and the collapser's priority queue are readable
   today. → `hvm4-bend-supgen.md`
4. **A benchmark.** DafnyBench took Dafny from 68%→96% in a year by making progress
   measurable. disp's optimizer claim is currently unfalsifiable. → `dafny.md`,
   `velvet-loom-wybecoder.md`
5. **Structured, machine-readable checker output** (Vow's JSON-for-agents) plus a
   **fallback ladder** (Velvet: SMT → tactics → property testing). `Ok false` gives
   a proposer nothing. → `vow.md`, `velvet-loom-wybecoder.md`
6. **Proof-coverage as a continuous build metric** with graceful degradation to
   runtime checks. A second smooth signal beside cost. → `salt.md`
7. **A CI-enforced `TRUST_ASSUMPTIONS.md` / axiom ledger.** → `indie-ai-first-cluster.md`
   (Verity), `metamath-zero.md` (thinking-sand)
8. **Source-directed `.compile()`** as the reflection escape hatch — full
   reflectivity by default, native speed on demand. → `logoslang.md`
9. **Scope the first real win like CryptOpt or Pancake did:** one narrow domain
   where search terminates and the equivalence checker can be small and verified.
   → `fiat-crypto-cryptopt-jasmin.md`, `cakeml-pancake.md`
10. **Mechanized metatheory for the two-op kernel** as the external anchor Q5 needs.
    NanoLang shows a solo dev can produce an `Admitted`-free development.
    → `nanolang.md`
11. **Inferred iteration bounds as a static cost certificate**, with the two failure kinds
    named (budget exhausted vs. input nothing bounds) and the analysis cached in a
    compile-once artifact. → `telomare.md`

---

## The uncomfortable finding

Two of them, stated plainly:

**disp's weakest axis is the one it identified as its spine.** FOUNDATIONS Part V
says A4 (the intensional/extensional gap) is make-or-break Q1. This survey confirms
disp is *last* on that axis among serious systems — and that two small projects
(Nova, Blight) already have working answers by choosing different theories, while
egg has a tooled, certificate-emitting answer for the first-order fragment.

**The thing disp wants to automate is the thing AI provers are worst at.** The
production verdict from the Rust→Lean pipeline (Runtime Verification + Ethereum
Foundation, May 2026): AI provers are a productivity multiplier for structural
lemmas and linear arithmetic, but **weak at domain-specific algebra and
loop-invariant discovery, and spec design stays human**. Proof-*closing* is getting
solved; invariant *discovery* and implementation *synthesis* are not. disp's A6
needs the second category.

Both point the same way: disp's differentiator (A1+A6 self-application) is real and
unoccupied, but it is gated behind A4, where disp is behind everyone and where the
available answers come from theories disp deliberately did not choose.
