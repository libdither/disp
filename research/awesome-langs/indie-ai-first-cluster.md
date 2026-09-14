# The indie AI-first contract-language cluster (2026)

A micro-genre that did not exist 18 months ago: solo developers building
contract-carrying languages *for AI authors*. Catalogued at
[agentlanguages.dev](https://agentlanguages.dev/) (Alasdair Allan, ~35 entries,
July 2026), which is itself the best index of the space.

Vow, NanoLang, Salt, and LogosLang have their own files. This covers the rest.

| Project | Dev | ★ | Verification | Perf leg | Verdict |
|---|---|---|---|---|---|
| [Vera](https://github.com/aallan/vera) | Alasdair Allan | 400 | Mandatory Z3-verified contracts on every function | ✗ unverified | Credible dev, no systems leg |
| [Aver](https://github.com/jasisz/aver) | Szymon Teżewski | 58 | Intent blocks + **export to Lean 4 / Dafny** | ✗ | Real proofs, borrowed |
| [Sigil](https://github.com/inerte/sigil) | Julio Nobrega | ~15 | Z3 refinement `where` clauses, requires/ensures across call boundaries, capability effects | ✗ **emits TypeScript** | Good ideas, wrong target |
| [Locque](https://github.com/jaggederest/locque) | Justin George | 3 | Dependently typed, strict value/computation split | ✗ | **Stalled** (Mar 2026); "built almost entirely by LLMs" |
| [Prove](https://code.botwork.se) | Magnus Knutas | — | Refinement types + **"refutation challenges"** | unknown | Self-hosted forge, hard to assess |
| [LSTS](https://github.com/Lambda-Mountain-Compiler-Backend/LSTS) | Andrew Johnson | 125 | Language **and proof assistant**; proof concepts moved "entirely to libraries" | ✅ **compiles to C** | Closest to disp's G2+G3+G4 in this tier |
| [CLR](https://github.com/ityonemo/clr) | Isaac Yonemoto | 277 | Lifetimes + refinement types for **Zig**, via AIR static analysis | ✅ Zig | A tool, not a language; high-credibility dev |
| [Verity](https://github.com/Th0rgal/verity) | Thomas Marchand | 143 | Verified compiler in **Lean 4**, spec → proven-equivalent bytecode, **0 axioms, no `sorry`** | ✗ EVM only | Best executed; wrong target |

## Scorecard (cluster-level)

| Axis | Cluster | Note | Clauses |
|---|---|---|---|
| G1 Substrate | ✗ 0% (none) | Contracts + SMT on a convenient host; nobody in the genre makes programs data. | 0 · 0 · 0 — contracts + SMT on a host; no reflection anywhere in the genre |
| G2 Specification | ◐ 25% (Z3 contracts) | Z3-discharged contracts are the genre default; Aver and Verity borrow Lean for real proofs. | ½(contracts) · ½(solver-discharged) · 0 · 0 — Z3 contracts throughout, Aver and Verity borrowing Lean proofs; no equality story |
| G3 Trust | ✗ 0% (SMT TCB) | The solver and the host toolchain are trusted; Verity (Lean, zero axioms) is the lone exception. | 0 · 0 · 0 — contracts + SMT; Verity (Lean, zero axioms) is the exception |
| G4 Execution | ◐ 25% (host legs) | Only LSTS (to C) and CLR (on Zig) touch native code; Verity compiles verified to EVM only. | ½(per member) · ½(one member) · 0 · 0 — LSTS compiles to C and CLR rides on Zig; Verity is a verified compiler to EVM bytecode |
| G5 Search | ◐ 33% (agent loops) | Agent loops around contracts throughout; Prove's refutation challenges are the novel move. | ½(agent loops) · ½(checker only) · 0 — agent loops around contracts; Prove's refutation challenges |

## The three findings that matter to disp

**1. Almost nobody has the performance leg.** Of the eight above, only LSTS and CLR
touch native code, and CLR isn't a language. The pattern across the whole genre is
*contracts + SMT + a convenient host* (TypeScript, JVM, EVM). "AI-first
verification" turned out to be much easier to attempt than "AI-first verification
**at systems speed**," which is precisely the gap disp is aimed at. That is genuine
evidence that disp's target is unoccupied for a structural reason, not an oversight.

**2. LSTS is the one with disp's architecture.** *"By moving proof-theoretical
concepts entirely to libraries, programmers can hopefully benefit from improved
sanity without too much additional stress"* — that is disp's §2/§5 thesis (the type
system is library code, not kernel surgery) stated independently. LSTS compiles to
**C with little or no overhead**, previously emitted x86 objects directly, and
notes it will *"revisit the direct targets to generate fully certified builds."*
Its self-hosting claim (the LM compiler is written mostly in LSTS) is also disp's
metacircular ambition in miniature. Worth a closer look than its 125 stars suggest.

**3. Verity is the best-executed, and its thesis is disp-adjacent.** A formally
verified compiler in Lean 4 with machine-checked semantics preservation across
three layers, CI-enforced no-`sorry` and zero axioms, plus an explicit
`TRUST_ASSUMPTIONS.md` boundary — by a 23-year-old founder. Its stated bet is *"we
bet that agents will make full formal verification practical."* Same wager as disp,
scoped to EVM bytecode where the cost model is simple (gas) and the programs are
tiny. **That scoping is the lesson**: it is why Verity can actually finish proofs.

## What disp could steal

- **`TRUST_ASSUMPTIONS.md` + `AXIOMS.md` as maintained, CI-enforced artifacts**
  (Verity). disp's trust boundary lives in prose across FOUNDATIONS and
  TYPE_THEORY; making it a checked file is cheap and forces honesty. Pairs with
  MM0's axiom-ledger idea.
- **Verification-status dashboards regenerated from the codebase** (Verity's
  `docs/VERIFICATION_STATUS.md`) — disp's ~1,200 tests could produce the analogous
  "what is actually proven" report.
- **"Refutation challenges"** (Prove): the compiler generates mutations of your
  code and makes you annotate why each is wrong. This is mutation testing aimed at
  *specification completeness* — a direct defense against the vacuous-spec failure
  mode disp's 0/1 scoring invites, and cheaper than AlphaVerus's critique phase.
- **Aver's "export to Lean/Dafny" pattern** as a cheap way to borrow a proof
  ecosystem rather than build one — relevant if disp ever wants external validation
  of its kernel's verdicts.

## Verdict

**A real genre with one structural hole in it, and disp is aimed at the hole.**
None of these is a design competitor to disp; collectively they establish that
"contracts for AI authors" is now a crowded idea while "reflective, dependently
typed, systems-fast, self-optimizing" remains empty.

**Distance from disp's goals: all shallow on G1/G5 and on equality; LSTS is the only one sharing
disp's library-not-kernel philosophy with a native backend.**
