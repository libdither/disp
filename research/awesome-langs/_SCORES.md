# Scores

Every score, in one place (2026-10-31). Each axis's maximum is what `GOALS.md` asks for, split into
three or four clauses in [`_AXES.md`](_AXES.md#grading-the-maximum-and-the-clauses); every clause
scores 0, ½ or 1 and the axis percentage is their weighted mean. The write-ups' scorecard notes are the
evidence. `†` marks a value taken from general knowledge beyond the write-up; `?` marks a clause the
write-up does not settle (scored 0, percentage provisional); `—` marks a clause that does not apply
(left out of the mean). Rows follow the master table, disp first. disp has two rows. **disp (theoretical)** is not what disp has: each clause scores what an existing argument says the design could reach, pinned by a test or written in a design note, and a clause with no argument scores what the argument stops at. The arguments are in [`_AXES.md`](_AXES.md#the-arguments). **disp (today)** is what the repo has, checked as of commit `a5e051f49b1b` (2026-11-02); bump the commit when you re-check it. Every other row is graded on what ships, and "ahead of disp" means ahead of the theoretical row. This file is the source:
`scripts/awesome-scores.py --write` recomputes the percentages and copies every cell into the
write-ups, the master table and disp's own table.

## G1 Substrate

(a) programs can take programs apart, quotation counts (½ a restricted part only: types, environments, runtime graphs, rewrite patterns) · (b) the checker is written in the language (½ type theories as library files over an outside checker, the verifier only stated in the language, or a self-hosted compiler whose verdicts come from a solver) · (c) the checker is callable from programs as an ordinary function (½ from metaprograms only)

Weights: 1 · 1 · 1

| Project | a | b | c | % | Why |
|---|:-:|:-:|:-:|--:|---|
| disp (theoretical) | 1 | 1 | 1 | 100 | pinned by tests: a program is a tree that `shape_of` takes apart; `honest` and `certify` are disp programs you apply |
| disp (today) | 1 | 1 | 1 | 100 | same as in theory: `shape_of` takes a program apart, and `honest` and `certify` are disp programs you apply |
| HVM4 / Interaction Calculus | ½(runtime graphs) | 0 | 0 | 17 | terms are runtime graph nodes with no in-language inspection; untyped, no checker |
| Bend 2 / BendTT / BendRT | 0 | 0 | 0 | 0 | no reflection; templates re-parse closed source text at compile time; the checker is TypeScript |
| Metamath Zero | 1 | ½(verifier stated in MM0) | ? | 50? | MM1 metaprograms manipulate terms; `verifier.mm0` states the verifier in MM0 while the running one is C; whether metaprograms can call it is not in the write-up |
| Verus | 0 | 0 | 0 | 0 | ghost code is erased; the checker is not a Verus program |
| F* / Low* / Pulse | 1 | ½(solver verdict)† | ½(metaprograms)† | 67 | Meta-F* reflects syntax; F* is self-hosted but Z3 gives the verdicts; tactics can call the typechecker |
| Lean 4 | 1 | 1 | 1 | 100 | `Expr` is programs-as-data; lean4lean is a checker for Lean written in Lean, an ordinary function |
| Nova | 0 | 0 | 0 | 0 | standard elaborator and kernel pipeline, written in Idris 2 |
| Blight | ? | 0 | 0 | 0? | s-expressions "invite metaprogramming" but the write-up does not say what a program can inspect; the kernel is Rust |
| Soma | 0 | 0 | 0 | 0 | conventional compiler pipeline in Lean and Rust |
| fiat-crypto + CryptOpt + Jasmin | 1 | 1† | 1† | 100 | Gallina deep embeddings; MetaCoq's safe checker is Rocq's checker written in Rocq, an ordinary function |
| Velvet / Loom / WybeCoder | 1 | 1† | 1† | 100 | inherits Lean's `Expr` and lean4lean |
| Agda / Cubical | 1 | 0 | ½(metaprograms)† | 50 | `quoteTerm`/`unquote`; the checker is Haskell, reachable through the TC monad |
| Narya | 0 | 0 | 0 | 0 | no reflection API; the checker is OCaml |
| Dedukti / Lambdapi | ½(rewrite patterns) | ½(theories as files) | 0 | 33 | rules match unevaluated calls; each type theory is a library file but the checker is OCaml |
| Andromeda 2 | ½(object judgements) | ½(theories as files) | ½(metaprograms) | 50 | AML matches object judgements and calls the nucleus; theories are user files over an OCaml nucleus |
| Idris 2 / QTT | 1 | 1† | ½(metaprograms) | 83 | elaborator reflection; Idris 2 is written in Idris 2 |
| CakeML + Pancake | 0 | 0 | 0 | 0 | conventional verified compiler; the proofs live in HOL4 |
| ATS3 / Xanadu | 0 | 1† | 0 | 33 | no programs-as-data; the compiler is written in ATS |
| Vow | 0 | ½(solver verdict) | 0 | 17 | macros excluded on purpose; the compiler is self-hosted but ESBMC gives the verdicts |
| Dafny | 0 | 0 | 0 | 0 | none |
| NanoLang | 0 | 0 | 0 | 0 | none |
| Rust cluster | 0 | 0 | 0 | 0 | the checkers are separate tools over Rust |
| Mojo | ½(types only) | 0 | 0 | 17 | `reflect[T]` reads type structure only; the checker is C++ |
| LogosLang | ½(walk only) | 0 | 0 | 17 | the Logic Graph is walkable today; self-rewriting and the proof layer are unbuilt |
| Acorn | 0 | 0 | 0 | 0 | none |
| Salt | 0 | 0 | 0 | 0 | none |
| Telomare (Stand-In Language) | ½(env only) | 0 | 0 | 17 | a closure's environment can be projected, its code cannot; every analysis is Haskell |
| Stellogen | 1 | 1 | 1 | 100 | code is a term that shape checkers unify against; types are library constellations you can apply |
| Nock / Hoon | 1 | 1 | 1 | 100 | formulas are nouns; Hoon's checker is written in Hoon, and `mint` and `nest` are ordinary gates |
| Thermite | 0 | 0 | 0 | 0 | Forge is an external tool and programs are never data |
| Indie AI-first cluster | 0 | ½(one member) | 0 | 17 | contracts + SMT on a host; LSTS's compiler is mostly written in LSTS |

## G2 Specification

(a) specifications mention values (½ contracts or refinements over runtime values, 1 dependent types) · (b) propositions and proofs are first-class, with a library in use (½ solver-discharged obligations, or proofs without a library) · (c) an equality you can state and check, for specs like "behaves like f": extensional, cubical, observational (½ propositional or SMT-fragment equality, tactic-driven or a decidable fragment)

Weights: 45 · 35 · 20

| Project | a | b | c | % | Why |
|---|:-:|:-:|:-:|--:|---|
| disp (theoretical) | 1 | 1 | 1 | 100 | a member of a function type is a lens (`implies`, pinned); proofs are lenses composed from the five rule lenses, so the library is work, not theory; equality at a type is agreement at every challenge, certified by induction (pinned for `id`/`shift` and the two doublings) |
| disp (today) | 1 | ½(no library) | ½(a few instances) | 72 | claims over bounded runs and lenses work, but the rule lenses and `certify` live in `lib/tests/claim.test.disp` with little library around them; equality is certified for `id` and `shift` only |
| HVM4 / Interaction Calculus | 0 | 0 | 0 | 0 | untyped; the typed language became Bend 2, which does not run on this runtime |
| Bend 2 / BendTT / BendRT | 1 | ½(small library) | ½(propositional) | 72 | affine dependent types with Type : Type, laws proven by defs over a small Base; quantities &0/&1/&2 are a usage grade; intensional J with a written motive, no extensionality |
| Metamath Zero | 0 | 1 | 0 | 35 | a HOL subset with explicit proof objects and a modest library; no dependent types, no resource types, equality not addressed |
| Verus | ½(contracts) | ½(solver-discharged) | ½(SMT-fragment) | 50 | first-order contracts with ghost state, Z3-discharged, over a large verified codebase; SMT-fragment equalities |
| F* / Low* / Pulse | 1 | 1 | ½(SMT-fragment) | 90 | dependent types, refinements, effects, Pulse, with HACL* as the library; SMT-fragment equality |
| Lean 4 | 1 | 1 | ½(propositional) | 90 | full dependent types, universes, mathlib; propositional equality with Quot and axioms, rewriting tactic-driven |
| Nova | 1 | ½(no library) | 1 | 82 | extensional MLTT: anything provably equal is treated as equal and certificates recover decidability; solo, no library |
| Blight | 1 | ½(no library) | 1 | 82 | cubical + QTT + effects: computational univalence and quantities in the kernel; two months old, no library |
| Soma | 1 | 0 | 0 | 45 | dependent types + QTT + rows, not a proof assistant; quantities are a usage grade; equality not addressed |
| fiat-crypto + CryptOpt + Jasmin | 1 | 1 | ½(asm fragment) | 90 | full CIC with fiat-crypto and bedrock2 as the library; a verified equivalence checker for straight-line assembly is the equality story, a tiny fragment |
| Velvet / Loom / WybeCoder | 1 | 1 | ½(SMT-fragment) | 90 | Lean + mathlib behind a Dafny-style surface; Lean's equality plus SMT in its fragment |
| Agda / Cubical | 1 | 1 | 1 | 100 | dependent types, HITs, univalence with the standard and cubical libraries; cubical equality computes |
| Narya | 1 | ½(no library) | ½(some formers) | 72 | HoTT + parametricity + modalities, no library yet; observational Id per former, transport computing on some formers so far |
| Dedukti / Lambdapi | 1 | 1 | ½(user rewrite rules) | 90 | dependent types with every theory a library file and large translated proof libraries; equality is conversion modulo user rules, decidable only when they are confluent and terminating, which is assumed |
| Andromeda 2 | 1 | ½(no library) | 1 | 82 | user-defined dependent theories with example theories only; equality reflection is a one-rule theory and a sound extensible checker handles user equality rules |
| Idris 2 / QTT | 1 | 1 | ½(propositional) | 90 | dependent types + QTT with a working standard library; quantities give erasure; intensional propositional equality |
| CakeML + Pancake | ½(contracts) | ½(compiler-level) | ½(pass preservation) | 50 | Viper contracts for user code, HOL4 proofs about the compiler; per-pass semantics preservation is the equality, proved by humans |
| ATS3 / Xanadu | 1 | 1† | 0 | 80 | dependent + linear types with an explicit proof language and a 25-year library; equality not addressed |
| Vow | ½(contracts) | ½(BMC-discharged) | 0 | 40 | contracts + loop invariants under bounded model checking; young; equality not addressed |
| Dafny | ½(contracts) | ½(solver-discharged) | ½(SMT-fragment) | 50 | first-order + quantifiers over a decade of AWS specs; SMT-fragment equality |
| NanoLang | 0 | 0 | 0 | 0 | no user-level spec language; the proofs are about the language |
| Rust cluster | 1 | 1 | ½(per-tool translation) | 90 | from Flux refinements to RefinedRust's Iris and Aeneas's Lean, with libcrux and ACE-RISCV as the library; Aeneas's borrow translation is a machine-produced equivalence, per tool |
| Mojo | ½(comptime values) | 0 | 0 | 22 | types indexed by compile-time values only; no propositions, no proofs, no equality |
| LogosLang | 0 | 0 | 0 | 0 | the proof layer and the rewriting engine are design documents |
| Acorn | 0 | 1 | 0 | 35 | a theorem-proving language for mathematics with a growing library; no program specifications |
| Salt | ½(contracts) | ½(solver-discharged) | 0 | 40 | Z3 contracts (claimed), no library, equality not addressed |
| Telomare (Stand-In Language) | ½(runtime refinements) | 0 | 0 | 22 | STLC-shaped statics with refinements as runtime validators; a static, inferred iteration bound per site; structural equality only |
| Stellogen | ½(value tests) | 0 | 0 | 22 | types are user-space test galaxies over values; no dependent types, no proofs; `==` is syntactic |
| Nock / Hoon | 0 | 0 | 0 | 0 | structural types with variance; nothing states a value property; opcode 5 is structural equality |
| Thermite | ½(contracts) | ½(solver-discharged) | ½(SMT-fragment) | 50 | contracts and invariants, solver-discharged along a recorded ladder; termination measures but no cost or resource types; SMT-fragment equality |
| Indie AI-first cluster | ½(contracts) | ½(solver-discharged) | 0 | 40 | Z3 contracts throughout, Aver and Verity borrowing Lean proofs; no equality story |

## G3 Trust

(a) a trusted core small enough to audit · (b) it mints unforgeable evidence: theorems, proof objects, certificates (½ proof objects checked only by a large checker) · (c) the clever layers are untrusted and re-checked by the core, and an independent checker exists (½ re-checked by the one core only)

Weights: 40 · 40 · 20

| Project | a | b | c | % | Why |
|---|:-:|:-:|:-:|--:|---|
| disp (theoretical) | 1 | 1 | ½(one core) | 90 | trust stops at five hand-proved rule lenses over a small machine; a certificate is a tree `certify` re-runs; `honest` re-checks independently but only refutes, and no second certificate checker is designed |
| disp (today) | ½(two checkers mid-move) | ½(certificates in tests only) | ½(one core) | 50 | `lib/kernel/` is still the checker in use beside the claim kernel; a verdict's `"Proved"` (`lib/verdict.disp`) is a bare string, and certificates exist only inside the test file; `honest` re-checks by enumeration but only refutes |
| HVM4 / Interaction Calculus | 0 | 0 | 0 | 0 | trust is `hvm.c` |
| Bend 2 / BendTT / BendRT | ½(one-file kernel) | ½(proof defs) | 0 | 40 | bend.ts is "the trusted kernel", one human-written file, checking in one pass; proofs are defs the kernel checks, no certificate minted; nothing re-checks, and the Lean model admittedly lags the checker |
| Metamath Zero | 1 | 1 | 1 | 100 | a C verifier small enough to formalize; MMB proof objects; MM1 untrusted; independent verifiers in Rust and Kotlin; MMC's verified compilation is the one licensed rewrite |
| Verus | 0 | 0 | 0 | 0 | TCB = Verus + Z3 + rustc |
| F* / Low* / Pulse | 0 | ½(large checker) | 0 | 20 | typechecker, Z3 and KaRaMeL are all trusted; proof terms exist, SMT verdicts have none |
| Lean 4 | ½(mid-size kernel) | 1 | 1 | 80 | a small kernel, larger than MM0's; proof terms; elaborator and tactics untrusted; lean4lean and lean4export re-check; `@[csimp]` swaps a definition for one the kernel proved equal (`implemented_by` is the unchecked escape hatch) |
| Nova | 1 | 1 | ? | 80? | a small kernel with its own spec re-checks certificate-carrying artifacts; no independent checker is mentioned |
| Blight | 1 | 1 | 1 | 100 | a microscopic kernel is the only thing that can mint a `Proof`; the tower is untrusted; a second independently written re-checker |
| Soma | 0 | 0 | 0 | 0 | trust the compiler |
| fiat-crypto + CryptOpt + Jasmin | ½(mid-size kernel) | 1 | 1† | 80 | Rocq's kernel; CryptOpt's equivalence checker is verified in Rocq; coqchk re-checks; CryptOpt's output is installed only on that verified check |
| Velvet / Loom / WybeCoder | ½(mid-size kernel) | 1 | 1 | 80 | Lean's kernel; Loom verifiers are foundational, no trusted VC generator |
| Agda / Cubical | 0 | ½(large checker) | 0 | 20 | the large typechecker is the TCB; proof terms exist, nothing smaller re-checks them; `COMPILE` pragmas swap a definition for unchecked Haskell |
| Narya | 0 | ½(large checker) | 0 | 20 | tens of thousands of OCaml lines are the trust base; NbE unproven |
| Dedukti / Lambdapi | ½(mid-size kernel) | 1 | 1 | 80 | a 4,389-line kernel; proof terms; Lambdapi and Kontroli re-check the same calculus, and re-checking other provers is the purpose; a rewrite rule is accepted on type preservation, confluence and termination assumed |
| Andromeda 2 | ½(mid-size nucleus) | 1 | ½(one core) | 70 | a 4,189-line nucleus is the only maker of judgements; the equality checker and AML are untrusted; no second checker |
| Idris 2 / QTT | 0 | ½(large checker) | 0 | 20 | the typechecker is the TCB; `%transform` swaps a definition at codegen, unchecked |
| CakeML + Pancake | 1 | 1 | 1† | 100 | HOL4's LCF kernel; theorems are the evidence; independent HOL checkers exist (OpenTheory, Candle); the compiler's proved passes are the only licensed rewrites, fixed rather than user-defined |
| ATS3 / Xanadu | 0 | ½(large checker)† | 0 | 20 | the typechecker is the TCB; proof terms exist |
| Vow | 0 | 0 | 0 | 0 | TCB = vowc + ESBMC |
| Dafny | 0 | 0 | 0 | 0 | TCB = Dafny + Boogie + Z3 |
| NanoLang | ½(soundness proof) | 0 | 0 | 20 | no kernel, but the core semantics are proved sound in Coq, `Admitted`-free |
| Rust cluster | ½(per tool) | ½(per tool) | ½(per tool) | 50 | Aeneas/hax/RefinedRust inherit Lean/F*/Rocq kernels; Kani/Creusot/Flux trust SMT |
| Mojo | 0 | 0 | 0 | 0 | the whole MLIR/LLVM C++ stack; MLIR rewrites are asserted compiler transforms, unverified |
| LogosLang | 0 | 0 | 0 | 0 | no kernel discipline described; `.compile()` lowers a function without licensing a replacement |
| Acorn | 0 | ? | 0 | 0? | has its own checker; the write-up does not characterize its TCB or proof objects |
| Salt | 0 | 0 | 0 | 0 | TCB = saltc + Z3 + MLIR (claimed) |
| Telomare (Stand-In Language) | 0 | 0 | 0 | 0 | the whole compiler is trusted; `--certificate` is a report nothing re-checks |
| Stellogen | 1 | 0 | 0 | 40 | ~3,200 lines with a normative spec and two trusted observations; checkers are user space; trust rests on paper theorems, no evidence objects to re-check |
| Nock / Hoon | 1 | 0 | 0 | 40 | a page of spec, a conformance suite, independent interpreters, but no evidence for any of them to re-check; jets are asserted equivalences dispatched on intensional identity, an unbounded trusted surface with no evidence and no production differential check |
| Thermite | ½(L4 via Lean) | ½(fragment evidence) | ½(audit replay) | 50 | L3 trusts Verus+Z3+rustc while L4 replays certificates through axiom-probed Lean; the manifest is a report, the Lean proofs are evidence for a fragment; audit re-derives the chain with independent replay but SMT verdicts carry nothing |
| Indie AI-first cluster | 0 | 0 | 0 | 0 | contracts + SMT; Verity (Lean, zero axioms) is the exception |

## G4 Execution

(a) the programs it produces run at C/Rust class by any route, handing work to an external language included (½ compiled but not systems class) · (b) the checker can reason about the target through a model or axioms of equivalence, counted equally (½ a fragment, a modelled VM, or assumed specs for part of the target) · (c) a primitive runs work in an external faster language and returns outputs plus time and memory as values (½ a foreign call with outputs only, or a cost report beside the run)

Weights: 40 · 30 · 30

| Project | a | b | c | % | Why |
|---|:-:|:-:|:-:|--:|---|
| disp (theoretical) | 1 | 1 | 1 | 100 | designed, not built: a hardware model is a tree program whose cost is its output (`research/OPTIMIZER.typ` §9), so claims certify code for it; the one impure driver at the program's boundary runs work in a faster language and returns outputs plus cost (`FOUNDATIONS.md` §8) |
| disp (today) | 0 | 0 | ½(cost report) | 15 | interpreted tree-walkers, no target model, no handoff; `--stats` reports steps beside the run |
| HVM4 / Interaction Calculus | 1 | 0 | ½(cost report)† | 55 | native C with GPU lineage; the runtime reports interaction counts; no verified path |
| Bend 2 / BendTT / BendRT | 1 | 0 | ? | 40? | 0.8–1.5× hand-written C sequential, 9–12× on 16 threads, up to 67× on the GPU, self-reported; clang, Metal and CUDA trusted and the C runtime unverified; the pre-release interaction counter was dropped ("bend has no --stats"); whether it can call out is not in the write-up |
| Metamath Zero | ½(not general) | 1 | 0 | 50 | MMC compiles to a formal x86 model with the correctness proved; not a general systems language; no cost model |
| Verus | 1 | ½(assumed specs)† | ½(FFI)† | 70 | it is Rust; rustc trusted, no cost model |
| F* / Low* / Pulse | 1 | 1† | ½(FFI)† | 85 | Low*→C competitive with hand-optimized C; KaRaMeL and the C compiler are trusted |
| Lean 4 | ½(not systems) | 0 | ½(FFI)† | 35 | via C, reference-counted; fine for tooling, not systems class; `@[extern]` calls C and returns outputs only |
| Nova | 0 | 0 | 0 | 0 | a research-scale Idris 2 program |
| Blight | 0 | 0 | 0 | 0 | no native backend, no cost model |
| Soma | 1 | 0 | ? | 40? | LLVM native, GC-free flat types at C cost; LLVM trusted; whether it can call out is not in the write-up |
| fiat-crypto + CryptOpt + Jasmin | 1 | 1 | ½(in the tool) | 85 | beats GCC/Clang; Jasmin and bedrock2 compile under verified compilers; cost is measured on the CPU inside the search rather than modelled |
| Velvet / Loom / WybeCoder | 0 | 0 | 0 | 0 | programs are extracted for testing only |
| Agda / Cubical | 0 | 0 | ½(FFI)† | 15 | research-grade GHC/JS backends |
| Narya | 0 | 0 | 0 | 0 | correctness-first, no performance story |
| Dedukti / Lambdapi | 0 | 0 | 0 | 0 | a proof checker: no compiled output, no cost model |
| Andromeda 2 | 0 | 0 | 0 | 0 | an interpreter for a proof checker |
| Idris 2 / QTT | ½(not systems) | 0 | ½(FFI)† | 35 | Chez backend; erasure is graded under Specification |
| CakeML + Pancake | 1 | 1 | ½(FFI)† | 85 | verified native code, the compiler proved down to the binary; no cost model |
| ATS3 / Xanadu | 1 | 0 | ½(FFI)† | 55 | to C, no GC, no runtime; the C compiler trusted |
| Vow | 1 | 0 | ? | 40? | Cranelift native with a byte-identical bootstrap; whether it can call out is not in the write-up |
| Dafny | 0 | 0 | ½(FFI)† | 15 | managed backends |
| NanoLang | 1 | ½(modelled VM) | ½(FFI) | 70 | transpiles to C; NanoISA is a modelled VM whose semantics are proved, used to sandbox FFI |
| Rust cluster | 1 | ½(assumed specs)† | ½(FFI)† | 70 | it is Rust |
| Mojo | 1 | 0 | ½(FFI)† | 55 | MLIR codegen for CPU/GPU/accelerators |
| LogosLang | 1 | 0 | ½(JIT call) | 55 | Cranelift JIT within 3× of vectorized C |
| Acorn | 0 | 0 | 0 | 0 | not a systems language |
| Salt | ½(claimed) | 0 | ? | 20? | MLIR→LLVM with claimed `-O3` parity, unverified; whether it can call out is not in the write-up |
| Telomare (Stand-In Language) | 0 | 0 | ½(cost report) | 15 | a Haskell tree-walker; `--meter` counts steps and nodes deterministically; the static bound is graded under Specification |
| Stellogen | 0 | 0 | 0 | 0 | execution has "a horrible complexity" |
| Nock / Hoon | ½(jetted interpreter) | 0 | ½(jets) | 35 | C and Rust interpreters with jets; the frozen spec models Nock itself, not the machine, and no checker reasons about it; `%bout` timing hints, cost as a value forbidden; jets run in C and return outputs only |
| Thermite | 1 | ½(validated fragment) | ½(FFI)† | 70 | native via rustc; translation-validated lowering for a fragment; no cost model |
| Indie AI-first cluster | ½(per member) | ½(one member) | ½(per member) | 50 | LSTS compiles to C and CLR rides on Zig; Verity is a verified compiler to EVM bytecode |

## G5 Search

(a) spec → implementation automatically (½ an external LLM loop, harness generation, basic proof search) · (b) scored by the checker and by measured cost, both in the loop (½ one of the two) · (c) self-play: the search writes its own score functions to then satisfy (½ it generates adversaries or tests) · (d) the search is aimed at itself (½ a flywheel started)

Weights: 35 · 35 · 15 · 15

| Project | a | b | c | d | % | Why |
|---|:-:|:-:|:-:|:-:|--:|---|
| disp (theoretical) | 1 | 1 | ½(counterexamples kept) | ½(Löbian ceiling) | 85 | `cegis` is pinned; generators derived from claims widen its pool (`research/CLAIMS_OVER_TREES.md` §4); score = verdict × cost (`research/OPTIMIZER.typ` §2); nothing designed writes new claims; a successor's soundness can't be proved from inside (`research/LOB_AND_CURRY.md` §1) |
| disp (today) | ½(basic search) | ½(checker only) | ½(counterexamples kept) | 0 | 42 | `cegis` walks a listed pool of candidates against a claim and keeps the counterexamples it finds; cost isn't in the loop, and nothing searches over disp itself |
| HVM4 / Interaction Calculus | ½(value enumeration) | ½(tests only) | 0 | 0 | 35 | the primer solves X + 2 = 4 by collapsing an infinite superposition of naturals; the program-hole filler (SupGen, NeoGen) was never published; equations rather than a checker, no cost objective |
| Bend 2 / BendTT / BendRT | ½(external LLM loop) | ½(checker only) | 0 | 0 | 35 | the shipped loop is a human LAWS.bend, an AI-written PROOF.bend and the checker as the gate (Bender wraps Anthropic/OpenAI models); the checker scores, cost never does; no search of its own |
| Metamath Zero | 0 | 0 | 0 | 0 | 0 | MM1 tactics are human-written |
| Verus | ½(external LLM loop) | 1 | 0 | 0 | 52 | IDS synthesizes code and proofs with benchmarks inside the loop (7/7 KV-store specs); the proposer is an external LLM |
| F* / Low* / Pulse | 0 | 0 | 0 | 0 | 0 | humans write the code and the proofs |
| Lean 4 | ½(external agents) | ½(checker only) | 0 | 0 | 35 | the richest external ecosystem of proof-search agents; nothing built in; no cost |
| Nova | 0 | 0 | 0 | 0 | 0 | none |
| Blight | 0 | 0 | 0 | 0 | 0 | tactics are human-written |
| Soma | 0 | 0 | 0 | 0 | 0 | none |
| fiat-crypto + CryptOpt + Jasmin | 1 | 1 | 0 | 0 | 70 | randomized search over assembly with measured cost and a verified equivalence check; straight-line only; unmaintained since 2024 |
| Velvet / Loom / WybeCoder | ½(external LLM loop) | ½(checker only) | 0 | 0 | 35 | WybeCoder's agentic loop proves 74% of Verina; correctness only, no cost |
| Agda / Cubical | 0 | 0 | 0 | 0 | 0 | `auto` is trivial |
| Narya | 0 | 0 | 0 | 0 | 0 | no tactics even |
| Dedukti / Lambdapi | ½(ATP proof export) | ½(checker only) | 0 | 0 | 35 | Zenon Modulo, LEO-III and SMT solvers via Carcara emit proofs the kernel checks; the `why3` tactic admits its goal as an axiom; no cost |
| Andromeda 2 | 0 | 0 | 0 | 0 | 0 | handlers direct hand-written proof procedures; no synthesis |
| Idris 2 / QTT | 0 | 0 | 0 | 0 | 0 | `auto` is basic |
| CakeML + Pancake | 0 | 0 | 0 | 0 | 0 | human-written |
| ATS3 / Xanadu | 0 | 0 | 0 | 0 | 0 | none |
| Vow | ½(LLM loop) | ½(checker only) | 0 | 0 | 35 | CEGIS with an external LLM; no cost objective |
| Dafny | ½(external LLMs) | ½(checker only) | 0 | 0 | 35 | LLMs write it best (82–96% on benchmarks) but the search lives in external tools; correctness only |
| NanoLang | 0 | 0 | 0 | 0 | 0 | designed as an LLM target with mandatory tests; no loop of its own for them to score in |
| Rust cluster | ½(harness gen) | ½(checker only) | 0 | 0 | 35 | Kani's autoharness; otherwise human-driven with AI assistance |
| Mojo | ½(sweeps) | ½(cost only) | 0 | 0 | 35 | offline parameter sweeps, cost only, correctness assumed |
| LogosLang | 0 | 0 | 0 | 0 | 0 | none |
| Acorn | ½(local model) | ½(checker only) | 0 | ½(data flywheel) | 42 | an embedded local model fills in proofs with the checker in the loop; the library becomes the next model's training data |
| Salt | 0 | 0 | 0 | 0 | 0 | none |
| Telomare (Stand-In Language) | 0 | 0 | 0 | 0 | 0 | superposition is aimed at bounds, not candidates |
| Stellogen | 0 | 0 | 0 | 0 | 0 | none intended |
| Nock / Hoon | 0 | 0 | 0 | 0 | 0 | `honk`'s parity policy is a differential oracle, not a search |
| Thermite | ½(agent loop) | ½(checker only) | ½(mutants) | 0 | 42 | a purpose-built external-agent loop with the checker and mutation scoring in it; no cost objective |
| Indie AI-first cluster | ½(agent loops) | ½(checker only) | ½(refutation challenges) | 0 | 42 | agent loops around contracts; Prove's refutation challenges |

## Approach (unscored)

How each project builds things disp's way: disp's bets from `FOUNDATIONS.md`, not requirements of `GOALS.md`, so they count toward no axis. Same 0, ½, 1 values; a — means there is no native path to install.

| Project | no quotation layer | cost in types | deterministic cost | licensed fast paths |
|---|:-:|:-:|:-:|:-:|
| disp | 1 | 0 | 1 | 0 |
| HVM4 / Interaction Calculus | ½(graphs only) | 0 | 1† | 0 |
| Bend 2 / BendTT / BendRT | 0 | ½(usage grade) | 0 | 0 |
| Metamath Zero | 0 | 0 | 0 | ½(compile only) |
| Verus | 0 | 0 | 0 | 0 |
| F* / Low* / Pulse | 0 | 0 | 0 | 0 |
| Lean 4 | 0 | 0 | 0 | 1† |
| Nova | 0 | 0 | 0 | — |
| Blight | 0 | ½(usage grade) | 0 | — |
| Soma | 0 | ½(usage grade) | 0 | 0 |
| fiat-crypto + CryptOpt + Jasmin | 0 | 0 | ½(measured outside model) | 1 |
| Velvet / Loom / WybeCoder | 0 | 0 | 0 | — |
| Agda / Cubical | 0 | 0 | 0 | ½(asserted)† |
| Narya | 0 | 0 | 0 | — |
| Dedukti / Lambdapi | ½(patterns only) | 0 | 0 | ½(type-preservation only) |
| Andromeda 2 | 0 | 0 | 0 | — |
| Idris 2 / QTT | 0 | ½(erasure) | 0 | ½(asserted)† |
| CakeML + Pancake | 0 | 0 | 0 | ½(fixed passes) |
| ATS3 / Xanadu | 0 | 0 | 0 | 0 |
| Vow | 0 | 0 | 0 | 0 |
| Dafny | 0 | 0 | 0 | — |
| NanoLang | 0 | 0 | 0 | 0 |
| Rust cluster | 0 | 0 | 0 | 0 |
| Mojo | ½(staging) | 0 | 0 | ½(asserted) |
| LogosLang | 1 | 0 | 0 | 0 |
| Acorn | 0 | 0 | 0 | — |
| Salt | 0 | 0 | 0 | 0 |
| Telomare (Stand-In Language) | 0 | ½(static bound) | 1 | — |
| Stellogen | ½(no eval back) | 0 | 0 | — |
| Nock / Hoon | 1 | 0 | ½(timing hints) | ½(asserted) |
| Thermite | 0 | 0 | 0 | 0 |
| Indie AI-first cluster | 0 | 0 | 0 | 0 |
