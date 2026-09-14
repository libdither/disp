# Scores

Every score, in one place (2026-09-14). Each axis's maximum is disp's own requirement, split into
three or four clauses in [`_AXES.md`](_AXES.md#grading-the-maximum-and-the-clauses); every clause
scores 0, ½ or 1 and the axis percentage is their mean. The write-ups' scorecard notes are the
evidence. `†` marks a value taken from general knowledge beyond the write-up; `?` marks a clause the
write-up does not settle (scored 0, percentage provisional); `—` marks a clause that does not apply
(left out of the mean). Rows follow the master table, disp first. This file is the source:
`scripts/awesome-scores.py --write` recomputes the percentages and copies every cell into the
write-ups, the master table and disp's own table.

## G1 Substrate

(a) programs can inspect other programs · (b) one representation is both run and inspected, no quotation layer · (c) the checker is callable from programs as an ordinary function (½ from metaprograms only)

| Project | a | b | c | % | Why |
|---|:-:|:-:|:-:|--:|---|
| disp | 1 | 1 | 1 | 100 | `shape_of` triages any program; the checker is an ordinary tree you apply |
| HVM4 / Bend2 / SupGen | ½(runtime graphs) | ½(graphs only) | 0 | 33 | terms are runtime graph nodes with no in-language inspection; untyped, no checker |
| Metamath Zero | 1 | 0 | ? | 33? | MM1 metaprograms manipulate terms over a deep embedding; whether they can call the verifier is not in the write-up |
| Verus | 0 | 0 | 0 | 0 | ghost code is erased; the checker is not a Verus program |
| F* / Low* / Pulse | 1 | 0 | ½(metaprograms)† | 50 | Meta-F* reflects F* syntax (deep embedding); tactics can call the typechecker |
| Lean 4 | 1 | 0 | ½(metaprograms)† | 50 | `Expr` deep embedding; `MetaM` exposes inference and checking to metaprograms |
| Nova | 0 | 0 | 0 | 0 | standard elaborator/kernel pipeline |
| Blight | ? | 0 | 0 | 0? | s-expressions "invite metaprogramming"; the write-up does not say what a program can inspect |
| Soma | 0 | 0 | 0 | 0 | conventional compiler pipeline |
| fiat-crypto + CryptOpt + Jasmin | 1 | 0 | ½(metaprograms) | 50 | Gallina metaprogramming over a deep embedding; tactics see the checker |
| Velvet / Loom / WybeCoder | 1 | 0 | ½(metaprograms) | 50 | Lean metaprogramming; Loom generates verifiers as Lean developments |
| Agda / Cubical | 1 | 0 | ½(metaprograms)† | 50 | `quoteTerm`/`unquote`; the TC monad can infer and check types |
| Narya | 0 | 0 | 0 | 0 | no reflection API at all |
| Idris 2 / QTT | 1 | 0 | ½(metaprograms) | 50 | elaborator reflection, quotation-based |
| CakeML + Pancake | 0 | 0 | 0 | 0 | conventional verified compiler |
| ATS3 / Xanadu | 0 | 0 | 0 | 0 | no programs-as-data |
| Vow | 0 | 0 | 0 | 0 | macros and metaprogramming excluded on purpose |
| Dafny | 0 | 0 | 0 | 0 | none |
| NanoLang | 0 | 0 | 0 | 0 | none |
| Rust cluster | 0 | 0 | 0 | 0 | structurally impossible in the two-layer design |
| Mojo | ½(types only) | ½(staging) | 0 | 33 | `reflect[T]` reads type structure only, natively; terms are never data; the checker is C++ |
| LogosLang | ½(walk only) | 1 | 0 | 50 | the Logic Graph is walkable today and is the program; self-rewriting and the proof layer are unbuilt |
| Acorn | 0 | 0 | 0 | 0 | none |
| Salt | 0 | 0 | 0 | 0 | none |
| Telomare (Stand-In Language) | ½(env only) | 0 | 0 | 17 | a closure's environment is a pair you can project; its code is an opaque `Defer`; every analysis is Haskell |
| Stellogen | 1 | ½(no eval back) | 1 | 83 | code is a first-order term that shape checkers unify against; no eval back to code; types are library constellations |
| Nock / Hoon | 1 | 1 | ½(unused vases) | 83 | formulas are nouns and opcode 2 is eval; vases let programs typecheck programs, but nothing checks Nock with it |
| Thermite | 0 | 0 | 0 | 0 | no reflection; Forge is an external tool and programs are never data |
| Indie AI-first cluster | 0 | 0 | 0 | 0 | contracts + SMT on a host; no reflection anywhere in the genre |

## G2 Specification

(a) specifications mention values (½ contracts or refinements over runtime values, 1 dependent types) · (b) propositions and proofs are first-class, with a library in use (½ solver-discharged obligations, or proofs without a library) · (c) resources and cost as types (½ a usage grade, erasure by quantity, or a static bound that is not a type) · (d) an equality theory you can state and check: extensional, cubical, observational (½ propositional or SMT-fragment equality, tactic-driven or a decidable fragment)

| Project | a | b | c | d | % | Why |
|---|:-:|:-:|:-:|:-:|--:|---|
| disp | 1 | ½(small library) | 0 | ½(witness slices) | 50 | Pi, telescopes, coproducts and proofs that run, over a small library; cost as a grade is designed; equality is intensional with witness-licensed slices and the decidable fragment open (Q1) |
| HVM4 / Bend2 / SupGen | 0 | 0 | 0 | 0 | 0 | untyped; Bend2's types and proofs are planned |
| Metamath Zero | 0 | 1 | 0 | 0 | 25 | a HOL subset with explicit proof objects and a modest library; no dependent types, no resource types, equality not addressed |
| Verus | ½(contracts) | ½(solver-discharged) | 0 | ½(SMT-fragment) | 38 | first-order contracts with ghost state, Z3-discharged, over a large verified codebase; SMT-fragment equalities |
| F* / Low* / Pulse | 1 | 1 | 0 | ½(SMT-fragment) | 62 | dependent types, refinements, effects, Pulse, with HACL* as the library; SMT-fragment equality |
| Lean 4 | 1 | 1 | 0 | ½(propositional) | 62 | full dependent types, universes, mathlib; propositional equality with Quot and axioms, rewriting tactic-driven |
| Nova | 1 | ½(no library) | 0 | 1 | 62 | extensional MLTT: anything provably equal is treated as equal and certificates recover decidability; solo, no library |
| Blight | 1 | ½(no library) | ½(usage grade) | 1 | 75 | cubical + QTT + effects: computational univalence and quantities in the kernel; two months old, no library |
| Soma | 1 | 0 | ½(usage grade) | 0 | 38 | dependent types + QTT + rows, not a proof assistant; quantities are a usage grade; equality not addressed |
| fiat-crypto + CryptOpt + Jasmin | 1 | 1 | 0 | ½(asm fragment) | 62 | full CIC with fiat-crypto and bedrock2 as the library; a verified equivalence checker for straight-line assembly is the equality story, a tiny fragment |
| Velvet / Loom / WybeCoder | 1 | 1 | 0 | ½(SMT-fragment) | 62 | Lean + mathlib behind a Dafny-style surface; Lean's equality plus SMT in its fragment |
| Agda / Cubical | 1 | 1 | 0 | 1 | 75 | dependent types, HITs, univalence with the standard and cubical libraries; cubical equality computes |
| Narya | 1 | ½(no library) | 0 | ½(some formers) | 50 | HoTT + parametricity + modalities, no library yet; observational Id per former, transport computing on some formers so far |
| Idris 2 / QTT | 1 | 1 | ½(erasure) | ½(propositional) | 75 | dependent types + QTT with a working standard library; quantities give erasure; intensional propositional equality |
| CakeML + Pancake | ½(contracts) | ½(compiler-level) | 0 | ½(pass preservation) | 38 | Viper contracts for user code, HOL4 proofs about the compiler; per-pass semantics preservation is the equality, proved by humans |
| ATS3 / Xanadu | 1 | 1† | 0 | 0 | 50 | dependent + linear types with an explicit proof language and a 25-year library; equality not addressed |
| Vow | ½(contracts) | ½(BMC-discharged) | 0 | 0 | 25 | contracts + loop invariants under bounded model checking; young; equality not addressed |
| Dafny | ½(contracts) | ½(solver-discharged) | 0 | ½(SMT-fragment) | 38 | first-order + quantifiers over a decade of AWS specs; SMT-fragment equality |
| NanoLang | 0 | 0 | 0 | 0 | 0 | no user-level spec language; the proofs are about the language |
| Rust cluster | 1 | 1 | 0 | ½(per-tool translation) | 62 | from Flux refinements to RefinedRust's Iris and Aeneas's Lean, with libcrux and ACE-RISCV as the library; Aeneas's borrow translation is a machine-produced equivalence, per tool |
| Mojo | ½(comptime values) | 0 | 0 | 0 | 12 | types indexed by compile-time values only; no propositions, no proofs, no equality |
| LogosLang | 0 | 0 | 0 | 0 | 0 | the proof layer and the rewriting engine are design documents |
| Acorn | 0 | 1 | 0 | 0 | 25 | a theorem-proving language for mathematics with a growing library; no program specifications |
| Salt | ½(contracts) | ½(solver-discharged) | 0 | 0 | 25 | Z3 contracts (claimed), no library, equality not addressed |
| Telomare (Stand-In Language) | ½(runtime refinements) | 0 | ½(static bound) | 0 | 25 | STLC-shaped statics with refinements as runtime validators; a static, inferred iteration bound per site; structural equality only |
| Stellogen | ½(value tests) | 0 | 0 | 0 | 12 | types are user-space test galaxies over values; no dependent types, no proofs; `==` is syntactic |
| Nock / Hoon | 0 | 0 | 0 | 0 | 0 | structural types with variance; nothing states a value property; opcode 5 is structural equality |
| Thermite | ½(contracts) | ½(solver-discharged) | 0 | ½(SMT-fragment) | 38 | contracts and invariants, solver-discharged along a recorded ladder; termination measures but no cost or resource types; SMT-fragment equality |
| Indie AI-first cluster | ½(contracts) | ½(solver-discharged) | 0 | 0 | 25 | Z3 contracts throughout, Aver and Verity borrowing Lean proofs; no equality story |

## G3 Trust

(a) a trusted core small enough to audit · (b) it mints unforgeable evidence: theorems, proof objects, certificates (½ proof objects checked only by a large checker) · (c) the clever layers are untrusted and re-checked by the core, and an independent checker exists (½ re-checked by the one core only)

| Project | a | b | c | % | Why |
|---|:-:|:-:|:-:|--:|---|
| disp | 1 | 1 | ½(one core) | 83 | a two-op core mints hypotheses; elaborator and library untrusted; five evaluators agree but nothing independent re-checks the kernel's verdicts |
| HVM4 / Bend2 / SupGen | 0 | 0 | 0 | 0 | trust is `hvm.c` |
| Metamath Zero | 1 | 1 | 1 | 100 | a C verifier small enough to formalize; MMB proof objects; MM1 untrusted; independent verifiers in Rust and Kotlin |
| Verus | 0 | 0 | 0 | 0 | TCB = Verus + Z3 + rustc |
| F* / Low* / Pulse | 0 | ½(large checker) | 0 | 17 | typechecker, Z3 and KaRaMeL are all trusted; proof terms exist, SMT verdicts have none |
| Lean 4 | ½(mid-size kernel) | 1 | 1 | 83 | a small kernel, larger than MM0's; proof terms; elaborator and tactics untrusted; lean4lean and lean4export re-check |
| Nova | 1 | 1 | ? | 67? | a small kernel with its own spec re-checks certificate-carrying artifacts; no independent checker is mentioned |
| Blight | 1 | 1 | 1 | 100 | a microscopic kernel is the only thing that can mint a `Proof`; the tower is untrusted; a second independently written re-checker |
| Soma | 0 | 0 | 0 | 0 | trust the compiler |
| fiat-crypto + CryptOpt + Jasmin | ½(mid-size kernel) | 1 | 1† | 83 | Rocq's kernel; CryptOpt's equivalence checker is verified in Rocq; coqchk re-checks |
| Velvet / Loom / WybeCoder | ½(mid-size kernel) | 1 | 1 | 83 | Lean's kernel; Loom verifiers are foundational, no trusted VC generator |
| Agda / Cubical | 0 | ½(large checker) | 0 | 17 | the large typechecker is the TCB; proof terms exist, nothing smaller re-checks them |
| Narya | 0 | ½(large checker) | 0 | 17 | tens of thousands of OCaml lines are the trust base; NbE unproven |
| Idris 2 / QTT | 0 | ½(large checker) | 0 | 17 | the typechecker is the TCB |
| CakeML + Pancake | 1 | 1 | 1† | 100 | HOL4's LCF kernel; theorems are the evidence; independent HOL checkers exist (OpenTheory, Candle) |
| ATS3 / Xanadu | 0 | ½(large checker)† | 0 | 17 | the typechecker is the TCB; proof terms exist |
| Vow | 0 | 0 | 0 | 0 | TCB = vowc + ESBMC |
| Dafny | 0 | 0 | 0 | 0 | TCB = Dafny + Boogie + Z3 |
| NanoLang | ½(soundness proof) | 0 | 0 | 17 | no kernel, but the core semantics are proved sound in Coq, `Admitted`-free |
| Rust cluster | ½(per tool) | ½(per tool) | ½(per tool) | 50 | Aeneas/hax/RefinedRust inherit Lean/F*/Rocq kernels; Kani/Creusot/Flux trust SMT |
| Mojo | 0 | 0 | 0 | 0 | the whole MLIR/LLVM C++ stack |
| LogosLang | 0 | 0 | 0 | 0 | no kernel discipline described |
| Acorn | 0 | ? | 0 | 0? | has its own checker; the write-up does not characterize its TCB or proof objects |
| Salt | 0 | 0 | 0 | 0 | TCB = saltc + Z3 + MLIR (claimed) |
| Telomare (Stand-In Language) | 0 | 0 | 0 | 0 | the whole compiler is trusted; `--certificate` is a report nothing re-checks |
| Stellogen | 1 | 0 | ½(paper theorems) | 50 | ~3,200 lines with a normative spec and two trusted observations; checkers are user space; trust rests on paper theorems, no evidence objects |
| Nock / Hoon | 1 | 0 | ½(conformance suite) | 50 | a page of spec, a conformance suite, independent interpreters; jets are an unbounded trusted surface with no evidence and no production differential check |
| Thermite | ½(L4 via Lean) | ½(fragment evidence) | ½(audit replay) | 50 | L3 trusts Verus+Z3+rustc while L4 replays certificates through axiom-probed Lean; the manifest is a report, the Lean proofs are evidence for a fragment; audit re-derives the chain with independent replay but SMT verdicts carry nothing |
| Indie AI-first cluster | 0 | 0 | 0 | 0 | contracts + SMT; Verity (Lean, zero axioms) is the exception |

## G4 Execution

(a) C/Rust-class native execution (½ compiled but not systems class) · (b) a verified or modelled path to the machine: a verified compiler or a hardware model in the logic (½ a verified backend for a fragment, or a modelled VM) · (c) the runtime accounts for its own cost deterministically: steps, interaction counts, a replayable model (½ timing hints or a report outside the model) · (d) definitions are replaced by faster equivalents on a checked license (½ asserted, like jets or unverified compiler passes, or a fixed set of proved passes)

| Project | a | b | c | d | % | Why |
|---|:-:|:-:|:-:|:-:|--:|---|
| disp | 0 | 0 | 1 | ½(one slice) | 38 | interpreted tree-walkers; `--stats` steps and `cold_equiv` are a deterministic, replayable cost model; no hardware model; `.opt.disp` overlays replace definitions under a license, one slice landed |
| HVM4 / Bend2 / SupGen | 1 | 0 | 1† | 0 | 50 | native C with GPU lineage; the runtime reports interaction counts; no verified path, no rewrite mechanism |
| Metamath Zero | ½(not general) | 1 | 0 | ½(compile only) | 50 | MMC compiles to a formal x86 model with the correctness proved; not a general systems language; no cost model; compilation is the only licensed rewrite |
| Verus | 1 | 0 | 0 | 0 | 25 | it is Rust; rustc trusted, no cost model, no rewrite license |
| F* / Low* / Pulse | 1 | 0 | 0 | 0 | 25 | Low*→C competitive with hand-optimized C; KaRaMeL and the C compiler are trusted |
| Lean 4 | ½(not systems) | 0 | 0 | 0 | 12 | via C, reference-counted; fine for tooling, not systems class |
| Nova | 0 | 0 | 0 | 0 | 0 | a research-scale Idris 2 program |
| Blight | 0 | 0 | 0 | 0 | 0 | no native backend, no cost model |
| Soma | 1 | 0 | 0 | 0 | 25 | LLVM native, GC-free flat types at C cost; LLVM trusted |
| fiat-crypto + CryptOpt + Jasmin | 1 | 1 | 0 | 1 | 75 | beats GCC/Clang; Jasmin and bedrock2 compile under verified compilers; CryptOpt's output is installed on a verified equivalence check; cost is measured on the CPU inside the search rather than modelled |
| Velvet / Loom / WybeCoder | 0 | 0 | 0 | 0 | 0 | programs are extracted for testing only |
| Agda / Cubical | 0 | 0 | 0 | 0 | 0 | research-grade GHC/JS backends |
| Narya | 0 | 0 | 0 | 0 | 0 | correctness-first, no performance story |
| Idris 2 / QTT | ½(not systems) | 0 | 0 | 0 | 12 | Chez backend; erasure is graded under Specification |
| CakeML + Pancake | 1 | 1 | 0 | ½(fixed passes) | 62 | verified native code, the compiler proved down to the binary; the proved passes are fixed, not user-defined; no cost model |
| ATS3 / Xanadu | 1 | 0 | 0 | 0 | 25 | to C, no GC, no runtime; the C compiler trusted |
| Vow | 1 | 0 | 0 | 0 | 25 | Cranelift native with a byte-identical bootstrap |
| Dafny | 0 | 0 | 0 | 0 | 0 | managed backends |
| NanoLang | 1 | ½(modelled VM) | 0 | 0 | 38 | transpiles to C; NanoISA is a modelled VM whose semantics are proved, used to sandbox FFI |
| Rust cluster | 1 | 0 | 0 | 0 | 25 | it is Rust |
| Mojo | 1 | 0 | 0 | ½(asserted) | 38 | MLIR codegen for CPU/GPU/accelerators; MLIR rewrites are asserted compiler transforms, unverified |
| LogosLang | 1 | 0 | 0 | 0 | 25 | Cranelift JIT within 3× of vectorized C; `.compile()` lowers a function, it does not license a replacement |
| Acorn | 0 | 0 | 0 | 0 | 0 | not a systems language |
| Salt | ½(claimed) | 0 | 0 | 0 | 12 | MLIR→LLVM with claimed `-O3` parity, unverified |
| Telomare (Stand-In Language) | 0 | 0 | 1 | 0 | 25 | a Haskell tree-walker; `--meter` counts steps and nodes deterministically; the static bound is graded under Specification |
| Stellogen | 0 | 0 | 0 | 0 | 0 | execution has "a horrible complexity" |
| Nock / Hoon | ½(jetted interpreter) | ½(modelled VM) | ½(timing hints) | ½(asserted) | 50 | C and Rust interpreters with jets; a page of frozen spec with a conformance suite is a modelled VM; `%bout` timing hints, cost as a value forbidden; jets are asserted equivalences dispatched on intensional identity |
| Thermite | 1 | ½(validated fragment) | 0 | 0 | 38 | native via rustc; translation-validated lowering for a fragment; no cost model; no licensed rewrites |
| Indie AI-first cluster | ½(per member) | ½(one member) | 0 | 0 | 25 | LSTS compiles to C and CLR rides on Zig; Verity is a verified compiler to EVM bytecode |

## G5 Search

(a) spec → implementation automatically (½ an external LLM loop, harness generation, basic proof search) · (b) scored by the checker and by measured cost, both in the loop (½ one of the two) · (c) the search is aimed at itself (½ a flywheel started)

| Project | a | b | c | % | Why |
|---|:-:|:-:|:-:|--:|---|
| disp | 0 | 0 | 0 | 0 | the optimizer is designed (`research/OPTIMIZER.typ`), not built; no search exists yet |
| HVM4 / Bend2 / SupGen | 1 | ½(tests only) | 0 | 50 | SupGen enumerates superposed candidates until tests pass; tests rather than a checker, no cost objective |
| Metamath Zero | 0 | 0 | 0 | 0 | MM1 tactics are human-written |
| Verus | 1 | 1 | 0 | 67 | IDS synthesizes code and proofs with benchmarks inside the loop (7/7 KV-store specs); the proposer is an external LLM |
| F* / Low* / Pulse | 0 | 0 | 0 | 0 | humans write the code and the proofs |
| Lean 4 | ½(external agents) | ½(checker only) | 0 | 33 | the richest external ecosystem of proof-search agents; nothing built in; no cost |
| Nova | 0 | 0 | 0 | 0 | none |
| Blight | 0 | 0 | 0 | 0 | tactics are human-written |
| Soma | 0 | 0 | 0 | 0 | none |
| fiat-crypto + CryptOpt + Jasmin | 1 | 1 | 0 | 67 | randomized search over assembly with measured cost and a verified equivalence check; straight-line only; unmaintained since 2024 |
| Velvet / Loom / WybeCoder | 1 | ½(checker only) | 0 | 50 | WybeCoder's agentic loop proves 74% of Verina; correctness only, no cost |
| Agda / Cubical | 0 | 0 | 0 | 0 | `auto` is trivial |
| Narya | 0 | 0 | 0 | 0 | no tactics even |
| Idris 2 / QTT | 0 | 0 | 0 | 0 | `auto` is basic |
| CakeML + Pancake | 0 | 0 | 0 | 0 | human-written |
| ATS3 / Xanadu | 0 | 0 | 0 | 0 | none |
| Vow | ½(LLM loop) | ½(checker only) | 0 | 33 | CEGIS with an external LLM; no cost objective |
| Dafny | ½(external LLMs) | ½(checker only) | 0 | 33 | LLMs write it best (82–96% on benchmarks) but the search lives in external tools; correctness only |
| NanoLang | 0 | ½(tests only) | 0 | 17 | designed as an LLM target with mandatory tests; no engine of its own |
| Rust cluster | ½(harness gen) | ½(checker only) | 0 | 33 | Kani's autoharness; otherwise human-driven with AI assistance |
| Mojo | ½(sweeps) | ½(cost only) | 0 | 33 | offline parameter sweeps, cost only, correctness assumed |
| LogosLang | 0 | 0 | 0 | 0 | none |
| Acorn | ½(local model) | ½(checker only) | ½(data flywheel) | 50 | an embedded local model fills in proofs with the checker in the loop; the library becomes the next model's training data |
| Salt | 0 | 0 | 0 | 0 | none |
| Telomare (Stand-In Language) | 0 | 0 | 0 | 0 | superposition is aimed at bounds, not candidates |
| Stellogen | 0 | 0 | 0 | 0 | none intended |
| Nock / Hoon | 0 | 0 | 0 | 0 | `honk`'s parity policy is a differential oracle, not a search |
| Thermite | ½(agent loop) | ½(checker only) | 0 | 33 | a purpose-built external-agent loop with the checker and mutation scoring in it; no cost objective |
| Indie AI-first cluster | ½(agent loops) | ½(checker only) | 0 | 33 | agent loops around contracts; Prove's refutation challenges |
