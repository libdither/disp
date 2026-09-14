# Scores

Draft for review (2026-09-14). Each axis's maximum is disp's own requirement, split into
three clauses in [`_AXES.md`](_AXES.md#grading-the-maximum-and-the-clauses); every clause
scores 0, ½ or 1 and the axis percentage is their mean. The write-ups' scorecard notes are
the evidence. `†` marks a value taken from general knowledge beyond the write-up; `?` marks
a clause the write-up does not settle (scored 0, percentage provisional). Rows follow the
master table, disp first. `scripts/awesome-scores.py` recomputes the percentages, ranks each
column, and lists every derived symbol that differs from the master table.

## A1 Reflection / programs-as-data

(a) programs can inspect other programs · (b) one representation is both run and inspected, no quotation layer · (c) the checker is callable from programs as an ordinary function

| Project | a | b | c | % | Why |
|---|:-:|:-:|:-:|--:|---|
| disp | 1 | 1 | 1 | 100 | `shape_of` triages any program; the checker is an ordinary tree you apply |
| HVM4 / Bend2 / SupGen | ½ | ½ | 0 | 33 | terms are runtime graph nodes with no in-language inspection; untyped, no checker |
| Metamath Zero | 1 | 0 | ? | 33? | MM1 metaprograms manipulate terms over a deep embedding; whether they can call the verifier is not in the write-up |
| Verus | 0 | 0 | 0 | 0 | ghost code is erased; the checker is not a Verus program |
| F* / Low* / Pulse | 1 | 0 | ½† | 50 | Meta-F* reflects F* syntax (deep embedding); tactics can call the typechecker |
| Lean 4 | 1 | 0 | ½† | 50 | `Expr` deep embedding; `MetaM` exposes inference and checking to metaprograms |
| Nova | 0 | 0 | 0 | 0 | standard elaborator/kernel pipeline |
| Blight | ? | 0 | 0 | 0? | s-expressions "invite metaprogramming"; the write-up does not say what a program can inspect |
| Soma | 0 | 0 | 0 | 0 | conventional compiler pipeline |
| fiat-crypto + CryptOpt + Jasmin | 1 | 0 | ½ | 50 | Gallina metaprogramming over a deep embedding; tactics see the checker |
| Velvet / Loom / WybeCoder | 1 | 0 | ½ | 50 | Lean metaprogramming; Loom generates verifiers as Lean developments |
| Agda / Cubical | 1 | 0 | ½† | 50 | `quoteTerm`/`unquote`; the TC monad can infer and check types |
| Narya | 0 | 0 | 0 | 0 | no reflection API at all |
| Idris 2 / QTT | 1 | 0 | ½ | 50 | elaborator reflection, quotation-based |
| CakeML + Pancake | 0 | 0 | 0 | 0 | conventional verified compiler |
| ATS3 / Xanadu | 0 | 0 | 0 | 0 | no programs-as-data |
| Vow | 0 | 0 | 0 | 0 | macros and metaprogramming excluded on purpose |
| Dafny | 0 | 0 | 0 | 0 | none |
| NanoLang | 0 | 0 | 0 | 0 | none |
| Rust cluster | 0 | 0 | 0 | 0 | structurally impossible in the two-layer design |
| Mojo | ½ | ½ | 0 | 33 | `reflect[T]` reads type structure only, natively; terms are never data; the checker is C++ |
| LogosLang | ½ | 1 | 0 | 50 | the Logic Graph is walkable today and is the program; self-rewriting and the proof layer are unbuilt |
| Acorn | 0 | 0 | 0 | 0 | none |
| Salt | 0 | 0 | 0 | 0 | none |
| Telomare (Stand-In Language) | ½ | 0 | 0 | 17 | a closure's environment is a pair you can project; its code is an opaque `Defer`; every analysis is Haskell |
| Stellogen | 1 | ½ | 1 | 83 | code is a first-order term that shape checkers unify against; no eval back to code; types are library constellations |
| Nock / Hoon | 1 | 1 | ½ | 83 | formulas are nouns and opcode 2 is eval; vases let programs typecheck programs, but nothing checks Nock with it |
| Indie AI-first cluster | 0 | 0 | 0 | 0 | contracts + SMT on a host; no reflection anywhere in the genre |

## A2 Spec power

(a) specifications mention values (½ contracts or refinements over runtime values, 1 dependent types) · (b) propositions and proofs are first-class objects (½ obligations discharged by a solver, no proof objects) · (c) the type system is library code over a kernel, not built into the compiler

| Project | a | b | c | % | Why |
|---|:-:|:-:|:-:|--:|---|
| disp | 1 | ½ | 1 | 83 | Pi, telescopes, coproducts as library code over the kernel; proofs run (`Eq`, induction) but the library is small and there is no universe hierarchy yet |
| HVM4 / Bend2 / SupGen | 0 | 0 | 0 | 0 | untyped; Bend2's types and proofs are planned |
| Metamath Zero | 0 | 1 | 1 | 67 | a HOL subset, weaker than dependent types by design; explicit proof objects; the logic is a user-written spec the verifier is generic over |
| Verus | ½ | ½ | 0 | 33 | first-order contracts, quantifiers and ghost state over runtime values; Z3 discharges them, no proof objects |
| F* / Low* / Pulse | 1 | 1 | 0 | 67 | dependent types + refinements + effects + Pulse separation logic, built into the typechecker |
| Lean 4 | 1 | 1 | 0 | 67 | full dependent types, universes, mathlib; the type theory lives in the kernel and elaborator |
| Nova | 1 | 1 | 0 | 67 | extensional MLTT, kernel-defined |
| Blight | 1 | 1 | 0 | 67 | cubical + QTT + effects in the kernel; only the tower is library code |
| Soma | 1 | 0 | 0 | 33 | dependent types + QTT + rows; not a proof assistant |
| fiat-crypto + CryptOpt + Jasmin | 1 | 1 | 0 | 67 | full CIC in Rocq |
| Velvet / Loom / WybeCoder | 1 | 1 | 0 | 67 | Lean + mathlib behind a Dafny-style surface |
| Agda / Cubical | 1 | 1 | 0 | 67 | dependent types, HITs, univalence; kernel-defined |
| Narya | 1 | 1 | 0 | 67 | HoTT + internal parametricity + modalities; kernel-defined |
| Idris 2 / QTT | 1 | 1 | 0 | 67 | dependent types + QTT; kernel-defined |
| CakeML + Pancake | ½ | ½ | 0 | 33 | Viper separation-logic contracts for user code; HOL4 proofs are about the compiler, not the program |
| ATS3 / Xanadu | 1 | 1† | 0 | 67 | dependent + linear types with an explicit proof language (props and proof functions) |
| Vow | ½ | ½ | 0 | 33 | contracts + loop invariants, bounded model checking |
| Dafny | ½ | ½ | 0 | 33 | first-order + quantifiers, SMT-shaped, no proof objects |
| NanoLang | 0 | 0 | 0 | 0 | no user-level spec language; the proofs are about the language |
| Rust cluster | 1 | 1 | 0 | 67 | from Flux refinements to RefinedRust's Iris and Aeneas's Lean; scored on the strongest members |
| Mojo | ½ | 0 | 0 | 17 | types indexed by compile-time values only; no propositions, no proofs |
| LogosLang | 0 | 0 | 0 | 0 | the proof layer is a design document |
| Acorn | 0 | 1 | 0 | 33 | a theorem-proving language for mathematics; no program specifications |
| Salt | ½ | ½ | 0 | 33 | Z3 contracts: bounds, postconditions, quantifiers, invariants (claimed) |
| Telomare (Stand-In Language) | ½ | 0 | 0 | 17 | STLC-shaped statics; refinements are runtime validators, checked statically only when the failure is unconditional |
| Stellogen | ½ | 0 | 1 | 50 | types are user-space test galaxies over values; no dependent types, function types only for the linear fragment |
| Nock / Hoon | 0 | 0 | 0 | 0 | structural types with variance and wet genericity; nothing states a value property |
| Indie AI-first cluster | ½ | ½ | 0 | 33 | Z3 contracts throughout; Aver and Verity borrow real proofs from Lean |

## A3 Kernel / trust

(a) a trusted core small enough to audit · (b) it mints unforgeable evidence: theorems, proof objects, certificates (½ proof objects checked only by a large checker) · (c) the clever layers are untrusted and re-checked by the core, and an independent checker exists (½ re-checked by the one core only)

| Project | a | b | c | % | Why |
|---|:-:|:-:|:-:|--:|---|
| disp | 1 | 1 | ½ | 83 | a two-op core mints hypotheses; elaborator and library untrusted; five evaluators agree but nothing independent re-checks the kernel's verdicts |
| HVM4 / Bend2 / SupGen | 0 | 0 | 0 | 0 | trust is `hvm.c` |
| Metamath Zero | 1 | 1 | 1 | 100 | a C verifier small enough to formalize; MMB proof objects; MM1 untrusted; independent verifiers in Rust and Kotlin |
| Verus | 0 | 0 | 0 | 0 | TCB = Verus + Z3 + rustc |
| F* / Low* / Pulse | 0 | ½ | 0 | 17 | typechecker, Z3 and KaRaMeL are all trusted; proof terms exist, SMT verdicts have none |
| Lean 4 | ½ | 1 | 1 | 83 | a small kernel, larger than MM0's; proof terms; elaborator and tactics untrusted; lean4lean and lean4export re-check |
| Nova | 1 | 1 | ? | 67? | a small kernel with its own spec re-checks certificate-carrying artifacts; no independent checker is mentioned |
| Blight | 1 | 1 | 1 | 100 | a microscopic kernel is the only thing that can mint a `Proof`; the tower is untrusted; a second independently written re-checker |
| Soma | 0 | 0 | 0 | 0 | trust the compiler |
| fiat-crypto + CryptOpt + Jasmin | ½ | 1 | 1† | 83 | Rocq's kernel; CryptOpt's equivalence checker is verified in Rocq; coqchk re-checks |
| Velvet / Loom / WybeCoder | ½ | 1 | 1 | 83 | Lean's kernel; Loom verifiers are foundational, no trusted VC generator |
| Agda / Cubical | 0 | ½ | 0 | 17 | the large typechecker is the TCB; proof terms exist, nothing smaller re-checks them |
| Narya | 0 | ½ | 0 | 17 | tens of thousands of OCaml lines are the trust base; NbE unproven |
| Idris 2 / QTT | 0 | ½ | 0 | 17 | the typechecker is the TCB |
| CakeML + Pancake | 1 | 1 | 1† | 100 | HOL4's LCF kernel; theorems are the evidence; independent HOL checkers exist (OpenTheory, Candle) |
| ATS3 / Xanadu | 0 | ½† | 0 | 17 | the typechecker is the TCB; proof terms exist |
| Vow | 0 | 0 | 0 | 0 | TCB = vowc + ESBMC |
| Dafny | 0 | 0 | 0 | 0 | TCB = Dafny + Boogie + Z3 |
| NanoLang | ½ | 0 | 0 | 17 | no kernel, but the core semantics are proved sound in Coq, `Admitted`-free |
| Rust cluster | ½ | ½ | ½ | 50 | Aeneas/hax/RefinedRust inherit Lean/F*/Rocq kernels; Kani/Creusot/Flux trust SMT |
| Mojo | 0 | 0 | 0 | 0 | the whole MLIR/LLVM C++ stack |
| LogosLang | 0 | 0 | 0 | 0 | no kernel discipline described |
| Acorn | 0 | ? | 0 | 0? | has its own checker; the write-up does not characterize its TCB or proof objects |
| Salt | 0 | 0 | 0 | 0 | TCB = saltc + Z3 + MLIR (claimed) |
| Telomare (Stand-In Language) | 0 | 0 | 0 | 0 | the whole compiler is trusted; `--certificate` is a report nothing re-checks |
| Stellogen | 1 | 0 | ½ | 50 | ~3,200 lines with a normative spec and two trusted observations; checkers are user space; trust rests on paper theorems, no evidence objects |
| Nock / Hoon | 1 | 0 | ½ | 50 | a page of spec, a conformance suite, independent interpreters; jets are an unbounded trusted surface with no evidence and no production differential check |
| Indie AI-first cluster | 0 | 0 | 0 | 0 | contracts + SMT; Verity (Lean, zero axioms) is the exception |

## A4 Equality

(a) a behavioral equality beyond syntactic identity (½ a decidable fragment or a fixed proved relation) · (b) it is mechanically checkable and composes (½ tactic- or human-driven) · (c) it licenses rewrites: something replaces programs by equivalent ones on its authority (½ one slice or a fixed set of passes)

| Project | a | b | c | % | Why |
|---|:-:|:-:|:-:|--:|---|
| disp | ½ | ½ | ½ | 50 | witness-licensed rewrites landed as one slice (map fusion); whether a decidable fragment is rich enough is open (Q1) |
| HVM4 / Bend2 / SupGen | 0 | 0 | 0 | 0 | none |
| Metamath Zero | 0 | 0 | 0 | 0 | a proof format, not an optimizing calculus |
| Verus | ½ | ½ | 0 | 33 | SMT-fragment equalities in proofs; nothing rewrites programs on their authority |
| F* / Low* / Pulse | ½ | ½ | 0 | 33 | SMT-decided equalities within the fragment |
| Lean 4 | ½ | ½ | 0 | 33 | propositional equality plus Quot and axioms; rewriting is tactic-driven |
| Nova | 1 | 1 | 0 | 67 | extensional: anything provably equal is treated as equal; certificates recover decidability; no optimizer consumes it |
| Blight | 1 | 1 | 0 | 67 | cubical univalence and HITs compute; no rewrite consumer |
| Soma | 0 | 0 | 0 | 0 | not addressed |
| fiat-crypto + CryptOpt + Jasmin | ½ | 1 | ½ | 67 | a verified, decidable equivalence checker for straight-line assembly licenses CryptOpt's output; a tiny fragment |
| Velvet / Loom / WybeCoder | ½ | ½ | 0 | 33 | Lean's equality plus SMT in its fragment |
| Agda / Cubical | 1 | 1 | 0 | 67 | cubical: computational univalence; no rewrite consumer |
| Narya | 1 | ½ | 0 | 50 | observational Id per type former; transport computes on only some formers so far |
| Idris 2 / QTT | ½ | 0 | 0 | 17 | intensional propositional equality, nothing more |
| CakeML + Pancake | ½ | 0 | ½ | 33 | per-pass semantics preservation proved by humans licenses the compiler's fixed passes |
| ATS3 / Xanadu | 0 | 0 | 0 | 0 | not addressed |
| Vow | 0 | 0 | 0 | 0 | not addressed |
| Dafny | ½ | ½ | 0 | 33 | SMT fragment only |
| NanoLang | 0 | 0 | 0 | 0 | not addressed |
| Rust cluster | ½ | ½ | 0 | 33 | Aeneas's functional translation of borrows is a machine-produced equivalence, per tool and human-directed |
| Mojo | 0 | 0 | 0 | 0 | MLIR rewrites are unverified compiler transforms |
| LogosLang | 0 | 0 | 0 | 0 | the rewriting engine is unspecified |
| Acorn | 0 | 0 | 0 | 0 | not addressed for programs |
| Salt | 0 | 0 | 0 | 0 | not addressed |
| Telomare (Stand-In Language) | 0 | 0 | 0 | 0 | structural only |
| Stellogen | 0 | 0 | 0 | 0 | `==` is syntactic by specification |
| Nock / Hoon | 0 | 0 | 0 | 0 | opcode 5 is structural; jets are asserted equivalences keyed on intensional identity |
| Indie AI-first cluster | 0 | 0 | 0 | 0 | none |
| Adjacent substrates | 1 | 1 | 1 | 100 | egg: e-graphs hold classes of equivalent programs, saturation emits certificates a Lean kernel replays, extraction picks the cheapest member |

## A5 Performance + cost model

(a) C/Rust-class native execution (½ compiled but not systems class) · (b) a primitive returns cost with results (½ a meter or report outside the language) · (c) cost is a typing-level resource (½ a usage grade, erasure, or a static bound that is not a type)

| Project | a | b | c | % | Why |
|---|:-:|:-:|:-:|--:|---|
| disp | 0 | ½ | 0 | 17 | interpreted tree-walkers; `--stats` steps are a deterministic meter outside the language; cost as a grade is designed |
| HVM4 / Bend2 / SupGen | 1 | ½† | 0 | 50 | native C with GPU lineage; the runtime reports interaction counts; no cost in types |
| Metamath Zero | ½ | 0 | 0 | 17 | MMC compiles to verified x86 but MM0 is not a general systems language; the hardware model is a spec, not a cost model |
| Verus | 1 | 0 | 0 | 33 | it is Rust; no cost primitive, no cost in types |
| F* / Low* / Pulse | 1 | 0 | 0 | 33 | Low*→C competitive with hand-optimized C; no cost model |
| Lean 4 | ½ | 0 | 0 | 17 | via C, reference-counted; fine for tooling, not systems class |
| Nova | 0 | 0 | 0 | 0 | a research-scale Idris 2 program |
| Blight | 0 | 0 | 0 | 0 | no native backend, no cost model |
| Soma | 1 | 0 | ½ | 50 | LLVM native, GC-free; QTT quantities are a usage grade the compiler spends, not cost |
| fiat-crypto + CryptOpt + Jasmin | 1 | 1 | 0 | 67 | beats GCC/Clang; on-CPU benchmarking is the fitness function |
| Velvet / Loom / WybeCoder | 0 | 0 | 0 | 0 | programs are extracted for testing only |
| Agda / Cubical | 0 | 0 | 0 | 0 | research-grade GHC/JS backends |
| Narya | 0 | 0 | 0 | 0 | correctness-first, no performance story |
| Idris 2 / QTT | ½ | 0 | ½ | 33 | Chez backend; erasure by quantity makes specs free at runtime |
| CakeML + Pancake | 1 | 0 | 0 | 33 | verified native code in production-ish settings |
| ATS3 / Xanadu | 1 | 0 | 0 | 33 | to C, no GC, no runtime |
| Vow | 1 | 0 | 0 | 33 | Cranelift native, linear types |
| Dafny | 0 | 0 | 0 | 0 | managed backends |
| NanoLang | 1 | 0 | 0 | 33 | transpiles to C |
| Rust cluster | 1 | 0 | 0 | 33 | it is Rust |
| Mojo | 1 | 0 | 0 | 33 | MLIR codegen for CPU/GPU/accelerators; benchmark sweeps are offline tooling |
| LogosLang | 1 | 0 | 0 | 33 | Cranelift JIT within 3× of vectorized C, measured |
| Acorn | 0 | 0 | 0 | 0 | not a systems language |
| Salt | ½ | 0 | 0 | 17 | MLIR→LLVM with claimed `-O3` parity, unverified |
| Telomare (Stand-In Language) | 0 | ½ | ½ | 33 | a Haskell tree-walker; `--meter` counts steps and nodes; `--certificate` is a static, inferred, input-universal iteration bound |
| Stellogen | 0 | 0 | 0 | 0 | execution has "a horrible complexity" |
| Nock / Hoon | ½ | ½ | 0 | 33 | native C and Rust interpreters with jets; `%bout` prints timings; cost as a value forbidden by design |
| Indie AI-first cluster | ½ | 0 | 0 | 17 | only LSTS compiles to C and CLR rides on Zig; the genre runs on hosts |

## A6 Search / self-application

(a) spec → implementation automatically (½ an external LLM loop, harness generation, basic proof search) · (b) scored by the checker and by cost (½ one of the two) · (c) the search is aimed at itself (½ a flywheel started)

| Project | a | b | c | % | Why |
|---|:-:|:-:|:-:|--:|---|
| disp | 0 | 0 | 0 | 0 | the optimizer is designed (`research/OPTIMIZER.typ`), not built; no search exists yet |
| HVM4 / Bend2 / SupGen | 1 | ½ | 0 | 50 | SupGen enumerates superposed candidates until tests pass; tests rather than a checker, no cost objective |
| Metamath Zero | 0 | 0 | 0 | 0 | MM1 tactics are human-written |
| Verus | 1 | 1 | 0 | 67 | IDS synthesizes code and proofs with benchmarks inside the loop (7/7 KV-store specs); the proposer is an external LLM |
| F* / Low* / Pulse | 0 | 0 | 0 | 0 | humans write the code and the proofs |
| Lean 4 | ½ | ½ | 0 | 33 | the richest external ecosystem of proof-search agents; nothing built in; no cost |
| Nova | 0 | 0 | 0 | 0 | none |
| Blight | 0 | 0 | 0 | 0 | tactics are human-written |
| Soma | 0 | 0 | 0 | 0 | none |
| fiat-crypto + CryptOpt + Jasmin | 1 | 1 | 0 | 67 | randomized search over assembly with measured cost and a verified equivalence check; straight-line only; unmaintained since 2024 |
| Velvet / Loom / WybeCoder | 1 | ½ | 0 | 50 | WybeCoder's agentic loop proves 74% of Verina; correctness only, no cost |
| Agda / Cubical | 0 | 0 | 0 | 0 | `auto` is trivial |
| Narya | 0 | 0 | 0 | 0 | no tactics even |
| Idris 2 / QTT | 0 | 0 | 0 | 0 | `auto` is basic |
| CakeML + Pancake | 0 | 0 | 0 | 0 | human-written |
| ATS3 / Xanadu | 0 | 0 | 0 | 0 | none |
| Vow | ½ | ½ | 0 | 33 | CEGIS with an external LLM; no cost objective |
| Dafny | ½ | ½ | 0 | 33 | LLMs write it best (82–96% on benchmarks) but the search lives in external tools; correctness only |
| NanoLang | 0 | ½ | 0 | 17 | designed as an LLM target with mandatory tests; no engine of its own |
| Rust cluster | ½ | ½ | 0 | 33 | Kani's autoharness; otherwise human-driven with AI assistance |
| Mojo | ½ | ½ | 0 | 33 | offline parameter sweeps, cost only, correctness assumed |
| LogosLang | 0 | 0 | 0 | 0 | none |
| Acorn | ½ | ½ | ½ | 50 | an embedded local model fills in proofs with the checker in the loop; the library becomes the next model's training data |
| Salt | 0 | 0 | 0 | 0 | none |
| Telomare (Stand-In Language) | 0 | 0 | 0 | 0 | superposition is aimed at bounds, not candidates |
| Stellogen | 0 | 0 | 0 | 0 | none intended |
| Nock / Hoon | 0 | 0 | 0 | 0 | `honk`'s parity policy is a differential oracle, not a search |
| Indie AI-first cluster | ½ | ½ | 0 | 33 | agent loops around contracts; Prove's refutation challenges |
