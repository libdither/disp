# Foundations

*disp's pieces, the past attempts on each, why those attempts stopped where they did, and the empirical questions that will make or break disp.*

> **Thesis.** disp is a novel *synthesis*, not a novel *mechanism*. Every piece below has a sharp precedent, and almost every precedent has a documented reason it stalled — a "graveyard reason." disp is a bet that *combining* the pieces defuses each individual graveyard reason rather than inheriting it. This document lays out the pieces bottom-up, names the precedent and the graveyard reason for each, and ends (§V) with what disp is actually attempting, why that specific combination has stayed unoccupied, and the falsifiable questions that decide whether the bet pays.

> **Status.** The substrate (Parts I–III) is largely built; the endgame (Part IV) is not (`OPTIMIZER.typ`: "none of this is built"). The novelty *and* the risk both live in Part IV. Keeping those two halves distinct is the single most important discipline when judging the project — the foundation's innovation is settled and modest; the endgame's is everything and unproven.

## How to read this

Each piece is one short entry with four labelled parts:

- **In disp** — how disp uses it and where it lives (file references).
- **Lineage** — the sharp precedents, with names and dates.
- **Why it didn't go farther** — the graveyard reason, the heart of this document.
- **disp's bet** — what disp inherits unchanged vs. what it changes, and what that buys.

The layering mirrors `GOALS.md`'s bootstrap order: a reflective calculus → a type system that is a program in it → an evaluator → an optimizer that turns the whole thing on itself.

Names in `monospace` — `tree_eq`, `lib/kernel/…`, `OPTIMIZER.typ` — are signposts into the disp codebase and docs for contributors. A general reader can ignore them; the prose is written to stand on its own.

### Four recurring reasons a good idea stalls

Most of the graveyard reasons below are instances of four general principles. They are referenced inline as **[P1]**–**[P4]**.

- **[P1] Generality is the enemy of predictability.** A system that can express anything has no legible cost model and no local reasoning. Survivors of every declarative paradigm won by *restricting* to a decidable, bounded-cost fragment (Datalog from Prolog, flexbox from constraint solvers, SMT from general search). The moment a design exposes unbounded search or global constraint satisfaction it inherits opaque performance, under/over-determination, and non-local failure.
- **[P2] The declarative reading must equal what runs.** When the operational reality diverges from the logical reading — Prolog's `cut` and clause-ordering, refinement types that are unsound under lazy evaluation — the abstraction leaks and users flee to something they can predict.
- **[P3] The Löbian ceiling.** By Gödel's second theorem no consistent system certifies its own consistency. Self-hosting and reflective self-checking buy assurance *relative to an external anchor* and are typically *slow*; they do not buy free trust or free speed.
- **[P4] Intensional is too fine for reasoning; extensional is expensive to compute.** *Intensional* equality compares programs by their structure (their text); *extensional* equality compares them by their behavior (same output on every input). Structural equality is cheap and ideal for reflection, hashing, and O(1) conversion — but it calls two programs different whenever they are written differently, even when they always compute the same thing. That is exactly the equality an optimizer must see through, since its whole job is to replace a program with a differently-written but behaviorally-identical faster one. Much of modern type theory is the cost of climbing back from cheap intensional equality to useful extensional equality.

---

# Part I — The substrate (built)

### 1. An intensional, reflective calculus with content-addressed conversion

Programs are binary trees; the type checker, elaborator, and (eventually) optimizer are themselves trees; conversion is O(1) identity.

- **In disp.** Barry Jay's tree calculus is the substrate (`src/tree.ts`, `KERNEL_DESIGN.md`). Programs are data — a prerequisite stated in `GOALS.md` for writing type checkers in the language itself. Conversion is `tree_eq`, a hash-consed O(1) structural-identity check (the one live native fast-path); deterministic elaboration guarantees *same type → same tree*, so definitional equality is pointer equality.
- **Lineage.** SF-calculus (Jay & Given-Wilson, 2011) and *Reflective Programs in Tree Calculus* (Jay, 2021, with Coq proofs): three rewrite rules giving extensional *and* intensional computation — a self-evaluator, self-equality, and self-sizing **without Gödel numbering or quotation**. Content-addressing-as-identity is Unison's idea (Chiusano & Bjarnason); hash-consing is older still (Ershov; Goto).
- **Why it didn't go farther.** Tree calculus is essentially a one-researcher program: no ecosystem, no production use, unstudied at scale. Its intensionality — the ability to pattern-match on any program — deliberately **breaks extensionality and parametricity**, which is simultaneously its selling point and a soundness hazard **[P4]**. Unison's content-addressing, meanwhile, buys distribution and caching, not *semantic* equality: two behaviorally-equal definitions still hash differently.
- **disp's bet.** disp is the most serious attempt to date to build a real dependently-typed language on Jay's calculus. The wager is that intensionality + O(1) conversion *materially* help the checker and optimizer (cheap conversion, programs that inspect programs) rather than merely being elegant. That is inherited novelty, not invented — and it reopens the equality problem (§7).

### 2. Types as predicates over an untyped universe

A type is a *predicate* — a function from raw trees to yes/no — and type checking is just running that predicate on a value.

- **In disp.** "Types are predicates; the type checker is a tree program" (`CLAUDE.md`, `TYPE_THEORY.typ`). A type is a wait-form `wait(checker)(metadata)`; recognition is signature comparison; checking is raw application. There is no separate typed term language — typed and untyped trees inhabit one universe.
- **Lineage.** This is precisely the **NuPRL / Computational Type Theory** tradition (Constable et al., 1986; Allen's PER semantics) and Martin-Löf's *meaning explanations*: a type is a relation over plain untyped values that says which values belong to it and which of them count as equal — there is no separate kingdom of "typed terms" sitting above the untyped ones. The decidable cousin is predicate subtyping (PVS — Owre, Rushby, Shankar, 1998), refinement types (Freeman & Pfenning, 1991 → Liquid Haskell), and set-theoretic/semantic subtyping (CDuce; Castagna).
- **Why it didn't go farther.** In the NuPRL line, membership and conversion are **undecidable in general** — type checking degrades to proof search **[P1]** — which is why NuPRL stayed a specialist proof environment and the broader field moved to *intrinsic*, bidirectional typing where checking is structural and decidable. The refinement pole stays decidable only by *retreating* to an SMT-shaped fragment, and even there encodings can be unsound under lazy evaluation **[P2]**.
- **disp's bet.** disp sits squarely in the NuPRL/meaning-explanation tradition but on an *intensional* substrate (NuPRL was extensional and λ-based). It buys decidability back not by restricting the logic but by making conversion O(1) and keeping evaluation total and eager — pushing the cost onto the *substrate's* determinism rather than the *type theory's* expressiveness.

### 3. The metacircular discipline: the object language is the specification

The *real* definition of every operation is a program written in disp itself; the fast versions written in the host language (TypeScript/Rust) are only accelerators that must produce identical results.

- **In disp.** "Metacircular" here means the type checker, elaborator, and conversion check are not hard-wired into the host runtime — they are ordinary disp programs (trees). The host runtime carries fast re-implementations of a few of them (`tree_eq` in `src/tree.ts`, `compileExpr` in `src/compile.ts`), but each is treated as an *optimization of the in-language version* and kept honest by tests demanding bit-for-bit identical output (bracket abstraction, for instance, is checked against 300 random terms). The disp program is the reference; the host code is a cache for it. (`CLAUDE.md`: "The object language is the specification. Host implementations are optimizations.")
- **Lineage.** The Lisp metacircular evaluator — Lisp written in Lisp (McCarthy, 1960). The **Futamura projections** (1971) and partial evaluation (Jones, Gomard & Sestoft, 1993): given an interpreter and a tool that specializes a program to fixed inputs, specializing the interpreter *to a program* is the same as compiling that program, and specializing the specializer *to itself* yields a compiler-generator — the general lesson being that a program which operates on programs can be turned on itself. GraalVM/Truffle ships this in production. Self-hosting *checkers*: MetaCoq / "Coq Coq Correct!" (Sozeau et al., JACM 2025), Lean4Lean (Carneiro, 2024), Andromeda.
- **Why it didn't go farther.** Writing a checker in its own language is well-trodden, but it has historically bought *assurance*, not *speed* or *self-improvement*. It hits the **Löbian ceiling [P3]**: MetaCoq verifies Coq's checker only *relative* to an outside trust anchor and still cannot check the full standard library, and reflective checkers tend to be slow. So people do it for confidence, then stop.
- **disp's bet.** Make the discipline pay off in two ways the prior work left on the table. **(1) Trust shrinks.** Because the meaning of each operation lives in a small, readable disp program, that program is all you must trust — not the fast-but-hairy host code, which is verified against it. As more of the system is expressed in-language with host code as a mere accelerator, the amount you take on faith narrows toward the tiny kernel. **(2) "Optimize yourself" becomes a literal instruction, not a category error.** In a normal toolchain the optimizer is written in C++/Rust — a *different* language from the programs it optimizes — so it can never be aimed at its own internals. In disp the checker, elaborator, and optimizer are themselves disp programs, so the optimizer (Part IV) can optimize *them* with the very machinery it uses on any user program. Self-application is then just the Futamura idea above, made concrete. This is the structural decision the entire endgame stands on — and most verification projects never make it.

### 4. The kernel as a generative-sealing nucleus

A tiny trusted center mints unforgeable evidence; everything else is untrusted library code that can only consume it.

- **In disp.** The kernel surface is two Σ-ops (`hyp_reduce`, `bind_hyp`) plus the `param_apply` dispatcher over a fixed Σ (`lib/kernel/engine.disp`). `seal(Σ) = forge(Σ)` are the trusted-token producers; the walker polices reflection so user code cannot forge or inspect a hypothesis (`SEALING.md`, `soundness.test.disp`). `bind_hyp` is generative — it mints a fresh hyp and escape-checks the body.
- **Lineage.** The **LCF architecture** (Milner, 1972 → HOL, Isabelle): `theorem` is an abstract type only the kernel's inference rules can construct, so an arbitrarily clever untrusted tactic can never produce a false theorem. Generative/dynamic **sealing** (Morris; Sumii & Pierce) and nominal techniques; **DCC** noninterference (Abadi et al.) for the escape discipline.
- **Why it didn't go farther.** LCF kernels are a success story — but in a *non-reflective* setting. Sealing in a calculus where programs can *inspect* other programs is in genuine tension: the thing that makes reflection powerful (read any tree) is the thing sealing must forbid (don't read the sealed token). disp's own open problem K3 (walker sealing) is exactly this tension **[P4]** **[P3]**.
- **disp's bet.** Reuse the LCF discipline — clever-and-untrusted around tiny-and-trusted — but on a reflective substrate, with the walker as the soundness bridge between bare λ-application and sealed evidence (`SEALING.md` frames `bind_hyp` as generative sealing, the escape check as DCC noninterference, the conjecture "sealing preserves parametricity" via a step-indexed logical relation).

### 5. One negative former (telescopes), not four

Π, Σ, records, and ⊤ are a single n-ary negative former that differ only in a per-cell observation.

- **In disp.** `Telescope` is the one negative former; `Pi`/`Sigma`/`Record`/`⊤` are instances differing only in the cell op (`mint`/`apply`/`proj`/`deriv`); a single mode-polymorphic walker `at` serves both recognition and response (`NEGATIVE_TYPES.md`, `types.disp`). Recursion and corecursion are *also* cells on the same walker (`TELESCOPE_FIXPOINT.md`); `Mu`/`Nu` wrappers were retired.
- **Lineage.** Telescopes (de Bruijn, 1991); records-as-Σ; normalization-by-evaluation (Berger & Schwichtenberg, 1991; Abel) and the "observation interface"/final-encoding view of negative types; the unification of records and dependent products is folklore in type theory.
- **Why it didn't go farther.** This is the one piece with no real graveyard: it is an *elegance/uniformity* move, not a place where projects die. The honest assessment is that nobody else *needed* one former badly enough to pay the cost — a generic walker costs ~2× the specialized recognizers (`APPLY_BUDGET` is 40M in `src/compile.ts`). It is low-risk and not where disp's fate is decided.
- **disp's bet.** Uniformity compounds: a new observation mode (a new optic, a new recursion scheme) plugs in as a new cell op with no walker edit. The cost is a constant-factor tax a validated lean fast-path could reclaim — see make-or-break Q6.

---

# Part II — The reasoning layer (partly built, partly being imported)

### 6. Dependent types

The general value proposition disp inherits — and the 40-year wall on it.

- **In disp.** A dependently-typed system (or stronger) is a stated requirement (`GOALS.md`). Π/Σ/Eq/Refinement/Intersection are ordinary library types; the spec for the optimizer is *a dependent type* (`OPTIMIZER.typ` §2).
- **Lineage.** Martin-Löf type theory; Coq, Agda (Norell, 2007), Idris (Brady), Lean (de Moura), F* (Swamy et al.).
- **Why it didn't go farther — i.e. why dependent types are not mainstream after 40 years.** Type inference for the λΠ-calculus is **undecidable** (typability is not recursive; arXiv 2306.07599), forcing annotations and interactive elaboration. On top sit the proof burden, totality/termination obligations, elaboration cost, and the "proving-vs-shipping" gap **[P1]**. The field's verdict is a *niche* one: dependent types win in **bounded, high-assurance** domains — CompCert (Leroy), seL4 (Klein et al., Isabelle/HOL), F\*/HACL\*/EverCrypt (deployed in Firefox, the Linux kernel, WireGuard, mbedTLS), Lean/mathlib for mathematics. Liquid Haskell wins precisely by *retreating* from full dependency to SMT-decidable refinements.
- **disp's bet.** The proven-valuable version of disp's exact pitch — *dependent spec → verified optimized low-level code* — already ships in F\*/HACL\*/EverCrypt. disp's delta is **automation**: F\* still requires a human to write the code and the proofs; disp wants the optimizer (§12–15) to *synthesize* both. That is a real, identifiable delta — and an unproven one.

### 7. The equality problem — the crux

What does it mean for two different trees to be the same program? Everything load-bearing lives here.

- **In disp.** Conversion is intensional `tree_eq` (O(1), syntactic). But verification and optimization need *behavioral* equivalence — the optimizer's whole job is to replace a program with a *different but equivalent* faster one. disp's licensing relation is `~_T` — a deliberately coarse, type-indexed notion of "these two programs behave the same" (a *logical relation*, in the jargon) that the type-walker already computes; cost-improving rewrites must additionally be *no slower*, via **Sands' improvement** ordering (`OPTIMIZER.typ` §6–7). The imported machinery — Cedille's φ, OTT funext, parametricity — is *cited* (`TYPE_THEORY.typ`), mostly not yet integrated.
- **Lineage.** Function extensionality is **unprovable** in intensional MLTT, and intensional equality is *too fine*. Three responses: **Cedille/CDLE** (Stump, 2018–2020) — the φ/"Kleene trick" gives λ-encodings *with* induction and definitionally zero-cost coercions; **Observational Type Theory** (Altenkirch & McBride, 2007) — matured into "Observational Equality: Now For Good" (Pujet & Tabareau, POPL 2022: decidable conversion, canonicity, normalization, machine-checked; CIC extension, TOPLAS 2025); **cubical type theory** (CCHM, 2016; Cubical Agda, 2021) — computational univalence + HITs, but heavyweight (interval/Kan operations carry real performance cost). Representation independence comes from **parametricity** (Reynolds, 1983; Wadler's "Theorems for free") *without* univalence.
- **Why it didn't go farther.** This is an *active, unconverged frontier* **[P4]**: cubical is powerful but expensive; OTT only recently became fully computational; φ is elegant but Cedille has been largely quiescent since ~2021. The field is still *paying to climb back* from intensional to extensional equality.
- **disp's bet — and the danger.** disp's intensional substrate *reopens* the exact wound the field is healing: `tree_eq` is the finest possible equality. disp currently **cites the solutions to a problem its substrate reintroduced**. The entire optimizer is a bridge across the gap between `tree_eq` (cheap, what the kernel knows) and `~_T` (undecidable in general, what licensing rewrites needs). This is **the** make-or-break question (Q1), not one frontier among several: the property that makes disp fast is the property that makes disp hard, and they are the same decision.

### 8. Effects as values, with one impure driver at the boundary

The substrate is pure; effects are a library construction, not a primitive.

- **In disp.** An effect is represented as ordinary data that *describes* an action to take (a "free monad"), which a separate interpreter (a "handler") carries out later; only a single impure *driver* at the program's boundary actually touches the outside world (`TYPE_THEORY.typ` §15). Because the substrate is pure, effects can only ever be inert values — never something the kernel dispatches on. (`postulate`, an older escape hatch, was removed.)
- **Lineage.** Algebraic effects (Plotkin & Power, 2002) and handlers (Plotkin & Pretnar, 2009): Eff, Koka (Leijen), Frank, and OCaml 5's effect handlers (2022). Free monad + interpreter is the functional-programming staple.
- **Why it didn't go farther.** Algebraic effects are young in practice: typing handlers well, their performance, and especially their *interaction with dependent types* (effect-dependent typing, effects under a proof obligation) are open research **[P1]**. Most systems keep effects untyped-ish or limited to unary effect rows.
- **disp's bet.** Make effects fall out of purity rather than be bolted on: because the substrate cannot dispatch on an effect, "run = handle = simulate" is one shape, and the single driver is the `GOALS.md` measurement primitive (run the external language, return outputs *and* the cost incurred). This is the cleanest connection between the type theory and the optimizer's hardware-cost story (`project_lowlevel_effects_cost_framing`).

### 9. Cost as a graded coeffect

Performance is not a side computation; it is a typing-level resource accounted in a graded ledger.

- **In disp.** Cost is one axis of a graded-coeffect/effect ledger shared with usage, sharing, and staging; the cost *bound* is a coeffect (what a term demands), the cost *incurred* is an effect (what a run spends), and a cost-soundness theorem links them — which *is* the guidance/soundness split (`OPTIMIZER.typ` §4; `project_lowlevel_effects_cost_framing`).
- **Lineage.** Coeffects (Petricek, Orchard & Mycroft, 2014) and Granule (Orchard et al.); Quantitative Type Theory (Atkey, 2018; McBride, 2016) shipping in Idris 2 (Brady, 2021); graded modal type theory.
- **Why it didn't go farther.** Graded systems are research-stage. Combining *several* graded modalities — usage × cost × staging × sharing — into one ledger with usable ergonomics *and* tractable metatheory is unsolved; most implementations expose one grade (linearity/usage) and stop **[P1]**.
- **disp's bet.** Treat cost, staging, usage, and sharing as axes of one algebra — a *semiring* (`OPTIMIZER.typ` §4, §7) — so that memoization, partial evaluation, JIT, and AOT are the *same* graded rewrite at different points of "how much input is fixed." If the unification holds, the optimizer's search space and its cost model are the same object.

---

# Part III — The execution substrate (partly built)

### 10. Evaluator plurality and the differential oracle

Multiple independent reducers that must agree byte-for-byte are a standing correctness oracle.

- **In disp.** Five backends behind one `Session` ABI — disp-eager, naive, rust-eager, rust-ic-net, and the lambada peers (`EVALUATOR.md`). The lambada peers are an *external* differential oracle: 11 evaluators agree disp⟷lambada, ternary byte-identical (`project_evaluator_session_abi`). Reproducibility is guarded by conformance differentials, not by trusting any one backend.
- **Lineage.** Differential testing (McKeeman, 1998); Csmith for C compilers (Yang et al., 2011) — finding bugs by cross-checking independent implementations on random inputs.
- **Why it didn't go farther.** This one didn't stall — it is *underused*. Most language implementations have a single blessed evaluator and a test suite, not a population of independent reducers held to bit-identity. The discipline is cheap insurance that few projects pay for.
- **disp's bet.** With the object language as spec (§3), each backend is a fast path validated against the others; this is the safety net that lets disp swap in an exotic reducer (§11) without trusting it. A lazy or parallel backend is sound iff it agrees with the eager reference on conversion — a property the harness checks rather than assumes (`project_eager_normative_is_scaffolding`).

### 11. Interaction nets and optimal reduction

The parallel, no-shared-memo substrate the optimizer is designed to run on — and the deepest-studied trap in this whole document.

- **In disp.** `rust-ic-net` is a *materialized* interaction-net reducer that **drops hash-consing** — agents and ports are real arena nodes, active pairs sit in a schedulable bag (`OPTIMIZER.typ` §3; `tc-net.typ`; `RUST_IC_NET_DESIGN.md`). Removing automatic memo is the design's *foundation*, not a regression: it keeps cost attributable per candidate and reduction provenance-preserving (the reverse-mode optimizer of §13 must blame/credit each candidate without entangling shared sub-results).
- **Lineage.** Interaction nets (Lafont, 1990); Lévy's optimal reduction (1978); Lamping's algorithm (1990); Gonthier–Abadi–Lévy; Asperti & Guerrini, *The Optimal Implementation of Functional Programming Languages* (1998). Recent practical incarnation: HVM/HVM2 (Taelin / Higher Order Co).
- **Why it didn't go farther.** The classic trap: "optimal" ≠ "fast." Lamping-style optimal reduction needs extra *bookkeeping* nodes (called fans, croissants, and brackets) to track who shares what, and Asperti–Mairson proved that bookkeeping's cost is **not bounded by any elementary function** — sharing does not come free **[P1]**. HVM regains practical parallel speed only by *restricting* (roughly: limiting how often a value may be duplicated and skipping the full bookkeeping), and then *loses* sharing wherever duplication is unrestricted. disp has *measured* its own version of this: dropping hash-consing inflates the work done by 4,000–67,000× (a 600× raw-speed loss), and the hoped-for property that type-checking distributes over duplication holds **only for "affine" code (values used at most once) — and is provably false for any check that inspects its input more than once** (`project_rust_ic_net_perf_decomposition`).
- **disp's bet.** The no-memo "loss" is the price of honest, attributable, provenance-preserving cost — *the* prerequisite for credit assignment in the optimizer. The open research is whether **label-coordinated duplication** can recover enough sharing to make superposition-based search affordable without re-entangling provenance. This is make-or-break Q2, and unlike most of Part IV it is already *half-answered* (and the answer so far is "not yet").

---

# Part IV — The endgame (unbuilt: where the novelty and the risk both live)

### 12. Verified optimization: untrusted optimizer + trusted re-checker

An arbitrarily clever optimizer whose every rewrite is re-validated by a tiny trusted checker.

- **In disp.** "The optimizer is arbitrarily clever and *untrusted*; a bad rewrite either fails certificate-checking or fails an independent re-type-check." The trusted base is the reduction relation, a ~30-line in-language checker, the rule library's soundness, one congruence theorem, and the cost metric (`OPTIMIZER.typ` §1, §6).
- **Lineage.** Proof-carrying code (Necula & Lee, 1996); translation validation (Pnueli, Siegel & Singerman, 1998); Alive/Alive2 (SMT-rechecking LLVM peepholes); **equality saturation** (Tate, Stepp, Tatlock & Lerner, 2009) / egg (Willsey et al., 2021), now emitting certificates a Lean kernel replays (ROVER; "Small Proofs from Congruence Closure"); CompCert and CakeML (verified, even bootstrapped, compilers).
- **Why it didn't go farther.** The pattern *works* — but per-domain, with hand-built rule sets, and it **validates** a rewrite rather than **synthesizing** one from a specification. Nobody has scaled "untrusted optimizer + trusted checker" into a general *synthesizer*.
- **disp's bet.** disp's checker architecture is textbook-correct; its differentiators are (a) the spec is a *dependent type* (richer than a reference implementation or a test suite) and (b) conversion is O(1) hash-cons identity in a metacircular kernel, so re-checking is cheap. The unproven leap is from *validating* rewrites to *synthesizing* programs.

### 13. Superoptimization with a cost objective

Search the space of programs for the cheapest one meeting the spec.

- **In disp.** Filling in the holes of a partial program is run *backwards*, much like backpropagation in a neural network: information flows forward to predict the result, then credit and blame for the final cost flow back to the choices that produced it, so the search is steered instead of random. Many candidate programs are explored at once, sharing their common work (*superposition*). (For the type-theory reader: this is reverse-mode evaluation over the net, with the duplicator playing the role of linear logic's exponential and the backward pass its derivative — `OPTIMIZER.typ` §8.) Cost is measured today as reduction steps (`ApplyStats.steps`).
- **Lineage.** Massalin's superoptimizer (1987); Denali (Joshi, Nelson & Randall, 2002, e-graph + prover); STOKE (Schkufza, Sharma & Aiken, 2013, stochastic x86 search); AlphaDev (Mankowitz et al., Nature 2023, RL → a sorting routine merged into libc++).
- **Why it didn't go farther.** Superoptimization has **never scaled past tiny straight-line kernels** **[P1]**: the search space is combinatorial, loops and branches are hard, and cost-model fidelity (does the proxy match the real hardware?) is a persistent problem. AlphaDev optimized a handful of small functions, not programs.
- **disp's bet.** A dependent-type spec is a far stronger oracle than STOKE's test cases or a reference impl, and reverse-mode credit assignment (§13) aims to make the search *directed* rather than stochastic. Whether that beats the combinatorial wall on anything bigger than a kernel is unknown (Q3, Q4).

### 14. Self-improvement: the optimizer turned on itself

Given a meta-utility, the optimizer optimizes *itself*, then re-derives the calculus, the checkers, and a hardware model.

- **In disp.** The endgame is explicitly the **third Futamura projection** (a program-optimizer applied to itself — see §3): the optimizer is a disp program, so optimizing it is just optimizing one particular program that happens to be the optimizer (`OPTIMIZER.typ` §1, §5; `GOALS.md`). Improvements are small, individually-verified swaps of one piece of its own code at a time — guided by profiling and each proven not to make things worse — rather than one monolithic recompile.
- **Lineage.** Futamura projections (1971); Schmidhuber's **Gödel machine** (2003) — provably-optimal self-rewrites; Turchin's supercompilation (1986); reflective towers / 3-Lisp (Smith, 1982).
- **Why it didn't go farther.** The Gödel machine was **never implemented**: finding the improvement *proof* is itself the wall **[P3]**, and the 2025 "Darwin Gödel Machine" (Zhang & Clune) gets empirical self-improvement only by **dropping the proof requirement**. Supercompilation blows up unpredictably; 3-Lisp's reflective tower had slippery semantics and no killer app. Self-improvement-with-proof is the most-attempted, least-achieved idea in this document.
- **disp's bet.** Outsource the proof *search* to an untrusted optimizer and shrink the *proof* to a re-checkable certificate (§12). This is a sensible re-factoring of the Gödel-machine wall — but it inherits the wall verbatim: nothing guarantees the optimizer *finds* certified improvements at a useful rate (Q3).

### 15. Neural-guided synthesis

A neural proposer searches; the verifier is a hard filter.

- **In disp.** `GOALS.md`'s "external optimizer" must "handle combinatorial search over programs" and "self-play, continually improve by generating score functions itself." The type checker is the hard 0/1 gate; cost is the smooth ranking signal (`OPTIMIZER.typ` §2). This is the only piece that is barely sketched even in design.
- **Lineage.** DeepCoder (Balog et al., 2017); AlphaCode (2022); **FunSearch** (Romera-Paredes et al., Nature 2023, a new cap-set bound from LLM-proposer + evaluator); **AlphaProof** (DeepMind, Nature 2025, LLM + AlphaZero in Lean, IMO-silver level); HyperTree Proof Search (Lample et al., 2022).
- **Why it didn't go farther.** Verifier-as-hard-filter demonstrably works — but every working system is **narrow, compute-heavy, and the verifier does the real work**; the proposer stays unreliable, and **none self-improves its own search**. Generalizing from "prove a benchmark theorem" to "synthesize certified hardware-optimal low-level code from a dependent spec" is strictly harder (a vastly larger space plus hardware cost-modeling).
- **disp's bet.** disp provides exactly the substrate this paradigm wants: a cheap, sound, machine-checkable verifier (§12) and a dense cost signal (§9, §13). The component that didn't exist until ~2022 — a capable neural proposer — is now real, which is much of why this endgame is *newly thinkable* (§V). Whether it is *tractable* is the open question the whole project rides on.

---

# Part V — What disp is attempting, why it likely hasn't been done, and what's make-or-break

### The attempt, stated plainly

disp is trying to build a **universal, verified, self-improving program optimizer**: write a specification as a dependent type, turn the type checker into a 0/1 score, multiply by a hardware-faithful cost score, and search a reflective low-level calculus for a program that is **both formally correct and efficient** — then turn that search on itself to improve the optimizer, the checkers, and the calculus (`GOALS.md`; `OPTIMIZER.typ`: *"verified gradient descent on a graded cost over a materialized net"*).

The closest *shipping* relative is F\*/HACL\*/EverCrypt: dependent spec → verified optimized code, deployed at scale. disp's one-sentence delta is **automation of the human's job there** — synthesize the code and the proof instead of writing them — on a substrate (intensional tree calculus) where the checker is itself an optimizable program. Every individual mechanism disp uses has a sharp precedent (§§1–15). The *integrated system* — a reflective/intensional substrate + types-as-predicates + a certificate-checked untrusted optimizer + neural synthesis + self-application — is, as far as the prior art shows, **unoccupied**.

### Why this combination has stayed unoccupied

Not because it required an insight no one had. For four structural reasons, distinct from the per-piece graveyard reasons above:

1. **Disciplinary silos.** The ingredients live in communities that barely cite each other — logic/constraint programming, dependent type theory, partial evaluation/metacompilation, programming-language reflection, and neural ML — plus Jay's near-solo tree-calculus program. A synthesis requires one person fluent in all of them. That rarity is most of the explanation.
2. **Each field has a *correct, local* reason to walk away from the combination.** Type theorists know dependent types don't scale to everyday code and treat "synthesize everything" as naive; synthesis researchers know proof search is the wall and avoid demanding full formal correctness; the reflection community watched 3-Lisp and supercompilation stall. The experts best positioned to build disp each hold a *locally valid* pessimism. That is how a thing stays undiscovered without anyone being wrong.
3. **A load-bearing component only just became real.** The Gödel machine sat unimplementable for ~20 years because the "thing that proposes certifiable improvements" did not exist. A neural proposer capable of that is a 2022-onward artifact. disp's endgame is, in a precise sense, **newly thinkable** — the strongest honest version of "why now, and not sooner."
4. **It is a marathon with deferred payoff.** A metacircular substrate whose value is bet on an unbuilt optimizer fits neither grant cycles nor product roadmaps that demand shippable increments. It takes an obsessive long-horizon builder, which self-selects hard.

### The crux, restated

disp chose an **intensional** substrate for speed and decidability (`tree_eq` = O(1) hash-cons identity), and its endgame is fundamentally about **extensional** equivalence (replace a program with a different, faster, *equal* one). The entire optimizer is a bridge across exactly the gap that choice opened. The thing that makes disp fast is the thing that makes disp hard — **[P4]** is not one risk among several; it is the spine of the project.

### Make-or-break empirical questions

Each is falsifiable and tied to machinery disp already has. In rough order from most-fundamental to most-engineering:

1. **Is there a decidable, locally-composable fragment of `~_T` rich enough to license real rewrites?** (§7) — *Success:* certify a non-trivial rewrite (map-fusion, a strength reduction) licensed by the walker-defined `~_T` and re-checked by the ~30-line trusted checker on the existing differential harness. *Falsifier:* the decidable fragment licenses essentially nothing past `tree_eq`; every useful rewrite class needs undecidable reasoning. *This is the one question whose answer most determines whether disp is a language or a dead end.*
2. **Can sound duplication recover enough sharing to make superposition search affordable?** (§11) — disp already measured that no-memo ic-net inflates work-units 4,000–67,000× and that the distributes-over-duplication conjecture is affine-only (provably false for project-twice recognizers). *Success:* label-coordinated duplication makes the conjecture hold for a non-affine recognizer class on the `sup_λ` prototype, with measured sharing recovery. *Falsifier:* no sound duplication discipline beats naive enumeration → the search substrate has no advantage and the optimizer cannot afford to explore.
3. **Can a proposer find certifiable improvements faster than checking junk costs?** (§§13–15) — the Gödel-machine wall. *Success:* on a fixed benchmark and compute budget, the optimizer finds certified improvements a baseline (STOKE/egg/a human) did not. *Falsifier:* proposer hit-rate × value < verification cost — disp spends more rejecting bad candidates than it gains.
4. **Can the cost model be made hardware-faithful?** (§§8–9, §13) — interaction count is a space-biased proxy, not wall-clock, and the kernel has no measurement primitive yet (`GOALS.md` wants a driver that returns time/memory). *Success:* a modeled-hardware cost whose ranking correlates with measured wall-clock on a real backend (the lambada/rust-eager differential is the measurement channel). *Falsifier:* proxy-optimal diverges from hardware-optimal unboundedly → optimizing the proxy optimizes nothing real.
5. **Can the metacircular checker self-verify without the Löbian wall mattering — and without being unusably slow?** (§§3–4) — the `coh_check` gate landed and `GoodRespond` now composes, but `StrictType` respond-kind dispatch is open and self-typing is vacuous-totality in places; the R0 work found a subject-reduction gap defended only by use-site re-checking. *Success:* `StrictType` checks `respond : RespondShape` non-vacuously and the kernel verifies its own fragments without a *growing* external anchor. *Falsifier:* self-verification is unsound past the use-site defense, or needs an anchor so large the metacircularity buys nothing **[P3]**.
6. **Does the whole edifice run fast enough to be a tool, not a demo?** (§5, §10) — 8 GB test heaps, ~2× generic-walker slowdown, OOM under auto-verify, per-file `clearCaches` as a stopgap. *Success:* a validated lean fast-path closes the generality tax without weakening the spec, and self-verification scales structurally (not per-file workarounds). *Falsifier:* the metacircular generality tax grows unbounded with system size → disp can *describe* itself but cannot *run* itself at useful speed **[P1]**.

### Bottom line

disp's foundation is a thoughtful, genuinely-novel **recombination** with one narrow novel hinge — NuPRL-style predicate typing on an intensional, reflective substrate — sitting on the *right* (predictable) side of the 60-year declarative pendulum. Its endgame is where the prize is, and it sits squarely on top of three walls that stopped its ancestors: the intensional/extensional equality gap (Q1), superoptimization's scaling failure (Q2–Q3), and the Gödel-machine proof-search wall (Q3). None of those walls has been shown to fall; disp's own docs concede the optimizer "is not yet built."

The right way to judge disp is therefore **not** as one innovation but as two claims with opposite risk profiles: a *settled, modest* foundation and an *everything-or-nothing, unbuilt* endgame whose feasibility reduces to the six questions above — most sharply, Q1. The experiment that would tell the world which kind of project disp is: prove a small version of Q1 — one certified, `~_T`-licensed rewrite, re-checked by the trusted kernel, on the differential harness that already exists. That single result is evidence no one in any of the five contributing fields currently has.

---

## Sources

Grouped by section; these are the load-bearing external references.

- **§1 Tree calculus / content-addressing.** Barry Jay, *Reflective Programs in Tree Calculus* (2021) — <https://github.com/barry-jay-personal/tree-calculus>. Unison content-addressed code — <https://www.unison-lang.org/>.
- **§2 Types as predicates.** NuPRL / Computational Type Theory (Constable et al.). PVS predicate subtyping (Owre, Rushby, Shankar, 1998). Liquid Haskell (Vazou et al.) — <https://goto.ucsd.edu/~nvazou/real_world_liquid.pdf>.
- **§3 Metacircularity.** Futamura projections (1971); Jones, Gomard & Sestoft, *Partial Evaluation* (1993). MetaCoq / "Coq Coq Correct!" (JACM 2025) — <https://dl.acm.org/doi/10.1145/3371076>. Lean4Lean (Carneiro, 2024) — <https://arxiv.org/abs/2403.14064>.
- **§4 LCF/sealing.** Milner, LCF (1972). DCC (Abadi, Banerjee, Heintze, Riecke).
- **§6 Dependent types.** Undecidability of λΠ typability — <https://arxiv.org/abs/2306.07599>. Project Everest / HACL\*/EverCrypt — <https://project-everest.github.io/>.
- **§7 Equality.** Cedille zero-cost coercions / λ-encodings (Stump et al.) — <https://arxiv.org/abs/1803.02473>. Observational Equality: Now For Good (Pujet & Tabareau, POPL 2022) — <https://inria.hal.science/hal-03367052v4/document>. Cubical Agda (JFP 2021). "Marriage of Univalence and Parametricity" — <https://arxiv.org/abs/1909.05027>.
- **§9 Coeffects/QTT.** Petricek, Orchard & Mycroft, "Coeffects" (2014). Atkey, "Quantitative Type Theory" (2018); Idris 2 (Brady, 2021).
- **§11 Interaction nets / optimal reduction.** Asperti & Guerrini, *The Optimal Implementation of Functional Programming Languages* (1998); Asperti–Mairson on bookkeeping cost. HVM2 — <https://github.com/HigherOrderCO/HVM>.
- **§12 Verified optimization.** Equality saturation (Tate et al., 2009); egg — <https://arxiv.org/abs/2004.03082>. ROVER: verified e-graph rewriting — <https://arxiv.org/abs/2406.12421>. CompCert — <https://compcert.org/>.
- **§13 Superoptimization.** STOKE (Schkufza, Sharma & Aiken, 2013). AlphaDev (Nature 2023) — <https://www.nature.com/articles/s41586-023-06004-9>.
- **§14 Self-improvement.** Schmidhuber, Gödel Machines — <https://arxiv.org/abs/cs/0309048>. Darwin Gödel Machine (2025) — <https://arxiv.org/abs/2505.22954>. 3-Lisp / reflective towers — <https://blog.sigplan.org/2021/08/12/reflective-towers-of-interpreters/>.
- **§15 Neural synthesis.** FunSearch (Nature 2023) — <https://deepmind.google/blog/funsearch-making-new-discoveries-in-mathematical-sciences-using-large-language-models/>. AlphaProof (Nature 2025) — <https://www.nature.com/articles/s41586-025-09833-y>.
- **Layout/Prolog context (the triggering discussion).** Cassowary (Badros, Borning & Stuckey) — <https://constraints.cs.washington.edu/solvers/cassowary-tochi.pdf>. "Why did Prolog lose steam?" (Mark J. Nelson) — <https://www.kmjn.org/notes/prolog_lost_steam.html>. "Fifty Years of Prolog and Beyond" — <https://arxiv.org/abs/2201.10816>.
