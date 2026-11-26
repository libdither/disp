# disp vs. size/simplicity-objective gaming: what it closes, what stays open

Scope: how disp (repo at `~/Projects/disp`, read 2026-11-09, HEAD `496d220ae35a`) handles, or would handle, minimizing the size or simplicity of programs and proofs, and which reward hacks against a size objective it closes. This is an assessment, not advocacy. It does not repeat the general verifier-hacking case record in `research/CONSTRAINED_PROGRAM_SEARCH.md` §6.7 or `research/NATURE_OF_OPTIMIZERS.md` §6; it builds on them.

Citation convention: repo links are relative to this file (`../../../` is the repo root), shown as `path:line`. "Implemented" means code in `lib/` or `src/` that the test suite runs. "Designed" means it appears only in `research/*.typ|md`.

Headline: **no size or simplicity objective exists in disp today.** No code computes program size as a score. The only node count in the checking path is a lemma threshold (`lib/rules.disp:158-173`). The optimizer that would multiply a 0/1 gate by a cost/size score is "Designed but not built" (README.md:148). Everything below is about which hacks the *substrate and checker* would let through once such an objective is attached.

---

## Q1. Does "everything is a tree" give a canonical, notation-free size measure? Where does surface syntax and elaboration still hide work?

### Takeaway
Mostly yes, at the level of the elaborated tree. Names, record field names, `::` doc annotations, comments and sugar never reach the tree, and library references are inlined as the referenced closed tree. So "node count of the elaborated tree" is a notation-free measure, much like Tromp's bit-length for BLC and CL. It is not free of every choice, though, and it moves the hiding place rather than removing it. Three things matter:
- the tree depends on the elaborator's bracket-abstraction algorithm, which is part of definitional equality and changes sizes several-fold;
- the elaborator *evaluates* closed applications at compile time, with a 400M-step budget, so the source can be tiny while unbounded work happens at elaboration and only the result is measured;
- tree size and DAG size diverge enormously because of library inlining.

### Cited Findings
- Every value is a tree grown from the leaf `t`. `true = t`, `false = t(t)`, and `3` is sugar for `succ(succ(succ(zero)))` with identical trees — [README.md:46-51](../../../README.md).
- Record field names "live only in scope, never in the tree, so the trees are exactly the hand-written ones". A `::` annotation is "a doc annotation: parsed ... and never resolved, compiled, or verified" — [SYNTAX.typ:150](../../../SYNTAX.typ).
- A variable that names a definition elaborates to that definition's compiled tree as a literal (`if (entry?.tree) return { tag: "lit", t: entry.tree }`). The emitted program is therefore a closed tree with library code embedded, not a tree with references — [src/elab/expr.ts:88-101](../../../src/elab/expr.ts).
- Bracket abstraction "defines which tree a binder becomes — i.e. it is part of definitional equality — so changes here change every compiled program". The rewrites (K-elimination, η, `S (K p) (K q) → K (p q)`) exist "to keep the emitted tree small" — [src/elab/cir.ts:1-5, 55-62](../../../src/elab/cir.ts).
- `cirToTree` reduces applications during elaboration: `return elab.cs.apply(cirToTree(e.f), cirToTree(e.x), B())` — [src/elab/cir.ts:118](../../../src/elab/cir.ts). The budget is `APPLY_BUDGET = 400_000_000`, "a DIVERGENCE bound, not a" cost limit — [src/elab/state.ts:19-37](../../../src/elab/state.ts).
- `test lhs = rhs` is a compile-time assertion that "both sides must elaborate to hash-cons-equal trees" — [SYNTAX.typ:167](../../../SYNTAX.typ). A test is proof by computation at elaboration time.
- Jay's book: plain bracket abstraction of a combinator of size k has size 5k−2 (`[x]I` has size 23 vs `K I` size 7). "This five-fold expansion is especially problematic when abstractions are nested", which motivates star abstraction — [Jay, *Reflective Programs in Tree Calculus*, Ch. 4, local copy](../../barry-jay/reflective-programs-in-tree-calculus-tree_book.pdf); [GitHub](https://github.com/barry-jay-personal/tree-calculus).
- Tromp: changing bracket abstraction from λ1 to λ2 shrinks one combinator from 2030 to 374 bits. "λ-calculus is a much more concise language than CL", because CL bracket abstraction "typically introduces at least one, and often several S's and K's (2 bits each) per level of depth per variable occurrence" — [Tromp, *Functional Bits*, §3](https://tromp.github.io/cl/LC.pdf).
- The canonical term interchange is the ternary preorder string `0/1/2`, "byte-identical across backends" — [EVALUATOR.md:20-21](../../../EVALUATOR.md). That string is a canonical, representation-free description of a tree whose length is its node count.
- The search source `Trees` enumerates "every tree, smallest first" by node count — [lib/trees.disp:1-24](../../../lib/trees.disp). Size order is already the built-in prior for the honest check.

### Inferences
- **Closed by the tree measure:** identifier-length games, notation and `notation`/`macro` sugar, comments, and naming. Lean token-count metrics have these problems: ProofOptimizer counts Lean tokens and "does not penalize long identifier names" ([Gu et al. 2025, §2](https://arxiv.org/html/2510.15700v1)). On an elaborated tree none of this survives.
- **Not closed — compile-time evaluation is the `decide`/reflection hole in a new place.** Writing `w := search_for_witness(spec)` as a closed definition makes the elaborator run the search, up to 400M steps, and store only the result tree. If the metric is source length, the hack is total. If the metric is elaborated-tree size, the hack disappears for *values*: you pay for the result's size, not the search. For *programs* it reappears in another form: a small program that searches at run time is legitimately small, and its cost shows only on a cost axis (Q2). The measure must be stated as "size of the elaborated closed tree", never "size of the `.disp` source".
- **Not canonical — the elaborator is a free parameter.** The same surface program yields different trees under different abstraction schemes, as Jay's 5× and Tromp's 5.4× gaps show. The scheme is fixed in TypeScript (`cir.ts`). Its in-language spec, `lib/elab/bracket.disp`, was deleted ([src/elab/cir.ts:4-5](../../../src/elab/cir.ts)). If a minimizer can choose or emit trees directly, it is not bound by the elaborator at all, which is fine: the tree is then the honest object. If it is scored on surface programs, elaborator quirks become an exploitable surface. Example: arrange code so `S (K p) (K q) → K (p q)` fires and evaluates work at compile time.
- **The invariance theorem limits any "canonical" claim.** Tree size is canonical *for tree calculus*. Across calculi it is canonical only up to an additive constant ([Tromp §4](https://tromp.github.io/cl/LC.pdf)), and for short proofs the constant dominates.

### Gaps
- I found no measurement in the repo of how much the elaborator's compile-time reduction changes tree sizes, nor any pinned tree-size numbers for library definitions.
- I did not find whether the deleted `bracket.disp` spec still matches `cir.ts`. The comment says to "recover via git when re-validating", which implies it is not continuously checked.

---

## Q2. Is there an analogue of `decide` / `native_decide` / proof by reflection? Can a tiny witness push work into the checker, and does cost accounting charge for it?

### Takeaway
Yes, and it is the default architecture rather than an exception. In the claim layer a Proved verdict has no proof object: the witness is the program, and the "proof" is the checker's bounded symbolic search (`judge`, `certify`, `eq_by_induction`, `over`). In the kernel, equality proofs are literally `refl`, with a ledger and reachability doing the work. A proof-length metric in disp is therefore close to vacuous, and the meaningful quantity is checker work. Checker work is bounded by fuel and split depth, but it is not reported in the verdict and not charged anywhere. The host fast paths (`tree_eq`, `m_advance`) are the `native_decide` analogue. Unlike `native_decide`, they sit under a differential discipline against in-language definitions, but they are still trusted at run time.

### Cited Findings
- The confirmer's verdicts come from a symbolic search: "a search splits a needed piece along its type's cases ... induction is licensed only on a strictly smaller input-derived piece" — [lib/confirm.disp:1-10](../../../lib/confirm.disp). Budgets are constants: `run_fuel := 4000` (line 379), `derive_fuel := 300000` (line 305), `split_depth := 3` (line 469).
- `judging` takes lemmas, split depth and step budget, and "None of them can turn a wrong verdict right: a deeper search or a longer run only settles more" — [lib/rules.disp:174-178](../../../lib/rules.disp). `judge_with` returns only a `Verdict` (Proved / refuted w / Open with a reason), not the fuel spent — [lib/rules.disp:180-189](../../../lib/rules.disp).
- `eq_by_induction(step_f, step_g)` takes two programs and no proof term; the checker runs both under a shared induction hypothesis — [lib/confirm.disp:704-711](../../../lib/confirm.disp); README demo [README.md:111-128](../../../README.md).
- Kernel equality: `Fn(Nat, {n} => Fn(Nat, {m} => Fn(Eq(Nat, n, m), {_} => Eq(Nat, m, n))), {n, m, p} => refl) = true`. Transitivity is proved by `refl` too, because "reachability over declared ends gives symmetry and transitivity" via "the session ledger". Induction proofs are `add_zero_rec(n, refl, {p, ih} => refl)` — [lib/kernel/kernel.test.disp:713-745](../../../lib/kernel/kernel.test.disp).
- Host natives: `NATIVE_RULES = { tree_eq: "treeEqRules", m_advance: "machineRules" }`. A native "is a disp definition the host answers for, keyed by the definition's compiled tree id"; a `null` answer falls back in-language "so the intercept stays transparent" — [src/eval/eager.ts:25-33](../../../src/eval/eager.ts), [src/core/tree.ts:272-278](../../../src/core/tree.ts). A conformance mode runs the natives in-language (`noNativeIntercept`) — [src/eval/eager.ts:31-33](../../../src/eval/eager.ts).
- The hosts "only accelerate these in-language definitions and are validated against them. Five evaluator backends ... must agree byte for byte" — [README.md:136](../../../README.md). `tree_eq` is "the one live native fast-path" — [FOUNDATIONS.md:41](../../../FOUNDATIONS.md).
- **Cost accounting on the default backend is memo-dependent.** In the eager reducer the memo lookup runs *before* the step counter, and a hit costs no step (`if (c !== undefined) { st.hits++; ... continue }` at line 652; `counters.steps++` only at line 656). Native rules increment their own counter, not `steps` — [src/core/tree.ts:615-656](../../../src/core/tree.ts).
- The in-language machine's step count is defined as "the eager evaluator's `steps` with the memo off". "leaf and stem heads only build and are free" — [lib/machine.disp:1-9](../../../lib/machine.disp). `lib/growth.disp` counts those steps and fits polynomials, and says explicitly: "A fit is evidence, not a proof" — [lib/growth.disp:1-5, 12-14](../../../lib/growth.disp).
- Cost types exist as a test-level theory: "a member of a function type names a step bound", and "the certificate's cost is a claim too: the bound is checked by the machine, not trusted" — [lib/theories/cost.test.disp:1, 45-47](../../../lib/theories/cost.test.disp).
- Design: score = `[typechecks(e,T)] × cheapness(e)`. Measured cost is "Guidance (untrusted)", and no-regression comes from a "proven improvement direction ... never from a measurement" — [research/OPTIMIZER.typ:91-107](../../../research/OPTIMIZER.typ). There are four cost rungs (static grade, intrinsic interaction count, modeled, measured); "soundness ... is always symbolic — it never runs a candidate" — [research/OPTIMIZER.typ:763-778](../../../research/OPTIMIZER.typ).
- External: ProofOptimizer found shortened Lean proofs "much slower than the original proof", where "a faster proof algorithm is replaced by a shorter but slower method" such as brute-force `interval_cases`. Switching the objective to heartbeats removed the "short but slow proofs" — [Gu et al. 2025, §5.2–5.2.1](https://arxiv.org/html/2510.15700v1).

### Inferences
- **"Minimize proof length" has no stable target in disp.** The witness for a Proved claim is the program, or `refl`. If size is scored on the witness, the minimizer is pushed toward whatever the checker can settle with the most search. That is the disp form of "replace a structured proof by `decide`/`omega`/`interval_cases`". Under fixed budgets (`run_fuel` 4000, `split_depth` 3) the push is capped: a witness that needs more checker work goes Open, not Proved. So the hack is *bounded by budget*, not charged by cost. A minimizer then optimizes right up to the budget edge, and verdicts become brittle to budget changes. The brittleness is monotone in the sound direction: more budget only settles more ([rules.disp:174-176](../../../lib/rules.disp)).
- **The fix the literature found (charge check-time, as heartbeats did for ProofOptimizer) is not wired up.** `judge_with` throws away the remaining fuel. A correct disp objective would be (witness tree size, checker fuel consumed) as a Pareto pair, or a two-part code including the certificate. Neither exists.
- **Memo-dependent `ApplyStats.steps` is a live cost-gaming surface for the designed M0 optimizer.** FOUNDATIONS §13 says "Cost is measured today as reduction steps (`ApplyStats.steps`)" ([FOUNDATIONS.md:161](../../../FOUNDATIONS.md)). On eager and rust-eager a candidate evaluated after another that warmed the memo looks cheaper; this is exactly the entanglement OPTIMIZER §3 cites to justify ic-net. The honest counters are the machine's memo-off steps (implemented, slow, in-language) and ic-net's interaction count. For ic-net, the README and OPTIMIZER both list the cost readouts as "next substrate work", i.e. not built ([OPTIMIZER.typ:806-810](../../../research/OPTIMIZER.typ)).
- **The machine's step model has free construction.** "leaf and stem heads only build and are free", so steps count K/S/triage dispatches, not allocation. A size-and-steps objective would not charge for large intermediate data. The designed space readout (peak live nodes, [OPTIMIZER.typ:135-143](../../../research/OPTIMIZER.typ)) is the missing axis.
- **`native_decide` comparison.** Lean's `native_decide` trusts compiled code that is not checked against the kernel. disp's natives are keyed to an in-language definition's tree id, fall back on `null`, and have a no-native conformance mode and a naive (no hash-cons) "honesty backend" ([EVALUATOR.md:38](../../../EVALUATOR.md)). This is a real mitigation: a bug is detectable by differential runs. It is not a closure: in normal operation the checker trusts the native answer, and the differential runs cover test fixtures, not every verdict a minimizer produces. A minimizer that finds a native/in-language divergence on some input it controls would get a wrong verdict accepted until someone runs the differential on that input.

### Gaps
- I did not verify whether `m_advance`'s native handler counts steps identically to the in-language machine for all inputs. `test/machine.test.ts` is cited as pinning this, but I did not read it.
- Whether a verdict produced under natives is ever automatically re-checked under `noNativeIntercept` (rather than only in the test suite): no evidence found.

---

## Q3. Reflection: can a witness inspect the claim or checker and special-case it? Can a generated type (claim) be vacuous?

### Takeaway
Reflection creates hacks that Lean structurally cannot have, and the repo knows about them and pins them. Within the claim/confirmer layer most are mitigated: forged holes are escaped as opaque, inspecting a symbolic piece forces a case split, and the induction lens only exposes strictly smaller pieces. In the kernel, `Guard` accepts *known lies*: a Pi with an inspecting dependent codomain certifies `true` while being false at every concrete point. Those lies are pinned as passing tests, and only the finite-probe `TwoFace` tier catches them, "not a proof". Spec gaming of the type itself is wide open: a type is any program, and nothing in disp checks that a claim is non-vacuous or matches intent.

### Cited Findings
- Any program can take any other apart "with no quoting and no reflection API" (`shape_of(not) = "fork"`) — [README.md:53-60](../../../README.md).
- Confirmer anti-forgery: "A program can build a marker-shaped tree on purpose. That tree is no hole of ours, so ... it is wrapped whole as a Forged hole, which every judge finds opaque" — [lib/confirm.disp:122-127](../../../lib/confirm.disp) (gap I in the header, line 28). "Only the triage rule and the function position inspect a value, so those are the only places a piece can be needed" — [lib/confirm.disp:370-378](../../../lib/confirm.disp).
- Induction: "The claim layer closes it by construction ... There is no way to state the hypothesis at the root" — [research/LOB_AND_CURRY.md:362-373](../../../research/LOB_AND_CURRY.md); the law is in [lib/rules.disp:55-68](../../../lib/rules.disp).
- Kernel forgery: an assembled forged hypothesis is refused by Meta and Guard (`test Pi(Guard, Nat, {_} => Bool, assembled_forgery) = false`) — [lib/kernel/kernel.test.disp:135-145](../../../lib/kernel/kernel.test.disp).
- Kernel known lies, pinned as `true`: `sniff_cod := {n} => if (is_neutral(n)) { Nat } else { False }` with `test Pi(Guard, Nat, sniff_cod, {n} => zero) = true`, and `lying_cod` likewise. The comment: "the Pi certificate is a lie at every concrete point. This is the residue of the step-1 barrier ... structural INSPECTION is the piece it did not reach" — [lib/kernel/kernel.test.disp:568-590](../../../lib/kernel/kernel.test.disp). Under the Enum tier, `Pi(Enum, Bool, sniff_motive, {b} => 5) = true` — line 153.
- `TwoFace` "conjoins a CONCRETE face ... It only ever adds obligations, so it can refuse but never admit more ... A lie detector in the gate-certification idiom, not a proof -- the battery is finite" — [lib/kernel/kernel.test.disp:592-603](../../../lib/kernel/kernel.test.disp). The battery is eight fixed trees: `probes := [t, 5, true, false, pair(5, 5), [1], inj("zz", t), succ(succ(zero))]` — [lib/kernel/kernel.disp:950-951](../../../lib/kernel/kernel.disp).
- A claim is any program `{witness, challenge} => Bool` — [lib/claim.disp:1-8](../../../lib/claim.disp). In the README's own example, `nat_fn` "says nothing about trees that are not numbers" — [README.md:67-70](../../../README.md).
- Transport along an equality "is licensed only for claims that never inspect the tree being swapped" — [research/CLAIMS_OVER_TREES.md:444-447](../../../research/CLAIMS_OVER_TREES.md).
- Prior work (already in the repo's notes): the kernel "does not verify that the statement faithfully encodes the intended informal problem", and surface three is "a generated type that is vacuous or incomplete" — [research/CONSTRAINED_PROGRAM_SEARCH.md:203](../../../research/CONSTRAINED_PROGRAM_SEARCH.md); [research/NATURE_OF_OPTIMIZERS.md:169-173](../../../research/NATURE_OF_OPTIMIZERS.md).

### Inferences
- **New hack class (made worse by reflection): checker-sniffing witnesses.** In Lean a proof term cannot observe that it is being type-checked. In disp a witness or type family can triage on its argument and behave differently on a hypothesis or neutral than on a concrete value. The confirmer handles this soundly by turning every inspection into a case split. The kernel's `Guard` tier does not for dependent codomains: an inspecting `sniff_cod` gives a `true` certificate that is false everywhere concrete. Under size pressure this hack is *cheap*: `sniff_cod` plus `{n} => zero` is a tiny witness. A minimizer told to produce small kernel-certified terms under `Guard` would find it, which is the extremal-Goodhart pattern of [NATURE_OF_OPTIMIZERS.md:161-167](../../../research/NATURE_OF_OPTIMIZERS.md). The defense is to require `TwoFace` or the confirmer, i.e. use the claim layer rather than the kernel's `Guard` alone. Even `TwoFace` is an 8-tree test.
- **Vacuous claims are cheaper than in Lean.** Because claims are arbitrary Bool programs, the smallest claim is `{_w,_c} => true`: one K-shaped tree, Proved by any witness. Conditional claims of the `if (NatR(x)) {...} else { true }` shape are vacuous wherever the guard is false. If a self-play loop generates claims (GOALS.md:16) and is rewarded for proving them compactly, the cheapest strategy is a claim whose guard rarely fires. Nothing in the implementation measures claim strength. Exploit-model checks (does a trivially wrong program pass?) are named in CONSTRAINED §6.7 but not implemented.
- **Mitigation the substrate offers for free:** the honest search is a ready-made exploit model. Running `by_search(claim, trivial_wrong_witness, budget)` and requiring `refuted` is a cheap non-vacuity test, because `Trees` enumerates all trees. It only works if someone supplies the "trivially wrong" witnesses, and it is bounded.

### Gaps
- Whether any production checking path (the `checked.disp` annotations, module loads) uses `Guard` alone, where the pinned lies are live, or always a lie-detecting tier: `checked.disp` uses `judge` (the claim layer), and the kernel tiers are selected per call. I did not trace every call site.
- No repo document measures how often small random witnesses hit the pinned lies.

---

## Q4. Trusted base: which Lean exploits (native_decide, csimp, axioms/sorry, environment hacking) have disp analogues?

### Takeaway
Every Lean surface has a disp analogue, and disp adds two:
- **types carry trusted slots** (`ends`, `gate`, `check`, `norm`, `members`), policed only by finite probes;
- **the shortcut and rule lenses are the trusted floor and are not machine-proved.**

disp has a clean `sorry`/axiom audit for the claim-layer module checker (`on_open := "assume"` reports "Assumed"). It also has a lemma mechanism whose input list is trusted by `judge_with`.

### Cited Findings
- Rule floor: "The rules for building shortcuts are the trusted part, and they have not themselves been proved sound" — [README.md:130-133](../../../README.md). LOB_AND_CURRY says "proved once per rule by hand", yet lists "The five laws are proved by hand ... A machine-checked proof would have to live in another system" under "What stays open" — [research/LOB_AND_CURRY.md:336-349, 409-411](../../../research/LOB_AND_CURRY.md). The two sources disagree: the README says unproved, LOB_AND_CURRY says hand-proved. I found no written proof in the repo.
- Declaration slots are trusted: "Coherence: the declaration slots are TRUSTED, so each one gets a probe. A type claiming distinct concrete endpoints must be empty (else an assumption launders a false equation into the ledger)" — [lib/kernel/kernel.test.disp:989-994](../../../lib/kernel/kernel.test.disp). The check is `list_all(Tree, {v} => not(T(v)), probes)`, i.e. emptiness is tested on the 8 probes only — [lib/kernel/types.disp:68-80](../../../lib/kernel/types.disp). "ends, a TRUSTED slot in the same class as gate/check -- a lying ends is a lying type" — [lib/kernel/kernel.test.disp:717-719](../../../lib/kernel/kernel.test.disp). `GoodGate(LieGate) = false` catches a top-typed gate — line 986-987.
- Module checker: "With `on_open := "assume"`, an Open definition loads, is reported, and stands as a lemma; a later one Proved with lemmas while any assumption stands is reported as resting on them." The default is `"reject"` — [lib/theories/checked.disp:33-50, 58-61](../../../lib/theories/checked.disp).
- Lemmas: `judge_with` substitutes any tree in `how.lemmas` with a `Lemma` hole judged by its declared type ("a lemma is a verdict already had") — [lib/rules.disp:154-189](../../../lib/rules.disp). Only definitions of at least `lemma_min_size := 64` nodes become lemmas: "A definition the size of a combinator is no lemma" — [lib/rules.disp:155-158](../../../lib/rules.disp).
- `use raw` "loads a file's values only: its annotations and `given`s are neither compiled nor verified" — [SYNTAX.typ:267](../../../SYNTAX.typ). Every `lib/*.disp` file, and the README examples, open their dependencies with `open use raw` — e.g. [lib/claim.disp:9-12](../../../lib/claim.disp).
- Evaluation-strategy trust: "the *termination domain* does not [agree]: an eager backend's K-discard shortcuts ... decide which diverging-but-discarded subterms are survivable, and the kernel's checking paths lean on that discard" — [EVALUATOR.md:64-71](../../../EVALUATOR.md).
- Designed rule tiers: "Tier 1 — test-certified ... A strong filter, not a proof; in the trusted base until promoted" — [research/OPTIMIZER.typ:610-611](../../../research/OPTIMIZER.typ). The trusted base also includes "the cost metric" — [research/OPTIMIZER.typ:84-87](../../../research/OPTIMIZER.typ).
- The repo's own mapping: "disp's analogues are the fast-path host code, the `.opt.disp` overlays, and guard rebinding" — [research/NATURE_OF_OPTIMIZERS.md:201](../../../research/NATURE_OF_OPTIMIZERS.md). The `.opt.disp` overlays now exist only under `archive/live-kernel/std/` (`nat.opt.disp`, `case.opt.disp`, `prelude.opt.disp`).
- External analogue: before a fix, Metamath's `minimize *` "could add dependencies on new axioms if it made the proof shorter", so the recommended form became `minimize_with * / allow_new_axioms * / no_new_axioms_from ax-*` — [metamath mailing list](https://go.mail-archive.com/metamath@googlegroups.com/msg00447.html) (as summarized by search; I did not read the full post). This is the canonical precedent for a size minimizer pulling in trust.

### Inferences
- **`sorry`/axiom injection: mitigated, with a better default than Lean RL harnesses.** The `checked.disp` module check rejects Open by default and labels assumptions. That is an in-language `#print axioms`. The remaining hole is the lemma list: whoever builds the `judging` record controls what is believed. In `checked.disp` the list is built only from Proved (or explicitly assumed) definitions, so a harness that lets the policy pass its own `judging` record reopens the hole.
- **Size-driven trust creep (the Metamath lesson).** A size minimizer gains by turning repeated sub-trees into lemmas (≥64 nodes) checked by type instead of by run. That is legitimate when the lemma was Proved. It is a channel for `assume`d lemmas if `on_open := "assume"` is ever on in the scored loop. The objective must count "Assumed" entries as infinite cost.
- **`csimp` analogue (made worse): trusted type slots.** In Lean, `csimp` lets an attribute swap implementations. In disp, a *type* can declare `ends`, `check`, `norm` or `members`, and the judge uses them. They are probed on 8 trees, so a generated type whose `ends` claim distinct endpoints but which is inhabited only by trees outside the probe set passes `Coherent`. That launders a false equation into the ledger, which the comment itself names as the risk. If AI-generated types or libraries enter a size-minimizing loop, this is the most direct way to make proofs `refl`-short.
- **`native_decide`: mitigated** (see Q2), by a differential discipline rather than kernel replay.
- **Environment hacking: partly addressed.** Guards and the rebind protocol mediate redefinition, and `use raw` skips verification by design. A minimizer that can edit or select imports controls what is verified. The structural statement check in CONSTRAINED §6.7 (compare the statement tree with what was asked) is cheap in disp, since it is `tree_eq` on the claim. It is not implemented as a harness rule.
- **Unique to disp: the floor is unproved.** Lean's kernel rules have independent re-implementations and metatheory work. disp's five lenses are "proved by hand" at best and "not proved sound" per the README. Size pressure does not attack the floor directly, but a minimizer that finds a lens-law counterexample gets a Proved for free. The honest check *can* refute a wrong lens law (README.md:96-106), so running `lens_holds` searches as a standing audit is the available mitigation.

### Gaps
- No machine-checked or written proof of the five rule lenses found; the README/LOB_AND_CURRY discrepancy is unresolved in-repo.
- I did not check whether `probes`-only coherence is the final design or a placeholder.

---

## Q5. Hash-consing, "no memo", and tree size vs DAG size

### Takeaway
disp's checker backends are hash-consed, so the natural in-memory size of a program is its DAG: unique nodes, with library code shared. The written-out tree, which is what the ternary interchange and `Trees` order count, can be exponentially larger. These are different measures with different hacks. "No automatic memo makes cost honest" is about *time cost on ic-net*, not about *size*, and it does not settle which size to use. On the default eager backend, both size intuitions and step counts are polluted by session-global hash-cons and memo state.

### Cited Findings
- Hash-consing is global and process-wide ("node identity is process-wide"), sharded intern tables — [src/core/tree.ts:45-90](../../../src/core/tree.ts). `tree_eq` is "a hash-consed O(1) structural-identity check" — [FOUNDATIONS.md:41](../../../FOUNDATIONS.md).
- OPTIMIZER: ic-net "does no automatic cross-occurrence memoization". Its "interaction count is a *pure function of the program's structure*, not of session-global hash-cons state" — [research/OPTIMIZER.typ:116-131](../../../research/OPTIMIZER.typ). "Interaction count is a reasonable but space-biased proxy, provably not wall-clock" — lines 134-143.
- Dropping hash-consing "inflates the work done by 4,000–67,000× (a 600× raw-speed loss)". The draft note says the 4,000–67,000× figure "has no in-repo source" and is "unverified until re-measured" — [FOUNDATIONS.md:7, 141, 212](../../../FOUNDATIONS.md).
- The naive backend is "structural, no hash-cons identity" and is called the "honesty backend" — [EVALUATOR.md:38](../../../EVALUATOR.md).
- Jay: `size` is node count (leaf 1, stem 1+, fork 1+sum), definable in-calculus and self-applicable — [Jay book §5.2](../../barry-jay/reflective-programs-in-tree-calculus-tree_book.pdf).

### Inferences
- **Tree size (unshared) penalizes reuse and rewards compression tricks.** A program that calls a library function twice contains the library tree twice. Under unshared size a minimizer gains by factoring through a local combinator that duplicates at run time, e.g. writing `W f` instead of two copies. That trades size for steps, a legitimate MDL trade. It also strongly punishes using the library at all, which pushes toward bespoke, unreadable combinators: the Massalin "anti-abstraction" outcome the abstraction note warns about ([THE_NATURE_OF_ABSTRACTION.typ:187](../../../research/THE_NATURE_OF_ABSTRACTION.typ)).
- **DAG size equals a two-part code for free, and that is a hack surface.** With hash-consing, shared subtrees cost once, so "DAG size of (library ∪ program)" is the library-plus-program-given-library decomposition. "DAG size of the program alone" makes everything already interned free. The interning arena is session-global and grow-only (scoped reclamation aside), so a candidate's marginal DAG size depends on what else was built in the session. That is the size analogue of the memo-entangled step count. A minimizer could pre-build large structures elsewhere so its submission's marginal DAG is tiny.
- **A defensible size measure for disp** is (i) unshared node count of the elaborated closed tree, which is canonical and state-free but harsh on libraries, or (ii) DAG node count of the closed tree computed fresh, with no session state, which is canonical and charges each distinct subtree once. (ii) is computable from the ternary dump plus a local hash-cons and is the natural MDL measure. Neither is implemented.
- **"No memo" does not transfer to size.** OPTIMIZER's honesty argument concerns cost attribution across candidates on ic-net. It says nothing about size, and the designed e-graph explicitly runs "on the hash-consed backend" ([OPTIMIZER.typ:671, 688-689](../../../research/OPTIMIZER.typ)). Mixing the two invites cross-candidate entanglement in size as well as in steps.

### Gaps
- No in-repo measurement of tree vs DAG size for real definitions (e.g. `judge`, the kernel). This is cheap to measure and would quantify the gap.

---

## Q6. Library-relative size (MDL): does disp charge for library additions, macros, syntax extensions, dialects?

### Takeaway
Not today. Because references inline, a program's elaborated tree already *contains* the library code it uses, so unshared tree size charges libraries per use, and nothing about a library is free. That closes the "add a giant lemma or tactic to the environment, then cite it in one token" hack *if* size is measured on the closed tree. The design documents get MDL right in principle: a two-part code, library and programs on one scale, and degenerate models named as failure modes. None of it is built. Syntax extensions and "decentralized dialects" are not implemented, so they neither help nor hurt yet. The lemma mechanism and trusted type slots are the actual environment-addition channels.

### Cited Findings
- Two-part code: "the library is the model S; user programs are the data-to-model code ... an optimal library for a corpus exists and is well-defined ... the optimum is incomputable" — [research/THE_NATURE_OF_ABSTRACTION.typ:98-106](../../../research/THE_NATURE_OF_ABSTRACTION.typ).
- Degenerate models: "without a constraint separating model from data parts, naive minimization admits degenerate models — S = {x} (one bespoke combinator per program: overfit) and S = everything (empty library: underfit) ... Any disp metric must penalize library complexity and program complexity on the same scale" — [research/THE_NATURE_OF_ABSTRACTION.typ:108](../../../research/THE_NATURE_OF_ABSTRACTION.typ).
- Designed ranking: a "Pareto frontier over two measured axes: (a) two-part MDL of the corpus, computed modulo the licensed rewrite theory ... and (b) measured cost, `ApplyStats.steps`", "never a scalar" — [research/THE_NATURE_OF_ABSTRACTION.typ:211](../../../research/THE_NATURE_OF_ABSTRACTION.typ). Failure modes to instrument include "Goodhart on steps (backend-declared units, never a scalar target)", "corpus overfit" and "optimization pressure drifting into the trusted layer (freeze, human review)" — line 282.
- "Does compression shorten proofs? ... the proof-length version is unestablished" — line 293. "Legibility has no formalization ... compression can diverge from it arbitrarily" — line 299.
- New library abstractions need a "new-name pathway" with "human review"; without schematic licensing, "axis (a) is near-syntactic MDL" — lines 273-276.
- Goal: "decentralized" dialects with "automatic translation between their dialects" — [README.md:5](../../../README.md). No dialect, macro or syntax-extension mechanism exists in `src/` or `lib/` (grep for dialect/macro/syntax extension found none). OUTLINE says dialects would be "a parser/pretty-printer pair" — [GOALS.md/OUTLINE, item 4](../../../OUTLINE.md).
- External: Metamath practice measures shortenings in bytes of set.mm and weighs whether a new helper theorem "pays for itself" by the steps it saves per use, i.e. de facto library-inclusive MDL ([set.mm Minimizing wiki](https://github.com/metamath/set.mm/wiki/Minimizing), as summarized by search; the "pays for itself" phrasing is from the search summary of mailing-list threads, not verified verbatim).

### Inferences
- **Closed structurally (for the closed-tree measure):** "add an environment declaration, cite it cheaply". You cannot cite in disp: the citation *is* the tree. The residual channels are those that make the checker *believe* rather than *run*:
  - lemmas (`judging.lemmas`), which are sound if the list is honest;
  - `use raw`, which skips verification;
  - trusted type slots.

  All three are trust additions, not size additions, and a size objective is blind to them.
- **Syntax extensions and dialects (future) would recreate Lean's notation and macro problem one level up only if size were measured on surface text.** If every dialect elaborates to trees and size is taken on trees, dialects are size-neutral by construction, while the elaborator becomes trusted (Q1). The README's dialect translation goal is unbuilt, so this is a design constraint to state, not an existing property.
- **The DAG measure (Q5) is the implementable two-part code.** DAG size of library-plus-corpus charges a library entry once, which is exactly the MDL accounting the abstraction note asks for, with the degenerate-model failure modes still live.

### Gaps
- Whether the optimizer would measure size on the surface, on CIR, or on trees is not specified anywhere I found. OPTIMIZER says only "cost/size score read from running `e`" ([OPTIMIZER.typ:91-93](../../../research/OPTIMIZER.typ)), which conflates size with cost.

---

## Q7. Open / budgeted verdicts: can a size-minimizer exploit Open ("no failure so far") as if it were Proved?

### Takeaway
The type of a verdict does not allow it: Open is a distinct value, "Proved" is only produced by exhaustion or a shortcut, and the docs repeat that running out of budget proves nothing. The *harness* is the risk. The designed 0/1 gate collapses verdicts, and several designed and implemented components are explicitly "evidence, not proof": growth fits, Tier-1 test-certified rules, the M0 optimizer's validation by tests, and TwoFace probes. Under size pressure, "passes bounded search" selects *for* programs whose smallest counterexample lies beyond the budget. Small programs can encode astronomically late failures.

### Cited Findings
- "A failure is final, but running out of budget proves nothing. Open only means 'no failure so far'". `sneaky`, wrong only on 3, is Open at budget 64 and refuted at 65 — [README.md:78-84](../../../README.md).
- "A Refuted is a fact about every tree; a Proved is one only when it came from exhausting a finite domain or from a shortcut whose contract is agreement with the honest stream" — [lib/verdict.disp:3-5](../../../lib/verdict.disp). `agrees`: "Open is consistent with either" — lines 16-19.
- `honest` can only refute; `over` can prove because a list runs out — [lib/claim.disp:14-17](../../../lib/claim.disp); [research/CLAIMS_OVER_TREES.md:400-404](../../../research/CLAIMS_OVER_TREES.md).
- Quantifier classes: "For all trees and budgets, a test (Π1). Search refutes it ... but never proves it"; Π2 ("every input finishes") "Search can neither refute nor confirm" — [research/CLAIMS_OVER_TREES.md:113-131](../../../research/CLAIMS_OVER_TREES.md).
- The confirmer gives a looping recognizer "Open there, never a verdict" — [lib/confirm.disp:26-27](../../../lib/confirm.disp).
- Designed gate: "collapse it to 0/1 (`param_apply T e == Ok true`)" — [research/OPTIMIZER.typ:91-93](../../../research/OPTIMIZER.typ). M0 validates "by re-running the program's tests + `param_apply`. Translation validation — sound w.r.t. tests and types, not yet `~`" — [research/OPTIMIZER.typ:784-788](../../../research/OPTIMIZER.typ).
- `growth.cheaper` compares two fits by degree, then by "the smaller value at a size past both fits" (n = 64). "A fit is evidence, not a proof" — [lib/growth.disp:1-5, 48-51](../../../lib/growth.disp).
- "Total correctness needs a witness. Without a step bound or a measure, a certified claim is partial correctness only" — [research/LOB_AND_CURRY.md:412-413](../../../research/LOB_AND_CURRY.md).
- The repo's own recommendation: a budget "that returns accepted on timeout is a native_decide-class hole" — [research/CONSTRAINED_PROGRAM_SEARCH.md:203](../../../research/CONSTRAINED_PROGRAM_SEARCH.md).

### Inferences
- **The type level closes it; the harness does not.** disp is better than most RL proof harnesses here, because Open is a first-class value rather than a timeout exception. But `param_apply == Ok true`, the kernel's Bool-valued `Pi(...)`, and growth-fit "cheaper" are two-valued interfaces. Which one the optimizer gates on decides whether the hack is open.
- **Size pressure plus a bounded check is adversarial selection for late counterexamples.** Among programs passing the first N challenges of `Trees`, the smallest is not the correct one when a smaller program agrees on all small inputs. With unary `Nat`, a hand-written special case on a large constant costs linear size, so naive memorization is expensive. A *computed* threshold is cheap, though: "wrong when n > 2^2^2^2" is a small tree whose first failure no bounded search reaches. This is the program-size analogue of the optimizer's curse ([NATURE_OF_OPTIMIZERS.md:157](../../../research/NATURE_OF_OPTIMIZERS.md)). Smallest-first challenge order makes it *easier* for a minimizer to predict which inputs will be tested.
- **Partial correctness hack.** Without a step bound, a non-terminating program is never refuted by `partial_at` (`test partial_at(spin_nat, pair(1, 500)) = true`, [lib/theories/cost.test.disp:35](../../../lib/theories/cost.test.disp)). A diverging program is often the smallest "witness" of a partial-correctness claim. Requiring `total_at` with a stated bound closes this; the cost theory shows how; nothing enforces it.
- **Growth fits as a cost proxy are gameable:** a program polynomial on sizes 1..k and exponential beyond passes `fit`. `cheaper` then prefers it at n = 64 until a search hits a larger size.

### Gaps
- Whether the eventual optimizer gate would accept Open: OPTIMIZER predates the three-valued verdict layer and is written against `param_apply`. No reconciliation found.

---

## Q8. What the repo's own documents already say (Goodhart, MDL, library cost, self-play), and how far it is built

### Takeaway
The documents anticipate most of this: "Goodhart on steps", the two-part code and its degenerate optima, "never a scalar", keep scorers out of the loop, quantilize, freeze the trusted core, never score legibility. They do not discuss *size* gaming specifically: proof-length or tree-size metrics, tree vs DAG size, or vacuous claims under a compactness reward. All of it is design. The only implemented, related artifacts are three-valued verdicts, the honest search, the cost-bound claims, the memo-off step machine, growth fits and the pinned lie tests.

### Cited Findings
- GOALS: combine the 0/1 checker "with functions that measure performance or program size". The optimizer "Must be able to self-play, continually improve by generating score functions itself" — [GOALS.md:4, 16](../../../GOALS.md).
- NATURE_OF_OPTIMIZERS §7: "The GOALS.md self-play clause is the most Goodhart-prone item in the plan" (item 4); "Quantilize at readback" (item 3); "Never score legibility" (item 6); "Audit the checker like Lean audits its kernel" (item 7) — [research/NATURE_OF_OPTIMIZERS.md:191-203](../../../research/NATURE_OF_OPTIMIZERS.md). "two reward functions can only be unhackable if one of them is constant" — line 163.
- Abstraction note: idealizations such as a cost model "must never migrate into the trusted" base — [research/THE_NATURE_OF_ABSTRACTION.typ:63](../../../research/THE_NATURE_OF_ABSTRACTION.typ). The other relevant lines are 108, 211 and 282 (cited in Q6).
- FOUNDATIONS Q4: "interaction count is a space-biased proxy ... Falsifier: proxy-optimal diverges from hardware-optimal unboundedly" — [FOUNDATIONS.md:214](../../../FOUNDATIONS.md).
- README Status: "Designed but not built: the optimizer itself ..., cost as a typing-level resource, ... and the neural proposer" — [README.md:148](../../../README.md).
- LOB_AND_CURRY §6: "if optimization pressure could change the five rule lenses, it would be changing what counts as a certificate" — [research/LOB_AND_CURRY.md:401-405](../../../research/LOB_AND_CURRY.md).

### Inferences
- The missing pieces specific to *size* objectives are:
  - a stated size measure: closed tree vs DAG vs surface;
  - charging checker fuel alongside witness size;
  - non-vacuity tests for generated claims;
  - a rule that the gate is "Proved" and never "not Refuted";
  - treating "Assumed", `use raw` and non-probe-covered trusted slots as disqualifying.

### Gaps
- None of the docs I read proposes a proof-size or witness-size metric explicitly. "cost/size score" appears only in passing ([OPTIMIZER.typ:55](../../../research/OPTIMIZER.typ)).

---

## Q9. External prior art: tree calculus / combinatory logic program size, and canonical size measures

### Takeaway
Canonical size measures in combinatory and λ settings exist and are well studied: Tromp's BLC/BCL bit-lengths and Jay's in-calculus `size`. They are canonical only up to the invariance constant and the choice of compilation (bracket abstraction) scheme. Size-minimal combinators are found by exhaustive search for tiny objects. I found no prior work proposing a program-size measure designed specifically to resist notation tricks in a proof setting beyond "measure the kernel term", and no work on reward hacking against tree-calculus size.

### Cited Findings
- Tromp: binary encodings in which "information content may seem to be highly dependent on choice of programming language, the notion is actually invariant up to an additive constant". The BLC self-interpreter is 206 bits; the CL self-interpreter is 263 bits; "S S K (S (K (S S (S (S S K)))) K) of size 35, the smallest possible fixpoint combinator as verified by exhaustive search by computer" — [Tromp, *Functional Bits*, §3–4](https://tromp.github.io/cl/LC.pdf); original [Dagstuhl 2006](https://drops.dagstuhl.de/entities/document/10.4230/DagSemProc.06051.4).
- Abstraction-scheme dependence: λ1 vs λ2 gives 2030 vs 374 bits for the same combinator — [Tromp §3](https://tromp.github.io/cl/LC.pdf). Jay: bracket abstraction gives 5k−2; star abstraction fixes it — [Jay book Ch. 4](../../barry-jay/reflective-programs-in-tree-calculus-tree_book.pdf).
- Jay's `size` is a reflective tree-calculus program computing node count, including its own — [Jay book §5.2](../../barry-jay/reflective-programs-in-tree-calculus-tree_book.pdf). Secondary sources disagree on the value of `size size` (168 vs 508), probably because of differing encodings — [search summary: trlisp docs](https://blosavio.github.io/trlisp/tree-calculus.definitions.html), [coq discourse](https://coq.discourse.group/t/1080) (unverified).
- Jay also minimizes his own programs: of the branch-first evaluator, "Although this solution meets the specification, there are smaller solutions ... it is worth the effort to make it smaller" — Jay book, local text extraction line ~4074.
- Lean proof-length RL: ProofOptimizer measures Lean tokens and notes "these metrics may not always be correlated". Shorter proofs were often slower, and a heartbeat objective fixed that. Its only anti-gaming guard is a linter for no-op tactics plus SafeVerify statement matching — [Gu et al. 2025](https://arxiv.org/html/2510.15700v1).
- Metamath minimization could add axioms to shorten proofs, until qualifiers fixed it — [metamath list](https://go.mail-archive.com/metamath@googlegroups.com/msg00447.html).

### Inferences
- The closest prior "notation-resistant size" is Metamath's compressed proof (kernel-level steps) and Tromp's BLC/BCL. Both are the move disp's tree measure makes: measure the kernel object, not the text. Both still had to add *trust* qualifiers (Metamath's axiom restrictions) and *cost* axes (ProofOptimizer's heartbeats). That is evidence that a canonical size alone does not stop gaming; it moves gaming to trust and cost.

### Gaps
- I found no literature on reward hacking against combinator or tree-calculus size, no study of size-minimal tree-calculus programs beyond Jay's hand optimization, and no source defining a "canonical program-size measure resistant to notation tricks" as an explicit goal. Coverage is thin: a few targeted searches, not a survey.

---

## Q10. Bottom line: Lean exploit classes vs. disp

### Takeaway
disp closes notation/naming games structurally and makes environment-citation games visible by inlining. It mitigates sorry/axiom injection, `native_decide` and Open-as-Proved at the type level. It does nothing about spec gaming, reward brittleness or proof-work-in-the-checker, and it worsens two things: reflection-based checker sniffing, and trusted slots inside user-definable types.

| Lean exploit class | disp status | Reason (evidence) |
|---|---|---|
| Source/token length games (names, notation, comments, long identifiers) | **Closes structurally**, if size is measured on the elaborated tree | Names and fields never reach the tree (SYNTAX.typ:150); the tree is the object |
| Automation hiding work (`simp`, `omega`, `decide`, `interval_cases`; short but slow) | **Doesn't address; arguably the default** | Proved has no proof object; `refl` and bounded symbolic search do the work (kernel.test.disp:713-745; rules.disp:174-189). Budgets cap the work, but fuel spent is not reported or charged. Compile-time evaluation hides work from source-length metrics (cir.ts:118, 400M steps) |
| Proof by reflection / `native_decide` (trusting compiled evaluation) | **Mitigates** | Natives keyed to in-language definitions, no-native conformance mode, naive "honesty backend", five-backend differential (eager.ts:25-33; EVALUATOR.md:38). Still trusted at run time; differential covers fixtures, not every scored verdict |
| `csimp` / implementation-swap hooks | **Makes worse (new surface)** | Types carry trusted `ends`/`check`/`norm`/`gate`/`members` slots, policed by 8 probes (types.disp:68-80; kernel.disp:950). A lying `ends` "launders a false equation into the ledger" (kernel.test.disp:989-991) |
| Axioms / `sorry` / environment additions | **Mitigates** | `checked.disp` rejects Open by default and labels "Assumed" (checked.disp:33-61). Inlining means library code is in the measured tree. Residual: `judging.lemmas` is trusted input; `use raw` skips verification; guard rebinding |
| Library/environment additions as free size (MDL) | **Closes on unshared closed-tree size; open on DAG or marginal size** | References inline (expr.ts:97). Session-global hash-cons makes marginal DAG size state-dependent (tree.ts:45-90). MDL accounting is design only (ABSTRACTION.typ:98-108, 211) |
| Trust-base bugs (Lean kernel/`apply?` sorry leak) | **Doesn't address better than Lean; differently exposed** | The five rule lenses are "not proved sound" (README.md:130-133); the K-discard termination domain is load-bearing (EVALUATOR.md:64-71). Small and readable, so auditable, but there is no independent re-checker like lean4checker |
| Reflection / checker sniffing | **Makes worse (new class); partly mitigated** | Programs can inspect hypotheses. `Guard` accepts pinned lies (`sniff_cod`, `lying_cod`, kernel.test.disp:579-590). The confirmer escapes forgeries and splits on inspection (confirm.disp:122-127, 370-378). TwoFace is "not a proof" |
| Spec gaming (vacuous/weak statements, misformalization) | **Doesn't address** | A claim is any Bool program; `{_,_} => true` is the cheapest Proved claim. No non-vacuity or statement-structure check is implemented; the honest search could serve as an exploit model but is not used as one |
| Open treated as success (timeout = accept) | **Closes at the type level; harness-dependent** | Open is a distinct verdict (verdict.disp:3-5; README.md:78-84). The designed 0/1 gate (OPTIMIZER.typ:91-93), growth fits and Tier-1 rules are "evidence, not proof". Size pressure selects for late counterexamples |
| Cost-metric gaming (memo, units) | **Mitigated in design, open in the implemented counter** | ic-net "honest cost" is designed, its readouts not built. Eager `ApplyStats.steps` skips memo hits (tree.ts:652-656). The machine's memo-off steps are honest but charge nothing for construction (machine.disp:4-6) |
| Brittleness (budget, backend, elaborator version) | **Mixed** | Budget increase is monotone-safe (rules.disp:174-176). Bracket abstraction is definitional equality, so changing the elaborator changes every tree and every size (cir.ts:1-5). Termination domain is backend-sensitive (EVALUATOR.md:64-71) |

### Inferences
- The most honest one-sentence answer to "does disp fix this?": **disp fixes the measure, not the incentive.** It gives a canonical, notation-free object to measure: the closed tree, whose DAG version gives a natural two-part code. It also gives an honest three-valued verdict. But proofs in disp have no size of their own: all proof work sits in checker search, `refl` and trusted type slots, so a size objective pushes pressure into exactly those places. Pressure flows to the parts a size metric cannot see.
- Cheapest concrete safeguards that the existing machinery already supports:
  1. Define size as the DAG node count of the closed, elaborated tree, computed in a fresh hash-cons (state-free).
  2. Report and charge checker fuel (return the remaining fuel from `judge_with`) as a second Pareto axis.
  3. Gate on `== "Proved"` only, under a kernel tier that is not plain `Guard`.
  4. Reject any verdict resting on "Assumed", `use raw`-loaded definitions, or types whose trusted slots are not covered beyond `probes`.
  5. Run `by_search` with fixed adversarial witnesses as an exploit model for generated claims.
  6. Never count ApplyStats.steps from a memoizing backend.

### Gaps
- All classifications of the optimizer path rest on design documents. No size-minimizing loop has been run against disp, so none of these hacks has been observed in practice; they are inferred from code and pinned tests.
