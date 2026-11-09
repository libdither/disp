# Prior work for disp's lens type theory: has anyone combined these pieces?

Research note, 2026-11-08. A literature survey run by a research agent; every link below
was fetch-verified with `scripts/verify-source.sh` (two were spot-checked again by hand).
Links that would not verify are listed in §4 and cited nowhere else.

The system surveyed is the type checker in `lib/claim.disp`, `lib/stream.disp`,
`lib/confirm.disp` and `lib/rules.disp`, described in `TYPES.html`. Its seven pieces, which
the rest of this note refers to by number:

1. **Substrate:** tree calculus (Barry Jay). Programs, data, types and proofs are all trees,
   and a program can inspect another program (reflection).
2. **A type is a recognizer:** a program `Tree -> Bool`, refined to partial equivalence
   relations (PERs: a type that also says when two members count as the same) so function
   types are extensional. "f : Nat -> Nat" is a claim over every input.
3. **Ground truth is refutation by brute force.** A claim is a program
   `{witness, challenge} => Bool`. Enumerate every tree smallest-first as a stream, keep the
   failing challenges, and read the verdict off how that stream ends: an item appears gives
   Refuted (a counterexample that re-runs); the source runs out gives Proved (finite domains
   only); the budget runs out gives Open.
4. **Shortcut:** symbolic execution of the ordinary evaluator on partly known trees with
   typed placeholders. The run splits a placeholder only when it inspects it (lazy
   narrowing). Case tables are derived by running the type's recognizer symbolically.
   Recursion uses an induction hypothesis that may only be applied to strictly smaller
   pieces.
5. **Shortcuts are justified as lenses between claims.** A claim is a game of witness
   against challenge; a lens has a forward map on witnesses and a backward map on
   challenges (Dialectica-like). The lens law is itself a claim, so the brute-force refuter
   can refute it but never prove it. A few atom laws are proved by hand.
6. **Higher-order inputs:** a function-typed input is a placeholder that can only be
   applied. A program that inspects a function argument is refuted with a stored pair of
   extensionally equal but distinct functions (`id`, `shift`). For `A => B` claims the
   witness is itself a lens (Dialectica).
7. **One verdict rule:** the symbolic search is a work-list stream (`s_worklist`), read by
   the same verdict rule as the brute force in 3.


## 1. Short answer

Nobody has put all seven together in anything the survey found. It checked about 35 sources, plus an extended search for testing combined with Dialectica or lenses, which found nothing. Most systems cover three or four of the seven features. Three pieces look unclaimed:

- **Reflection makes extensionality a real obligation (1+2+6).** On a substrate where programs can take functions apart, "f : Nat → Nat" becomes a claim that can fail. disp refutes it with a stored pair of equal-but-distinct functions (id, shift). Every other tool runs on a host language where functions are opaque, so the question never comes up for them.
- **Shortcuts justified as Dialectica lenses (5).** Their laws are claims that the same brute-force search can refute. No testing tool frames checkers this way.
- **One verdict reader for two streams (3+7).** Both the brute-force stream and the symbolic work-list are read by the same rule.

The closest conceptual ancestor is [Dybjer 2012](https://www.cse.chalmers.se/~peterd/papers/MartinLofFestschrift.pdf#page=1). It reads Martin-Löf type theory as a testing manual: judgements are "conjectures which can be tested in order to be corroborated or refuted". Unknowns are instantiated only "when it blocks further computation" ([p.17](https://www.cse.chalmers.se/~peterd/papers/MartinLofFestschrift.pdf#page=17)). Function inputs are built on demand as input–output pairs, with game semantics as the stated model ([p.18](https://www.cse.chalmers.se/~peterd/papers/MartinLofFestschrift.pdf#page=18)). But it is only a sketch. It never proves anything, and it gives meaning to equality only when it is decidable ([p.2](https://www.cse.chalmers.se/~peterd/papers/MartinLofFestschrift.pdf#page=2)). So equality at N → N is left out.

- **Closest working tool:** Nguyen, Tobin-Hochstadt and Van Horn's higher-order symbolic execution. It covers 2 (partly), 3, 4 and 6.
- **Closest on brute force plus symbolic refinement (3+4):** Lazy SmallCheck and its 2012 successor.
- **Closest on lenses (5):** the Dialectica and open-games literature, which does no testing at all.

## 2. The closest works

| Work | Shares | Lacks compared with disp |
|---|---|---|
| **Dybjer 2012**, testing reading of type theory ([p.1](https://www.cse.chalmers.se/~peterd/papers/MartinLofFestschrift.pdf#page=1), [p.17](https://www.cse.chalmers.se/~peterd/papers/MartinLofFestschrift.pdf#page=17), [p.18](https://www.cse.chalmers.se/~peterd/papers/MartinLofFestschrift.pdf#page=18)). Companion: [Dybjer, Qiao, Takeyama](https://www.cse.chalmers.se/~peterd/papers/Dallas.pdf#page=1), testing inside the Agda/Alfa proof assistant | 2, 3, 4 (lazy instantiation), 6 (function inputs as on-demand input–output pairs) | 1. No sameness at function types (decidable equality only). No Proved or Open, so no symbolic proof or induction. No 5 or 7. Not implemented for function inputs. |
| **Lazy SmallCheck 1 and 2** (Runciman, Naylor, Lindblad 2008; Reich, Naylor, Runciman 2012), via [Reich's thesis](https://etheses.whiterose.ac.uk/id/eprint/5650/1/Jason%20S%20Reich%20-%20Thesis.pdf#page=35): "exactly that component is refined" | 3 (bounded, smallest-first, concrete counterexample). 4 (lazy narrowing, i.e. splitting an unknown only when evaluation needs it). 6 partly: [first-order function values](https://etheses.whiterose.ac.uk/id/eprint/5650/1/Jason%20S%20Reich%20-%20Thesis.pdf#page=39), stored as tries (lookup trees) that either [ignore their argument or case on it](https://etheses.whiterose.ac.uk/id/eprint/5650/1/Jason%20S%20Reich%20-%20Thesis.pdf#page=51) | 1. 2: types are Haskell types, not recognizers or PERs (partial equivalence relations), and functions can't be inspected, so extensionality is never at stake. No proofs past the depth bound and no induction. No 5. No 7: it prints "OK, required N tests at depth D", with no Proved vs Open split. |
| **Isabelle Quickcheck narrowing** ([Bulwahn's thesis](https://mediatum.ub.tum.de/doc/1115870/1115870.pdf#page=67): "partially instantiated terms … refine these terms as needed") and **Nitpick** ([three-valued logic, genuine vs potential counterexamples](https://web.archive.org/web/20220701070821id_/https://www21.in.tum.de/~nipkow/pubs/itp10.pdf#page=7)) | 3 and 4, inside a proof assistant. Nitpick's genuine/potential split matches disp's rule of re-running before reporting. | 1, PERs, 5, 7. 6: functions are finite approximations. Proofs come from Isabelle, not from the tester. |
| **Nguyen, Tobin-Hochstadt, Van Horn**, JFP 2017 ([p.1](https://arxiv.org/pdf/1507.04817#page=1): counterexamples "may involve solving for an unknown function"; [p.9](https://arxiv.org/pdf/1507.04817#page=9): "only how the unknown function interacts with its context" matters) | 2 partly (contracts, i.e. runtime-checked predicates, in untyped Racket). 3 (it verifies or refutes). 4 (unknowns are refined as they pass predicates, and the evaluator forks). 6 (opaque unknown functions). | 1: Racket closures are opaque. No relational or PER types. Base-type questions go to an off-the-shelf solver instead of brute force. No 5 or 7. |
| **ACL2s counterexample generation** ([p.1](https://arxiv.org/pdf/1105.4394#page=1): untyped, "all variables can take on any value"; [p.2](https://arxiv.org/pdf/1105.4394#page=2): turning "subgoal counterexamples … into counterexamples for the original conjecture") | 1 partly (one untyped universe, code is data, first-order only). 2 ("type" hypotheses are ordinary predicates). 3. 4 through the prover. 5 partly: counterexamples are carried back through proof steps. | 6. PERs. The backward map is never checked as a claim of its own. 7. |
| **Korat** ([p.1](https://web.cs.ucla.edu/~harryxu/research/papers/korat.pdf#page=1): fields the predicate never reads can't affect validity) and **lazy initialization** ([Khurshid, Păsăreanu, Visser p.5](https://users.ece.utexas.edu/~khurshid/testera/GSE.pdf#page=5): fields set "when they are first accessed") | 4: cases come from running the recognizer and watching what it reads, and an unknown splits on first touch. 3, bounded. | Proofs past the bound. 1, PERs, 5, 6, 7. |
| **Leon** ([Suter, Köksal, Kuncak](https://infoscience.epfl.ch/record/161285/files/OnSatisfiabilityModuloComputableFunctions_1.pdf#page=1): re-runs candidate counterexamples as code; "sound for proofs and counterexamples") | 3 (both verdicts, symbolic "No" re-run concretely). 4 (recursive definitions unfolded on demand, simple inductive proofs). | 1, 2, 5, 6, 7. |
| **DART** ([p.4](https://css.csail.mit.edu/6.566/2022/readings/dart.pdf#page=4): found a bug / ended with all paths covered / "will run forever") and **negation as failure** ([Clark p.2](https://www.doc.ic.ac.uk/~klc/NegAsFailure.pdf#page=2): "the exhaustive but unsuccessful search for a proof") | The verdict rule of 3 and 7, exactly: Refuted / Proved-by-running-out / Open. | Everything to do with types and higher-order inputs. |
| **Dialectica, lenses and open games**: [de Paiva](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-213.pdf#page=5); [Avigad & Feferman p.10](https://math.stanford.edu/~feferman/papers/dialectica.pdf#page=10) ("from a counterexample v to the conclusion … a counterexample y to the hypothesis"); [Pédrot](https://www.pédrot.fr/articles/lics2014.pdf#page=2) (counters as "opponents … or stacks"); [Ghani, Hedges et al.](https://arxiv.org/pdf/1603.04641#page=3) (open games "equivalent to polymorphic lenses") | 5. 6: an A ⇒ B witness is a function paired with a map that pulls counterexamples back. | Any search or refuter. Never used as the soundness format for a checker. |
| **Relational testing**: [Hriţcu et al. on noninterference](https://arxiv.org/pdf/1409.0393#page=11) (generate "all pairs of indistinguishable states"); [RelSym](https://arxiv.org/pdf/1711.08349#page=1) ("refutation, as well as proving of relational properties") | 2 (claims about pairs of runs on related inputs, the same shape as PER membership). 3. 4 (RelSym's symbolic execution). | 1, 5, 6, 7. |

Also relevant, each sharing one piece:
- [HipSpec](https://www.cse.chalmers.se/~jomoa/papers/hipspec-cade.pdf#page=1): testing plus induction (3, 4).
- [Target](https://arxiv.org/pdf/1410.5370#page=16): function inputs built at call time (6).
- [QuickChick's derived checkers](https://par.nsf.gov/servlets/purl/10407308#page=3), which return "a boolean option", i.e. three values (3).
- [CurryCheck](https://cpm.curry-lang.org/DOC/currycheck-4.0.0/main.pdf#page=5): "one can even prove a property if the number of test cases is" finite. It enumerates every type "except for function types" (3).
- [Fischer's thesis on narrowing in Curry](https://macau.uni-kiel.de/servlets/MCRFileNodeServlet/dissertation_derivate_00003319/diss_fischer.pdf#page=36) (4).
- [Beginner's Luck](https://arxiv.org/pdf/1607.05443#page=4) (2+4; already cited in the repo).
- [Jay & Bader](https://arxiv.org/html/2604.12194v1#S12.SS4.p1.1.2): tree calculus can "decide equality of programs" (1 only).
- [Abramsky, Jagadeesan, Malacaria](https://arxiv.org/pdf/1311.6125#page=5): game semantics where questions are requests for data (6).

## 3. Ideas worth stealing

1. **Lazily drawn function tables (feature 6, and the refuter in 3).** Dybjer, Target and Lazy SmallCheck 2 all build a function input only when the run applies it. Each new argument gets its output drawn from the codomain's generator, and the answer is remembered. Listing these partial tables smallest-first would give the brute-force refuter a real stream at function-typed domains. TYPES.html §5 says it can currently only draw on functions already certified. A failing table becomes a concrete tree (a case program) and is re-run, so the rule that a symbolic "No" is only a suggestion still holds.
2. **Compute the backward maps instead of writing them (5).** [Kerjean & Pédrot](https://wollic2022.github.io/mariekerjean.pdf#page=1) show "the Dialectica translation is in fact a reverse differentiation algorithm". So the F in `implies A B` could be a reverse pass over the run's record of where it applied its argument. That is the "origins" step CLAIMS_OVER_TREES §9 names as next. ACL2s is a working example: each proof step records what it eliminated, so counterexamples translate back.
3. **Fair interleaving in the work-list (7).** [LogicT](https://okmij.org/ftp/papers/LogicT.pdf#page=1) shows fair search is easy when backtracking is built on streams. `s_worklist` currently puts the cases of each split at the front, so it searches depth-first with a depth cap. Interleaving fairly would reach every counterexample eventually. "Split depth exhausted" would then become an ordinary budget Open.
4. **State verdict soundness in DART's three-clause form (3, 7).** (a) Refuted means a real input exists. (b) Running out means no counterexample, with every case covered. (c) Otherwise the search runs forever. Clark adds the caveat that disp already has: proof by exhaustion holds only when the source is the type's own generator.
5. **One derivation, three products, proved once (2, 4, 5).** QuickChick derives a checker (with fuel), an enumerator and a generator from one relation, and proves the derivation scheme sound in Coq. disp could get case tables, generator and checker from one recognizer, with one hand-proved law per scheme rather than per type.
6. **Related pairs by variation (2).** For sameness relations with no normal-form function (function types, weak streams), `quot_pairs` has nothing to pair each value with. The noninterference recipe fills the gap: generate one state, vary it, and keep every related pair reachable.
7. **Equality by comparing residual programs (4).** [Klyuchnikov & Romanenko](https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/hosc/psi09.pdf#page=25) prove two terms equivalent by supercompiling both (a transformation that evaluates symbolically and folds repeated states) and comparing the results syntactically. That is CLAIMS §4's "one hash-cons comparison". Their folding is a more general induction than "strictly smaller piece". How they make it terminate was not checked.
8. **Polymorphic laws and rule laws (TYPES §11, feature 5).**
   - [Bernardy, Jansson, Claessen](https://web.archive.org/web/20151022082353id_/http://www.cse.chalmers.se/~bernardy/PolyTest.pdf#page=1) show one monomorphic instance can stand for all instances. It relies on parametricity, which disp's reflection breaks, so it only applies inside the certified non-inspecting sub-universe.
   - [Roberson et al.](https://web.archive.org/web/20240416221307id_/https://web.eecs.umich.edu/~bchandra/publications/oopsla08.pdf#page=1) check type soundness by bounded enumeration. The same could run as a standing job that tries to refute the hand-proved rule laws.

## 4. Links that failed verification

- **Lazy SmallCheck 2008 paper:** the Tufts PDF and the ACM DOI 10.1145/1411286.1411292 both returned 403, so Reich's thesis is cited for it instead.
- **Klyuchnikov & Romanenko paper:** the typeset.io/scispace copy returned HTML with no text, so the authors' slides are cited instead.
- **Bernardy draft:** the live URL is a 404, so the Wayback copy is cited.
- **Nitpick and Roberson:** the live pages timed out, so Wayback copies are cited.
- **NuPRL book page:** it fetched, but has no passage about PERs, so it is left out. FOUNDATIONS.md already covers NuPRL.
- **Not fetched (publisher-only or skipped for budget):** Dybjer, Qiao, Takeyama, TPHOLs 2003 itself (only the companion paper was verified); "Advances in Lazy SmallCheck" as a standalone paper; Bulwahn, CPP 2012; Kerjean & Pédrot, LICS 2024 full paper; Zeno; KLEE; Antoy, Echahed, Hanus on needed narrowing; EasyCheck (FLOPS 2008); Hyland & Ong; Claessen's "Shrinking and showing functions".
