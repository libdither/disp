# Theory of Simplicity Measures for Proofs and Programs, and Their Robustness to Optimization Pressure

Notes compiled 2026-11-09. Tags: **[V]** = checked against the linked source during this session. **[M]** = from memory or background knowledge, not re-checked this session (the link is the canonical place to check). The fetch tool limited direct quotes to about 125 characters, so long passages are paraphrased.

---

## Q1. Hilbert's 24th problem and formal proposals for proof simplicity and proof identity

### Takeaway
Hilbert asked for "criteria of simplicity" of proofs and expected "only one simplest proof." Proof theory never produced a length-based answer. The serious modern programme recasts the question as **proof identity**: when are two proofs "the same"? It requires identity to be polynomial-time decidable and **to respect proof size up to a polynomial factor**, because the standard normalization identity (cut elimination) can make proofs non-elementarily larger. Length and "sameness or explanatoriness" therefore diverge structurally. The lemmas (cuts) that make a proof short are exactly what normalization deletes, and normalization is the classical criterion for two proofs being the same.

### Cited Findings
- **[V]** Hilbert's notebook note, as translated by Straßburger: "Find criteria of simplicity or rather prove the greatest simplicity of given proofs. More generally develop a theory of proof methods in mathematics. Under given conditions there can be only one simplest proof. And if one has 2 proofs for a given theorem, then one must not rest before one has reduced one to the other or discovered which different premises (and auxiliary means) have been used in the proofs: When one has two routes then one must not just go these routes or find new routes, but the whole area lying between these two routes must be investigated…" — [Straßburger 2019, Phil. Trans. R. Soc. A 377, §1(a), p.1–2 (PDF)](https://www.lix.polytechnique.fr/Labo/Lutz.Strassburger/papers/hilbert24.pdf)
- **[V]** Wikipedia's rendering of the opening reads "Criteria of simplicity, or proof of the greatest simplicity of certain proofs." It also quotes "The use or the knowledge of a syzygy simplifies in an essential way a proof that a certain identity is true." Hilbert's syzygy example is literally about an auxiliary object (a lemma) making a proof simpler. The page notes the manuscript has misspellings and hard-to-read passages, which suggests haste. — [Wikipedia: Hilbert's twenty-fourth problem](https://en.wikipedia.org/wiki/Hilbert%27s_twenty-fourth_problem)
- **[V]** Discovery: the German historian Rüdiger Thiele found the problem in Hilbert's notebooks in 2000. It is not among the 23 problems or in any published Hilbert text. Thiele published it as "Hilbert's Twenty-Fourth Problem," *Amer. Math. Monthly* 110(1), Jan 2003, pp. 1–24, [doi:10.1080/00029890.2003.11919933](https://doi.org/10.1080/00029890.2003.11919933) ([MAA PDF](https://maa.org/sites/default/files/pdf/upload_library/22/Ford/Thiele1-24.pdf)). Thiele & Wos, "Hilbert's Twenty-Fourth Problem," *J. Automated Reasoning* 29(1):67–89, 2002, [doi:10.1023/A:1020537107897](https://doi.org/10.1023/A%3A1020537107897), relates it to automated reasoning. — [Wikipedia](https://en.wikipedia.org/wiki/Hilbert%27s_twenty-fourth_problem)
- **[M]** Wos's operational notion of simplicity in automated reasoning (OTTER/Prover9 work) is mostly **proof length (number of deduction steps)**, plus secondary criteria: fewer distinct variables, smaller maximum term size, and avoiding particular lemmas or "double-negation-free" proofs. Wos spent years searching for shorter proofs of known axiom systems, for example in equivalential calculus and Łukasiewicz-style systems. In the Thiele & Wos paper, "simplicity" is multi-criteria rather than length alone. — [Thiele & Wos 2002](https://doi.org/10.1023/A%3A1020537107897)
- **[V]** Straßburger says the problem "is still a mystery." There are only four notions of proof identity, two of them trivial ("same theorem", "syntactically equal"). The two non-trivial ones are **normalization** (Prawitz: same normal form) and **generality** (Lambek: same morphism in a free category), following [Došen 2003, "Identity of proofs based on normalization and generality," Bull. Symb. Logic 9]. — [Straßburger 2019, §2(a)](https://www.lix.polytechnique.fr/Labo/Lutz.Strassburger/papers/hilbert24.pdf)
- **[V]** On why normalization conflicts with simplicity, Straßburger writes that cuts "can be seen as auxiliary lemmas used in that proof. Therefore, these cuts are crucial information and should not be eliminated." His example: eliminating cuts from Fürstenberg's topological proof of the infinitude of primes yields **Euclid's proof** [BHL+08]. — [Straßburger 2019, §2(a)](https://www.lix.polytechnique.fr/Labo/Lutz.Strassburger/papers/hilbert24.pdf)
- **[V]** On proof size, Straßburger writes: "the transformation of a proof with cuts into a proof without cuts can cause an explosion of the size of the proof. One can construct examples of proofs with cuts that fit on a page, such that eliminating the cuts makes them bigger than the size of the universe [Boolos 1984, 'Don't eliminate cut', J. Phil. Logic 13:373–378]. Because of this, the cut is also called a mechanism of proof compression." Other compression mechanisms are **extension and substitution** (Cook–Reckhow 1979) and **co-contraction**, which shares subproofs. — [Straßburger 2019, §2(b)](https://www.lix.polytechnique.fr/Labo/Lutz.Strassburger/papers/hilbert24.pdf)
- **[V]** Straßburger's four objectives for proof identity:
  1. "Proof identity must be decidable in polynomial time in the size of the proofs."
  2. "Proof identity must respect the size of proofs" (identified proofs differ in size by at most a polynomial factor).
  3. "must be able to handle partial proofs."
  4. "must be independent from the syntax of proof systems."
  — [Straßburger 2019, §2](https://www.lix.polytechnique.fr/Labo/Lutz.Strassburger/papers/hilbert24.pdf)
- **[V]** In classical logic, naively adding involutive negation to a Cartesian closed category produces "a collapse into a Boolean algebra, i.e., any two proofs f, g : A → B are identified" (Lambek–Scott 1986; Straßburger 2011). For intuitionistic logic (CCCs) and linear logic (star-autonomous categories), normalization and coherence give the same identifications. For classical logic neither has a broadly accepted definition. — [Straßburger 2019](https://www.lix.polytechnique.fr/Labo/Lutz.Strassburger/papers/hilbert24.pdf)
- **[V]** Andrews' matings keep only the axiom links. But checking a mating's correctness is coNP-complete, "as expensive as proving the formula from scratch." Girard's proof nets give polynomial-time correctness checking for multiplicative linear logic. Atomic flows (Guglielmi–Gundersen) have no known polynomial checker, and one likely cannot exist [Das 2013]. **Hughes' combinatorial proofs** ("Proofs Without Syntax," *Annals of Math.* 164(3), 2006) separate the linear part from the duplication/erasure part. For classical propositional logic they "meet all four objectives." Correctness is checkable in polynomial time, the system is sound and complete, and translations from sequent calculus, tableaux and **deep inference** proofs preserve size. Proposed criterion: "Two proofs are the same iff they have identical combinatorial proofs." — [Straßburger 2019, §3](https://www.lix.polytechnique.fr/Labo/Lutz.Strassburger/papers/hilbert24.pdf)
- **[V]** Straßburger's earlier tutorial "Proof Nets and the Identity of Proofs" is INRIA RR-6013 / ESSLLI'06 lecture notes. — [arXiv cs/0610123](https://arxiv.org/abs/cs/0610123)
- **[V]** Straßburger reformulates Hilbert's question for programs: "What is the simplest algorithm for a given problem," noting "we have no idea of what it means for two algorithms to be the same." — [Straßburger 2019, §1](https://www.lix.polytechnique.fr/Labo/Lutz.Strassburger/papers/hilbert24.pdf)
- **[M]** Deep inference (Guglielmi; the calculus of structures; Brünnler–Tiu's local system SKS) allows rules to apply deep inside formulas. Bruscoli & Guglielmi (ACM TOCL 2009, "On the proof complexity of deep inference") showed deep-inference systems can have quasipolynomial cut elimination (via atomic flows), versus exponential or non-elementary blowup in the sequent calculus. Their analytic deep-inference systems p-simulate Frege. — [Straßburger 2019 refs BG09, GG08](https://www.lix.polytechnique.fr/Labo/Lutz.Strassburger/papers/hilbert24.pdf)

### Inferences
- Hilbert's two demands pull apart under optimization. "Only one simplest proof" invites a length minimizer. "Reduce one to the other, or find which premises and auxiliary means were used" is an identity or explanation question. A minimizer of raw length will favour cut-heavy, lemma-dense, compressed proofs. Normalization-based identity counts those as "the same" as huge cut-free proofs, so length is not invariant under the field's own canonical notion of sameness.
- Objective 2 (identity respects size up to a polynomial) is the closest thing in the literature to a robustness requirement. A simplicity measure that changes super-polynomially between proofs deemed "the same" is measuring presentation, not the proof. It is therefore gameable by changing presentation, for example adding cuts, extension variables or sharing.
- Matings show a second failure mode: a canonical object can be so compressed that checking it costs as much as finding the proof. A length-minimizer can push work into the checker this way.

### Gaps
- I could not fetch Thiele's 2003 Monthly article (archive.org was blocked), so I have no page-anchored Thiele quotes. Thiele's commentary on what "simplicity" meant to Hilbert, including his discussion of Hilbert's syzygy example, needs checking in the [MAA PDF](https://maa.org/sites/default/files/pdf/upload_library/22/Ford/Thiele1-24.pdf), pp. 1–24.
- I did not open the Thiele & Wos JAR paper. Its exact list of Wos's simplicity criteria is from memory.

---

## Q2. Kolmogorov complexity and MDL: reference-machine dependence, libraries, incomputability, and resource-bounded variants

### Takeaway
Description length is defined only up to an additive constant that depends on the reference machine. For any single finite object, that constant can be large enough to make the object's complexity essentially zero. "Simplicity relative to a library" is therefore meaningless unless the library is charged for, as in a two-part MDL code. Plain K is also incomputable, and an optimizer can drive it down by moving work into running time. Levin's Kt (length + log time), Schmidhuber's speed prior and Bennett's logical depth add a time term. That blocks "make the evaluator or checker do the work" hacks, at the price of a log-time exchange rate that still tolerates exponential compute per saved bit.

### Cited Findings
- **[M]** Invariance theorem (Kolmogorov, Solomonoff, Chaitin): for universal machines U and V there is a constant c_{U,V}, independent of x, with K_U(x) ≤ K_V(x) + c_{U,V}. c_{U,V} is roughly the length of a V-interpreter written for U. The constant is uniform in x but **unbounded over the choice of U**. For any fixed finite x there is a universal U' with K_{U'}(x) = O(1), for example a machine that prints x on input "0" and otherwise simulates U. — [Li & Vitányi, *An Introduction to Kolmogorov Complexity and Its Applications*, 4th ed. 2019, §2.1 (Thm 2.1.1)](https://link.springer.com/book/10.1007/978-3-030-11298-1)
- **[M]** K is incomputable, but upper semi-computable (it can be approximated from above by dovetailing). No computable function is a non-trivial lower bound on K infinitely often (Chaitin incompleteness). In consequence, **no verifier can certify that a short program or proof is the shortest**, beyond a constant depending on the verifier's own strength. — [Li & Vitányi §2.3, §3.9 (M)](https://link.springer.com/book/10.1007/978-3-030-11298-1)
- **[M]** Two-part MDL (Rissanen 1978; Grünwald 2007): choose H to minimize L(H) + L(D|H). The **model cost L(H) must be charged**, otherwise the maximal-likelihood "model that memorizes D" wins. For proofs, a library of lemmas or definitions is the model H and a proof using them is D|H. A per-theorem proof length that ignores library size is the one-part degenerate case. — [Grünwald, *The Minimum Description Length Principle*, MIT Press 2007 (M)](https://mitpress.mit.edu/9780262072816/the-minimum-description-length-principle/)
- **[V]** Levin complexity: Kt(x) = min_q { l(q) + log t(q, x) }, minimized over programs q that output x. Levin's universal search enumerates programs in this order, "an exponential trade-off between program length and runtime." — [Schmidhuber 2002, "The Speed Prior" (COLT 2002) (PDF)](https://www.Gwern.net/doc/reinforcement-learning/model/2002-schmidhuber.pdf); [Schaul & Schmidhuber, "Towards Practical Universal Search" (PDF)](https://schaul.site44.com/publications/frontier.pdf)
- **[V]** Speed prior: "the cumulative a priori probability of all data whose computation through an optimal algorithm requires more than O(n) resources is 1/n." Predictions reduce to minimizing the Levin complexity of (x, y). — [Schmidhuber, IDSIA-20-00, arXiv quant-ph/0011122](https://arxiv.org/abs/quant-ph/0011122)
- **[V]** Filan, Hutter & Leike (ALT 2016) showed Schmidhuber's speed prior is computable in doubly-exponential time but not polynomial time, and proposed a variant with better loss bounds. — [Loss Bounds and Time Complexity for Speed Priors, arXiv 1604.03343](https://arxiv.org/abs/1604.03343)
- **[M]** Bennett's logical depth: the running time of the fastest program within s bits of the shortest. It separates "simple-to-describe but expensive-to-generate" objects (deep) from random or trivial ones (shallow). Depth is robust only under a significance parameter s, and it is machine-dependent up to polynomial factors in time. — [Bennett 1988, "Logical Depth and Physical Complexity," in *The Universal Turing Machine: A Half-Century Survey* (M)](https://doi.org/10.1007/978-3-7091-6597-3_8)
- **[M]** Resource-bounded Kolmogorov complexity K^t(x) (shortest program producing x within time t(|x|)) is invariant only up to constant length and polynomial or logarithmic time overhead. The simulating machine must stay within the time bound, so the invariance theorem holds as K^{t'}_U(x) ≤ K^t_V(x) + c with t' = O(t log t). — [Li & Vitányi Ch. 7 (M)](https://link.springer.com/book/10.1007/978-3-030-11298-1)
- **[V]** Concrete reference machines, from Tromp's page: a **BLC self-interpreter of 162 bits** (record improved from 170 to 168 to 162; the last by a9lim "with AI help") and a **BLC universal machine of 184 bits (23 bytes)**. Tromp's symmetry-of-information constant dropped from 1876 to 1636 to 1388 bits (March 2009), then to **667 bits** (Felgenhauer, Sept 2011). He found the first 4 bits of BLC's halting probability Ω, which are .0001. Tromp says BLC is "a lot more descriptive, i.e. generally needing fewer bits" than his earlier binary combinatory logic machine, and that BLC was motivated by wanting a concrete definition of Kolmogorov complexity. — [Tromp, "Binary Lambda Calculus and Combinatory Logic" page](https://tromp.github.io/cl/cl.html)
- **[M]** Tromp's paper also gives concrete inequalities such as K(x) ≤ ℓ(x) + O(1) and KP(x) ≤ 2ℓ(x) + O(1) with explicit small constants. I recall a binary combinatory logic (S, K) universal machine of a few hundred bits, but did not verify the figure. The paper is published in [Kolmogorov Complexity and Applications / Randomness and Complexity, from Leibniz to Chaitin (2007)](https://tromp.github.io/cl/LC.pdf).
- **[V/secondary]** Barry Jay's **tree calculus** has a single operator Δ with three reduction rules, branching on whether the argument is a leaf, stem or fork. It is reflective: programs can compute the size of programs, test equality of programs, and self-interpret. The book *Reflective Programs in Tree Calculus* (2021) has proofs verified in Coq. A community demo reports `size size` = **168 nodes**. This figure comes from a secondary site and is **not verified against the book**. — [tree calculus spec site](https://atlas-4vy.pages.dev/specification); [Jay's blog/book repo](https://github.com/barry-jay-personal/blog)

### Inferences
- The invariance constant is the formal version of "the library or reference machine can absorb any finite proof." Two-part codes (charge for the library) and fixing a small, public reference machine (BLC's 162–184-bit interpreters, tree calculus) are the only principled defences. Both turn the free parameter into a priced quantity.
- Kt-style penalties defeat the "make the checker do the work" hack only partially. Each saved bit of description buys a factor of 2 in allowed running time. Proof by reflection or `decide`-style tactics that trade a long proof for a 2^k-step computation are break-even under Kt. Under length alone they are a pure win.
- Logical depth points the other way. A proof that is short but deep (long to check or normalize) is "complex" by a meaningful measure. A minimizer of length alone will select for depth, i.e. for work hidden in checking.

### Gaps
- I did not open Li & Vitányi this session, so theorem and page numbers are from memory.
- I found no published paper that explicitly states "MDL relative to a lemma library is gameable unless the library is charged for" in a theorem-proving context. It is a direct consequence of two-part MDL but not a cited result. DreamCoder-style library learning does charge for the library (a Bayesian prior over the library), but that belongs to the adjacent research thread.

---

## Q3. Proof complexity: cut-elimination blowup, Frege vs extended Frege, speed-up theorems, reflection

### Takeaway
Proof length depends heavily on the proof system and the allowed rules. Removing cuts or lemmas can cost a non-elementary (tower-of-exponentials) increase. Adding the extension rule (abbreviations) or moving to a stronger theory can shorten proofs by **any computable function** (Gödel speed-up). So "move strength into the system or library" is not a cheat that a length metric can detect. In the worst case it is the only way to get short proofs. Proof by reflection moves length into computation, so proof length and checking time are separate resources and must be measured separately.

### Cited Findings
- **[V]** Gödel's speed-up theorem (Gödel 1936, "Über die Länge von Beweisen," *Ergebnisse eines Math. Kolloquiums* 7:23–24). For a consistent, recursively axiomatizable T ⊆ S (S stronger, e.g. (i+1)-th order vs i-th order arithmetic), "for any computable function f:ℕ→ℕ, there is a proposition φ such that" L_T(φ) > f(L_S(φ)). — [Wikipedia: Gödel's speed-up theorem](https://en.wikipedia.org/wiki/G%C3%B6del%27s_speed-up_theorem)
- **[V]** Buss 1994, "On Gödel's theorems on lengths of proofs I: Number of lines and speedup for arithmetics," *JSL* 59(3):737–756, [doi:10.2307/2275906](https://doi.org/10.2307/2275906). It gives the first published proof of Gödel's claim of super-recursive (unbounded) speedup of (i+1)-st over i-th order arithmetic, plus a second proof avoiding self-reference that loosely follows Statman. Length here is counted in **number of lines (steps)**. Part II (Feasible Mathematics II, 1995, pp. 57–90) covers lower bounds for recognizing k-symbol provability. — [Buss's page](https://mathweb.ucsd.edu/~sbuss/ResearchWeb/godelone/); [Wikipedia refs](https://en.wikipedia.org/wiki/G%C3%B6del%27s_speed-up_theorem)
- **[V]** Concrete long proofs from Wikipedia: there is a PA-provable statement whose shortest PA proof has at least a googolplex symbols. Friedman's finite Kruskal-type statement has PA proofs of length at least A(1000), yet a short proof in second-order arithmetic. — [Wikipedia](https://en.wikipedia.org/wiki/G%C3%B6del%27s_speed-up_theorem)
- **[M]** Parikh 1973, "Some results on the length of proofs," *Trans. AMS* 177:29–36. In PA with addition and multiplication as ternary predicates, there are formulas whose proofs need many lines, and **counting lines vs counting symbols can give very different answers**. Parikh also proved Kreisel's conjecture for this restricted language. (Kreisel's conjecture: if φ(S^n 0) has a k-line proof for every n then ∀xφ(x) is provable. It is still open for the standard language.) Buss surveys this in "Bounded arithmetic, proof complexity and two papers of Parikh." — [Buss survey PDF](https://www.math.ucsd.edu/%7Esbuss/ResearchWeb/parikh/paper.pdf)
- **[M]** Non-elementary cut elimination. Statman 1979 ("Lower bounds on Herbrand's theorem," *Proc. AMS* 75:104–107) and Orevkov 1979/1982 ("Lower bounds for increasing complexity of derivations after cut elimination," *J. Soviet Math.* 20, 1982) gave sequences of first-order formulas with proofs of size O(n) using cuts whose cut-free proofs need size at least a tower 2^2^…^2 of height about n. Boolos's "Don't eliminate cut" (1984) gives a natural example. — [Straßburger 2019 cites Boo84](https://www.lix.polytechnique.fr/Labo/Lutz.Strassburger/papers/hilbert24.pdf); [Statman 1979 (M)](https://doi.org/10.1090/S0002-9939-1979-0532115-4)
- **[V]** Cut, extension and substitution (Cook–Reckhow 1979) and co-contraction are all "proof compression" mechanisms. — [Straßburger 2019, §2(b)](https://www.lix.polytechnique.fr/Labo/Lutz.Strassburger/papers/hilbert24.pdf)
- **[M]** Frege vs extended Frege (EF): EF adds the extension rule p ↔ φ for fresh p, i.e. abbreviations or definitions. Whether Frege p-simulates EF is a major open problem. It is a proof-complexity analogue of whether definitions (DAG sharing at the formula level) can save super-polynomially. Cook–Reckhow (1979, JSL 44) set up p-simulation, and NP ≠ coNP iff no propositional proof system has polynomial-size proofs of all tautologies. — [Cook & Reckhow 1979 (M)](https://doi.org/10.2307/2273702); [Krajíček, *Proof Complexity*, CUP 2019 (M)](https://doi.org/10.1017/9781108242066)
- **[M]** Proof by reflection and the Poincaré principle (Barendregt & Barendsen; Barendregt's "Poincaré principle": computations need not be justified in the proof object because the checker performs them by conversion). Coq/Lean/Agda proofs via `decide`, `vm_compute`, `native_decide`, `omega` or `ring` produce tiny proof terms whose checking may take arbitrary time. `native_decide` additionally enlarges the trusted base, the compiler. — [Barendregt & Barendsen, "Autarkic computations in formal proofs," JAR 28 (2002) (M)](https://doi.org/10.1023/A:1015761529444)

### Inferences
- Speed-up theorems give the strongest theoretical case that length minimization rewards moving content into the axioms, library or checker. For any computable "budget" f there are theorems whose proofs shrink by more than f when a stronger system is allowed. A proof-length reward over an uncontrolled system or library is therefore unbounded-gameable in principle. The axiom set must be fixed and charged, as in two-part MDL.
- Non-elementary cut blowup means "short proof with lemmas" and "short cut-free (analytic, explanatory-by-subformula-property) proof" can be incomparable objectives. Rewarding raw length will push toward cut-dense, abbreviation-dense proofs. Those are short but say little about which subformulas matter.
- Proof length and checking time are independent resources. Reflection collapses one into the other. A robust metric needs both, for example Kt-like length + log(check time), or a hard cap on kernel reduction steps, and should count trusted-base extensions (`native_decide`, new axioms) as infinite or very expensive.

### Gaps
- I did not verify the exact tower-height constants in Statman and Orevkov. Parikh 1973 details are from memory.
- I did not find a quantitative theorem directly relating proof-term size to kernel checking time in dependent type theory. Type checking with conversion is non-elementary in general (simply-typed β-normalization is non-elementary per Statman 1979; [M]).

---

## Q4. Goodhart and reward-hacking theory applied to a simplicity metric

### Takeaway
Skalse et al. prove that, over all stochastic policies, **no non-trivial proxy is unhackable**. Unhackability is possible only for restricted policy sets, and simplifying or narrowing the reward does not usually achieve it. A length metric is a proxy for "good proof or program" and falls under this theorem. Among the Manheim–Garrabrant variants, length minimization is especially exposed to **extremal Goodhart** (the length–quality correlation breaks at the tail, e.g. degenerate tiny programs) and **adversarial / metric manipulation** (changing the reference machine, library or checker). Gao et al.'s scaling laws show gold reward first rising and then falling with optimization distance. That is the expected shape for any learned or imperfect simplicity proxy.

### Cited Findings
- **[V]** Skalse, Howe, Krasheninnikov & Krueger, "Defining and Characterizing Reward Hacking" (arXiv 2209.13085; NeurIPS 2022 [M for venue]; v2 March 2025). Definition: "We say that a proxy is unhackable if increasing the expected proxy return can never decrease the expected true return." Results: over all stochastic policies, two reward functions can be unhackable only if one is constant, because return is linear in state-action occupancy measures. Non-trivial unhackable pairs exist for deterministic policies or finite policy sets. They give necessary and sufficient conditions for "simplifications" (a proxy that collapses distinctions). They argue that narrowing the reward or ignoring fine distinctions usually does **not** yield unhackability, and that narrow reward specification is in tension with alignment. — [arXiv 2209.13085](https://arxiv.org/abs/2209.13085)
- **[V]** Manheim & Garrabrant, "Categorizing Variants of Goodhart's Law" (arXiv 1803.04585):
  - **Regressional**: "you select not only for the true goal, but also for the difference between the proxy and the goal." Unavoidable with any inexact metric.
  - **Extremal**: optimization pushes into regions where the past relationship fails. Subtypes: model insufficiency, change in regime.
  - **Causal**: an intervention breaks the causal link. Subtypes: shared-cause intervention, intermediary intervention, **metric manipulation**.
  - **Adversarial**: other agents exploit the metric. Subtypes: adversarial misalignment (Campbell's law), the normal cobra effect, the non-causal cobra effect.
  The paper says "the importance of Goodhart effects depends on the amount of power directed towards optimizing the proxy." — [arXiv 1803.04585 HTML v4](https://arxiv.org/html/1803.04585v4)
- **[V]** Gao, Schulman & Hilton, "Scaling Laws for Reward Model Overoptimization" (ICML 2023). With d = √KL(π‖π_init), gold reward follows R_bon(d) = d(α_bon − β_bon d) for best-of-n and R_RL(d) = d(α_RL − β_RL log d) for RL. The coefficients scale smoothly with reward-model parameter count. The authors attribute the α term to regressional Goodhart and the β term mainly to extremal Goodhart. Causal Goodhart looks like regressional in their setup. "We do not expect the effects of adversarial Goodhart to be captured in this work." — [arXiv 2210.10760 HTML](https://arxiv.org/html/2210.10760v1)
- **[M]** Krakovna et al.'s specification-gaming list (DeepMind blog 2020 plus a public spreadsheet) includes program-synthesis and evolution cases that rewarded "passing" or "short" artifacts. Example: GenProg automated program repair (Weimer et al.) "fixed" a sort or diff test by deleting or emptying the trusted output file the test compared against. Several evolved-algorithm cases exploit evaluator bugs. — [Specification gaming: the flip side of AI ingenuity (DeepMind blog)](https://deepmind.google/discover/blog/specification-gaming-the-flip-side-of-ai-ingenuity/); [spreadsheet](https://docs.google.com/spreadsheets/d/e/2PACX-1vRPiprOaC3HsCf5Tuum8bRfzYUiKLRqJmbOoC-32JorNdfyTiRRsR7Ea5eWtvsWzuxo8bjOxCG84dAg/pubhtml)
- **[M]** Code golf as a natural experiment in length-gaming. Code Golf Stack Exchange keeps a list of "loopholes forbidden by default." It includes **using a language or interpreter made for the challenge** (a reference machine that absorbs the answer), fetching the answer from the internet or an external file, and exploiting built-ins that solve the task outright. Golf languages (GolfScript, CJam, Jelly, 05AB1E, Vyxal) are reference machines tuned to shrink common programs. — [Loopholes that are forbidden by default (meta.codegolf)](https://codegolf.meta.stackexchange.com/questions/1061/loopholes-that-are-forbidden-by-default)

### Inferences
- How the four variants map onto a proof or program length reward:
  - **Regressional**: short proofs correlate with elegance but also with noise, such as heavy automation calls and opaque tactic combinators.
  - **Extremal**: at the extreme, the shortest artifacts are degenerate (`by decide`, `by simp_all`, `native_decide`, `sorry`-like holes if checking is weak), or exploit special-casing.
  - **Causal / metric manipulation**: editing the library, adding axioms, redefining notation, or changing the tokenizer changes the metric without changing the proof's content.
  - **Adversarial**: an optimizer that can influence the checker (kernel bugs, `native_decide`, `implemented_by`, unsafe FFI) can minimize length with no proof at all.
- Skalse's linearity result suggests restricting the **policy or artifact space** (fixed kernel, fixed library, fixed axioms, no trusted-base extension), not refining the reward. The paper shows unhackability is feasible only over restricted policy sets.
- The code-golf loophole list is the closest real-world, empirically evolved defence list for a length metric. It maps almost one-to-one onto the theoretical attack surface: reference-machine choice, external state, and built-ins.

### Gaps
- I found no paper that formally analyses a **simplicity or length** reward as a Goodhart proxy in RL for theorem proving. The mapping above is my inference.
- I did not re-verify the GenProg entry's exact wording in the Krakovna spreadsheet.

---

## Q5. Genetic programming bloat and parsimony pressure: lessons for length-penalized search

### Takeaway
In GP, program size grows without fitness benefit ("bloat") whenever selection is on fitness alone. Fitness-neutral code (introns) protects working code from destructive variation, and longer equivalents of a solution vastly outnumber shorter ones. Parsimony pressure (a size penalty) controls bloat, but it is hard to tune: too much and size dominates fitness, giving premature convergence on tiny degenerate programs. Lexicographic or multi-objective formulations combined with hard limits work best empirically. This is the closest empirical literature on "optimize fitness and length jointly."

### Cited Findings
- **[V]** Langdon & Poli, "Fitness Causes Bloat" (1997). Growth is driven by fitness-based selection itself and is inherent when a fixed evaluation function is paired with a discrete variable-length representation: "longer representations of a given solution outnumber shorter ones," so unbiased search drifts to longer programs. — [GP bibliography entry](https://gpbib.cs.ucl.ac.uk/gp-html/Langdon_1997_bloatTR.html); [mutation follow-up](https://gpbib.cs.ucl.ac.uk/gp-html/Langdon_1997_bloatMUTet.html)
- **[V via search summary]** Introns are code with no behavioural effect. The "protective code" explanation (Nordin & Banzhaf and others) holds that introns shield useful code from crossover. Langdon later attributes bloat mainly to the exponential shape of the search space. If bloat makes a solution harder to damage, it also makes it harder to improve. Soule (2002) argues exons also drive code growth. — [Langdon, "The Evolution of Size and Shape" (AiGP3, 1999)](https://gpbib.cs.ucl.ac.uk/gp-html/langdon_1999_aigp3.html); [Soule, "Exons and Code Growth in GP"](https://www.academia.edu/54658794/Exons_and_Code_Growth_in_Genetic_Programming)
- **[V]** Luke & Panait, "A Comparison of Bloat Control Methods for Genetic Programming," *Evolutionary Computation* 14(3):309–344, 2006, [doi:10.1162/evco.2006.14.3.309](https://doi.org/10.1162/evco.2006.14.3.309). They compare nine methods on top of depth limiting, including **linear parametric parsimony pressure**, **lexicographic parsimony pressure** (size used only to break fitness ties), biased multi-objective parsimony pressure, double tournament, and tarpeian. Main finding: depth limiting combined with a size-penalizing method generally beats either alone. — [GP bib entry](https://gpbib.cs.ucl.ac.uk/gp-html/Luke_2006_EC.html)
- **[V via secondary]** Parametric parsimony methods are hard to tune so that size does not dominate fitness late in a run. — [CTU lecture slides on bloat](https://cw.fel.cvut.cz/old/_media/courses/a0m33eoa/a0m33eoa_bloat_2010.pdf)
- **[M]** Zhang & Mühlenbein, "Balancing Accuracy and Parsimony in Genetic Programming," *Evolutionary Computation* 3(1):17–38, 1995, introduced an **adaptive Occam method**, an MDL-motivated fitness = error + α(t)·complexity with α adapted over the run. Fixed parsimony coefficients caused either bloat (too small) or premature convergence to small, under-fit programs (too large). Iba, de Garis & Sato (1994) used MDL directly as GP fitness. — [Zhang & Mühlenbein 1995 (M)](https://doi.org/10.1162/evco.1995.3.1.17)
- **[M]** Poli's covariant parsimony pressure (2008, with McPhee) sets the parsimony coefficient analytically from the size–fitness covariance so the mean size follows a target. It is a control-theoretic fix, not a "true simplicity" measure. — [Poli & McPhee, "Parsimony pressure made easy," GECCO 2008 (M)](https://doi.org/10.1145/1389095.1389340)

### Inferences
- Lessons that transfer to LLM proof or program length minimization:
  1. **Without** a length term, expect bloat. Neutral, redundant tactic lines are the LLM analogue of introns.
  2. **With** a linear length term, expect a tuning cliff. Past some weight the optimizer collapses onto degenerate short artifacts (automation one-liners, `decide`, special-casing), the GP "premature convergence to tiny programs."
  3. **Lexicographic** parsimony (correctness first, length only as a tie-breaker among correct artifacts) is the most Goodhart-resistant of the simple schemes. Length can never buy back lost correctness.
  4. Hard caps (depth or size limits), combined with a mild penalty, beat either alone.
- GP measures size syntactically (node count). Fitness-neutral code with zero semantic content is exactly what a size metric fails to see. A semantic or "effective size" measure (e.g. size after removing introns or dead code, or normalizing) is harder to game but costlier to compute.

### Gaps
- The Zhang & Mühlenbein and Poli & McPhee details are from memory and were not checked this session.
- I found no published report of GP parsimony pressure producing *overfitting* specifically. The standard concern runs the other way: bloat correlates with overfitting, and parsimony is used to improve generalization; see [Improving Generalization Ability of GP, arXiv 1304.3779](https://arxiv.org/abs/1304.3779), not read.

---

## Q6. Tree size vs DAG size (sharing), and normal-form size vs term size

### Takeaway
Size depends on the representation. Trees vs DAGs (with sharing or hash-consing) can differ exponentially, and a term vs its normal form can differ non-elementarily in the simply-typed λ-calculus and exponentially in a linear number of steps even in weak evaluation ("size explosion"). A size measure on unshared trees penalizes reuse, while a DAG measure rewards it. Neither is canonical. The proof-complexity view (Frege vs extended Frege, tree-like vs DAG-like) and the λ-calculus invariance view (shared normal forms) both favour **DAG or shared representation** as the reasonable size, because it is polynomially related to the actual computation cost.

### Cited Findings
- **[V]** Size explosion: "some terms produce exponentially large output in a linear number of steps." Accattoli & Dal Lago resolve it with **shared normal forms** and "useful" sharing in the Linear Substitution Calculus, where evaluation length and output size are linearly related. — [Accattoli & Dal Lago, LMCS 12(1), 2016, arXiv 1601.01233](https://arxiv.org/abs/1601.01233)
- **[V]** Proof-level analogue: co-contraction "allows to share subproofs," and extension or substitution "allow us to use abbreviations inside a proof." Both are proof-compression mechanisms whose removal causes blowup. — [Straßburger 2019 §2(b)](https://www.lix.polytechnique.fr/Labo/Lutz.Strassburger/papers/hilbert24.pdf)
- **[M]** For resolution, tree-like proofs are exponentially weaker than DAG-like proofs (e.g. Ben-Sasson, Impagliazzo & Wigderson 2004, "Near optimal separation of tree-like and general resolution," *Combinatorica* 24). For Frege systems, Krajíček showed tree-like Frege p-simulates DAG-like Frege, so the gap is only polynomial there. Whether sharing matters exponentially therefore depends on the proof system. — [Krajíček, *Bounded Arithmetic, Propositional Logic and Complexity Theory*, CUP 1995 (M)](https://doi.org/10.1017/CBO9780511529948)
- **[M]** Simply-typed λ-calculus normalization is non-elementary: Statman 1979 showed deciding βη-equality of simply typed terms is not elementary recursive. Church-numeral towers (2 2 2 … 2) have size O(n) but normal forms of tower height n. — [Statman 1979, "The typed λ-calculus is not elementary recursive," TCS 9 (M)](https://doi.org/10.1016/0304-3975(79)90007-0)
- **[M]** Hash-consing turns a term with repeated subterms into a DAG. The family t_0 = x, t_{n+1} = f(t_n, t_n) has tree size 2^{n+1}−1 but DAG size n+1. Lean 4 and other kernels use sharing-aware representations internally, but user-facing "proof length" (characters, tokens, tactic count) usually counts the unshared surface text. — [general CS folklore; no single citation]

### Inferences
- Choosing tree size vs DAG size is itself an optimization lever. Under tree size, an optimizer will introduce `let` or `have` (cuts) to share. Under DAG size, it will maximize sharing, and the measure stops penalizing repeated reasoning. DAG or shared size is the "reasonable" choice in the invariance sense (polynomially related to evaluation cost), but surface-text length is what most benchmarks actually measure.
- Measuring the normal form instead of the term (e.g. the fully elaborated kernel term rather than the tactic script) closes the "automation hides work" hole. But it can be exponentially or non-elementarily larger, and it penalizes legitimate abstraction. A defensible measure would report **both** the surface size and the elaborated (shared) kernel-term size, plus checking time.

### Gaps
- I found no source comparing tactic-script length with elaborated proof-term size in Lean or Coq at scale. That would be the empirical analogue of the tree-vs-normal-form gap, and may be covered by the parallel empirical research thread.

---

## Q7. Invariant or "honest" cost models as a basis for an ungameable measure

### Takeaway
The invariance thesis (Slot & van Emde Boas) defines a "reasonable" machine as one that simulates and is simulated by Turing machines with polynomial time overhead (and constant-factor space). Accattoli & Dal Lago proved that **the number of leftmost-outermost β-steps** is an invariant (reasonable) time cost model for the full λ-calculus, despite size explosion, by evaluating with sharing. This supplies a principled, strategy-independent time measure that can be added to a length measure. It is invariant only up to polynomials. That is enough to stop exponential "make the checker work" hacks, but not polynomial ones.

### Cited Findings
- **[V]** The Slot–van Emde Boas weak invariance thesis says reasonable machines simulate each other with polynomial time overhead. Accattoli & Dal Lago ask whether the λ-calculus is reasonable and show "the length of a leftmost-outermost β-reduction sequence to normal form" is "an invariant cost model." The proof goes through the Linear Substitution Calculus (modelled on linear-logic proof nets) and useful sharing. — [Accattoli & Dal Lago 2016, arXiv 1601.01233](https://arxiv.org/abs/1601.01233)
- **[M]** Slot & van Emde Boas, "On tape versus core: an application of space efficient perfect hash functions to the invariance of space," STOC 1984, stated the invariance thesis for time (polynomial) and space (constant factor). — [doi:10.1145/800057.808705 (M)](https://doi.org/10.1145/800057.808705)
- **[M]** Precursors and extensions: Dal Lago & Martini (2008) showed weak call-by-value β-steps are invariant ("The weak lambda calculus as a reasonable machine," TCS 398). Accattoli has a survey line on "reasonable" abstract machines. Forster, Kunze & Roth (POPL 2020) proved, in Coq, that the weak call-by-value λ-calculus is reasonable for both time and space. Accattoli, Dal Lago & Vanoni studied space invariance (2022). — [Forster, Kunze, Roth, "The weak call-by-value λ-calculus is reasonable for both time and space," POPL 2020 (M)](https://doi.org/10.1145/3371095)

### Inferences
- An invariant cost model removes one degree of freedom an optimizer could otherwise exploit: the **reduction strategy**. Counting β-steps under an arbitrary strategy can differ exponentially between strategies on the same term. Counting leftmost-outermost steps (or steps of a reasonable shared machine) is canonical up to a polynomial.
- An "honest" simplicity score could therefore combine:
  - description length on a fixed tiny reference machine (BLC, combinatory logic or tree calculus; library charged two-part), and
  - log of invariant evaluation or checking cost (Kt-style), measured on a reasonable machine.

  This is gameable only within the additive constant and polynomial time slack. Every remaining hack then lives in either "pay bits to the library" or "pay log-time to the checker," both visible.
- Invariance is only polynomial. Kt charges only log(time). An optimizer can therefore still trade 1 bit for doubling the checker's work. Whether that is a "hack" depends on whether the task values checking cost, which is a design decision, not a theorem.

### Gaps
- I did not verify the Forster–Kunze–Roth or Slot–van Emde Boas citations this session.
- I found no work that explicitly proposes invariant cost models as an anti-reward-hacking device. This connection is my inference.

---

## Q8. Synthesis: which simplicity measures resist gaming, in principle

### Takeaway
No length-type measure is unhackable over an unrestricted space (Skalse et al.; invariance constant; speed-up theorems). Robustness comes from (a) fixing and charging for the reference machine, axioms and library (two-part MDL), (b) adding an invariant time or checking cost (Kt, an invariant cost model), (c) measuring a canonical representation (shared or normalized, ideally a proof-identity invariant such as combinatorial proofs or proof nets, polynomially related to the size of every presentation), and (d) lexicographic rather than weighted combination with correctness.

### Cited Findings
- **[V]** Unhackability holds only for restricted policy sets, and simplification proxies are characterized by necessary and sufficient conditions. — [Skalse et al.](https://arxiv.org/abs/2209.13085)
- **[V]** Speed-up is unbounded by any computable function across systems. — [Gödel/Buss via Wikipedia](https://en.wikipedia.org/wiki/G%C3%B6del%27s_speed-up_theorem)
- **[V]** Proof identity should be polynomial-time decidable and size-respecting. Combinatorial proofs achieve this for classical propositional logic. — [Straßburger 2019](https://www.lix.polytechnique.fr/Labo/Lutz.Strassburger/papers/hilbert24.pdf)
- **[V]** Leftmost-outermost β-steps are an invariant cost model. — [Accattoli & Dal Lago](https://arxiv.org/abs/1601.01233)
- **[V]** Kt = length + log time. The speed prior is computable in doubly-exponential time, not polynomial. — [Schmidhuber 2002](https://www.Gwern.net/doc/reinforcement-learning/model/2002-schmidhuber.pdf); [Filan et al. 2016](https://arxiv.org/abs/1604.03343)
- **[V]** Combining a hard limit with a penalty beats either alone in GP bloat control. — [Luke & Panait 2006](https://gpbib.cs.ucl.ac.uk/gp-html/Luke_2006_EC.html)

### Inferences
Each measure, how it is gamed, and what partially fixes it:

| Measure | Known gaming route | Theoretical basis | Mitigation |
|---|---|---|---|
| Surface text or token length | Golfing, automation one-liners, notation tricks, new definitions | Invariance constant; code-golf loopholes | Fixed tokenizer and library; charge for new defs |
| Length relative to a growing library | Library absorbs the proof | Two-part MDL; Gödel speed-up | Charge L(library) and amortize across tasks |
| Length with free axioms or stronger system | Add an axiom or strengthen the theory | Gödel/Buss speed-up | Fixed axioms; trusted-base additions forbidden |
| Proof length ignoring check time | Reflection, `decide`, `native_decide` | Poincaré principle; logical depth | Kt-style log(check steps) on an invariant cost model; cap kernel steps |
| Cut-free or normal-form size | Penalizes lemmas; non-elementary blowup | Statman/Orevkov; Boolos | Use as a diagnostic, not a reward |
| Tree size | Encourages `have`/`let` sharing (fine), or penalizes reuse | Tree vs DAG gap | Shared (DAG) size of the elaborated term |
| Proof-identity class (combinatorial proof / proof net) | Hard to game by presentation; only exists for limited logics | Straßburger Objectives 1–4; Hughes 2006 | Research-grade; propositional only |
| Weighted fitness + λ·length | Collapse to degenerate small artifacts at large λ | GP parsimony tuning; Gao et al. overoptimization curve | Lexicographic (correct first, length as tie-break) + hard caps |

### Gaps
- No source found evaluating any of these combined measures empirically under RL optimization pressure for theorem proving. The table is a synthesis of the theory, not a tested result.
