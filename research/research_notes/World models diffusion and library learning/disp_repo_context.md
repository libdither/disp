# The user's own frameworks in the disp repo that bear on world models, diffusion/denoising, sampling, abstraction and library learning

Scope: repository files only (read 2026-11-05, working tree with uncommitted changes as in the session's git status). Every citation is `path:line` (or a line range) relative to the repo root. Authorship caveat for the report writer: most research essays are AI-drafted syntheses the user reviewed; the user's own words are in `GOALS.md` (README rates it "🧑 85% · 🤖 15%", `README.md:180`), `IDEAS.md` ("No LLM should write in this file", `IDEAS.md:1`), `OUTLINE.md`, and quoted intuitions such as `research/NATURE_OF_OPTIMIZERS.md:7`. `FOUNDATIONS.md` and `research/CLAIMS_OVER_TREES.md` are rated "🤖" in `README.md:181`, `:186`. The `NATURE_OF_*` essays call themselves research notes that grew out of conversations (e.g. `research/NATURE_OF_AGENTS.md:3`).

Vocabulary glosses used below (the user asked that project codenames be glossed):
- **tree calculus**: Barry Jay's one-atom, five-rule reflective combinator calculus; programs can take other programs apart (`README.md:3`, `README.md:42-62`).
- **claim / witness / challenge / lens**: the current type layer. A claim is `{witness, challenge} => Bool`; a lens carries witnesses forward and challenges back (`lib/claim.disp:1-8`, `lib/claim.disp:19-23`).
- **confirmer**: the symbolic checker over "trees with holes" (`lib/confirm.disp:1-30`).
- **the strand lattice**: `evaluators/rust-ic-strands`, the tree-calculus interaction net run as a cellular automaton / physical substrate (`research/COARSE_GRAINED_LATTICE.md:9-12`).
- **memo snapshot / `cold_equiv`**: the evaluator's persistent cache of reduction facts, and the cost measure that prices cache hits at their cold cost (`CLAUDE.md:19`, `evaluators/rust-eager/crate/src/snapshot.rs:1-26`).
- **`~_T`**: the coarse, behavioral equivalence (same behavior at type T), versus `tree_eq`, the finest (identical trees) (`archive/one-offs/SINGULAR_LEARNING_THEORY.md:331-343`).

---

## Key question 1: What the user's essays say about abstraction, agents, optimizers, compression, coarse-graining and search

### Takeaway
Across the essays the user's framework is consistent. An abstraction is an equivalence class plus a chosen representative. A library is the search prior, and it is the model half of a two-part (MDL) code. An agent is a self-judge that has to model itself coarsely, and regularizers are prices that decide which agent results. A general optimizer pays exactly one over the prior it gives the answer. Coarse-graining is a projection that commutes with the dynamics up to an error bound. The essays never use "world model", "JEPA", "diffusion", "denoising", "score function" (in the gradient-of-log-density sense) or "Langevin". The closest machinery is cost and hardware models, tempered posteriors and free energies, Doob/SMC-corrected sampling, and a thermodynamic substrate.

### Cited Findings

**Abstraction (`research/THE_NATURE_OF_ABSTRACTION.typ`, dated 2026-08-14, `:14`)**
- The core definition, from four literatures: "an equivalence relation over prior objects, together with a representative that clients may swap for any other member of the class". Frege abstraction, Berkeley's representative particulars, a Kolmogorov two-part code, a cut formula, a Galois connection, an MDP homomorphism and an e-class with an extracted representative are "the same object wearing eight costumes". disp's relation objects are "a ninth costume" — `research/THE_NATURE_OF_ABSTRACTION.typ:32`.
- Berkeley over Locke. Generality is "a functional role, not an intrinsic property of any content"; "Deletion-based metrics, if Berkeley is right, measure the wrong object" — `research/THE_NATURE_OF_ABSTRACTION.typ:46-48`.
- The only purpose-free criterion for a good abstraction is a universal property. "Everything else — compression, search-cost reduction, invariance, transfer — is relative to a task distribution" — `research/THE_NATURE_OF_ABSTRACTION.typ:34`, `:71-90`.
- The account needs two sorts: "structural role (predicate, swap-invariant) plus resource profile (not swap-invariant, the thing optimization improves)"; "there is no developed philosophy of resource-sensitive abstraction to import" — `research/THE_NATURE_OF_ABSTRACTION.typ:90`.
- Abstraction is not idealization. "A cost model, a loss surrogate, an optimizer's simplifying assumption ... is an idealization: useful, false, and it must never migrate into the trusted layer" — `research/THE_NATURE_OF_ABSTRACTION.typ:63`.
- The two-part code: "the library is the model $S$; user programs are the data-to-model code". An optimal library "exists and is well-defined" but is incomputable. Degenerate optima: one bespoke combinator per program (overfit) versus an empty library (underfit) — `research/THE_NATURE_OF_ABSTRACTION.typ:98-108`.
- "A good library entry is _cached depth_ — expensive-to-rediscover, cheap-to-reuse computational work ... You cannot cheaply generate genuinely valuable abstractions, only cheaply reuse them" (Bennett's logical depth) — `research/THE_NATURE_OF_ABSTRACTION.typ:117`.
- A lemma is a cut, and cut elimination costs a tower of exponentials. "Tree-grammar induction is thereby the mathematically canonical library-learning algorithm for disp's substrate" — `research/THE_NATURE_OF_ABSTRACTION.typ:121-123`.
- Coarse-graining. Israeli and Goldenfeld coarse-grain cellular automata so that "the right abstraction can move questions across the decidability boundary". Mehta and Schwab's mapping from variational renormalization (RG) to deep networks "supplies the flow picture: abstract, measure induced error against the spec, re-abstract". The information bottleneck sweep gives "no single optimal abstraction, there is a curve" — `research/THE_NATURE_OF_ABSTRACTION.typ:147-151`.
- Library learning. DreamCoder is "the strongest published argument that a library is not a convenience but _the learned prior that makes future search tractable_". Its loop has three phases: wake (search), sleep-abstraction (MDL compression), and sleep-dreaming, which retrains the recognizer "on samples from the current library". Stitch and babble follow ("library learning modulo a theory") — `research/THE_NATURE_OF_ABSTRACTION.typ:155-163`.
- RL state abstraction is the soundness-first precedent. An MDP homomorphism is "abstraction as a commuting square with a proof". Bisimulation metrics give $|V^*(s) - V^*(s')| <= d(s, s')$. The "ε-metric end ... has _no program-optimization analogue anywhere_" — `research/THE_NATURE_OF_ABSTRACTION.typ:169-173`.
- Chunking. Chase and Simon's chess masters show that "expertise is a grown, distribution-specific library of chunks" — `research/THE_NATURE_OF_ABSTRACTION.typ:177`. On superoptimization: "the learned library as the proposal distribution for the superoptimizer — is exactly disp's bootstrapping story" — `:187`.
- "The library is the optimizer's prior ... stdlib quality is not hygiene — it is the optimizer's asymptotic search budget" — `research/THE_NATURE_OF_ABSTRACTION.typ:196`, also `:251`.
- The layered objective: gate (licensed only), then a Pareto frontier "never a scalar" over two-part MDL and measured steps, then canonicity, then "the corpus is the metric" — `research/THE_NATURE_OF_ABSTRACTION.typ:205-217`. The Löbian wall (Löb's theorem: a system cannot prove its own proof checker sound) means the trusted core and license checker stay outside optimization — `:262`.

**Agents (`research/NATURE_OF_AGENTS.md`, 2026-10-14)**
- The governing question: "No judge can judge itself exactly. What can it give up instead, and which give-up do the prices make cheapest?" — `research/NATURE_OF_AGENTS.md:11`.
- An agent is "a judge-and-act loop, behind a boundary, that stays an approximate fixed point of judging itself at the substrate's prices" — `research/NATURE_OF_AGENTS.md:23`.
- One way to pay is fidelity, "models itself coarsely": "A self-model smaller than the self is an abstraction" — `research/NATURE_OF_AGENTS.md:96`, `:104`.
- The definition has three parts: a boundary (Markov blanket), "a loop that stays put", and "a cheaper description: ... 'it is trying to X' predicts it more cheaply than simulating its parts" — `research/NATURE_OF_AGENTS.md:111-115`.
- "A part that carries a model of what lies outside it stops paying for the cut. Thermodynamics rewards parts that model their surroundings" — `research/NATURE_OF_AGENTS.md:113`. This is the essays' closest statement of why world models pay.
- "Regularizers are prices." A KL price yields "the default reweighted by e^(βU), a softmax"; "quantilize at readback" is the hard-threshold version, a q-quantilizer "exactly log(1/q) of KL divergence from the base" — `research/NATURE_OF_AGENTS.md:148-156`.
- The resource-price table matches each priced resource to the agents that win: description length favors compressed agents with general search inside; runtime favors "tables and reflexes"; erasure favors history-keepers that run back — `research/NATURE_OF_AGENTS.md:158-169`.
- A soft judge with temperature T undergoes symmetry breaking. The truth-teller is a mean-field magnet, m = tanh(m/(4T)), with one undecided self above T = 1/4 and "two committed selves" below — `research/NATURE_OF_AGENTS.md:173-185`. In general: "the regularizer changed the kind of agent" is "a phase transition in the set of stable self-judges". The essay cites IB bifurcations, deterministic-annealing cluster splits, and Genewein et al.: "Changing the temperature leads to a natural emergence of different levels of abstraction" — `research/NATURE_OF_AGENTS.md:200-209`.
- Proposed experiments, none started: a soft judge over fractions, a price sweep (size versus step penalties), a weighted committee of heuristics, and a heat-versus-fuel sweep on the lattice — `research/NATURE_OF_AGENTS.md:233-237`.

**Optimizers (`research/NATURE_OF_OPTIMIZERS.md`, 2026-09-16; `research/OPTIMIZER.typ`)**
- The user's intuition, quoted: "create a general purpose optimizer that can take an arbitrary type defining a search space, and generate the required hierarchy of heuristics that traverse the search space well in an efficient manner, and that this heuristics-generating process should be simple and interpretable" — `research/NATURE_OF_OPTIMIZERS.md:7`.
- "The price of generality is one over the prior." Levin search, HSEARCH and Levin tree search share "one shape: a general searcher pays exactly one over the prior it assigns to the answer. The exponent never disappears; it moves into the prior" — `research/NATURE_OF_OPTIMIZERS.md:31-39`.
- Every precedent has three layers: spec-derived, meta-loop, and "the prior that makes hard instances tractable". Legibility drops at the third — `research/NATURE_OF_OPTIMIZERS.md:124-138`.
- "A type checker is not only a 0/1 oracle. Its failure trace is a rich, structured, interpretable signal" — `research/NATURE_OF_OPTIMIZERS.md:136`.
- The only literal mention of "world model" in the repo comes through Guaranteed Safe AI ("world model, safety specification, verifier, auditable proof certificate"). The essay maps it directly: "the guarantee is explicitly relative to the specification and the world model. For disp that means relative to the type and the cost model" — `research/NATURE_OF_OPTIMIZERS.md:185`.
- Consequences adopted: quantilize at readback (`:197`); "Instrument proxy divergence as a first-class number ... report it per run the way `cold_equiv` is reported now" (`:199`); "Never score legibility" (`:200`).
- The one-sentence optimizer design: "verified gradient descent on a graded cost over a materialized net" — `research/OPTIMIZER.typ:26`, `:58-76`. The score is "[typechecks] × cheapness", with correctness as a hard gate and cost as the smooth signal — `research/OPTIMIZER.typ:91-99`. Cost has two uses: guidance (untrusted, measured) and soundness (trusted, proven) — `:102-107`.
- "Memoization is the same rewrite ... made uniformly for every x, by the substrate, with no cost model and no choice": memo is "the *degenerate, zero-intelligence corner*" of the staging axis, which runs memo, then partial evaluation, then JIT, then AOT, then codegen — `research/OPTIMIZER.typ:189-215`.
- The search loop: enumerate per-hole candidates "ordered by a prior — size, library frequency, an LLM's logits"; superpose; reduce the recognizer; collapse; and on failure "reify the conflict" — `research/OPTIMIZER.typ:616-629`.
- E-graph versus superposition: "merge what is *proven equal* (compact, certificate-bearing) and keep distinct what is *still being chosen*" — `research/OPTIMIZER.typ:666-697`.

**Coarse-graining (`research/COARSE_GRAINED_LATTICE.md`, 2026-10-10; `research/coarse-graining/`, untracked)**
- A fitted coarse law for the lattice's running time, "Clocks ≈ 23·D + 0.7·W" (D = parallel depth, W = work). It fits 304 runs to ×1.20 and predicts 8 held-out programs to ×1.10 — `research/COARSE_GRAINED_LATTICE.md:33-39`.
- "The lattice already projects exactly onto a coarser system ... What does not commute is time ... The law is the exchange rate between the two clocks" — `research/COARSE_GRAINED_LATTICE.md:155-160`. Gap-closing time "grows as distance^2.1, the signature of diffusion" (physical diffusion of walkers) — `:161-168`. A call-graph level is "the same law one level up" — `:184-187`.
- The inverter-chain demo (`research/coarse-graining/inverter-chain.html`, `.js`; untracked, Nov 2) maps a coarse-graining explicitly. The fine level is voltages plus shot or thermal noise (`inverter-chain.html:98`). The "Lens (forward map)" is "Read each wire as 1 if it is above half the supply" (`:99`). It also gives a coarse step rule (`:100`), a "Price from below" measured from the fine model (`:101`), a "Domain: Gain above 1 (else there are no bits at all), noise well inside the margin" (`:102`), and an "Error bound" (`:103`). The error bound has two numbers. "Run alone" is the coarse simulation from the input. "One step" is the commuting square: "read the fine state, take one coarse step, compare with reading after the fine step". The page subtitle: "The coarse model is only as good as the place where the two agree."
- "Where the slope is steeper than −1 the gate amplifies; that gain is what pushes nearby voltages back to the rails" — `research/coarse-graining/inverter-chain.html:90`. "State commutes almost exactly here, time does not ... A register resets them, at the price of waiting a whole cycle" — `:105`. The JS scores the coarse model against the lens-read fine run (`research/coarse-graining/inverter-chain.js:58-161`), adding Gaussian shot noise per step (`:115`).

**Search, claims, reversibility**
- "A claim is a test on bounded runs of the machine, under a prefix of quantifiers over all trees"; "an optimization is a certified way of removing or bounding a quantifier" — `research/CLAIMS_OVER_TREES.md:32-39`, `:142-171`. "Searching for programs and searching for the witnesses that certify them [is] one search" — `:170-171`.
- "Forgetting turns paths into basins, and the answer is where a basin bottoms out" (a fully reversible system only permutes states) — `research/COMBINATOR_SEARCH_PLAN.md:22-27`. There are two kinds of forgetting: forgetting time and forgetting structure (Landauer heat) — `:29-38`.
- "A stage f : A → B that forgets things becomes a bijection A ≅ B × R once it also returns its residue R, what it forgot". Reduction forgets redexes (R = a history) — `research/REVERSIBLE_PIPELINE.md:10-17`. 75–84% of lattice rewrites lose nothing, and 90–98% of erased agents are copies — `:25-30`. "Forgetting is a separate act ... Speculation becomes storage" — `:221-225`.
- The constrained-search survey says masking is "a Doob transform with the wrong h". The correction factor Φ(prefix), "the probability that a random completion type-checks", is "simultaneously the sampling correction that masking omits, the twist function of SMC, and the value function a search over partial programs wants" — `research/CONSTRAINED_PROGRAM_SEARCH.md:45-51`. RL as inference: "The constrained target is p(y | A = 1) ∝ p₀(y)·A(y)", and a step-budgeted checker gives "a monotone family of cheap potentials" — `research/CONSTRAINED_PROGRAM_SEARCH.md:187-191`; `research/constrained-program-search/rl-and-formal-systems.md:140`.
- The repo's only diffusion-model mention is constrained decoding for diffusion LLMs, reduced to deciding whether an intersection of languages is empty — `research/constrained-program-search/constrained-decoding.md:237`.

**Singular learning theory note (archived)**
- "disp's objective is already a free energy". The synthesis posterior is post(e) ∝ prior e · score T e, and "F(T) = − log Σ_e prior e · score T e — the free energy of a spec" — `archive/one-offs/SINGULAR_LEARNING_THEORY.md:298-317`. Its dictionary has "tempered posterior p_β ... `score = gate × exp(−β·cost)` Gibbs measure" — `:294`. "Degeneracy/effective-complexity is *measurable* by sampling a tempered posterior" — `:186-187`, `:384-391`. A third equivalence sits between `tree_eq` and `~_T` — `:331-352`.

**User's own statements**
- GOALS: specification as a type, the checker as a 0/1 score combined with performance measures, an optimizer that "must handle combinatorial search over programs" and "self-play, continually improve by generating score functions itself" — `GOALS.md:4`, `:13-16`. The meta-utility "ideal version of itself" and hardware models are at `:20-21`.
- OUTLINE: "*A structure that previously needed to be manually configured, its configuration can now be derived from a more general structure*" — `OUTLINE.md:10`. "To get from a high-level specification to an actual configuration of silicon you need to have a model of the silicon to effectively search over the space" — `OUTLINE.md:41`. Lenses and claims are the latest idea and are "still hard to explain" — `OUTLINE.md:28-32`. A specification of optimizers grades them on a variety of tasks — `OUTLINE.md:33-39`.
- IDEAS: "We have a program that produces a stream of trees ... All sampling ops from this stream needs to be reasonable about in terms of how long it takes" — `IDEAS.md:3-5`.

### Inferences
- In the user's terms a world model is a coarse-graining: a lens (encoder) plus a coarse step rule (predictor), valid on a domain, with a measured error bound. The inverter demo already measures the two errors that matter for JEPA-style predictors. "One step" is latent next-step prediction consistency (encode, then step, against step, then encode). "Run alone" is open-loop rollout drift.
- The essays already contain the bridge from abstraction to world models: MDP homomorphisms and bisimulation metrics (`THE_NATURE_OF_ABSTRACTION.typ:169-173`), and "parts that model their surroundings" (`NATURE_OF_AGENTS.md:113`). The report can present JEPA-style latent predictors as the learned, ε-approximate end of the user's "soundness lattice" for abstractions.
- The user's sampling vocabulary is posterior-shaped: prior × gate × exp(−β·cost), temperature, quantilizer, SMC twist, Doob h. It is not score-field or Langevin vocabulary. Linking diffusion guidance to the Doob transform, and Langevin to the thermal lattice, would be the report's own inference, not a repo claim.

### Gaps
- No repo document discusses JEPA, latent prediction, diffusion models (beyond `constrained-decoding.md:237`), denoising score matching, Tweedie's formula, energy-based models or Langevin sampling. A grep over `*.md`, `*.typ`, `*.disp` and `*.html` for these terms returned nothing relevant.
- `CLAIMS_OVER_TREES.md:438-439` points to an external project, `~/Projects/libdither/learner-theory` ("a stage sends goods forward and gets prices back"), which likely holds the user's learner-as-lens ideas. It is outside this repo and was not read.

---

## Key question 2: Which existing disp concepts map onto encoder/latent, predictor, denoiser, sampler, library and prior, and where disp could embody or test the ideas

### Takeaway
Every slot has a concrete disp artifact. The best fits:
- **(a) Encoder and latent:** a lens's forward map, or "behaviour" (what a program does), as opposed to the program tree itself.
- **(b) Predictor:** the machine step, the coarse lattice law, growth fits, and `run_model`, a hardware simulator written as a tree program.
- **(c) Denoiser:** normalization. Reduction to normal form plus hash-consing is canonical only up to reduction. Behavior-level canonicalizers are quotient normalizers (`QuotC(Base, nf)`) and confirmer case tables. The inverter's gain is a literal physical denoiser.
- **(d) Sampler:** the size-ordered `Trees` stream, superposition and collapse, quantilizing, SMC/Doob-corrected decoding, and the strand lattice's thermal mode.
- **(e) Library:** the stdlib plus licensed overlays. The memo snapshot is a degenerate, automatically grown library of ground facts priced by cold cost.
- **(f) Prior and cost model:** gate × cheapness, cost as a graded coeffect, the four rungs for costing a candidate, and `cold_equiv`.

### Cited Findings

**(a) Encoder / latent state**
- A lens is `record lens :: {fwd : W -> V, back : W -> D -> C} -> Lens`. Composition: `lens_then` composes forward maps in order and backward maps in reverse — `lib/claim.disp:19-23`. "A checker, a typing rule or an optimization is a LENS between claims: witnesses go forward, challenges come back" — `research/CLAIMS_OVER_TREES.md:406-414`.
- The inverter "Lens (forward map)" thresholds a voltage to a bit — `research/coarse-graining/inverter-chain.html:99`. The lattice's state projects exactly onto the abstract net — `research/COARSE_GRAINED_LATTICE.md:155-160`.
- Behavior as latent, existing tested code (`IDEAS.disp:53-59`). Forty enumerated trees give 7 Boolean functions, which collapse to 3 behaviors:
  ```disp
  candidates :: List(Tree) := Trees.as_items.bounded(60).s_take(40).s_items
  boolfns :: List(Tree) := candidates.filter(BoolFn)
  test boolfns.length = 7
  behaviors :: List(Pair(Tree, Tree)) := boolfns.map({fn} => pair(fn(true), fn(false))).dedup
  test behaviors.length = 3
  ```
  Then `idfns` (4 programs that agree on true/false) still give 2 distinct outputs on an off-type probe: the "latent" (behavior on the type) hides syntactic detail that inspection can recover — `IDEAS.disp:61-68`.
- The `~_T` fibre, "many trees, one behaviour", is "the locus of degeneracy SLT scores" — `archive/one-offs/SINGULAR_LEARNING_THEORY.md:288`, `:326-328`.
- The confirmer's partly-known values are "a tree with holes, each a named piece of the challenge" (a symbolic or partial latent) — `lib/confirm.disp:2-4`. Results are `answered`, `need`, `stuck` and `coarse` — `lib/confirm.disp:185-189`.
- Residue split: A ≅ B × R. B is the kept view and R "must include at least all information missing from the view" (lens complement) — `research/REVERSIBLE_PIPELINE.md:10-17`.
- The decoder direction exists as a research note: reading trees back as source "at whatever fidelity the information at hand allows" — `research/TREE_READBACK.md:1-6`.

**(b) Predictor / transition**
- The fine transition: `m_step` is "one counted step: one K, S or triage dispatch on a fork head"; `m_advance` runs k steps; "Not done after k steps is never a verdict" — `lib/machine.disp:1-9`, `:56-59`, `:86`. `m_trace` streams machine states — `lib/stream.disp:120`.
- Coarse predictors of cost: the lattice law (`research/COARSE_GRAINED_LATTICE.md:33-39`), and `lib/growth.disp`. "A fit is evidence, not a proof: search refutes it the first time a larger input takes more steps" (`lib/growth.disp:1-5`); `fit`, `bound_of` and `cheaper` are at `lib/growth.disp:45-49`. Cost types use them: "a member of a function type names a step bound" — `lib/theories/cost.test.disp:1`.
- A world model of hardware, in the optimizer design: `run_model : Asm → Input → (Output, Cost)`, "the simulation *is* the reduction". "Real hardware is opaque (measurable, never provable); a model is type-checkable" — `research/OPTIMIZER.typ:735-751`. Four rungs to cost a candidate: static grade, intrinsic interaction count, modeled (`run_model`), and measured (an effect, untrusted). "Soundness ... is always symbolic", while guidance may run candidates — `research/OPTIMIZER.typ:763-776`.
- The coarse step rule, and the commuting-square error, are in the inverter demo — `research/coarse-graining/inverter-chain.html:100`, `:103`.

**(c) Denoiser / canonicalizer / normal-form reducer**
- Hash-consed reduction gives "O(1) conversion + cross-occurrence sharing" — `EVALUATOR.md:46-48`. The memo is pure because "`apply(f,x)=r` is a pure function of `(f,x)`" — `evaluators/rust-eager/crate/src/memo.rs:1-3`.
- The limit of normal forms as canonical forms: `dbl_step` and `dbl_add` are different normal-form trees (`test tree_eq(dbl_step, dbl_add) = false`) but are proved equal by `eq_by_induction` — `README.md:113-128`; `eq_by_induction` is defined at `lib/confirm.disp:679`. FOUNDATIONS calls the gap between the intensional `tree_eq` and extensional equivalence "the spine of the project" — `FOUNDATIONS.md:205`.
- Behavior-level canonicalizers:
  - `QuotC :: {Base : Code, nf : Tree} -> Code` (`lib/confirm.disp:48`), with examples `Parity := QuotC(NatR, parity)` and `All := QuotC(NatR, always_zero)` (`:63-64`). "Quotient respect uses the NORMALIZER: f a ~ f (nf a)" (`:21-23`). `respects_nf` pairs "the class representative with the member: (nf a, a)" (`:630-640`).
  - `canon :: Val -> Val := canon_with(table)` — `lib/confirm.disp:438`.
  - Case tables: "three differently written doublings produce identical tables, so their equality is one hash-cons comparison" — `research/CLAIMS_OVER_TREES.md:157-160`.
- Representative extraction is the abstraction itself: "an e-class with an extracted representative" — `research/THE_NATURE_OF_ABSTRACTION.typ:32`, `:48`, `:227`.
- A physical denoiser: inverter gain "pushes nearby voltages back to the rails", and a bit level exists only with "Gain above 1" — `research/coarse-graining/inverter-chain.html:90`, `:102`. A register "resets" accumulated timing noise each cycle — `:105`.
- Forgetting makes basins: "the answer is where a basin bottoms out" — `research/COMBINATOR_SEARCH_PLAN.md:26-27`.
- Readback-time cleanup: "correlated collapse ... recovers the diagonal ... soundly, at readback" — `research/OPTIMIZER.typ:657-660`.

**(d) Sampler**
- The `Trees` stream gives every tree, smallest first (`lib/trees.disp:1-3`, `:24`; built in `IDEAS.disp:5-21`). The open decision is "which prior over trees weighted sums should use (geometric weights by enumeration position is the natural default)" — `research/CLAIMS_OVER_TREES.md:385-386`.
- Prior-ordered candidate streams plus superposition (`sup_λ`) "shared in one net", then collapse — `research/OPTIMIZER.typ:624-642`. Collapse soundness holds; distribution over superposition holds "only for AFFINE recognizers" — `:649-655`.
- Loss-optimal enumeration: "generate each program once in non-increasing probability" (Heap Search) — `research/CONSTRAINED_PROGRAM_SEARCH.md:87`.
- Corrected constrained sampling: the Doob transform with h = Φ, and SMC with "Refuted zeroing a particle and Open leaving its weight alone" — `research/CONSTRAINED_PROGRAM_SEARCH.md:45-51`.
- Tempered posterior, `score = gate × exp(−β·cost)`, and WBIC — `archive/one-offs/SINGULAR_LEARNING_THEORY.md:294`, `:384-391`.
- A quantilizer as a priced sampler (`research/NATURE_OF_AGENTS.md:156`; `research/NATURE_OF_OPTIMIZERS.md:197`). A soft judge at temperature T (`research/NATURE_OF_AGENTS.md:173`). `dither` replaces randomness with a deterministic tally (`research/NATURE_OF_AGENTS.md:67-85`).
- A physical stochastic substrate. In `thermo=1` "every move is proposed as often as its reverse". Energy is a function of state; each site has a heat bank (a Creutz demon, a local energy store that pays for moves); a drive makes forward likelier; and "the ledger is exact" — `evaluators/rust-ic-strands/README.md:1562-1594`. "The drive is the speed-for-heat knob, with a threshold" — `:1609-1612`. Also `phys=1`, "every rewrite undoable while it is remembered, a drive that makes forward likelier than back" — `:1486-1497`.
- Adversarial challenge sampling: `cegis` keeps challenges that refuted earlier candidates — `lib/claim.disp:29-36`. `honest` searches a source of challenges — `lib/claim.disp:14-15`.

**(e) Library / learned abstraction**
- The memo snapshot. "Memo entries are calculus-level facts (`apply(f,x) = r` under the fixed rules), so kernel/library/elaborator changes never invalidate them — only an evaluator change does" — `evaluators/rust-eager/crate/src/snapshot.rs:1-5`. Each entry stores its cold cost (`memo_c`) and its "FRESH-FOREST PARENT" (`memo_p`), so a warm run prices hits exactly with no double count — `snapshot.rs:17-23`. Only facts costing at least `DISP_MEMO_MIN_COST` (default 1000 steps) are persisted — `test/disp.test.ts:147-149`. A generational shed policy keeps kernel-keyed entries across files — `evaluators/rust-eager/crate/src/memo.rs:5-10`. `cold_equiv` = steps + cold cost of hits — `CLAUDE.md:19`.
- The optimizer design calls memo the "degenerate, zero-intelligence corner" of rewriting and moves it out of the optimizer's substrate on purpose. ic-net drops automatic memo so that cost stays attributable per candidate — `research/OPTIMIZER.typ:113-132`, `:204-215`.
- Schematic library entries as parametric programs: `map_fusion := {f, g, xs} -> pair (map f (map g xs)) (map (compose f g) xs)`, where "matching the instantiated rule is `tree_eq`, not unification" — `research/OPTIMIZER.typ:535-552`. This is design syntax, not current disp.
- Ground versus schematic licenses: "disp's licenses today are ground facts ... A vocabulary of a handful of ground equations quotients almost nothing" — `research/THE_NATURE_OF_ABSTRACTION.typ:213`.
- The trusted atom library: the "rule lenses" are case split, generic run, induction, finite cover (`over`) and composition (`lens_then`). The three atoms are "proved by hand once" — `lib/rules.disp:1-13`.
- The plan to build a library learner: the "opt_q2 slice" is corpus MDL modulo licensed relations, babble-style anti-unification and a "realizability gate" — `research/THE_NATURE_OF_ABSTRACTION.typ:269-282`.
- Library-learning cautions already in the notes: "Library Learning Doesn't", where reuse was negligible — `research/constrained-program-search/typed-enumerative-synthesis.md:212-213`. Also "prospective compression": compressing the past "can overfit to already-solved holes" — `:218`, `:223`.

**(f) Prior / cost model**
- `score(e) = [typechecks(e, T)] × cheapness(e)` — `research/OPTIMIZER.typ:96-99`. Cost as a graded coeffect (the bound) with an effect dual (what a run spends) — `:149-181`.
- "The cost model *is* the online signal" — `research/OPTIMIZER.typ:220-223`. FOUNDATIONS Q4 asks: "Can the cost model be made hardware-faithful? ... interaction count is a space-biased proxy" — `FOUNDATIONS.md:214`.
- One over the prior (`research/NATURE_OF_OPTIMIZERS.md:31-39`); the library is the prior (`research/THE_NATURE_OF_ABSTRACTION.typ:196`). "Regularizers are prices" and the speed prior versus description-length prior — `research/NATURE_OF_AGENTS.md:148-169`, `:205`.
- Degeneracy-aware cheapness. A raw-size prior "systematically mis-prices exactly the programs disp most wants (compressible, reuse-heavy ones)" — `archive/one-offs/SINGULAR_LEARNING_THEORY.md:360-370`.

### Inferences

**Is the memo cache a learned library?** Partly. It is a library of ground instances (`apply(f,x)=r`), grown automatically from the workload. Each fact carries its cold cost, which is literally the "cached depth" value the abstraction essay assigns to a good library entry (`THE_NATURE_OF_ABSTRACTION.typ:117`). Persistence is gated by a cost threshold (an ACT-R-like tenure rule, `THE_NATURE_OF_ABSTRACTION.typ:177`). What it lacks is the abstraction step: no variables, no anti-unification, so it is DreamCoder's library before sleep-abstraction. The fresh-forest parent links give a call-tree "corpus" of recomputations that a Stitch/babble-style pass could mine for schematic entries. `cold_equiv` is then a ready-made compression or "search-saved" metric: steps saved by the library.

**Is reduction to normal form denoising?** Only in a narrow sense. A tree-calculus normal form plus hash-consing is the canonical representative of the reduction class (unevaluated redexes are the "noise"). It is not canonical for behavior (`dbl_step` versus `dbl_add`). Behavior-level denoisers in disp are user-chosen projections:
- quotient normalizers `nf`;
- case-table canonicalization (`canon`, `table`) on first-order recognizers;
- e-graph representative extraction (design only).

This matches the abstraction essay's "equivalence class + representative". A diffusion denoiser is the learned, soft, stochastic version: it projects toward high-density representatives instead of a fixed `nf`. The repo's own analogue of a restoring force is the inverter's gain > 1 and the bistable soft-judge magnet. A bit (a discrete latent) exists only where the dynamics contract toward attractors, and only below a critical noise or temperature (`inverter-chain.html:90,102`; `NATURE_OF_AGENTS.md:175-185`).

**Is coarse-graining renormalization or latent abstraction?** Yes, in the user's own framing. Israeli–Goldenfeld projections that commute with dynamics, and Mehta–Schwab's RG-to-deep-learning mapping, are cited as the flow picture (`THE_NATURE_OF_ABSTRACTION.typ:147-151`; `COARSE_GRAINED_LATTICE.md:155-160,348-372`). The inverter demo is a worked encoder (lens), predictor (coarse step rule), domain and error-bound pipeline. Its register is a periodic re-encoding that resets drift, which is functionally a denoise step interleaved with prediction.

**Does the optimizer essay define a world model of cost?** Yes. `run_model : Asm → Input → (Output, Cost)` is a type-checkable simulator of hardware, and the four rungs (static grade, intrinsic, modeled, measured) are a hierarchy of cost world models of increasing fidelity and decreasing trust (`OPTIMIZER.typ:735-776`). The essays already map Guaranteed-Safe-AI's "world model" onto disp's "cost model" (`NATURE_OF_OPTIMIZERS.md:185`). The coarse lattice law and `growth.disp` fits are learned or fitted predictors of that cost.

**World models versus library learning, in the user's terms.** Both are the "model" half of a two-part code (`THE_NATURE_OF_ABSTRACTION.typ:98-108`):
- A library compresses programs (the search prior, the generative side; DreamCoder's sleep-dreaming samples from it).
- A world model compresses dynamics (the coarse step rule plus lens, the predictive side).
- The reversible-pipeline split A ≅ B × R (`REVERSIBLE_PIPELINE.md:10-17`) is the invertible form of both: B is the latent or abstraction, R the residue or "noise".
- MDP homomorphisms (`THE_NATURE_OF_ABSTRACTION.typ:171`) are where they coincide: an abstraction that is also a world model because the square with dynamics commutes.

**Diffusion or Langevin as sampling a representative from the joint.** The closest repo objects are:
- the synthesis posterior and free energy F(T) (`SINGULAR_LEARNING_THEORY.md:298-317`);
- p(y|A=1) ∝ p₀(y)A(y) with the Doob h-transform / SMC twist Φ (`CONSTRAINED_PROGRAM_SEARCH.md:45-51,187-191`);
- the thermal strand lattice, a physical Metropolis/Creutz-demon dynamics with a drive (`evaluators/rust-ic-strands/README.md:1562-1612`).

Treating Φ as the discrete analogue of classifier guidance, and the lattice's drive plus thermal moves as a Langevin-like sampler, would be the report's inference.

**Concrete hooks / experiments (all unbuilt; none run here):**
1. **Memo-as-library experiment.** Mine the snapshot's fresh-forest parent tree (`snapshot.rs:17-23`) for recurring `(f, x)` shapes and anti-unify them into schematic candidates. Score each by `cold_equiv` delta on a fixed workload (`CLAUDE.md:19`: deltas are reliable even against a biased cache). That makes "library learning = compressing the cache" measurable with existing instrumentation.
2. **Commuting-square (JEPA-style) error for disp.** Port the inverter demo's "one step versus run alone" metric (`inverter-chain.html:103`) to the lattice-to-net projection or to `growth.disp` fits. Predict the next coarse state or step count from the current one, and report both errors.
3. **Temperature sweep.** Run the soft judge experiment (`NATURE_OF_AGENTS.md:234`) and the size-versus-step price sweep (`:235`) as the disp analogue of a noise schedule. Look for the critical points where abstractions (committed selves, bits) appear.
4. **Φ / twist learning over the `Trees` stream.** Implement SMC over prefix-ordered candidates with `honest`/`Verdict` as the potential (Refuted zeroes, Open keeps weight; `CONSTRAINED_PROGRAM_SEARCH.md:51`). Learned Φ is the discrete "guidance" term.
5. **Denoise-by-normalizer.** Treat `QuotC(Base, nf)` (`lib/confirm.disp:48`) as a hand-written denoiser. A learned or searched `nf` that maps programs to behavior representatives and passes `respects_nf` would be a verified, discrete "denoiser". The confirmer already certifies agreement, so it can act as the gate for any learned canonicalizer.
6. **Reverse reduction as generation.** Under the residue framing (`REVERSIBLE_PIPELINE.md:10-17`), running reduction backward from an answer with a sampled residue R generates programs in the answer's fibre. This is a disp-native "reverse process", testable on the lattice's `resid=1` mode (`evaluators/rust-ic-strands/README.md:1681`).

### Gaps
- No repo artifact implements or measures a learned encoder or predictor. Only hand-built lenses and fitted laws exist.
- No probabilistic sampler over programs is built; the size-ordered stream and the superposition prototype are deterministic. The SMC/Doob loop in `CONSTRAINED_PROGRAM_SEARCH.md` §7 is a proposal.
- Whether the strand lattice's thermal mode satisfies detailed balance (versus the drive deliberately breaking it) is described only qualitatively in the README; it was not checked here.

---

## Key question 3: disp vocabulary and existing stdlib names a report must use correctly

### Takeaway
The current, maintained surface is the claim layer plus `lib/` modules: prelude, list, stream, machine, verdict, claim, confirm, rules, trees, growth, theories, kernel. Many names in the optimizer and abstraction essays (`param_apply T e == Ok true`, `license_guard`, `two_face`, `q_lift`, `Refinement`, `std/relation.disp`, `lib/opt/rulebook.disp`) belong to the archived kernel or are design-only. Quote disp code only from tested files. `reduce` in `lib/list.disp` is list fold, not tree reduction.

### Cited Findings
- **Syntax shapes seen in tested code:** `name :: {arg : Type, ...} -> Ret :=> body`, `rec name :: ...`, `record name :: {...} -> Type`, `name := expr`, `{x, y} => body` lambdas, `test expr = expr`, method-call sugar (`xs.map(f)`, `v.fst`), `let x := e` inside braces, `if (c) { a } else { b }`, and `open use raw "lib/prelude.disp" {}` — `IDEAS.disp:1-68`, `lib/claim.disp:9-36`.
- **Prelude (`lib/prelude.disp`):** `triage` (`:74`), `pair` (`:103`), `not` (`:109`), `fix` (`:126`), `tree_eq` (`:145`), `succ` (`:178`), `nat_pred` (`:181`), `is_zero` (`:185`); the leaf is `t`; `true = t`, `false = t(t)` — `README.md:47-49`.
- **List (`lib/list.disp`):** `reduce` is fold over a list (`:9`); `cons` (`:5`), `is_nil` (`:6`), `head` (`:7`), `tail` (`:8`), `map` (`:13`), `append` (`:14`), `concat` (`:15`), `reverse` (`:16`), `length` (`:17`), `filter` (`:18`), `all` (`:19`), `any` (`:20`), `dedup` (`:22`), `cross` (`:23`), `zip` (`:24`).
- **Stream (`lib/stream.disp`):** coalgebras `c_new`/`c_fold` (`:6`, `:14`), `emit` (`:36`), `stream_new` (`:59`), `nats` (`:63`), `as_items` (`:64`), `s_from_list` (`:66`), `s_map` (`:70`), `s_filter` (`:72`), `bounded` (`:84`), `settle`/`stopping_list` (`:90`, `:95`), `s_items`/`s_rest`/`s_take` (`:98-100`), `s_all` (`:111`), `m_trace` (`:120`).
- **Trees (`lib/trees.disp`):** `Trees :: Stream(Tree)` (`:24`) and `Bool` (`:25`).
- **Machine (`lib/machine.disp`):** `m_start` (`:20`), `m_step` (`:59`), `m_advance` (`:86`), `m_finished`, `m_result` (`:17-19`).
- **Verdict (`lib/verdict.disp`):** `refuted` (`:8`), `counterexample` (`:9`), `open_on` (`:10`), `is_proved` (`:11`), `agrees` (`:18`); verdicts are "Proved" | `refuted w` | "Open".
- **Claim (`lib/claim.disp`):** `honest` (`:14`), `over` (`:16`), `lens` (`:19`), `lens_holds` (`:21`), `lens_then` (`:23`), `call_with` (`:25`), `implies` (`:27`), `cegis` (`:30`).
- **Confirmer (`lib/confirm.disp`):** `ArrowC` (`:47`), `QuotC` (`:48`), `Parity`/`All` (`:63-64`), `hole` (`:109`), `piece` (`:135`), `answered`/`need`/`stuck`/`coarse` (`:185-189`), `table` (`:436`), `canon` (`:438`), `search` (`:594`), `run` (`:596`), `respects_nf` (`:635`), `eq_by_induction` (`:679`).
- **Rules (`lib/rules.disp`):** `by_search := honest(Trees.as_items)` (`:24`), `split_lens` (`:42`), `certify` (`:72`), `judge` (`:156`), `holds` (`:162`).
- **Growth (`lib/growth.disp`):** `steps_of` (`:14`), `curve` (`:17`), `fit` (`:45`), `bound_of` (`:47`), `cheaper` (`:49`).
- **Decision (`lib/kernel/decision.disp`):** `united` (`:156`), Pareto "single-agent-ness" after Demski — `:1-15`, `:150-156`.
- **Fixtures:** `NatR` (`lib/tests/fixtures.disp:9`), `dbl_step` (`:24`), `add` (`:25`).
- **Names present only in archive or design:** `param_apply` appears in `lib/` only as an export-check field (`lib/prelude.disp:31-32`), while `archive/` uses it 70 times. `two_face`, `license_guard`, `q_lift` and `Refinement` have 0 hits in `lib/*.disp` (grep counts this session); the kernel has a "TwoFace" tier per `README.md:192`.
- A tested, honest-search example the report can reuse verbatim: `test by_search(nat_fn, {n} => pair(n, n), 60) = refuted(1)` — `README.md:75-76`.

### Inferences
- For illustration the report should reuse tested snippets: `IDEAS.disp:53-59` (behaviors via `dedup`), `README.md:75-84` (honest search), `README.md:113-128` (`eq_by_induction`), or `lib/confirm.disp:63` (`Parity := QuotC(NatR, parity)`). Do not invent `sample`, `denoise` or `prior` functions; none exist in `lib/`.
- The disp snippets in `archive/one-offs/SINGULAR_LEARNING_THEORY.md:302-329` are archived-kernel or explicitly "SCHEMATIC" and should not be shown as correct disp.

### Gaps
- `parity` and `always_zero` were not traced to their definitions. They are used in `lib/confirm.disp:63-64`; they likely live in fixtures or theories but were not verified.

---

## Key question 4: Docs that look out of date against the current code

### Takeaway
The abstraction and optimizer essays describe the archived kernel's licensing machinery; the optimizer document says so itself. Several cross-references point to deleted files. The coarse-lattice note's fixed-unit sections were superseded by later commits. `EVALUATOR.md` omits the persistent memo and `cold_equiv`, which are documented only in code and `CLAUDE.md`. The `.typ` files should be updated, per the repo rule; the `.md` notes self-declare their deletion conditions.

### Cited Findings
- `research/OPTIMIZER.typ`: the status note says the licensing layers it cites "ride the kernel now archived at `archive/live-kernel/`; they have not yet been re-landed" (`:30-33`). It points to `lib/opt/rulebook.disp` (`:558`, which does not exist) and `std/relation.disp` (`:792`, archived). Its pipeline uses `param_apply T e == Ok true` (`:92`, `:575`), the archived API. Recommendation: update (typ file) to the claim/lens layer; the design ideas stay valid.
- `research/THE_NATURE_OF_ABSTRACTION.typ`: it describes "two Sigma-ops (`bind_hyp`, `hyp_reduce`) ... about 232 lines" (`:21`), plus `license_guard`/`two_face_guard` (`:197`, `:209`), `two_face` and `PositiveFnCoverage` (`:237-241`), and `Quotient`/`q_lift` (`:173`). All belong to the archived kernel; none of `license_guard`, `two_face` or `q_lift` is in `lib/`. Recommendation: keep the philosophy and math (§§2–4), and update the design chapters (§§5–8) to claims and lenses.
- `research/NATURE_OF_OPTIMIZERS.md:14` cites `research/LOCAL_SYNTH.md`, which is missing. The file's own deletion condition is at `:3`. Recommendation: fix the dangling reference; keep until the optimizer exists.
- `research/COARSE_GRAINED_LATTICE.md` (last commit 2026-10-11, `dc6ab4f`: "rust-ic-strands: take fixed units out again: they are not local"): the "stuck silicon" sections (`:24-27`, `:47-60`, §3–5) describe a mechanism the strands code removed. The note itself says the constants "must be refitted for each design" (`:370-372`), and the strands thermo model changed afterwards (commits 2026-10-19). Recommendation: keep §1–2 (the coarse law and its method); trim or mark §3–5 as historical.
- `research/COMBINATOR_SEARCH_PLAN.md:400`, `:412` and `research/NATURE_OF_AGENTS.md:128` cite `research/REVERSIBLE_CORE.md`. The file is deleted but correctly referenced via `git show d7e3b96:...`. No action needed.
- `archive/one-offs/SINGULAR_LEARNING_THEORY.md`: its disp snippets (`:302-329`) use archived names (`param_apply`, `Refinement`, `Ok`/`Err`). It is already archived; fine as a reference, but its code is not current disp.
- `FOUNDATIONS.md:7` self-flags that entries citing `archive/live-kernel/…` describe the retired kernel and that "Q5's success and falsifier lines are still the retired kernel's". It also flags an unverified "4,000–67,000×" figure (`:212`).
- `EVALUATOR.md` describes backends and the ABI (`:13-55`) but not the persistent memo snapshot, fresh-forest parents or `cold_equiv`. Those live in `evaluators/rust-eager/crate/src/snapshot.rs:1-26`, `memo.rs:1-16` and `CLAUDE.md:19`. This is an omission, not an error; a one-paragraph addition would help.
- `research/CLAIMS_OVER_TREES.md:8-11` claims its code blocks run against `lib/prelude.disp`, `lib/list.disp`, `lib/machine.disp`, `lib/verdict.disp`, `lib/stream.disp`, `lib/tests/fixtures.disp` and `IDEAS.disp`, with 25 pins passing as of 2026-09-27. This was not re-run (read-only constraint), so it is unverified against the current, uncommitted `lib/` edits.

### Inferences
- The repo's live line of work (claims, lenses, the confirmer, growth fits, the thermodynamic lattice) is newer than the essays that frame the optimizer and library learning. A report connecting world models and library learning to disp should anchor on `lib/claim.disp`, `lib/confirm.disp`, `lib/growth.disp`, the memo snapshot and the coarse-graining demo, rather than on archived kernel names.

### Gaps
- No tests were run, so whether quoted snippets still pass on the current working tree (with uncommitted edits to `lib/confirm.disp`, `lib/rules.disp` and `lib/theories/types.disp`) is unverified.
