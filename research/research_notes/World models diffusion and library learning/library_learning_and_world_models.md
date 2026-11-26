# Library learning and world models: shared skeleton, program world models, denoising over programs, critiques

Scope: library learning (learning reusable program abstractions), world models written as programs, compression as prediction, abstraction in RL, and denoising over programs. Tweedie/score-matching math, JEPA vs. denoiser architectures, Langevin/energy sampling and diffusion world models belong to sibling researchers and are only touched on here.

Verification: every quote below was located with `scripts/verify-source.sh -q` (batch mode `-Q`) on 2026-11-05, and every link is the fragment link the script minted. Format of each finding: claim, then the exact verified quote, then the link. Nothing is marked UNVERIFIED because every source relied on was verified. Two papers are named only as context (TroVE and LEGO-Prover, both via the critique paper that studies them); their own PDFs were not fetched.

## Q0. The user's question: can a generalized world model (physical prediction + denoising prediction) be framed as library learning? What is the relationship?

### Takeaway
Partly, yes. Both live inside one Bayesian skeleton, "wake-sleep": a generative model (prior over latent explanations plus a likelihood) and a learned recognition model that inverts it. A denoiser is the recognition half trained on (corrupted, clean) pairs from a fixed, known corruption process. Library learning is the other half: it rewrites the generative model's prior itself (which building blocks are cheap) to minimize total description length (MDL). So "world model as library learning" means: the world model's prior over explanations (programs for transitions, or repair moves for corruptions) is a learned library, and learning it is compression of the explanations found so far. No paper found does library learning over denoising trajectories, and no program world model found reuses a compressed library across many environments in the DreamCoder sense (TheoryCoder-2's curriculum comes closest).

### Cited Findings
- DreamCoder states the skeleton directly: wake-sleep trains a generative model that is the learner's prior plus a recognition network that inverts it. Quote: "a neural network recognition model that learns to invert this generative model" — [DreamCoder p4](https://arxiv.org/pdf/2006.08381#page=4)
- In DreamCoder the library is the generative model over both programs and tasks. Quote: "Its learned language defines a generative model over programs and tasks" — [DreamCoder p4](https://arxiv.org/pdf/2006.08381#page=4)
- The abstraction-sleep objective is literally MDL. Quote: "is equivalent to minimizing the description length of the" [library plus the description lengths of refactorings of programs found during waking] — [DreamCoder p7](https://arxiv.org/pdf/2006.08381#page=7)
- Tree diffusion is a denoiser over programs: the noise is random small edits to syntax trees, and a network learns to undo them. Quotes: "we randomly make small mutations to the syntax trees of programs"; "train a conditional neural model to invert these small mutations" — [Kapur et al. 2024 p3](https://arxiv.org/pdf/2405.20519#page=3)
- Program world models already write the physical transition model as code. WorldCoder: "builds a Python program representing its knowledge of the world" — [WorldCoder p1](https://arxiv.org/pdf/2402.12275#page=1). PoE-World uses a product of many small programs: "exponentially-weighted product of programmatic experts" — [PoE-World p1](https://arxiv.org/pdf/2505.10819#page=1)
- The closest system to "library learning for a world model" keeps an explicit library of abstract states and actions and grows it over a curriculum. Quotes: "a set of PDDL program files representing a library of abstract states and actions"; "gradually growing the library of abstractions through a curriculum" — [TheoryCoder-2 p3](https://arxiv.org/pdf/2602.00929#page=3)
- 2026 coding-agent world models for ARC-AGI-3 (the interactive game benchmark) use MDL-like refactoring inside one game but explicitly do no cross-task library learning. Quote: "they do not perform formal MDL optimization or cross-task library learning" — [Rodionov 2026b p4](https://arxiv.org/pdf/2607.15439#page=4)

### Inferences
- A clean formal mapping (my synthesis of the sources above, not a claim any single paper makes):
  - Latent z = an explanation: a program rho for a task in DreamCoder, a transition program T in WorldCoder, or the clean signal x_0 in a denoiser.
  - Prior p(z | L) = the library L. DreamCoder learns it. A Gaussian or masked diffusion model fixes the forward corruption process and learns the data prior only implicitly, through the denoiser.
  - Likelihood p(x | z) = executing the program (DreamCoder, WorldCoder) or the corruption channel q(x_t | x_0) (diffusion).
  - Recognition q(z | x) = DreamCoder's Q(rho | x), the inference-compilation network, or the denoiser D(x_t) ≈ x_0. All three are trained on pairs from the joint.
  - Library learning = the "abstraction sleep" M-step: re-parameterize p(z | L) so the corpus of found explanations is short. Diffusion training has no such step. Its "vocabulary" of moves (the noise kernel) is fixed by hand.
- So a "generalized world model" that does both physical prediction and denoising can be framed as library learning by making the library the prior over both kinds of explanation:
  - "physical" explanations: programs that map (s, a) to s';
  - "repair" explanations: edit sequences that map a corrupted s back to a clean s.
  Then compress both corpora with one MDL objective. Shared sub-programs (object types, laws, canonical edits) would be the abstractions.
- Physical prediction and denoising differ in one way that matters for library learning. Physical dynamics come from the world, so their regularities are worth compressing. Denoising moves come from a noise kernel the designer chose, so their regularities partly mirror the kernel rather than the world. Any library learned from the repair moves would pick up the kernel's design choices.

### Gaps
- No paper found that runs library learning (Stitch-, babble- or DreamCoder-style compression) on denoising or repair trajectories, i.e. "common denoising moves → learned rewrite library". This remains a design hypothesis.
- No paper found that trains one model for both next-state prediction and program denoising under a shared learned library.

## Q1. Wake-sleep as the shared skeleton; is DreamCoder's dream phase literally sampling the joint to train an amortized inverse?

### Takeaway
Yes, with one twist. Hinton et al.'s sleep phase trains recognition weights on "fantasies" sampled from the generative model. DreamCoder's dream phase samples programs from the library, executes them to create tasks, and trains the recognition model on those (task, program) pairs, mixed 50/50 with "replays" of real solved tasks. The twist: DreamCoder does not always regress onto the sampled program. It re-solves the dreamed task with search and trains Q toward the MAP program, which makes it closer to self-training on synthetic data than to plain joint sampling. Like a denoiser trained on synthetic corruptions, it is an amortized inverse trained on simulated data. Unlike diffusion, the simulator (the library) is itself learned.

### Cited Findings
- **Wake-sleep (Hinton, Dayan, Frey, Neal 1995).** Wake phase: "phase, neurons are driven by recognition connections" — [ws.pdf p1](https://www.cs.toronto.edu/~hinton/absps/ws.pdf#page=1). Sleep phase: "phase, neurons are driven by generative connections" — [ws.pdf p1](https://www.cs.toronto.edu/~hinton/absps/ws.pdf#page=1)
- Sleep samples are samples from the model's joint. Quote: "These fantasies provide an unbiased sample of the network['s generative model of the world]" — [ws.pdf p4](https://www.cs.toronto.edu/~hinton/absps/ws.pdf#page=4)
- The known flaw, which reappears in DreamCoder and in any denoiser trained on synthetic data: recognition is fit to fantasies, not to data. Quotes: "the sleep phase optimizes the recognition weights for fantasy data"; "Early in the learning, fantasies will have a quite different distribution" [than the training data] — [ws.pdf p4](https://www.cs.toronto.edu/~hinton/absps/ws.pdf#page=4)
- **Helmholtz machine (Dayan, Hinton, Neal, Zemel 1995).** The learned generative model is used to train the recognition model. Quote: "a separate generative model, which is also learned, is used to train the recognition model" — [hm95.pdf p1](https://www.gatsby.ucl.ac.uk/~dayan/papers/hm95.pdf#page=1)
- **DreamCoder (Ellis et al.)** explicitly inherits this. Quote: "in the spirit of the Helmholtz machine" — [DreamCoder p5](https://arxiv.org/pdf/2006.08381#page=5)
- Overall loop: "trains the neural network on imagined and replayed problems" — [DreamCoder p1](https://arxiv.org/pdf/2006.08381#page=1)
- Fantasies come from the library acting as a generative model. Quote: "programs sampled randomly from the learned library as a generative model" — [DreamCoder p5](https://arxiv.org/pdf/2006.08381#page=5)
- The naive version is exactly joint sampling. Quote: "execute it to generate a task, and train the recognition network to predict the sampled program from the sampled task" — [DreamCoder p7](https://arxiv.org/pdf/2006.08381#page=7)
- DreamCoder modifies the naive version: it solves dreamed tasks with the same search used while awake and trains toward the MAP program. Quotes: "We instead think of dreaming as creating an endless stream of random problems"; "which we then solve during sleep in an active process using the same program search process"; "we train Q to perform MAP inference" — [DreamCoder p7](https://arxiv.org/pdf/2006.08381#page=7)
- Replays guard against the fantasy-distribution mismatch, and fantasies give diversity. Quotes: "Replays ensure that the recognition model is trained on the actual tasks"; "fantasies provide a large and highly varied dataset to learn from" — [DreamCoder p7](https://arxiv.org/pdf/2006.08381#page=7). The mix: "We train on a 50/50 mix of replays and fantasies" — [DreamCoder p8](https://arxiv.org/pdf/2006.08381#page=8)
- A grown library produces new composite structures when dreaming. Quote: "the model fantasizes complex structures it has not seen during waking" — [DreamCoder p12](https://arxiv.org/pdf/2006.08381#page=12)
- Abstraction sleep uses version spaces and e-graph ideas to represent many refactorings at once. Quote: "combining ideas from version space algebras" — [DreamCoder p7](https://arxiv.org/pdf/2006.08381#page=7). It refactors programs semantically, not just syntactically: "we refactor the programs to expose reused semantic patterns" — [DreamCoder p7](https://arxiv.org/pdf/2006.08381#page=7)
- Same pattern outside program synthesis. **Inference compilation (Le, Baydin, Wood 2017)** trains an amortized proposal on unlimited samples from a probabilistic program's joint. Quotes: "trained with an infinite stream of training data sampled from the generative model" — [Le et al. p2](https://arxiv.org/pdf/1610.09900#page=2); "allow the trained neural network to be used as a proposal distribution" — [Le et al. p1](https://arxiv.org/pdf/1610.09900#page=1)
- **Memoised Wake-Sleep (Hewitt, Le, Tenenbaum 2020)** replaces pure dreaming with a memory of the best discovered programs. Quote: "extends Wake Sleep by explicitly storing and reusing the best programs discovered" — [MWS p1](https://arxiv.org/pdf/2007.03132#page=1)

### Inferences
- Dictionary, from wake-sleep to diffusion: fantasy ↔ synthetic noisy sample; recognition model ↔ denoiser or amortized posterior; replay ↔ real data pushed through the noise kernel. Diffusion training is "replay only": every training pair starts from a real x_0, and the "generative" part is just the fixed corruption kernel. So it avoids the fantasy-mismatch problem Hinton et al. flag, but it never learns the generative model's building blocks.
- DreamCoder's "re-solve the dream" variant means its recognition model learns to imitate the searcher's output (MAP under the current library), not the true sampled latent. That is a self-distillation of search, closer to expert iteration than to denoising score matching.
- For a disp-style design, the analogue of DreamCoder's dream phase is: sample terms from the current library prior, run or normalize them, and train a guide to recover the source term from the result. That is a denoiser whose noise process is "evaluation under the library", a different object from tree diffusion's random-mutation noise.

### Gaps
- No source found gives a formal equivalence proof between DreamCoder's dream-sleep objective and a denoising objective. The correspondence above is structural, not a published theorem.

## Q2. Library learning methods and the objective they optimize

### Takeaway
The mainline methods (DreamCoder, Stitch, babble, LILO, LAPS, ShapeCoder) all optimize compression of a corpus of programs under the library, i.e. MDL. They differ in search strategy (bottom-up refactoring, top-down synthesis, e-graph anti-unification) and in what counts as "the same" code (syntax vs. modulo an equational theory). LLM-era systems (ReGAL, LILO's AutoDoc) add naming and refactoring by an LLM. Human-cognition work in 2026 argues the objective should be prospective (compress future tasks), not retrospective.

### Cited Findings
- **DreamCoder.** The objective is MDL over library plus refactored corpus — [DreamCoder p7](https://arxiv.org/pdf/2006.08381#page=7) (quote in Q0).
- **Stitch (Bowers et al., POPL 2023).** Method: "corpus-guided top-down synthesis" — [Stitch p1](https://arxiv.org/pdf/2211.16605#page=1). Objective: "a good abstraction is one which minimizes the size of the corpus" — [Stitch p3](https://arxiv.org/pdf/2211.16605#page=3). Speed vs. DreamCoder's compressor: "Stitch is 3-4 orders of magnitude faster" — [Stitch p1](https://arxiv.org/pdf/2211.16605#page=1)
- **babble (Cao et al., POPL 2023).** "Library learning compresses a given corpus of programs by extracting common structure" — [babble p1](https://arxiv.org/pdf/2212.04596#page=1). It adds an equational theory: "library learning modulo theory" — [babble p1](https://arxiv.org/pdf/2212.04596#page=1). Motivation: "a purely syntactic library learning approach cannot generate this abstraction" [e.g. λx. x + 1 when addition commutes] — [babble p2](https://arxiv.org/pdf/2212.04596#page=2). Result: "babble achieves better compression orders of magnitude faster than the state of the art" — [babble p1](https://arxiv.org/pdf/2212.04596#page=1)
- **LILO (Grand et al., ICLR 2024).** "iteratively synthesizes, compresses, and documents code" — [LILO p1](https://arxiv.org/pdf/2310.19791#page=1). It names abstractions with an LLM: "infers natural language names and docstrings" — [LILO p1](https://arxiv.org/pdf/2310.19791#page=1). Naming helps the synthesizer itself: "AutoDoc boosts performance by helping" [LILO's synthesizer to interpret and deploy learned abstractions] — [LILO p1](https://arxiv.org/pdf/2310.19791#page=1)
- **LAPS (Wong et al., ICML 2021).** Language annotations guide "joint learning of libraries and neurally-guided search models" — [LAPS p1](https://arxiv.org/pdf/2106.11053#page=1)
- **ShapeCoder (Jones et al. 2023).** Library learning for 3D shape programs. Quote: "automatically discovers abstraction functions, and infers visual programs that use these abstractions" [to compactly explain an input dataset of shapes] — [ShapeCoder p1](https://arxiv.org/pdf/2305.05661#page=1)
- **ReGAL (Stengel-Eskin et al. 2024).** LLM-driven refactoring: "learning a library of reusable functions via code refactorization" — [ReGAL p1](https://arxiv.org/pdf/2401.16467#page=1)
- **2026, prospective compression.** "Existing algorithms treat library learning as retrospective compression over a static task distribution" — [Hernandez Cano et al. 2026 p1](https://arxiv.org/pdf/2605.09985#page=1). Human behaviour "cannot be captured by existing retrospective compression-based algorithms" — [same, p1](https://arxiv.org/pdf/2605.09985#page=1)
- **2026, online library learning in people.** The authors "point to online library learning as a core mechanism in human problem solving" — [Zhao et al. 2026 p1](https://arxiv.org/pdf/2603.23244#page=1)

### Inferences
- The objective is always some version of min over L of [ -log P(L) + sum over tasks of min over refactorings rho of -log P(x | rho) P(rho | L) ]. Methods differ in how they search over L and over refactorings.
- babble's "modulo theory" makes abstractions range over equivalence classes of programs, a quotient. That is the program-side analogue of state aggregation in RL (Q6): two fragments count as "the same" if an equational theory equates them, just as two states count as the same if bisimulation equates them.
- Prospective compression is the library-learning analogue of value equivalence (Q6): pick abstractions for what they will buy later, not for how well they reconstruct the past.

### Gaps
- Exact Stitch utility formula details (rewrite cost, arity penalties) were not extracted. Only the "minimizes the size of the corpus" statement was verified.

## Q3. World models as programs; does any of them learn a reusable library across environments?

### Takeaway
A sizeable line of work learns world models as code: BPL, theory learning as stochastic search, AutumnSynth, WorldCoder, Code World Models, PoE-World, One Life, OPINE-World, TheoryCoder/-2, and the 2026 ARC-AGI-3 coding agents. Most learn one model per environment, and several explicitly isolate environments. Cross-environment reuse appears as:
- WorldCoder editing old code;
- PoE-World's experts generalizing to new levels of the same game;
- TheoryCoder-2 growing a PDDL library of abstract states and actions over a curriculum (the only one that is library learning in the strict sense).
None of them uses Stitch- or DreamCoder-style MDL compression of a multi-environment corpus.

### Cited Findings
- **BPL (Lake, Salakhutdinov, Tenenbaum 2015).** "This paper introduces the Bayesian program learning (BPL) framework" — [Lake et al. p2](https://www.cs.cmu.edu/~rsalakhu/papers/LakeEtAl2015Science.pdf#page=2). Reuse is built into its prior: "new programs by reusing the pieces of existing ones" — [Lake et al. p2](https://www.cs.cmu.edu/~rsalakhu/papers/LakeEtAl2015Science.pdf#page=2)
- **Theory learning as stochastic search (Ullman, Goodman, Tenenbaum 2012).** Theories are "sets of logical laws generated by a probabilistic context-free grammar" — [MIT DSpace record](https://dspace.mit.edu/entities/publication/0e042702-d4f0-4c95-a14b-1d5b648a8d4c#cris-layout-leading) (author manuscript; publisher DOI 10.1016/j.cogdev.2012.07.005 blocks fetches)
- **The child as hacker (Rule, Tenenbaum, Piantadosi 2020).** "learning as programming" — [Rule et al. p1](https://par.nsf.gov/servlets/purl/10226808#page=1). Code as representation: "provide the best formal knowledge representation we have" — [Rule et al. p1](https://par.nsf.gov/servlets/purl/10226808#page=1)
- **AutumnSynth (Das, Tenenbaum, Solar-Lezama, Tavares, POPL 2023).** Program synthesis for causal dynamics: the automata step "adds any additional latent state necessary to fully account for the observations" — [Das et al. p1](https://par.nsf.gov/servlets/purl/10403561#page=1) (NSF repository copy; ACM DOI 10.1145/3571249 blocks fetches)
- **WorldCoder (Tang, Key, Ellis 2024).** "builds a Python program representing its knowledge of the world" — [WorldCoder p1](https://arxiv.org/pdf/2402.12275#page=1).
  - Cross-environment transfer is by editing code: "it can transfer its knowledge across environments by editing its code"; "allows sample-efficient transfer across tasks by reusing pieces of old programs" — [WorldCoder p1](https://arxiv.org/pdf/2402.12275#page=1)
  - Learning objective: "favors optimism in the face of (world model) uncertainty" — [WorldCoder p2](https://arxiv.org/pdf/2402.12275#page=2)
- **Code World Models (Dainese et al. 2024).** "world models generated by a Large Language Model" [in Python, for model-based RL] — [Dainese et al. p1](https://arxiv.org/pdf/2405.15383#page=1). **Code World Models for General Game Playing (Lehrach et al. 2025):** the generated code "serves as a verifiable simulation engine for high-performance planning algorithms" — [Lehrach et al. p1](https://arxiv.org/pdf/2510.04542#page=1)
- **PoE-World (Piriyakulkij et al. 2025).**
  - Learns stochastic models from little data: "learn complex, stochastic world models from just a few observations" — [PoE-World p1](https://arxiv.org/pdf/2505.10819#page=1). Evaluated on "Montezuma's Revenge" — [PoE-World p1](https://arxiv.org/pdf/2505.10819#page=1)
  - Modularity: "because we no longer search for a single monolithic" [program] — [PoE-World p2](https://arxiv.org/pdf/2505.10819#page=2)
  - Generalization: the experts "encode abstract knowledge that generalizes to new game levels" — [PoE-World p2](https://arxiv.org/pdf/2505.10819#page=2)
- **Doing experiments and revising rules (Piriyakulkij, Langenfeld, Le, Ellis 2024).** A model of how to "infer natural language rules by doing experiments" — [Piriyakulkij et al. p1](https://arxiv.org/pdf/2402.06025#page=1)
- **One Life (Khan et al., ICLR 2026).** Laws as small conditional programs: "conditionally-activated programmatic laws within a probabilistic programming framework" — [One Life p1](https://arxiv.org/pdf/2510.12088#page=1). Built on "our reimplementation of the popular Crafter environment" — [One Life p1](https://arxiv.org/pdf/2510.12088#page=1)
- **TheoryCoder-2 (Ahmed, Irie, Tenenbaum, Bates, Gershman 2026)**, the cross-environment library case.
  - It fixes TheoryCoder's reliance on hand-written abstractions. The predecessor (TheoryCoder, ["Synthesizing world models for bilevel planning"](https://arxiv.org/pdf/2503.20124#page=1), TMLR 2025) is described as follows: "they heavily rely on human-provided abstractions and sidestep the abstraction-learning problem" — [TheoryCoder-2 p1](https://arxiv.org/pdf/2602.00929#page=1)
  - Its goal is to "actively learn reusable abstractions rather than relying on hand-specified ones" — [TheoryCoder-2 p1](https://arxiv.org/pdf/2602.00929#page=1)
  - The library is "a set of PDDL program files representing a library of abstract states and actions", grown by "gradually growing the library of abstractions through a curriculum" — [TheoryCoder-2 p3](https://arxiv.org/pdf/2602.00929#page=3). The paper has an experiment section, "Transferring abstractions to harder problems" — [TheoryCoder-2 p5](https://arxiv.org/pdf/2602.00929#page=5)
- **ARC-AGI-3 coding agents (2026)** isolate games, so no library carries across them.
  - Rodionov: "refactors it toward simpler abstractions as a practical proxy for an MDL-like simplicity bias"; each playthrough runs "with no access to files or conversation state from earlier playthroughs"; with GPT-5.5 the agent "fully solved 15 games and achieved a mean per-game RHAE of 58.12%" — [Rodionov 2026a p1](https://arxiv.org/pdf/2605.05138#page=1)
  - Tycho: "Independent runs use separate workspaces and never share information across games" — [Tycho p12](https://arxiv.org/pdf/2607.28287#page=12); "GPT-5.6 Sol reaches 100.00 RHAE, completing all 25 games and all 183 levels" — [Tycho p1](https://arxiv.org/pdf/2607.28287#page=1); "Opus 5 uses 61% fewer scored actions than the aggregate official human baselines" — [Tycho p1](https://arxiv.org/pdf/2607.28287#page=1)
  - Tycho also cites a known failure mode on ARC-AGI-3: agents "complete one level without extracting mechanics that transfer to the next" — [Tycho p2](https://arxiv.org/pdf/2607.28287#page=2)
  - OPINE-World extends WorldCoder for when the symbolic state is unknown, raising Claude Opus 4.8 "from 1.5% to 78.40% on the ARC-AGI-3 benchmark" — [OPINE-World p1](https://arxiv.org/pdf/2607.01531#page=1)

### Inferences
- PoE-World experts and One Life laws are already "library-shaped": a world model is a weighted product or mixture of small reusable programs. What is missing is a compression step that merges recurring experts or laws across environments into shared, parameterized abstractions (Stitch or babble run over a corpus of expert programs).
- TheoryCoder-2's library of "abstract states and actions" is the most explicit bridge between library learning and world models. The library is simultaneously:
  - a state abstraction (PDDL predicates), and
  - an action abstraction (PDDL operators).
  This is the coarse-grained world model a library induces (see Q4 and Q5).
- ARC-AGI-3 isolates games by protocol to avoid leakage, so these benchmarks currently cannot measure cross-environment library learning. That is a design limit of the evaluation, not evidence against the idea.

### Gaps
- Whether TheoryCoder-2's learned abstractions transfer across different games (as opposed to harder levels of a curriculum within a domain family) was not verified in detail.
- No 2025–2026 paper found that runs MDL library learning (Stitch, babble) over a corpus of world-model programs from many environments.

## Q4. Compression = prediction; why a library is a better prior; is a library a coarse-grained state space?

### Takeaway
Solomonoff, MDL, Hutter and Delétang et al. establish that a good predictor is a good compressor and vice versa. A library is a re-parameterization of the prior over programs: it makes frequent structure short, i.e. probable. Learning a library is therefore learning a better prior (a better generative world model) over the space of explanations. On the "coarse-graining" reading, library abstractions act like latent variables or macro-states: they name equivalence classes of concrete structure. The direct evidence for this reading is TheoryCoder-2's "library of abstract states and actions" and babble's equivalence-modulo-theory.

### Cited Findings
- **Solomonoff (1964).** Probabilities assigned from "the lengths of inputs to a universal Turing machine that are required to produce the sequence" [of interest] — [Solomonoff p1](https://raysolomonoff.com/publications/1964pt1.pdf#page=1)
- **Hutter (2000).** "theory of universal induction formally solves the problem" [of sequence prediction] — [Hutter p1](https://arxiv.org/pdf/cs/0004001#page=1)
- **MDL tutorial (Grünwald 2004).** "any regularity in the data can be used to compress the data"; "the more we are able to compress the data, the more" [we have learned about the data] — [Grünwald p5](https://arxiv.org/pdf/math/0406077#page=5)
- **Schmidhuber (2008/2009).** Curiosity as improvement in compression: "the first derivative of subjective beauty or compressibility" — [Schmidhuber p1](https://arxiv.org/pdf/0812.4360#page=1)
- **Language modeling is compression (Delétang et al., ICLR 2024).** "predictive models can be transformed into lossless compressors and vice versa" — [Delétang et al. p1](https://arxiv.org/pdf/2309.10668#page=1). Example: Chinchilla 70B "compresses ImageNet patches to 43.4% and LibriSpeech samples to 16.4%" [of raw size] — [Delétang et al. p1](https://arxiv.org/pdf/2309.10668#page=1)
- **World Models (Ha & Schmidhuber 2018)**, the neural-world-model framing that sibling researchers cover: "learn a compressed spatial and temporal representation" — [Ha & Schmidhuber p1](https://arxiv.org/pdf/1803.10122#page=1)
- **The library as a prior.** DreamCoder's library "defines a generative model over programs" — [DreamCoder p4–5](https://arxiv.org/pdf/2006.08381#page=4). The MDL equivalence is in Q0.

### Inferences
- Why library = prior. Under a library L, a program's code length is -log P(rho | L). Adding an abstraction that captures reused structure shortens every program that uses it, so it raises the prior mass on the world's typical structure. That is the same move as a better predictive model (Delétang et al.), applied to programs instead of tokens. Library learning is therefore "learning a world model over explanations".
- Coarse-graining. An abstraction f = λx. body groups every concrete fragment body[x := e] under one name. Its argument slots are latent variables, and its body is a shared mechanism. In a world model the analogous object is a macro-state (an abstract predicate) plus a macro-transition (an operator). TheoryCoder-2 makes this explicit. babble's e-graphs make the "grouping" a true quotient by an equational theory.
- MDL picks abstractions that are cheap to describe and often reused. RL abstraction (Q6) picks groupings that preserve value or behaviour. These criteria can disagree: a compressible regularity can be useless for control, and vice versa. That is the core tension in "library learning as world-model learning".

### Gaps
- No primary source found that formally proves "library abstractions = latent state abstraction". The link is argued by analogy plus TheoryCoder-2's concrete design.
- Rissanen's 1978 paper (Automatica) was not fetched (publisher blocks). Grünwald's tutorial stands in for MDL.

## Q5. Temporal abstraction: options, skill libraries, hierarchical world models

### Takeaway
On the action side, reusable sub-programs correspond to options (temporally extended actions). Skill libraries (Voyager), action-abstraction libraries (Ada, TheoryCoder-2) and MDL-learned options (LOVE) are library learning applied to behaviour. Hierarchical world models (Director) learn the matching high-level latent space. So a library of sub-programs is the action-side analogue of a latent state abstraction, and the strongest systems learn both together (PDDL predicates plus operators).

### Cited Findings
- **Options (Sutton, Precup, Singh 1999).** Options are "closed-loop policies for taking action over a period of time"; "options may be used interchangeably with primitive actions" — [Sutton et al. p1](https://people.cs.umass.edu/~barto/courses/cs687/Sutton-Precup-Singh-AIJ99.pdf#page=1)
- **Voyager (Wang et al. 2023).** "an ever-growing skill library of executable code" — [Voyager p1](https://arxiv.org/pdf/2305.16291#page=1)
- **Director (Hafner et al. 2022).** Hierarchical behaviour by "planning inside the latent space of a learned world model" — [Director p1](https://arxiv.org/pdf/2206.04114#page=1)
- **Learning Options via Compression (LOVE; Jiang et al. 2022).** An MDL objective applied to skills: a "penalty on the description length of the skills" — [LOVE p1](https://arxiv.org/pdf/2212.04590#page=1); "we hypothesize that compression is a good objective for skill learning" — [LOVE p2](https://arxiv.org/pdf/2212.04590#page=2)
- **Ada (Wong et al. 2023/ICLR 2024).** "interactively learns a library of planner-compatible high-level action abstractions" — [Ada p1](https://arxiv.org/pdf/2312.08566#page=1)
- **TheoryCoder-2**: "library of abstract states and actions" — [TheoryCoder-2 p3](https://arxiv.org/pdf/2602.00929#page=3)

### Inferences
- LOVE is the cleanest bridge: the same MDL objective as DreamCoder or Stitch, applied to action sequences instead of program text. An option is a library function whose "execution" is a closed-loop policy.
- The pairing "abstract action (option or operator) ↔ abstract state (predicate or latent)" is forced. To plan with an option you need a model of where it ends up, i.e. an abstract transition model over abstract states. So a learned action library implies a learned coarse world model. PDDL operators carry this as preconditions and effects; One Life's precondition-effect laws have the same shape (Q3).

### Gaps
- Voyager and Ada were not checked for whether their libraries are pruned or compressed by an MDL criterion. Voyager appears to add skills rather than compress them (not verified quantitatively here).

## Q6. Denoising / diffusion over programs; does it connect to library learning? E-graphs as canonicalization

### Takeaway
Tree diffusion (Kapur, Jenner, Russell 2024) is the clearest "denoiser over programs": noise means random small subtree mutations of programs sampled from a grammar, and a network learns to invert them, guided by a value model plus search. Discrete text diffusion for code (DiffuCoder, Mercury, LLaDA) denoises token sequences, not syntax trees, and has no library notion. E-graphs and equality saturation give an exhaustive "normalize modulo rewrites" step and are already used inside library learning (babble, DreamCoder's refactoring). Ruler shows rewrite rules can themselves be learned. No source connects "common denoising moves" to a learned library.

### Cited Findings
- **Tree diffusion (Kapur et al. 2024).**
  - The abstract frames it as image-style diffusion: "our method also inverts" ["noise" applied to syntax trees] — [Kapur p1](https://arxiv.org/pdf/2405.20519#page=1). Concretely, "we randomly make small mutations to the syntax trees of programs" and "train a conditional neural model to invert these small mutations" — [Kapur p3](https://arxiv.org/pdf/2405.20519#page=3)
  - Training programs are random draws from a grammar, the analogue of DreamCoder's fantasies: "randomly sample programs from our CFG under exact constraints" — [Kapur p4](https://arxiv.org/pdf/2405.20519#page=4)
  - Search coupling: "By training a value model alongside our diffusion model" — [Kapur p1](https://arxiv.org/pdf/2405.20519#page=1)
  - Limitation that blocks library-style abstraction: "we operate on expressions with no variable binding, loops, strings, continuous parameters" — [Kapur p9](https://arxiv.org/pdf/2405.20519#page=9)
- **DiffuCoder (2025).** Masked diffusion for code: "MDMs iteratively refine the entire sequence in parallel, which allows for global planning" — [DiffuCoder p2](https://arxiv.org/pdf/2506.20639#page=2)
- **Mercury (Inception Labs 2025).** "By generating tokens in parallel in a coarse-to-fine manner" — [Mercury p2](https://arxiv.org/pdf/2506.17298#page=2)
- **LLaDA (2025).** "a diffusion model trained from scratch" [as an alternative to autoregressive LLMs] — [LLaDA p1](https://arxiv.org/pdf/2502.09992#page=1)
- **egg (Willsey et al. 2021).** Equality saturation "repurposes e-graphs to implement state-of-the-art" [rewrite-driven compiler optimizations] — [egg p1](https://arxiv.org/pdf/2004.03082#page=1)
- **Ruler (Nandi et al. 2021).** Equality saturation "can also be used to infer rewrite rules" — [Ruler p1](https://arxiv.org/pdf/2108.10436#page=1)
- **babble** uses e-graphs to find abstractions modulo an equational theory — [babble p1](https://arxiv.org/pdf/2212.04596#page=1) (quotes in Q2)

### Inferences
- Tree diffusion fits the wake-sleep dictionary exactly:
  - its training programs are fantasies from a fixed grammar prior;
  - its noise kernel is "small random subtree replacement";
  - its denoiser is an amortized recognition model over edits.
  Swapping the fixed CFG for a learned library would make the fantasies DreamCoder-like, and the edits could insert library calls. That would be "library-aware tree diffusion" (not found in the literature).
- A plausible link from "denoising moves" to "rewrite library": record the edits the denoiser makes, then compress the corpus of edit traces (Stitch over edit programs). Frequent multi-step repairs would become macro-rewrites, and a learned rewrite library is also what Ruler produces by enumeration. This is a hypothesis, untested in the literature.
- Equality saturation is not denoising in the probabilistic sense. It is deterministic canonicalization (the e-graph holds every equivalent form, and extraction picks the cheapest). The analogy: a denoiser maps a noisy point to the posterior mean or mode, while extraction maps an equivalence class to its minimum-cost representative. Both are "projection to a canonical representative", one under a noise model and one under a rewrite theory. (Tweedie's formula, i.e. posterior-mean denoising, belongs to sibling researchers.)

### Gaps
- No paper found that learns libraries from denoising or diffusion traces, or that uses a learned library as the vocabulary of a program-diffusion model.
- No follow-up to tree diffusion (2025–2026) that adds variable binding or loops was verified.

## Q7. Abstraction as state aggregation in RL: what a latent world model keeps vs. what an abstraction keeps

### Takeaway
RL formalizes "what to keep" via:
- state abstraction / aggregation (Li, Walsh, Littman);
- bisimulation metrics (Ferns et al.; deep bisimulation for control, DBC);
- value equivalence (Grimm et al.; MuZero).
All of these keep only what matters for behaviour or value, not for reconstruction. Library learning's MDL objective keeps whatever compresses the corpus. The 2026 ARC-AGI-3 evidence shows the gap concretely: a world model that matches transitions is not automatically useful for acting.

### Cited Findings
- **Li, Walsh, Littman (2006).** Abstraction treats "groups of states as a unit by ignoring irrelevant state information" — [Li et al. p1](http://rbr.cs.umass.edu/aimath06/proceedings/P21.pdf#page=1)
- **Ferns, Panangaden, Precup (2004).** "The formulation of our metrics is based on the notion of bisimulation for MDPs" — [Ferns et al. p1](https://arxiv.org/pdf/1207.4114#page=1)
- **DBC (Zhang et al. 2020).** "bisimulation metrics quantify behavioral similarity between states" — [Zhang et al. p1](https://arxiv.org/pdf/2006.10742#page=1)
- **Value equivalence (Grimm et al. 2020).** Two models are equivalent if they "yield the same Bellman updates" — [Grimm et al. p1](https://arxiv.org/pdf/2011.03506#page=1)
- **MuZero (Schrittwieser et al. 2019).** No "requirement for the hidden state to capture all information necessary to reconstruct the original observation" — [MuZero p2](https://arxiv.org/pdf/1911.08265#page=2)
- **Tycho (2026)**, empirical version of the same point: "transition match shows whether a simulator reproduces observed dynamics"; "not whether it has identified the objective or whether consulting it improves the next action" — [Tycho p1](https://arxiv.org/pdf/2607.28287#page=1)

### Inferences
- Read through RL, a library abstraction is a quotient on programs and a bisimulation abstraction is a quotient on states. In both, the right equivalence depends on what is downstream. MDL alone corresponds to "reconstruction-equivalence" (keep what compresses observations). Value-equivalence-style objectives would keep only abstractions that change decisions.
- A library-learning world model could add a value-equivalence term: score an abstraction by both corpus compression and planning gain. Tycho's and Rodionov's results suggest that compression or transition fidelity alone does not guarantee planning gain.

### Gaps
- No paper found that combines a bisimulation or value-equivalence criterion with symbolic library learning (Stitch, babble, DreamCoder).

## Q8. Critiques and failure modes

### Takeaway
Four kinds of failure show up:
1. LLM "library learning" often does not reuse its library (gains come from retries and voting).
2. Wake-sleep dreaming suffers from fantasy/data distribution mismatch.
3. Program world models were long restricted to deterministic, symbolic, fully specified settings. Humans still beat frontier models at world-model learning, and on ARC-AGI-3 an executable world model is not uniformly better than text.
4. Retrospective compression mis-models non-stationary task streams.
Compute cost (DreamCoder's slow compressor) and syntactic brittleness (pre-babble) are older, partly solved issues.

### Cited Findings
- **Library Learning Doesn't (Berlot-Attwell, Rudzicz, Si 2024)**, on LEGO-Prover and TroVE: "function reuse is extremely infrequent" — [p1](https://arxiv.org/pdf/2410.20274#page=1); gains come from elsewhere: "self-correction and self-consistency are the primary drivers" — [p1](https://arxiv.org/pdf/2410.20274#page=1); "lemma reuse is very uncommon, and no lemma reused twice" — [p2](https://arxiv.org/pdf/2410.20274#page=2)
- **LLM Library Learning Fails: A LEGO-Prover Case Study (2025).** "We find no evidence of the direct reuse of learned lemmas" — [p1](https://arxiv.org/pdf/2504.03048#page=1)
- **Wake-sleep mismatch.** "Early in the learning, fantasies will have a quite different distribution" — [ws.pdf p4](https://www.cs.toronto.edu/~hinton/absps/ws.pdf#page=4)
- **WorldCoder limitation.** "Currently we assume deterministic dynamics" — [WorldCoder p9](https://arxiv.org/pdf/2402.12275#page=9)
- **One Life on prior work.** "Previous research on symbolic world modeling has focused on largely deterministic environments" — [One Life p1](https://arxiv.org/pdf/2510.12088#page=1)
- **OPINE-World on WorldCoder-style models.** They are "insufficient when the symbolic representation and goal are unknown and continually changing" — [OPINE-World p1](https://arxiv.org/pdf/2607.01531#page=1)
- **AutumnBench / WorldTest (Warrier et al., incl. Tenenbaum, Ellis, Tavares; 2025, v4 2026).** Humans beat frontier models, "a gap we attribute to differences in exploration" [and belief updating] — [AutumnBench p1](https://arxiv.org/pdf/2510.19788#page=1). Note: the v4 title is "Benchmarking World-Model Learning with Environment-Level Queries", while v1 was "Benchmarking World-Model Learning".
- **ARC-AGI-3 ablation (Rodionov 2026b).** "textual outperforms flexible-interface executable in both gpt-5.5 conditions"; at maximum effort, "the three imposed mechanisms are not required for action-efficient public-set completion" — [Rodionov 2026b p1](https://arxiv.org/pdf/2607.15439#page=1)
- **Retrospective compression.** Fails under non-stationarity — [Hernandez Cano et al. 2026 p1](https://arxiv.org/pdf/2605.09985#page=1) (quotes in Q2)
- **Older fixes.** DreamCoder's compressor was slow ("Stitch is 3-4 orders of magnitude faster" — [Stitch p1](https://arxiv.org/pdf/2211.16605#page=1)), and syntactic methods are brittle (babble: "a purely syntactic library learning approach cannot generate this abstraction" — [babble p2](https://arxiv.org/pdf/2212.04596#page=2))
- **Tree diffusion's expressiveness limit** (no variable binding or loops) — [Kapur p9](https://arxiv.org/pdf/2405.20519#page=9)

### Inferences
- Measure reuse, not just accuracy. Any library-learning world model should report how often each abstraction is actually used on held-out tasks; the Berlot-Attwell papers show accuracy gains can be unrelated to the library.
- The ARC-AGI-3 results are confounded by model capability: per Rodionov 2026b, capability and effort gains often exceed differences between agent variants. Claims that "program world models help" need matched-capability ablations.
- Pure MDL is retrospective and reconstruction-oriented. Both critiques (prospective compression and value equivalence) point toward an objective that scores abstractions by expected future use.

### Gaps
- No systematic study found of library transfer failure across environments for world-model programs specifically. The negative results above are on math (LEGO-Prover, TroVE) and on single-environment agents.
- TroVE's and LEGO-Prover's own papers were not fetched. Their characterization comes from the critique papers only.
