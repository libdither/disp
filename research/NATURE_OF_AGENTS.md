# The nature of agents: judging yourself on a budget

> Research note, 2026-10-14. Origin: a conversation that read the probabilistic liar (a sentence saying "my probability is not above r") as a program that asks an oracle about itself, then asked what that means for agents, subagents, randomness and regularizers. Method: every link fetched with `scripts/verify-source.sh`, fragments land on the quoted passage; the soft-judge numbers in §7 were computed for this note; the disp snippets were run with `scripts/probe.sh -r` (all tests pass). Builds on `research/NATURE_OF_OPTIMIZERS.md` (search, priors, Goodhart) and `research/THE_NATURE_OF_ABSTRACTION.typ` (the Löbian wall) without repeating them. Delete once its claims live in code: a soft judge in the kernel, priced search in the optimizer, or sections of `research/OPTIMIZER.typ`.

## 1. The question to ask

The questions as asked: what is an agent, why are agents made of smaller agents, what are regularizers for, and why does changing a regularizer change what kind of agent exists?

The question underneath them:

> **No judge can judge itself exactly. What can it give up instead, and which give-up do the prices make cheapest?**

A *judge* here is any program that scores programs: a type checker, a scorer, a probability assignment, an oracle. An *agent* is a judge that acts on its own verdicts, so sooner or later it has to judge itself. The liar shows exact self-judgment is out of reach whatever the judge returns:

- **true/false:** no consistent answer (Tarski, Turing);
- **true/false/"never returns":** the answer is "never returns";
- **probabilities:** an answer exists, but only by flipping a coin at exact ties and knowing yourself only to within an interval, and no machine can compute it for every program.

So every agent that models itself is paying somewhere, and agents differ mainly in *where* they pay. A regularizer is a price on a resource, so it decides where paying is cheapest. The four questions then fall out:

| asked | answer in one line | where |
|---|---|---|
| What is an agent? | A judge-and-act loop, behind a boundary, that stays an approximate fixed point of judging itself at the substrate's prices. | §4 |
| Why made of smaller agents? | Splitting is usually the cheapest payment on a local substrate: no part can hold the whole, histories must stay private, and a committee of strategies beats any single bet. | §5 |
| What are regularizers for? | They are prices. A price smooths the judge so a fixed point exists and can be reached, and it sets which resource gets spent. | §6 |
| Why do they change the kind? | The set of stable fixed points changes at critical prices (bifurcations). Between them, agents shift smoothly; across them, the population of possible agents changes. | §7 |

Randomness (§8) turns out to be one of the payments, and like agency it is relative to whoever is watching.

## 2. The ground: judges on a priced substrate

**A self-judging program.** disp programs are trees, so a program can hand its own code to a judge with no quoting step:

```disp
liar_at := {r} => fix ({self, oracle} => not (oracle self r))
teller_at := {r} => fix ({self, oracle} => oracle self r)
test liar_at (pair 1 2) ({_m, _r} => true) = false
test liar_at (pair 1 2) ({_m, _r} => false) = true
test liar_at (pair 1 2) ({m, r} => tree_eq m (liar_at r)) = false
test teller_at (pair 1 2) ({_m, _r} => true) = true
test teller_at (pair 1 2) ({_m, _r} => false) = false
```

`oracle m r` answers "does `m` return true with probability above r?" The liar does the opposite of the answer, the truth-teller does what it is told. Both fixed oracles are wrong about the liar (tests 1–2) and right about the truth-teller (tests 4–5). Test 3 shows the `self` handed over is the program's own code.

**What the liar forces.**

- A probability assignment that can see its own values to within any open interval must give the liar exactly its threshold: ["a reflectively consistent distribution P will assign P (G) = p"](https://intelligence.org/files/DefinabilityTruthDraft.pdf#page=5), and ["No matter how precisely it estimates P (G), it remains uncertain"](https://intelligence.org/files/DefinabilityTruthDraft.pdf#page=5) which side of p it sits on.
- Read as programs, an *oracle* is Turing's word for ["some unspecified means of solving number-theoretic problems"](https://sites.math.rutgers.edu/~zeilberg/akherim/TuringPhd.pdf#page=12) that ["cannot be a machine"](https://sites.math.rutgers.edu/~zeilberg/akherim/TuringPhd.pdf#page=13): a box of answers a program may consult. A *reflective oracle* answers "above r?" correctly except at exact ties, where it may answer at random ([definition](https://arxiv.org/pdf/1508.04145#page=2)). The liar machine, ["asking the oracle what it will return and then returning the opposite"](https://arxiv.org/pdf/1508.04145#page=3), is then "no contradiction".
- Nobody can compute that answer for every program at once: ["any coherent P is necessarily uncomputable"](https://intelligence.org/files/DefinabilityTruthDraft.pdf#page=5). Physics would have the same problem: in a loop in time, ["Nature has to produce a (probabilistic or quantum) fixed-point of some evolution operator"](https://arxiv.org/pdf/0808.2669#page=1), and the grandfather paradox resolves as ["you are born with 1/2 probability"](https://arxiv.org/pdf/0808.2669#page=2).

| the judge returns | what the liar does |
|---|---|
| true/false, callable from inside | no consistent answer |
| true/false/"never returns" | loops forever, so its value is "never returns" |
| a probability, via "above r?" questions | lands exactly on r; the judge flips a coin at the tie |

**Where the payment lands on the lattice.** The strand lattice (`evaluators/rust-ic-strands`, the tree-calculus net run as local physics) and the reversible core (`research/REVERSIBLE_CORE.md`) put concrete prices on resources:

| resource | lattice form | what it buys a self-judge |
|---|---|---|
| space | fuel: empty seats a duplicator builds copies in, refunded when the run goes back (`research/REVERSIBLE_CORE.md:256`) | unroll self-simulation into copies; for the liar the answer flips at every level and never settles |
| time | rounds | one copy, answered by its own running tally; settles to r within 1/n after n runs |
| entropy | random bits from outside; the `phys=1` drive (the lattice run as physics) is thermal (`evaluators/rust-ic-strands/README.md:1488`) | the one-shot mixed answer at the tie |
| heat | a *commit* (erasing a record of a past rewrite) emits heat that drifts to the lattice edge (`research/REVERSIBLE_CORE.md:265`) | forgetting history so space can be reused |

The time row, written out: the liar run against a judge that answers from the tally so far.

```disp
add := fix ({self, n, m} => if (is_leaf n) { m } else { succ (self (pair_snd n) m) })
mul := fix ({self, n, m} => if (is_leaf n) { zero } else { add m (self (pair_snd n) m) })
frac_le := {a, b} => nat_le (mul (pair_fst a) (pair_snd b)) (mul (pair_fst b) (pair_snd a))
frac_lt := {a, b} => not (frac_le b a)
/// Run the liar n times; the oracle answers "above r?" from the tally (trues, runs) so far.
dither := fix ({self, r, n} => if (is_leaf n) { pair 0 0 } else {
  let tally := self r (pair_snd n)
  let out := liar_at r ({_m, q} => frac_lt q tally)
  pair (if out { succ (pair_fst tally) } else { pair_fst tally }) (succ (pair_snd tally))
})
test dither (pair 1 3) 6 = pair 2 6
test dither (pair 2 3) 9 = pair 6 9
test dither (pair 1 2) 4 = pair 2 4
```

No random bits and no heat: from the start, the tally at step n is fixed by n, so the run is a clock. That is special to a closed loop. With inputs, many histories reach the same tally, and forgetting which one happened costs heat.

## 3. The menu: six things a self-judge can give up

Each row is one way to stop needing an exact verdict on yourself. Real agents mix several.

| give up | the agent then | spends | priced by |
|---|---|---|---|
| exactness | knows itself only to within an interval | bits of self-knowledge | an information cost |
| determinism | randomizes at its own ties | entropy | a temperature or entropy bonus |
| immediacy | learns about itself over time | time | learning rate, discounting, a speed prior |
| wholeness | splits into parts that judge each other | space, wiring | connection and communication costs |
| fidelity | models itself coarsely | self-model size | description length, an information bottleneck |
| self-trust | trusts its successors or its own core only weakly | authority | a frozen core, a weaker successor logic |

1. **Exactness.** Christiano et al. picture P ["as having access to arbitrarily precise information about P"](https://intelligence.org/files/DefinabilityTruthDraft.pdf#page=4) but not its exact values, so ["P is "mistaken" about itself, but its error is given by the infinitesimal"](https://intelligence.org/files/DefinabilityTruthDraft.pdf#page=5). Their reading of Tarski: undefinability is ["an artifact of the infinite precision demanded by reasoning about complete certainty"](https://intelligence.org/files/DefinabilityTruthDraft.pdf#page=7). An agent paying here is calibrated about itself but cannot tell which side of its own thresholds it is on: "I'm not sure whether I'd do it" is the honest state.
2. **Determinism.** The coin at the tie. Reflective agents in a game ["play a Nash equilibrium, with the randomization in mixed strategies coming from the randomization in the oracle's answers"](https://arxiv.org/pdf/1508.04145#page=1). Where the coin comes from is §8.
3. **Immediacy.** Logical induction (a computable way to assign probabilities to mathematical claims that improves day by day) gives its liar sentences ["probabilities that converge on p"](https://arxiv.org/pdf/1609.03543#page=46). It also ["learns to have accurate beliefs about its own current beliefs, in a manner that avoids the standard paradoxes of self-reference"](https://arxiv.org/pdf/1609.03543#page=1), and ["Logical inductors also learn to trust their future beliefs more than their current beliefs"](https://arxiv.org/pdf/1609.03543#page=1) (Theorem 4.12.4, [Self-Trust](https://arxiv.org/pdf/1609.03543#page=48)). Belief becomes indexed by time: what you believe about yourself today is a forecast, corrected tomorrow.
4. **Wholeness.** A fixed point too big to solve in one place can be solved as a game, each part best-responding to its model of the others. For reflective agents those games end in Nash equilibrium, and ["for every particular Nash equilibrium there is an oracle that causes the players to behave according to this equilibrium"](https://arxiv.org/pdf/1508.04145#page=2). §5 is about this row.
5. **Fidelity.** An embedded agent is part of its world, so it ["must also be smaller than the environment"](https://arxiv.org/pdf/1902.09469#page=3), and ["An agent can’t fit inside its own head."](https://arxiv.org/pdf/1902.09469#page=13) A self-model smaller than the self is an abstraction: it keeps some parts and treats the rest as black boxes (`research/THE_NATURE_OF_ABSTRACTION.typ` is about choosing which).
6. **Self-trust.** Löb's theorem says ["A consistent mathematical theory T cannot trust itself in the sense of verifying that a proof in T of any formula φ implies φ’s truth"](https://intelligence.org/files/TilingAgentsDraft.pdf#page=2), which at first seems to mean an agent ["can only trust the reasoning of successors that use weaker mathematical systems than its own"](https://intelligence.org/files/TilingAgentsDraft.pdf#page=2). The tiling-agents paper then finds workarounds for special cases. disp already pays here: the abstraction note's Löbian wall keeps the trusted core and the license checker outside the optimization loop (`research/THE_NATURE_OF_ABSTRACTION.typ:262`). Rows 1–3 are the alternative: trust a successor to within an error instead of exactly.

## 4. What an agent is

Simon set the program in 1955: ["the task is to replace the global rationality of economic man with a kind of rational behavior that is compatible with the access to information and the computational capacities that are actually possessed by organisms"](https://competitionpolicyinternational.com/assets/0d358061e11f2708ad9d62634c6c40ad/SimonwCover.pdf#page=4). This note adds one thing to it: the organism has to model itself too, and that is where the liar bites.

A working definition for modeling, in three parts:

1. **A boundary.** A region whose inside and outside interact through a thin interface. ["A Markov blanket defines the boundaries of a system in a statistical sense."](https://pmc.ncbi.nlm.nih.gov/articles/PMC5805980/#abstract1) (A Markov blanket is the set of states that screens the inside off from the outside: given the blanket, the two are independent.) Thermodynamics says where cheap boundaries go: modular control pays a minimum extra heat ["proportional to the reduction in mutual information between the controlled and uncontrolled subsystems"](https://arxiv.org/pdf/1708.03030#page=2), so a cut costs least where the two sides share little information. The same paper shows the extra cost can be dodged by ["designing internal ratchet states that capture the global correlations"](https://arxiv.org/pdf/1708.03030#page=1): a part that carries a model of what lies outside it stops paying for the cut. Thermodynamics rewards parts that model their surroundings.
2. **A loop that stays put.** It judges (itself included, coarsely), acts on the verdict, and the result is an approximate fixed point that persists at the substrate's prices. Exact fixed points are uncomputable (§2), so a real agent is a *process* that keeps re-approximating its own fixed point: a tally, a market, a learning rule.
3. **A cheaper description.** To some observer, "it is trying to X" predicts it more cheaply than simulating its parts. Whether something is an agent is, in Orseau, McGill and Legg's words, a question that ["is observer-relative"](https://arxiv.org/pdf/1805.12387#page=1), and they make it Bayes over two model classes: ["Bayes’ rule can then be applied to calculate the subjective probability of a system being a device or an agent, based only on its behaviour."](https://arxiv.org/pdf/1805.12387#page=1)

Two consequences:

- **Agency is relative, like randomness and like good belief.** The same system is an agent to a small observer and a mechanism to a big one. Pseudorandomness has the same shape (§8). Logical induction defines good belief the same way: ["it should not be possible to exploit the market prices using any trading strategy that can be generated in polynomial-time"](https://arxiv.org/pdf/1609.03543#page=5). Good belief means no observer within a given budget can profit from it.
- **Beliefs move with time and with price.** The liar at r = 0.3 answered by a soft judge (§7) settles at 0.46 when the judge is warm (T = 1), 0.41 at T = 0.3, and 0.31 at T = 0.01: the judge's default answer (½) leaks into the belief, more the warmer the judge.

## 5. Why agents are made of smaller agents

Boundaries nest: ["autonomous systems are hierarchically composed of Markov blankets of Markov blankets"](https://pmc.ncbi.nlm.nih.gov/articles/PMC5805980/#abstract1). Six pressures push that way, each enough on its own in some regime:

1. **Locality.** On the lattice, "Nothing walks a program: every pair whose principal ports meet rewrites where it is, inside one 2×2×2 block" (`evaluators/rust-ic-strands/README.md:1475`). A decision can only use what reached it in time, so a big system's parts must decide on local information, each with its own partial view.
2. **No part can hold the whole.** An exact self-model is impossible (§2), and modeling everything ["would require the agent’s self-model to be as “big” as the whole agent"](https://arxiv.org/pdf/1902.09469#page=13). So the whole's model of itself treats its parts as black boxes summarized by what they do, and by §4.3 a black box predicted by its goals is an agent to whoever models it. Subagents are partly a fact about the self-model, not only about the wiring.
3. **Private histories.** On a reversible substrate, sharing code ties histories together. The reversible core's design review found that "copies buy private histories" (`research/REVERSIBLE_CORE.md:34`): when every call reads the same nodes, undo becomes last-in-first-out across unrelated calls, so the core copies code per call. Independent undo needs individuals, and a part with its own history is the substrate's version of one.
4. **A committee beats a single bet.** When you cannot compute which strategy is right, run many and reweight them by results. Freund and Schapire's Hedge does this with multiplicative weights over N strategies, and its regret bound ["depends only logarithmically on N"](https://www.schapire.net/papers/FreundSc95.pdf#page=6), so ["this bound is reasonable even for a very large number of strategies"](https://www.schapire.net/papers/FreundSc95.pdf#page=6). The same shape recurs: Bayes mixtures over hypotheses, Levin search time-sharing programs by prior (`research/NATURE_OF_OPTIMIZERS.md` §2.1), and the logical inductor, which reads ["a reasoner’s probabilities as prices in a stock market"](https://arxiv.org/pdf/1609.03543#page=5) and sets the prices so that every trader in an enumeration, aggregated ["into one “supertrader”"](https://arxiv.org/pdf/1609.03543#page=11), would make no trades. The smaller agents are the experts.
5. **Wiring costs and modular change.** A price on wiring alone does it: ["selection pressures to maximize network performance and minimize connection costs yield networks that are significantly more modular and more evolvable"](https://arxiv.org/pdf/1207.2743#page=2), and ["selection to reduce connection costs causes modularity, even in unchanging environments"](https://arxiv.org/pdf/1207.2743#page=6). Goals that change part by part do it too: ["“modularly varying goals” lead to the spontaneous evolution of modular network structure and network motifs"](https://pmc.ncbi.nlm.nih.gov/articles/PMC1236541/#abstract1), while ["Networks evolved under randomly varying goals (with no common subgoals) do not seem to evolve modular structure."](https://pmc.ncbi.nlm.nih.gov/articles/PMC1236541/#sec3) And hierarchy is what has time to form: ["hierarchic systems will evolve far more quickly than non-hierarchic systems of comparable size"](https://faculty.sites.iastate.edu/tesfatsi/archive/tesfatsi/ArchitectureOfComplexity.HSimon1962.pdf#page=3). In Simon's parable, a watchmaker interrupted mid-build loses only the current subassembly if the subassemblies are stable, and without them ["it will take Tempus, on the average, about four thousand times as long to assemble a watch as Hora."](https://faculty.sites.iastate.edu/tesfatsi/archive/tesfatsi/ArchitectureOfComplexity.HSimon1962.pdf#page=5) A stable subassembly is a checkpoint: work that never has to be redone. The result is near-decomposable: ["in the long run, the behavior of any one of the components depends in only an aggregate way on the behavior of the other components"](https://faculty.sites.iastate.edu/tesfatsi/archive/tesfatsi/ArchitectureOfComplexity.HSimon1962.pdf#page=9).
6. **Coherence does not force one goal.** The argument that an agent nobody can exploit must maximize one utility has a gap. Wentworth: ["This committee is not exploitable and does not throw away resources"](https://www.lesswrong.com/posts/3xF66BNSC5caZuKyC/why-subagents#Preference_By_Committee), and ["Markets, for example, are efficient/inexploitable but can’t be represented by a utility function. They have hidden internal state - the distribution of wealth over agents - which makes their preferences path-dependent."](https://www.lesswrong.com/posts/3xF66BNSC5caZuKyC/why-subagents#Conclusion) disp's `lib/kernel/decision.disp` already defines one-agent-ness this way: `united` holds when no rival policy, from a finite list, is a Pareto improvement in the eyes of the trial's own decision points (`lib/kernel/decision.disp:156`), i.e. the moments do not infight. That formalizes Demski's [Coherent Care](https://www.lesswrong.com/posts/CDkbYSFTwggGE8mWp/coherent-care#Naturalism): ["an agent’s various parts don’t fight; they are not in conflict"](https://www.lesswrong.com/posts/CDkbYSFTwggGE8mWp/coherent-care#Naturalism), and ["If a Pareto improvement exists, there’s conflict."](https://www.lesswrong.com/posts/CDkbYSFTwggGE8mWp/coherent-care#Naturalism) One agent is a coalition with nothing left to trade.

What being made of parts costs:

- The parts' equilibrium can be bad for all of them (a prisoner's dilemma inside one agent). Proof-searching bounded agents can sometimes escape it: Critch's reach ["mutually cooperative program equilibrium in the Prisoner’s Dilemma"](https://arxiv.org/pdf/1602.04184#page=1) using ["an effective version of Löb’s theorem which is applicable given such bounded resources"](https://arxiv.org/pdf/1602.04184#page=1).
- Modular control costs heat beyond the global minimum (§4, part 1).
- Parts can turn on the whole. Embedded Agency names it: ["Subsystem alignment is about how to be one unified agent that doesn’t have subsystems that are fighting against either you or each other."](https://arxiv.org/pdf/1902.09469#page=6) An optimizer ["might spin up other optimizers as subroutines, either intentionally or unintentionally"](https://arxiv.org/pdf/1902.09469#page=3).
- A parent cannot fully trust a child that reasons as strongly as it does (§3 row 6).

**A recipe for modeling an agent.** "One agent" is the special case where the parts' game has no Pareto-improving deviation left; the general case is:

1. List the resources and their prices: copies (space), rounds (time), random bits, wiring, erasure (heat).
2. Find the boundary: the cut with the least mutual information across it.
3. Find the parts and write the game among them: what each one judges, and from what view.
4. Find the game's stable fixed points at those prices. These are the agent's possible selves.
5. Sweep the prices and mark where fixed points split, merge or lose stability. Those are the places where a small change in cost changes what kind of agent exists (§7).

## 6. Regularizers are prices

**A budget and a price are two forms of one constraint.** Boyd and Vandenberghe's reading of Lagrange duality: a constraint is ["some limit, such as a limit on resources (e.g., warehouse space, labor)"](https://web.stanford.edu/~boyd/cvxbook/bv_cvxbook.pdf#page=254), and the dual problem imagines that ["the limits can be violated, by paying an additional cost which is linear in the amount of"](https://web.stanford.edu/~boyd/cvxbook/bv_cvxbook.pdf#page=254) violation. For well-behaved (convex) problems the two pick the same solutions at a matching budget and price. A regularizer is the price form of a resource budget.

**What a price does to a judge.**

- **Smooths it.** A hard threshold jumps; a priced one ramps. A continuous response on [0, 1] always has a fixed point, and once its slope stays below 1 in size there is exactly one, which simple iteration finds.
- **Makes it hard to exploit.** Kalai and Vempala's example: against costs that alternate between two experts, ["following the leader (without perturbations) always incurs a cost of 1, while each expert incurs a cost of about t/2 over t periods"](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/2005-Efficient_Algorithms_for_Online_Decision_Problems.pdf#page=4). In general ["any deterministic algorithm can be forced to have a cost of t over t periods"](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/2005-Efficient_Algorithms_for_Online_Decision_Problems.pdf#page=4), and ["By adding perturbations, the algorithm becomes less predictable"](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/11/2005-Efficient_Algorithms_for_Online_Decision_Problems.pdf#page=4). That is the liar's lesson as a learning rule: a judge that commits deterministically to its best guess gets diagonalized against. Abernethy et al. show the two fixes are one: ["adding a strongly convex penalty function to the decision rule and adding stochastic perturbations to data correspond to deterministic and stochastic smoothing operations, respectively"](https://arxiv.org/pdf/1405.6076#page=1). Regularizing and randomizing are the same move, made with a formula or with coins.
- **Sets an exchange rate.** Bounded-rational choice, in Ortega and Braun's thermodynamic version, maximizes ["a trade-off between an expected utility gain (first term) and the information processing cost of transforming p0 into p (second term)"](https://arxiv.org/pdf/1204.6481#page=9), the cost measured by ["the Kullback-Leibler divergence with respect to an initial policy or belief"](https://arxiv.org/pdf/1204.6481#page=16). The best policy is the default reweighted by e^(βU), a softmax. The price has a compute reading: one can ["interpret the inverse temperature as a resource parameter that determines how many samples are drawn to estimate the maximum"](https://arxiv.org/pdf/1204.6481#page=13). Its two ends: ["The limit β → ∞ corresponds to the perfectly rational actor that infallibly selects the action that maximizes gain"](https://arxiv.org/pdf/1204.6481#page=7), and ["The limit β → 0 is an actor without resources that simply selects his action according to his prior."](https://arxiv.org/pdf/1204.6481#page=7) The repo's "quantilize at readback" (`research/NATURE_OF_OPTIMIZERS.md:197`) is the hard-threshold version: a q-quantilizer samples the top q of a base distribution with density base/q there, which puts it exactly log(1/q) of KL divergence from the base. Choosing q is choosing the price.

**Which resource is priced decides which agents win.**

| resource priced | regularizer | agents that win |
|---|---|---|
| bits of deliberation | KL to a default policy, entropy bonus | stochastic, partway between the default and the maximizer |
| description length | simplicity prior, minimum description length | compressed; general-purpose search inside ([hypothesized](https://arxiv.org/pdf/1906.01820#page=14)) |
| runtime | speed prior, step penalties | tables and reflexes, little inner search ([hypothesized](https://arxiv.org/pdf/1906.01820#page=14)) |
| wiring | connection cost | modules (§5 pressure 5) |
| active parts | L1 penalty | sparse: a few parts on, the rest ["exactly 0"](https://web.archive.org/web/20210709150716id_/https://statweb.stanford.edu/~tibs/lasso/lasso.pdf#page=2) |
| erasure | heat per committed bit | history-keepers that run back instead of forgetting (`research/REVERSIBLE_CORE.md` §3.6) |
| random bits | price per bit | deterministic, dithering over time; exploitable if watched (§8) |
| self-trust | frozen core, weaker successor logic | layered authority (§3 row 6) |

## 7. Why prices change the kind, not just the amount

**A soft judge.** Replace the reflective oracle's hard threshold with a ramp: answer "above r" with probability σ((x − r)/T), where x is the program's actual chance of returning true and σ is the logistic function. That is the softmax that a KL price produces (§6), and T, the temperature, is the price's strength. T → 0 is the hard oracle.

**The truth-teller is a magnet.** Its fixed points solve x = σ((x − r)/T). For r = ½, writing m = 2x − 1 turns this into m = tanh(m/(4T)), the mean-field (Curie–Weiss) magnet, so the split happens at T = ¼:

| T | stable selves | unstable |
|---|---|---|
| 1.0 | 0.5 | |
| 0.26 | 0.5 | |
| 0.24 | 0.33, 0.67 | 0.5 |
| 0.2 | 0.14, 0.86 | 0.5 |
| 0.1 | 0.007, 0.993 | 0.5 |

Above ¼ there is one self, undecided. Below, two committed selves with an unstable middle between them. The sentence did not change; only the price did. With r = 0.3 the split is lopsided: one stable self that believes itself (0.57 at T = 1, 0.999 at T = 0.1) until T ≈ 0.091, where a second stable self near 0 appears out of nowhere (a saddle-node bifurcation; at T = 0.05 the selves are 0.003 and 1.0 with an unstable 0.24 between).

**The liar keeps one answer, but its dynamics split.** The liar's response falls as x rises, so it has exactly one fixed point at every T. A self-simulation that sets its next guess to the judge's last answer converges only while the response's slope is below 1 in size. That slope is x(1 − x)/T at the fixed point, which crosses 1 at T = ¼ for r = ½ and at T ≈ 0.24 for r = 0.3. Colder than that, it flip-flops:

| r | T | fixed point | slope size | guess := last answer | step 5% toward the answer |
|---|---|---|---|---|---|
| 0.5 | 1.0 | 0.5 | 0.25 | settles at 0.5 | 0.5 |
| 0.5 | 0.2 | 0.5 | 1.25 | flips 0.14 ↔ 0.86 | 0.5 |
| 0.5 | 0.1 | 0.5 | 2.5 | flips 0.007 ↔ 0.993 | 0.5 |
| 0.3 | 1.0 | 0.46 | 0.25 | settles at 0.46 | 0.46 |
| 0.3 | 0.1 | 0.358 | 2.3 | flips 0.002 ↔ 0.95 | 0.358 |
| 0.3 | 0.01 | 0.308 | 21.3 | flips 0.0 ↔ 1.0 | 0.308 |

A step of size η toward the answer converges when η < 2/(1 + slope size). The sharper the judge, the slower it must learn about itself.

**The general pattern.** A price change moves fixed points smoothly until two collide or one splits, and there the set of stable agents changes. The same shape shows up across fields:

- **Compression.** In the information bottleneck (squeeze X while keeping what predicts Y, traded off by β), ["At β = 0 our quantization is the most sketchy possible—everything is assigned to a single point—while as β → ∞ we are pushed toward arbitrarily detailed quantization"](https://arxiv.org/pdf/physics/0004057#page=9), and the solutions ["separate (bifurcate) at some finite (critical) β through a second order phase transition"](https://arxiv.org/pdf/physics/0004057#page=15). In learned representations the transitions ["correspond to the onset of learning new classes"](https://arxiv.org/pdf/2001.01878#page=1). Clustering by deterministic annealing: ["as the temperature is lowered, the system undergoes a sequence of "phase transitions," which consists of natural cluster splits"](https://web.ece.ucsb.edu/publications/rose/pubs/pub42-Proc11-98.pdf#page=5).
- **Hierarchies.** Genewein et al. test ["the hypothesis that abstraction and hierarchical information processing might in fact be the consequence of limitations in information-processing power"](https://www.frontiersin.org/journals/robotics-and-ai/articles/10.3389/frobt.2015.00027/full#h1): ["Changing the temperature leads to a natural emergence of different levels of abstraction"](https://www.frontiersin.org/journals/robotics-and-ai/articles/10.3389/frobt.2015.00027/full#F2). Which pathway exists at all is a price question: when one inverse temperature beats the other, ["it becomes more economic to shift all the information processing to the cheaper pathway (either serial or parallel) thus rendering the other pathway obsolete"](https://www.frontiersin.org/journals/robotics-and-ai/articles/10.3389/frobt.2015.00027/full#F11).
- **Games.** Logit equilibria (every player a soft judge with the same temperature) start at uniform play and, ["as the error goes to zero, QRE approaches a subset of Nash equilibria and also implies a unique selection from the set of Nash equilibria in generic games"](http://www.dklevine.com/archive/refs4510.pdf#page=1). Ortega and Braun note ["The QRE model is a special case of the model presented here where all prior probabilities are assumed to be uniform."](https://arxiv.org/pdf/1204.6481#page=17), so the soft judge of §7 is the KL price with a uniform default.
- **Learned optimization.** A mesa-optimizer is a learned model that is itself an optimizer. Hubinger et al. hypothesize that ["penalizing description length will favor mesa-optimizers while penalizing time complexity will disfavor them"](https://arxiv.org/pdf/1906.01820#page=14), because ["a time penalty on the learned algorithm is likely to incentivize the base optimizer to do more precomputation itself"](https://arxiv.org/pdf/1906.01820#page=14). They also warn that the time penalty ["would also promote pseudo-alignment in those cases where mesa-optimizers do arise"](https://arxiv.org/pdf/1906.01820#page=20) (inner goals that match only on the training cases).
- **Evolution.** A wiring price switches evolved networks from tangled to modular (§5 pressure 5), and under a fixed goal the same process ["typically yields nonmodular solutions with fewer network motifs"](https://pmc.ncbi.nlm.nih.gov/articles/PMC1236541/#sec3).
- **Sparsity.** An L1 price switches parts off exactly rather than shrinking them (§6 table).

So "the regularizer changed the kind of agent" means a phase transition in the set of stable self-judges.

## 8. Randomness

- **What it is to an agent.** Bits that whoever models you cannot predict within their budget. Goldreich: ["(pseudo)randomness is not an inherent property of an object, but is rather subjective to the observer"](https://www.wisdom.weizmann.ac.il/~oded/PS/prg08.pdf#page=4), and his primer treats randomness ["as being relative to the observer's abilities (of analysis)"](https://www.wisdom.weizmann.ac.il/~oded/PS/prg08.pdf#page=7). It is relative, like agency (§4).
- **When it is needed.**
  - Something predicts you and profits from it: matching pennies, the liar against any fixed oracle (§2), follow-the-leader against an adversary (§6).
  - Parts that must agree while any of them might stall. Deterministic agreement is impossible: ["no completely asynchronous consensus protocol can tolerate even a single unannounced process death"](https://groups.csail.mit.edu/tds/papers/Lynch/jacm85.pdf#page=2). Ben-Or's coin-flipping protocol ["guarantees that as long as a majority of the processes continues to operate, a decision will be made"](https://cs.nyu.edu/~apanda/classes/sp25/papers/benor83.pdf#page=1).
- **When it is not.** If nothing predicts you, time can stand in for it: the tally in §2 produces the liar's mixed answer as a frequency, with no random bits and no heat.
- **Why it is hard to get.** Physical noise is cheap (the lattice's drive is thermal). The scarce thing is independence from the decider's own models: a mind's "random" choices come out of the same machinery that models patterns, so they carry those patterns.
  - **People.** ["People attempting to generate random sequences usually produce more alternations than expected by chance."](https://srri.umass.edu/sites/srri/files/FalkKonold1997/index.pdf#page=1) Wagenaar's 1972 review of the experiments: ["These randomization experiments generally show that man is a bad randomizer."](https://publications.tno.nl/publication/34643184/uVrpxSyQ/wagenaar1972diss.pdf#page=6)
  - **Cultures outsourced it.** Moore read Naskapi scapulimancy (cracks in a heated caribou shoulder blade choose where to hunt) this way: ["it is difficult for human beings to avoid patterning their behavior in a regular way"](https://cs.uwaterloo.ca/~shallit/Papers/ch.pdf#page=2), ["Unwitting regularities in behavior provide a basis for anticipatory response"](https://cs.uwaterloo.ca/~shallit/Papers/ch.pdf#page=2), and the rite is ["a very crude way of randomizing human behavior under conditions where avoiding fixed patterns of activity may be an advantage"](https://www.sciepublish.com/index/article/download_article/id/792.html#page=2). Henrich's gloss is the point here: it ["may have provided a crude randomizing device that helped hunters avoid their own decision-making biases"](https://www.sciepublish.com/index/article/download_article/id/792.html#page=4). Dove on Kantu' farmers: ["The Kantu' diversify, and avoid the temptation of prediction, by timing and locating their swiddens according to random bird omens."](https://web.archive.org/web/20240710081530/https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2400851)
  - **Contested.** Hong (2025) reviews critiques ["that have rendered its original formulation empirically and theoretically untenable"](https://www.sciepublish.com/index/article/download_article/id/792.html#page=1), and notes ["caribou lack the capacity for anticipatory response"](https://www.sciepublish.com/index/article/download_article/id/792.html#page=3). What survives is the weaker claim: the device protects the deciders from their own patterns.
  - **Machines.** Pseudorandomness is the computational version: bits made cheaply that the observer cannot afford to predict.
- **Its price on a reversible lattice.** Random bits come in from outside and leave as heat once forgotten: a step loses information exactly ["if the output of a device does not uniquely define the inputs"](https://sites.pitt.edu/~jdnorton/lectures/Rotman_Summer_School_2013/thermo_computing_docs/Landauer_1961.pdf#page=4).

## 9. What this means for disp

- **disp's judges on the ladder.** A type is a program answering yes or no about a value, checked under a step budget that may end "refuted", "no counterexample within budget" or "proven" (`research/constrained-program-search/gradual-fuzzy-types.md:3`): the gap rung, where "unknown" means "ran out before deciding". The optimizer in `GOALS.md` judges programs with scorers and is meant to "self-play, continually improve by generating score functions itself": a self-judge. Today it pays with self-trust (§3 row 6).
- **The optimizer's cost terms choose its kind.** Size, steps and memory penalties are prices, and §6's table predicts the searchers each produces; `research/NATURE_OF_OPTIMIZERS.md` §6.5 (an optimizer that grows an inner optimizer) is the description-length row. Quantilizing at readback is an explicit price on deviating from the prior.
- **On the lattice, as hypotheses (untested).**
  - Cheap copies and costly heat should favor many small parts with private histories that run back instead of forgetting.
  - Costly copies should favor fewer, shared parts whose histories tangle (the failure the reversible core's review found in Riders, its design that read code in place instead of copying it).
  - A thermal drive makes randomness cheap but locally correlated, so mixed strategies are cheap only where parts are far enough apart to be independent.
- **Experiments that would test this (none started).**
  1. A soft judge in disp over fractions: answer "above r" by a ramp of half-width w and check that the truth-teller's selves split at w = ½ (the ramp's slope is 1/(2w), so w plays the role of 2T).
  2. A price sweep in the optimizer: the same search under size penalties versus step penalties, looking for a switch between inner search and tables.
  3. A weighted committee of candidate heuristics as the optimizer's learned layer (`research/NATURE_OF_OPTIMIZERS.md` §4), with its log-N price measured.
  4. On the lattice: sweep the heat price against the fuel price and count how many independent histories a run keeps.

## 10. Open questions

- Is there one measurable number that predicts where an agent's boundary goes, such as the mutual information across a cut, and can the lattice measure it?
- What is the exchange rate between the three currencies for self-knowledge (copies, rounds, random bits), in the way Bennett's pebbling trades time for space?
- Do the critical prices (T = ¼ for the logistic judge, w = ½ for the ramp) have a resource meaning, such as bits per decision?
- When a judge is made of parts, which part does the self-judging? Markets suggest none of them: the price does.

## 11. Verification notes

- **Links.** Every link was fetched with `scripts/verify-source.sh` and every quote located in the fetched text; fragments are the script's, with these exceptions. The LessWrong anchors (`#Naturalism`, `#Preference_By_Committee`, `#Conclusion`) come from LessWrong's table of contents, because the script reads posts through GraphQL and finds no heading ids. Genewein et al.'s `#F2` and `#F11` are the nearest figure ids before each quote. Dove is cited through a Wayback capture of his SSRN abstract (the live page returns 403), so it has no fragment.
- **Secondhand quotes.** Moore 1957 (American Anthropologist; Wiley returns 403) is quoted through Shallit's paper and Hong 2025, and Henrich 2016 through Hong 2025. Simon 1955 is the 2010 Competition Policy International reprint, which says it is ["reprinted here as it was published in 1955"](https://competitionpolicyinternational.com/assets/0d358061e11f2708ad9d62634c6c40ad/SimonwCover.pdf#page=3). McKelvey and Palfrey is an OCR scan served over http only; the quoted words are exact.
- **Versions.** The Tiling Agents and Definability links are 2013 drafts. Goldreich is his 2008 draft of the 2010 primer. Boyd, Mandal and Crutchfield's arXiv v1 carries an earlier title than the published Phys. Rev. X paper. arXiv links are unversioned; page numbers match v5 of 1609.03543 and 1602.04184 and v1 of 1508.04145 and 1605.03142.
- **Claims checked and narrowed.** The Definability draft never frames itself around agents, so it is cited only for the mathematics. The tiling paper's "only weaker successors" is the naive reading, which it then works around. Hubinger et al. state the simplicity-versus-speed effect as a hypothesis. Kashtan and Alon's effect needs goals that share subgoals. Wentworth's committees lack a complete utility function but still have a weak one. Hong and Henrich 2021, sometimes cited for divination as randomizing, never discusses it; Hong 2025 does. Ortega and Braun is quoted from arXiv v2, not the 2013 journal version.
- **Computed for this note.** §7's tables: the logistic soft judge, fixed points found by sign changes on a 200,000-point grid plus bisection, stability from the slope, undamped iteration run 5,000 steps from 0.123, damped iteration with step 0.05 for 20,000 steps; critical temperatures by bisection on the fixed-point count (truth-teller, r = 0.3) and on slope = 1 (liar, r = 0.3). The Curie–Weiss mapping and the quantilizer's log(1/q) are short derivations, not citations.
- **disp snippets.** Both blocks together ran under `scripts/probe.sh -r` (the raw prelude): 8 of 8 tests pass in 0.4 s. The same tally code under the full kernel probe (no `-r`) was killed at 2.1 GB after 56 s on a cold cache; not investigated.
