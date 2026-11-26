# Finding an ideal interaction net: what "ideal" means, a score, and a search plan

The question: how could one find the *best* interaction-net system for a reflective λ-calculus (one whose programs can read their own code), say formally what "best" means, and write a score that an ideal system would minimize?

Short answer. "Ideal" splits into two parts. Hard constraints decide what counts as a system at all: a fixed finite set of agent kinds, honest copying and erasing, correct on λ-programs, and able to read itself. A score then ranks the systems that pass: the bits needed to write down the rule table, plus the bits of a reader and a self-interpreter written *in the system itself*. The table is coded so that a rule with a generic shape costs about one bit, and only exceptions cost their full size. Run time enters at log scale, if at all. On the systems measured, this score puts Lafont's interaction combinators first in their class and agrees with the user's verdict on the table design. Literal-size scores get both of those wrong.

How to read this. *Measured* means a number from a probe in `research/linear-ic-net/ideal/`; the commands are at the end. *Inference* marks reasoning that no probe or source checks. Words used throughout:
- **agent**: a node of the net.
- **kind**: an agent's label.
- **principal port**: each agent's one main port.
- **auxiliary ports**: its other ports.
- **active pair**: two agents joined principal to principal, which a rule rewrites.
- **right side**: what the rule puts in the pair's place.
- **made agents**: the agents a right side creates.

## 1. What "ideal" means

A **system** is a signature plus a table. The signature is a finite list of kinds, each with an arity (number of auxiliary ports) and, if the system is polarized, a direction per port. The table has at most one rule per pair of kinds. A rule's right side is a list of made agents and a wiring: a perfect matching of the pair's auxiliary wires and the made agents' ports.

Hard constraints. A system that fails any of these is out, whatever its score.

| | constraint | formal statement | where it is checked now |
|---|---|---|---|
| H1 | well formed, polarized, deterministic | every rule matches every port exactly once; every wire joins an out port to an in port; one rule per pair | `TableOk` in `lean/Net.lean`, by `decide` |
| H2 | **uniform** | the list of kinds and the table are fixed before any program is seen, so every right side has a size bounded independently of the program | the type of `Admissible` in `lean/Ideal.lean` (new) |
| H3 | total | every pair that can meet has a rule, or is a declared stuck pair (applying code is a deliberate error) | `Total` in `lean/Ideal.lean`; `covered` in `lean/Tables.lean` |
| H4 | **resource-honest** | only a rule with a copier on its left side makes copiers; only one with an eraser makes erasers | `Honest` in `lean/Ideal.lean`; `only_copying_copies`, `only_erasing_drops` in `lean/Tables.lean` |
| H5 | universal | a compositional compiler, a fixed small net per λ-construct, whose nets evaluate to the reference value on every test program and on random held-out ones | `candidateOk`, `rln_every_order` in `lean/Checked.lean` (per program) |
| H6 | reflective | an in-system reader whose views tell every kind apart, and whose parts give back the code (round trip) | `views_tell_kinds_apart` (kinds only) |
| H7 | self-interpreting | a "full" self-interpreter written in the system (one that interprets function bodies, not one that hands every call to the built-in evaluator) is correct on the test programs | `nets/selfinterp.py`, `nets/tables_check.py` (Python) |
| H8 (optional) | garbage-free / reversible | no erasers needed on linear programs; every rule invertible | `nets/linear.py` (Python) |

H7 is what catches the first design, where reading an opened function looked like reading a closed one. That design passes every table theorem except `conflated_readers_share_an_arm`, and an interpreter written on it gets 6 of 14 programs wrong (*measured* earlier in `lean/PATHWAY.md`, not by this session's probes).

## 2. The score

Among systems that pass H1–H7:

**J(S) = sig(S) + table(S) + prog_S(reader) + prog_S(self-interpreter)**, in bits.

- `sig`: an Elias-gamma code (a standard variable-length code for whole numbers) for each kind's arity, plus a bit per port direction.
- `table`, coded as **default plus exceptions**. There is one global default rule: two agents of the same kind annihilate (their auxiliary wires are joined straight across and both agents vanish), and two of different kinds commute (Lafont's commutation: each agent is copied once per auxiliary port of the other, and the copies are wired in a grid). Every pair that can meet costs 1 bit, saying whether it follows the default. A pair that does not pays a tag of log2 5 bits for {no rule, annihilate with a permutation, commute with substituted kinds, pass-on, literal} plus that choice's parameters. A literal rule pays for its made-agent list and log2 of the number of possible wirings of its ports.
- `prog_S(p)`: the program p compiled into a net of the system: log2(kinds) bits per agent plus log2 of the number of possible wirings of its ports. This follows Tromp, who sizes a self-interpreter in its own language ([a self-interpreter of size 206](https://tromp.github.io/cl/LC.pdf#page=5) bits for binary λ-calculus) and uses those sizes to compare languages: ["we conclude that λ-calculus is a much more concise language than CL"](https://tromp.github.io/cl/LC.pdf#page=10). Counting the program in the system's own agents means a big built-in primitive can't hide: it shows up in `table`.

Second key, compared only between systems whose J is close or as a Pareto front (the set of systems that no other beats on both keys): cost on a benchmark. The cost terms are rewrites per read, erasures (heat), and compile size per λ-node. If a single number is required, use **J + Σ over the benchmark of log2(rewrites)**. That is length plus log time, the exchange rate surveyed (with sources) in [`../Proof length minimization reward hacking/theory_of_simplicity_measures.md`](../Proof%20length%20minimization%20reward%20hacking/theory_of_simplicity_measures.md#q2-kolmogorov-complexity-and-mdl-reference-machine-dependence-libraries-incomputability-and-resource-bounded-variants). Never add raw rewrites (section 4 shows why).

Optional term for "rules as matter": prog_S of an in-system interpreter for *any* rule table given as data. That would internalize Lafont's universality, which is a translation done outside the system. Not measured.

## 3. Validation

### 3a. Interaction combinators: the code decides who wins

Lafont's system has [three symbols and six rules](https://dcreager.net/pdf/Lafont1997.pdf#page=1): ε (an eraser, no auxiliary ports), γ and δ (two auxiliary ports each). His §2.8 says the system [remains universal if one removes](https://dcreager.net/pdf/Lafont1997.pdf#page=22) the δ⋈ε rule. He also reports a [universal interaction system with only two symbols and three rules](https://dcreager.net/pdf/Lafont1997.pdf#page=22) by Bechet, but says [one of his rules is quite complicated](https://dcreager.net/pdf/Lafont1997.pdf#page=22), and conjectures that the combinators are ["essentially the simplest one satisfying Theorem 1"](https://dcreager.net/pdf/Lafont1997.pdf#page=22). So a good score must rank IC (interaction combinators) above both.

*Measured* (`score.py`), in bits including the signature:

| system | kinds | rules | literal code | per-rule shape code | default + exceptions |
|---|---|---|---|---|---|
| interaction combinators | 3 | 6 | 84 | 48 | **17** |
| IC without δ⋈ε | 3 | 5 | 73 | 42 | 19 |

Counting symbols and rules ranks Bechet's system first and IC third. The literal code and the per-rule shape code both prefer IC without δ⋈ε. Only the default code puts IC first, because all six IC rules are the default (same kind annihilates, different kinds commute: ε⋈γ is a commutation that leaves two erasers). IC's whole table costs 6 bits, one per pair. *Inference:* the elegance of IC is that its table is *generated by one rule about rules*, and a literal-size score cannot see that.

*Measured* (`minimal.py`): all 7,795 systems with 1–3 kinds (arity ≤ 3 for 1–2 kinds, ≤ 2 for 3 kinds) whose rules use only the two generic shapes. The filter applies Lafont's necessary conditions, read as general ones (*Inference*): some rule [increases the number of cells](https://dcreager.net/pdf/Lafont1997.pdf#page=22); some rule has an empty right side; some rule is a pure nonempty wiring. It adds one more condition: some rule makes more agents with ports, since growth in erasers alone can't loop. 156 systems pass, **none with 1 or 2 kinds**. IC is first at 17.0 bits, tied with exactly one other system: ε, a one-port agent, and δ. The score can't separate those two; a universality certificate must. *Inference:* Lafont's remark that both γ⋈γ and δ⋈δ are needed suggests the one-port look-alike is not universal, but nothing here checks it.

Tree calculus was not scored. *Inference:* on λ-programs it fails H5's bounded compile size. The repo measured quadratic blowup on λx1…xn. xn…x1: 72 tree nodes at n = 4 and 1,134 at n = 16 (`nets/DESIGN.md`). On its own spec (programs are trees) it would need its own benchmark.

### 3b. The two reflective designs

*Measured* (`score.py`; tables pulled from `lean/Tables.lean` through `ideal/dump_old.lean`, and from `nets/tables.py` by firing each pair once):

| | old design (inert code, λ-view reader) | table design (closures `clo_k` + kept table) |
|---|---|---|
| kinds | 13, fixed | 10 + 2 per definition: 64 for `size`, 120 for `interp`, 140 for `interp` on 1+1 |
| rules | 33 | 345 for `size`, 681 for `interp` |
| largest right side (made agents) | 18, fixed | 38 for `size`, 24 for `interp`: depends on the program |
| non-copy rules that make copiers (H4) | 0 (also Lean-proven: `rln_honest`) | 76 for `size`, 158 for `interp`: every `Ctab ⋈ X` makes a `dup` |
| fixed table, bits | 1,202 (991 with the default code) | 630 |
| `interp` as a net | 249 agents, 3,074 bits | 8 agents, 76 bits, plus 24,721 bits of rules its 55 definitions add (5,081 of them the layout rules alone) |
| J = table + interp | **4,276** | 25,427; 5,787 even if only layout rules are charged |

The table design fails H2 (uniformity) and H4 (honesty) outright. The score agrees on its own: even if the per-definition copy, erase, thaw and read rules are treated as free, it still costs about 1,500 more bits. One reason (*measured* via the rule extraction): every definition is laid out twice, once running (`O ⋈ clo_k`) and once as code (`O ⋈ cloL_k`). The surprise inside this result is that the table design's *fixed* part is cheaper (630 against 1,202 bits). All of its cost comes from code moved into the rule table.

In the old design, the five view rules (the λ-terms the reader builds) take 762 of the 1,202 bits (63%). The copy and erase rules are all commutations and cost about 10–18 bits each. *Inference:* the score points at the views as the place to improve. A resource-honest native case agent, one that hands the other arms and the parts to the chosen arm through a chain of applications, would take about 291 bits by the same code (a rough count in `score.py`, not a built design).

Cost, *measured* (rewrites; reads; erase rewrites):

| program | old | table design | log2 ratio |
|---|---|---|---|
| `size suc` | 20,803; 10; 11,498 | 128; 10; 32 | 7.3 bits |
| `size size` | 165,713; 80; 91,009 | 1,938; 158; 462 | 6.4 bits |
| `interp` on 1+1 | 35,745; 27; 20,466 | 386; 26; 155 | 6.5 bits |

On log scale, the table design's speed is worth 20.3 bits over the three programs. Its description costs at least 1,511 bits more. So **J + Σ log2(rewrites) agrees with the user's verdict; J + Σ rewrites does not**: 222,261 against 2,452 rewrites would swamp every description term. Which exchange rate you pick is the single weight that decides this comparison.

## 4. How the score can be gamed, and the fix for each

| exploit | what happens | fix |
|---|---|---|
| no rules at all | the empty table is shortest | H5: correctness on λ-programs, including random held-out ones |
| put the program in the agent labels | `clo_k` per definition, or one agent whose label encodes the body (a Gödel number) makes the reader 1 rewrite and the program 8 agents | H2: the kinds are one finite list fixed before any program; or charge the per-program table to the program (25,427 bits, measured) |
| giant primitive (a built-in `eval` or reader agent) | the in-system interpreter becomes one line | its rules are counted in `table`; H7 asks for a *full* interpreter, so deferring to a built-in thaw on function bodies doesn't count |
| hidden copying in a case | a case that copies the taken arm avoids handing arms on | H4 (the kept table fails it) |
| drop the unchosen arms | a case that erases arms is shorter than a view that hands them on | H4 (only erasers erase); it is what forces the view to hand the other arms to the chosen one |
| arity inflation | one agent with many ports replacing many agents lowers the kind count | count bits of wiring, not kinds or rules; the wiring term grows like n log n in the ports |
| declare pairs stuck | fewer rules to write | stuck pairs cost a tag each in the default code; H3 lists them, and tests check they never form except where intended |
| an uninformative reader | a reader that returns a constant view still "yields something to branch on" | H6 needs views that tell kinds apart and round-trip; H7 then checks an interpreter can be built on it |
| exceptions dressed as rules | a table of many different literal rules that each happen to be short | the default code charges 1 bit per pair plus a tag per exception, so "almost the default" beats "short but irregular" |
| overfitting the benchmark | a rule specialized to `size` | log-scale time (a specialized rule buys a few bits at most); random held-out programs; charge the in-system programs, not their run time |
| a cheap compile step outside the system | bracket abstraction hidden in the compiler | H5's compiler must be compositional with a constant-size net per construct (tier 2's `agents_le_twice` is the existing check) |
| a cheap encoding chosen per system | the description code itself becomes a free parameter | fix the code (bits per agent, wirings as matchings, the five tags) once, for all systems; report the literal code beside it as a check |

## 5. Weights: a lexicographic order, not one weighted sum

*Inference*, with the measured numbers above behind it:
- Hard constraints first, never as penalties. A penalty for non-uniformity can always be outbid by speed. The table design takes 85–160× fewer rewrites on these programs.
- Then J. Within J, the choice of table code decides the ranking (section 3a): literal size picks IC without δ⋈ε, the default code picks IC. Use the default code and report the literal one as a check.
- Then cost, at log scale, as a tie-breaker or a Pareto front. At 1 bit per doubling, that speed-up buys the table design 6.4–7.3 bits per program.
- J's two halves need no weight between them: both are bits in the same code. The one free weight is how many benchmark programs the cost term sums over. More reader-heavy programs make speed count more.

## 6. Search

How big the space is (*measured*, `count.py`). Counts are of labeled right sides, so they are upper bounds; dividing by renamings of made agents shrinks them by at most 24× at 4 made agents.

| space | size |
|---|---|
| unpolarized, 3 kinds, arity ≤ 2, right sides ≤ 4 agents | 10^49 systems (all signatures) |
| the IC signature (0, 2, 2) alone, right sides ≤ 4 | 10^40.8 systems; up to 37 million right sides for one pair |
| unpolarized, 4 kinds, arity ≤ 3, right sides ≤ 3 | 10^93 |
| the old 13-kind signature, polarized, right sides ≤ 3 | 42 pairs can meet; 3.6 million right sides in all (up to 185k for one pair); 10^200 whole tables |
| generic shapes only, ≤ 3 kinds, arity ≤ 2 / ≤ 4 kinds, arity ≤ 3 / ≤ 5 kinds, arity ≤ 3 | 7,233 / 3.5×10^8 / 8.4×10^12 |
| the old design's view rules | 14–18 made agents each: out of reach of any enumeration of wirings |

Whole tables can never be enumerated. The plan that is feasible:

1. **Shape level, exhaustive.** Enumerate signatures and, for each pair, a shape {none, annihilate(permutation), commute(substitution), pass-on, literal hole}. Prune with the necessary conditions and H1–H4, which are local checks. Up to 3.5×10^8 systems at 4 kinds is a job for compiled code, not Python; 3 kinds runs in seconds (*measured*: `minimal.py`). For the reflective problem, fix the resource layer by shape: `copy_forced` and `erase_forced` in `Tables.lean` already prove the copy and erase rules forced once the code kinds are fixed. That leaves only the computing pairs (apply, thaw, read) as holes: 11 of the old design's 33 rules.
2. **Holes by counterexample-guided search (CEGIS).** Propose a right side for each hole (≤ 3 made agents: at most 185k per pair, *measured*), run the test programs, and on a failure record a nogood: the set of hole choices that fired in the failing run. Runs touch few rules, so nogoods are small. `lake exe forced` already does the one-hole version (up to 40,320 wirings for one rule, all 28 rules in about 1.5 minutes on 16 cores, per `lean/PATHWAY.md`). *Inference:* ~10^6 candidate evaluations for 11 near-independent holes is roughly a 15-minute to 2-hour run at that rate. A SAT solver can propose candidates under H1–H4, but reduction itself (unbounded steps) is a poor fit for SAT.
3. **Reflection as a parameter, not a wiring.** Search the reader over a small family: a λ-view (arm order, part order) or a native case shape, scored by J. Neither can be enumerated as free wirings.
4. **Impossibility by `decide`.** On a bounded shape space with a finite test set, "no admissible table with ≤ n kinds passes" is a finite statement. `native_decide` can prove it, turning "we searched" into a theorem in the style of `copy_forced`.

## 7. Formal statement

`lean/Ideal.lean` (not in the default build; `lake env lean Ideal.lean`, compiles with no errors) defines, over the generic `Sig`/`GRule` of `Net.lean`:
- the shape tests `GRule.isAnn`, `isCom` (with oriented copies allowed) and `isPass`;
- the predicates `Total` and `Honest`;
- `Admissible`: `TableOk`, plus totality, honesty, and closure over one fixed kind list, which is H2 as a type;
- `ruleBits`, `sigBits` and `score`;
- `Ideal Spec`: admissible, meets `Spec`, and no admissible system of *any* kind type meeting `Spec` scores lower.

`Spec` stays abstract: it takes the per-program run checks of `Checked.lean` and the measured program bits. Proven there: `rln_admissible`, meaning the old table passes H1–H4 by `decide`. Its `#eval` gives 25 of 33 rules with a generic shape and a table score of 1,283 bits, a coarser code than `score.py`'s. Limit: `TableOk` asks for opposite principal polarities, and IC can't be polarized (γ⋈γ), so IC needs an unpolarized `TableOk` before it can be stated there.

## 8. One correction to the working hypotheses

"A local case can branch only on the kind of agent met and the port entered": a rule fires only at two principal ports, so it branches on the **pair of kinds** alone. Which auxiliary wire leads where is fixed by the rule, not chosen by it. *Inference* from the definition. It makes the reflection problem sharper: anything a reader tells apart must already be a difference of kind.

## Commands

From `research/linear-ic-net/`:
- `cd lean && lake env lean ../ideal/dump_old.lean > ../ideal/old_table.jsonl`: the old table as JSON.
- `systemd-run --user --scope -q -p MemoryMax=4G -p MemorySwapMax=1G timeout 900 python ideal/count.py`: section 6.
- `... python ideal/score.py` (add `-v` for each rule's code): sections 3a and 3b, about 20 s.
- `... python ideal/minimal.py`: section 3a's enumeration, a few seconds.
- `cd lean && lake env lean Ideal.lean`: section 7.
