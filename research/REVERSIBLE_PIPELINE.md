# A reversible pipeline: text to answer and back

The question: can disp go Text ↔ (AST, layout and sugar) ↔ (application tree, names) ↔ (answer,
what reduction forgot), each step a bijection, ideally because every base operation is reversible and
the "irreversible" ones keep what they would throw away in a form that can be processed again? How
far is the strand lattice from running that way, and what would it take?

## Summary

- **One idea covers every stage.** A stage f : A → B that forgets things becomes a bijection
  A ≅ B × R once it also returns its residue R, what it forgot, and stages compose. R is the view's
  complement, which
  ["must include at least all information missing from the view"](https://www.cis.upenn.edu/~bcpierce/papers/lenses-toplas-final.pdf#page=50). Parsing forgets
  layout and comments (R = trivia), elaboration forgets names, sugar and which of several sources
  compiled to a tree (R = the origin table of research/TREE_READBACK.md plus choice bits), reduction
  forgets the redexes it consumed (R = a history). If each stage is itself written as a program, a
  reversible evaluator is the only mechanism needed, and each residue is that evaluator's history.
  This is Bennett's construction: keep a history, make an
  ["extra copy"](https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/Bennett_Reversibiity.pdf#page=2)
  of the result, then
  ["reversibly disposes of all the undesired intermediate results by retracing the steps of the first stage in backward order"](https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/Bennett_Reversibiity.pdf#page=1).
  The "store it instead of erasing it" form of this is James and Sabry's: a reversible core plus
  ["a global garbage dump (which absorbs the undesired results)"](https://web.archive.org/web/20161007070937id_/http://www.cs.indiana.edu/~sabry/papers/information-effects.pdf#page=2),
  which ["exposes creation and erasure of information as explicit effect operations"](https://web.archive.org/web/20161007070937id_/http://www.cs.indiana.edu/~sabry/papers/information-effects.pdf#page=1).
- **The lattice's rewrites are already mostly reversible.** On five disp programs, 75–84% of rewrites
  (67–72% with copying ahead) lose nothing (they only rearrange, build or copy). The rest are the K step dropping an argument
  (`T1·L`), the second dispatch dropping the arms it did not take (`Sel·*`), and erasers removing what
  those drop. Of the agents erasers remove, 90–98% were made by a duplicator: they are copies, and
  erasing one of two equal copies loses nothing. What is really lost (originals, and copies whose twin
  went too) is 25–112 agents per run, against 2,100–9,400 rewrites.
- **What is missing:** (1) a record of which rule fired where, so running backward is deterministic
  (Bennett's history; here one small token per rewrite, 2.4–8× the run's peak sites); (2) erasers that
  merge a copy back into its duplicator ("uncopy") and keep the rare original as residue; (3) moves
  that are a bijection of the lattice's state: the schedule is already deterministic (its dice are a
  hash of site and clock), but Metropolis acceptance and "try a move, else do nothing" map two states
  to one; a local energy bank per site (a demon) and moves that are their own inverse fix that;
  (4) collection and cooling become an explicit, separate act of forgetting.
- **Bijective programs can leave no residue at all**, by computing, copying the answer, and running
  backward to clean up, given the inverse program (Bennett's case of
  ["many calculations in which the output uniquely determines the input"](https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/Bennett_Reversibiity.pdf#page=5));
  or by being written in a reversible style in which K and triage hand back what they would drop.
  Nothing more is possible: reversible machines
  ["can compute exactly all injective, computable functions"](https://researchprofiles.ku.dk/en/publications/what-do-reversible-programs-compute/#main-content).

## Step 1, done: the net run backward

`evaluators/rust-ic-strands/crate/src/rev.rs` (README section Running backward, `player/rev.html`)
runs the abstract net with every rewrite an event that keeps the agents it used up and where their
ports now lead. What it showed:
- **Exact, in any causal order.** An event can be undone while everything it made is still there;
  86 terms run forward, partly back, forward again and then back in random order all come back to
  the loaded net, agent for agent.
- **Erasing need not be avoided.** Once each erasing step is recorded, nothing is lost, and keeping
  erased values whole stores about as much (1 + 1: 1,490 events and 642 agents of residue, against
  1,936 events and none). The sketch below expected residue to be the way; it is not needed.
- **Un-copy is the lever**: as recorded events, the lattice's collection cuts the history 3.6–10×.
- **The history is 15–40× the loaded program** (about 2.3 used-up agents an event). Shrinking it is
  the next problem: Bennett's pebbling, or forgetting on purpose.
- **Bennett's copy needs a reader that does not consume.** Copying the answer out and running
  backward fails as stated: the duplicator uses the answer up, so the steps that made the answer
  cannot be undone without undoing the copy. Garbage-free bijections (step 2) need a copy that reads
  the answer and leaves it in place, such as a token walking the tree.

## Native: two-way programs, no receipts at all

Receipts exist only because the tree calculus's rules destroy information: K drops an argument,
dispatch drops arms, and every rewrite forgets which rule made its result. A calculus whose steps
destroy nothing needs none. James and Sabry build one from
["the notion of type isomorphisms"](https://web.archive.org/web/20161007070937id_/http://www.cs.indiana.edu/~sabry/papers/information-effects.pdf#page=1);
`evaluators/rust-ic-strands/crate/src/iso.rs` is one on trees (README section Two-way programs), with
copying and its partial inverse added (Π has no copying: its isomorphisms
["do not include some familiar logical tautologies"](https://web.archive.org/web/20161007070937id_/http://www.cs.indiana.edu/~sabry/papers/information-effects.pdf#page=3)
such as b ↔ b × b). Its rearrangements by pattern do every swap and regrouping; a choice's arms must
give different shapes, the reversible functional languages' rule that
["the leaf expression of any branch in a case expression must not match any leaf expression in a branch preceding it"](https://arxiv.org/pdf/2105.09929#page=2);
a loop states tests for its first and last rounds, as Janus's loops do.
- **The pipeline is a tree.** Programs, data, the library and the trace are all trees. `run` runs a
  program that arrives as data; `dagger`, a two-way program, inverts programs given as trees.
- **A machine walking the program needs no record** (tried and removed, commit eca0073). Only the
  token's place and what it carries change, so turning it around runs it back. Danos and Regnier's
  interaction abstract machine (the geometry of interaction's token machine) is reversible for the
  same reason: it
  ["needs no external garbage-collection"](https://www.pure.ed.ac.uk/ws/files/16874549/Reversible_irreversible_and_optimal_machines.pdf#page=4),
  where a machine that keeps results as it goes collects information that
  ["grows steadily at each step"](https://www.pure.ed.ac.uk/ws/files/16874549/Reversible_irreversible_and_optimal_machines.pdf#page=4).
- **The tree calculus is derived**, as the two-way program `tc`, from rearrangement, copying and calls:
  K puts the dropped argument on a trace, S copies its argument and makes three applications, triage
  puts the arms it does not take on the trace. It evaluates eagerly, which disp's programs survive (all
  six tried give the lazy oracle's answers, in 300–1,500 applications), so current disp runs on it, and
  runs back exactly, with the trace (receipts as an ordinary tree) handed back beside the answer.
- **What the trace holds, and sharing.** Mostly copies of code: a recursive function copies itself at
  each call and K throws the copy away at the base case (87% of `add 1 1`'s trace; 10,828 nodes, 427
  distinct). With copies sharing what they copy (a copy one more holder; a shared tree copied one layer
  at a time only when taken apart, as the strand lattice's duplicators copy), the trace held pointers
  and memory fell 8–12 times on the token machine (`add 1 1` 10,813 → 1,406 nodes, `fib 3` 82,282 → 6,570). What is left is
  about one small entry per application, saying which rule fired. Keeping the applied program instead of
  using it up (`tck`) lets that be read off the program, leaving only what the data decides: a third
  fewer entries and a quarter less memory, for 26% more steps. Staged uncomputing (pebbling) trades the
  rest for time.
- **Flexible.** The trace can be kept, cleaned up by Bennett's construction (`bennett`: twice the steps,
  leaving program, arguments and answer), or thrown away (`forget`, the one step with no inverse). A disp
  program that is a bijection runs with nothing left over given its inverse, by Bennett's second step
  (`bijection rev rev [1, 2, 3]`). Programs written two-way have no trace to clean.
- **The limit.** Without receipts, exactly the injective functions (Axelsen and Glück, above). A
  bijection runs without leftovers; anything else hands back what it would have dropped.
- **Is it slower?** Not by itself: a two-way program for an injective algorithm takes about the steps
  of the one-way one (a loop reverses n items in 4n + 11 steps; recursion takes order n²). The cost is in
  what cannot be dropped. Keeping it costs space (step 1's history, 15–40 times the program; the derived
  tree calculus's trace, 10,000–80,000 nodes for small disp runs; on the lattice, matter that crowds
  wires). Getting rid of it costs time: a value can only be removed by computing it backward from what
  came before it, the pebble game's rule that one may
  ["remove a pebble from node i, provided node i - 1 is pebbled at the time"](https://arxiv.org/pdf/quant-ph/9703022#page=7),
  so saving space means recomputing later. The trade runs from
  ["the LMT-simulation using exponential time and no extra space"](https://arxiv.org/pdf/quant-ph/0101133#page=9)
  to Bennett's square space and subquadratic time. An irreversible machine deletes for free in time and
  pays in heat (Landauer, above). A token machine also walks again what a sharing machine would look up,
  which is what Danos and Regnier's jumping machine
  ["shortcuts redundant steps"](https://www.pure.ed.ac.uk/ws/files/16874549/Reversible_irreversible_and_optimal_machines.pdf#page=4)
  of.
- **Open.** A smaller instruction set in the tree calculus's style (one operator, rules by shape) has
  not been found: the tree calculus's own building rules (`△ x = △x`, `△a x = △ a x`) are already
  two-way, but K, S and triage each need a replacement that keeps what they drop. Calling two-way code
  from disp without the trace needs a way to mark it in the tree calculus's trees. The token machines
  ran with few tokens busy (relay's adder never more than 5, its rounds doubling with its bits where
  the lazy net's grow by about 140) and did everything where a token was: in a net, reading a node turns
  it toward the reader, so a second reader needs its own copy of the code. Lazy evaluation is where
  disp's parallelism is.

## How the stages decompose

| stage | forward | residue that makes it a bijection |
|---|---|---|
| text ↔ AST | parse | layout, comments, the exact spelling of each token: a lossless syntax tree, as rust-analyzer's (["All comments and whitespace get preserved"](https://raw.githubusercontent.com/rust-lang/rust-analyzer/master/docs/book/src/contributing/syntax.md#design-goals)) |
| AST ↔ application tree | desugar, resolve names, bracket abstraction, compile-time evaluation | names written, binder names, spans, which sugar, which of the sources that compile alike (η, `S (K p) (K q) = K (p q)`), the closed applications run while compiling |
| application tree ↔ answer | reduction | the history: which rule fired where, and what each step dropped |

Today the first two stages keep none of it: `tokenize` (`src/parse.ts`) drops spaces, tabs and
comments and keeps only line numbers, `Expr` nodes have no spans, and names are resolved to trees.
Most of the middle residue is the origin table research/TREE_READBACK.md proposes: with it, reading
back is `put` in lens terms, and with nothing it is `create`. Bracket abstraction is many to one, so
its residue includes choice bits: which preimage a tree came from.

**What a residue has to hold depends on what is kept fixed.** From an answer alone, getting back the
text means storing nearly all of the program: `5` says little about how it was computed, so the
residue of text ↔ answer is about the size of the text. The bijections worth having keep the
program fixed: input ↔ output for a program that is a bijection (residue nothing, below), and a
run ↔ its earlier states (residue the history, which is what time travel needs).

The last stage is the one that needs a reversible evaluator. Two levels:
- **Logical**: the net's rewriting can be run backward to the input from the answer and the history.
  This is what a bijective pipeline needs, and it can run on the lattice as it is (moves stay random).
- **Physical**: the lattice's whole state evolves by a bijection, clock by clock. This is what
  low-energy hardware would want: a step is irreversible
  ["if the output of a device does not uniquely define the inputs"](https://sites.pitt.edu/~jdnorton/lectures/Rotman_Summer_School_2013/thermo_computing_docs/Landauer_1961.pdf#page=4),
  and costs ["of the order of kT for each irreversible function"](https://sites.pitt.edu/~jdnorton/lectures/Rotman_Summer_School_2013/thermo_computing_docs/Landauer_1961.pdf#page=1).
  It needs reversible moves too.

## What the lattice loses today (measured)

`programs/bench.ts --json` (rewrites by rule) and `TRACE` (which rewrite made each erased agent), one
seed each, lazy copying (`dnroom=0`) and copying ahead (`dnroom=5`):

| program | rewrites | keep everything | drop (K, dispatch) | erase | erased agents that were copies |
|---|---|---|---|---|---|
| `add 2 3` | 2,109 / 3,373 | 1,782 / 2,420 | 167 / 167 | 160 / 786 | 145 of 161 / 777 of 790 |
| `relay_add 13 6` | 5,854 / 7,732 | 4,606 / 5,542 | 372 / 365 | 876 / 1,825 | 627 of 700 / 1,215 of 1,234 (arguments given as data) |
| `rev [1, 2]` | 5,164 / 7,571 | 3,877 / 5,094 | 347 / 347 | 940 / 2,130 | 877 of 940 / 2,111 of 2,139 |
| `fib 3` | 8,831 / 12,425 | 6,600 / 8,429 | 556 / 556 | 1,675 / 3,440 | (`fib 2`: 928 of 1,018 / 1,882 of 1,911) |
| `isort [2, 1]` | 9,393 / 13,331 | 7,159 / 9,133 | 601 / 601 | 1,633 / 3,597 | |

- **Other compilers' code looks alike.** The lambada benchmarks (`fib:1`, `fib:2`, `sort:1`, `exp:1`,
  lazily): 62–85% of rewrites keep everything, 75–89% of erased agents are copies, and 4–305 agents'
  worth are really lost per run.
- **Dropping is mostly un-copying.** Bracket abstraction routes an argument with S, which copies it
  for both sides, and K, which drops a side's copy where it is unused. In the net that is a duplicator
  and an eraser on one of its copies. Collection already turns "eraser on a duplicator's output" into
  "the duplicator becomes a wire": that is un-copying, and it is reversible if the duplicator leaves a
  mark. Copying ahead (`dnroom`) more than doubles the copies erased; on a reversible lattice that waste
  would cost un-copying work instead of nothing.
- **Real losses are few**: agents loaded with the program and never used (untaken code; recoverable
  from the program itself), values computed and then dropped (work done ahead of need, or results a
  branch did not use), and copies whose twin was erased too. Counting those, about 25 (`add 2 3`),
  80 (`rev [1, 2]`), 95 (`relay_add`) and 112 (`fib 2`) agents' worth per run, lazily; 69–297 with
  copying ahead, which erases both copies of what it copied in vain more often.
- **The history is the big cost**: every rewrite, reversible or not, needs a mark saying it happened
  there, or running backward cannot tell a reduct from an ordinary piece of tree (an `S` made by
  `A·L` looks like any other). Reversible combinatory logic does exactly this: reduction
  ["incorporates information useful to reconstruct the redex from the contractum"](https://www.doc.ic.ac.uk/~herbert/papers/mscs06.pdf#page=2),
  each step the combinator and its position, and for K also the argument dropped, after which
  ["backward reduction, is deterministic"](https://www.doc.ic.ac.uk/~herbert/papers/mscs06.pdf#page=6).
  In reversible term rewriting, rules that drop nothing store no bindings: the substitution
  ["will record the bindings of erased variables"](https://arxiv.org/pdf/1710.02804#page=8) only. One
  token per rewrite is 2.4–8.2× the peak sites of these runs. Bennett's pebbling, which
  ["uses least additional space for the greatest number of simulated steps"](https://arxiv.org/pdf/quant-ph/9703022#page=1),
  trades time for that space; for a bijective program it can go to zero.

## Rules, sketched (`rust-ca-lattice/crate/src/rules.rs` terms)

- **A mark per rewrite.** Each rule also makes a history token at its redex, labelled with the rule; the
  inverse rule needs it to fire, and consumes it. Tokens chain, so the backward run finds them in
  order, but each stays where its rewrite happened: local, as the rewrites are.
- **Un-copy instead of erase.** An eraser meeting a duplicator's output already collapses the
  duplicator into a wire (`Lattice::collect`). On a reversible machine
  ["garbage collection for recycling storage can always be performed by a reversed (sub)computation"](https://www.plover.com/~mjd/misc/hbaker-archive/ReverseGC.html). Reversibly, the duplicator becomes a ghost (a mark that
  a copy was taken back), and copying is undone by merging, the inverse of `Dn·F`/`Dn·S`/`Dn·L`.
- **Keep originals.** An eraser meeting anything that is not a copy (a value nothing else holds,
  untaken code) wires it to a residue agent instead of taking it apart: `T1·L` (K) and `Sel·*`
  (dispatch) then drop nothing; what they drop hangs off the residue.
- **Triage needs its branch recorded.** Run backward, a three-way choice must know which way it went,
  as Janus's conditionals do with an exit assertion: ["in the backward direction an assertion acts as a test and a test as an assertion"](https://people.cs.rutgers.edu/~uli/cs516/spring2020/readings/ReversibleComputing-TheoreticalCompSci2010.pdf#page=5).
  A built-in case analysis is where reversibility is hardest: for combinatory logic with constants and
  a conditional, ["this option is not open to us if we wish to retain reversibility"](https://arxiv.org/pdf/1111.7154#page=24)
  (Abramsky, for the history-free kind).
- **Interaction nets are not reversible as they stand**: in Salikhmetov's sense
  ["An interaction system is reversible if and only if no two different active pairs clash and each rule is connected"](https://arxiv.org/pdf/1806.07275#page=4),
  and Lafont's combinators fail it; whether
  ["there exists a reversible equivalent of interaction combinators"](https://arxiv.org/pdf/1806.07275#page=6)
  is open. The history tokens above sidestep that: they make each backward step unique by record.
- **Forgetting is a separate act.** Collection (erasing residue and ghosts, cutting the history) is the
  only irreversible step, done when and where one chooses: after the answer is copied out, or to
  free space. That is where an energy cost would go.
- **Speculation becomes storage.** Work done ahead of need (forking, copying ahead) that turns out
  unneeded is residue to keep or to undo, not garbage to drop.
- **Running backward doubles as a debugger.** A lattice that can run backward needs no saved states for
  the player's timeline; the residue is itself a tree in the same net, so it can be inspected, undone
  where it was made (uncomputing finished parts as the run goes, Bennett's pebbling), or kept.

## What it would take, in order

1. **Reversible rewriting on the abstract net**: done (Step 1, above). Every rewrite is a recorded
   event; erasers un-copy as events and erase as events; nothing is kept as residue.
2. **Garbage-free bijections** (natively, for programs written two-way: Native, above): compute, copy the answer, run backward to clear the history, then the
   inverse program on the copy, run backward again to clear the input: answer only. Needs the inverse,
   written or derived (a printer–parser pair written once, research/TREE_READBACK.md).
3. **The lattice, logically reversible**: each event's record as matter on sites, wired to what it
   made. An agent's three ports are all in use, so this needs a fourth port per agent (or records
   built of several agents); records are matter that what they made drags along.
4. **The lattice, physically reversible**: moves as their own inverses on a block, energy from and to
   a demon per site rather than Metropolis acceptance, so each clock is a bijection on block states;
   collection and cooling as a separate forgetting phase. The block schedule is Margolus's, made for
   this: ["Since each distinct initial state of a block is mapped onto a distinct final state, this rule is reversible"](https://fab.cba.mit.edu/classes/862.16/notes/computation/Margolus-1984.pdf#page=6),
   and ["the global dynamics is invertible if f is invertible"](https://people.csail.mit.edu/nhm/ica.pdf#page=16).
   Creutz's demons replace the temperature: a per-site energy bank whose dynamics, on a partitioned
   lattice, ["in addition to being deterministic, this dynamics is reversible"](https://www.latticeguy.net/mypubs/pub088.pdf#page=4),
   and which ["will locally reduce to the Metropolis"](https://www.latticeguy.net/mypubs/pub070.pdf#page=2)
   algorithm.
5. **Reversible front end**: a lossless syntax tree for text ↔ AST, and the elaborator emitting its
   residue (the origin table plus choice bits) for AST ↔ application tree.

## Sources not verified

Bennett 1989 (pebbling; cited through Li and Vitányi), Vichniac's and Pomeau's Q2R papers (through
Toffoli and Margolus), Bancilhon and Spyratos 1981 (through Foster et al.), and the full text of
Axelsen and Glück (only its abstract page). No work on a reversible tree calculus was found.
