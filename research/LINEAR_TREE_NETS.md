# Linear tree nets: moving and reading code instead of copying it

Design analysis with a prototype, 2026-10-09. Its movement agents landed in rust-ic-strands as
`views=1` (5a3158d, README "Movement agents"); the rest is not in an evaluator. The
prototype is `research/linear-nets/` (experimental, a crate of its own that only reads the
evaluators' code); how to rerun every number is at the end.

Terms used throughout:
- **the net**: disp's tree-calculus interaction net, `evaluators/rust-ca-lattice/crate/src/rules.rs`
  (26 rules, plus 8 for superpositions). Values are L (leaf), S (stem), F (fork); A applies,
  T1 and Sel are the two levels of triage, Pair/Unp carry triage arms, Dn duplicates, Eps
  erases, Nrm normalizes, P is a suspended application.
- **the strand lattice**: `evaluators/rust-ic-strands`, the net run on a grid where wires are
  physical and nothing has an address.
- **the lazy machine**: the strand lattice's idealised model (`memo.rs` `ideal_depth`, its
  README's "Fork" section): only demanded rewrites fire, all of them at once each round, erasers
  collect garbage as on the lattice, and the S rule's two halves both run. Its rewrites are the
  rewrites the lattice does; its rounds are the run's parallel depth.
- **the abstract net**: `rust-ca-lattice` `Net::reduce`: every active pair fires, nothing is
  collected.

## Status (2026-10-10): what was built, and what changed since

This note's analysis stands; what was built from it, and numbers measured since:
- **Movement agents are in the strand lattice** (`views=1`, rust-ic-strands README "Movement agents"),
  on the CPU and the GPU, bit for bit, and in the player. On 13 programs: 0.55× the clocks and 0.50× the
  rewrites. The loader no longer makes K views: numbers and lists are K shapes all the way down, so K
  views mostly labelled data; without them 0.605× the clocks, 0.515× the rewrites.
- **The code table (§4b) was built on the lattice and dropped.** It reached 0.19× the clocks and 0.31× the
  rewrites, but every block unit would need its own program-sized table, so it cannot be a cellular
  automaton. §4b and §5 step 2 are kept as a record; read them as the upper bound for free sharing.
- **Corrected table numbers.** A counting bug (one-step applies not counted) had made the native table
  rules look better. With an apply reading table code and firing its rule in one step (`apply`), a triage
  reading its arms from the table (`triage`) and T1 reading a table head (`head`): 0.30×, 0.28×, 0.25× the
  rewrites (27 programs, idealised machine), all with the invariants checked every round under both
  schedules. 32–52% of the lattice's interactions were then between agents one rewrite made, against 0%.
- **Native routing without a table does not work:** an S rule peeking at whether its sides are K-marked
  (`native`) gives 0.96× (0.86× even seeing through copies in progress), as code is nearly always half
  copied when it runs, and an interaction cannot see below the node it meets.
- **Labelling only what is used** (`pp`: views only where the code's structure says they will be applied)
  gives 0.989×: most code is passed around as a value (through `fix`, currying, returned functions)
  before it is applied, in the places data goes. Plain trees with their shape as a 3-bit latent label an
  apply reads (`hints`): 0.762×, as code is still copied node by node. `views` without K (`nok`): 0.567×;
  with a latent label on K forks: 0.554×; every shape: 0.544×.
- **Collapsing B/C chains into wires** would save little: routing applies are 6.4% of rewrites with views,
  and 5% of those are chained; firing chained routing for free gives 0.540× against 0.544× (`CHAIN=1`).
- **Copies against need** (`REUSE=1`): code is used 75,990 times and copied 109,171 times over the 27
  programs, but at most 13,723 uses of a piece fall within 16 rounds of each other: 6–33% of copies per
  program are ever needed at once (unary `add 7`: 1,935 copies, 125 needed; `fib 5`: 12,370, 952). In a
  net every use consumes a copy, so this gap can only close if code is read without being consumed
 .

## Reading code without consuming it, on the lattice (2026-10-10)

The copies-against-need gap above is about time only. Closing it means a use reads code without
consuming it. Design iteration, then a measurement on the strand lattice:
- **In an interaction net it cannot be done locally.** A value is met only at its principal port, by
  one wire, and meeting it consumes it. A second reader needs a duplicator (a copy) or a shared store
  (the code table). Rebuilding a node after reading it only moves the duplicator down a level.
- **The lattice could do it as a cellular automaton**, with fan-in: persistent code nodes with a few
  extra "tap" ports, references as wires from readers to taps, a read being a trip along the wire and
  back (pulses go a strand a clock), and a copy made when a node's taps are full or its readers far.
  But a reference is one wire: a read that needs both children of a node needs a second wire laid back
  to the reader, which only walking agents can lay. And readers are far from the code.
- **Measured** (`READDIST=1 strands-run TERM --latest --fit views=1`; every read of a copy of a loaded
  value by an apply, triage or normalizer): reads are 9–64 sites from the nearest place that value was
  loaded for 54–69% of reads, within 2 sites for 1–3% (lambada fib(1), fib(2), sort(1), exp(1); disp
  `add 2 3`, `fib 2`, `rev [1, 2]`). Kept where it was last read instead (up to 4 places a value), the
  next read is within 2 sites 13–21% of the time, 3–8 sites 37–43%, 9–32 sites 22–28%, a first use 13–21%.
  Today a reader meets its value 1–4 strands away (median 2: the README's "Quiet stretches"), because
  the copy is delivered to it, and a walker closes a gap at 0.12–0.28 strands a clock.
- **So on this lattice, copying is how code gets to where it is used.** Reusing an instance would save
  the duplicators' work (25–38% of rewrites with movement agents) but cost a trip of 3–16 sites per read
  where a copy costs about 2 strands; the work saved does not pay for the travel. The copies that look
  unneeded in time are needed in space. Reuse would win where a computation stays in place (a loop
  using its function again where it is), or on a substrate with a cheap broadcast or a shared store (the
  table, 0.19× the clocks), which is not a cellular automaton. Not built.

## 1. The answer

**Where the copying comes from.** Applying a value takes it apart (A·F consumes the fork), so
anything applied twice is copied first, one layer at a time, by a duplicator. On 27 programs
(disp's `fib`, `add`, `mul`, list programs and binary adders, and the lambada benchmarks, the
hand-written tree-calculus programs in `bench/programs/`) run on the lazy machine:
- 31–37% of rewrites are duplicators, and 87–95% of those copy the **loaded program's code**
  (71% in `greet`). Copies of input data are 0.2–5% (18% in `greet`, whose input is a string);
  the other 5–11% copy values the run built. The two hand-unrolled adders, which have no `fix`,
  are the exception: duplicators are 6% of their work, mostly copying input bits.
- 19–39% of rewrites erase, about half of it code: triage arms not taken, and the half of a
  shared argument the K rule throws away.
- 83–96% of S-rule firings are **movement in disguise**: `△(△(△△f))g c` (B: the argument only
  goes right) or `△(△f)(△△g) c` (C: only left). The rule copies `c` and one copy is erased at
  once. Only 9% of all S-rule firings (4–13% per program) use the argument twice for real.

**Two redesigns**, both prototyped on the abstract net and the lazy machine. Every answer agrees
with the eager evaluator on the 27 programs, and with the oracle (the independent normalizer) on
6,000 random terms, 5,000 of them code rich in the new agents' shapes and triaged as data: 84,000
runs over 7 variants and both schedules, no disagreement.
- **Movement agents** (§4a): K, Sx (the S rule), B, C, I and W as agents of their own, made by
  the loader from shapes the compiler already emits. Applying one is one rewrite; anything that inspects one
  spells it out as its tree first, so triage and reflection see the same trees. Geometric mean
  over the 27 programs: **0.55× the rewrites and 0.52× the rounds**. No compiler change, no new
  hardware concept: 6 agent kinds, 36 rules.
- **Code in a read-only table** (§4b): the loaded program is hash-consed into a table, and
  `Ref k` (an agent holding entry number k) stands for subtree k. A duplicator copies a `Ref` in
  one rewrite and an eraser deletes it in one, whatever its size; anything else reading it gets
  entry k's node with its children as fresh `Ref`s. The program is read, never consumed.
  **0.59× the rewrites** alone; duplicators fall from 31–37% of the work to 7–14%.
- **Both: 0.28× the rewrites** (0.39× if every table read is charged as a rewrite), 0.49× the
  rounds, 0.14× the peak live agents, and 0.035× the agents at load (`fib 3`'s 723 agents become
  11 plus an 81-entry table).

On the abstract net, which has no collection, the gap is far larger (0.13×, 0.058× and 0.019×),
because there a B or C shape copies its whole argument before erasing one copy. With the S rule
run lazily (without "fork", so `b c` runs only when asked for), it is 0.41×, 0.37× and 0.19×.

**Recommendation** (§5): build the movement agents first, on the abstract net and the strand
lattice, and measure clocks; then the table, on the GPU first, where a read-only table is a
plain buffer.

## 2. Why the net is non-linear where it is

### Which rules copy, erase and move

| rule | what it does to its inputs |
|---|---|
| A·L, A·S | builds a stem or fork: linear |
| A·F | takes the fork apart: its left child goes to T1, its right child into the arms pair. The program tree is consumed |
| T1·L (K) | keeps `b`, **erases** `c` |
| T1·S (S) | **copies** `c` (a Dn), builds `(s c)(b c)` |
| T1·F, then Sel·L/S/F (triage) | dispatches on `c`; Sel **erases** the two arms not taken |
| Dn·L/S/F | copies one layer, leaving Dns on the children |
| Dn·P | forces the suspension once, then copies its value (call by need) |
| Eps·* | erases one layer |
| Unp·Pair | unpacks the arms: linear |

Only the S rule makes a duplicator (`tc-net.typ` §"The S-Rule as Sole Source of Sharing"). But
every application consumes its function, so whatever is applied more than once (a recursive
function through `fix`, a function passed to `map`, a combinator used in several places) must be
copied before each use. In an interaction net "read it and leave it" is not available: a port
has exactly one wire, so a second reader needs a duplicator, and reading a layer through a
duplicator means copying that layer. Laziness makes the copy one layer at a time and only where
read, but a layer read is still a layer copied.

### Where the work goes

The prototype tags every agent with where it came from (the loaded program, the input data, or
built by the run; a copy keeps its original's tag) and counts each rewrite against the value it
reads. It reimplements the lazy machine and gets **exactly** the strand lattice's own numbers
(rewrites and rounds) on every program (`--check-strands`). Summed over the 27 programs, the
current net does 351k rewrites:

| work | share | of which on code |
|---|---|---|
| copying (Dn·L/S/F) | 34% | 31 points |
| dispatch (A·F, T1, Sel, Unp·Pair) | 33% | |
| erasing (Eps, and collecting a duplicator or a dead computation) | 28% | ≥ 14 points |
| building (A·L, A·S) | 5% | |

The strand lattice itself, whose timing lets more duplicators fire before erasers reach them,
measured 45% copying and 30% erasing on the same disp programs (its README, "Caching what a
program throws away").

Code copies are almost never read as data: in `fib 3`, 3,296 code layers are copied, and Sel
(triage on a value) reads code 29 times. The copies are made to be run, and many are
erased: 1,597 code layers are erased in the same run.

### The S rule is mostly movement

At each S-rule firing the prototype looks, through duplicators, at what the stem and the arms
hold:

| program family | S firings | B (`s = K f`) | C (`b = K g`) | W (`s = b`) | real S |
|---|---|---|---|---|---|
| all 27, summed | 16,876 | 68% | 21% | 2% | 9% |
| per program | | | | | 4–13% |

This is the shape disp's compiler emits on purpose. Bracket abstraction (`src/elab/cir.ts`)
turns `[x](f g)` into `S [x]f [x]g` and abbreviates `[x]f` to `K f` when `x` is not in `f`. So
`{x} => succ (nat_pred x)` compiles to `△(△(△△ succ)) nat_pred`, a B shape, `{x} => add x 1` to
`△(△ add)(△△ 1)`, a C shape, and only `{x} => add x x` compiles to a real S, `△(△ add) I`
(checked with the player's compiler). The calculus has no B or C, so the net carries out a
movement as a copy plus an erasure. On the lattice the erasure is cheap when the eraser reaches
the duplicator before it fires (it becomes a wire), but the K rule's own A·F, T1·L and Unp·Pair
still run, and on the abstract net the whole argument is copied first: there 96–99% of the
duplicator work is code in 21 of the 27 programs, and `lambada sort(3)` takes 3.3M rewrites
against 31k lazily.

### What is not the problem

Work is not repeated. Tree calculus has no binders, so with call-by-need duplicators every
application runs at most once (`tc-net.typ` Theorem 6); the problems optimal reduction solves
(sharing work under binders, with brackets and labels) do not arise. What is left is
**distribution** (moving each argument to its uses, the S/K routing) and **copying values that
are only read** (code). Both are what this note attacks.

## 3. Literature

### Movement, erasure and duplication as separate combinators

- Schönfinkel's and Curry's combinators split what S and K bundle: `B x y z = x (y z)`,
  `C x y z = x z y`, `K x y = x`, `W x y = x y y`. In the usual vocabulary K ["is a cancellator with respect to its second argument"](https://plato.stanford.edu/entries/logic-combinatory/#ref-9),
  W and S ["are duplicators"](https://plato.stanford.edu/entries/logic-combinatory/#ref-9),
  and C and its kin ["are permutators"](https://plato.stanford.edu/entries/logic-combinatory/#ref-10)
  (Bimbó, Stanford Encyclopedia of Philosophy; we found no open copy of Schönfinkel's 1924 paper).
- Turner brought B and C into compilation for exactly the shapes disp's net copies and erases
  today: they ["introduce the extra combinators B and C"](https://courses.grainger.illinois.edu/cs522/sp2016/ANewImplementationTechniqueForApplicativeLanguages.pdf#page=5)
  for the ["special cases where the variable being abstracted is absent from one or more subexpressions"](https://courses.grainger.illinois.edu/cs522/sp2016/ANewImplementationTechniqueForApplicativeLanguages.pdf#page=5)
  (Turner, 1979), i.e. `S (K e₁) e₂` and `S e₁ (K e₂)`. His later S′, B′ and C′ prefix a
  term with a ["“tag” composed entirely of combinators"](https://arxiv.org/pdf/1510.03794#page=5)
  (Czajka's account), and Kiselyov's bulk combinators route whole environments so that
  ["output and the running time can both be made linear in the size"](https://okmij.org/ftp/tagless-final/ski.pdf#page=1)
  of the term (FLOPS 2018).
- Linear and affine logic make the split a discipline. Linear λ-terms (each variable used
  exactly once) are the BCI terms and affine ones (at most once, ["in which bound variables occur at most once"](https://arxiv.org/pdf/1702.03085#page=1))
  the BCK terms (Lescanne, after Hindley 1989). Linear combinatory algebras, the algebraic side
  of the Geometry of Interaction (Abramsky, Haghverdi & Scott, [MSCS 2002](https://www.cambridge.org/core/journals/mathematical-structures-in-computer-science/article/abs/geometry-of-interaction-and-linear-combinatory-algebras/983F3238099D7BCBC9FAA3BC9ABBAD51#sec0)),
  have ["combinators B, C, I, K, W, D, δ, F satisfying the following equations"](https://arxiv.org/pdf/1806.06759#page=12)
  (as stated by Ciaffaglione et al.): moving and erasing are free, and W duplicates only an
  argument marked `!` as copyable. The movement agents give the net the same split: B, C, K and
  I never copy, only Sx and W do.
- **Director strings** annotate each application node with where a variable goes: left, right,
  both or nowhere. ["B x y takes an argument and directs it just to y"](https://lsv.ens-paris-saclay.fr/Publis/PAPERS/PDF/FMS-aaecc05.pdf#page=5),
  and substitutions ["are copied only when they need to be: if there is just one occurrence of a variable"](https://lsv.ens-paris-saclay.fr/Publis/PAPERS/PDF/FMS-aaecc05.pdf#page=5)
  (Fernández, Mackie & Sinot, AAECC 2005). They began as combinators with ["abstraction rules that preserve the applicative structure of the original lambda term"](https://ueaeprints.uea.ac.uk/id/eprint/23346/#format)
  (Kennaway & Sleep, TOPLAS 1988); a textbook account is ["a string of directors which direct the flow of successive arguments into the graph"](https://www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987.pdf#page=287)
  (Peyton Jones, 1987, §16.3). Kiselyov's bulk combinators are director strings ["which tell for each application node which parameters should go left or right or both ways"](https://okmij.org/ftp/tagless-final/ski.pdf#page=15).
  The views are one-variable director strings: B is right, C is left, Sx is both, K is nowhere.
- **Closed reduction** copies or moves only closed terms: ["only closed substitutions can be moved through certain constructs"](https://www.cambridge.org/core/journals/mathematical-structures-in-computer-science/article/abs/closed-reduction-explicit-substitutions-without-alphaconversion/340CB747EE4CFF8D58F0B3E79F53EDDA#sec0)
  (Fernández, Mackie & Sinot, MSCS 2005), and it falls out of director strings when
  ["substitutions are propagated through abstractions only if they are closed"](https://lsv.ens-paris-saclay.fr/Publis/PAPERS/PDF/FMS-aaecc05.pdf#page=25).
  With explicit copy and erase, closedness is what keeps copying local, ["since only closed substitutions are copied/erased"](https://arxiv.org/pdf/1003.5515#page=11)
  (Fernández & Siafakas, 2009). Every tree is closed, so the net already meets this condition;
  what it still pays for is copying closed *code*.
- **Supercombinators** compile each function into its own rule, ["as a set of rewrite rules"](https://www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987.pdf#page=237)
  whose reduction ["only takes place when all the arguments are present"](https://www.microsoft.com/en-us/research/wp-content/uploads/1987/01/slpj-book-1987.pdf#page=237)
  (Peyton Jones, ch. 13). The table of §4b is the fine-grained form: one entry per node, unfolded
  one node per read.

### Programs as fixed graphs: the Geometry of Interaction and token machines

- In the Geometry of Interaction (GoI) a term is a fixed automaton that a token walks: the
  machine runs by ["entering the automaton with a word at some given entry node"](https://www.pure.ed.ac.uk/ws/files/16874549/Reversible_irreversible_and_optimal_machines.pdf#page=4),
  and the word is all the state it needs (Danos & Regnier, TCS 1999). The program is never
  consumed and needs no garbage collector.
- In hardware, Geometry of Synthesis turns each subterm into circuitry; using a function twice
  is not a copy but ["a stateful multiplexer-demultiplexer"](https://www.microsoft.com/en-us/research/wp-content/uploads/2011/09/Compiling-Affine-Recursion-into-Static-Hardware.pdf#page=6)
  in front of one shared circuit, and recursion folds every instance into one, ["indexed by the counter"](https://www.microsoft.com/en-us/research/wp-content/uploads/2011/09/Compiling-Affine-Recursion-into-Static-Hardware.pdf#page=8)
  of a hardware stack (Ghica, Smith & Singh, ICFP 2011). The first version could ["allow only for limited forms of recursion"](https://www.irif.fr/~mellies/mpri/mpri-ens/articles/ghica-geometry-of-synthesis.pdf#page=10)
  (Ghica, POPL 2007).
- The dynamic GoI machine interleaves the token with rewriting: a pure token machine pays for
  ["repeating the same moves of a token on the fixed graph"](https://arxiv.org/pdf/1703.10027#page=1),
  so the token rewrites the graph where that avoids recomputation (Muroya & Ghica, CSL 2017),
  and copies a shared value only when the token reaches it, raising a flag ["that triggers on-demand duplication"](https://arxiv.org/pdf/1802.06495#page=5)
  (WPTE 2017).
- The price of a pure token machine is time: the interaction abstract machine ["can be exponentially slower than environment machines"](https://arxiv.org/pdf/2010.12988#page=2)
  (Accattoli, Dal Lago & Vanoni, POPL 2021), while it can run in linear space ([LICS 2021](https://arxiv.org/pdf/2104.13795#page=12)).

What this gives the net: code need not be consumed to be run. What it costs on the strand
lattice: a fixed graph means fixed wires, so every reader's token travels to the one copy, and
time grows with distance and with re-walking. An environment machine keeps code fixed and passes
*pointers* to it; the table of §4b is that, with entry numbers as the pointers.

### Sharing graphs and lazy duplication

- Optimal reduction ["performs the minimum amount of duplication required by the computation"](https://arxiv.org/pdf/1701.04240#page=5),
  but needs ["suitable operators acting as brackets in the graph to delimit the scope of duplicators"](https://arxiv.org/pdf/1701.04240#page=5),
  whose bookkeeping is its own cost (Asperti, 2017; Mairson's summary: ["other supportive ``bookkeeping'' operations grow exponentially"](https://archive.dimacs.rutgers.edu/Workshops/Programming/Report/hmairson.html#summary)).
  Tree calculus needs none of it (`tc-net.typ` §"No Binding-Scope Bookkeeping").
- HVM copies a value ["layer by layer, on-demand"](https://github.com/HigherOrderCO/HVM1/blob/master/guide/HOW.md#hvms-solution-make-clones-cheap)
  instead of whole, as the net's duplicator does, and adds superpositions, which the net already
  has ([Interaction Calculus](https://github.com/HigherOrderCO/HVM3/blob/main/IC.md#the-interaction-calculus)).
- HVM2's **REF** nodes are the closest precedent for the table. A REF points to a closed
  definition in a static book; ["they represent an immutable net that is expanded in a single interaction"](https://raw.githubusercontent.com/HigherOrderCO/HVM2/main/paper/HVM2.pdf#page=5),
  and a duplicator meeting one ["will just copy the REF port, rather than expanding the"](https://raw.githubusercontent.com/HigherOrderCO/HVM2/main/paper/HVM2.pdf#page=16)
  definition when that is safe, that is, unless ["a REF which contains DUP nodes"](https://raw.githubusercontent.com/HigherOrderCO/HVM2/main/paper/HVM2.pdf#page=7)
  must be expanded first (Taelin). Two differences matter here: a REF expands a whole definition
  in one interaction, an unbounded rewrite a lattice cannot seat, where `Ref k` unfolds one node
  per read; and tree-calculus code holds no duplicators, so every entry is always safe to copy.

### Interaction nets with more than one reader

- Lafont fixed the model: each cell ["has one principal port and n auxiliary ports"](https://dcreager.net/pdf/Lafont1997.pdf#page=3),
  and asked ["is it reasonable to consider erasing and duplication as atomic operations"](https://dcreager.net/pdf/Lafont1997.pdf#page=2)
  (Interaction Combinators, 1997).
- Multiport nets allow ["cells to have more than one principal port"](https://lipn.univ-paris13.fr/~mazza/papers/mINSAndConcurrency-CONCUR05.pdf#page=2),
  so one cell can serve many requests, but such a cell ["can in general be involved in several active pairs"](https://lipn.univ-paris13.fr/~mazza/papers/mINSAndConcurrency-CONCUR05.pdf#page=4),
  which gives up determinism and strong confluence (Mazza, CONCUR 2005, after Alexiev's thesis).
- Mackie's group names the problem directly: in interaction nets ["interacting agents are consumed"](https://www.user.tu-berlin.de/o.runge/tfs/workshops/gtvmt08/Program/paper_38.pdf#page=13),
  and their stated goal was ["to extend interaction nets with data sharing mechanisms"](https://www.user.tu-berlin.de/o.runge/tfs/workshops/gtvmt08/Program/paper_38.pdf#page=13)
  (Hassan, Mackie & Sato, 2008). We found no later work with persistent or read-only agents.
- Rules that look two levels deep are sugar: ["Reduction rules in interaction nets are constrained to pattern match exactly one argument at a time"](https://arxiv.org/pdf/1003.4562#page=1),
  and nested patterns compile to auxiliary agents (Hassan, Jiresch & Sato, 2009). The net's own
  T1/Sel is such a translation. Spotting B or C at run time would add a third level to every S
  rule; spotting them at load time costs nothing.
- Token-passing nets move one token to choose which redex fires (Sinot; see [Almeida, Pinto & Vilaça](https://webarchive.di.uminho.pt/haslab.uminho.pt/jba/files/07tnfp.pdf#page=1)):
  a strategy, not sharing. The lattice's demand pulses are already this.

### Tree calculus

- Jay's star abstraction, which disp's `src/elab/cir.ts` follows (plus K-composition), has four
  cases: `K t` when x is absent, η, `I`, and otherwise `d{λ*x.u}(λ*x.t)`, the S shape. There is
  no B or C case: it exists to ["optimise the bracket abstraction to exploit variable occurrence"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=55),
  against a size blowup that is ["especially problematic when abstractions are nested"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=54),
  but when x is on one side only it still sends x both ways (Jay, *Reflective Programs in Tree
  Calculus*, 2021). Bader's specification has one rule that distributes an argument, the S-like
  `x z (y z)`, and it always duplicates ([treecalcul.us](https://treecalcul.us/specification/#main-centered)).
- Jay and Bader keep plain S/K abstraction on purpose in their typed work: ["First, optimisations make proofs harder to follow"](https://arxiv.org/pdf/2604.12194#page=4)
  (arXiv 2604.12194). The views respect that: the calculus and the compiled trees do not change,
  only how the net holds them.
- In SF-calculus, reading structure is linear: factorisation gives ["N P Q if P Q is a compound"](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=7),
  each part once (Jay & Given-Wilson, JSL 2011). Triage is the same. The copying comes from the
  net's one-reader ports when code is *run* twice, not from inspection.
- We found no linear, affine or substructural tree, triage or SF calculus, no compilation of tree
  calculus to B and C, and no tree calculus on interaction nets besides disp's own `tc-net.typ`
  (searched the web and 25 tree-calculus repositories on GitHub).

## 4. Redesign options

### (a) Movement agents

Six new producers, each a *view* of a fixed tree shape, each at most 3 ports (`Sx` is the S
rule's fork, named apart from the stem S):

| agent | stands for | applied to `c` (A·agent, one rewrite) | fresh agents |
|---|---|---|---|
| `K y` | `△△y` | `y`; ε on `c` | Eps |
| `Sx s b` | `△(△s)b` | `(s c)(b c)`, δ on `c` | Dn, A, A, A |
| `B f g` | `△(△(△△f))g` | `f (g c)` | A, A |
| `C f g` | `△(△f)(△△g)` | `(f c) g` | A, A |
| `I` | `△(△△△)△` | `c` (a wire) | none |
| `W s` | `△(△s)s` | `t t` where `t = s c` runs once, δ on `t` | A, Dn, A |

The other rules:
- **δ·view** copies the agent in one rewrite (as Dn·F copies a fork), **ε·view** erases it in
  one. A view stands for 2–5 tree nodes, so code loads as about half the agents and copying it
  costs about half the rewrites.
- **T1, Sel or Nrm reading a view** spells it out as its tree and reads that (one extra rewrite,
  fresh: the reader again and the tree's agents). This only happens when code is inspected as
  data, so reflection is unchanged and costs one rewrite more.

36 rules (6 apply, 6 copy, 6 erase, 18 spell-out), all one per pair of kinds, checked
mechanically as the 26 are: each template's wires are a perfect matching of the live ports, and
each wire joins a port that supplies a value to one that needs it (`net.rs` `validate`).

**Where views come from.** The loader rewrites the program's shapes into views; the compiler is
unchanged. Bracket abstraction already emits exactly `K x`, `S (K p) q` (B), `S p (K q)` (C) and
`I`. Views are made only in loaded code: data such as numbers (forks over a leaf, so `K` shapes)
are triaged constantly and should stay trees. Values built during the run stay trees too.

**Effect** (lazy machine, geometric mean over 27 programs; per program in §6):

| variant | rewrites | rounds | agents at load | peak agents |
|---|---|---|---|---|
| K, Sx and I views only | 0.74× | 0.64× | 0.63× | 0.71× |
| + B and C (movement) | 0.55× | 0.52× | 0.52× | 0.58× |
| + W | 0.54× | 0.52× | 0.50× | 0.57× |

The K, Sx and I shortcuts alone give 0.74×: one rewrite instead of A·F, T1 and Unp·Pair, and one
agent to copy instead of up to five. Movement proper (B and C, which skip the copy of `c`, its
collection and the K rule's own three rewrites) gives another 0.75× on top. Rounds halve
because each application is one step deep instead of three. On the abstract net the views alone
give 0.13×.

**Lattice cost.** 6 agent kinds (14 → 20), 36 rules (26 → 62: the 5-bit rule field of the
cascade, rust-ca-lattice's cellular engine, needs 6 bits, 7 with superpositions; the copy rules
carry labels as Dn·F's do). At most 6 fresh agents a rewrite (spelling out `I`), as now.
Everything stays local: no payload, no address.

**Risks.** Each view rule equals the tree rules it replaces, composed, minus the copy and erasure
of `c` (the differentials agree on every case). Two differences: `I` passes its argument on
unforced where `△(△△△)△` triages it first, so it is lazier (same answers); and `W` runs `s c`
once instead of twice, which is sharing, not a change of result. The strand lattice checks its
run against the abstract net wire by wire, so both need the new kinds. Readback must spell views
out.

### (b) Reading without consuming: code in a read-only table

The Geometry of Interaction's lesson (§3) is that the program can be a fixed graph that is never
consumed. On the strand lattice a fixed graph means fixed wires, and wires to one place are long
(option c). The table does the same with names instead of wires:

| rule | result |
|---|---|
| δ · `Ref k` | `Ref k`, `Ref k` (one rewrite, any size) |
| ε · `Ref k` | nothing |
| A, T1, Sel or Nrm · `Ref k` | entry k is `(kind, k₁, k₂)`: the reader meets a fresh node of that kind whose children are `Ref k₁`, `Ref k₂`, in the same rewrite |

The table is the loaded program hash-consed: equal subtrees are one entry, as in the eager
evaluator, and entries can be views. Its size is small, because compiled code reuses a few
combinators: 80–294 entries as plain trees, **49–194 with views** (fib's 713 code nodes are 132
entries). An entry number fits in 8–9 bits.

**One wire, one reader.** Nothing in interaction-net theory changes. Every `Ref` is an ordinary
linear agent with one principal port read once; a copy is cheap because it holds a name, not a
structure. Formally `Ref k` is a separate agent kind for each entry, with its rules generated
from the entry: the program becomes part of the rule table, as interaction-net languages compile
each function into its own rules, and as HVM's references to global definitions work (§3). A
*persistent* agent whose principal port could be read by many wires is not an interaction net any
more (multiport nets lose determinism, §3) and, on a lattice, many wires to one agent are a bus
or a tree of duplicators, which is what the net already has.

**No addresses.** An entry number names a piece of code, not a place. The chip's block units
already hold the rule tables (`evaluators/rust-ic-strands/hw/README.md`); the code table is a
program-specific extension of them, about half a kilobyte (194 entries × ~22 bits). It could be
replicated per tile, broadcast once at load, or, on the GPU, be a read-only buffer. Only
rewrites that read a `Ref` look at it, and it never changes, so there is no coherence problem.

**Effect** (lazy machine, geometric means over 27 programs):

| variant | rewrites | rewrites + table reads | rounds | agents made | peak agents | agents at load |
|---|---|---|---|---|---|---|
| table, code only | 0.59× | 0.82× | 0.89× | 0.74× | 0.17× | 0.035× |
| table, code and inputs | 0.57× | 0.81× | 0.89× | 0.73× | 0.17× | 0.008× |
| views + W + table | **0.28×** | **0.39×** | **0.49×** | 0.35× | 0.14× | 0.035× |

Duplicators fall from 31–37% of the work to 4–15% (mostly 6–8%), and the code they still copy
is the top few layers of closures built at run time. Inputs in the table add little: data was
never most of the copying. On the abstract net the table alone gives 0.058×, with views 0.019×.

The second column charges each table read as a rewrite. On the lattice a read is a lookup inside
the rewrite that makes up to two more agents (the children's `Ref`s), so a rewrite seats at most
8 fresh agents (Sel reading a tabled fork) instead of 6; for code it is at most 7.

`relay_add`, which recurses through `fix`, was 3× the work of the hand-unrolled adder
(`relay_for`): its program is copied at every call. With views and the table it takes 4.9k
rewrites at 16 bits, less than the unrolled adder's 5.3k today, and loads as 84 agents against
the unrolled adder's 2,913. The table gets the unrolling's gain without the unrolled code.

**Lattice cost.** One kind (`Ref`) with an 8–9 bit payload, and a table read in the rewrite
stage. Readback and the projection check must expand `Ref`s through the table. Runtime-built
values stay agents: only loaded values are tabled, since writing to the table at run time would
need a global, shared, writable structure.

**Risks.** Laziness is unchanged (a `Ref` is a value; copying and erasing it force nothing).
Answers agree with the oracle on every case, including triage on tabled code. The design gives up
"the lattice holds the whole program", which the player's views draw; the program would be drawn
from the table instead.

### (c) Code stored once in one place, readers walking to it

This is the GoI token machine and the Geometry of Synthesis multiplexer (§3), and it was measured
on the strand lattice already: building every repeated part once and reading it through
duplicators (`share=8`, its README "Sharing equal parts of the program") loaded 23–61% fewer
agents but took 16–81% more clocks and up to 3.3× the peak sites. Readers are spread over the
drawing and the shared part is in one place, so the copies (or tokens) travel along long wires.
Token machines also cost time to save space (§3). The table is this option made local: the code
is stored once *per table*, and every site reads it where it is.

### (d) Director strings: routing many variables at once

Views move one argument per node. A curried function of several arguments routes each through
its own chain of B, C and S agents; director strings annotate each node with where *every*
variable goes, so one pass routes all (§3). With views and the table, the routing rewrites (A·K,
A·B, A·C, A·I, A·Sx, A·W) are 16% of the remaining work, so merging chains can save at most part
of that, perhaps 10%. It needs a variable-length annotation per node, which a 3-port agent of
fixed width cannot hold without nesting. Not worth it before (a) and (b).

### (e) Linear types to skip duplicators

`tc-net.typ` suggested typing values as linear to drop δ. Data copies are 0.2–5% of the copying
in programs with `fix` (18% in `greet`); the rest is code, and a recursive function's code is
used more than once by definition. A type system cannot remove that; the table does.

### (f) Sharing equal computations by name

With entry numbers, `S s s` (W) is spotted at load and runs `s c` once: 0–2% fewer rewrites on
disp programs, 6–7% on lambada fib and exp, whose memo twins mostly come from this. The strand
README's "cheaper piece" (an S rule comparing names) is this, done at load time for free. Twins
built at run time still need the memo machinery the README tried.

## 5. Recommendation and first experiment

Do (a), then (b).

1. **Movement agents on the real evaluators.** Add the six views to `rules.rs` as a separate
   table (as `SUP_RULES` is kept apart, so the cascade keeps its 26), recognize the shapes when
   the strand lattice loads a program (`share.rs` `net`), and run `programs/bench.ts` on the disp
   programs and the adders for **clocks**, the number this note cannot give. Expected from the
   lazy machine: about half the rewrites and half the depth; clocks should fall less than
   rewrites (in its README, compiling `ripple`'s recursion away cut rewrites 4.1× and clocks
   1.9×). Gates: the oracle soak, the projection check, the superposition tests with δ_ℓ·view
   rules added. This needs no payload, no table and no compiler change.
2. **The table on the GPU.** `Ref` with its entry number in the superposition label field (8
   bits, enough for every table with views), the table as a read-only buffer next to the rule
   tables. Measure clocks and peak sites; the prediction is a lattice a fraction of today's size
   (agents at load 0.035×, peak 0.14×), since the program no longer needs to be laid out.
3. Only then decide how a chip holds the table (per tile or broadcast), with the measured gain in
   hand.

## 6. Reproducing, and the numbers per program

```sh
# compile the calls (as the player does, inputs worked out first)
npx tsx research/linear-nets/emit.ts --let 'ru8 := {x, y} => relay_for (bit_tree 3 0) x y ({c} => c) b0' 'fib 3' ... > research/linear-nets/calls.txt
cd research/linear-nets && cargo build --release
./target/release/linear-nets --calls calls.txt --lambada --modes base,views,views+w,rom-code,views+w+rom-code [--sched all] [--rules] [--check-strands]
./target/release/linear-nets --shapes 5000 --corpus 1000 --modes base,views,views+w,views+nobc,rom-code,rom-all,views+w+rom-all [--sched all]   # differentials
```

`calls.txt` is committed (21 disp calls). Modes: `views`, `+nobc` (no B or C), `+w`, `rom-code`
(the table holds the program), `rom-all` (inputs too). Every run is memory-capped and timed out
as `AGENTS.md` asks; the whole suite takes under a minute.

Per program, on the lazy machine (rounds = parallel depth; the table rows count the input data
as agents, since only the program is tabled):

| call | base: rewrites, rounds, Dn % | views + W | table | views + W + table: rewrites (+ table reads), rounds | agents at load: base → table |
|---|---|---|---|---|---|
| `fib 3` | 10,185, 756, 34% | 5,039 | 5,286 | 2,350 (+1,010), 348 | 723 → 11 + 81 entries |
| `fib 4` | 20,510, 1125, 36% | 10,134 | 10,541 | 4,688 (+2,017), 516 | 725 → 13 + 81 entries |
| `fib 5` | 36,457, 1498, 36% | 18,021 | 18,857 | 8,404 (+3,609), 687 | 727 → 15 + 81 entries |
| `add 2 3` | 2,389, 415, 32% | 1,213 | 1,662 | 750 (+313), 193 | 306 → 17 + 57 entries |
| `add 7 1` | 6,702, 1138, 33% | 3,374 | 4,678 | 2,126 (+888), 521 | 312 → 23 + 57 entries |
| `mul 2 2` | 8,530, 691, 34% | 4,201 | 5,011 | 2,227 (+950), 314 | 613 → 15 + 75 entries |
| `is_even 3` | 2,618, 510, 31% | 1,268 | 1,755 | 761 (+323), 228 | 252 → 11 + 49 entries |
| `sum [1, 2]` | 7,306, 693, 34% | 3,707 | 4,165 | 1,854 (+795), 312 | 599 → 15 + 79 entries |
| `rev [1, 2]` | 5,803, 608, 32% | 2,939 | 3,406 | 1,600 (+624), 298 | 706 → 15 + 81 entries |
| `doubled [1, 2]` | 7,829, 739, 32% | 3,960 | 4,767 | 2,278 (+875), 359 | 759 → 15 + 99 entries |
| `isort [2, 1]` | 10,646, 1067, 35% | 5,289 | 5,853 | 2,568 (+1,087), 493 | 1421 → 15 + 145 entries |
| `size [5, 6, 7]` | 3,105, 552, 32% | 1,666 | 2,157 | 978 (+397), 241 | 345 → 47 + 59 entries |
| `greet "a"` | 4,534, 668, 26% | 3,112 | 3,437 | 2,510 (+676), 398 | 1064 → 201 + 173 entries |
| ripple_add, 8-bit tree, 255 + 1 | 22,561, 648, 35% | 11,168 | 12,411 | 5,957 (+2,351), 306 | 712 → 44 + 109 entries |
| relay_add, 8-bit tree, 255 + 1 | 7,789, 258, 34% | 5,008 | 4,784 | 2,343 (+900), 127 | 346 → 44 + 69 entries |
| relay unrolled (`relay_for`), 8 bits, 255 + 1 | 2,595, 127, 6% | 1,349 | 2,035 | 1,094 (+394), 85 | 1433 → 44 + 64 entries |
| lookahead_add, 8-bit tree, 255 + 1 | 25,466, 625, 34% | 13,091 | 13,814 | 6,977 (+2,786), 292 | 949 → 44 + 137 entries |
| ripple_add, 16-bit tree, 65535 + 1 | 47,355, 842, 35% | 23,569 | 26,076 | 12,540 (+4,943), 385 | 752 → 84 + 109 entries |
| relay_add, 16-bit tree, 65535 + 1 | 15,986, 327, 34% | 10,300 | 9,856 | 4,868 (+1,860), 168 | 386 → 84 + 69 entries |
| relay unrolled, 16 bits, 65535 + 1 | 5,296, 158, 6% | 2,769 | 4,170 | 2,258 (+810), 111 | 2913 → 84 + 72 entries |
| relay_add, 16-bit tree, 51566 + 22995 | 17,005, 326, 35% | 10,814 | 10,043 | 4,786 (+1,886), 153 | 387 → 85 + 69 entries |
| lambada fib(2) | 5,473, 300, 32% | 3,407 | 2,729 | 1,247 (+560), 148 | 724 → 10 + 78 entries |
| lambada fib(3) | 9,033, 406, 35% | 5,473 | 4,350 | 1,988 (+878), 206 | 725 → 11 + 78 entries |
| lambada fib(4) | 17,590, 570, 36% | 10,460 | 8,501 | 3,867 (+1,693), 293 | 726 → 12 + 78 entries |
| lambada exp(1) | 4,599, 375, 29% | 2,706 | 2,324 | 1,110 (+476), 194 | 902 → 8 + 131 entries |
| lambada sort(2) | 12,607, 587, 36% | 7,562 | 6,572 | 3,096 (+1,327), 309 | 1840 → 17 + 194 entries |
| lambada sort(3) | 31,330, 1462, 37% | 18,157 | 16,353 | 7,646 (+3,225), 744 | 1848 → 25 + 194 entries |
