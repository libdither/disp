# A reversible core: footprints and a reader

**Status (2026-10-15): archived.** No design from this review was adopted; a later one was: keeping history (research/reversible-core/residual.py, run on the lattice as `resid=1`, rust-ic-strands README "Keeping history").

**Status at the time (2026-10-14): no design adopted.** The review did not find a core that is both
simple and history-free; it is kept for its map of the literature (every quote fetch-verified), its
measurements of the earlier designs, and the lesson that copies buy private histories. The models and
the review's raw outputs are in research/reversible-core/.

This note asks how disp could run as reversible physics, with history as something programs spend rather than a log. It was written on 2026-10-13. Scouts read the repo: the lattice, rev.rs, the substrate, and the two-way calculus whose machines were deleted in 2a32736 (kept in eca0073). A literature sweep covered sixteen fields. Five designs were proposed and scored by five judges (simplicity, locality, physics, expressiveness, rigour), and critics then attacked the two leaders.

Numbers marked *(model)* come from scratch Python models of the abstract net. These have no lattice and use their own round scheduler, so only their ratios carry over. All other numbers come from the repo's READMEs and research notes, or from the linked sources.

## 1. The question and the answer in brief

**The question.** The repo runs the tree-calculus net on the strand lattice and runs it back using the substrate's records and bonds. Is there a simpler core that is local, fully parallel, linear, plausible as reversible physics, and runs disp? It should also make history something programs use, not a log.

- **Recommended core: footprints and a reader.**
  - Keep the tree-calculus net and change one thing. A rewrite no longer destroys the two agents it uses. It fuses them into one *footprint*, and every agent the rule makes has a back port wired to that footprint.
  - Going back is then the same kind of event as going forward. A footprint takes back a product nothing has used yet, which is two agents meeting along one wire. Once all its products are back, it splits into the pair it was. Nothing carries a number.
  - Add a *reader*: five rules that copy a value without consuming it and leave no footprints. Bennett's cleanup (run, copy the answer, run back) then happens inside the net and in parallel.
  - On all 21 compiled disp calls tried, this ends with zero footprints, a correct copy of the answer, and the loaded program and input back agent for agent *(model)*.
  - Erasing a footprint (*commit*) is the only step that loses information.
  - Given an inverse program, the same moves also erase the input, so (a, b) → (a, a+b) leaves nothing behind.
- **Why it is simpler.**
  - Compared with the substrate (`phys=1`, the lattice run as physics), it drops all of this: the record table keyed by numbers, agent ids that undo must restore, per-port mark stacks, the table of where agents sit, bonds, gathering pieces toward each other, the cascading forget, and the special record kinds (787 lines of substrate.rs). History is the used-up pairs themselves, sitting where the work happened.
  - Compared with the two-way calculus (iso.rs), it drops the 11 instruction kinds and the walker. Programs need not be written two-way: any compiled disp runs and runs back.
  - It is not smaller than the plain net, which it contains. It has 29 rules (as many as today's 26 plus the lattice's three collection moves), two pairs of laws, and five reader rules.
- **What the earlier "moving things around" design got right.** That design was iso.rs: rearrange trees by pattern, copy with an inverse, and keep one lossy Forget.
  - It lost nothing except at Forget, and read every inverse off the program text.
  - It made copying a step whose inverse takes back only two equal halves, which is fuel and refund.
  - Its persistent walker (walk.rs) ran with no log at all: any interleaving of token moves undid cleanly.
  - It showed that history is mostly redundant. `tc add 1 1` (tc is the tree calculus written as a two-way program that returns a trace) left 10,828 trace nodes but only 427 distinct subtrees. 87% of them were arguments K dropped, and 102 of the 116 dropped pieces were copies of the program's own code.
  - All of this survives here: un-reading is un-copy from two equal halves, running back is the inverse, and iso.rs's `bennett` and `bijection` become ways of running a program.
- **How parallel it really was: barely.**
  - Two-way programs ran as one token (`add 2 3`: 29 moves).
  - On disp's own trees through tck (tc that keeps the applied program), the walker took 3,939 rounds with at most 4 tokens for `add 1 1`. The net takes 418 rounds with up to 12 rewrites at once.
  - For a 16-bit `relay_add` (programs.disp's binary adder) it took 32,835 rounds with 9 tokens, against the net's 685 rounds with a widest round of 144.
  - That is 9.4–51× the net's rounds at 1.4–1.8 moves a round, and the gap doubles with each doubling of the bits. Splitting tokens at `both` gained only 1.4–1.8× over a single token.
  - The reasons are structural. In a net, reading consumes, so a second reader of code needs its own copy. And circuits built from one-token-in, one-token-out elements can never act in parallel.
- **The review's main lesson: copies buy private histories.** The design the judges ranked first, Riders (code read in place by pointer agents, never copied), failed on exactly this point. When every call reads the same nodes, every call shares those nodes' history. Undoing without ids then becomes last-in-first-out across unrelated calls. So the core keeps copying code as today and makes copies cheap to give back.
- **Not yet shown:**
  - that footprints fit the lattice (up to 10 wire ends, against 8 pairings a site);
  - that demand (the laziness signal) can become reversible matter;
  - that a deterministic 2×2×2 block version exists.

  Section 6 puts the experiments in order.

## 2. What the literature says

Each entry gives the claim, its source, and what it means for disp.

### Block lattices that run backward

- **Block permutations make the whole lattice reversible.** ["Since each distinct initial state of a block is mapped onto a distinct final state, this rule is reversible"](https://fab.cba.mit.edu/classes/862.16/notes/computation/Margolus-1984.pdf#page=6); ["the global dynamics is invertible if f is invertible"](https://people.csail.mit.edu/nhm/ica.pdf#page=16). A table can be checked entry by entry, whereas invertibility of an arbitrary 2D local map is undecidable. For disp: write every lattice move as a permutation of 2×2×2 block states.
- **Every reversible cellular automaton has a block form, at a price.** ["We built a simulation in linear time of reversible cellular automata by reversible block cellular automata"](https://dmtcs.episciences.org/articles/2297/download#page=1). The construction needs blocks sized to the rule's reach, and cells that hold the old and new state at once. A fixed 2×2×2 table over today's agents is therefore a real restriction.
- **Block permutations read directly as quantum dynamics.** ["any quantum cellular automaton is structurally reversible, i.e., that it can be obtained by applying two blockwise unitary operations"](https://arxiv.org/pdf/quant-ph/0405174#page=1).
- **Energy can be booked locally and still reversibly**, with a small counter per site (a Creutz demon). ["in addition to being deterministic, this dynamics is reversible"](https://www.latticeguy.net/mypubs/pub088.pdf#page=4), and it ["will locally reduce to the Metropolis"](https://www.latticeguy.net/mypubs/pub070.pdf#page=2) acceptance the lattice uses now. Also: ["This heatbath CA is perfectly invertible, since applying the dynamics twice to the same sublattice leaves both the spins and the heatbath unchanged"](https://arxiv.org/pdf/comp-gas/9811002#page=7).
- **Rule tables carry invariants nobody asked for.** In the HPP gas, ["the four-bit states (1, 0, 1, 0) and (0, 1, 0, 1) are exchanged; all other states are left unchanged"](https://content.wolfram.com/sites/13/2018/02/01-4-7.pdf#page=5), which conserves momentum along every row. Check what a rule table conserves besides what was meant.
- **No fixed infrastructure.** A physically universal automaton can ["implement any transformation on a finite region of the CA by initializing the complement of the region"](https://eccc.weizmann.ac.il/report/2014/084/download/#page=1). The billiard-ball automaton cannot, because its mirrors are immovable walls. Wires and code should be ordinary matter.
- **A collision can be a short-lived particle.** ["Two particles colliding at right angles turn into a single new particle of twice the mass for one step"](https://arxiv.org/pdf/0806.0127#page=6). This is the shape of a footprint: the pair fuses, then decays into its products.

### What copying and erasing cost

- **A step loses information exactly** ["if the output of a device does not uniquely define the inputs"](https://sites.pitt.edu/~jdnorton/lectures/Rotman_Summer_School_2013/thermo_computing_docs/Landauer_1961.pdf#page=4), and costs heat ["of the order of kT for each irreversible function"](https://sites.pitt.edu/~jdnorton/lectures/Rotman_Summer_School_2013/thermo_computing_docs/Landauer_1961.pdf#page=1).
- **Copying and un-copying are free.** Bennett names two reversible operations: ["copying unknown data onto a blank (ie initially zero) register, and the reverse process"](https://arxiv.org/pdf/physics/0210005#page=2), which is erasing one of two copies known to be equal. Copies are fuel: of seven identical random tapes, ["Six of these could be converted into blank tapes by a logically and thermodynamically reversible process"](https://sites.pitt.edu/~jdnorton/lectures/Rotman_Summer_School_2013/thermo_computing_docs/Bennett_1982.pdf#page=31). Also: ["When the gases are identical they still mix, but there is no entropy increase"](https://bayes.wustl.edu/etj/articles/gibbs.paradox.pdf#page=4). For disp: copying draws empty seats, and giving a copy back refunds them with no heat.
- **The refund needs the twins still matched.** ["Once mixed, however, templates cannot be matched to copies a priori, and correlations cannot be leveraged"](https://arxiv.org/pdf/1609.05554#page=3). Something must remember which copy is whose twin until the copy goes back.
- **Erasing locally costs more than the global minimum.**
  - ["the minimum irretrievable modularity dissipation over the duration of an operation due to the locality of control is proportional to the reduction in mutual information"](https://arxiv.org/pdf/1708.03030#page=2).
  - A local, periodic device escapes this cost only in one case: ["One is if all islands have a single element, i.e., the local update function ρ is invertible"](https://arxiv.org/pdf/2208.06895#page=8).
  - A fixed rule table pays its mismatch over and over: ["each iteration will contribute a strictly positive mismatch cost in general, without cancellation of terms"](https://arxiv.org/pdf/1905.05669#page=46).
  - For circuits: ["we should choose the wiring diagram for which the parents of each gate are as strongly correlated among themselves as possible"](https://arxiv.org/pdf/1806.04103#page=14).
  - For disp: erasing a copy away from its twin costs real heat. Give it back at the twin, or run back.
- **Entropy is relative to what the system knows.** Only the states that can occur must map one-to-one: ["only the subset of initial states that are assigned nonzero probability in a given statistical operating context must be transformed one-to-one into final states"](https://arxiv.org/pdf/1806.10183#page=1). The field puts the question as ["does it depend, for example, on what we know about the system?"](https://www.nature.com/articles/nphys3230#Abs1-content), and in the slow limit ["any logically irreversible computation can be performed in a thermodynamically reversible manner in the quasi-static limit"](https://arxiv.org/pdf/1311.1886#page=1).
- **Clean computing needs room, not tokens.** ["the essential function of this register is to provide the computation with spare room rather than tokens"](https://www.cs.princeton.edu/courses/archive/fall04/cos576/papers/fredkin_toffoli82.pdf#page=21). Fuel is empty space.
- **The quantum limit.** ["Classically we may delete one copy against the others, uncopying it in a perfectly reversible manner"](https://arxiv.org/pdf/quant-ph/9911090#page=1); quantum states cannot be deleted this way. Copying and un-copying exist only in a fixed basis.

### Brownian computers and token circuits

- **A Brownian computer random-walks along its computation under a weak drive.** ["the energy dissipated approaches zero only in the limit of zero speed"](https://fab.cba.mit.edu/classes/863.18/notes/computation/Bennett-1982.pdf#page=1). RNA polymerase makes ["11 forward steps for each 10 backward steps"](https://fab.cba.mit.edu/classes/863.18/notes/computation/Bennett-1982.pdf#page=10).
- **Merges are what slow it.** They ["would cause the Brownian computer to spend most of its time bogged down in extraneous predecessors of states on the intended path"](https://fab.cba.mit.edu/classes/863.18/notes/computation/Bennett-1982.pdf#page=2), unless each step pays ["about k T times the logarithm of the mean number of immediate predecessors"](https://fab.cba.mit.edu/classes/863.18/notes/computation/Bennett-1982.pdf#page=19). For disp: a core where nothing merges except an explicit erase needs only a small drive.
- **Polynomial time still needs some bias.** ["achieving efficient polynomial-time computation generally requires a finite forward bias corresponding to a nonzero energy input"](https://arxiv.org/pdf/2512.24728#page=1).
- **In token circuits, the join is where entropy is made.** ["The CJoin is the only circuit element producing environment entropy"](https://arxiv.org/pdf/2205.10735#page=9). Noise does an arbiter's job: ["they use fluctuations to make tokens backtrack their way out of deadlocks"](http://www.oldcitypublishing.com/wp-content/uploads/2016/11/IJUCv12n5-6p341-362Lee.pdf#page=4). For disp: bias only the meetings and the readout, and let everything else diffuse.
- **One token walking a circuit is serial by construction.** ["No set of serial modules can be universal for the set of all modules"](https://web.archive.org/web/20240502171700id_/https://scholarship.claremont.edu/cgi/viewcontent.cgi?article=1288&context=hmc_fac_pub#page=8). Morita's rotary element turns a particle that crosses its bar: ["it makes a right turn, and rotates the bar by 90 degrees counterclockwise"](https://www.cs.auckland.ac.nz/~cristian/UMCreadings/revcomputCA.pdf#page=2). With one particle it is a garbage-free walker and, like iso.rs's machines, sequential.
- **With several tokens, reversible parts stop composing.** ["reversibility is not closed under circuit composition"](https://www2.nict.go.jp/nano/results/abstract/17_217.html): arrival order becomes information. Keeping cause and effect is the thing that has to be paid for.

### Reversible combinators, interaction nets and token machines

- **Abramsky's reversible combinators are automata walked by a token, not reductions.** The automaton a combinator compiles to has rules in which ["each variable which occurs does so exactly once in r and exactly once in s"](https://arxiv.org/pdf/1111.7154#page=6); the run is reversible, ["not in the simplistic sense that application between combinators is itself reversible"](https://arxiv.org/pdf/1806.06759#page=4) (research/RULES_TO_LATTICE.md §7).
  - Copying can be a tag on the question instead of a copy: ["using the copy-tag field of !b to split its address space into two"](https://arxiv.org/pdf/1111.7154#page=18).
  - A built-in conditional breaks this: ["this option is not open to us if we wish to retain reversibility"](https://arxiv.org/pdf/1111.7154#page=24). Tree calculus loses information exactly at K and at triage.
- **Reversible combinatory logic records, per step, what** ["incorporates information useful to reconstruct the redex from the contractum"](https://www.doc.ic.ac.uk/~herbert/papers/mscs06.pdf#page=2). ["An important property of rCL is that reductions always increase the length of the history term"](https://www.doc.ic.ac.uk/~herbert/papers/mscs06.pdf#page=8).
- **Interaction nets reverse under a clean condition.** ["An interaction system is reversible if and only if no two different active pairs clash and each rule is connected"](https://arxiv.org/pdf/1806.07275#page=4). Whether ["there exists a reversible equivalent of interaction combinators"](https://arxiv.org/pdf/1806.07275#page=6) is open. Footprints sidestep the question by keeping the pair.
- **Reading consumes; token machines walk instead, and pay for it.**
  - In a net, ["interacting agents are consumed"](https://www.user.tu-berlin.de/o.runge/tfs/workshops/gtvmt08/Program/paper_38.pdf#page=13), so a second reader needs a copy.
  - Token machines walk a fixed graph instead. The interaction machine ["needs no external garbage-collection"](https://www.pure.ed.ac.uk/ws/files/16874549/Reversible_irreversible_and_optimal_machines.pdf#page=4), but it re-walks and ["can be exponentially slower than environment machines"](https://arxiv.org/pdf/2010.12988#page=2).
  - The shortcut that saves that time costs ["the possibility to jump from one subterm to another, thus losing locality and reversibility"](https://arxiv.org/pdf/2606.27062#page=2).
  - Many tokens on one graph are parallel only while they do not interact: ["multiple tokens could travel the net in parallel, but what we compute is exactly the same, because the tokens do not interact"](https://arxiv.org/pdf/1405.3427#page=4).
- **Lafont already ran nets two ways.** ["we interpret the nets of combinators as reversible 2-stack machines"](https://dcreager.net/pdf/Lafont1997.pdf#page=22). He also asked ["is it reasonable to consider erasing and duplication as atomic operations"](https://dcreager.net/pdf/Lafont1997.pdf#page=2).

### Reversible languages

- **Reversible programs compute exactly the injective functions.** ["can compute exactly all injective, computable functions"](https://researchprofiles.ku.dk/en/publications/what-do-reversible-programs-compute/#main-content). Tree calculus is not injective, so it runs reversibly only embedded, as x ↦ (x, f x), with either the run's history or Bennett's cleanup.
- **Information as a linear resource.** James and Sabry treat ["information is treated as a linear resource that can neither be duplicated nor erased"](https://web.archive.org/web/20161007070937id_/http://www.cs.indiana.edu/~sabry/papers/information-effects.pdf#page=1), with losses sent to ["a global garbage dump (which absorbs the undesired results)"](https://web.archive.org/web/20161007070937id_/http://www.cs.indiana.edu/~sabry/papers/information-effects.pdf#page=2).
- **Copy and equality are one operation read two ways.** ["the duplication of a value in one direction requires an equality check in the other direction, and vice versa"](https://di.ku.dk/english/research/phd/phd-theses/2018/Robin_Kaarsgaard_Thesis.pdf#page=88). Un-reading in this core is that equality check. Duplication and erasure can also be derived: ["build isos to encode notions of duplication, erasure, and constant manipulation thanks to partiality"](https://arxiv.org/pdf/2309.12151#page=6).
- **Branches need exits you can tell apart.** ["in the backward direction an assertion acts as a test and a test as an assertion"](https://people.cs.rutgers.edu/~uli/cs516/spring2020/readings/ReversibleComputing-TheoreticalCompSci2010.pdf#page=5). Likewise ["the leaf expression of any branch in a case expression must not match any leaf expression in a branch preceding it"](https://arxiv.org/pdf/2105.09929#page=2).
- **In-place updates need the target absent from the expression.** ["The variable x on the left-hand side of an assignment must not appear in the expression e on the right-hand side"](https://people.cs.rutgers.edu/~uli/cs516/spring2020/readings/ReversibleComputing-TheoreticalCompSci2010.pdf#page=4). That is why `b += a` needs no history.
- **A single global history does not compose.** ["a fundamental limitation of this approach is that it fails to be compositional"](https://arxiv.org/pdf/2405.20842#page=3). History has to live where it was made.

### Undo in concurrent systems

- **There are two places to keep the past.**
  - On each thread: ["assign to each currently running thread an individual memory stack keeping track of past communications"](http://web.archive.org/web/20060528171615id_/http://www.pps.jussieu.fr:80/~danos/pdf/rccs.pdf#page=2).
  - In the term itself, with shared keys: ["In order to manage correctly both communication and the reversing of communication we employ communication keys"](https://www.cs.le.ac.uk/people/iu3/Publications/RC2012-Web.pdf#page=2).
  - For CCS the two behave the same, and the second is never copied: ["In CCSK, history information is embedded in the process structure, hence it is never duplicated"](https://iris.unito.it/bitstream/2318/1891802/2/s00236-019-00346-6.pdf#page=2).
- **The general sound recipe is a memory per step that names its products.** ["each entity of a system is labelled with its unique key. Also, each step of the system produces a memory allowing one to undo it"](https://drops.dagstuhl.de/storage/00lipics/lipics-vol171-concur2020/LIPIcs.CONCUR.2020.33/LIPIcs.CONCUR.2020.33.pdf#page=5). A footprint is that memory, with wires in place of keys.
- **A gate can keep what it absorbed, plus a pointer to its progress.** ["the symbol ^, called gate pointer, indicates the next operations (one forward and one backward)"](https://lucacardelli.name/Papers/Reversibility%20in%20Massive%20Concurrent%20Systems%20%28SACS%29.pdf#page=8).
- **Where each result has one producer, mirror rules alone reverse the run.** ["an occurrence net can be reversed by adding a reverse transition for each of its transitions"](https://arxiv.org/pdf/1910.04266#page=3).
- **Thermodynamics of history-keeping processes.** Copying histories must be priced: ["processes must have a probability to fork that decreases exponentially as a function of the size of their local memory"](http://www.cs.ox.ac.uk/people/ohad.kammar/publications/bacci-danos-kammar-on-the-statistical-thermodynamics-of-communicating-processes.pdf#page=2). Symmetric base rates keep detailed balance: ["time reversibility can be obtained by using in the operational semantic rules backward rates equal to the corresponding forward rates"](https://arxiv.org/pdf/2205.01420#page=3).

### History and clean space as resources

- **Bennett's cleanup.** Keep a history, make an ["extra copy"](https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/Bennett_Reversibiity.pdf#page=2), then the machine ["reversibly disposes of all the undesired intermediate results by retracing the steps of the first stage in backward order"](https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/Bennett_Reversibiity.pdf#page=1).
  - The history is not noise: ["there exists a subtle mutual redundancy between it and the machine that produced it"](https://www.cs.princeton.edu/courses/archive/fall06/cos576/papers/bennett73.html#Introduction).
  - For ["many calculations in which the output uniquely determines the input"](https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/Bennett_Reversibiity.pdf#page=5), an inverse program erases the input too.
- **Dropping history early is pebbling.** ["put a free pebble on node i or remove a pebble from node i, provided node i - 1 is pebbled at the time"](https://arxiv.org/pdf/quant-ph/9703022#page=7). On graphs, ["a pebble can be removed from a vertex v if and only if all direct predecessors of the vertex v are covered by pebbles"](https://www.numdam.org/item/10.1051/ita:2004008.pdf#page=3). The no-history extreme is a single walker: ["the LMT-simulation using exponential time and no extra space"](https://arxiv.org/pdf/quant-ph/0101133#page=9).
- **Borrowed space must come back exactly**: ["it is not sufficient to have just reversibility"](https://ir.cwi.nl/pub/22659/22659B.pdf#page=2). Idle space can be borrowed: ["dirty bits are more plentiful, since any temporarily unused bit is a borrowable dirty bit"](https://arxiv.org/pdf/1706.07884#page=1).
- **In-place adders spend their own history.** Cuccaro's adder parks the carries on the way up, then ["we apply the UMA gate, which restores ai to Ai and ci to Ai-1 and writes si to Bi"](https://arxiv.org/pdf/quant-ph/0410184#page=2). Also: ["This enables us to reuse the same temporary register"](https://arxiv.org/pdf/quant-ph/9511018#page=3).
- **A value can be uncomputed when what remains can rebuild it.** ["uncomputing x is possible whenever in every summand of the state, the value of x can be reconstructed"](https://files.sri.inf.ethz.ch/website/papers/pldi20-silq.pdf#page=6). Baker: ["garbage collection for recycling storage can always be performed by a reversed (sub)computation"](https://www.plover.com/~mjd/misc/hbaker-archive/ReverseGC.html).

### Clocks, heat and distance on a physical lattice

- **Feynman's quantum computer has one cursor, one gate at a time.** Its garbage clearer runs the same machine both ways: ["then sending the data back to the machine in the opposite direction"](https://opn-web-afd-d3bfbkd5bcc5asbs.z02.azurefd.net/opn/media/images/pdfs/11557/11557_23417_110730.pdf#page=8).
- **Many cursors need local clocks.** ["add a single bit of update-count to each cell, we can run the system asynchronously while retaining a perfectly synchronous causality"](https://people.csail.mit.edu/nhm/pqc.pdf#page=9), shown in 1D. For circuits, ["the global clock in the Feynman-Kitaev construction gets replaced by a time-configuration"](https://arxiv.org/pdf/1311.6101#page=5).
- **Heat can be particles that carry history off and bring it back.** ["Information about exactly when and where a particle undergoes a phase transition is stored in the heat particles"](https://people.csail.mit.edu/nhm/ra.pdf#page=3).
  - Reversible chemistry gets its fuel back: ["brings the chemical system into the same exact physical state as it was in the beginning, including recovery of fuel reagents and any energy used"](https://www.dna.caltech.edu/Papers/DNA_stack_machines2011_LNCS.pdf#page=3).
  - A gate can be its own history: ["the same gate can only be used next to consume a B signal, produce an A signal, and reset itself to the forward orientation"](https://www.cs.ubc.ca/~condon/papers/dna-pspace.pdf#page=3).
- **Erasing is limited by area, and only neighbourly machines scale.** ["irreversible computers turn out to ultimately be limited to processing rates that are only proportional to their surface area"](https://web.archive.org/web/20040705045908id_/http://www.cise.ufl.edu/~mpf/rc/thesis/phdthesis.pdf#page=27); ["The main result is that scalability holds only for neighborly interconnections, such as the square mesh"](https://cs.brown.edu/research/pubs/techreports/reports/93/cs93-20.pdf#page=3). No ids, no tables.
- **Finding a partner along a chain is fast; in open 3D it is slow.** ["The strong dependance of the mean reaction time with the target size in this limit is the signature of a non-compact exploration"](https://arxiv.org/pdf/1209.1538#page=5). What matters ["is not the subdiffusive vs diffusive nature of the transport process, but its compact vs non compact type"](https://arxiv.org/pdf/1006.3477#page=8). For disp: an undo should be two agents meeting along one wire, not several agents gathering in one block. The substrate's 10–12× slower way back is the second kind.
- **Independently placed annihilators clump.** ["This conservation law provides a slow mode in the dynamics that is crucial in determining the long-time behavior"](https://arxiv.org/pdf/cond-mat/0501678#page=34). Make refunds where their partner is.
- **Wire layout.** Local moves that keep a wire's length cannot reach every layout: ["The nonergodicity of the Verdier-Stockmayer algorithm was noticed as early as 1968"](https://arxiv.org/pdf/hep-lat/9405016#page=40). Fold needs an inverse that adds length mid-wire. Densely connected wiring needs volume: ["Kolmogorov and Barzdin essentially observed that a random graph is an expander"](https://arxiv.org/pdf/1103.3423#page=1).

### Chemistries, dataflow hardware, distributed bookkeeping

- **Conserving combinator chemistries make copies and losses explicit.** ["S duplicates its third argument x. Thus, to make a chemical system with conservation laws"](https://arxiv.org/pdf/2003.07916#page=3) the copy must come in as a reactant. ["In order to be conservative in nodes and edges, we need to add tokens"](https://arxiv.org/pdf/2306.00938#page=5).
- **Disjoint reactions need no scheduler.** ["any number of reactions can be performed in parallel, provided that they involve disjoint sets of molecules"](https://courses.grainger.illinois.edu/cs522/sp2016/TheChemicalAbstractMachine.pdf#page=2); ["Computation in the MFM is based on event windows"](https://www.cs.unm.edu/~ackley/papers/hotos-11.pdf#page=2).
- **Dataflow machines solved related problems.**
  - They share code by naming activations: ["keeping a single copy of the function body, and by tagging each token with a context identifier"](https://csg.csail.mit.edu/pubs/memos/Memo-271/Memo271IEEE.pdf#page=4).
  - They bound unfolding with cycles: ["loops exhibit bounded unfolding if and only if the graph forms a single strongly connected component"](https://ftp.mirrorservice.org/sites/www.bitsavers.org/pdf/mit/lcs/tr/MIT-LCS-TR-0332.pdf#page=3).
  - Names need only be local: ["Each local tag space can be tiny (two tags is enough) without risking deadlock"](https://www.pdl.cmu.edu/ftp/Storage/Agarwal2024.pdf#page=2).
  - Dropped work is cancelled backward: ["tokens carrying information move in the forward direction and anti-tokens canceling information move in the backward direction"](https://www.cs.upc.edu/~jordicf/gavina/BIB/files/dac07_early.pdf#page=1).
  - Dying cells become the new ones: ["In case 4, cells 2 and 3 can both be reused, which is very convenient as we were just about to generate two new cells"](https://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-81.pdf#page=38).
- **Return paths without ids.** ["each edge will have carried as many messages in the one direction as it has carried signals in the opposite direction"](https://www.cs.utexas.edu/~EWD/ewd06xx/EWD687a.PDF#page=4); ["If it has recovered the whole sum, the environment announces global quiescence"](https://vs.inf.ethz.ch/publ/papers/Credit_TermDet.pdf#page=3); ["When a reference is copied, its weight is split among the existing and the new reference"](https://vs.inf.ethz.ch/publ/papers/TermDet_Garb_coll.pdf#page=18). Footprints form exactly such a tree, and a run is undone when its root is back where it started.

### Tree calculus and rewriting under reversibility

- **Where tree calculus loses.** ["Note that (K) deletes the third argument, (S) duplicates the third argument"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=40).
- **A rewrite step need only record what it erases.** ["will record the bindings of erased variables"](https://arxiv.org/pdf/1710.02804#page=8). With arguments shared by name, a machine ["exhibits a particularly compact Landauer's embedding, requiring only constant space overhead for each step"](https://arxiv.org/pdf/2605.09179#page=1).
- **Irreversibility is the graph of configurations branching backward.** ["irreversibility of M translates precisely into the fact that Gw is a tree and not simply a line"](https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/LMT_ReversibleSpace.pdf#page=5).
- **Graph rewriting that deletes only isolated nodes is reversible by construction.** ["An interesting consequence of the definition of DPO semantics is that rule applications are always reversible"](https://arxiv.org/pdf/2204.07175#page=4). Split nodes can carry their ancestry: ["reversibility forces us to allow for the merger of two node names as the correct inverse operation of node splitting"](https://arxiv.org/pdf/2510.03296#page=3).
- **Copying can carry a budget.** ["still valid w.r.t. bounded exponentials : the bounds are respectively given by 0, 1, +"](https://girard.perso.math.cnrs.fr/LLL.pdf#page=2).

## 3. The recommended core

### 3.1 Agents

Every agent except the root has three kinds of port: a front port (principal, listed first), its other ports, and a **back port** wired to the footprint that made it. Loaded agents have their back port capped, meaning they have no past.

| role | agent | ports, front first |
|---|---|---|
| matter | leaf, stem, fork | `L[p]`, `S[p; c]`, `F[p; l, r]` |
| apply | `A` | `A[f; z, r]`: faces the function; argument z, result r |
| first look | `T` | `T[c; y, z, r]`: faces the applied fork's left child c; holds its right child y |
| dispatch | `Q` | `Q[z; w, x, y, r]`: faces the argument; holds the three arms |
| copy | `D` | `D[v; o1, o2]`: copies one layer |
| pass on | `R` | `R[v; o]`: hands a value on whole. As the K step's product, it keeps that step undoable until the value moves on |
| read out | `N` | `N[v; o]`: the root's reader |
| collect | `E` | `E[v]`: takes a dropped value apart; every step it takes is a footprint |
| reader | `Rd`, `Rdd` | `Rd[u; v]` sits on a wire; `Rdd[u; v, o]` has read the value below and holds its twin at o. No back ports |
| footprint | `φ(k, m)` | one per rewrite: rule k and the set m of products that are out. Holds the used pair's back wires, one child port per product out, and the wires of products still inside |
| root | `Out` | `Out[in]` |

- Fuel is empty seats. Heat leaves as one particle per committed footprint.
- Gone from today's table: the suspension `P`, `Pair` and `Unp` (T and Q hold their arms directly), and `Seam`.
- Superpositions and movement agents are left out. They could come back later.

### 3.2 Rules

- **Notation:**
  - `X[a; b, c]` is agent X with its front on wire a and its other ports on b and c, in order.
  - `C · P` is two fronts meeting.
  - m, n, a, b, c, d are new wires inside the result.
- Back ports are not written: every product's back port goes to the rule's footprint.
- Each line's two sides are the same tree-calculus term.

```
apply     A[f; z, r] · L          →  S[r; z]                                △ z = △z
          A[f; z, r] · S[x]       →  F[r; x, z]                             △x z = △xz
          A[f; z, r] · F[c, y]    →  T[c; y, z, r]                          look at the fork's left child
K         T[c; y, z, r] · L       →  R[y; r]  E[z]                          △△y z = y
S         T[c; y, z, r] · S[s]    →  D[z; a, b]  A[s; a, m]  A[y; b, n]  A[m; n, r]
                                                                            △(△s)y z = s z (y z)
triage    T[c; y, z, r] · F[w, x] →  Q[z; w, x, y, r]
          Q[z; w, x, y, r] · L    →  R[w; r]  E[x]  E[y]                    z = △    →  w
          Q · S[u]                →  A[x; u, r]  E[w]  E[y]                 z = △u   →  x u
          Q · F[u, v]             →  A[y; u, m]  A[m; v, r]  E[w]  E[x]     z = △uv  →  y u v
copy      D[v; o1, o2] · L        →  L[o1]  L[o2]
          D · S[c]                →  S[o1; a]  S[o2; b]  D[c; a, b]
          D · F[l, r]             →  F[o1; a, c]  F[o2; b, d]  D[l; a, b]  D[r; c, d]
pass on   R[v; o] · X             →  X with its front on o                  X = L, S or F
read out  N[v; o] · L             →  L[o]
          N · S[c]                →  S[o; a]  N[c; a]
          N · F[l, r]             →  F[o; a, b]  N[l; a]  N[r; b]
collect   E · L                   →  nothing
          E · S[c]                →  E[c]
          E · F[l, r]             →  E[l]  E[r]
          E on D's o1             →  R[v; o2]          (mirror: E on o2 → R[v; o1])   un-copy before copying
          E on A's r              →  E[f]  E[z]                             work not yet started
          E on T's r              →  E[c]  E[y]  E[z]
          E on Q's r              →  E[z]  E[w]  E[x]  E[y]
          E on R's o, N's o       →  E on its input
          E · E                   →  nothing
```

That is 29 rules. `E` on `N`'s output and `E · E` never fired on the 21 calls; they are there to complete the table.

### 3.3 Laws: every rule runs both ways

```
meet      C · P            ↔   φ(k, ∅)                   drive: −ε forward, +ε back
emit j    φ(k, m)          ↔   φ(k, m + j), product j    −ε′ forward, +ε′ back
commit    φ(k, all out)    →   one heat particle         the only lossy step
```

- **Meet and part.** When a rule's two fronts meet, they fuse into one footprint holding their outside wires and their two back wires. Read right to left, this is **part**: a footprint with nothing out splits back into its pair.
- **Emit and absorb.** Emit puts product j in an empty seat, wired as rule k says, with its back port on the footprint's child port j. Read right to left, this is **absorb**. It applies only if the agent on child port j is product j itself and untouched: its back port is that wire and it has met nothing since. That test is local.
- **Commit** erases a footprint whose products are all out. It caps their back ports and the back wires it held. The older footprints behind it can then never split again, so forgetting spreads into the past one commit at a time.
- **No merges, so little drive.** Nothing merges two states except commit, so a small drive is enough: this is Bennett's unbranched case.
  - Meets carry the drive ε.
  - Emits carry a small ε′, so a finished answer does not flicker.
  - Reading out the answer ends with one one-way latch at the root.
- A rule fires as meet then emits. With room in the block, that is one lattice move, as today.

### 3.4 The reader

`Rd over X` means a reader on the wire into X's front.

```
split   Rd over S[c]                 ↔   S as it was;  Rd on c
        Rd over F[l, r]              ↔   F as it was;  Rd on l,  Rd on r
leaf    Rd over L                    ↔   Rdd over L, holding a new L′ at o
join    Rdd on the child of S        ↔   Rdd over S, holding a new S′ over the child's twin
        Rdd on both children of F    ↔   Rdd over F, holding a new F′ of the two twins
```

- **Twins have no past.** L′, S′ and F′ are fresh matter with their back port capped.
- **The value read is never touched.** Its footprints could still take it back, so either finish forward before reading, or have readers mark the nodes they pass.
- **Read right to left, the rules un-read.** A join runs backward only when the twin node is the same kind as the node beside it. So un-reading a value against a tree is RFun's equality check, one layer at a time, and it sticks where the two differ.
- **Cost** *(model)*: reading takes 3–35 rounds on the 21 calls, except 215 for `greet "a"`, whose unary character codes are deep. Un-reading takes exactly as many rounds and returns the net exactly.

### 3.5 Copy, delete, reuse

- **Copying draws fuel.** `D` builds each layer of its two copies in empty seats, only as readers take them. The reader builds its twin the same way. Running back returns the seats. Footprints take seats too, so a run's room grows until it is run back.
- **Un-copy is a refund.**
  - A copy's footprint takes back a copy nobody used.
  - An `E` that reaches a `D`'s unused output turns the `D` into a relay before any copy is made. This happened 26,366 times over the 21 calls.
  - Both leave a footprint, so they are free only once that footprint is run back.
  - Until then, the footprint is what knows the twins are equal. After a commit the twins can no longer be matched, and erasing one costs real heat.
- **Deleting is commit.**
  - `E` takes dropped values apart, but every step it takes is a footprint, so nothing is lost until a commit.
  - A commit costs what the footprint holds that the remaining matter does not determine: its rule and mask (about 9 bits), where its wires led, and the strands it frees. That is more than one unit.
  - Heat leaves as a particle that drifts to the edge, so sustained erasing is limited by the lattice's surface.
- **Reuse means borrow and return.**
  - Code is still copied layer by layer by the S rule. This is how code multiplies to where it is read, i.e. parallelism by self-reproduction.
  - Running a call back returns its code copies to their duplicators. A function is reused over time, not shared in place.
  - Sharing in place is what Riders tried, and it ties every caller's history together.
  - Copying ahead where a block has room costs no heat here, only seats and the time to give them back.

### 3.6 History as a resource

The footprints are the run's history, held as matter: each is the pair a rewrite used, wired to what it made. The system and programs use it in five ways.

1. **A clean run (Bennett).** Run, read the answer, run back. The footprints are spent, the inputs and code come back, and the answer's twin stays. All 21 calls end with zero footprints *(model)*. It costs about twice the time, plus the copy's fuel.
2. **The inverse's history erases the input** (Bennett's stages 4–7). Run an inverse on the copy, un-read the input against its output, then run the inverse back. The input goes, its seats are refunded, and nothing else is left.
3. **Undo anywhere, in causal order.** Any product nothing has used can be taken back, so any finished part of a run can be undone locally and in parallel. This gives a debugger and time bar with no saved states.
4. **Measure the real loss.** Running back only the branches that missed the answer clears 18–31% of footprints. What remains beside the answer is exactly the loaded originals that were dropped without ever being copied *(model)*: 24 agents for `add 2 3`, 65 for `rev [1, 2]`, 70 for 8-bit `relay_add`, 83 for `fib 3`. That is the run's true information loss.
5. **Forget for heat.** Commit trades history for heat when time and room matter more.

Programs ask for 1 and 2 through two definitions whose meaning is ordinary disp; only the way the core runs them is new (3.7). A per-call version, running each call back as soon as its result is read (pebbling the run call by call), is the next step and is untested.

### 3.7 Reversible add, written out

The definitions below are an add that keeps a, its inverse, and the two ways of running them. With evaluators/rust-ic-strands/programs/programs.disp open for `add`, all of these pass `scripts/probe.sh -r`:

```disp
sub := fix ({self, s, a} => if (is_leaf a) then s else (self (nat_pred s) (nat_pred a)))
add_keep := {p} => pair (pair_fst p) (add (pair_fst p) (pair_snd p))
sub_keep := {p} => pair (pair_fst p) (sub (pair_snd p) (pair_fst p))
bennett := {f, x} => pair x (f x)
bijection := {f, g, x} => f x

test sub_keep (add_keep (pair 4 1)) = pair 4 1
test bennett add_keep (pair 2 3) = pair (pair 2 3) (pair 2 5)
test bijection add_keep sub_keep (pair 2 3) = pair 2 5
```

`bennett` and `bijection` mean what their definitions say. What is new is how the core runs them:

- **`bennett f x`:** apply, read the result, run the application back. This leaves x, f, and the copy of f x, with no footprints.
- **`bijection f g x`**, when `g (f x) = x`:
  1. run `bennett f x`;
  2. run g on the copy;
  3. un-read x against g's output;
  4. run g back.

  This leaves only f x.

Here is `bijection add_keep sub_keep (pair 2 3)` step by step *(model)*. Numbers are unary: n is n forks over a leaf.

| step | what happens | cost |
|---|---|---|
| 1 | run `add_keep (pair 2 3)` forward | 906 rounds, 2,749 footprints, widest round 15 |
| 2 | read the answer (2, 5) | 13 rounds; the copy takes 17 empty seats |
| 3 | run back | 862 rounds; zero footprints; the program and (2, 3) back agent for agent |
| 4 | run `sub_keep` on the copy | 935 rounds, 2,974 footprints; gives a fresh (2, 3) |
| 5 | un-read the input (2, 3) against it | about 9 rounds (reading 13 agents takes 9); 13 seats back; sticks if the two differ |
| 6 | run `sub_keep` back | 938 rounds; zero footprints; the copy (2, 5) is whole again |

- **Left over:** (2, 5) and the two programs. No footprints, and nothing committed.
- **Fuel:** 17 − 13 = 4 seats, which is 2a: the forks and leaves that a+b has beyond b.
- **Heat:** none beyond the drive.
- **History used:** add's own footprints give back its input and the code it copied (step 3). sub's footprints are what erase the input (steps 5–6).
- **Time:** about 3,660 rounds, against 873 for a plain, lossy `add 2 3` under the same scheduler. That is about 4× to leave nothing, or 2× (steps 1–3) to keep the input.
- **Not built:**
  - Steps 1–3 were measured as one run. Steps 4 and 6 were measured as `sub_keep (pair 2 5)` on its own.
  - The driver that hands values between steps does not exist yet. It must unhook the input from its application, hook the copy into `sub_keep`, and place the un-reader.

Binary works the same way. `relay_add` on 8-bit 255 + 1 runs forward, reads (19 rounds) and runs back (531 rounds) to zero footprints *(model)*. A binary subtraction to play `sub`'s role is not written.

Writing the adder in place, as Cuccaro does with the carries parked in the data, does not remove the run back here. Every tree-calculus rewrite leaves a footprint whatever the program's shape. Steps with no footprint at all are an open question (section 7).

### 3.8 How disp's programs run

disp's compiled trees load unchanged as matter, with back ports capped. Each application loads as an `A`, and the root as `N` and `Out`. Demand works as today: wanted results pull their producers, and both halves of an S step start wanted (fork).

| step | rules | left behind |
|---|---|---|
| △ z = △z, △x z = △xz | `A·L`, `A·S` | one footprint |
| K: △△y z = y | `A·F`, `T·L` | two footprints; an `R` until y moves on; an `E` on z that takes it apart or un-copies it at a duplicator |
| S: △(△s)y z = s z (y z) | `A·F`, `T·S` | two footprints; the `D` copies z a layer at a time as it is read |
| triage on z | `A·F`, `T·F`, then `Q·L`, `Q·S` or `Q·F` | three footprints; `E` on the two arms not taken (mostly code copies, given back) |

- **Mix at the answer** *(model)*: copying is 28–34% of footprints (4% in the unrolled relay), taking apart 28–39%, apply 15–23%, dispatch 11–18%, passing on 10–15%. This is the same split as today's lattice work.
- **Size.** One footprint per rewrite, which is 8.7–37× the loaded program (1.7× for the unrolled relay). Kept whole, matter peaks at 6.2–8.1× today's net. Besides commit, the only way down is running back, with or without reading first.
- **Checked** *(model)*:
  - All 21 compiled calls in research/linear-nets/calls.txt reach the eager evaluator's answer and run back to the loaded net agent for agent. The calls are fib 3–5, add, mul, is_even, sum, rev, doubled, isort, size, greet, and the ripple, relay, unrolled relay and lookahead adders at 8 and 16 bits.
  - So do 8,000 random terms with random changes of direction. An independent re-run passed 4,000 of 4,000.

### 3.9 Local and parallel

- **Local.** Every step is two agents joined by one wire, plus empty seats in one 2×2×2 block. The four kinds of step are:
  - front to front (meet);
  - a collector's front on an output (collect);
  - a child port on a back port (absorb);
  - a reader on a wire (read).

  No ids, tables, record numbers or distance reads.
- **Parallel.** Each agent has one front port, so ready meetings are disjoint, with one exception: a collector can reach a duplicator's output while the duplicator's own meeting is ready. Over the 21 calls this happened in 268 agent-rounds, plus 473 with two collectors on one duplicator. Settling these collector-first inside a block, or at random, changes footprint counts by at most 1%.
- **Forward** *(model, same scheduler for both tables)*: 0.94–1.08× today's table's rounds, geometric mean 1.02×.

  | call | widest round, footprints | widest round, today's table |
  |---|---|---|
  | `add 2 3` | 15 | 12 |
  | `fib 3` | 36 | 36 |
  | 16-bit `relay_add` | 92 | 52 |

  This scheduler takes 873 rounds for today's `add 2 3` where rev.rs's strict rounds take 631, so only the ratios carry over.
- **Back** *(model)*:
  - 0.55–1.09× the forward rounds when each footprint takes back one product a round, and 0.28–0.55× when it takes back all of them in one move.
  - Each causal step costs two rounds (absorb, then part). So against a forward not slowed by demand, going back takes 2.0× (`fib 3`: 1,505 rounds against 752).
  - The widest backward round holds 171–3,058 events.
- **Against the alternatives.** The walkers took 9.4–51× the net's rounds; this keeps the net's width in both directions. Copying and erasing add width more than depth: counting them as free shortens rev.rs's causal chain by only 5–11%. So their footprints sit mostly beside the critical path.

### 3.10 On the strand lattice

- **Seats and wires as now.** Each agent gains a back port, and back wires are strand paths like any other wire.
- **Moves:**
  - meet + emit is today's rewrite, with the footprint seated where the pair was;
  - absorb needs a product one strand from its footprint, along its back wire;
  - part;
  - the reader's moves;
  - commit;
  - step, exchange, fold and flip as now, plus unfold (fold's inverse: a U-turn pushed into a neighbour), so every move has an inverse.
- **Energy.**
  - A meet releases the drive ε and a part takes it back. Emits get a small ε′. Reading ends with a one-way latch at the root.
  - When driven backward, tension on back wires pulls products home along a real path. This does the job of the substrate's bonds with no position read and no gathering.
  - Fuel is empty seats, so copying waits for room as now.
  - Heat particles take a seat and drift to the edge, so erasing warms its neighbourhood and is limited by the lattice's surface.
- **Ports are the problem.**
  - Partly taken back, the footprints of `T·S` and `D·F` have 10 wire ends, `Q·F` has 8, and `T·F`, `Q·S`, `D·S` and `N·F` have 7.
  - A site's switchboard holds 8 pairings, and crowding costs p²/4. That is already 9 for one 6-end footprint, more than the substrate's drive ε = 8.
  - Options: raise the limit (a pairing is two end numbers, 10 bits in 3D), or build a big footprint as a short chain of 3-port agents.
- **Demand is the other problem.**
  - On the lattice, a demand pulse is dropped and sent again when its strand moves. That is an erasure nothing records. The model instead works out demand by walking the net from the root each round.
  - Fix: a pulse becomes a token that rides its strand, and each footprint keeps the wanted bits of what it used, as the substrate's records do. This is untested.
  - Dropping demand works (all 21 calls finish, in 1.9–3.9× fewer rounds) but costs 1.7–24.6× the footprints: eager `fib 3` peaks at 143,612 footprints against 723 loaded agents.
  - Laziness also matters on the lattice: without demand pulses, runs are 40–66% slower under blocks.

### 3.11 On a reversible or quantum block substrate

- **The laws are one-to-one on the net.** A footprint's kind names its rule and which products are out, and nothing else makes it. Absorbing checks that the product is untouched. Nothing merges two states except commit, so a Brownian run needs only a small drive.
- **On the lattice, layout breaks this.** A meet "one strand apart", an absorb "within one strand" and a fold each forget where something sat. There are two ways forward:
  - **Stochastic** (do this first, since it is what the lattice is): random local proposals with detailed balance, accepted against a per-site energy counter (a Creutz demon) instead of a global temperature. Claim reversibility only in the statistical sense.
  - **Deterministic:**
    - every agent carries a direction bit, and running backward flips them all;
    - products are emitted and absorbed only in fixed relative seats;
    - a footprint stores its partner's direction (about 3 bits);
    - fold is paired with an unfold that remembers the turn;
    - each block's choice of move comes from a hash of block and clock.

    Then each move is a permutation of block states, which validate() can check by enumeration, and the alternating Margolus blocks make each clock invertible. Whether this fits a fixed 2×2×2 table over these agents is open.
- **Conserved per block:**
  - seats (agents plus empty);
  - wire ends (each rule is a perfect matching);
  - back wiring (each back port is on exactly one footprint, or capped);
  - energy (demons plus drive).
- **Quantum reading** (deterministic version only):
  - Block permutations are permutation unitaries.
  - Commit is the one step that needs an environment: heat particles leaving at the edge.
  - `D` and the reader copy only in the computational basis, so a superposition under a `D` gets entangled, not copied.

### 3.12 The critics' fixes, and what could still sink it

| problem the critics found | fix folded in | status |
|---|---|---|
| The footprint design held that Bennett's cleanup cannot run inside the net, so zero history meant one sequential walker | the reader | measured: 21 of 21 calls to zero footprints; reading and un-reading exact |
| Demand came from a global walk; on the lattice, pulses are dropped | pulses as tokens; footprints keep wanted bits | untested |
| Lattice moves forget layout, and the design mixed a drive with a permutation | stochastic first; the deterministic version needs direction bits, fixed seats, unfold | untested |
| With only meets biased, a finished answer flickers: `add 2 3`'s answer is whole in about 4% of samples, `size [5, 6, 7]`'s in 12% | bias emits by ε′; latch the root reader | untested |
| Footprints reach 10 wire ends; a site holds 8 pairings | measure inert footprints first; more pairings or chained footprints | untested |
| History-free "exchange" walkers were unsound beside footprints, and could not touch disp's bits (trees, not agent kinds) | dropped from the core; use the reader's cleanup | n/a |
| One unit of heat per commit undercounts | commit costs the rule and mask bits, the wiring, and the strands freed | design |
| A collector and a duplicator's own meeting can both be ready | collector first inside a block | measured: at most 1% change |
| Going back takes two rounds per causal step | take back every unused product already in the block in one move | measured: 0.28–0.55× forward rounds |
| History could only be cleared for the whole run | a reverse marker that runs one call back once its result is read | untested |
| Some citations were used for claims they do not make | cited here only for what they say | n/a |

**Fatal flaws: none survived.** The critic attacked the laws themselves: are they one-to-one, is "unused" detected correctly, is cause and effect kept. They held. Garbage collection by running back is local: freezing the root reader's footprints gives exactly the same state as the global computation on all 21 calls.

What could still sink the core is cost:
- carrying footprints on the lattice;
- making demand reversible without losing laziness;
- whether any fixed 2×2×2 table runs it deterministically.

### 3.13 Against the nine criteria

| # | criterion | status |
|---|---|---|
| 1 | local | every rule and law touches two agents on one wire plus empty seats in one block; demand still has to become matter |
| 2 | maximally parallel | forward keeps the net's width; going back takes 0.55–2.0× the forward rounds; the one clash is settled inside a block |
| 3 | lattice | each move fits a 2×2×2 block; footprints overflow today's 8 pairings a site (untested) |
| 4 | physical | the laws are one-to-one and nothing merges but commit; heat is particles leaving at the edge; a deterministic block version is open |
| 5 | linear | one front port per agent; copying only by `D` and the reader (fuel); losing information only by commit (heat) |
| 6 | resources | seats are fuel, returned by running back; commit is heat; code is borrowed and returned rather than shared |
| 7 | history as a resource | the footprints, spent by running back; an inverse's footprints erase inputs; (a, b) → (a, a+b) leaves nothing (pieces measured, driver unbuilt) |
| 8 | simpler than now | replaces the substrate's records, ids, bonds, gathering and forget cascade; rule count is about today's |
| 9 | runs disp | all 21 compiled calls and 8,000 random terms, there and back |

## 4. Runner-up designs

The judges ranked Riders first and footprints second. The critics then broke Riders and repaired footprints.

- **Riders: code read in place, never copied.**
  - What it was: a rider is a pointer agent that walks down code without consuming it. Each application is a frame with two riders. Finished frames become the stems and forks they built, and K leaves the frame behind as a receipt. Its own model claimed 0.17× the lazy net's work at 0.52× its depth.
  - Why the critic found it unworkable as a core:
    - Every call that reads a shared node shares that node's history. Exact undo without ids then becomes last-in-first-out across unrelated calls: 28–36% of the dropped work in `fib 4` to `fib 8` sits under later work that is still needed. The alternatives are paying heat or adding ids.
    - A shared node serves one reader at a time, so parallel callers need copies after all.
    - Records pile up in the read path. Charged honestly, the benchmark falls to 0.31× the work and 0.74× the depth. `add 64 1` takes 2.05× the lazy net's work at 2.84× its depth, and `fib 8` takes 1.80× and 4.85×.
  - Grafted: the reader (a rider used once, on a value nothing else holds), and the lesson that copies buy private histories. Labels on code (movement shapes and jets as facts about one code node) remain an optional speed-up.
- **Tree gas: the 26 rules as a reversible lattice gas.**
  - What it was: every block move is its own inverse, paid from a per-site energy counter. It has fuel and ash particles, and eraser, normalizer and duplicator are one fan agent with a count of 0, 1 or 2. Going back means counting the clock down.
  - Why not chosen:
    - reversal is all or nothing, on a closed lattice;
    - demand is not a bijection;
    - a clock that is its own inverse pushes toward one move per block, which costs 5–7× the clocks of random turns, against 2–2.5× for today's chip schedule;
    - speed is unmeasured.
  - Grafted:
    - per-site demons;
    - every move written with its inverse (unfold);
    - validate() checks that right-hand sides are unique and that seats and energy are conserved;
    - move choice by a hash of block and clock, for the deterministic version.
- **Loan net: room borrowed, heat spent, no log.**
  - What it was: a waiting duplicator is a loan that an eraser repays, and a pending operation is its own undo. An append primitive gives unary add.
  - Why not chosen: it gives up rewinding ordinary programs. Its one-to-one claim relies on clashing rules putting different numbers of identical demon quanta into a counter, and a counter cannot tell two pasts apart that way.
  - Grafted:
    - fuel as empty seats;
    - "repay" as the word for un-copy;
    - sizing a rule's drive by how many places its reverse could start;
    - the heat-as-a-field experiment;
    - copying ahead only where a block has room and is cool.
- **Three moves: today's rules plus a copy bit and a heat particle, run back by inverse programs.**
  - What it was: on the lattice, its inverse programs went back in 1.06× the forward clocks (`rsub` after `radd`) and 1.53× (`sub_keep` after `add_keep`).
  - Why not chosen:
    - its rules are not one-to-one: `T1·L` and `Sel·L` leave the same right side, and collecting dead work merges three cases into one;
    - the copy bit misprices copies whose twin is gone;
    - inverses are written by hand, and one silently dropped data;
    - its two-way binary adder cost 2.2× `ripple_add`'s events and erased 529 code nodes a call.
  - Grafted: the check that both sides of each rule are the same tree-calculus term, the exact copy-or-original classifier as ground truth for heat, and `add_keep` and `sub_keep` in plain disp.

## 5. Compared with what the repo has

| | where history lives | local? | parallel | going back | size |
|---|---|---|---|---|---|
| substrate (`phys=1`, substrate.rs) | records under numbers, `made_by`, per-port mark stacks, `seat_of`; bonds priced from far agents' positions | moves yes, bookkeeping no | the lattice's; 8–17% more clocks forward than plain | 10–12× the forward clocks (`add 2 3`: 90,215 against 9,188), waiting for an undo's pieces to come together | 787 lines; at the answer, 2.2–2.6× the loaded agents and 2,500–6,200 records |
| log engine (rev.rs: every rewrite an undoable event) | a global event log by number, about 2.3 used-up agents an event | no: ids and handles on the abstract net | the net's; `add 2 3` in 631 strict rounds, widest 12 | any causal order, exact (86 test terms) | 506 lines; history 15–40× the loaded program |
| two-way calculus and its machines (iso.rs; walk.rs and inet.js deleted) | the token's stack, or wires turned toward the walker; disp code through tc/tck's trace tree | data yes; calls jump across the program | 1 token, or 4–9 with tck; 9.4–51× the net's rounds | turn the token round; nothing recorded | 11 instruction kinds; the raw-tree walker net has 104 rules |
| movement agents (`views=1`) | none | yes | 0.52× rounds and 0.55× rewrites (lazy machine); 0.605× clocks on the lattice | never combined with a reversible engine | 36 more rules |
| **footprints and a reader** | the used-up pairs, wired to what they made | yes, once demand is matter | forward 0.94–1.08× the net's rounds *(model)* | 0.55–2.0× the forward rounds; read + run back leaves zero footprints | 29 rules, two law pairs, 5 reader rules; footprints 8.7–37× the program until run back |

If the core lands, two things would have nothing left to do: the substrate's record machinery, and iso.rs's tc and tck (disp run backward through a trace). rev.rs stays useful as the oracle that checks the new engine.

## 6. First experiments, in order

The models behind the *(model)* numbers are in research/reversible-core/: `footprints/` (the footprint net and today's table under one scheduler; `python run.py 'add 2 3'`), `critic/` (the reader, Bennett's cleanup and the critics' probes; `python run_bennett.py ../footprints/calls.txt`), and `riders/` (the Riders model and its runs).

1. **Port the model next to rev.rs, and make validate() stricter.**
   - Build footprints and the reader on the abstract net.
   - validate() also checks:
     - each rule's wiring is a perfect matching;
     - each (rule, mask) footprint kind is unique;
     - both sides of each rule are the same tree-calculus term;
     - seats are conserved.
   - Measure, on the 21 calls and rev.rs's 86 test terms: forward, read and back in rev.rs's strict rounds; peak agents plus footprints; exactness under random interleavings of forward and backward steps.
   - Pass: everything returns exactly, and forward stays within 1.1× rev.rs's rounds.
2. **The driver: run `bennett` and `bijection` for real.**
   - Build whatever hands values between steps.
   - Measure `bijection add_keep sub_keep (pair a b)` for a and b up to 8:
     - footprints left (want 0);
     - seats drawn (want 2a);
     - rounds (expect about 4× a plain add).
3. **Find out why the substrate is slow going back.** This is a cheap diagnostic in substrate.rs.
   - Undo one product at a time against an anchor site, instead of needing every product in one block.
   - Measure back clocks on `add 1 1`, `add 2 3`, `fib 2` and `relay_add 13 6`. Today they are 59,152, 90,215, 121,698 and 68,385, against 5,306, 9,188, 11,937 and 5,733 forward.
   - If back falls within 2× forward, the 10–12× came from undo needing many agents at once, which footprints remove. If not, products drifting apart is the cause, and the lattice half of this core needs rethinking.
4. **Inert footprints on the lattice.**
   - Leave a footprint with back strands at every rewrite, and never undo it.
   - Measure forward clocks against plain `latest`, blocked moves, and switchboard fill.
   - Try variants with 10–12 pairings, and with footprints as chains of 3-port agents.
   - Kill the design if forward goes above 1.5× the plain lattice.
5. **Demand as matter.**
   - Pulses ride their strands and are never dropped; footprints keep the wanted bits they used.
   - Measure footprints and rounds against the model's global walk, and against running eagerly.
6. **Running back on the lattice, and settling.**
   - Absorb along back strands, drive backward, bias emits by ε′, and latch the root reader.
   - Measure:
     - back clocks against forward (pass: within 2×);
     - the share of time the answer is whole (today about 4% for `add 2 3` with unbiased emits);
     - heat per commit.
7. **Heat and fuel as fields, and cleanup per call.**
   - Per-site demons. Commit leaves heat particles that drift to the edge. A reverse marker runs a call back once its result is read.
   - Measure:
     - the ledger every clock;
     - peak footprints with per-call cleanup, against cleaning up only at the end;
     - the erase rate the edge can drain.

## 7. Open questions

- **A deterministic block version.** Do direction bits, fixed seats, unfold and a hashed choice of move make every move a permutation of 2×2×2 block states over these agents? The general constructions need bigger blocks and cells that hold two states (Durand-Lose, above).
- **Smaller footprints.** Most of a footprint is implied by its rule: rev.rs found that one receipt per event is what really needs storing. Can a footprint drop wiring it can rebuild, and fit in 3 ports?
- **Sharing code without sharing history.** Riders shows that reading in place ties callers' histories together. Is there a middle way that is cheaper than today's layer-by-layer copying, such as copying a node when a second caller arrives and merging it back when that caller leaves?
- **Steps with no footprint.** Could some rules need no footprint and still mix soundly with footprint rules? Candidates are movement-style routing, and exchanges that swap a walker with a node. That would let in-place two-way code skip the run back.
- **Pebbling by energy.** Price each stored footprint so the dynamics trades room against recomputing. The graph pebble rule (keep a value while its inputs are present) is local.
- **Where inverse programs come from.** `sub_keep` was written by hand, while iso.rs's `dagger` inverts encoded two-way programs. For `bijection` to be practical, disp needs either a compiler pass for two-way definitions or a library.
- **The heat floor.** What does commit really cost, compared with the run's true loss of 24–83 loaded originals dropped without being copied?
- **Superpositions and movement agents** with footprints: untested.
- **If this core lands**, research/REVERSIBLE_PIPELINE.md's "Rules, sketched" and "What it would take, in order" become history and can be cut, along with the substrate's record machinery.