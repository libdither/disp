# Reversible and signal-based fabrics that construct, read and destroy themselves

Literature notes for the question: a fabric where signals run along wires reversibly, and a signal
reaching a cell can pass (read it), print (turn a blank into a cell of some kind) or, as print run
backward, read-and-destroy, with blanks as the resource and erasing as the only heat. What has been
built, and what each source says about reading, printing, destroying, garbage and drive.

Every quote below was fetched and matched with `scripts/verify-source.sh` (Nov 2026); the link is
the fragment it minted. Morita's 2008 survey is cited through its Wayback copy (the Hiroshima
repository link is dead).

## 1. Reversible logic elements with memory (Morita): one particle on wires

- **Cells and signal.** A reversible logic element with memory (RLEM) is a box with a few bits of
  state and k input/output lines; one particle enters, the state and the exit line are a bijective
  function of (state, entry line). The rotary element (RE) is the standard one: a bar in one of two
  orientations; a particle parallel to the bar goes straight, while "[i]f a particle comes from the
  direction orthogonal to the bar" it turns right and rotates the bar
  ([Morita 2013, p.2](https://arxiv.org/pdf/1309.1264#page=2)).
- **Universality.** "infinitely many kinds of non-degenerate RLEMs are "all" universal besides only
  four exceptions" ([p.1](https://arxiv.org/pdf/1309.1264#page=1)): any reversible sequential
  machine, hence any reversible Turing machine, is a circuit of copies of one RLEM. No fan-out:
  "fan-out of an output is not allowed" ([p.4](https://arxiv.org/pdf/1309.1264#page=4)).
- **Reading is read-and-clear.** In the construction of a reversible sequential machine from REs,
  the machine's state is "which column's bottom RE is H". An input particle first resets that RE:
  "after setting the bottom RE of the 1st column to V, the particle appears on the line q1 a2"
  ([p.4](https://arxiv.org/pdf/1309.1264#page=4)). The state is moved out of the memory into the
  particle's path, then written into a different column. That is exactly "print backward =
  read-and-destroy, then print elsewhere", with no garbage: the survey calls these circuits
  "completely garbageless", no extra input or output lines
  ([Morita 2008, p.11](https://web.archive.org/web/20170815133232id_/http://ir.lib.hiroshima-u.ac.jp/files/public/2/25576/20141016152033521657/TCS_395_101.pdf#page=11)).
- **In a reversible cellular automaton.** In Morita's 81-state reversible partitioned CA P3 a whole
  reversible counter machine runs with one signal: "computation is carried out by a single signal
  that interacts with REs and position markers"
  ([Morita 2008, p.39](https://web.archive.org/web/20170815133232id_/http://ir.lib.hiroshima-u.ac.jp/files/public/2/25576/20141016152033521657/TCS_395_101.pdf#page=39)).
  The counters are single cells (position markers) that the signal moves: a marker "can be pushed or
  pulled by one cell by giving an appropriate signal"
  ([p.37](https://web.archive.org/web/20170815133232id_/http://ir.lib.hiroshima-u.ac.jp/files/public/2/25576/20141016152033521657/TCS_395_101.pdf#page=37)).
  So a signal already modifies the layout (moves matter) reversibly; it moves a cell into empty space
  rather than printing one.
- **Garbage in gate circuits.** For gates rather than elements with memory the survey states the
  standard problem and fix: "disposing the garbage g outside of the circuit is in fact an
  irreversible process" ([p.5](https://web.archive.org/web/20170815133232id_/http://ir.lib.hiroshima-u.ac.jp/files/public/2/25576/20141016152033521657/TCS_395_101.pdf#page=5)),
  and "If we connect Φ and Φ-1 in series, garbage signals are erased reversibly"
  ([p.6](https://web.archive.org/web/20170815133232id_/http://ir.lib.hiroshima-u.ac.jp/files/public/2/25576/20141016152033521657/TCS_395_101.pdf#page=6)).
- **Cost.** A circuit has a fixed number of elements; one particle at a time, so no parallelism;
  heat zero in the model. The fabric is fixed: elements are never built or removed.

## 2. Self-reproduction in reversible cellular automata (Morita, Imai)

- Morita's survey: "Self-reproduction is also possible in RCAs. There are a 2-D reversible PCA [34]
  and even 3-D reversible PCA [15] in which various shapes of objects can self-reproduce"
  ([Morita 2008, p.40](https://web.archive.org/web/20170815133232id_/http://ir.lib.hiroshima-u.ac.jp/files/public/2/25576/20141016152033521657/TCS_395_101.pdf#page=40)).
- The 3-D version (Imai, Hori, Morita 2002, Artificial Life 8:155) abstract: "It can self-reproduce
  various patterns in three-dimensional reversible cellular space without dissipating garbage
  signals" ([PubMed abstract](https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=12171635&rettype=abstract&retmode=text); no page anchor, it is plain text).
- The mechanism is shape-encoding (self-inspection): the object is read into a gene (a command
  sequence), the gene is copied, and the gene is interpreted to build. Sayama and Nehaniv: the
  shape-encoding mechanism "was originally proposed by Morita and Imai [38] for self-replication of
  various shapes in reversible cellular automata" ([Sayama, Nehaniv 2024, p.11](https://arxiv.org/pdf/2402.03961#page=11)).
- **Not verified:** the 1996 TCS paper itself ("Self-reproduction in a reversible cellular space",
  doi:10.1016/S0304-3975(96)00083-7; ScienceDirect returns 403, no open copy found). Its abstract, as
  reported by search snippets, says encoding the shape into a gene, copying the gene and
  interpreting it "are all performed reversibly" in an 8^5-state partitioned CA (SR8). I could not
  fetch the text, so how it supplies blanks and where the gene's spent copy goes is not checked.
  Morita's book (Theory of Reversible Computing, Springer 2017) was not reachable either.

## 3. Construction-universal reversible cellular automata (Toffoli)

- Toffoli 1977 (JCSS 15:213, not fetched: Elsevier 403) is summarized in his 1980 report: for any
  cellular automaton there is "a reversible cellular automaton that realizes it", so reversible CA
  that are computation- and construction-universal exist
  ([Toffoli 1980, p.12](https://cqi.inf.usi.ch/qic/80_Toffoli.pdf#page=12)). The price is an extra
  dimension in which the history goes, and a prepared environment: "computation in reversible systems
  requires a higher degree of "predictability"" about the environment's initial conditions
  ([p.12](https://cqi.inf.usi.ch/qic/80_Toffoli.pdf#page=12)).
- The cleanest statement of "blanks are the resource, garbage is heat": "each constant input entails
  the supply of energy of predictable form, or work" and each garbage output "the removal of energy
  of unpredictable form, or heat" ([p.6](https://cqi.inf.usi.ch/qic/80_Toffoli.pdf#page=6),
  [same page](https://cqi.inf.usi.ch/qic/80_Toffoli.pdf#page=6)).

## 4. Conservative logic and the billiard ball model (Fredkin, Toffoli; Margolus)

- Claim: "ideally possible to build sequential circuits with zero internal power dissipation"
  ([Fredkin, Toffoli 1982, p.1](https://www.cs.princeton.edu/courses/archive/fall06/cos576/papers/fredkin_toffoli82.pdf#page=1)).
- Constants and garbage: an invertible gate computes AND if one is "willing to supply "constants" to
  this gate alongside with the argument, and accept "garbage"" ([p.8](https://www.cs.princeton.edu/courses/archive/fall06/cos576/papers/fredkin_toffoli82.pdf#page=8)).
  With compute, copy, uncompute "garbage can be made not only the same size as the argument" but
  equal to it ([p.19](https://www.cs.princeton.edu/courses/archive/fall06/cos576/papers/fredkin_toffoli82.pdf#page=19)).
- Fan-out is a physical act, not free wiring: "we shall require that any fan-out of signals take place
  within explicit signal-processing elements" ([p.5](https://www.cs.princeton.edu/courses/archive/fall06/cos576/papers/fredkin_toffoli82.pdf#page=5)).
  (Fan-out of a 0/1 signal onto a constant-0 line is a reversible copy; the constant is the blank.)
- Billiard balls: "a conservative-logic wire is realized as a potential ball path, as determined by
  the mirrors" ([p.16](https://www.cs.princeton.edu/courses/archive/fall06/cos576/papers/fredkin_toffoli82.pdf#page=16)).
  The mirrors are fixed and infinitely massive; balls cannot move or build them.
- Margolus's billiard-ball cellular automaton puts the mirrors in the same medium as the balls: "The
  'mirror' consists of two adjacent stable squares" of the same 1-cells that make up moving signals
  ([Margolus 1984, p.9](https://people.csail.mit.edu/nhm/bbmca-physica.pdf#page=9)). So walls and
  signals are one stuff, but nothing in the paper has signals build walls. Garbage: unwanted
  intermediate results "can be mostly 'erased' by copying the answer once you have it" and running
  back ([p.8](https://people.csail.mit.edu/nhm/bbmca-physica.pdf#page=8)).
- Where heat can live inside a reversible lattice: Creutz's trick, used by Margolus, gives each site
  a few heat-bath bits ("at every site in our 3D lattice we have added a few extra heatbath bits") and
  the result stays exactly invertible ("This heatbath CA is perfectly invertible")
  ([Margolus 1998, p.7](https://arxiv.org/pdf/comp-gas/9811002#page=7),
  [same page](https://arxiv.org/pdf/comp-gas/9811002#page=7)). Heat is then just per-cell disorder
  that has to be carried to the boundary.

## 5. Landauer and Bennett: erasing costs, copying onto a blank does not

- Landauer: logical irreversibility "requires a minimal heat generation, per machine cycle,
  typically of the order of kT for each irreversible function"
  ([Landauer 1961, p.1](https://sites.pitt.edu/~jdnorton/lectures/Rotman_Summer_School_2013/thermo_computing_docs/Landauer_1961.pdf#page=1)).
- **Copying onto a blank is free in principle.** Bennett 1973: the extra copy of the output
  "can be done reversibly if the tape used for the copy is initially blank"
  ([Bennett 1973, p.2](https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/Bennett_Reversibiity.pdf#page=2)).
  Bennett 1982: "copying onto blank tape is already a 1: 1 operation"
  ([p.20](https://www.cs.princeton.edu/courses/archive/fall06/cos576/papers/bennett82.pdf#page=20)),
  and measurement is reversible when the apparatus starts in a standard state, "like the copying of
  a bit onto previously blank tape, does not overwrite information previously stored there"
  ([p.23](https://www.cs.princeton.edu/courses/archive/fall06/cos576/papers/bennett82.pdf#page=23)).
  Bennett 2003: "Among the most important logically reversible operations are copying unknown data
  onto a blank (ie initially zero) register, and the reverse process, namely erasure of one of two
  copies of data known to be identical" ([Bennett 2003, p.2](https://arxiv.org/pdf/physics/0210005#page=2),
  [same](https://arxiv.org/pdf/physics/0210005#page=2)); copying is costly "only when they are
  conducted in such a way as to overwrite previous information" ([p.6](https://arxiv.org/pdf/physics/0210005#page=6)).
- **Un-copy is free too.** The quoted reverse process (erase one of two known-equal copies) is the
  literature name for what `rev.rs` calls un-copy.
- **What drives it forward** (Bennett 2003, p.4-5): ballistic machines run "at constant velocity
  without dissipating any energy" but must be isolated from noise
  ([p.4](https://arxiv.org/pdf/physics/0210005#page=4)); externally clocked Brownian machines have
  "dissipation per step is proportional to the driving force"
  ([p.5](https://arxiv.org/pdf/physics/0210005#page=5)); fully Brownian machines cost what is
  "determined by the weakest spring sufficient to achieve a net positive drift velocity"
  ([p.5](https://arxiv.org/pdf/physics/0210005#page=5)). The model is RNA polymerase, a "logically
  reversible copying process driven chiefly not by prior activation of the reactant, but by removal of
  one of the reaction products" ([p.4](https://arxiv.org/pdf/physics/0210005#page=4)). Bennett 1982
  quantifies: dissipation ε per step gives "a net copying speed proportional to" ε
  ([p.11](https://www.cs.princeton.edu/courses/archive/fall06/cos576/papers/bennett82.pdf#page=11)).

## 6. Brownian circuits (Peper, Lee and others)

- Tokens on wires move by thermal noise, back and forth; the circuits are delay-insensitive (any
  wire delay is fine). The point of the noise is that tokens can "search and backtrack out of
  deadlocks" ([Peper et al. 2013, p.2](https://www.cs.upc.edu/~jordicf/gavina/BIB/files/BrownianCircuits_JETC2013.pdf#page=2)),
  which lets a smaller set of primitives be universal.
- Cost of no drive: "the expected time for a token to move along a line of length L is of the order"
  L² ([p.19](https://www.cs.upc.edu/~jordicf/gavina/BIB/files/BrownianCircuits_JETC2013.pdf#page=19)).
- Drive: a ratchet is a diode on a wire; "the insertion of a ratchet results in the removal of a
  backward transition" ([p.19](https://www.cs.upc.edu/~jordicf/gavina/BIB/files/BrownianCircuits_JETC2013.pdf#page=19)),
  and ratchets spaced D apart speed a line up by L/D, but "ratchets consume energy, as shown by
  Feynman" ([p.20](https://www.cs.upc.edu/~jordicf/gavina/BIB/files/BrownianCircuits_JETC2013.pdf#page=20)).
  Ratchets must not sit where backtracking is needed. So the heat goes exactly where the circuit
  commits to a direction.
- A recent physical model of the token-conserving join with one bit of memory puts the cost of its
  periodic reset at a lower bound "remaining within a few multiples of" kT
  ([Utsumi 2025, p.1](https://arxiv.org/pdf/2505.12390#page=1)).
- Brownian cellular automata (Lee, Peper; 3 states, a few rules) embed these circuits in a fixed
  cell space; the circuits are laid out in advance, not built by tokens. Survey statement: these
  computations "are realised with particles that follow Brownian movements and which interact through
  special "gates"" ([Fatès 2014, p.14](https://arxiv.org/pdf/1406.0792#page=14)). The primary Brownian
  CA papers (J. Cell. Autom. 2010, Automata 2008) were not fetched (no open copy found).

## 7. Delay-insensitive circuits in asynchronous cellular automata, and construction there

- Peper, Lee, Adachi, Mashiko 2003 lay delay-insensitive circuits on an asynchronous cellular array
  and state the self-modifying-FPGA problem directly: the paper "leaves open the challenge of
  configuring the inherently homogeneous cellular hardware" into circuits, and names two ways:
  cells with a configuration mode ("Configuration is then achieved by rewriting the memories from cell
  to cell", as in the Cell Matrix) or "The second method uses the collective behaviour of the cells to
  copy information" from one part to another, close to self-reproduction
  ([Peper et al. 2003, p.15](https://web.archive.org/web/20170706053101id_/http://web.cecs.pdx.edu/~mperkows/CLASS_573/Asynchr_Febr_2007/t30412.pdf#page=15),
  [same](https://web.archive.org/web/20170706053101id_/http://web.cecs.pdx.edu/~mperkows/CLASS_573/Asynchr_Febr_2007/t30412.pdf#page=15),
  [same](https://web.archive.org/web/20170706053101id_/http://web.cecs.pdx.edu/~mperkows/CLASS_573/Asynchr_Febr_2007/t30412.pdf#page=15)).
- Takada, Isokawa, Peper, Matsui answered the second way: a universal constructor on a self-timed
  (asynchronous) CA (ACRI 2004, IJMPC 2006), then a purely asynchronous one. Fatès' survey: they show
  "the existence of an asynchronous, rotation-symmetric rule with 15 states" and von Neumann
  neighbourhood with universal construction and computation ([Fatès 2014, p.12](https://arxiv.org/pdf/1406.0792#page=12)).
  Computation universality there is shown by building Morita's rotary element. **Not verified from
  the primary text**: Takada et al. JCSS 72:1368 (2006) and Physica D 227:26 (2007, self-reproducing
  loops with arbitration) are behind ScienceDirect 403s; the snippet-level claim (6 states for
  computing, 9 for construction) is unverified. These rules are not reversible.

## 8. Von Neumann's constructor and its descendants: what reads, what writes

- **Von Neumann / Pesavento.** Pesavento's 1995 implementation (32-state variant of von Neumann's 29
  states). Cells are blank, ordinary or special transmission, or confluent. Printing: "A pattern of
  five bits (excitations) injected into a cell in the blank state" turns it into one of the nine
  quiescent kinds; destroying: an excitation from a special into an ordinary element (or back) "results
  in a transition to the blank state" ([Pesavento 1995, p.11](https://fab.cba.mit.edu/classes/865.18/replication/Pesavento.pdf#page=11),
  [same](https://fab.cba.mit.edu/classes/865.18/replication/Pesavento.pdf#page=11)). Destroy is
  irreversible here: the cell's kind is lost, not carried off.
  **Reading is destructive and then repaired**: "Because bit reading is a destructive operation (as it
  implies that the bit read is always replaced by a 1) the information coming from the discriminator
  is used also to restore the tape information" ([p.5](https://fab.cba.mit.edu/classes/865.18/replication/Pesavento.pdf#page=5),
  [same](https://fab.cba.mit.edu/classes/865.18/replication/Pesavento.pdf#page=5)).
  **Self-modification is the control mechanism**: build and kill "are mainly used to deviate the
  traffic of information" by re-orienting switch cells ([p.11](https://fab.cba.mit.edu/classes/865.18/replication/Pesavento.pdf#page=11)),
  and a gate is a confluent cell plus "an apparatus that creates and destroys a transmission element
  pointing toward it" ([p.13](https://fab.cba.mit.edu/classes/865.18/replication/Pesavento.pdf#page=13)).
  The tape (description) is cells of the same medium; the constructor reads it with an extensible
  loop and builds in a blank region with a construction arm. Cost: the full machine is thousands of
  cells and replication takes on the order of 10^10 steps (Wikipedia table; not checked against the
  paper).
- **Codd, Langton.** Langton's account of Codd: wires are core cells in a sheath; signals are
  sequences; the sequence 7-0-6-0 at a wire's capped end "results in the data-path being extended by
  one cell", and there is a "similar signal sequence for retracting the data-path by one cell"
  ([Langton 1984, p.4](https://www-users.york.ac.uk/~gt512/BIC/langton84.pdf#page=4),
  [same](https://www-users.york.ac.uk/~gt512/BIC/langton84.pdf#page=4)). So print and destroy happen
  only at the tip of a wire: the construction arm, scanned over an empty area, "setting the cells it
  passes over to the proper states" ([p.4](https://www-users.york.ac.uk/~gt512/BIC/langton84.pdf#page=4)).
  Fan-out is free and irreversible: at a T-junction "signals are duplicated at the branching points"
  ([p.4](https://www-users.york.ac.uk/~gt512/BIC/langton84.pdf#page=4)). The tape is used twice, as
  instructions and as "uninterpreted data, which must be copied"
  ([p.2](https://www-users.york.ac.uk/~gt512/BIC/langton84.pdf#page=2)). Langton's loop drops
  universality and keeps the description circulating: "storing its description in a dynamic "loop",
  rather than on a static "tape"" ([p.1](https://www-users.york.ac.uk/~gt512/BIC/langton84.pdf#page=1)).
- **Evoloops (Sayama).** Destruction as garbage collection: an undefined local situation
  "would generate a "dissolving" state that would propagate through contiguous active states" and
  erase them, and "Turn-over of generations is made possible in this way by freeing up space"
  ([Sayama, Nehaniv 2024, p.10](https://arxiv.org/pdf/2402.03961#page=10),
  [same](https://arxiv.org/pdf/2402.03961#page=10)). Blanks are recycled by irreversible erasing.
- **Hutton's Squirm3.** Not a cellular automaton but bonded atoms moving on a grid with local
  reactions; atoms have a fixed type and a state. A chain replicates by template, "drawing on a random
  soup of atoms in state 0" ([Hutton 2002, p.5](https://faculty.cc.gatech.edu/~turk/bio_sim/articles/hutton_rep_molecules.pdf#page=5));
  when blanks of a needed type run out, copies stall: they "are unable to replicate further because
  there aren't any e0's left" ([p.6](https://faculty.cc.gatech.edu/~turk/bio_sim/articles/hutton_rep_molecules.pdf#page=6)).
  This is "blanks are the resource" made literal, and copies are typed: a blank must already be the
  right kind of atom.
- **Nobili-Pesavento 1996** ("Generalised von Neumann's automata"): no open copy found; not verified.

## 9. Tokens on a fixed graph vs. rewriting the graph (GoI machines, reversible combinators)

- **Danos and Regnier.** The interaction abstract machine (IAM, from geometry of interaction) runs a
  token whose word is changed by the fixed net it travels through. It is bi-deterministic: "no two
  transitions apply, nor could two transitions have pushed the word there. So that the mechanism is
  reversible", and the word "will sometimes grow and sometimes shrink. It needs no external
  garbage-collection" ([Danos, Regnier 1999, p.4](https://www.pure.ed.ac.uk/ws/portalfiles/portal/16874549/Reversible_irreversible_and_optimal_machines.pdf#page=4),
  [same](https://www.pure.ed.ac.uk/ws/portalfiles/portal/16874549/Reversible_irreversible_and_optimal_machines.pdf#page=4)).
  Krivine's machine, which jumps instead, "has to call for an external garbage-collection mechanism
  to dispose of the obsolete information" ([p.4](https://www.pure.ed.ac.uk/ws/portalfiles/portal/16874549/Reversible_irreversible_and_optimal_machines.pdf#page=4)).
  This is the pass-only fabric: reversible because it never prints or destroys; its cost is
  recomputation.
- **Dynamic GoI machine (Muroya, Ghica).** "GoI keeps the underlying graph fixed and uses the data
  stored in the token to route it"; the DGoIM "interleaves token passing with graph rewriting", the
  token triggering rewrites where it goes; with no rewrites it is "an ordinary token-passing abstract
  machine" ([Muroya, Ghica 2019, p.2](https://arxiv.org/pdf/1803.00427#page=2),
  [same](https://arxiv.org/pdf/1803.00427#page=2), [p.3](https://arxiv.org/pdf/1803.00427#page=3)).
  This is the closest published model of "a signal that rewrites the net it travels on"; it is not
  reversible and not local in the lattice sense.
- **Reversible interaction systems (Salikhmetov).** An interaction system is reversible when "two
  active pairs never reduce to overlapping nets and any net can be the result of at most one active
  pair" ([Salikhmetov 2018, p.4](https://arxiv.org/pdf/1806.07275#page=4)); with all rules
  reversible, no two rules may have the same total arity. Whether a universal reversible system
  exists is left open: "One may wonder if there exists a reversible equivalent of interaction
  combinators" ([p.6](https://arxiv.org/pdf/1806.07275#page=6)).
- **Reversible combinatory logic (Di Pierro, Hankin, Wiklicky).** Starts from "beta reduction –
  destroys the redex" ([p.1](https://www.doc.ic.ac.uk/~herbert/papers/mscs06.pdf#page=1)) and makes
  CL reversible by pairing each term with a history: "H is a list of elements which record the
  reduction steps" ([p.5](https://www.doc.ic.ac.uk/~herbert/papers/mscs06.pdf#page=5)). Same shape
  as `rev.rs`: the history is the garbage.
- **Abramsky.** Compiles linear combinatory logic into reversible pattern-matching automata
  (partial involutions on trees); "The size of the automaton is linear in the size of the functional
  term" ([Abramsky 2005, p.1](https://arxiv.org/pdf/1111.7154#page=1)). Again a fixed automaton plus
  moving data, like the IAM.

## What this means for the sketch

The sketch: a signal meeting a cell can pass (read: leave the cell, note its kind), print (meet a
blank, leave a cell of a kind), and print run backward is read-and-destroy; a read that keeps the
cell is a copy and needs a blank; blanks are the resource and erasing is the only heat.

- **Pass (read and keep).** Confirmed with a correction: noting the kind in the signal is itself a
  copy into the signal, so the signal must carry a blank slot for it. This is Bennett's reversible
  measurement: fine "provided the measuring apparatus ... is in a standard state" (Bennett 1982, p.23
  above). A signal that already holds a note cannot read again without first giving the note up.
  Morita's rotary element shows the other option: a particle reads a 1-bit memory by being steered
  by it, and in one of the two cases the read rotates the bar, so the read changes the cell unless the
  circuit puts it back.
- **Print backward = read-and-destroy.** Confirmed, and it is reversible only because the signal
  leaves carrying the kind. Morita's reversible sequential machines read state exactly this way
  (reset the RE, carry the state on the particle's path, write it elsewhere). In von Neumann's
  irreversible rule, by contrast, kill drops the kind (heat), and reading is destructive and must
  be repaired (Pesavento p.5). So the sketch's print/unprint pair is the reversible version of
  von Neumann's build/kill pair.
- **Read that keeps the cell needs a blank.** Confirmed (Bennett 1973/1982/2003). Two extensions:
  (1) the inverse, destroying one of two cells known to be equal, is also free (Bennett 2003 p.2;
  `rev.rs`'s un-copy); (2) in Squirm3 copies need blanks of the right type and stall when they run out,
  so a typed blank supply is a real design choice.
- **Blanks are the resource, erasing is the only heat.** Confirmed as stated by Toffoli 1980 (constants
  are work, garbage is heat) and Landauer. Correction: erasing is the only *necessary* heat; driving
  the fabric forward at nonzero speed also costs heat, proportional to speed (Bennett 1982: copying
  speed proportional to dissipation per step), and every ratchet costs energy (Peper et al. 2013).
- **What drives a reversible fabric forward.** Three answers in the literature: ballistic (billiard
  balls; isolated, no noise allowed, fixed mirrors), externally clocked (cost proportional to speed),
  or Brownian with a weak bias (ratchets placed where no backtracking is needed; without them a token
  takes L² time on a wire of length L). A self-modifying fabric run by noise needs ratchets at commit
  points (finishing a print, sending a finished machine off), which is where the heat goes.
- **Where garbage goes.** Four options in the sources: run back after copying the answer
  (Bennett; Fredkin-Toffoli Φ then Φ⁻¹; Margolus); design elements so there is none (Morita's
  garbageless RE circuits, the IAM's token that needs no garbage collection); push it into per-cell
  heat-bath bits that must be pumped to the boundary (Creutz/Margolus); or erase it irreversibly to
  recycle blanks (evoloop dissolving state). A self-modifying fabric that keeps building will run out
  of blanks unless some region is erased (heat) or uncomputed (time).
- **Extensions the sketch lacks.** (a) Moving a cell (Morita's position markers) is a third reversible
  primitive beside print and unprint, and it needs empty space but no new blank. (b) In every working
  constructor (Codd, Langton, von Neumann, Takada), printing happens only at the tip of an arm; a
  signal does not print beside an arbitrary wire. (c) Fan-out of signals must be an explicit element
  (Fredkin-Toffoli), while Codd/Langton T-junctions copy for free, which is irreversible. (d) Using
  build/kill for control (von Neumann's gates and switches) means self-modification is not a separate
  feature: it is how the constructor itself routes signals.
- **Gap.** No source found combines all three: reversible local rule, signals that print and unprint
  cells of the same medium the signals run in, and a universal constructor. Morita-Imai comes closest
  (reversible, self-reproducing, no garbage dissipated) but builds by a fixed reproduction scheme, and
  its text was not verifiable here. The interaction-net side is open: a universal reversible
  interaction system is posed as an open question (Salikhmetov), and the DGoIM's token-guided rewriting
  is irreversible.

## Searched, not found or not verified

- Morita and Imai 1996 (TCS 168:337), Imai-Morita ICES 1996 (LNCS 1259), Morita-Imai Artificial
  Life V 1997: no open copy; ScienceDirect and Springer return 403 or a challenge page. Covered only
  by Morita's 2008 survey, the 3-D abstract on PubMed and Sayama's survey.
- Morita, Theory of Reversible Computing (Springer 2017): no open chapters found.
- Toffoli 1977 (JCSS 15:213): Elsevier 403; covered by Toffoli 1980.
- Takada et al. JCSS 2006, IJMPC 2006, Physica D 2007; Lee-Adachi-Peper "Reliable self-replicating
  machines in asynchronous cellular automata" (Artificial Life 2007); Peper et al. "Self-timed cellular
  automata" (FGCS 2002); Lee, Peper et al. "On reversible computation in asynchronous systems" (a
  3-line reversible delay-insensitive element); Brownian cellular automata (J. Cell. Autom. 2010): all
  behind publisher pages, no open copy found. Only Fatès' survey statements were verified.
- Nobili-Pesavento 1996: no open copy found.
- "Reversible interaction nets" as a title or topic (WebSearch, Nov 2026): only Salikhmetov's
  upward-confluence paper treats reversibility of Lafont-style interaction systems; other hits are
  reversible Petri nets and event structures (different formalism). No paper found where tokens in an
  interaction net print or erase agents reversibly.
- A reversible cellular automaton in which signals build and erase circuit elements (a reversible
  construction arm) as a stated, general mechanism: not found beyond Morita-Imai's self-reproduction
  and Toffoli's existence result.
- Searches used WebSearch; the arXiv API was rate-limited during this session.
