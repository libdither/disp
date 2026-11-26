# Graphs that read, copy or rebuild themselves with local rules

How to read these notes.

- Every quote was found by `scripts/verify-source.sh` (about 85 checked; one planned quote missed and was
  dropped). The link is the fragment the script printed. "PDF p." is the PDF page, not the printed one.
- *Inference* marks reasoning no source states.
- Not repeated here (see `research/research_notes/Reading code in interaction nets/graph_and_net_introspection.md`):
  port-graph rewriting (PORGY), rules as port graphs, hierarchical graphs, binets, nets-within-nets,
  reflective Petri nets, GP 2, tree calculus, HVM2, Mogensen quoting, token machines, Lafont's menus and codes.
- Words. *Local rule*: a rewrite that looks at a bounded piece of the graph. *Pairwise* or *binary* rule:
  looks at two adjacent nodes (an interaction-net active pair is one). *Cover* of a graph G: a bigger graph
  that maps onto G so that every node's neighbourhood maps one-to-one (think: G unrolled; two copies of a
  triangle glued into a hexagon cover the triangle). *View* or *universal cover*: the infinite tree you get by
  unrolling G from one node, following every walk. *Minimum base*: the smallest graph with the same views.

## 1. What a local process can learn about the graph it runs on

### Boldi and Vigna, fibrations of graphs (2002)

Graph: directed, coloured multigraph; processors are nodes, all running the same program. Read: a processor
learns only by exchanging with neighbours. Main fact: it can learn its view (the unrolled tree), never more.

- Same view, same state, always: "processors with the same universal total graph always ... remain in the
  same state" — [Boldi–Vigna, PDF p.15](https://vigna.di.unimi.it/ftp/papers/FibrationsOfGraphs.pdf#page=15)
- Stated as a limit on all algorithms: "no algorithm will ever be able to drive processors having the same
  universal total graph into distinct states" — [PDF p.16](https://vigna.di.unimi.it/ftp/papers/FibrationsOfGraphs.pdf#page=16)
- And the view is learnable, level by level: the algorithm "consists in reading (at step k) from all
  neighbours their universal total graph truncated at depth k" — [PDF p.16](https://vigna.di.unimi.it/ftp/papers/FibrationsOfGraphs.pdf#page=16)
- Finite depth suffices (n = number of nodes): "two nodes x and y have the same universal total graph
  (rooted covering, rooted symmetric covering) iff" they agree to depth n−1 —
  [PDF p.15](https://vigna.di.unimi.it/ftp/papers/FibrationsOfGraphs.pdf#page=15)
- The best a local process can rebuild is the minimum base, which is bisimulation-equivalent to the graph:
  "any graph G is strongly bisimilar to its minimum base" — [PDF p.19](https://vigna.di.unimi.it/ftp/papers/FibrationsOfGraphs.pdf#page=19)
- With ports told apart (as in interaction nets), symmetry breaks exactly when the graph covers nothing
  smaller: "the networks admitting an election algorithm with the assumption of distinguished outcoming links
  are exactly those whose underlying graph is covering prime" — [PDF p.11](https://vigna.di.unimi.it/ftp/papers/FibrationsOfGraphs.pdf#page=11)

Same idea for port-numbered graphs (the interaction-net case): "The view of a node in a port-labeled network
is an infinite tree encoding all walks in the network originating from this node" —
[Dereniowski–Kosowski–Pająk 2014, p.1](https://arxiv.org/pdf/1407.2511#page=1). Rebuilding the whole graph
("topology recognition") means each node must "deterministically produce an isomorphic copy of the underlying
graph"; "This task is usually unfeasible without any a priori information" —
[Fusco–Pelc–Petreschi 2016, p.1](https://arxiv.org/pdf/1601.06040#page=1). The standard counterexample is "the
class of oriented rings in which ports at each node are numbered 0,1 in clockwise order" —
[p.2](https://arxiv.org/pdf/1601.06040#page=2).

### Chalopin and Métivier, local computations on edges (2010)

This is the closest published model to an interaction-net active pair. Graph: labelled simple graph. Rule:
picks one edge and rewrites the labels of its two ends together — "local computations on labelled edges
(which allow two adjacent vertices to synchronize" —
[Chalopin–Métivier 2010, p.1](https://pageperso.lis-lab.fr/~jeremie.chalopin/publis/CM-DC10.pdf#page=1). Rules
relabel only; they do not add nodes or rewire.

- The limit is again coverings. Angluin's lifting lemma, restated for edge rules: "Lemma 10 (Lifting Lemma [3])
  Let R be a locally generated relabelling relation on labelled edges" (any run on a graph lifts to a run on
  each of its covers) — [p.8](https://pageperso.lis-lab.fr/~jeremie.chalopin/publis/CM-DC10.pdf#page=8)
- Result: "there exists an election algorithm for G if and only if G is minimal for the covering relation" —
  [p.3](https://pageperso.lis-lab.fr/~jeremie.chalopin/publis/CM-DC10.pdf#page=3); minimal graphs are those
  "which can cover only themselves" — [p.4](https://pageperso.lis-lab.fr/~jeremie.chalopin/publis/CM-DC10.pdf#page=4)
- Cost: the positive side is an enumeration (naming) algorithm that gives every node a distinct number and
  then builds the map; it is not a constant-step read.

### Klavins, Ghrist and Lipsky, graph grammars for self-assembly (2004)

Graph: simple labelled graph of robots or particles. Rule: a small labelled graph on the left (one, two or
three nodes) rewritten to another on the right, applied wherever it matches, concurrently. Rules are not part of
the graph.

- Pairwise rules build trees, not unique cycles: "to generate an acyclic graph we synthesize a binary grammar
  (rules involve at most two parts)", and "for a general graph we synthesize a ternary grammar (rules involve at
  most three parts)" — [Klavins–Ghrist–Lipsky 2004, p.1](https://www2.math.upenn.edu/~ghrist/preprints/icra2004.pdf#page=1)
- The reason is covers. "A rule is acyclic if its right hand side contain no cycles" —
  [p.2](https://www2.math.upenn.edu/~ghrist/preprints/icra2004.pdf#page=2). Theorem 3.1: "Let Φ denote an
  acyclic rule set. Then any cover of a graph in the reachable set" is also reachable —
  [p.3](https://www2.math.upenn.edu/~ghrist/preprints/icra2004.pdf#page=3); "The proof of Theorem 3.1 is
  topological, utilizing tools from covering space theory" — [p.2](https://www2.math.upenn.edu/~ghrist/preprints/icra2004.pdf#page=2);
  "Theorem 3.1 implies that binary rules are insufficient to construct a unique stable cycle" —
  [p.3](https://www2.math.upenn.edu/~ghrist/preprints/icra2004.pdf#page=3)

*Inference:* this is the structural-rewrite version of Boldi–Vigna. A rule that sees two nodes cannot tell a
cycle of length n from a cycle of length 2n, so it cannot settle "do these two wires end at the same node".

### Klavins, universal self-replication with graph grammars (2004)

Pairwise rules copy any labelled strand from a seed. "A graph grammar is described that can be used to
replicate arbitrarily labeled strands of particles (modeled as linear graphs)"; "The rules of the grammar
dictate how pairs of particles should attach or disassociate upon random collisions" —
[Klavins 2004, p.1](http://faculty.washington.edu/klavins/papers/klavins-icmens04.pdf#page=1). The description
lives in the graph, not the rules: "the desired assembly is encoded in the rules of the system, whereas in the
later problem, the desired assembly is encoded as a seed assembly" — [same](http://faculty.washington.edu/klavins/papers/klavins-icmens04.pdf#page=1).
The copy is a template copy (DNA-like), and only of strands (linear graphs), consistent with the cover theorem.

## 2. Graph-rewriting automata (Tomita, Kurokawa, Murata)

Graph: every node has exactly three links, in a fixed cyclic order: "3-link planar graph" —
[Tomita–Kurokawa–Murata 2006 slides, PDF p.5](https://www.wolframscience.com/conference/2006/presentations/materials/tomita.pdf#page=5);
"Link order at each node" — [PDF p.9](https://www.wolframscience.com/conference/2006/presentations/materials/tomita.pdf#page=9).
The link order plays the role of ports. Nodes carry a finite state.

Rules (local, finite, written in a regular table like a cellular automaton): node rules `trans` (change state
from own state plus the three neighbours' states) and `div` (split a node into three); link rules `com`
(rewire across a link, chosen by the two end states) and `anh` (delete the two nodes at a link's ends).
A link rule looks at the two nodes joined by one link, which is the shape of an interaction-net active pair.
Scheduling is global and synchronous: "link rules (com, anh) at odd time", with lateral inhibition to "suppress
neighbor link rule activation" — [PDF p.13](https://www.wolframscience.com/conference/2006/presentations/materials/tomita.pdf#page=13).
An asynchronous variant needs extra states for local synchronization: "Execution of structural change can be
detected by neighbors" — [PDF p.35](https://www.wolframscience.com/conference/2006/presentations/materials/tomita.pdf#page=35).

Self-description (journal abstract, Artificial Life 2007): "Structural change is possible along with state
transition"; self-replication "is performed using a construction arm, which is realized as a subgraph, that
executes a program described in the graph structure"; and "a metanode structure is introduced to embed rule
sets in the graph structure as self-description for computation" —
[Tomita–Murata–Kurokawa 2007, PubMed abstract](https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=17716018&rettype=abstract&retmode=text).
The slides call it von Neumann style: "Self-replication with translation/transcription" of an encoded program —
[PDF p.38](https://www.wolframscience.com/conference/2006/presentations/materials/tomita.pdf#page=38). Cost:
a universal Turing machine that reproduces needs "30 states, 955 rules (for reproducing)" (plus 745 for
computing) — [PDF p.34](https://www.wolframscience.com/conference/2006/presentations/materials/tomita.pdf#page=34).

How the description is read: by a construction arm (a subgraph) moving along an encoded program, i.e. a tape
inside the graph. The rule set itself is not inspected by rules; the metanode stores rules as a structure that
a fixed rule set interprets. Reversibility: none mentioned in the verified material. Not verified: the metanode
layout and how the arm reads branching structure (body of the ECAL 2005 and ALife 2007 papers was not reachable).

## 3. Chemlambda, directed interaction combinators, chemSKI (Buliga)

Graph: port graphs of λ-calculus or combinator nodes plus fan-in/fan-out nodes; rewrites "of the kind
encountered in Interaction Combinators" — [Buliga 2020, p.1](https://arxiv.org/pdf/2005.06060#page=1).
Chemlambda's selling point: "all interactions are purely local in space and time" —
[Buliga–Kauffman 2014, p.1](https://arxiv.org/pdf/1403.8046#page=1).

- Copying is structural, never a read. In graphic λ-calculus copying was one non-local move; chemlambda replaces
  it: "this self-multiplication is done via the move GLOBAL FAN-OUT, but chemlambda has only local moves" —
  [p.4](https://arxiv.org/pdf/1403.8046#page=4). The fan-out node walks through the molecule; "The sequence of
  moves depends on the combinator molecule" — [p.4](https://arxiv.org/pdf/1403.8046#page=4). Cost: one rewrite
  per node copied.
- A quine here is a graph that rewrites back to itself, not one that prints its description: "a graph G is a
  quine graph if it has a non-void maximal collection of non-conflicting matches", such that "after the parallel
  application of the rewrites we obtain a graph isomorphic with G" — [Buliga 2020b, p.1](https://arxiv.org/pdf/2003.14332#page=1);
  "a graph quine is a molecule which admits a periodic evolution under graph rewrites" —
  [Buliga 2020, p.4](https://arxiv.org/pdf/2005.06060#page=4).
- Interaction-combinator quines exist: "We can experiment with 10 chemlambda quines and with 5 Interaction
  Combinators quines" — [p.4](https://arxiv.org/pdf/2005.06060#page=4), "starting with Lafont' quine, which
  appears in the foundational article" — [p.5](https://arxiv.org/pdf/2005.06060#page=5); "In the case of an IC
  quine, the reduction does not terminate, it is periodic" — [p.5](https://arxiv.org/pdf/2005.06060#page=5).
- Locality is enforced on the simulator itself: the machine may only "explore from a node only a limited portion
  of the graph" — [p.2](https://arxiv.org/pdf/2005.06060#page=2).

Nothing in chemlambda reads a molecule. Rules are not part of the graph.

## 4. Artificial chemistries over λ and combinators

### Fontana and Buss, AlChemy (1994)

Molecules are λ-terms; "the collision of molecules generates specific new molecules" —
[Fontana–Buss 1994, PDF p.1](https://web.archive.org/web/20250102042623id_/https://scholar.harvard.edu/files/walterfontana/files/arrival.pdf#page=1).
A molecule acts on another by being applied to it, so interactions are not given from outside: the system
"specifies the interactions among objects not externally, but rather internally to the objects as a function of
their structure" — [PDF p.3](https://web.archive.org/web/20250102042623id_/https://scholar.harvard.edu/files/walterfontana/files/arrival.pdf#page=3).
"Collision between A and B creates a new function" (A applied to B, then normalised) —
[PDF p.13](https://web.archive.org/web/20250102042623id_/https://scholar.harvard.edu/files/walterfontana/files/arrival.pdf#page=13).
A reaction is a whole normalisation, not a local step, with hard caps: "the space limit is 4000 characters, and
the time limit is 10,000 reduction steps"; "If no normal form is produced within these limits, the collision is
elastic" — [PDF p.16](https://web.archive.org/web/20250102042623id_/https://scholar.harvard.edu/files/walterfontana/files/arrival.pdf#page=16).
The simplest organisations are copiers: "Level 0 is defined by selfcopying objects" —
[PDF p.1](https://web.archive.org/web/20250102042623id_/https://scholar.harvard.edu/files/walterfontana/files/arrival.pdf#page=1).
A λ-term can only apply its partner, never inspect it.

### Kruszewski and Mikolov, Combinatory Chemistry (2020)

Molecules are SKI terms in a multiset. To conserve mass, S does not copy: plain combinatory logic would because
"S duplicates its third argument x", so a reaction instead needs "a copy of its third argument x (the reactant)
must also be present in the multiset P" — [Kruszewski–Mikolov 2020, p.3](https://arxiv.org/pdf/2003.07916#page=3).
*Inference:* checking that an equal copy is in the soup is a whole-term equality test, a global read. Copying is
outsourced to finding an identical molecule.

### Stringmol (Hickinbotham, Stepney and others)

Molecules are strings that are both program and data: "the machine acts as the blueprint for itself" —
[Stringmol spec 0.2, PDF p.3](https://www.cs.york.ac.uk/library/reports/2010/YCS/458/YCS-2010-458.pdf#page=3).
Binding is by approximate string alignment, then one molecule runs its code over both strings: "Copying is a
bi-molecular process in which one partner acts as the template and one acts as the copier" —
[PDF p.4](https://www.cs.york.ac.uk/library/reports/2010/YCS/458/YCS-2010-458.pdf#page=4). Reading is by
pointers: "There are four pointer types: instruction" (flow, read, write), and each molecule has two pointer
sets: "One set, p1 is used to interact with the string of the molecule itself", the other with the partner —
[PDF p.15](https://www.cs.york.ac.uk/library/reports/2010/YCS/458/YCS-2010-458.pdf#page=15). This is the one
chemistry here where a molecule truly reads another, and it does so because molecules are strings walked by a
head, not graphs.

### Chemical Abstract Machine (Berry and Boudol, 1990)

States are multisets of molecules; "a magical mechanism stirs the solution"; "membranes that force reactions to
occur locally" — [Berry–Boudol 1990, p.1](https://www.lix.polytechnique.fr/~fvalenci/papers/cham.pdf#page=1).
Molecules react by rules matched on their syntax (terms of a process calculus), so rules read structure, but
they are fixed outside the solution. No self-description.

## 5. Walkers: one token reading a graph

- A single finite-state walker cannot even visit every graph: "No finite automaton with a finite number of
  pebbles can explore all graphs" (Rabin, 1966) — [Fraigniaud–Ilcinkas slides 2004, PDF p.18](https://www.labri.fr/perso/ilcinkas/slides/MFCS_2004.pdf#page=18);
  "No finite set of finite automata can cooperatively perform exploration of all cubic planar graphs" (Rollik) —
  [PDF p.19](https://www.labri.fr/perso/ilcinkas/slides/MFCS_2004.pdf#page=19); for a K-state robot there is a
  planar graph "with at most K+1 nodes, that the robot cannot explore" — [PDF p.20](https://www.labri.fr/perso/ilcinkas/slides/MFCS_2004.pdf#page=20)
- Even on trees, a walker is weaker than a case that branches at every node: there is "a tree language which is
  regular but not recognized by any (nondeterministic) tree-walking automaton" —
  [Bojańczyk–Colcombet 2005, p.1](https://www.mimuw.edu.pl/~bojan/papers/twareg.pdf#page=1)
- Walkers can be made reversible cheaply: Kunc and Okhotin showed an n-state graph-walking automaton over k
  edge labels "can be transformed to a returning GWA with 3nk states and to a reversible GWA with 6nk + 1 states"
  — [Martynova–Okhotin 2020, p.2](https://arxiv.org/pdf/2011.14856#page=2)

## 6. Matching a pattern in a graph: local or not

- General DPO/SPO-style matching is not local: "to match the left-hand graph L of a rule within a host graph G
  requires" time size(G)^size(L) — [Bak–Plump 2012, p.3](https://eprints.whiterose.ac.uk/id/eprint/195067/1/BakPlump.GraBaTs.12.pdf#page=3)
- Roots make it local: "This enables graph transformation rules to be matched in constant time, provided that
  host graphs have a bounded node degree" — [same](https://eprints.whiterose.ac.uk/id/eprint/195067/1/BakPlump.GraBaTs.12.pdf#page=3).
  *Inference:* an interaction net is the extreme rooted system: the root is a principal port, the pattern is two
  nodes, the degree is fixed.
- Causal graph dynamics (Arrighi–Dowek) define locality as a function of a disk: the "pointed graph whose graph
  is the induced subgraph of G around the neighbours of radius r of A" — [Arrighi–Dowek 2012, p.7](https://arxiv.org/pdf/1202.1098#page=7).
  Nodes have ports and anonymous names: "the names of the vertices are somewhat immaterial, they can be renamed,
  unlike states and ports" — [p.5](https://arxiv.org/pdf/1202.1098#page=5). *Inference:* a disk of radius r is
  an induced subgraph, so it does show which wires meet at one node within distance r. That is strictly more than
  an interaction-net rule sees (two nodes, with no knowledge of where their other wires go).

## 7. "Case" on data and on binding structures

### Pure pattern calculus (Jay and Kesner, 2006)

Case on any structure by "allowing any term to be a pattern" —
[Jay–Kesner 2006, p.1](https://www.irif.fr/~kesner/papers/purepattern.pdf#page=1). Its data model is the one
tree calculus also uses: "Every data structure is either an atom or a compound" —
[p.1](https://www.irif.fr/~kesner/papers/purepattern.pdf#page=1); "Path polymorphism supports searches or
queries along all paths through an arbitrary data structure" — [p.1](https://www.irif.fr/~kesner/papers/purepattern.pdf#page=1).
Data structures are trees (applications of constructors); binders are not data.

### Locally nameless terms (Charguéraud, 2012)

The standard way to take a binder apart: bound variables are de Bruijn indices, free ones are names. The
representation "relies on de Bruijn indices to represent bound variables but uses names to represent free
variables" — [Charguéraud 2012, p.1](https://chargueraud.org/research/2009/ln/main.pdf#page=1). Taking a λ
apart is "opening": "Variable opening turns some bound variables into free variables" —
[p.5](https://chargueraud.org/research/2009/ln/main.pdf#page=5). The case is then on a tree with leaves
`bvar i | fvar x`, and the fresh name is the marker.

### Director strings (Kennaway and Sleep; Fernández, Mackie and Sinot)

Each binary node records, per variable in scope, where that variable goes: "Terms are annotated with
information - directors - that indicate how substitutions should be propagated" —
[Fernández–Mackie–Sinot 2005, p.1](https://lsv.ens-paris-saclay.fr/Publis/PAPERS/PDF/FMS-aaecc05.pdf#page=1).
The four directors are the routing combinators: "S x y takes an argument and directs it to both x and y" (B to
the right only, C to the left only, K nowhere) — [p.5](https://lsv.ens-paris-saclay.fr/Publis/PAPERS/PDF/FMS-aaecc05.pdf#page=5);
"Director strings were introduced by Kennaway and Sleep [22] for combinator reduction" —
[p.2](https://lsv.ens-paris-saclay.fr/Publis/PAPERS/PDF/FMS-aaecc05.pdf#page=2). Per variable the shapes are
finite (four letters); the string at a node grows with the number of variables in scope.

### Ordered nameless terms (Abel and Kraus, 2011)

The binder knows its occurrences: "number and relative position of all occurrences of the bound variable are
stored", and "application carries the additional information where to cut the variable context into function
and argument part" — [Abel–Kraus 2011, p.1](https://arxiv.org/pdf/1111.0085#page=1). Every node holds a number,
so shapes are not finite.

### λ-graphs with jumps (Accattoli and Kesner, 2012; Accattoli and Guerrini, 2009)

Scope boxes are replaced by extra edges: "It denies boxes by representing them with additional edges called
jumps" — [Accattoli–Kesner 2012, p.2](https://lmcs.episciences.org/847/pdf#page=2). The rules branch on the
number of occurrences of the substituted variable, three cases, which "correspond, respectively, to
weakening-box, dereliction-box and contraction-box" (0, 1, more than 1 occurrences) —
[p.2](https://lmcs.episciences.org/847/pdf#page=2). The rules act at a distance: "regardless the distance between
the involved constructors" — [p.2](https://lmcs.episciences.org/847/pdf#page=2).

### Lafont (already covered)

Universality is a compile-time translation: "Any interaction system can be translated into the system of
interaction combinators" — [Lafont 1997, p.82](https://dcreager.net/pdf/Lafont1997.pdf#page=14). The open copy
is the dcreager.net mirror of the journal article; the author's page has only PostScript. One more fact relevant
here: a cycle of principal ports is invisible to rules: "Clearly, a vicious circle can never be eliminated,
because no" cell in it can interact first — [p.79](https://dcreager.net/pdf/Lafont1997.pdf#page=11).

## What this means for the project

- *Inference:* "a local rewrite cannot tell which wires lead to the same node" is a theorem, not a design flaw.
  Angluin's lifting lemma, Boldi–Vigna's fibrations, Chalopin–Métivier for edge rules, and Klavins' Theorem 3.1
  for structural pairwise rules all say: a process made of local rules cannot tell a graph from its covers. It
  learns the unrolled tree (the view) and nothing more. An interaction-net rule is such a process.
- *Inference:* the escape is a distinguished node. A graph with one marked node covers nothing smaller (a cover
  would have several marks), so everything becomes learnable, by naming: spread distinct marks out and compare
  them. The project's "opening" (send a fresh marker down the binder's wire) is the cheapest instance of this,
  one name per binder, and locally nameless "variable opening" is the same move in term syntax.
- *Inference:* trees read cleanly because a tree is its own view. Tree calculus avoids the problem by having no
  shared nodes and no back-wires, so the unrolled graph is the graph. Any λ-graph with a back-wire from variable
  to binder is not its own view, and reading it needs marks.
- *Inference:* "case on a graph" means case on the node you stand on: its label and which port you came in by.
  That set of shapes is finite (the agent kinds). What the case cannot have is an arm for "this occurrence
  belongs to that binder"; that is learnable only by marks. The honest finite case on a λ-node branches on how
  its variable wire is used: unused (eraser), used once (direct wire), shared (fan). This is the λj 0 / 1 / many
  split, and in a net with explicit erasers and fans it is visible at the λ's variable port, one wire away.
- *Inference:* the decomposition of a binding graph with a finite set of local shapes is the routing one: at
  each binary node, for one variable, a director from {both, left, right, none} = {S, B, C, K}. One variable at a
  time this is bracket abstraction (finite shapes, no back-wires, larger terms). All variables at once it is
  director strings (finite letters, unbounded strings). Interaction-net λ-graphs with fans and erasers encode the
  same routing as wiring instead of letters.
- Every self-reproducing graph system found copies without reading: fan-out walking through the molecule
  (chemlambda, interaction-combinator quines), template copy of strands (Klavins), copy by finding an equal
  molecule (Combinatory Chemistry), or application (AlChemy). Only systems whose description is a string or a
  tape read it: Stringmol's pointers and Tomita's construction arm. *Inference:* if the project wants a net
  that reads its own code, the code to be read should be stored tree- or tape-shaped, and copying should stay a
  duplicator walk that never inspects.
- Rules stored in the graph exist (Tomita's metanodes, Stringmol code), but in each case a fixed outer rule set
  interprets them. No source found where local rules read and rewrite graph-shaped rules.
- A single walking reader is weak: finite walkers cannot explore all graphs, and on trees are weaker than
  branching automata. *Inference:* a net's reader should branch (spawn a reader on each part, as interaction
  nets do naturally), not walk one token. If reversibility matters, a walker can be made reversible at about
  6nk states.
- Richer patterns cost locality: general matching is size(G)^size(L); constant time needs roots and bounded
  degree. A deeper case (looking two or more nodes ahead, or asking whether two wires meet) leaves the
  interaction-net model, as Arrighi–Dowek disks and Klavins' ternary rules show.

## Searched, not found

- **Body of Tomita et al. self-description papers** (ECAL 2005, doi 10.1007/11553090_71; Artificial Life 13(4),
  2007, doi 10.1162/artl.2007.13.4.383) and **Physica D 171 (2002)**. Springer, MIT Press and Elsevier block;
  the author's AIST publication page did not load and the Wayback Machine refused the fetch; Semantic Scholar
  was rate-limited. Only the PubMed abstract and the 2006 slides were verified, so the metanode layout and how the
  arm reads branching structure are not verified.
- **Yamashita–Kameda (IEEE TPDS 1996) and Angluin (STOC 1980).** Paywalled; cited only through Boldi–Vigna and
  Chalopin–Métivier.
- **Fraigniaud et al., "Graph exploration by a finite automaton", TCS 2005.** No open full text found; only the
  first co-author's own slides (cited above).
- **Klavins–Ghrist–Lipsky, IEEE TAC 2006.** IEEE page gave no text; the ICRA 2004 version was used.
- **Accattoli–Guerrini, "Jumping boxes", CSL 2009.** Paywalled; the LMCS 2012 follow-up was used.
- **Wadsworth's thesis (1971), Kauffman-style constructive systems, amorphous/spatial-computing
  "self-inspection".** Not searched beyond one web query for the last ("self-inspection local rules network
  topology"), which returned only the topology-recognition literature cited above.
- **Interaction-net quines or self-reproduction other than Buliga's IC quines and Lafont's example.** One web
  search ("interaction nets self-reproducing OR quine OR self-replicating interaction combinators") found
  nothing else. An arXiv API search for the same terms returned no relevant hits.
- **Reversibility in graph-rewriting automata.** Nothing in the verified material; not searched further.
- One quote from chemSKI (arXiv 2306.00938, "dirty quine graph") missed and was dropped.
