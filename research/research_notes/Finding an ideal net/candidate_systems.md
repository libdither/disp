# A candidate net between λ-terms and a reversible lattice

The question: find a local graph-rewriting system whose programs can read other programs with no quote step done outside the system (Barry Jay's intensional completeness), and that sits close to λ-terms on one side and to a reversible lattice of cells on the other.

Short answer. One concrete candidate is built, tested and scored: the **quote-and-table net**. It is an interaction net (agents that rewrite in pairs) with 14 fixed kinds of agent. It keeps the older reflective net's core (variables are wires, a function's body stays inert until called) and replaces its reader. Against the older design, measured:

- the rule table is shorter (1,024 against 1,202 bits) and the self-interpreter is half the size (121 against 249 agents), so the score J falls from 4,276 to 2,511 bits;
- the same reading jobs take 12 to 97 times fewer rewrites;
- programs in it decide equality of programs, count binders, and interpret themselves, in every order of rewrites tried.

Three limits came out as well, each with an argument or a measurement: a net of this kind cannot share a value lazily between two users, so copying is eager and deep; no rewriting net can run backward without a record per rewrite, so heat grows with the work done and not with the surface; and wires do not stay short as programs grow. The candidate is a measured point, not a proven optimum. Nothing here is a lattice: the second translation is a cost model with stated assumptions.

How to read this. *Measured* means a number from a probe in `research/linear-ic-net/ideal/`; commands are at the end. *Proven* means a Lean theorem in `research/linear-ic-net/lean/QuoteNet.lean`. *Inference* marks reasoning that no probe, proof or source checks. A linked phrase is what the linked source says; every link was matched by `scripts/verify-source.sh` (the quotes are collected in [`sources_for_candidates.md`](sources_for_candidates.md)). Words used throughout:

- **agent**: a node of a net. **kind**: its label. **principal port**: each agent's one main port. **auxiliary ports**: its others.
- **active pair**: two agents joined principal to principal, the only place a rule fires. **right side**: what the rule puts in the pair's place.
- **old design**: the reflective net of `research/linear-ic-net/nets/net.py` (13 kinds; a reader agent that builds a five-argument function at every read).
- **table design**: `nets/tables.py`, the fast variant whose agent kinds depend on the program loaded.
- **interaction combinators**: Lafont's three-kind system.
- **J**: the score of [`objective_and_search.md`](objective_and_search.md): bits of the rule table plus bits of a self-interpreter written in the system.
- **blank**: an empty cell a rewrite can fill. **record**: a mark left behind so a rewrite can be undone.

## 1. What "intensionally complete" means here

Jay uses three notions, and only two are formal.

| notion | where | what it says | formal? |
|---|---|---|---|
| intensional completeness | tree calculus book, §11.3 | ["Intensional completeness is perhaps the ability to perform arbitrary program analyses"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=138), ["as supported by pattern-matching functions"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=138); a pattern is any term such that ["if all variables in the pattern are replaced by programs then the result is a program"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=138) | no: Jay's own word is "perhaps" |
| program completeness | same book, §11.2 | ["a theory is program-complete if it supports the same class of functions of binary trees as tree calculus"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=137) and ["supports an invertible function from programs to binary natural trees"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=137) | yes, but tied to tree calculus by definition |
| structure completeness | Jay and Given-Wilson, 2011 | ["A confluent combinatory calculus is structure complete if, for every normal term P and term M"](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=15) the case that sends P to M ["is represented by some term P → M"](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=15); a calculus ["is structure complete if and only if it supports S, K, F and eqatom"](https://opus.lib.uts.edu.au/bitstream/10453/14486/1/2010003973.pdf#page=16) | yes |

Two more of Jay's statements fix the spirit. What counts as intensional is what a query sees without running the program: ["The intensional behaviour of a program p is all behaviour which is revealed by making it the argument of some query"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=66), ["without trying to apply p to arguments"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=66). And no quote step is allowed: ["the programs are already data structures, so that no pre-processing is required"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=18). His test case is equality: combinatory logic fails because it ["cannot distinguish combinators that have the same functional behaviour"](https://raw.githubusercontent.com/barry-jay-personal/tree-calculus/master/tree_book.pdf#page=107).

**Adopted definition.** Structure completeness, carried over to a calculus with binders. A system is intensionally complete when its programs include:

1. every λ-definable function (the S and K of the theorem);
2. **factorisation** (the F): one program step that tells which kind of node sits at the top of any value and hands back that node's parts, the value being taken as it stands at run time, with no translation applied to it first;
3. **equality of atoms** (the eqatom): here there is one atom, and factorisation already tells it apart from everything else.

Because the calculus has binders, a fourth thing is needed that the combinatory theorem does not mention: **opening**, a step that hands back a function's body as code with a mark of the reader's choice in place of the bound variable. *Inference:* without it no local rule can tell which variable use belongs to which binder (the locality limit recorded in [`../../reports/Self-modifying fabrics.md`](../../reports/Self-modifying%20fabrics.md)).

Why this one. It is the only one of the three that is both formal and not defined by reference to tree calculus, and its theorem says exactly which primitives to look for. The property is then checked by Jay's own witnesses, each run as a program of the system: equality of programs, size, and a self-interpreter that interprets itself (§4).

Not claimed: the second half of program completeness in the direction tree → program. A program of the candidate can take any value apart completely, but it cannot rebuild a bare application node from its two sides or a binder from an opened body (§7).

## 2. The object and the objective

**The object** is not a net system alone. It is a net system together with its two translations:

| part | what it is | here |
|---|---|---|
| a calculus L | terms, values and a reference machine: what we want to say | the quote-and-table calculus, `ideal/kref.py` |
| a net system S | a fixed list of kinds and one rule per pair | `ideal/ktable.py`, `ideal/knet.py` |
| compile and read-back | L-terms → nets of S, one fixed small net per construct; finished nets → values | `KNet.emit`, `readback` |
| square 1 | the value the machine gives equals the read-back of the finished net, in every order of rewrites; stuck programs stay stuck | tested, not proven (§4) |
| a fabric F | a lattice of cells with one local rule | not built |
| embed and decode | nets of S → cell patterns and back, with one rewrite equal to a bounded piece of the lattice's run | a cost model only (`ideal/lattice.py`) |

On the three open questions:

- *What is being sought?* The triple with both squares. A net system with no read-back map cannot even state that it computes the right thing, and one with no embedding has no lattice cost.
- *λ syntax or an abstract specification on top?* *Inference:* both, with different jobs. The specification is §1's (S, K, F, eqatom, plus opening). λ syntax is the yardstick for the first translation's size, because the choice "variables are wires" is what keeps that translation at 1.25 agents per term node (§4).
- *Is the lattice rule fixed or searched?* *Inference:* fix its class, and search above it. Lafont's translation turns any rule table into matter over the three combinators: the translation of a cell ["represents the finite tree of all possible futures of a cell"](https://dcreager.net/pdf/Lafont1997.pdf#page=19), and in the general case a package ["plays the role of a genetic code"](https://dcreager.net/pdf/Lafont1997.pdf#page=22). So a fabric only ever has to do two things, annihilate and commute, and the choice of net system becomes a choice of encoding. What that encoding costs for this candidate is not measured.

**The objective**, in order:

1. Hard constraints H1–H7 of `objective_and_search.md`: well formed and polarized, fixed kinds, every pair has a rule or is listed stuck, honest copying and erasing, correct through a compiler with one net per construct, a reader that tells kinds apart, a full self-interpreter.
2. J in bits.
3. The two stretches, compared between systems whose J is close:
   - λ → net: agents per term node; rewrites per step of the reference machine; how much is copied.
   - net → lattice: cells per agent, wire included; lattice steps per rewrite (how far a signal travels); how far away the blanks are; records per rewrite (heat, if the records are dropped).

**Criteria weighed.**

| criterion | verdict | why |
|---|---|---|
| no global clock | keep, hard | free in an interaction net; *proven* for this table (`q_strong_confluence`, `q_finished_unique`) |
| one fixed finite set of kinds | keep, hard | it is what the table design broke |
| conservation: every rule a bijection on agents plus blanks | drop as hard, keep as a cost | *measured*: 5 of the 35 rules leave no agent and 14 more cannot be told apart afterwards (§5). A universal interaction system that needs no record is an open question: ["One may wonder if there exists a reversible equivalent of interaction combinators"](https://arxiv.org/pdf/1806.07275#page=6) |
| bounded wire length | drop as hard, keep as a cost | *measured*: travel grows with the side of the cube the net fills (§4) |
| heat scales with surface, work with volume | false for plain rewriting | *measured*: on programs that erase nothing, 73% of rewrites leave no trace at every size (§5). It holds only if records are kept and then undone |
| parallelism | keep as a cost | *measured*: threading a table through a program cuts rewrites 3 times and raises rounds 4 times (§4) |
| rules as matter | belongs to the encoding below the net, not to the net | Lafont's translation, above; unmeasured |
| size of the self-interpreter | already in J | |
| ports per agent (added) | a cost | a cubic cell has 6 faces; the candidate's reading agent has 7 ports, the old design's agents 3 |
| reads leave a trace (added) | a property worth asking for | *measured*: each of the five reading rules has a right side no other rule has |
| depth of a copy (added) | a cost | what one use of a value copies: here everything under it (§5) |

## 3. The candidate

Notation in this section is the candidate calculus's own (`ideal/kref.py`), not disp. Every program shown is copied from `ideal/kcheck.py`, where it is tested.

### The calculus

Code has four kinds of node and a table:

| written | node | means |
|---|---|---|
| `x` | a bound name | |
| `f a` | an application | |
| `\x. b` | a function | |
| `'M` | a quote | run, it hands back the code `M` as a value, unrun. A name inside `M` is still bound outside |
| `ρ` | the atom | |
| `[k1 \| k2 \| k3 \| k4 \| k5]` | a table | one slot per kind of value: function, application, quote, atom, table |

Running is call by value and never goes under a binder. There are three ways to apply:

- `f v`, f a function: its body runs with x standing for v.
- `t v`, t a table: the slot for v's kind is applied to the table itself, then to v's parts. The parts of a function are the function itself; of an application, its two sides; of a quote, what it quotes; of the atom, nothing; of a table, its five slots.
- `'g v`, a quote applied (**opening**): the code that `g v` would run, handed back unrun. For a function that is its body with the variable standing for v. For a table it is the slot, the table and the parts, written as an application.

Applying the atom or an application is an error (stuck).

A table hands itself to the slot it runs, so a table is its own loop and no fixpoint is needed. Counting binders:

```
size := [ \t f. suc (t ('f ρ))
        | \t g b. add (t g) (t b)
        | \t w. zero
        | \t. zero
        | \t a b c d e. add (t a) (add (t b) (add (t c) (add (t d) (t e)))) ]
```

The whole self-interpreter. `ap x t y` applies x to y; both a function and a table are opened on y and the code that comes back is interpreted:

```
ap := [ \u f t y. t ('f y)
      | \u g b t y. err
      | \u w t y. 'w y
      | \u t y. err
      | \u s1 s2 s3 s4 s5 t y. (\x. t ('x y)) [s1 | s2 | s3 | s4 | s5] ]
interp := [ \t f. f
          | \t g b. ap (t g) t (t b)
          | \t w. w
          | \t. ρ
          | \t a b c d e. [t a | t b | t c | t d | t e] ]
```

### The net

14 kinds once copiers and erasers are split by the way they face (11 if not):

| | agents (principal port first) |
|---|---|
| code, inert, principal port toward its user | `lam(p; x, b)`, `app(p; f, a)`, `var(p; s)`, `rho(p)`, `tab(p; k1..k5)` |
| running | `O(p; a, r)` apply, `A(p; a, r)` open-apply, `T(p; o)` thaw, `C(p; k1..k5, r)` a table reading |
| resources | `sig` (where a variable splits), `dupI`, `dupO` (a copy in flight), `eraI`, `eraO` (erasers) |

35 rules, 14 pairs left stuck on purpose:

| pair | result | shape |
|---|---|---|
| `O ⋈ lam` | argument to the binder; the body under a thaw, to the result | |
| `A ⋈ lam` | argument to the binder; the body to the result | annihilation |
| `O ⋈ var` | `A`, facing what the quote holds | |
| `O ⋈ tab` | `C` facing the argument, its result under a thaw | |
| `A ⋈ tab` | `C` facing the argument | |
| `C ⋈ X`, five rules | a copier on the slot for X's kind; one copy back into a rebuilt `tab`; the result is the code "copy, applied to the table, then to the parts", each under a quote | |
| `T ⋈ app` | `O` with a thaw on each side | |
| `T ⋈ var` | the two wires joined | annihilation |
| `T ⋈ tab` | the table with a thaw on each slot | Lafont's commutation |
| `T ⋈ lam`, `T ⋈ rho` | the value itself | |
| a copier meets code | two copies; a copier on each auxiliary port | commutation |
| an eraser meets code | erasers on each auxiliary port | commutation |
| `dupI ⋈ dupO`, `eraO ⋈ eraI` | wires joined; nothing | annihilation |

A term compiles one agent per node, plus a `sig` per extra use of a variable and an eraser per unused binder, as in the old design.

### What changed from the old design, and why

1. **A quote and a variable use are the same agent.** `var`'s one auxiliary port leads to a binder or to a piece of code; a thaw meeting it hands over what is there, unrun. This removes the opened-function kind (`Lam`): an opened function is a function whose body is a quote.
2. **Opening is an application that does not thaw.** `A ⋈ lam` joins two pairs of wires and makes nothing. `O` is `A` followed by a thaw. A program asks for it by applying a quote.
3. **Reading is choosing a slot of a table that survives.** Code agents already are the tagged values (an application agent is "application, with these two parts"), so nothing is built to represent them. The read copies the one slot it takes, and nothing else.
4. **A read leaves code, not a running call.** This is what makes opening work on tables as it does on functions, and it is what halves the interpreter (§6 has the version without it).

*Inference:* `rho`, `var`, `app` are Jay's leaf, stem and fork (zero, one and two children), `lam` adds the binder, and the table is triage with one more arm. The candidate is tree calculus's reader on a net whose variables are wires.

### Proven about the table

All by `decide`, in `lean/QuoteNet.lean` (18 theorems, no error; it reuses the general theorems of `Net.lean`, `Runs.lean`, `Moves.lean`):

| theorem | says |
|---|---|
| `q_tableOk`, `q_runOk` | every rule wires every port once, every wire joins an out port to an in port, one rule per pair |
| `q_strong_confluence`, `q_finished_unique` | two steps from one net meet again in one more step each; two finished runs end in the same net |
| `q_total` | every pair that can meet has a rule or is one of the 14 listed stuck pairs |
| `q_code_faces_up` | code never reacts with code |
| `q_only_erasing_drops` | only an erase rule makes an eraser |
| `q_honest`, `q_reads_copy` | only copiers and tables make copiers; with tables not counted, the rules that break this are exactly the five reading rules |
| `q_reads_tell_kinds_apart` | the five reading rules put their copier on five different slots |
| `q_admissible` | the hard constraints H1–H4 as one statement |

So H4 (honest copying) holds only in a weakened form: a table is counted as a place where the program asks for a copy, the way a variable used twice is.

## 4. Measured

### Correct, in every order tried

*Measured* (`kcheck.py`; the net against the reference machine, each program in three orders of rewrites: queue order and two shuffles):

| check | result |
|---|---|
| 12,000 random programs of 3–40 nodes (plain λ-terms; with quotes and openings; with tables that read) | all agree: 8,191 values, 3,667 stuck both ways, 142 skipped for fuel |
| 54 hand-written programs | all agree, in three orders each |
| random programs run directly and under each of the two interpreters (3,000 each) | 2,092 same values, 878 stuck both ways, 30 skipped; 148 of them also run as nets |
| `equal` on 300 random functions | a function against itself with its binders renamed: true, 300 of 300; against another function: right, 300 of 300 (18 of those pairs equal) |
| the 19 adds of `nets/check.py`, which use no reader | same answers and the same number of rewrites as on the old net |
| a reader that erases nothing (`mirror`) | 7 reads, 0 erase rewrites, 0 copies started by a variable used twice |
| bookkeeping, every run | no cancel ever joined two different copies; no running agent was copied or erased; no agent left over |

The witnesses of §1, all as programs of the system: `equal` (it answers false for `\x. x` against `\x. 'ρ`, the case where a careless mark would be forged), `size size`, and the interpreter interpreting itself interpreting a program, three levels deep.

### The score

*Measured* (`kscore.py`). Bits, signature included; "default" is the default-plus-exceptions code of `objective_and_search.md`; "dual default" is added here: a kind may name a dual, a kind meeting its dual annihilates, any other pair commutes. A polarized system needs it, since two agents of one kind can never meet there.

| system | kinds | rules | stuck pairs | literal | per-rule shape | default | dual default | most agents a rule makes | most ports |
|---|---|---|---|---|---|---|---|---|---|
| interaction combinators | 3 | 6 | 0 | 84 | 48 | 17 | 23 | 4 | 3 |
| old design | 13 | 33 | 9 | 1,481 | 1,202 | 991 | 1,031 | 18 | 3 |
| candidate | 14 | 35 | 14 | 1,460 | 1,024 | 800 | 845 | 15 | 7 |

The reading rules alone: 762 bits in the old design, 493 in the candidate. By the coarser code of `Ideal.lean`: 1,283 against 1,069 (*proven* arithmetic, by `#eval`).

Programs as nets:

| program | old design | candidate |
|---|---|---|
| `kindOf` | 80 agents, 811 bits | 50 agents, 484 bits |
| `size` | 134 agents, 1,614 bits | 151 agents, 1,998 bits |
| the self-interpreter | 249 agents, 3,074 bits | 121 agents, 1,487 bits |
| the self-interpreter with its tables threaded through (`interpT`) | – | 226 agents, 3,132 bits |
| `equal` | none written | 792 agents, 11,960 bits |

| J = table + self-interpreter | shape code | default | dual default |
|---|---|---|---|
| old design | 4,276 | 4,065 | 4,105 |
| candidate | **2,511** | 2,288 | 2,332 |
| candidate, counting `interpT` | 4,156 | 3,933 | 3,977 |

With the note's second key, J plus log2 of the rewrites on `size suc`, `size size` and "interpret 1 + 1": 4,323 for the old design, 2,548 for the candidate with the same small interpreter that J counts.

### Cost of the same jobs

*Measured* (`kscore.py`). "Threaded" is the same program with its table passed along and handed back at every read, so that it is never copied or dropped.

| job | old design | candidate | candidate, threaded | table design |
|---|---|---|---|---|
| `size suc` (10 reads) | 20,803 | 1,799 | 917 | 128 |
| `size` on the code of the old design's `size` (102 reads) | 208,233 | 16,097 | 7,127 | – |
| `size` on itself | 165,713 | 24,527 | 13,446 | 1,938 |
| interpret 1 + 1 | 35,745 | 3,682 | 1,194 | 386 |
| interpret 2 + 2 | 52,023 | 5,436 | 1,620 | – |
| the interpreter interpreting itself on `(\x. x) (\y. y)` | 942,777 | 20,675 | 9,670 | – |

- Rewrites per read: 1,300 to 2,100 in the old design, 158–227 in the candidate, 58–92 threaded, 12–15 in the table design.
- Old over threaded candidate: 12 to 97 times, or 3.6 to 6.6 bits on a log scale.
- The table design is still 3 to 7 times ahead, but its count is not comparable: one of its rewrites makes up to 38 agents. In agents made, interpreting 1 + 1 costs 71,207 (old), 2,152 (threaded candidate), 778 (table design).
- One more level of self-interpretation costs 17 to 24 times (555 to 9,670 rewrites on `(\x. x) (\y. y)`, 1,194 to 28,888 on 1 + 1, threaded), against about 80 times in the old design (11,502 to 942,777).

Where the rewrites go, as shares of a run:

| run | copy | cancel | erase | thaw | apply | read |
|---|---|---|---|---|---|---|
| old: `size` on the old `size` | 34% | 8% | 55% | 2% | 1% | 0% |
| candidate, threaded: the same job | 37% | 12% | 5% | 32% | 10% | 3% |
| candidate, threaded: the interpreter on itself | 31% | 11% | 6% | 38% | 11% | 3% |

So in the threaded candidate about half of the rewrites are the program's own work (thaw, apply, read) and most of the rest is copying. *Inference:* a net with free sharing could be at most about twice as fast on these jobs.

Work against time (*measured*, rounds = the least time with no limit on how many pairs fire at once):

| run | rewrites | rounds | rewrites a round |
|---|---|---|---|
| old: interpret 1 + 1 | 35,745 | 438 | 82 |
| candidate: interpret 1 + 1 | 3,682 | 111 | 33 |
| candidate, threaded: interpret 1 + 1 | 1,194 | 418 | 2.9 |

Threading saves work and gives up parallelism: the table becomes a token that one read holds at a time.

### First translation: λ-terms into the net

*Measured* (`klambda.py`):

- 1.25 agents per term node on random λ-terms (worst 1.62 of 2,000 terms), 1.24 with quotes, 1.53 with tables. The compiler is the old one for λ-terms.
- 7 to 10 rewrites per step of the reference machine (a β-step, a read or an opening) on six of eight programs; 15 on `ordinary_list_add` and 27 on `equal`, which copy and drop lists or tables.

### Second translation: the net on a 3D grid

*Measured* (`lattice.py`), under a crude model stated in that file: one agent per cell; wires run anywhere and cross freely; agents never move; a pair fires over the wire between its two agents, and that distance is how far a signal travels; made agents go into the nearest free cells; the program starts as a filled cube.

| run | rewrites | travel, mean | 95th percentile | largest | blanks from | wire per agent |
|---|---|---|---|---|---|---|
| old: interpret 1 + 1 | 35,745 | 7.2 | 14 | 42 | 1.6 | 5.2 |
| candidate, threaded: interpret 1 + 1 | 1,194 | 4.7 | 9 | 18 | 1.0 | 3.6 |
| candidate, threaded: `size` on the old `size` | 7,127 | 5.4 | 14 | 38 | 1.1 | 5.3 |
| candidate, threaded: the interpreter on itself | 9,670 | 5.6 | 15 | 44 | 0.9 | 5.0 |

As a program grows (`linear_tree_add`, an add that erases nothing, the same on either net):

| size | rewrites | most agents at once | side of the cube that holds them | travel, mean | wire per agent |
|---|---|---|---|---|---|
| 4 bits | 2,236 | 839 | 10 | 6.0 | 5.5 |
| 16 bits | 9,428 | 3,477 | 16 | 7.8 | 6.5 |
| 64 bits | 38,148 | 14,096 | 25 | 12.0 | 9.7 |
| 256 bits | 152,980 | 56,532 | 39 | 16.9 | 13.4 |

- A rewrite costs about 5 to 17 cell-steps of signal travel on these runs, and a blank is one or two cells away.
- Travel is not bounded: it grows with the cube's side, and is 0.4 to 0.6 of it here.
- A long run drifts. `ordinary_list_add` on 256 bits never holds more than 5,523 agents, which fit a cube of side 18, yet it spreads over a box of 240 × 243 × 77 cells, because agents are born where pairs meet and nothing moves them back.

## 5. Limits

### A net of this kind cannot share lazily

**Proposition** (*Inference*: a pen-and-paper argument, not machine-checked). Take any interaction system, where a cell ["has one principal port"](https://dcreager.net/pdf/Lafont1997.pdf#page=3) and ["cells can only interact pairwise, through their principal ports"](https://dcreager.net/pdf/Lafont1997.pdf#page=5). Take a net in which nothing can fire, with free ports c1 and c2, and plug a consumer into c1 only. Then the rewrites that follow can consume only the agents that hang below c1, where "b hangs below a" means b's principal port is wired to an auxiliary port of a.

*Argument.* An agent is consumed only when its principal port meets another principal port. Its principal port is wired to one place. If that place is an auxiliary port of an agent a, the wire changes only when a itself is consumed, since a rule rewires only the auxiliary ends of the two agents it takes. So an agent is reached only after the one it hangs from, and so on up a chain that is unique and must start at c1. A rewrite can also send a new agent along a wire to an auxiliary port of an agent below c2; it arrives principal port against auxiliary port, and waits.

**Consequence.** A value held without being touched hangs below at most one free port. So of two users of one value, only one can wake it. Either the value does the copying itself the moment it arrives (eager), or one user is served first and the other waits for it. A share that either user can trigger needs an agent with two principal ports, and that leaves interaction nets: with several principal ports ["one cell may be involved in several active pairs, as many as its principal ports"](https://lipn.fr/~mazza/papers/Thesis-1.0.pdf#page=25), and ["The choice of which one is reduced is non-deterministic"](https://lipn.fr/~mazza/papers/Thesis-1.0.pdf#page=25).

What it means here. Sharing graphs copy lazily because a copier stops at an application whose principal port faces away: they ["duplicate the abstraction node without jointly duplicating the whole body of the function"](https://arxiv.org/pdf/1701.04240#page=2). In the candidate all code faces its user, which is what lets a thaw walk it and a table read it, so a copier never stops: a copy is the whole value, with everything it captured. *Measured*: 32 to 49% of the threaded candidate's rewrites are copies and cancels (32% when interpreting 1 + 1). The earlier probe `nets/lazy_copy/` tried a sharer with two principal ports; it was 9 times cheaper than plain copying on one reading job (2,617 against 22,929 rewrites) and dearer on the adds (7,838 against 4,636).

### No net runs backward without records

A reversible interaction system must be one where ["two active pairs never reduce to overlapping nets and any net can be the result of at most one active pair"](https://arxiv.org/pdf/1806.07275#page=4). *Measured* (`klambda.py`, `kscore.py`), the candidate's 35 rules:

| right side | rules | which |
|---|---|---|
| no agent at all | 5 | `A⋈lam`, `T⋈var`, `dupI⋈dupO`, `eraI⋈rho`, `eraO⋈eraI` |
| the value alone, as if nothing had met it | 2 | `T⋈lam`, `T⋈rho` |
| the same as another rule's | 12 | a copy by a split and a copy by a copier in flight; two erases |
| one no other rule has | 16 | the five reads among them |

The old design: 5, 3, 14 and 11 of 33. *Inference:* the interaction combinators fail the condition too (two of their six rules leave bare wires), and so does any system with an annihilation, so this is not a defect to design away: a rewriting net needs one record per traceless rewrite.

**Heat.** *Measured*: on adds that fire no erase rule, the share of rewrites that leave no trace is the same at every size.

| add | 4 bits | 16 bits | 64 bits | 256 bits |
|---|---|---|---|---|
| `linear_tree_add`: traceless rewrites, share of all | 72% | 73% | 73% | 74% |
| the same, per bit of the numbers | 400 | 431 | 438 | 439 |
| `linear_list_add`: share | 72% | 73% | 73% | 73% |
| the same, per bit | 269 | 224 | 213 | 210 |

So a fabric that keeps no record must forget something at about three rewrites in four, erase-free program or not. Heat then grows with the work done, which fills the volume, and the hypothesis "heat scales with surface" is false for plain rewriting. That matters because heat can only leave through the surface: irreversible machines are ["limited to processing rates that are only proportional to their surface area"](https://web.archive.org/web/20230405014547id_/https://web1.eng.famu.fsu.edu/~mpf/Frank-99-PhD-bookmarked.pdf#page=27).

*Inference:* there are three ways to live with it, and they are the three places a system can sit between the two anchors.

| way | time | space | heat | known from |
|---|---|---|---|---|
| rewrite and drop the records | 1 | the net | grows with work | every net so far |
| rewrite, keep records, copy the answer, undo | about 2 | the net plus one record per traceless rewrite (0.73 per rewrite here) | at input and output only | Bennett: a machine ["can be simulated by a reversible input-saving machine running in time O(T) and space"](https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/Bennett_Tradeoffs.pdf#page=2) O(S + T) |
| never rewrite: leave the net fixed and send a token through it | can be far worse | the net plus the token's stack | none | Lafont reads the combinators as ["reversible 2-stack machines"](https://dcreager.net/pdf/Lafont1997.pdf#page=22); the token machine ["can be exponentially slower than environment machines"](https://arxiv.org/pdf/2010.12988#page=2) |

The third way is also the only one in which code is not used up by running it. In reversible languages a branch is an isomorphism (conditionals ["are expressible using the distributivity laws"](https://homes.luddy.indiana.edu/sabry/files/information-effects.pdf#page=4)) because both arms stay where they are. A rewriting net runs code by consuming it, so it must copy the arm it takes or erase the ones it leaves. The candidate copies the one it takes.

## 6. What failed, and why

| tried | outcome | why |
|---|---|---|
| a table read that runs the slot directly (first version of the candidate) | table 831 bits, but the interpreter 314 agents and 4,583 bits: J = 5,414, worse than the old design's 4,276. It ran slightly faster (6,563 rewrites on the 102-read job against 7,127) | to apply a table, the interpreter had to redo the five-way choice inside a five-way choice. Fixed by making the read leave code, so that a table can be opened like a function. These numbers were measured before the change; the files hold only the final version |
| honest copying, to the letter | fails | a kept table copies the slot it takes (`q_reads_copy`). The honest alternative hands the four slots not taken to the one taken: the older probe `nets/case_table/` measured it at about 2,000 rewrites a read with programs written the plain way |
| every rule a bijection, as a hard constraint | fails for every system with an annihilation | §5 |
| heat scales with surface | false without records | §5 |
| bounded wire length | not on these runs | §4 |
| the first layout probe | wrong: it laid the program out as a line, and travel grew in step with program size | fixed by starting from a filled cube; only those numbers are reported |
| the gap to the table design | still 3 to 7 times in rewrites | its functions are one agent each because their code sits in the rule table; here code is matter and is copied (§5) |
| a full inverse of reading | not available | no rule builds a bare application from two code values, or a binder from an opened body (§7) |

## 7. What is proven, measured, or neither

- **Proven** (Lean, `decide`): everything in the table of §3. Not proven: that a compiled program's run ends holding the reference's value (the old design's `Simulated`, open there too), and nothing at all about programs, since the per-program Lean checks (`Check.lean`) are written for the old table's kinds.
- **Measured**: §4 and the tables of §5. The second translation is a model, not a lattice: wires take no room, agents do not move, and no reversible rule is written.
- **Inference only**: the proposition of §5; that free sharing would gain at most about 2 times; that the interaction combinators cannot read their own nets; the reading of the candidate as tree calculus's triage on a net.
- **Not done**: a search. The candidate was designed, then compared with two neighbours (the old reader, and the first version of §6). No enumeration of reader rules was run, so "best in its family" is not claimed. The score of the candidate compiled into interaction combinators is not measured. Sizes for a self-interpreter are not comparable with Tromp's ["self-interpreter E of size 206"](https://tromp.github.io/cl/LC.pdf#page=5) bits, which interprets a binary encoding given from outside.
- **Known gaps in the candidate**:
  - A 7-port agent (`C`) and a 6-port one (`tab`); a chain of two-slot cells would bring every agent to 3 ports at the price of more kinds (the older probe's `chain` variant needed 9).
  - Reading is one-way. A program builds code only by quoting a template, so a built application carries a quote around each side, and built code runs only through the interpreter. *Inference:* a use of a variable compiled as a bare wire, with no `var` agent, would give both an exact application constructor and an `eval`, with no new rule; untested.
  - `equal` needs a mark that cannot be forged; it builds one deeper than either program, which costs a walk over both.

## 8. Next step

*Inference*, in the order I would take them:

1. **Make the second translation real for the two generic rule shapes.** Annihilate and commute, with a record per annihilation, on the repo's existing strand lattice (`evaluators/rust-ic-strands/`, which already undoes rewrites). Then the travel, blank and record numbers of §4 and §5 become lattice measurements, and compaction (the drift of §4) can be tested.
2. **Run the search this note skipped.** `kscore.py` now computes J end to end for a reader, so the family "which parts each kind hands over, in what order, as code or as a call" can be enumerated with J as the objective and `kcheck.py` as the filter.
3. **Decide the sharing question on the fabric, not in the net.** §5 says lazy sharing needs an agent with two principal ports, which is a physical arbiter. Morita's rotary element is the reversible version of such a part (a ["completely garbageless circuit composed only of REs"](https://web.archive.org/web/20170815133232id_/http://ir.lib.hiroshima-u.ac.jp/files/public/2/25576/20141016152033521657/TCS_395_101.pdf#page=12) simulates any reversible sequential machine). A token that walks shared code, with the candidate's reader on top, is the design to try next.

## Commands

From `research/linear-ic-net/ideal/`, each behind `systemd-run --user --scope -q -p MemoryMax=8G -p MemorySwapMax=1G timeout 3000`:

- `python kcheck.py` (`--quick` for a tenth of the random programs): §4's correctness table, about 5 minutes.
- `python kscore.py`: §4's score and cost tables and §5's heat table, about a minute.
- `python klambda.py`: the first translation's numbers and the rule classes of §5.
- `python lattice.py`: the second translation's numbers, about a minute.
- `python ktable.py` prints the rule table; `python ktable.py --lean` rewrites `../lean/QuoteNet.lean`.
- `cd ../lean && lake env lean QuoteNet.lean`: the theorems of §3 (no output but one `#eval` line means proven).
