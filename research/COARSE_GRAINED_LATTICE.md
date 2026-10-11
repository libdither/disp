# Coarse-grained cost of the strand lattice, and compiling to fixed units

Design analysis with fits to existing data, 2026-10-10. Nothing here is built. Questions: what
large-scale law gives the strand lattice's running time from a few properties of a program; how
fixed, reused hardware units ("stuck silicon") would enter that law; and what compiling a program
onto such units automatically would take.

Terms used throughout:
- **the strand lattice**: `evaluators/rust-ic-strands`, disp's tree-calculus interaction net run as
  a cellular automaton whose wires are paths of strands and whose agents walk, in the current design
  (`latest`: the chip's block schedule, the demand field, and fork, where the S rule starts both its
  halves at once). A **clock** is one turn for every site.
- **the idealised machine**: the lattice's model with no space (`memo.rs` `ideal_depth`, the
  README's "Fork"): every wanted rewrite fires at once each round. Its **rounds** D are the
  program's parallel depth, its **rewrites** W the work. **Width** P = W/D is the average number of
  rewrites a round.
- **sites in use** A: the peak number of sites holding anything.
- **movement agents**: the six view agents (`views=1`, README "Movement agents").
- **the adders** (README "Adding binary numbers"): ripple carry, carry select and carry lookahead,
  on number trees or on bit lists; *relay*, ripple carry that passes the rest of the sum on as a
  function; *relay unrolled* (`relay_for`), relay built for one width while compiling, with no
  recursion left. *Lambada* programs (`fib(2)`, `exp(1)`, …) are the hand-written tree-calculus
  benchmarks in `bench/programs/`.
- **stuck silicon**: a unit fixed in the lattice that is called rather than copied. It stays where
  it is, is not used up, and has its own area, latency (clocks from arguments in to answer out) and
  rate (how often it can take a new call). The ordinary agents, copied and used up, are the
  **general lattice**.

**Inference** marks a claim no source makes: it is ours.

## Summary

- **The law.** Clocks ≈ 23·D + 0.7·W. Two constants fit 304 adder runs (five binary adders on lists
  and trees and unary `add`, 1 to 64 bits, with and without movement agents) to within ×1.20
  typically, and predict eight programs they were not fitted on (disp `add`, `fib`, `mul`, `rev`,
  `isort`, `greet`, lambada `fib(2)` and `exp(1)`) to within ×1.10. A round costs about 23 clocks
  (closing a gap of about two strands) plus 0.7 for each rewrite running beside it. So a computation
  does at most about 1.4 rewrites a clock, and reaches half of that at about 33 rewrites a round:
  Brent's scheduling bound with 1.4 processors, or a roofline with its knee at width 33.
- **The README's "a round costs √(sites in use)"** (How they scale) holds for tree adders only,
  where it is the width in disguise. Lists refute it as a general law: their sites in use grow 2–24×
  while a round's cost stays nearly flat. Out of sample it is off by ×1.48. Area adds a smaller term
  on trees, as wire delay does in VLSI.
- **The width cost is per computation**, not a global limit: parts side by side do not slow each
  other. Its constant varies 5× between programs (compact programs pay less), and it is not yet
  derived; the latency is, roughly, from the walkers' measured speeds.
- **Stuck silicon** enters as a resource with area, latency, rate and an interface cost, reached at
  some speed. What decides reuse against copies is the size of the thing reused. A copy costs about
  0.7 clocks per agent copied; a trip to a fixed unit costs the distance over the speed (0.13
  strands a clock for walking matter, 1 for a pulse). For a code node the copy wins at any distance
  beyond a site or two, which is what `research/LINEAR_TREE_NETS.md` measured; for a whole 8-bit
  adder (about 6k clocks on the lattice) one unit pays even if arguments walk 300 sites to it.
  Replicas are then set by distance, not by load.
- **Compiling** to such units is high-level synthesis (choose units, count, place, schedule) plus a
  discrete time–cost trade-off: NP-hard in general. The coarse law makes it tractable in practice:
  every candidate configuration is costed in closed form, and disp programs have few distinct
  functions to harden.
- **First experiments**: (0) derive the two constants from the lattice's own trace of each rewrite's
  delays, no code change; (1) one persistent adder unit serving m callers against m copies, which
  the law predicts cross over at about 30–60 calls for an unpipelined unit.

## 1. A coarse model of the lattice

### The data

- **Fitted on**: the adder sweep behind the README's "How they scale" (`programs/bench.ts` driving
  `strands-bench`; its tables are in `research/coarse-lattice/`): 304 (program,
  width, inputs, setting) points, each the mean of 1–3 seeds, every answer checked, each paired with
  the idealised machine's D and W for the same call. Seeds spread clocks by ±3–5%.
- **Tested on**: the README's "Movement agents" table (plain runs: clocks, rewrites, peak sites)
  with D and W from `research/LINEAR_TREE_NETS.md` §6 ("base"), for disp `add 2 3`, `fib 3`,
  `mul 2 2`, `rev [1, 2]`, `isort [2, 1]`, `greet "a"` and lambada `fib(2)`, `exp(1)`; and nine runs
  summing several numbers (`added`, `saved`).

Least squares on log clocks, so errors are factors; "×1.20" is the typical (root mean square)
factor, "max" the worst:

| model | plain | views | both | max | constants (both) | 8 other programs |
|---|---|---|---|---|---|---|
| T = k·D | ×1.47 | ×1.58 | ×1.53 | ×6.9 | 39.5 | ×1.40, 1.37× too many clocks on average |
| T = k·W | ×1.63 | ×1.72 | ×1.68 | ×5.8 | 2.14 | ×1.44 |
| T = k·D·√A (a round crosses the area) | ×1.32 | ×1.33 | ×1.42 | ×3.7 | 0.76 | ×1.48, 1.43× too many |
| T = D·(a + b·√A) | ×1.25 | ×1.28 | ×1.32 | ×3.5 | 15.9, 0.43 | ×1.39 |
| **T = a·D + b·W** | **×1.18** | **×1.22** | **×1.20** | ×2.5 | **23.4, 0.70** | **×1.10** |
| T = a·D + b·W + c·D·√A | ×1.16 | ×1.20 | ×1.19 | ×2.5 | 19.6, 0.59, 0.12 | ×1.14 |

On the nine sums of several numbers the law is off by ×1.26 with no bias (worst ×1.78, `added` of
four 8-bit numbers), against ×1.53 for T = k·D·√A.

Fitting plain and views runs separately gives the same constants (a = 22.5 and 24.4, b = 0.71 and
0.68), so movement agents change D and W, not the machine's speed: halving both halves the clocks,
as measured (README: 0.55× the clocks for 0.50× the rewrites). Fitted against the lattice's own
rewrites instead of the idealised machine's, seed by seed (802 runs): a = 21.9, b = 0.89, ×1.18.

### Reading the law

T ≈ a·D + b·W with a ≈ 23 clocks a round and b ≈ 0.7 clocks a rewrite. Three readings of the same
formula:
- **A round costs a + b·P**: a fixed latency, plus a cost for every rewrite running beside it. Unary
  `add` (P ≈ 6) pays about 25 clocks a round at every size from 2 to 512; lists (P ≈ 7–22) 24–48;
  trees, whose width grows with the bits, 22 at one bit and 60–115 at 32–64.
- **Throughput saturates.** Rewrites a clock = P/(a + b·P): about P/23 while few run at once, at
  most 1/b ≈ 1.4, half of that at P = a/b ≈ 33. This is the roofline's shape
  ([Williams, Waterman & Patterson](https://www2.eecs.berkeley.edu/Pubs/TechRpts/2008/Archive/EECS-2008-134.pdf#page=4)),
  with width in place of arithmetic intensity, and Brent's bound, time ≤ depth + work / processors
  ([Brent 1974](https://maths-people.anu.edu.au/~brent/pd/rpb022.pdf#page=4)), with about 1.4
  processors.
- **Little's law**
  ([Little](https://people.cs.umass.edu/~emery/classes/cmpsci691st/readings/OS/Littles-Law-50-Years-Later.pdf#page=1)):
  rewrites in flight = rate × time each takes. Taking about a round (23 clocks) for each, the
  lattice keeps 5 in flight on unary `add` (80% of its width), 8–11 on lists (half to two thirds)
  and up to 40 on the widest trees (a sixth to a third).

**A check it was not fitted for.** Merging equal computations (README "Memo radius") takes rewrites
off parallel branches, leaving D about as it was, so the law predicts clocks fall by b per rewrite
saved (0.85 a lattice rewrite):

| program | rewrites saved (r = 16) | clocks, predicted | clocks, measured |
|---|---|---|---|
| fib(3) | 30% | −15% | −10% |
| fib(4) | 36% | −23% | −9% |
| exp(1) | 4% | −1% | +9% |
| sort(2) | 8% | −4% | −2% |
| disp `fib 3` | 16% | −5% | −4% |
| disp `fib 4` | 27% | −11% | −11% |

Four of six within a few points. exp(1) pays for the merges themselves, which the law does not see;
on fib(4) the merges probably also lengthen D. The README's reading of that experiment ("it saves
work, not time") is the law's: saved work costs b, a thirtieth of a round, per rewrite.

### Where it fails

- **The width cost is per computation.** Parts run side by side do not slow each other (README "Side
  by side": `add 20 22` alone 75.7k clocks, beside `relay_add 20 22` 74.9k), and four adders
  superposed with nothing shared take 1.21× the slowest alone where a global b would predict 1.7–2×.
  So b is not a processor count; it is a cost of width inside one computation's region.
- **b varies between programs.** Fitted per adder, a is 13–30 and b 0.27–1.5. Relay on a tree, the
  most compact, reaches 1.75 rewrites a clock, above the fitted 1.4, and the law overestimates it by
  up to 2× at 32–64 bits. Relay unrolled, whose laid-out code sprawls (sites in use grow as n^1.07),
  pays the most (b 1.2–1.5). Carry select on a 32-bit tree is underestimated 2.5×, carry lookahead
  on 64 bits with views 1.8×. These are far outside seed noise.
- **Only trees identify b.** Unary and lists keep the same width at every size, so their D and W
  grow together and a and b cannot be told apart within one program; trees carry the split.
- **Area is a second term, on trees only.** On trees alone, T ≈ D·(13 + 0.35·P + 0.35·√A) fits ×1.24
  against ×1.30 for either term alone; on lists the √A coefficient is about 0, although their area
  grows 2–24× (relay unrolled on a list: √A from 31 to 150, a round 24 to 31 clocks). This is what
  wire delay proportional to length predicts: a list is a chain of neighbours, a tree has long edges
  near its root. The README's √(sites in use) per round was this term and the width together, since
  on the tree adders both grow with the bits.
- **Area itself** (for area–time trade-offs): A ≈ 3·(agents at load) + 45·P (×1.5): the loaded
  program's footprint plus about 45 sites for each rewrite running at once.

### Deriving it rather than fitting it

- **The lattice already projects exactly onto a coarser system.** Israeli and Goldenfeld
  coarse-grain a cellular automaton by a projection of cells onto supercells that commutes with the
  dynamics ([PRE 2006](https://arxiv.org/pdf/nlin/0508033#page=4)). Here the projection is the
  lattice onto the abstract net, checked wire by wire at the end of every run, and rewrites on the
  lattice are rewrites of the net. What does not commute is time: one step of the net takes a
  variable number of clocks. The law is the exchange rate between the two clocks.
- **The latency a, from the walkers.** A wanted reader sends demand at one strand a clock, and pairs
  become ready 1–4 strands apart (median 2). Closing a gap took 2.9 clocks at one strand and 29 at
  three under the chip's schedule (README "Quiet stretches", measured before the demand field): time
  grows as distance^2.1, the signature of diffusion (an unbiased walk needs distance² steps; a walk
  with drift v needs distance/v) ([Krapivsky & Redner](https://arxiv.org/pdf/1807.07651#page=3)). So
  at these distances the wire's pull is weak against the temperature. Two strands then cost about 12
  clocks, plus the pulse and a few for room: 15–20, against a fitted 20–24. Right in size; a fresh
  trace with the current design would pin it.
- **The width cost b** is not derived. Three mechanisms would give it, and the data cannot tell them
  apart: (i) congestion, as in lattice traffic models, where flow rises with density and then
  saturates (in the exclusion process the current is (2q − 1)·ρ·(1 − ρ), largest at half filling:
  [Kriecherbauer & Krug](https://arxiv.org/pdf/0803.2796#page=13)); (ii) chance, since with random
  delays the slowest of many parallel branches sets the pace, so a wide dependency graph takes
  longer than D times the mean delay; (iii) distance, since far branches get demand and values late.
  That b grows with how spread out a program is (relay unrolled against relay) favours (i) and
  (iii). Experiment 0 below separates them.
- **The area term, from VLSI.** With wire delay proportional to length (Chazelle and Monier, section
  2), time is at least the distance a result must cross, and trees laid out in the plane have edges
  near the root as long as the layout is wide; Rent's rule gives how average wire length grows with
  size
  ([Stroobandt](https://www.cl.cam.ac.uk/research/srg/han/ACS-P35/readinglist/stroobandt_tutorial_slip_3.pdf#page=22)).
  The lattice has two speeds where VLSI has one: pulses cross a strand a clock, walking matter
  0.12–0.14, so a long hop costs 7–8× more when matter carries it than when a signal does.
- **A coarser scale** (inference). Grouping rounds into function calls gives a call graph whose
  nodes cost (a·D_f + b·W_f clocks, W_f rewrites, some area). That is the same law one level up, and
  a stuck unit for f is such a coarse node made physical: its latency, rate and area are fixed by
  its design rather than by the walkers.

## 2. Prior work

### Area, time and the length of wires

- **Area–time.** Thompson bounds
  ["the product of chip area A with the square of the time T"](https://www.cs.auckland.ac.nz/~cthombor/Pubs/cmu-cs-80-140/cmu-cs-80-140ocr.pdf#page=5)
  for sorting and Fourier transforms (AT² = Ω(N² log² N)), because
  ["the area A of a chip is at least proportional to the square of the width of its communication graph"](https://www.cs.auckland.ac.nz/~cthombor/Pubs/cmu-cs-80-140/cmu-cs-80-140ocr.pdf#page=6)
  (thesis, CMU 1980). On our adders at 16 bits (plain runs) relay on a list has the least A·T and
  relay on a tree the least A·T².
- **Adders.** Brent and Kung show
  ["addition of n-bit numbers can be performed in time O(log n)"](https://maths-people.anu.edu.au/~brent/pd/rpb060.pdf#page=1)
  in area O(n log n), and that n-bit numbers
  ["can be added in time O(n/w + log w)"](https://maths-people.anu.edu.au/~brent/pd/rpb060.pdf#page=1)
  in area O(w log w + 1)
  ["if the input bits from each operand are available w at a time"](https://maths-people.anu.edu.au/~brent/pd/rpb060.pdf#page=1)
  (IEEE TC 1982): serial reuse of a width-w unit against a full-width one, as one formula. Their
  multiplication bounds say outright
  ["We do not need to assume that the propagation time increases with the length of the wire"](https://maths-people.anu.edu.au/~brent/pd/rpb055.pdf#page=3)
  (JACM 1981); with that assumption a log-depth adder is log-time.
- **Wire delay proportional to length.** Chazelle and Monier's model is
  ["based on the assumption that time for propagating information is at least linear in the distance"](https://www.cs.princeton.edu/~chazelle/pubs/ModelComputVLSI.pdf#page=1),
  with long wires decomposed
  ["into pieces of constant length connected by nodes"](https://www.cs.princeton.edu/~chazelle/pubs/ModelComputVLSI.pdf#page=5)
  (JACM 1985): the lattice's strands exactly. In it fan-out costs distance
  (["the maximum distance between a point and N arbitrary points in the plane is at least"](https://www.cs.princeton.edu/~chazelle/pubs/ModelComputVLSI.pdf#page=5)
  of order √N), so a unit serving N callers in the plane has its farthest caller √N away, and
  ["the model clearly favors regular and pipelined architectures"](https://www.cs.princeton.edu/~chazelle/pubs/ModelComputVLSI.pdf#page=1).
  This is the √A term on trees, and why log depth "mostly does not show" on the lattice.
- **What scales.** Assuming
  ["signal transmission time is proportional to the length of the connection"](https://cs.brown.edu/research/pubs/techreports/reports/93/cs93-20.pdf#page=7),
  Bilardi and Preparata find
  ["scalability holds only for neighborly interconnections, such as the square mesh"](https://cs.brown.edu/research/pubs/techreports/reports/93/cs93-20.pdf#page=3),
  where
  ["latency hiding reduces for the mesh to time-multiplexing the s processes"](https://cs.brown.edu/research/pubs/techreports/reports/93/cs93-20.pdf#page=18)
  (Brown TR 1993). Rent's rule gives how wire length grows with a circuit's size: in the plane, as
  ["the average length L as a function of the number of logic blocks G"](https://www.cl.cam.ac.uk/research/srg/han/ACS-P35/readinglist/stroobandt_tutorial_slip_3.pdf#page=22)
  grows as G^(p−1/2) for Rent exponent p > 1/2 (Stroobandt's SLIP 2001 tutorial; the 1971 original
  by Landman and Russo has no open copy).
- **Spatial cost models.** In the spatial computer
  ["processors have locations on a conceptual two-dimensional grid, and their distance therein determines their communication cost"](https://arxiv.org/pdf/2205.04934#page=1),
  and
  ["the largest total energy of any chain of messages that consecutively depend on each other is the wire-depth"](https://arxiv.org/pdf/2205.04934#page=2)
  (Gianinazzi et al., 2022): our D with distance folded in. Margolus argues
  ["portions of our computation that need to communicate quickly must be physically located close together"](https://arxiv.org/pdf/comp-gas/9811002#page=25)
  ("Crystalline computation", 1998), and Ackley and Cannon that
  ["no fixed-cost global communications or arbitrary long range links are possible"](https://www.cs.unm.edu/~ackley/papers/hotos-11.pdf#page=1)
  (HotOS 2011).

### Work, depth and throughput

- **Brent's bound.** A computation done
  ["in time t with q operations"](https://maths-people.anu.edu.au/~brent/pd/rpb022.pdf#page=4) runs
  ["with p such processors"](https://maths-people.anu.edu.au/~brent/pd/rpb022.pdf#page=4) in time
  t + (q − t)/p (JACM 1974, ignoring communication). Our law has this form with p ≈ 1.4. Greedy list
  scheduling on n identical units is within 2 − 1/n of the best
  ([Graham, BSTJ 1966](https://ia801900.us.archive.org/27/items/bstj45-9-1563/bstj45-9-1563_text.pdf#page=19)).
- **Cost semantics.** Blelloch and Greiner
  ["add to the semantics of the language an accounting of costs, and then to prove a mapping of these costs into running time"](https://www.cs.cmu.edu/~blelloch/papers/BG96nesl.pdf#page=1),
  work and depth, mapped to
  ["O(w/p + d log p) time"](https://www.cs.cmu.edu/~blelloch/papers/BG96nesl.pdf#page=1) (ICFP
  1996). Space depends on the schedule:
  ["parallel pebblings of this graph correspond to different scheduling policies"](https://www.cs.cmu.edu/~blelloch/papers/SBHG08.pdf#page=1)
  (Spoonhower et al., ICFP 2008), as fork's space savings show here.
- **Interaction nets.** In a net
  ["all sequences of rewrites are of the same length"](https://eceasst.org/index.php/eceasst/article/download/2205/2376/2387#page=4),
  and a parallel step reduces
  ["all active pairs in a net"](https://eceasst.org/index.php/eceasst/article/download/2205/2376/2387#page=4)
  at once (Mackie & Sato, 2016): the idealised machine's W and D. HVM2 relies on the same
  invariance,
  ["giving us freedom to evaluate in parallel without generating extra work"](https://raw.githubusercontent.com/HigherOrderCO/HVM2/main/paper/HVM2.pdf#page=19).
  Whether the space of an interaction machine is a reasonable cost measure is open:
  ["space efficiency induces time inefficiencies, as it prefers re-computing to saving"](https://arxiv.org/pdf/2104.13795#page=1)
  (Accattoli, Dal Lago & Vanoni, LICS 2021). We found no cost model for interaction nets laid out in
  space.
- **Roofline and queues.**
  ["Attainable GFlops/sec = Min(Peak Floating Point Performance, Peak Memory Bandwidth x Operational Intensity)"](https://www2.eecs.berkeley.edu/Pubs/TechRpts/2008/Archive/EECS-2008-134.pdf#page=4)
  (Williams, Waterman & Patterson, 2008). Little's law,
  ["the average number of items in a queuing system, denoted L, equals the average arrival rate"](https://people.cs.umass.edu/~emery/classes/cmpsci691st/readings/OS/Littles-Law-50-Years-Later.pdf#page=1)
  times the wait,
  ["holds independent of queue discipline"](https://people.cs.umass.edu/~emery/classes/cmpsci691st/readings/OS/Littles-Law-50-Years-Later.pdf#page=3)
  (Little, 2011).

### Reusing fixed units against replicating them

- **Systolic arrays** move data past fixed cells:
  ["once a data item is brought out from the memory it can be used effectively at each cell it passes"](https://web.archive.org/web/20220120231116id_/http://www.cs.virginia.edu/~smk9u/CS6501F19/kung_-_1982_-_why_systolic_architectures.pdf#page=3)
  (Kung, IEEE Computer 1982). Reuse by flow: the data travels, the units stay.
- **Spatial computation** replicates instead:
  ["computation structures are never shared, and each program operation is synthesized as a different functional unit"](https://www.cs.cmu.edu/~mihaib/research/asplos04.pdf#page=1),
  because
  ["ASH replicates computation units to simplify interconnect"](https://www.cs.cmu.edu/~mihaib/research/asplos04.pdf#page=1)
  (Budiu et al., ASPLOS 2004): today's lattice, which copies every use.
- **Hard blocks in a general fabric**: FPGAs (Kuon & Rose, section 3).
- **Hot code in fixed units, a general core as the fallback.**
  ["The c-cores target hot regions of specific applications that represent significant fractions of the target system’s workload"](https://www-new.llvm.org/pubs/2010-03-ASPLOS-ConservationCores.pdf#page=3),
  and
  ["The CPU serves as a fallback for parts of applications that are not important enough to be supported by the c-cores"](https://www-new.llvm.org/pubs/2010-03-ASPLOS-ConservationCores.pdf#page=3);
  functions are
  ["tagged for conversion into c-cores based on profile information"](https://www-new.llvm.org/pubs/2010-03-ASPLOS-ConservationCores.pdf#page=8)
  (Venkatesh et al., ASPLOS 2010). The motive is dark silicon:
  ["Even at 22 nm (just one year from now), 21% of a fixed-size chip must be powered off"](https://research.cs.wisc.edu/vertical/papers/2011/isca11-darksilicon.pdf#page=1)
  (Esmaeilzadeh et al., ISCA 2011). This is the stuck-silicon arrangement, with the lattice as the
  fallback.
- **Where replicas go.** Facility location:
  ["determine a set of locations at which to open facilities so as to minimize the total facility and assignment costs"](https://ics-archive.science.uu.nl/research/techreps/repo/CS-1997/1997-39.pdf#page=1)
  (Shmoys, Tardos & Aardal, STOC 1997, within 3.16 of the best); now
  ["a 1.488-approximation algorithm for the metric uncapacitated facility location (UFL) problem"](https://www.cs.princeton.edu/techreports/2014/969.pdf#page=3),
  and
  ["Both UFL and k-median are NP-hard"](https://www.cs.princeton.edu/techreports/2014/969.pdf#page=16)
  (Li, thesis 2014). Online, without a profile: when
  ["The cost of satisfying a request is the distance from the requested node to the nearest node with the page"](https://www.cs.cmu.edu/~sleator/papers/migration-problems.pdf#page=5),
  count what requests have paid and copy once it reaches the copy's price
  (["When this reaches r the page is replicated"](https://www.cs.cmu.edu/~sleator/papers/migration-problems.pdf#page=5)),
  which is 2-competitive, and
  ["the lower bound on the competitive factor for this problem is 2"](https://www.cs.cmu.edu/~sleator/papers/migration-problems.pdf#page=4)
  (Black & Sleator, CMU 1989).

### Compiling to fixed hardware

- **Scheduling against units.**
  ["Operation scheduling determines the serial/parallel trade-offs of the design, which approximately determines the cost-speed trade-offs"](https://web.archive.org/web/20240806002203id_/https://ocw.snu.ac.kr/sites/default/files/MATERIAL/2494.pdf#page=1),
  and
  ["if there is a limit on chip area, the scheduler can be asked to serialize operations"](https://web.archive.org/web/20240806002203id_/https://ocw.snu.ac.kr/sites/default/files/MATERIAL/2494.pdf#page=1)
  (Paulin & Knight, IEEE TCAD 1989). With the units fixed,
  ["resource-constrained scheduling problem is NP-hard in general"](https://web.archive.org/web/20140616062817id_/http://www.csl.cornell.edu/~zhiruz/pdfs/sdc-dac2006.pdf#page=3)
  (Cong & Zhang, DAC 2006). A pipelined loop's rate is set by
  ["a critical resource that is fully utilized or a critical chain of dependences running through the loop iterations"](https://web.archive.org/web/20210609180212id_/https://www.hpl.hp.com/techreports/94/HPL-94-115.pdf#page=12)
  (Rau, HP Labs 1994): the law's b·W against a·D.
- **Hardware/software partitioning.** Gupta states
  ["the co-synthesis problem as a partitioning problem under constraints"](https://si2.epfl.ch/demichel/graduates/theses/rajesh.pdf#page=5),
  whose exact search
  ["is typically exponentially related to the number of operations"](https://si2.epfl.ch/demichel/graduates/theses/rajesh.pdf#page=198)
  (thesis, Stanford 1993). COSYMA's
  ["simulated annealing starts with an all software solution"](https://cecs.uci.edu/~papers/compendium94-03/papers/1994/iccad94/pdffiles/02b_2.pdf#page=1)
  and moves code into hardware until deadlines hold, by an estimate that
  ["considers the potential speedup and communication costs"](https://cecs.uci.edu/~papers/compendium94-03/papers/1994/iccad94/pdffiles/02b_2.pdf#page=2)
  (Henkel et al., ICCAD 1994).
- **Custom instructions.** Atasu, Pozzi and Ienne pick
  ["maximal-speedup convex subgraphs of the application"](https://www.epfl.ch/labs/lap/wp-content/uploads/2018/05/AtasuJun03_AutomaticApplicationSpecificInstructionSetExtensionsUnderMicroarchitecturalConstraints_DAC03.pdf#page=1)
  under limits on register ports;
  ["Although still exponential, the algorithm reduces in practice the search space very tangibly"](https://www.epfl.ch/labs/lap/wp-content/uploads/2018/05/AtasuJun03_AutomaticApplicationSpecificInstructionSetExtensionsUnderMicroarchitecturalConstraints_DAC03.pdf#page=4)
  (DAC 2003).
- **Geometry of Synthesis** is the closest to stuck silicon.
  ["Every sub-term of the program, when synthesised, becomes a physical resource which must be managed"](https://www.irif.fr/~mellies/mpri/mpri-ens/articles/ghica-geometry-of-synthesis.pdf#page=1),
  and
  ["every procedure call, involves some potentially dangerous sharing of circuitry"](https://www.irif.fr/~mellies/mpri/mpri-ens/articles/ghica-geometry-of-synthesis.pdf#page=2)
  (Ghica, POPL 2007). Type inference
  ["is used to statically determine "concurrency bounds" on all identifiers in the program"](https://research.birmingham.ac.uk/en/publications/geometry-of-synthesis-iii-resource-management-through-type-infere-2/#main-content),
  after which
  ["serialization"](https://research.birmingham.ac.uk/en/publications/geometry-of-synthesis-iii-resource-management-through-type-infere-2/#main-content)
  makes every bound one (Ghica & Smith, POPL 2011; only the abstract is open). Later
  ["the compiler can decide whether the circuit for a particular function must be reinstantiated or can be reused"](https://www.microsoft.com/en-us/research/wp-content/uploads/2011/09/Compiling-Affine-Recursion-into-Static-Hardware.pdf#page=2),
  inlining being
  ["replication of circuitry"](https://www.microsoft.com/en-us/research/wp-content/uploads/2011/09/Compiling-Affine-Recursion-into-Static-Hardware.pdf#page=2)
  (Ghica, Smith & Singh, ICFP 2011), and a shared circuit is reached through
  ["a stateful multiplexer-demultiplexer"](https://www.microsoft.com/en-us/research/wp-content/uploads/2011/09/Compiling-Affine-Recursion-into-Static-Hardware.pdf#page=6).
  What it lacks for the lattice is distance: its multiplexers cost nothing to reach.

### Coarse-graining

- **Exact coarse-graining of automata.** In Israeli and Goldenfeld's scheme
  ["nearby cells are grouped into a supercell according to some specified rule"](https://arxiv.org/pdf/nlin/0309047#page=1),
  and they require that running then projecting
  ["is equivalent to projecting the initial condition and then running the renormalized CA"](https://arxiv.org/pdf/nlin/0508033#page=4)
  (PRL 2004, PRE 2006). Their supercells advance in lockstep; the lattice's projection onto the net
  commutes in state but not in time (section 1).
- **Hydrodynamic limits.** Random-walk lattice gases give deterministic equations
  ["as laws of large numbers from an underlying microscopic stochastic dynamics in the diffusive scaling limit"](https://arxiv.org/pdf/1404.6466#page=5),
  holding distance² over time fixed (the heat equation); a biased one holds distance over time
  fixed, ["keeping x/t fixed"](https://arxiv.org/pdf/1404.6466#page=32) (Bertini et al., Rev. Mod.
  Phys. 2015). Which scaling applies is the Péclet number,
  ["a dimensionless measure of the influence of the bias relative to diffusive fluctuations"](https://arxiv.org/pdf/2201.10048#page=8)
  (Redner); with drift the mean time to meet is ℓ/|v| and
  ["Diffusion plays no role in this average hitting time"](https://arxiv.org/pdf/1807.07651#page=2)
  (Krapivsky & Redner, 2018). The lattice's 2.9 and 29 clocks for one and three strands put its
  walkers on the diffusive side at these distances.
- **Renormalisation of reactions.** Wires are one-dimensional, and
  ["For spatial dimension d ≤ 2 random walks are re-entrant, which enables a pair of particles to find each other with probability one"](https://arxiv.org/pdf/cond-mat/0501678#page=5);
  above two dimensions coarser scales
  ["only serve to modify the rate constant in some nonuniversal way"](https://arxiv.org/pdf/cond-mat/0501678#page=8)
  (Täuber, Howard & Vollmayr-Lee, 2005). Read for the lattice (inference): the form of the law
  should survive changes to the energies while a and b, being such constants, move (the demand field
  halved the clocks), so they must be refitted for each design.

## 3. Stuck silicon in the model

### A unit, in the law's terms

A unit for a function f has an area α (sites, or standard cells on the chip), a latency ℓ (clocks
from arguments in to answer out), an initiation interval II (clocks between calls it can take; 1 if
pipelined) and an interface cost io (clocks to take in argument values and put out the answer, which
on the lattice are trees of agents). Callers reach it at a speed s: about 0.13 strands a clock if
arguments walk, 1 if they travel as pulses (the lattice has only one-bit demand pulses today). A
call of f then costs:
- **on the general lattice**: about a·D_f + b·W_f clocks if it is on the critical path, only b·W_f
  if it runs beside it, plus getting f's code there, about b per agent copied (duplicators are
  25–38% of rewrites even with movement agents);
- **at a unit r sites away**: ℓ + io + 2r/s, plus waiting if the unit is busy.

### Reuse against copies: the size of what is reused decides

A unit beats a copy when it is closer than r* = s·(G_f − ℓ − io)/2, with G_f the call's cost on the
general lattice.
- **A code node**, the grain at which the lattice copies today, costs one duplicator rewrite to
  copy, about b ≈ 0.7 clocks. A trip at walking speed costs about 15 clocks a site. Even if a copy
  cost a whole round, r* would be about 1.5 sites, so copying wins. Reading one fixed copy node by
  node is worse still, a round trip per node. That is what `research/LINEAR_TREE_NETS.md` ("Reading
  code without consuming it") measured: reads sit 9–64 sites from the loaded copy, and copies win.
- **A whole function** with much work per call is the opposite. An 8-bit add by relay unrolled costs
  about 6.2k clocks on the lattice (3.8k with movement agents). For a unit with ℓ + io of a few
  hundred clocks, r* ≈ 0.13 × 6,000 / 2 ≈ 390 sites even if arguments walk, about 3,000 if they
  travel as pulses. That program's working area is at most about 70 sites across (the square root of
  its 4.6k sites in use), so one unit could serve all of it.

So: harden whole functions that do a lot per call, and keep copying small code. Movement agents are
the small end of the same spectrum: six combinator shapes applied in one rewrite, still copied.

### How many replicas, and where

- **For load**: a unit is busy for call rate × II of the time, over k replicas. A whole computation
  does at most about 1.4 rewrites a clock, so calls to any one function arrive much more slowly than
  one a clock, and a pipelined unit is almost never busy: k = 1. An unpipelined unit (II about ℓ +
  io) needs k ≥ rate × II, plus about √(rate × II) spare against queueing: square-root staffing,
  where ["at least R agents"](https://ir.cwi.nl/pub/4385/04385D.pdf#page=6) are
  ["required to guarantee stability"](https://ir.cwi.nl/pub/4385/04385D.pdf#page=6) and the extra
  ["level is proportional to the square-root of the"](https://ir.cwi.nl/pub/4385/04385D.pdf#page=11)
  offered load (Borst, Mandelbaum & Reiman, 2000/2004).
- **For distance**: with a travel budget t a unit serves callers within r = s·t/2, and covering a
  region whose footprint is F columns (the lattice is 8 sites deep) takes k ≈ F/(π·r²). With t one
  round (23 clocks), walking (r ≈ 1.5) needs a unit for every 7 or so columns, a copy beside nearly
  every use: today's lattice. Pulses (r ≈ 11) need one for about 400 columns: a handful for a
  program the size of fib(2), whose peak is 2,000–4,000 sites.
- **The two bounds explain each other.** In time only 6–33% of code copies are ever needed at once
  (`research/LINEAR_TREE_NETS.md`, "copies against need"); in space, at walking speed, nearly all
  are. Reuse needs a coarse grain and fast access, both.
- **Where** (inference, a continuum estimate): with calls at density ρ per column, unit area α
  priced at p clocks a site (for the area–time product, p = T/A), and the nearest of n units a
  column on a grid about 0.38/√n columns away, a column costs ρ·0.76/(s·√n) + p·α·n. The best
  density is n = (0.38·ρ/(s·p·α))^(2/3): units follow the calls to the two-thirds power, so twice
  the calls get 1.6× the units, not 2×. A chip fixes its units before it knows the program, so it
  tiles them evenly, as FPGAs set hard multiplier and memory blocks among their lookup tables; there
  such ["“hard” blocks"](https://www.eecg.utoronto.ca/~jayar/pubs/kuon/kuontcad06.pdf#page=1)
  ["reduce this average area gap significantly to as little as 18"](https://www.eecg.utoronto.ca/~jayar/pubs/kuon/kuontcad06.pdf#page=1),
  from 35 against custom chips (Kuon & Rose, 2007).

### When hardening pays: a 16-bit add

- **On the general lattice**: 9.6k clocks and 9.7k sites at the peak by relay unrolled, 14.3k clocks
  and 3.6k sites by relay (with movement agents 5.6k and 4.7k, 9.0k and 2.3k).
- **On the chip** that is hundreds to thousands of block units: one is 87,000 standard cells for 8
  sites (`hw/README.md`), about 11,000 cells a site. A 16-bit adder as a circuit is on the order of
  a hundred cells and settles well inside the chip's 50 ns clock (inference).
- **The cost moves to the interface.** A 16-bit number tree is 31 agents, so a call reads two and
  writes one, about 95 agents. At around an agent a clock that is 100–300 clocks a call: 30–100×
  faster than the general lattice, in a few sites instead of thousands (inference: no interface is
  designed).
- **Rule of thumb**: hardening pays when the work per call per agent of interface (W_f / io, the
  roofline's operational intensity) is high and calls are many, and most on the critical path, where
  a call costs a·D_f and not just b·W_f. Code run once gains at most its one run, and only on the
  critical path.

### What a unit is in the net

- An agent kind U_f whose rule with an apply takes the argument, gives f's answer after ℓ clocks,
  and leaves U_f in place. One unit serves several callers through a **merge**: an agent with two
  inputs that passes on whichever request comes first. That is two principal ports, which
  interaction nets rule out because which pair fires is no longer determined: a multiport cell
  ["can in general be involved in several active pairs"](https://lipn.univ-paris13.fr/~mazza/papers/mINSAndConcurrency-CONCUR05.pdf#page=4)
  (Mazza, CONCUR 2005). Here only the timing becomes nondeterministic: f is a pure function, so
  every order of service gives the same answers (inference). This is Geometry of Synthesis's
  multiplexer (section 2), as agents.
- **A spectrum**: movement agents (fixed shapes, one rewrite, copied) → one rule per function, as
  supercombinators and HVM2's REF nodes (a rewrite on the lattice must seat its products in a 2×2
  square, so a big body cannot be expanded at once) → persistent units, which seat only the answer.
- **Calls, not reads.** A fixed piece of code read node by node by a token pays a trip per node:
  that is the reading-without-consuming `research/LINEAR_TREE_NETS.md` found loses on the lattice. A
  unit's interface has to be a whole call.

## 4. Compiling to a hardware model

### The problem

- **Given**: the program's calls from a profile (for each call of a function f: its rounds D_f,
  rewrites W_f and interface size on the general lattice, and which calls wait on which); the law's
  constants and the two speeds; a library of units (for each candidate f: area α_f, latency ℓ_f,
  interval II_f, interface io_f); an area budget, or an objective trading area for time (A·T, or
  Thompson's A·T²).
- **Choose**: which functions get units, how many replicas each, where they sit, and which calls go
  to which replica in what order.
- **Cost**: the longest path through the calls, a call of f costing a·D_f + b·W_f on the general
  lattice or ℓ_f + io_f + 2r/s plus its wait at a unit, plus b per rewrite beside the path.

### How hard it is

- **Ordering calls on replicas** with dependencies between them is scheduling with precedence on m
  machines: NP-hard
  ([Graham, Lawler, Lenstra & Rinnooy Kan's survey](https://ir.cwi.nl/pub/9688/9688D.pdf#page=20),
  after Ullman 1975), though
  ["The problem is in P, however, if the precedence relation is of the tree-type or if m = 2"](https://ir.cwi.nl/pub/9688/9688D.pdf#page=20).
  Greedy list scheduling is within 2 − 1/m of the best (Graham, section 2).
- **Choosing what to harden** to shorten the longest path within an area budget is a discrete
  time–cost trade-off. The linear version (cost linear in time saved) is solved exactly by network
  flow
  (["A network-flow method for solving the linear-programming problem of computing the least-cost curve"](https://www.rand.org/pubs/papers/P1947.html#RANDTitleHeadingId),
  Fulkerson 1960), but
  ["its discrete variant is known to be NP-hard"](https://api-depositonce.tu-berlin.de/server/api/core/bitstreams/d77b476f-37a7-4bbe-9717-7b2f76199a3e/content#page=2)
  (Skutella 1997), with constant-factor approximations for restricted cases and, in general, a
  result
  ["that under the same assumptions no constant-factor approximation algorithm exists for general time-cost tradeoff instances"](https://arxiv.org/pdf/2011.02446#page=1)
  (Daboul, Held & Vygen 2021, after Svensson). Ours differs in one way that makes it harder (one
  unit speeds every call of f at once) and one that makes it easier (few distinct functions).
- **Placing replicas** is facility location: NP-hard, within 1.488 (section 2).
- **Units smaller than a function** need subgraph selection, exponential in the worst case and
  practical in fact (Atasu et al., section 2).
- High-level synthesis meets all of these every day and answers with list scheduling, force-directed
  scheduling and integer programs (section 2).

### What the coarse law buys

- **Closed-form costing.** A candidate is costed by one longest-path pass over the calls, instead of
  a lattice run of minutes to hours.
- **The parts come apart.** With the hardened set fixed, replica counts and places follow from the
  load and distance bounds of section 3. Ordering is the lattice's own (demand); where a unit is
  shared the compiler can at most give calls a priority, least slack first.
- **Small instances.** A disp program's code, hash-consed, is 49–194 entries
  (`research/LINEAR_TREE_NETS.md` §4b), and the functions worth a unit are far fewer, so choosing
  them can be exhaustive or a small integer program.

So the problem is NP-hard in general and practical at disp's sizes. The weak point is the law: ×1.2
typical and ×2.5 at worst, with b varying 5× between programs. A compiler would refit a and b on the
program at hand from a few small runs.

### An automatic compiler for disp (inference; nothing built)

1. **Profile** on small inputs: the idealised machine (`strands-bench --ideal`) for D and W, which
   would need charging to calls (new), or the eager evaluator's memo cache, whose entries already
   store each `apply(f, x)` fact's cold cost and its parent (`AGENTS.md`): a call tree with costs,
   in eager steps.
2. **Candidates**: each entry of the hash-consed code table is a closed function, a natural unit
   boundary. Rank by calls × (a·D_f + b·W_f) / io_f, and by how often a call is on the critical
   path.
3. **Unit models**: a soft unit is the function laid out for the shapes it is called with and kept
   (`relay_for` is this, done by hand, used once); a hard unit is a circuit, sized by synthesis
   (`hw/flow/size.sh` reports cells per stage). Aladdin does this step for accelerators: it
   ["uses dynamic data dependence graphs (DDDG) as a representation of an accelerator without having to generate RTL"](https://people.eecs.berkeley.edu/~ysshao/assets/papers/shao2014-isca.pdf#page=1)
   and
   ["estimates performance, power, and area of accelerators within 0.9%, 4.9%, and 6.6% with respect to RTL implementations"](https://people.eecs.berkeley.edu/~ysshao/assets/papers/shao2014-isca.pdf#page=1)
   (Shao et al., ISCA 2014).
4. **Choose** the units, replicas and places as above, against the area–time objective.
5. **Emit**: the loader places the units and turns each hardened call into a call agent wired toward
   its replica.
6. **Check** answers against the eager evaluator as `programs/bench.ts` does, measure clocks, and
   refit a and b.

A hardened function is another implementation of the same definition, which is the role `.opt.disp`
files already have for faster code (inference: nothing there targets hardware).

## 5. First experiments

### 0. The law from the lattice's own trace (no code change)

- Run `TRACE=file strands-run TERM --latest --fit`, memory-capped, on six programs spanning width:
  unary `add 15 1`, relay on a 16-bit list and tree, relay unrolled on 8- and 32-bit trees, disp
  `fib 3`. The trace (`Trace` in `lattice.rs`) gives each rewrite's clock, when its pair first
  existed and which rewrite made it, when its reader was first wanted, and how far apart the pair
  was. The "which rewrite made it" links are the dependency graph the lattice actually ran.
- Measure: (1) each link's delay, split into waiting for demand and closing the gap, against a and
  against the walker's distance law; (2) whether a link's delay grows with the number of other ready
  pairs within a few sites (congestion) or with its distance from the computation's centre (late
  start); (3) the longest path through the graph with every delay replaced by one drawn at random
  from all links: if that reproduces the clocks, the width cost is chance; if it comes out short, it
  is congestion.
- Gives a and b from microscopic statistics rather than a fit, and says which mechanism a hardware
  design should attack. Minutes of CPU.

### 1. One adder unit, many callers (needs a new agent kind; CPU simulator only)

- **Status: built and taken out** (evaluators/rust-ic-strands README, "Things tried"): a unit is not
  local. Callers found it by its number (a pointer, looked up in a table), and it read its arguments
  through the abstract net. Measured before it went: a call's cost is the walk, about 50 clocks a
  site wandering (a third of the 0.13 strands a clock assumed below), under 4 steered; one unit
  beat copies out to 100–120 sites wandering, 340–1,600 steered; copies stayed nearly flat in m
  (6.1k to 7.8k clocks for 1 to 8 calls of `relay_add` on 4-bit trees).

- **Workload**: m independent 8-bit additions in one program (inputs worked out first, as
  `bench.ts --data` does), m = 1, 2, 4, …, 64.
- **Configurations**: (i) `relay_add` as compiled, its code copied for each call; (ii) `relay_for` 8
  bits per call, a laid-out circuit copied per call; (iii) one unit `U_add8` placed by the loader:
  latency ℓ ∈ {1, 10, 100} clocks, its answer computed by the eager evaluator as an oracle and
  seated beside it one agent a turn, arguments walking to it along the wires the program's
  duplicators would have used, callers merged first come first served; (iv) k = 2 and 4 units spread
  evenly. CPU only, like the memo detector; the chip and GPU would refuse it.
- **Predictions**: (i) and (ii) take clocks flat in m (the parts run side by side; the width cost is
  per computation), about 6.2k for (ii), with sites in use growing in proportion to m. (iii) takes
  about 2r/0.13 + ℓ + m·II, linear in m, with II the time to read the arguments and seat the answer,
  and sites flat in m. The crossover with (ii) is at m ≈ (6.2k − travel − ℓ)/II: about 30–60 for II
  of 100–200 clocks, and k times that with k units. The queue at the unit should obey Little's law,
  callers waiting = arrival rate × time in the system.
- **Tests**: what a call to a unit really costs (is it ℓ + io + 2r/s?), whether the general lattice
  really stays flat as copies multiply, the crossover, and the area–time product. If walking
  arguments make travel dominate, a second version sends them as pulses, which the law predicts cuts
  travel 7–8× and needs new state per site.

## Reproducing

The fits read the adder sweep's `summary.csv` (lattice: clocks, rewrites, peak sites, agents at
load) and `summary_ideal.csv` (idealised machine: rounds and rewrites), joined by call and setting,
and fit by least squares on log clocks (numpy and scipy). Both tables and the scripts are in
`research/coarse-lattice/` (`fits.md` is the sweep's per-adder exponents); run one with
`nix shell --impure --expr 'with import <nixpkgs> {}; python3.withPackages (p: [p.numpy p.scipy])' -c
python research/coarse-lattice/fit.py`. The sweep is rerun by `programs/bench.ts`
(README, "Adding binary numbers"). The eight test programs' numbers are copied from the README's
"Movement agents" table and `research/LINEAR_TREE_NETS.md` §6.
