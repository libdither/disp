# rust-ic-strands

disp's tree-calculus interaction net on a lattice where **wires are physical and nothing has
an address**. It is the answer to two problems with the earlier spatial machines:
- `rust-ca-lattice` ("the cascade") also used physical wires, but jammed. It completes 130 of
  160 random terms.
- `rust-ic-mesh` completes everything, but by using global addresses and a router in every
  tile.

Open `player/index.html` to watch it run (rebuild with `./build-player.sh`). Step one move at a
time with `+1` or `→` (shift: to the next rewrite), each move told in words; hover any site to
see what its agents are, where each of their wires leads and what they are waiting for (click
to pin). Garbage is drawn dimmed.

## The machine

- **Sites and strands.** Sites form a 2D or 3D grid. Each link between neighbouring sites
  carries up to L numbered *strands*. Strand i of a link is one piece of wire seen from both of
  its sites, so no site ever names anything elsewhere.
- **The switchboard.** A site holds up to K agents and a switchboard: a pairing of the ends
  present there, namely agent ports and strand ends. A wire is a path: port, switchboard,
  strand, switchboard, …, port. Two wires cross for free by passing through the same
  switchboard.
- **Moves.** Every move touches at most one 2×2 square of sites:
  - **step**: an agent moves to a neighbouring site. It eats the strand it walks along and
    drags its other wires one strand longer.
  - **exchange**: an agent trades places with an agent in a full site, so walkers get through
    crowds.
  - **fold**: a wire that leaves a site and comes straight back snaps shut.
  - **flip**: a wire's corner turns to the other side of its square.
  - **rewrite**: once a consumer and its producer meet (same site, or one strand apart), the
    rule fires inside a 2×2 square containing both, in any plane. A small search seats the
    fresh agents in the square's free slots and wires them with free strands. If there is no
    room, nothing changes and the pair waits.
- **Scheduling is thermal.** Moves are random local proposals, accepted Metropolis-style by an
  energy:
  - wire tension, heavier on principal wires, so reactants find each other;
  - crowded links: a link carrying n strands costs n²/2, so wire pulls harder where it would
    fill every lane;
  - crowded sites;
  - a temperature, which lets matter step out of the way.

  Most turns (80%) go to sites holding agents; bare wire only needs enough turns to straighten.
  Nothing schedules anything, and there are no rules against cycles.
- **Laziness.** Only *wanted* consumers react:
  - The normalizer, the root and erasers start out wanted.
  - A wanted reader puts a *demand pulse* on its principal wire. The pulse crosses one strand
    per clock, following the switchboards. When it reaches another computation's output, that
    computation is wanted too. Touching it by walking there does the same.
  - If the strand under a pulse is moved, the pulse is dropped and the reader sends another, so
    a pulse can never jump onto another wire.
  - In a rewrite, a fresh consumer is wanted when its output feeds a wanted reader.
- **Garbage.** An eraser that touches a computation's output collects it, by contact like a
  rewrite:
  - a duplicator becomes a plain wire from its input to its other output, and both agents
    vanish. Every consumer that reads a duplicator also has rules for whatever the duplicator
    itself would have read, so no pair without a rule can appear;
  - an apply, triage or dispatch is dead, so it and the eraser are replaced by two erasers, one
    on each of its inputs;
  - an unpair whose two parts are both being erased (erasers in its site or the next) becomes
    one eraser on the pair.

  None needs room, so all can always happen. Without them, lazy evaluation leaves erasers
  parked on garbage nobody wants, and it crowds out the reactions that matter. With them the
  machine cleans up completely: run on past the answer (`run_on`, `strands-run --clean`, and the
  player does it by itself) and only the answer is left on the lattice, usually within a few
  dozen clocks.

Every rewrite and every collection is replayed on the abstract net, and a rewrite must be a
pair the net has. At the end, the lattice is compared with the abstract net wire by wire.
Tests also re-check every invariant after every single move.

## Two schedules, and what a clock is

- **Random turns** (an asynchronous chip): one site at a time makes a move. A *clock* is one
  turn for the busiest site, since a chip gives every site at most one turn per clock. This
  counts the extra turns agent sites get.
- **Blocks** (`--margolus`, a synchronous chip): every clock the lattice is cut into 2×2×2
  blocks at a random offset (the Margolus neighbourhood of classic physical cellular automata),
  and every block holding matter makes one move inside it. No two moves ever touch the same
  site, so the clock count is literal.

Blocks cost 5–7× more clocks than random turns: each site gets a fraction of a move per
clock, and a walker's next strand leads out of its block half the time.

An earlier version measured time as proposals divided by live sites, which ignored extra turns.
It made "self-propelled walkers" (extra turns for wanted readers) look like a 4× win; counted
honestly they are 25× slower, and they are gone.

## What the exploration found

Measured on the cascade's 160-term soak corpus (`strands-sweep`) and on the lambada benchmark
programs (`strands-run`).

**Temperature does the work that rules did in the cascade.** At 2 slots per site, temperature
0.6 leaves terms stuck, and 1.2–2.0 completes all 160. The cascade needed relief rungs,
anti-cycle orders and stamps to approach that; here random jiggling escapes every jam the
corpus produces.

**Small sites are enough for small terms.** Corpus completion by configuration (all answers
correct in every run):

| space | agents/site | strands/link | rewrites in | done of 160 | site size |
|---|---|---|---|---|---|
| 2D | 1 | 4 | site + neighbours | 160 | ~99 bits |
| 2D | 1 | 3 | site + neighbours | 147 | ~64 bits |
| 2D | 2 | 3 | 2×2 square | 159; 160 lazy with exchanges and pulses | ~98 bits |
| 2D | 2 | 2 | 2×2 square | 158 | ~64 bits |
| 3D | 1 | 3 | site + neighbours | 160 | ~109 bits |
| 3D | 2 | 2 | 2×2 square | 160 | ~98 bits |
| 3D | 2 | 3 | 2×2 square, blocks schedule | 160 | ~135 bits |

Rewriting inside a 2×2 square is both more local and better than spilling into a site's four
neighbours (158 vs 146 at 2 slots, 2 strands).

**Real programs need wire capacity, and that means 3D.**
- **2D jams.** Lazy fib stalls after a few dozen rewrites. Diagnosis: the reaction zone fills
  with fresh agents (every site full) and its links fill with wires (no free strand to drag),
  so a wanted walker 15 strands from its target cannot advance. Neither stronger crowding,
  pressure, repulsion nor weaker tension on idle matter cured it. They only move the jam. With
  4 strands per link, collection and crowded links, 2D fib(0) and fib(1) do finish, in 50k and
  79k clocks (7–11× 3D); with 3 strands per link they still jam.
- **3D works**, with six links per site: 2 agents and 4 strands per link, about 165 bits per
  site with the pulse.

**Where the time goes.** `strands-run --profile` samples what every wanted reader is waiting
on. On fib(0), before pulses:
- some pair was ready to react during only 17% of clocks: lazy evaluation is close to
  sequential, so each step's latency is the whole cost;
- readers carrying demand to a computation spent most of the rest, walking ~9 strands to touch
  it;
- readers walking to the value they need took most of the remainder, ~4 strands.

Each fix took a share of that, on fib(0) with random turns:

| change | clocks |
|---|---|
| exchanges off | 54k |
| exchanges on | 24k |
| + most turns to agent sites | 19k |
| + demand pulses | 13–15k |
| + rewrite squares in any plane (they were all horizontal, so 3D pairs had to share a layer) | 10–12k |
| + erasers collect duplicators (below) | 9.7k |
| + erasers collect dead computations | 8.1–9.1k |
| + erasers collect unpairs, so garbage cascades all the way | 6.5–7.2k |

**Garbage blocks large programs.** fib(2) froze after 3,306 rewrites: the one rewrite that
mattered (a duplicator meeting a fork, the largest rule) had no room, because 196 erasers were
parked on duplicator outputs around it. Collecting duplicators removes them and also saves
work: fib(0) drops from 830 rewrites to 556. Collecting dead applies too takes it to 454, and
fib(2) from 5,451 to 3,186.

**Then wires swell.** With collection, fib(2) froze again at 2,246 rewrites. This time a wanted
reader sat 2–4 strands from its value for 1.7M clocks while wire grew from 31k to 156k strands.
At temperature 2 a wire's entropy outweighs its tension: in 3D a path has about 5 ways to
continue, so with aux tension 1 an extra strand costs 1 − 2·ln 5 < 0. Wire grows until every
lane near the reaction is full, and a reader cannot step without a free lane for its other
wires. The threshold is sharp: at aux tension 3, just below 2·ln 5 ≈ 3.2, fib(2) still swells
(45k to 167k strands) and freezes at 4,970 rewrites; at 4, wire stays near 43k strands and
fib(2) finishes, in 455k clocks.

Strong tension everywhere costs up to 2.3× on small programs, because compact means crowded.
Charging crowded links instead costs nothing there: wire stays loose in open space, but adding
a strand to a link that already holds n costs n more, which stops swelling exactly where lanes
would fill. fib(2) then takes 124–147k clocks.

**Against the address-based mesh.** Clocks to the answer with pulses, all collection and
crowded links (c = 1), over 1–2 seeds; every run matches the oracle and the abstract net, and
afterwards cleans up to only the answer. Rewrites include erasing garbage, as the mesh's do:

| program | mesh rewrites | mesh ticks | strand rewrites | random turns | blocks |
|---|---|---|---|---|---|
| fib(0) | 1,627 | 7,450 | 1,067 | 6.5–7.2k | 43k |
| fib(1) | 1,821 | 8,928 | 1,094 | 6.9–7.1k | 45k |
| sort(1) | 2,174 | 2,615 | 210 | 2.7k | 13k |
| exp(1) | 15,139 | 100k | 3,453 | 46k | — |
| fib(2) | 20,808 | 137k | 3,569 | 60–61k | — |

With random turns the strand lattice is level with the mesh on sort(1), slightly faster on
fib(0) and fib(1), and over twice as fast on exp(1) and fib(2). With blocks it is 5–7× slower
than with random turns. A mesh tile is about 5 kbit plus a router; a strand site here is about
165 bits, and fib(2) peaks at about 2,000 sites in use.

## Things tried that did not help, and why

- **Pressure** from blocked rewrites (a diffusing field agents drift down): no measurable change
  on the corpus or on fib. Wire tension holds neighbours in place at least as strongly.
- **Short-range repulsion**: same.
- **Weak or no tension on idle matter**: wires grow without bound and the reaction zone still
  congests.
- **Splitting big rules** so each step creates at most two agents, with a temporary "builder"
  agent decaying one step at a time. I searched every rule for its best decay order:
  - Dn·S, Nrm·F, Sel·S and Sel·F need 3-port builders;
  - T1·S needs a 4-port builder;
  - T1·F and Dn·F need 5-port builders.

  Five ports would make every switchboard bigger, so this stayed an analysis.
- **Walker priority.** Letting a wanted reader take its site's turn: neutral at 50%. At 100%
  it livelocks: a reader whose rewrite has no room retries forever and its site never does
  anything else to make room.
- **Under blocks**: a higher step rate is worse (wires need reshaping turns too), and letting
  a ready rewrite go first in its block changes nothing.
- **Eager evaluation** finishes fib(0) in 11k clocks, but with about 67× the rewrites.

## Open

- **The synchronous schedule** pays one move per block per clock. Block rules that make several
  moves at once (a walker eating every strand of its wire inside the block) would close part
  of the gap, at the cost of bigger block logic.
- **Bits.** The switchboard dominates site size: 30 ends at 5 bits in the 3D configuration.
  Most pass-throughs run straight across a site, so an encoding where "straight" is free could
  roughly halve it.

## Running it

From `crate/` (memory-cap long runs, see `AGENTS.md`):

```sh
cargo test --release                                  # ~5 s: corpus in 4 configurations, per-move invariants, collection
cargo run --release --bin strands-run -- disp-t --k 2 --lanes 3 --block --lazy --temp 2
cargo run --release --bin strands-run -- fib:0 --grid 256 --depth 8 --k 2 --lanes 4 --block --lazy --temp 2 --swap 1 --agents 0.8 --pulse --gc --link 1
cargo run --release --bin strands-run -- fib:0 --grid 256 --depth 8 --k 2 --lanes 4 --block --lazy --temp 2 --swap 1 --agents 0.8 --pulse --gc --link 1 --margolus
cargo run --release --bin strands-run -- fib:2 --grid 300 --depth 8 --k 2 --lanes 4 --block --lazy --temp 2 --swap 1 --agents 0.8 --pulse --gc --link 1 --budget 5000000000 --clean 100000
cargo run --release --bin strands-sweep -- "k=2 lanes=2 temp=2.0 grid=48 block=1" "k=2 lanes=3 temp=2.0 grid=48 depth=6 block=1 lazy=1 pulse=1 swap=1 agents=0.8 gc=1 link=1"
```

`strands-run --clean N` runs on past the answer (up to N clocks) and reports when only the
answer is left; `PIECES=1` lists the connected pieces of the net at the end.
`strands-run --profile` prints what wanted readers spend their time waiting on;
`--progress N` prints the wanted readers and their wire lengths every N proposals, and `WHO=1`
adds what each one is waiting on, which is how the 2D jam was diagnosed.
