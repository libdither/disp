# rust-ic-strands

disp's tree-calculus interaction net on a lattice where **wires are physical and nothing has
an address**. It is the answer to two problems with the earlier spatial machines:
- `rust-ca-lattice` ("the cascade") also used physical wires, but jammed. It completes 130 of
  160 random terms.
- `rust-ic-mesh` completes everything, but by using global addresses and a router in every
  tile.

Open `player/index.html` to watch it run (rebuild with `./build-player.sh`).

## The machine

- **Sites and strands.** Sites form a 2D or 3D grid. Each link between neighbouring sites
  carries up to L numbered *strands*. Strand i of a link is one piece of wire seen from both of
  its sites, so no site ever names anything elsewhere.
- **The switchboard.** A site holds up to K agents and a switchboard: a pairing of the ends
  present there, namely agent ports and strand ends. A wire is a path: port, switchboard,
  strand, switchboard, …, port. Two wires cross for free by passing through the same
  switchboard.
- **Moves.** Every move touches at most one 2×2 block of sites:
  - **step**: an agent moves to a neighbouring site. It eats the strand it walks along and
    drags its other wires one strand longer.
  - **exchange**: an agent trades places with an agent in a full site, so walkers get through
    crowds.
  - **fold**: a wire that leaves a site and comes straight back snaps shut.
  - **flip**: a wire's corner turns to the other side of its square.
  - **rewrite**: once a consumer and its producer meet (same site, or one strand apart), the
    rule fires inside a 2×2 block containing both. A small search seats the fresh agents in the
    block's free slots and wires them with free strands. If there is no room, nothing changes
    and the pair waits.
- **Scheduling is thermal.** Moves are random local proposals, accepted Metropolis-style by an
  energy:
  - wire tension, heavier on principal wires, so reactants find each other;
  - crowding;
  - a temperature, which lets matter step out of the way.

  Nothing schedules anything, and there are no rules against cycles.
- **Laziness by contact.** Only *wanted* consumers react:
  - The normalizer, the root and erasers start out wanted.
  - A wanted reader that reaches another computation's output makes that computation wanted.
  - In a rewrite, a fresh consumer is wanted when its output feeds a wanted reader.

  Demand travels with the walkers; nothing is signalled along wires.
- **Active matter.** Wanted consumers get turns of their own (self-propelled); idle matter
  only gets thermal turns.

Every rewrite is replayed on the abstract net and must be a pair it has. At the end, the
lattice is compared with the abstract net wire by wire. Tests also re-check every invariant
after every single move.

## What the exploration found

Measured on the cascade's 160-term soak corpus (`strands-sweep`) and on the lambada benchmark
programs (`strands-run`). "Sweeps" are proposals divided by live sites: the time a chip
would take, since every site acts at once.

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
| 2D | 2 | 3 | 2×2 block | 159 | ~98 bits |
| 2D | 2 | 2 | 2×2 block | 158 | ~64 bits |
| 3D | 1 | 3 | site + neighbours | 160 | ~109 bits |
| 3D | 2 | 2 | 2×2 block | 160 | ~98 bits |

Rewriting inside a 2×2 block is both more local and better than spilling into a site's four
neighbours (158 vs 146 at 2 slots, 2 strands). With it, every transition of the machine is a
function of one 2×2 block: the Margolus neighbourhood of the classic physical cellular automata.

**Real programs need wire capacity, and that means 3D.**
- **2D jams.** Lazy fib stalls after a few dozen rewrites. Diagnosis: the reaction zone fills
  with fresh agents (every site full) and its links fill with wires (no free strand to drag),
  so a wanted walker 15 strands from its target cannot advance. Neither stronger crowding,
  pressure, repulsion nor weaker tension on idle matter cured it. They only move the jam.
- **3D works.** Six links per site instead of four, at 2 agents and 3 strands per link
  (~128 bits per site):

  | program | rewrites | sweeps | sweeps per rewrite |
  |---|---|---|---|
  | fib(0) | 830 | 20k | 24 |
  | fib(1) | 998 | 31k | 31 |

  That is with exchanges and self-propelled walkers. Without them, fib(0) took 76k sweeps:
  walkers spent most of their turns facing full sites, which is exactly what exchanges fix.
- **Comparison.** The address-based mesh does the same programs at about 5 ticks per
  rewrite: faster, but with a 5-kbit tile, a router and global addresses.

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
- **More self-propulsion** (90% of turns to walkers): worse. Walkers retry blocked rewrites
  while idle matter never gets the turns it needs to make room.

## Open

- **Speed.** About 25–30 sweeps per rewrite, against about 5 ticks for the address-based mesh.
  Most proposals still go to kink motion on idle wires, and walkers wander. A better initial
  layout would help: the term is drawn in one layer of the 3D grid, as a 2D tree drawing.
  Biasing proposals toward where reactions wait would help too.
- **Larger programs.** fib(2), exp and sort have not been run to completion yet.
- **The hardware schedule.** The simulator draws proposals one at a time. A chip would update
  all 2×2 blocks of one parity at once (Margolus), each picking its own random move.
- **Bits.** The switchboard dominates site size: 24 ends at 5 bits in the 3D configuration.
  Most pass-throughs run straight across a site, so an encoding where "straight" is free could
  roughly halve it.

## Running it

From `crate/` (memory-cap long runs, see `AGENTS.md`):

```sh
cargo test --release                                  # ~3 s: corpus in 3 configurations, per-move invariants
cargo run --release --bin strands-run -- disp-t --k 2 --lanes 3 --block --lazy --temp 2
cargo run --release --bin strands-run -- fib:0 --grid 256 --depth 8 --k 2 --lanes 3 --block --lazy --temp 2 --swap 1 --active 0.5
cargo run --release --bin strands-sweep -- "k=2 lanes=2 temp=2.0 grid=48 block=1" "k=1 lanes=4 temp=2.0 grid=48"
```

`strands-run --progress N` prints the wanted readers and their wire lengths every N
proposals. `WHO=1` adds what each one is waiting on, which is how the 2D jam was diagnosed.
