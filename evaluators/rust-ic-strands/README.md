# rust-ic-strands

disp's tree-calculus interaction net on a lattice where **wires are physical and nothing has
an address**. It is the answer to two problems with the earlier spatial machines:
- `rust-ca-lattice` ("the cascade") also used physical wires, but jammed. It completes 130 of
  160 random terms.
- `rust-ic-mesh` completes everything, but by using global addresses and a router in every
  tile.

Open `player/index.html` to watch it run (rebuild with `./build-player.sh`):
- **Programs:** ordinary disp definitions (below) with arguments typed as disp literals, or the
  benchmark programs.
- **Stepping:** one move at a time with `+1` or `→` (shift: to the next rewrite), each move told
  in words.
- **Inspecting:** click a site to see its agents, where each of their wires leads, and what they
  are waiting for. Garbage is drawn dimmed.
- **Views**, since each wins on something:
  - *layers stacked*: compact, but layers overlap;
  - *layers side by side*: exact, with every site easy to click;
  - *3D*: three.js, with orbit, spread or isolate layers, and a camera that follows the action.
    `build-three.sh` rebuilds its bundled `three.min.js` (three 0.186.1, MIT).

## The machine

- **Sites and strands.** Sites form a 2D or 3D grid. Each link between neighbouring sites
  carries up to L numbered *strands*. Strand i of a link is one piece of wire seen from both of
  its sites, so no site ever names anything elsewhere.
- **The switchboard.** A site holds up to K agents and a switchboard: a pairing of the ends
  present there, namely agent ports and strand ends. A wire is a path: port, switchboard,
  strand, switchboard, …, port. Two wires cross for free by passing through the same
  switchboard. It is stored as a list of at most 8 pairings of two end numbers each (`--pairs
  8`: 80 bits in 3D, against 150 for a mate for every end), and a move that would need a ninth
  pairing in a site does not happen.
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
  - crowded switchboards (`--board 0.5`): a site whose switchboard holds p pairings costs
    p²/4, so wire pulls harder where it bunches and a site rarely needs all 8 of its slots;
  - crowded sites, and above all two *idle* agents (anything but a wanted reader) sharing a
    site, so idle matter keeps a seat free for traffic;
  - a temperature, which lets matter step out of the way.

  With random turns, most turns (80%) go to sites holding agents; bare wire only needs enough
  turns to straighten. With blocks, a site holding a wanted reader spends half its turns on it
  (`--active 0.5`): a step along its principal wire, or its rewrite. Nothing schedules
  anything, and there are no rules against cycles.
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

## Running real disp code

`programs/programs.disp` holds ordinary disp definitions over the prelude (`add`, `mul`, `fib`,
`is_even`, `sum`, `size`, `rev`, `doubled`, `isort`, `greet`) with tests. `programs/emit.ts`
compiles them with the real elaborator into trees for the player (`player/programs.js`,
regenerated by `build-player.sh`).

The player encodes arguments the way disp does and decodes the answer by the program's
declared kind, falling back to disp's own printer:
- a number n is n forks over a leaf;
- lists, and strings as lists of code points, are cons forks ending in a leaf;
- `true` is a leaf and `false` is a stem.

The benchmark programs (`fib`, `exp`, `sort`, …) come from the lambada suite and use their own
encoding: binary numbers, least significant bit first. Their fib counts from fib 0 = 1, so its
fib 1 = 1 and fib 2 = 2 are right for that program; disp's `fib` gives fib 2 = 1.

Compiled disp is general rather than hand-tuned, so it costs more. Clocks to the answer under
random turns (one seed each, every answer correct): with crowded links, then with idle matter
keeping a seat free, then with crowded switchboards of at most 8 pairings (the default now):

| call | crowded links | + idle repulsion | switchboards | rewrites |
|---|---|---|---|---|
| `size [5, 6, 7]` | 57k | 40k | 41k | 3.6k |
| `greet "a"` | 72k | 47k | 43k | 6k |
| `add 3 4` | 87k | 66k | 68k | 6.5k |
| `is_even 5` | 90k | 75k | 74k | 6.5k |
| `fib 2` | 111k | 72–85k | 72k | 6.3k |
| `doubled [1, 2, 3]` | 162k | 118k | 148k | 21k |
| `fib 3` | 297k | 192k | 212k | 13k |
| `mul 2 3` | 315k | 197k | 204k | 18k |
| `sum [1, 2, 3]` | 330k | 225k | 227k | 20k |
| `isort [2, 1]` | 405k | 255k | 231k | 16k |
| `rev [1, 2, 3]` | 498k | 454k | 393k | 15k |

Single seeds vary by about ±15%, so the last two columns are a draw. The switchboards are half
the size.

disp's `fib 3` (= 2) takes about 4× the rewrites of the lambada program's `fib 2` (also 2): the
cost of the compiler's general recursion and conditionals, not of the lattice. The
address-based mesh needs 27.5k rewrites for disp's `fib 2`.

## Schedules, and what a clock is

- **Random turns** (an asynchronous chip): one site at a time makes a move. A *clock* is one
  turn for the busiest site, since a chip gives every site at most one turn per clock. This
  counts the extra turns agent sites get.
- **Blocks** (`--margolus`, a synchronous chip): every clock the lattice is cut into 2×2×2
  blocks at a random offset (the Margolus neighbourhood of classic physical cellular automata).
  Moves stay inside their block, so blocks never interfere. Inside a block, either:
  - *one move per block*, the classic rule;
  - *a turn for every site* (`--block-moves`): the sites take their turns one after another,
    ready rewrites first, then sites with agents, then bare wire, each group in block order
    rotated by the block's random word. A turn that looks at a site an earlier turn changed
    this clock is dropped.

  The second is what [`hw/`](hw/README.md) builds as a chip (`--chip`). There every random
  choice of a turn comes from a 64-bit word that is a hash of the site's coordinates, the clock
  and the seed; energies are integers in quarter units, with a table for acceptance; and pulses
  all step at once. The chip and the simulator agree bit for bit.

One move per block costs 5–7× the clocks of random turns. A turn for every site costs 2–2.5×,
and 1.4–1.8× once walkers get half their site's turns. What is left is mostly the block
edge: half the time a walker's next strand leads out of its block. Blocks 3 sites wide
(`--block-side 3`) cut that to a third and save another 7–15%, at the price of an arbiter over
27 sites instead of 8.

The first version of a turn for every site let later turns steer around sites that earlier
turns had changed, and counted every site that had had its turn as changed, even when its turn
did nothing. A chip deciding all turns at once cannot steer like that. Making it faithful was
also 1.4–1.5× faster, because idle sites no longer block their neighbours. Making it exactly
reproducible on a chip (the hashed random words, integer energies, pulses stepping together, a
pending infection that cannot leak into the next turn, a dropped turn that really does nothing
more) cost nothing: 4–19% fewer clocks on every program.

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
| 3D | 2 | 3 | 2×2 square, blocks, a list of 8 pairings | 160 | ~95 bits |

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
  site with the pulse (95 once the switchboard is a list of 8 pairings, below).

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
| + idle matter keeps a seat free (below) | 5.0–5.5k |

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
would fill. fib(2) then takes 124–147k clocks. (Crowded switchboards later replaced crowded
links, below.)

**Then the blob jams.** With all that in place, wires pull matter into a dense blob: 75% of the
sites holding agents are full (two agents in two seats), and so are 79% of the sites wanted
readers try to step into. Only 7% of those blocked steps get through as exchanges, and rewrites
wait for room about three times per rewrite that fires. Stronger crowding of all agents helps
little (crowding 3: 4–21% fewer clocks): tension still packs everything.

The fix is a distinction physics already makes between active and passive matter. Two idle
agents (anything but a wanted reader) sharing a site cost energy (10, against temperature 2),
and wanted readers are exempt. Idle matter spreads to about one agent per site; full sites drop
from 75% to 8–14%, blocked rewrites by 5–10×, and wanted readers walk straight through. On
disp's `fib 2` clocks fall 111k → 72–75k, `add 3 4` 87k → 63–66k, the benchmark's `fib(2)`
61k → 34–39k; under blocks `fib(2)` 402k → 287k. The strength has a sweet spot: at 16 and
beyond idle matter can barely move, and things slow down again.

**Switchboards are mostly empty.** On fib(1) and fib(2) a live site uses about 5 of its 30
ends. About half its pairings join an agent port to a strand, and 26–30% turn a corner. Straight
runs make up 15–25%; U-turns and port-to-port pairings are at most 2% each. 31–40% of live sites
hold one pairing, and about 1% hold 8 or more. So a switchboard can be a short list of pairings
rather than a mate for every end, with a hard cap: a move that would overfill a site does not
happen.

**Crowding belongs to the switchboard, not the link.** Charging p²/4 for a site whose
switchboard holds p pairings, instead of n²/2 for a link carrying n strands, is exactly what a
short list needs, and it is also faster. It stops swelling the same way, since a strand adds a
pairing at every site it passes. And because an agent's wired ports are pairings too, it also
keeps agents apart, so the separate charge for crowded sites now matters little (turning it
off changes clocks by −5% to +8%). Clocks under blocks with a turn for every site, 3 seeds
each:

| crowding | fib(1) | fib(2) | exp(1) | disp `fib 2` | disp `add 3 4` |
|---|---|---|---|---|---|
| links | 14.2k | 89k | 75k | 183k | 153k |
| switchboards | 12.9k | 67k | 64k | 139k | 121k |
| switchboards, at most 8 pairings | 14.9k | 75k | 65k | 151k | 127k |
| switchboards, at most 6 pairings | 14.9k | 106k | 88k | 221k | 185k |

Under random turns switchboard crowding is 7–19% faster as well (fib(2) 36.7k → 30.9k clocks,
disp `fib 2` 80k → 65k). Stronger (p²/2) or weaker (p²/8) switchboard crowding is slower. Eight pairings (80 bits,
against 150) cost 2–16% against no cap and still beat crowded links on four programs of five;
six cost 16–59%.

**Walkers then want turns, not room.** With idle matter spread out, only 12% of the sites a
walker heads into are full, and 8% of those hold its own partner. On fib(1) under blocks a
walker tries a step about 0.4 times per clock and 60% of the tries go through. Letting a site
that holds a wanted reader spend half its turns on it cuts clocks under blocks by 5–24%. Under
random turns the same rule is neutral. Spending every turn on the reader livelocks under both:
a reader whose rewrite has no room retries forever, and its site never does anything else to
make room.

**Quiet stretches are one reader walking.** The rate of rewrites comes in bursts: on fib(2),
60–80% of the time passes in stretches of 20 clocks or more with no rewrite anywhere. Tracing
when each pair first exists, when its reader is first wanted and when it fires
(`TRACE=file strands-run`) shows what they are:
- during 78–91% of those stretches exactly one pair is ready, wanted and waiting to meet. Lazy
  evaluation runs nearly single file, so nothing else can happen;
- pairs become ready close by: 1–4 strands apart (median 2), only 1% 12 or more. Nothing moves
  as a chunk; there are no big trees to haul;
- they close slowly: 0.28 strands per clock under random turns, 0.12–0.14 under blocks. A pair
  one strand apart fires in 1.4–2.9 clocks, three apart in 14–29;
- bursts are the cascades that follow a rewrite whose products are already in reach of each
  other, such as copying or erasing a tree.

So time ≈ rewrites × the time to close a gap of two or three strands, and the levers are the
walker's speed and where a rewrite seats what will react next.

**Against the address-based mesh.** Clocks to the answer with everything above (pulses, all
collection, idle matter keeping a seat free, switchboard crowding, at most 8 pairings per
site), 3 seeds; blocks are the chip's schedule (`--chip`). Every run
matches the oracle and the abstract net, and afterwards cleans up to only the answer. Rewrites
include erasing garbage, as the mesh's do:

| program | mesh rewrites | mesh ticks | strand rewrites | random turns | blocks |
|---|---|---|---|---|---|
| fib(0) | 1,627 | 7,450 | 960–1,090 | 5.2k | 8.5k |
| fib(1) | 1,821 | 8,928 | 1,050–1,120 | 6.1k | 9.9k |
| sort(1) | 2,174 | 2,615 | 164–318 | 1.7k | 2.3k |
| exp(1) | 15,139 | 100k | 3,370–3,510 | 31.5k | 45.0k |
| fib(2) | 20,808 | 137k | 3,510–3,570 | 32.7k | 57.7k |

With random turns the strand lattice beats the mesh on every program: 1.4–1.5× on fib(0),
fib(1) and sort(1), and 3.2–4.2× on exp(1) and fib(2). Blocks take 1.4–1.8× the clocks of
random turns, which still beats the mesh by 2.2–2.4× on exp(1) and fib(2) and slightly on
sort(1), and loses by about 1.1× on fib(0) and fib(1). A mesh tile is about 5 kbit plus a
router; a strand site is about 95 bits of state (two agent tags with their wanted bits, 8
pairings, the pulse), and fib(2) peaks at 2,000–2,300 sites in use. What the logic around those
bits costs is in [`hw/`](hw/README.md).

## Things tried that did not help, and why

- **Pressure** from blocked rewrites (a diffusing field agents drift down): no measurable change
  on the corpus or on fib. Wire tension holds neighbours in place at least as strongly.
- **Short-range repulsion between all agents in neighbouring sites**: same. (The repulsion that
  works is narrower: same site only, idle agents only.)
- **Stronger crowding of every agent**: 4–21% fewer clocks at crowding 3; tension still packs
  everything.
- **Calling values**: letting a demand pulse that reaches a value mark it active, so it also
  passes through idle matter toward its reader. Within seed noise, and blocked rewrites roughly
  tripled as called values crowd in around their readers.
- **Weak or no tension on idle matter**: wires grow without bound and the reaction zone still
  congests.
- **Splitting big rules** so each step creates at most two agents, with a temporary "builder"
  agent decaying one step at a time. I searched every rule for its best decay order:
  - Dn·S, Nrm·F, Sel·S and Sel·F need 3-port builders;
  - T1·S needs a 4-port builder;
  - T1·F and Dn·F need 5-port builders.

  Five ports would make every switchboard bigger, so this stayed an analysis.
- **Lower temperature** with the idle repulsion (1.5): slower everywhere.
- **Walker priority under random turns**: neutral at 50%, livelock at 100% (above). Under
  blocks 80% is already slower than 50%.
- **Under blocks with one move per block**: a higher step rate is worse (wires need reshaping
  turns too), and letting a ready rewrite go first in its block changes nothing.
- **Heavier tension**: principal 4 instead of 3 is within seed noise; principal 6 and aux 2
  is slower.
- **Three strands per link** instead of four: much slower, and some runs never finish.
- **No exchanges**, now that idle matter keeps seats free: −4% to +13% under blocks, slower on
  four programs of five.
- **Weaker idle repulsion** (5 instead of 10) next to switchboard crowding: −16% to +5%, about
  even.
- **No demand pulses**: 40–66% slower under blocks.
- **Eager evaluation** finishes fib(0) in 11k clocks, but with about 67× the rewrites.

## Open

- **The synchronous schedule** still takes about 1.8× the clocks of random turns, mostly
  because a walker's next strand leaves its block half the time. Wider blocks help a little
  (above); a walker that eats every strand of its wire inside its block in one turn might help
  more.
- **Bits.** With a list of pairings, the number of strands per link costs only the log of the
  number of ends, so wider links are nearly free. Whether they help is untested.

## Running it

From `crate/` (memory-cap long runs, see `AGENTS.md`):

```sh
cargo test --release                                  # ~5 s: corpus in 4 configurations, per-move invariants, collection
cargo run --release --bin strands-run -- disp-t --k 2 --lanes 3 --block --lazy --temp 2
cargo run --release --bin strands-run -- fib:0 --grid 256 --depth 8 --k 2 --lanes 4 --block --lazy --temp 2 --swap 1 --agents 0.8 --pulse --gc --idle-crowd 10 --board 0.5 --pairs 8
cargo run --release --bin strands-run -- fib:0 --grid 256 --chip                   # the chip's schedule (blocks)
cargo run --release --bin strands-run -- fib:2 --grid 300 --depth 8 --k 2 --lanes 4 --block --lazy --temp 2 --swap 1 --agents 0.8 --pulse --gc --idle-crowd 10 --board 0.5 --pairs 8 --budget 5000000000 --clean 100000
cargo run --release --bin strands-sweep -- "k=2 lanes=2 temp=2.0 grid=48 block=1" "k=2 lanes=3 temp=2.0 grid=48 depth=6 block=1 lazy=1 pulse=1 swap=1 agents=0.8 gc=1 idlecrowd=10 board=0.5 pairs=8"
```

`--chip` is the configuration `hw/` builds; `hw/validate.sh` checks that the chip matches it
(see [`hw/README.md`](hw/README.md)). `TRACE=file strands-run ...` writes when each rewrite's pair
first existed, when its reader was first wanted, when it fired and how far apart the pair was.
`strands-run --clean N` runs on past the answer (up to N clocks) and reports when only the
answer is left; `PIECES=1` lists the connected pieces of the net at the end.
`strands-run --profile` prints what wanted readers spend their time waiting on, how crowded it
is where walkers go, and what switchboards hold; `--progress N` prints the wanted readers and
their wire lengths every N proposals, and `WHO=1` adds what each one is waiting on, which is
how the 2D jam was diagnosed.
