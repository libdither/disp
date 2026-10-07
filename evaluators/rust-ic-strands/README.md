# rust-ic-strands

disp's tree-calculus interaction net on a lattice where **wires are physical and nothing has
an address**. It is the answer to two problems with the earlier spatial machines:
- `rust-ca-lattice` ("the cascade") also used physical wires, but jammed. It completes 130 of
  160 random terms.
- `rust-ic-mesh` completes everything, but by using global addresses and a router in every
  tile.

Open `player/index.html` to watch it run (rebuild with `./build-player.sh`). It shows only the
current design (`lattice.rs` `latest`: the chip's schedule with the demand field and forking S
rules, below), with the field drawn as an amber tint and called values ringed in orange:
- **Programs:** one input box takes ordinary disp code (`fib 2`, `add 2 (mul 2 2)`,
  `map [1, 2] succ`), a benchmark program (`sort:1`) or a raw term. Its list (`▾` or `↓`) offers
  each program with an example.
- **Stepping:** one move at a time with `+1` or `→` (shift: to the next rewrite), each move told
  in words.
- **Inspecting:** click a site to see its agents, where each of their wires leads, and what they
  are waiting for. Garbage is drawn dimmed.
- **Views**, since each wins on something:
  - *layers stacked*: compact, but layers overlap;
  - *layers side by side*: exact, with every site easy to click;
  - *3D*: three.js, with orbit, spread or isolate layers, and a camera that follows the action.
    `build-three.sh` rebuilds its bundled `three.min.js` (three 0.186.1, MIT).
- **CPU or GPU** (the selector next to the speed, or `G`): the GPU runs the same design through
  WebGPU (below), and a run moves between the two at any clock as it is.
- **Going back** (the time bar, `←` one clock, `shift ←` to the last rewrite): every move is a hash
  of its site and its clock, so a run repeats exactly from any state of it. States are saved in the
  engine (`wasm.rs` `save_state`) and thinned with distance from the clock on show (`timeline.js`):
  each time the budget (64 MB) is full, the one whose neighbours are nearest for its distance goes,
  so near the clock on show there is about one every clock and far away about one every tenth of the
  distance. Any clock reached is shown by putting back the nearest saved state before it and running
  on, saving more densely near the target, so stepping back costs a few clocks of replay and a jump a
  tenth of its length.
- **At the end** the run cools: once only the answer is left, the temperature halves every 100
  clocks down to 0.05 (`Lattice::cool`), so the wire straightens and the answer contracts as far as
  it will go (disp `add 2 3`: 32 strands of wire become 11, one a wire). The clock it began at is
  recorded, and replays cool there too.

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

Anything else typed into the player is compiled in the browser, with the prelude, `lib/list.disp`
and `programs.disp` in scope (`programs/compiler.ts`). The elaborator would work `fib 3` out to 2
on the spot, so it runs on a session that leaves every reduction undone: applying a fork gives an
application node (applying a leaf or a stem only builds a tree, and numerals are still spelled
out), and anything the elaborator looks inside is worked out first. What comes out is the
expression's own applications over compiled trees, for the lattice to run. `programs/bundle.ts`
(run by `build-player.sh`) packs the elaborator, the rust-eager evaluator as WebAssembly and those
three files into `player/compiler.js`, about 160 KB. The player loads it the first time something
needs compiling and runs it in a worker started from a Blob URL, so it works from disk too. In
Floorp the first compile takes about 65 ms, loading included, and later ones 1–4 ms. Once loaded,
`StrandsDisp.names` lists every definition in scope as `{ name, tree }`, the tree in ternary.

The player encodes arguments the way disp does and decodes the answer by the program's
declared kind (a programs.disp `///` line, or the declared type of a library function), falling
back to disp's own printer:
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

## Fields: demand that clears its own way

Matter learns about the rest of the machine only along wires: tension pulls along them and demand
pulses run along them. Matter not wired to a reaction cannot know it is there, and parks in the
way. A *field* adds a channel through space. Every site, empty or not, keeps a few small numbers
and recomputes them each clock from what it holds, its own numbers and its face neighbours'
numbers of the clock before, the way pulses move (`crate/src/field.rs`, `--field`). A channel has:
- sources: a wanted reader, a demand pulse, a rewrite, a rewrite without room, or the
  switchboard's pairings;
- a falloff per hop through space and per hop across a face that carries a strand, and a fade
  per clock: a site takes the most of its source, each neighbour's value less the hop's falloff,
  and its own value less the fade;
- weights: energy per unit of the channel climbed by an idle agent, a called value and a wanted
  computation, and for wire that an idle agent drags or a flip moves (switchboard crowding times
  the channel).

**The demand field** (`--demand`):
- a demand pulse that reaches a value's principal port wants it, as one reaching a computation's
  output does (`calls`; a value's wanted bit was unused), and a wanted value walks along its
  principal wire toward its reader, as wanted readers do;
- one channel of 2 bits: a wanted reader or a demand pulse sets its site to 3, which falls by one
  a hop and a clock;
- an idle agent pays 4 a unit to climb it, a called value gains 2, and wire laid down by idle
  matter pays crowding times the field.

So demand becomes a field: idle matter yields along the reader's wire and around both of its
ends, and the value it asks for comes to meet it. On the chip's schedule, 2 seeds each, every run
finishing (the answers checked against the oracle on fib(2), `add 3 4` and `isort`, and on the
160-term corpus in `tests/corpus.rs`):

| program | clocks | with the demand field |
|---|---|---|
| fib(0) | 7.6k | −31% |
| fib(1) | 9.7k | −30% |
| sort(1) | 2.3k | −22% |
| exp(1) | 43.1k | −28% |
| fib(2) | 57.9k | −48% |
| disp `add 3 4` | 106.7k | −51% |
| disp `fib 2` | 115.8k | −48% |
| disp `fib 3` | 264.6k | −51% |
| disp `rev [1, 2, 3]` | 280.5k | −49% |
| disp `isort [2, 1]` | 308.1k | −51% |

How it got there, on fib(2), disp `add 3 4` and disp `fib 2` (geometric mean of clocks):

| field | bits a site | clocks |
|---|---|---|
| a wanted reader's own site, idle agents pushed out | 1 | −3% |
| its face neighbours too (2 at the reader, 1 next to it) | 2 | −13% |
| + wire laid down by idle matter pays | 2 | −20% |
| + one hop further (3 at the reader, falling by one a hop) | 2 | −27% |
| + demand pulses as sources, fading one a clock: the reader's whole wire is cleared | 2 | −33% |
| + a called value is drawn up the field | 2 | −36% |
| + a called value counts as wanted, so idle crowding spares it | 2 | −41% |
| + called values walk toward their readers | 2 | −48% |

What it is and is not:
- **It clears the walker's way, more than it makes room for rewrites.** Rewrites without room fall
  to a third, but cutting them further (letting the walker's own wire pay too) gained nothing.
  On disp `add 3 4`, walker steps that found no seat fall from 58k to 14k and those that found no
  free lane from 17k to 5.5k.
- **Only the reader's partner is related.** With the abstract net deciding what is related to a
  reader, sparing everything within 6 steps of it did worse than sparing only its partner, and
  sparing nothing (pushing the partner away too) gained nothing. Separating whole trees is not
  the point. Without the called mark the field gains −15% rather than −36%.
- **Each half needs the other.** Called values walking toward their readers without the field
  gain about 20%, with blocked rewrites more than doubled as values crowd their readers; the field
  alone gains about 40%; together about 48%.
- **What did not help:** rewrites, rewrites without room or a stuck pair's growing pull as
  sources; 3 bits and a longer reach; spreading further along wires than through space; the
  reader drawn up its own field; weights a quarter stronger or weaker.

Cost: 2 bits a site (on about 95), none per agent. A site's update is the most of eight 2-bit
inputs with decrements, and a step or flip adds three small terms to its energy. On fib(1) and
fib(2) about 130 sites a clock hold a field, against some 2,000 holding something, so a GPU that
runs only busy blocks visits few more, though working the field out still makes its clock about a
fifth dearer (On a GPU, below). In analog a channel is a diode-OR mesh: each node sits at
the most of its source and its neighbours less one diode drop, and a leak makes the fade. It runs
on the GPU (below) and is what the player shows, but it is not yet on the chip (`hw/rtl`), and
`--chip` alone still leaves it out.

## Fork: the S rule runs both halves

Demand alone runs one thing at a time. On an idealised lazy machine (demand arrives at once,
every wanted rewrite fires at once, garbage collected as the lattice does it) the programs below
fire 1.2 to 2.4 rewrites a step, and 74–92% of steps fire one. On the lattice about one wanted
reader walks at a time while 3 to 20 wait on it, like a call stack, and a rewrite fires in one
clock in ten.

Tree calculus forks in one place: the S rule, `△(△s) b c = s c (b c)`, gives `c` to two
computations. Lazily, `(s c)` runs first and `(b c)` waits until the result of `(s c)` asks for
it. The eager reference evaluator (`src/core/tree.ts`) also runs them in order: it skips `b c` when
`s c` comes out K-headed, which the kernel relies on.

**Fork** (`fork`, part of `latest`): both halves start wanted. It is two bits of the rule table,
nothing else: the S rule's `(b c)` apply and the duplicator sharing `c` start wanted
(`fresh_wanted_in`), so the GPU runs it from its tables as they are. When `(s c)` throws `(b c)`
away, erasers collect it: with a diverging `(b c)` thrown away, nothing of it is left 66 clocks
after the answer. Lattices sized as the player sizes them, 2 seeds, every answer checked:

| program | clocks | with fork | rewrites | peak sites |
|---|---|---|---|---|
| disp `add 2 3` | 41,585 | 10,455 (4.0×) | −50% | −51% |
| disp `mul 2 2` | 131,061 | 18,634 (7.0×) | −46% | −43% |
| disp `sum [1, 2]` | 109,000 | 18,145 (6.0×) | −44% | −39% |
| disp `rev [1, 2]` | 82,666 | 16,741 (4.9×) | −38% | −48% |
| disp `fib 2` | 65,594 | 15,316 (4.3×) | −27% | −19% |
| disp `isort [2, 1]` | 135,259 | 33,415 (4.0×) | −31% | −15% |
| disp `doubled [1, 2]` | 74,548 | 20,011 (3.7×) | −44% | −60% |
| disp `is_even 3` | 41,058 | 12,690 (3.2×) | −38% | −49% |
| disp `size [5, 6, 7]` | 41,128 | 13,564 (3.0×) | −25% | −6% |
| disp `greet "a"` | 46,736 | 15,930 (2.9×) | −37% | −4% |
| fib(1) | 7,452 | 3,184 (2.3×) | +18% | +29% |
| fib(2) | 34,512 | 10,418 (3.3×) | +43% | +49% |
| exp(1) | 33,723 | 11,896 (2.8×) | +19% | +35% |
| sort(1) | 2,066 | 1,256 (1.6×) | +64% | −2% |
| sort(2) | 114,380 | 20,996 (5.4×) | −17% | −7% |

On disp programs, 4.2× fewer clocks (geometric mean), 38% fewer rewrites and 36% less space:
- **Less work.** A computation that runs sooner finds its garbage sooner: a triage throws away its
  unused branches before anyone has asked the duplicator holding them to copy, so the eraser turns
  the duplicator into a wire. In the ideal model of `add 2 3`, copying falls by 60%.
- **Less space.** Pending computations no longer pile up waiting to be asked: lazy evaluation's
  space leak.
- **Several reactions at once.** On fib(2) 8 wanted readers walk at a time rather than 1, and a
  rewrite fires in a third of clocks.

The hand-written benchmarks throw some `(b c)` away after it has run, for up to 64% more rewrites.
The demand field still matters with fork: without it the runs above take 30–68% more clocks, and
without called values walking 20–37%. Wanting every computation's inputs, not only the S rule's,
gains nothing more in the ideal model: the S rule is the only rule that wires a computation to
another's input.

**Caching what a program throws away (tried, not kept).** The eager evaluator is fast by caching:
equal trees are one node, so copying is free, and `apply(f, x)` facts are memoized and kept
across runs with what they cost. A net has neither: a duplicator copies a value one layer at a
time as it is read (on disp programs 45% of rewrites copy and 30% erase), and equal computations
built apart both run; only a shared suspension runs once (`Dn·P`). What does carry over is the S
rule's choice, as a fact about code: for each S node of the loaded program, its `(b c)` is needed
nearly always or nearly never. Learned on a smaller input and kept with the program as a mark that
copies inherit, like the memo snapshot (fib(1) for fib(2), exp(0) for exp(1), sort(1) for sort(2)),
no fork at marked nodes takes 13–29% fewer rewrites than forking everywhere on the lattice, but no
fewer clocks (exp(1) 8% more): the lattice has room and time to spare, so waste costs energy, not
time. It would take a new agent tag, an S that does not fork. Marks from the code's shape alone (`s` is K or `K (K w)`, the
eager evaluator's own shortcut) remove almost nothing.

**Leases (tried, not kept).** Demand recomputed every step from the normalizer stops a
thrown-away `(b c)` at once, rather than when erasers reach it: in the ideal model fib(2)'s extra
rewrites fall from 38% to 4%, but much of the speed goes too (size-self 10.9× fewer steps → 2.6×),
since an argument loses its lease while it sits in a pair waiting to be triaged. Letting leases
pass through pairs recovers some. It also needs demand sent again and again: more pulses and
state.

## Things tried that did not help, and why

- **Pressure** from blocked rewrites (a diffusing field agents drift down): no measurable change
  on the corpus or on fib. Wire tension holds neighbours in place at least as strongly. A field that
  works centres on demand instead, and spares the reader's partner (Fields, above).
- **Short-range repulsion between all agents in neighbouring sites**: same. (The repulsion that
  works is narrower: same site only, idle agents only.)
- **Stronger crowding of every agent**: 4–21% fewer clocks at crowding 3; tension still packs
  everything.
- **Calling values**: letting a demand pulse that reaches a value mark it active, so it also
  passes through idle matter toward its reader. Within seed noise, and blocked rewrites roughly
  tripled as called values crowd in around their readers. Today it saves
  about 20% on its own, and with the demand field it is part of the largest gain (Fields, above).
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

- **The demand field and fork on the chip** (`hw/rtl`), so that `--chip` can include them (Fields
  and Fork, above). Fork is only the rule table's wanted bits.
- **Each step now costs about 16 clocks**, mostly walking: with fork the number of steps is within
  about 1.6× of the longest chain of rewrites that depend on each other.
- **The synchronous schedule** still takes about 1.8× the clocks of random turns, mostly
  because a walker's next strand leaves its block half the time. Wider blocks help a little
  (above); a walker that eats every strand of its wire inside its block in one turn might help
  more.
- **Bits.** With a list of pairings, the number of strands per link costs only the log of the
  number of ends, so wider links are nearly free. Whether they help is untested.

## On a GPU

`crate/src/gpu` runs the chip's schedule (`--chip`) on a GPU through wgpu and Vulkan. It holds the
same semantics as `hw/rtl`, ported stage by stage, and nothing else. A thread runs one 2×2×2 block,
and the block only reads and writes its own 8 sites, so whatever runs on the GPU still runs on the
chip. It matches the simulator bit for bit: `hw/validate.sh` replays every recorded turn and block
through it, and runs it in lockstep with the simulator.

Its speed comes from locality. A clock moves anything at most one site, inside its block, so a
clock need run only the blocks holding something: its busy list. Each block makes the next clock's
list as it finishes its turns, marking the next clock's blocks that hold its sites (one atomic
per block, so each joins once). So a clock is a single dispatch, of a fixed number of workgroups
that share out however many blocks are busy; nothing waits to learn the list's length. Each
clock's pulse phase runs at the start of the next clock's turns; the pulses sit in two buffers
by clock parity, so a block reads its neighbours' while writing its own. A thread keeps its
sites in workgroup memory and changes them in place.

Against the simulator, on one machine (Ryzen 5 7640U, single-threaded, against its integrated
Radeon 760M), same seed, both looking for the answer as they go: the chip's schedule, with the
demand field (Fields, above), and the current design, which adds fork (Fork, above):

| program | lattice | clocks | CPU | GPU |
|---|---|---|---|---|
| fib(0) | 232×232×8 | 7,568 | 2.91 s | 0.69 s |
| … with the demand field | | 5,181 | 2.61 s | 0.65 s |
| … and fork | | 2,789 | 2.40 s | 0.52 s |
| fib(1) | 300×300×8 | 9,898 | 3.94 s | 1.04 s |
| … with the demand field | | 6,691 | 3.57 s | 0.78 s |
| … and fork | | 2,724 | 2.38 s | 0.56 s |
| sort(1) | 490×490×8 | 2,501 | 4.45 s | 0.74 s |
| … with the demand field | | 1,759 | 3.43 s | 0.60 s |
| … and fork | | 1,014 | 2.13 s | 0.40 s |
| fib(2) | 300×300×8 | 57,949 | 21.2 s | 5.71 s |
| … with the demand field | | 28,260 | 12.0 s | 3.56 s |
| … and fork | | 9,830 | 6.76 s | 1.84 s |

**What a clock costs.** These programs keep 2,000–3,000 sites in use, a few hundred busy blocks
a clock, and every clock waits on the one before. So a clock takes as long as its slowest wave
(the 32 threads that run in lockstep): on fib(1) about 75 µs, against 380 µs for the CPU's whole
clock. Loading and storing the blocks, the pulse phase and the busy list take about a seventh of
it; the turns take the rest. A wave runs every path any of its blocks takes, one after another,
each a long chain of dependent steps that a GPU thread runs far more slowly than a CPU core. What
made the clock 2.5× faster than the version before, in the order made, each against the one
before it, on fib(1):

- each block lists its turns first, so a wave runs as many turns as its busiest block has sites,
  not one for every position and group (24): 19%;
- 16 blocks to a workgroup rather than 64, so a wave runs fewer blocks' paths and the waves
  spread over more of the GPU: 20%;
- natively, no counter on every loop (wgpu adds them unless told every loop ends; they keep the
  driver from unrolling the loops): 23%;
- the block's loads issued together, not site by site: 10%;
- the code a turn usually runs shorter and in one place (both halves of an exchange through one
  copy of the step, the step's writes as loops, the rare stages last): about 10%;
- each position's pair looked for once a clock, not again at its turn (a turn changes only the
  positions it takes, so the pair stands unless it was seen across a strand at one of them): 6%;
- a pulse lost to a rewired strand looked for only where the turn wrote: 3%.

Code size matters: the turn kernel is about 100 KB against a 32 KB instruction cache. Running the
pulse phase as eight inlined copies, to load all 48 neighbours' pulses at once, made the kernel
170 KB and a clock 18% slower.

Two things that do not help. Running every block and site each clock (`--dense`) is 7.5× slower
even on the smallest lattice fib(1) fits in, all but 0.4% of which is empty. Running several
clocks per dispatch on a tile kept in workgroup memory would save at most the seventh of a clock
that loading and storing take, and the tiles do not fit: a clock reaches two sites out (one for
the block, one for the pulses), so k clocks need a border 2k wide, and 64 KB holds only about
14×14×8 sites of 40 bytes, of which two clocks leave a 6×6 interior.

More work per clock is where the GPU gains: copies of fib(1) side by side, through the browser
path below, 2,048 clocks without the demand field (clocks a second):

| copies | sites in use | CPU (wasm) | GPU | ratio |
|---|---|---|---|---|
| 1 | 2,015 | 1,606 | 6,583 | 4.1× |
| 4 | 8,060 | 372 | 2,778 | 7.5× |
| 16 | 32,240 | 77 | 902 | 11.7× |

Orders of magnitude over a core would need turns sorted by kind, so that a wave runs one path,
and sites kept as their list of pairings, as the chip keeps them, so that more of them fit; and
even then only for runs with tens of thousands of sites busy. For programs as small as these, a
clock is a few hundred dependent jobs, and a core runs those nearly as fast as a GPU can.

**The demand field** runs there too (`demand=1`; `--latest` adds fork, which is only the
rule table's wanted bits), matched bit for bit, field and all. A site's
field rides in its pulse word, above the pulse, so the two pulse buffers by clock parity carry it:
a block works out its sites' field at the start of its turns, from its own and its neighbours'
words of the clock before, right after their pulse phase. A block holding only field stays busy,
and a field high enough to reach across a face puts the neighbour's block on the next clock's
list. `tables.rs` `gpu_unfit` takes one field of up to 4 bits, set by wanted readers and demand
pulses. Working out the field makes a GPU clock about a fifth dearer: on fib(1) with the field's
weights set to zero, so that the run is the same, 94 µs a clock against 79. The cost is spread
about evenly over computing it, the blocks holding only field and marking the blocks a strong
field reaches, so there is no one piece to cut. Called values walking add turns too. So on the
GPU the field saves less time than clocks (the table above). Fork runs from the rule table as it
is. It gives a clock more to do, several reactions at once, which is what a GPU is good at: on
fib(1) a clock takes 125 µs against 94, with five times the rewrites.

**In the browser.** `player/gpu.js` drives the same kernels through WebGPU, with the shader the
engine generates for the loaded settings. The engine stays the record of the run: a GPU stretch
starts from the engine's sites and ends by putting the GPU's back, with its counts, and the engine
rebuilds its abstract net from them (`Lattice::adopt`). So the CPU can take over at any clock
and carry on exactly as if it had run all along (`tests/resume.rs`).

Stretches run ahead of the engine. Reading one back is slow in Firefox, whose WebGPU looks for
finished work on a timer: up to 100 ms for a readback, even of 4 bytes, against well under 1 ms on
Dawn, Chrome's WebGPU. So the player keeps enough stretches out to cover that (`gpu.js`
`latency`), each copying its sites out within the queue (`stretch`), and the engine takes them back
in turn, one a frame. Anything else that touches the engine drops the stretches still out, and the
GPU starts again from the engine's sites; a stretch whose sites did not fit its copy runs again,
as a run is fixed by its sites, clocks and seed. Waiting for each stretch instead, the player ran
15 clocks a second in Floorp, its estimate of a clock's cost swallowed by the wait.

`player/gpu-check.js` hands a run back and forth between GPU and CPU in batches of varying size,
the GPU's stretches out several at a time and sometimes too short of room, and it must match one
that stays on the CPU, site for site and count for count. `player/dawn.sh` runs it in Node on
Dawn on the machine's own GPU (`check`, or `bench` for speeds); `player/firefox.sh` runs it in
headless Firefox or Floorp (`check`), and runs the player itself there from clock 0 on each engine
(`player`); `player/check.mjs` runs it in headless Chromium, which here only gets SwiftShader, a
GPU emulated on the CPU. The kernels need 8 KB of workgroup memory, within WebGPU's default of
16 KB.

In Floorp (Firefox 154), the player reaches the answer of disp `add 3 4` in 3.2 s on the GPU
against 5.8 s on its CPU engine. On Dawn's bench, on fib(1), the GPU runs about 7,500 clocks a
second, 5,200 to 6,200 with the demand field (which needs a quarter fewer clocks) and 3,750 to
4,650 in the current design (which needs two thirds fewer), against 2,000, 1,550 and 760 to 970
for the CPU engine (WebAssembly, nearly as fast as the native simulator). An earlier version listed each
clock's busy blocks in a kernel of its own and launched the turns as an indirect dispatch (one
whose size the GPU reads from a buffer). Dawn checks every indirect dispatch on the CPU, about
60 µs each, so recording a clock cost more than the GPU took to run it: 4,400 clocks a second.

## Running it

From `crate/` (memory-cap long runs, see `AGENTS.md`):

```sh
cargo test --release                                  # ~8 s: corpus in 5 configurations (one with the demand field), per-move invariants, collection, handover
cargo run --release --bin strands-run -- disp-t --k 2 --lanes 3 --block --lazy --temp 2
cargo run --release --bin strands-run -- fib:0 --grid 256 --depth 8 --k 2 --lanes 4 --block --lazy --temp 2 --swap 1 --agents 0.8 --pulse --gc --idle-crowd 10 --board 0.5 --pairs 8
cargo run --release --bin strands-run -- fib:0 --grid 256 --chip                   # the chip's schedule (blocks)
cargo run --release --bin strands-run -- fib:0 --grid 256 --chip --demand          # with the demand field (field.rs; --field SPEC for others)
cargo run --release --bin strands-run -- fib:0 --grid 256 --latest                 # the current design: the demand field and fork
cargo run --release --bin strands-run -- fib:2 --grid 300 --depth 8 --k 2 --lanes 4 --block --lazy --temp 2 --swap 1 --agents 0.8 --pulse --gc --idle-crowd 10 --board 0.5 --pairs 8 --budget 5000000000 --clean 100000
cargo run --release --bin strands-sweep -- "k=2 lanes=2 temp=2.0 grid=48 block=1" "k=2 lanes=3 temp=2.0 grid=48 depth=6 block=1 lazy=1 pulse=1 swap=1 agents=0.8 gc=1 idlecrowd=10 board=0.5 pairs=8"
```

`--chip` is the configuration `hw/` builds; `hw/validate.sh` checks that the chip and the GPU
version match it (see [`hw/README.md`](hw/README.md)). `crate/gpu.sh TERM --grid N [--depth D]`
runs a term on the GPU (`--check` in lockstep with the simulator, `--dense` without the tiles,
`--vectors FILE...` replays recorded turns, `--latest` runs the current design, `key=value` changes
a setting as `strands-sweep` spells it); `strands-hw --wgsl` prints its generated tables.
`player/dawn.sh check 'src=sort:1'` checks the browser's GPU path on the design the player shows
(`player/firefox.sh check` the same in Firefox; `player/firefox.sh player 'src=add 3 4'` times
the player itself there, and `player/firefox.sh input` checks its input box and compiler), and `player/dawn.sh bench 'src=fib:1'` times it against the browser's CPU engine
(`copies=16&clocks=512` for many copies side by side, `demand=0&fork=0` for the chip's schedule alone). `TRACE=file strands-run ...` writes when each rewrite's pair
first existed, when its reader was first wanted, when it fired and how far apart the pair was.
`strands-run --clean N` runs on past the answer (up to N clocks) and reports when only the
answer is left; `PIECES=1` lists the connected pieces of the net at the end.
`strands-run --profile` prints what wanted readers spend their time waiting on, how crowded it
is where walkers go, and what switchboards hold; `--progress N` prints the wanted readers and
their wire lengths every N proposals, and `WHO=1` adds what each one is waiting on, which is
how the 2D jam was diagnosed.
