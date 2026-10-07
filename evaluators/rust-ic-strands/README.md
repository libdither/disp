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
- **Layout**, as in the website's tree visualizer: the lattice fills the page with the answer at
  the top left, and the input box, time bar and transport sit at the bottom, speed in the left
  corner and round buttons in the right: the view (a pop-out of the four views and the segments
  and moves toggles, as the tree visualizer's options pop out), the GPU and the details.
  Leaf, stem and fork are greys: colour is kept for applications and the agents that carry them
  out, and an agent's shape says what kind it is (`player/shapes.js`, the same in every view and
  the legend): values are triangles with a dark hole for each child, applications disks (a
  suspended one hollow), choices diamonds, the arms of a choice a pill (an unpair splits it in
  two), a duplicator a Y, an eraser a ×, a normalizer a hexagon, the root a ring around a dot; the
  principal port is where the shape interacts, on top or, in the graph view, toward its wire.
  Small on screen, holes go first, then shapes become squares. Everything else (picked
  terms, numbers, legend, how it works) is in a details drawer, `D`, shut until wanted. Rewrites
  flash for about four clocks of the run as shown, at most 0.6 s, and the more fire at once the
  fainter each is, so a fast run is not buried in rings.
- **Programs:** one input box takes ordinary disp code (`fib 2`, `add 2 (mul 2 2)`,
  `map [1, 2] succ`), a benchmark program (`sort:1`) or a raw term. Its list (`▾` or `↓`) offers
  each program with an example.
- **Stepping:** `▸` or `→` runs on to the next clock where something happens, `⏭` or `shift →` to
  the next rewrite. The clock is summed up in a line over the input box (rewrites, steps, folds),
  told in words in the details, and drawn as a faint arrow for each agent that moved (the moves
  toggle). In the graph view the net changes only when something rewrites, so there `▸` and `◂`
  go on to the next rewrite and back to the last (garbage collected counts too).
- **Inspecting:** click a site to see its agents, where each of their wires leads, and what they
  are waiting for. Garbage is drawn dimmed.
- **Segments** (`C`, on by default): agents and their wires are coloured by where they are in the
  term the root computes. Every application splits its colour among its function and arguments,
  and applications nested in those split theirs again (`net.js`, from the abstract net that
  `readback.rs` `wires` hands over), so each part of the term shows up as a region of the net.
  Values (code and data) stay grey, only tinted toward their part's hue; what kind an agent is,
  its shape says. With something picked, the segments are the pick's, and its term in the details
  is tinted to match.
- **Reduction state:** shift+click an agent to pick the computation it heads (hold shift to
  preview): it and every agent feeding its inputs are lit in pink, everything else dimmed, and
  the details drawer opens to write what it means now as disp (Reading back, below). Picks follow their
  agents as they move and rewrite; one whose value a rewrite uses up drops out. Going back keeps
  the root picked; other picks are found again only where the net still looks as it did. `Esc`
  lets go.
- **Views**, since each wins on something:
  - *layers stacked*: compact, but layers overlap;
  - *layers side by side*: exact, with every site easy to click;
  - *3D*: three.js, the lattice as it is: each layer over a faint labelled floor, each wire one
    path in its segment's colour, agents as solids of their kind's shape (pyramids for values,
    balls for applications, octahedra for choices, a ring for a suspension, a Y, a ×, the root a
    ring around a ball; one instanced mesh a kind) a few pixels wide however far away. It opens
    fitted at a slant across the net's long side (`F` again); drag to orbit, right-drag to pan, the wheel
    zooms toward the pointer; spread or isolate layers, or follow the action.
    `build-three.sh` rebuilds its bundled `three.min.js` (three 0.186.1 with its wide lines, MIT).
  - *graph* (`player/graph.js`): the lattice left out, only who is wired to whom. Springs along
    wires, a push between nodes that crowd, and a pull that hangs what feeds an input below its
    reader lay the net out so the term reads as a tree from the root down. Nodes keep their places
    as the run plays: a new agent starts where its neighbours are. A gold wire with a spark joins
    two agents about to rewrite; a value, pair, unpair or duplicator turns its tip to its principal
    wire.
- **CPU or GPU** (the chip toggle, or `G`): the GPU runs the same design through
  WebGPU (below), and a run moves between the two at any clock as it is.
- **Going back** (the time bar, `◂` or `←` one clock, `⏮` or `shift ←` to the last rewrite): every move is a hash
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
`is_even`, `sum`, `size`, `rev`, `doubled`, `isort`, `greet`, and binary adders: Adding binary
numbers, below) with tests. `programs/emit.ts`
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
- `true` is a leaf and `false` is a stem;
- a binary number (`-> bits`) is a number tree (Adding binary numbers, below), read back as its
  value. Its examples name the bits `b0` and `b1`, so they go through the compiler.

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

**Sharing equal parts of the program (tried, not kept; `share=N`).** Built with every part of at
least N agents that occurs more than once built once and read through duplicators (`share.rs`, as
if the program had named it), a disp program loads with 20–69% fewer agents and gives the same
answers (`tests/share.rs`). It runs slower: a shared part sits in one place and its readers are
spread over the drawing, so its copies travel along long wires, and code that was laid out ready
is now copied a layer at a time as it is read. On the disp programs, `--latest`, 2 seeds, grids
sized as the player sizes them (sharing parts of 8 or more agents · of 24 or more; 0% where no
part that big repeats):

| program | agents at load | clocks | rewrites | peak sites |
|---|---|---|---|---|
| disp `add 2 3` | −35% · 0% | +33% · 0% | +9% · 0% | +68% · 0% |
| disp `mul 2 2` | −58% · −29% | +31% · +24% | +7% · +6% | +59% · +160% |
| disp `fib 2` | −61% · −32% | +81% · +63% | +11% · +11% | +221% · +271% |
| disp `is_even 3` | −23% · 0% | +16% · 0% | +4% · 0% | +49% · 0% |
| disp `sum [1, 2]` | −52% · −20% | +47% · +24% | +6% · +2% | +226% · +154% |
| disp `size [5, 6, 7]` | −28% · 0% | +27% · 0% | +2% · 0% | +73% · 0% |
| disp `rev [1, 2]` | −55% · −38% | +29% · +24% | +7% · +4% | +64% · +56% |
| disp `doubled [1, 2]` | −53% · −20% | +32% · +19% | +8% · +5% | +207% · +154% |
| disp `greet "a"` | −56% · −47% | +20% · +1% | +17% · +14% | +25% · −28% |

Sharing parts of 3 or more is worse again (`doubled` unfinished in 900 s, `is_even` too tangled to
lay out). A disp program is code, a value, applied to its arguments: there is no computation in it
to share, so all that sharing saves is room at load, paid for in copying and in distance. On a
lattice a part used in two places is a wire between them. `set=share=8` in the player's link shows
the shared parts once, behind their duplicators, in the graph view and as `x₁` in the details.

**Leases (tried, not kept).** Demand recomputed every step from the normalizer stops a
thrown-away `(b c)` at once, rather than when erasers reach it: in the ideal model fib(2)'s extra
rewrites fall from 38% to 4%, but much of the speed goes too (size-self 10.9× fewer steps → 2.6×),
since an argument loses its lease while it sits in a pair waiting to be triaged. Letting leases
pass through pairs recovers some. It also needs demand sent again and again: more pulses and
state.

**Keeping the term's parts apart (tried, not kept).** The player colours agents by segment (the
parts of the term the root computes, split at every application), but on the lattice the parts
interleave. Three energies, all off by default, push them apart. Each uses only what an agent's
site and its neighbours hold, and an idle agent pays it as it steps:
- *strangers* (`strangers=w`): w for every agent in its site or a neighbouring one that is not at
  the end of one of its wires within two strands (`reach`), as in a force-directed drawing of a
  graph;
- *tree labels* (`trees=w`): w for every such stranger with another label. Every agent carries a
  2-bit label its reader gives it (`label_from`): an application's function and argument get labels
  different from each other and from the application's, and anything else's inputs share its label.
  Labels are set from the root down when a term is loaded, and a rewrite's fresh agents take the
  consumer's. On every turn, an agent whose reader is in its site or one strand away takes the
  label that reader gives (`relabel`), so labels follow the tree wherever readers touch what they
  read. Then 88–97% of the agents the root's computation reaches carry the label their reader gives
  them;
- *garbage* (`garbage=w`): what an eraser reads takes a fifth label, passed down the garbage the
  same way, and a garbage stranger next to a live agent costs w more.

To measure it, `readback.rs` `mixing` cuts the root's term into the player's segments and counts
the pairs of agents in one site or in neighbouring sites that belong to different segments, over
the share expected if the same agents were scattered at random (1: as mixed as chance, 0: every
segment apart). `strands-run --mix 50` averages it over a run, sampled every 50 clocks. On the ten
disp programs (lattices sized as the player sizes them, 2 seeds each, every answer checked), against
the current design, geometric means of the clocks to the answer, the rewrites, and peak strands and
sites:

| setting | mixing | clocks | rewrites | peak strands | peak sites |
|---|---|---|---|---|---|
| the current design | 0.30 | | | | |
| `strangers=1` | 0.29 | −1% | +1% | +2% | +3% |
| `strangers=3` | 0.23 | +4% | +3% | +8% | +13% |
| `trees=4` | 0.19 | +2% | +1% | +2% | +3% |
| `trees=4 garbage=4` | 0.19 | +3% | +1% | +4% | +3% |

Tree labels make the parts about a third less mixed at every depth of the term (cut at the root's
first application, 0.29 → 0.19), for the price of seed noise: per program the clocks move between
−7% and +6%. Weaker labels (`trees=2`) separate less (about 0.23) and stronger ones (`trees=8`) a
little more (about 0.18) for 6% more clocks. Repelling every stranger separates less and spreads
matter out (13% more sites): what keeps parts apart is knowing which part an agent is in, not
keeping clear of everything. Garbage labels add nothing measurable. Separation gains no speed
either, as with the demand field (only the reader's partner is related, Fields above): rewrites
happen where parts meet, and walkers and called values do not pay. So the parts can be kept apart
for 2 bits an agent (3 with garbage), but not faster. In the player the difference is hard to see:
most of a picture is the program's code, one long value, and the parts being computed are a small
knot at its end. Moves read sites two away (the neighbours of the site a step goes to), so neither
the chip nor the GPU runs these settings, and the GPU says so (`tables.rs` `gpu_unfit`); on a chip
a field of one bit for each label, spread one hop a clock, could stand in for that (untested). The
player takes such settings from its link, `#src=fib%202&set=trees=4`.

## Memo radius: merging equal computations (tried, not kept; `memo=r`)

The eager evaluator memoizes `apply(f, x)` by global ids, which a lattice does not have. The local
version is a radius: two equal computations closer than r sites are merged, one computing and the
other getting a copy of its result through a duplicator, and farther ones are both computed. r = 0
is the plain net, r = ∞ a memo table, which would need addresses. Equal means equal terms as they
read back (`memo.rs` `Terms` interns every output of the abstract net, duplicators transparent), so
a suspension `P(f, x)` built in two places from copies of one `x` is the other's twin. Merging two
suspensions is call by need across places, and `Dn·P` (force once, copy the value) does the rest.
Twins at different stages of evaluation read differently and are missed.

**How many twins there are.** `strands-memo TERM --every N --keys --ideal` reads every computation
the answer depends on (P, A, T1, Sel) every N clocks, counts those with an equal twin within each
radius, and estimates what merging them then would save: the idealised lazy machine (Fork, above)
run on from the net as it is, with and without the merges. `latest`, lattices sized as the player
sizes them, seed 1, every 100 clocks (200 for disp):

| program | computations alive | share of the time with a twin within 1 · 2 · 4 · ∞ sites | saved by merging then, at most |
|---|---|---|---|
| fib(3) | 14 | 7% · 17% · 27% · 32% | 21% of the work left |
| fib(4) | 21 | 8% · 19% · 27% · 37% | 22% |
| exp(1) | 7 | 3% · 4% · 4% · 4% | 9% |
| sort(2) | 13 | 0% · 1% · 4% · 6% | 10% |
| disp `fib 3` | 15 | 1% · 3% · 7% · 16% | 9% |
| disp `fib 4` | 22 | 3% · 10% · 18% · 28% | 9% |

- Few computations are alive at once, and most of the time none has a twin. When there is one, it
  can carry a fifth of the work left.
- Counted by a memo key instead (function and argument worked out to values, as the eager
  evaluator's memo sees them), there are 1.5× (lambada fib) to 2–3× (disp fib) as many twins: on
  disp fib most twins differ only in how far an input has been evaluated, and terms miss them.
- Samples 100 clocks apart miss twins that come and go between them: merging (below) finds 14 to
  40 within one site in a run.

**Merging them with a central detector** (`memo=r`, every 8 clocks, `memoevery`): the twins within r
are merged pair by pair. A fresh duplicator beside the one kept (a running computation rather than
a suspension) takes its output and hands one copy to its reader and the other, along a new wire, to
the dropped one's reader; erasers in or beside the dropped one's site take its inputs, and
collection does the rest. Seats and wires are planned first (free lanes, at most 8 pairings a site),
and a merge with no room waits. The abstract net changes alike, and `tests/memo.rs` checks every
invariant and the projection after every clock and every merge. Against `latest`, 2 seeds, every
answer the oracle's and every projection exact:

| program | clocks, rewrites without | rewrites at r = 1 · 2 · 4 · 8 · 16 | clocks at r = 1 · 2 · 4 · 8 · 16 | peak sites at r = 1 · 4 · 16 |
|---|---|---|---|---|
| fib(3) | 14.2k, 8.1k | −25% · −16% · −29% · −28% · −30% | −8% · −4% · −9% · −12% · −10% | 0% · +2% · +2% |
| fib(4) | 20.5k, 15.6k | −18% · −27% · −26% · −34% · −36% | −7% · −9% · −6% · −6% · −9% | −5% · −6% · −6% |
| exp(1) | 11.9k, 4.3k | −2% · −3% · −3% · −4% · −4% | −1% · +6% · +7% · +9% · +9% | 0% · +3% · +4% |
| sort(2) | 21.0k, 10.9k | −1% · −3% · −11% · −5% · −8% | +2% · −1% · −4% · −1% · −2% | −10% · −11% · −8% |
| disp `fib 3` | 24.1k, 9.0k | −12% · −9% · −16% · −16% · −16% | −7% · +1% · −2% · −3% · −4% | −1% · −6% · −6% |
| disp `fib 4` | 39.1k, 18.2k | −21% · −23% · −25% · −27% · −27% | −7% · −9% · −7% · −10% · −11% | −8% · −22% · −21% |

- **On fib it saves a fifth to a third of the rewrites, most of it within a site or two:** twins
  are born close. `MEMO_LOG=1` lists each merge (what the twins' inputs share, how far apart they
  are, how big). Lambada fib's are mostly equal code built apart applied to the two copies of one
  argument, as the S rule's two halves `(s c)(b c)` are when `s` and `b` are equal, born one or two
  sites apart. Disp fib's are most often applications whose function and argument are both equal
  values built apart, 1 to 6 sites apart.
- **It saves work, not time.** Clocks fall at most 12% on fib, about the seed noise. Twins run side
  by side (fork), so a merge takes work off a parallel branch, not off the longest chain. As with
  caching what a program throws away (Fork, above), the lattice has room and time to spare.
- Elsewhere there is little to merge (exp, sort: 1–11% of rewrites), and exp(1) takes 6–9% more
  clocks.
- Where the matter peaks during the run (fib(4), disp fib) the peak falls too, by up to a fifth of
  the sites, as the dropped twins' matter goes.

**Can the detector be local?** With `memolocal=1` only computations in one 2×2×2 block of a cut at
the clock's offset are compared, every clock (`memoevery=1`), and the merge stays inside the block:
its three agents and five wires get the block's 8 sites, as a rewrite gets its square. With
`memonames=1` they are compared by names instead of terms (below). Rewrites and clocks against
`latest`, 2 seeds, every answer the oracle's:

| program | central, r = 16 | terms, in a block | names, in a block |
|---|---|---|---|
| fib(3) | −30%, −10% | −23%, −9% | −17%, −10% |
| fib(4) | −36%, −9% | −21%, −6% | −23%, −8% |
| exp(1) | −4%, +9% | −5%, +8% | −1%, 0% |
| sort(2) | −8%, −2% | −8%, −4% | −5%, −4% |
| disp `fib 3` | −16%, −4% | −12%, −2% | −12%, −4% |
| disp `fib 4` | −27%, −11% | −26%, −10% | −18%, −5% |

A block keeps between half and all of what the central detector gets at r = 16, by terms or by
names. What a chip would need for it:
- **Names, not terms.** A term read back changes whenever an input is evaluated, and so does every
  reader's up to the root: a wave of hashing along every chain of readers after every rewrite. A
  name fixed when an agent is made never changes: a hash of its tag and its inputs' names
  (duplicators transparent; a suspension and an apply count as one application), and it goes on
  meaning what it meant, since a rewrite keeps a computation's value (`memo.rs` `Names`, here given
  when the detector first sees an agent). A rewrite would make its fresh agents' names from the
  names the dying pair keeps for its inputs, inside its square; collection needs none. So every
  agent keeps its two inputs' names.
- **Bits.** A wrong merge is a wrong answer, so names must not collide. A run compares about 10^7
  pairs (a few hundred busy blocks for tens of thousands of clocks); 48-bit names make a collision a
  10^-7 chance a run, 32-bit ones 10^-3. Two names an agent and two agents a site is about 190 bits
  more a site, on the 95 it has now, and a hasher in the rewrite stage and comparators in every
  block.
- **A cheaper piece.** Most of lambada fib's twins come from one S rule, whose `(s c)` and `(b c)`
  are twins exactly when `s` and `b` have one name. An S rule that compares the two names it holds
  could build `(s c)` once and copy it, with no comparison in the block and no merge move. That
  leaves disp fib's twins, which are born apart.

So the idea holds for work but not for time: equal computations are born close enough that a block
catches most of them, but they run side by side anyway, and catching them would triple a site.
Memo stays the optimizer's job (research/OPTIMIZER.typ). The detector is central and CPU only; the
GPU refuses it (`tables.rs` `gpu_unfit`).

## Adding binary numbers

disp numbers are unary, so `add a b` takes a steps one after another, each about 0.8k rewrites
and 3.5k clocks. `programs.disp` also has binary numbers and several adders for them, to see how
parallel and local addition can be here:
- **Number trees.** A bit is `b0 = t` or `b1 = t t`, and a pair of number trees is its low half
  then its high half: `pair lo hi` is lo + 2^(bits in lo) × hi. One triage tells the three apart.
  A list of bits, least significant first, is a number tree (its nil is a `b0` on top), and so is
  a perfect tree of 2^k bits, so every adder takes either shape and the shape alone decides how
  parallel it can be. Each gives `pair sum carry`, a number tree again; the player shows these as
  numbers (`-> bits`).
- **A column** (`column`): bits x and y kill the carry (both 0), pass it on (they differ) or start
  one (both 1). `kind` picks a function of the carry in by that, so a column that kills or starts
  a carry knows its carry out before its carry in.
- **Ripple carry** (`ripple_add`): each column's carry out goes into the next.
- **Carry select** (`select_add`): each half is summed with carry in 0 and with carry in 1 at
  once, and the low half's carry out picks the high half's sum.
- **Carry lookahead** (`lookahead_add`): each subtree gives its carry rule (its carry out for
  carry in 0 and for 1; two `pick`s join two rules) and its sum as a function of the carry in,
  which sends the carries back down. O(log n) deep.
- **Carry save** (`saved`, for a sum of many numbers): a digit tree may also hold 2s (`t b1`).
  Adding a binary number into one moves each column's carry one column up and no further, so no
  column waits on another (`save`: ripple's recursion with another column). The carries are done
  once at the end, by lookahead (`resolve`). `added` adds the numbers one by one by lookahead.

Every carry going the whole way (2^n − 1 plus 1), clocks and rewrites, lattices sized as the
player sizes them, 2 seeds, every answer the oracle's and decoded to the sum:

| adder | 2 bits | 4 bits | 8 bits | 16 bits |
|---|---|---|---|---|
| unary `add` | 14.0k, 3.0k | 56.2k, 12.2k | 910k, 198k (1 seed) | — |
| ripple, list | 15.2k, 5.9k | 24.6k, 11.1k | 47.1k, 20.9k | 94.8k, 40.4k |
| ripple, tree | 9.9k, 3.6k | 17.1k, 9.0k | 28.0k, 19.7k | 65.5k, 41.3k |
| carry select, tree | 10.6k, 4.1k | 20.9k, 10.3k | 34.7k, 22.6k | 92.4k, 47.2k |
| carry lookahead, tree | 11.3k, 4.0k | 19.2k, 10.0k | 36.4k, 22.2k | 62.4k, 46.2k |

16 bits on ordinary inputs, 51566 + 22995 (the longest run of columns passing a carry on is 4):
ripple on a list 92.2k clocks, ripple on a tree 37.9k, carry lookahead 56.6k, carry select 65.6k
(1 seed). Lookahead on a list takes 99.2k with every carry going the whole way (1 seed), no
better than ripple on a list: the shape sets the depth, not the algorithm.
- **Binary passes unary at 4 bits.** At 8 bits unary takes 19–32× the clocks and 9× the
  rewrites, and 16 bits (65,535 steps) is out of reach.
- **Work, not depth, sets the time.** Every binary adder does 2.5–3k rewrites a bit (two fifths
  duplicators copying, a sixth erasers), so its work is linear in its bits, and its clocks grow
  1.6–2.7× with each doubling of the bits, lookahead's too. What a tree buys is rewrites at once:
  0.4 a clock on a list, 0.5–0.8 on a tree, 1.1 for ripple on a tree on ordinary inputs. A pair
  is made 35–85 clocks before its reader is wanted, and fires 20–50 clocks after that.
- **Laziness makes ripple carry skip.** The carry is only a wire, so every column starts at once,
  and a column that kills or starts a carry has its carry out at once. Only a run of columns
  passing a carry on waits, one column after another. On a tree that makes plain ripple carry the
  fastest adder: 1.5× faster than lookahead on ordinary inputs, and about as fast (65.5k against
  62.4k) when every carry goes the whole way. Carry select gains the same way (92.4k → 65.6k), as
  its picks wait only on carries still being passed on. On a list the walk down the list costs
  the same either way.
- **Carry select is the slowest tree adder**: it sums every half twice, while the S rule already
  runs the halves side by side.
- **Trees cost space**: at 16 bits 28–34k strands and 12–14k sites at the peak, against 7k and
  4k for a list.

A sum of four numbers below 64 as 8-bit trees (1 seed): `added` takes 112.9k clocks and 68.7k
rewrites, three 8-bit lookahead adds end to end with no overlap (3 × 36.4k). `saved` takes 86.4k
clocks, 23% fewer, as the saves overlap, but 98.3k rewrites, 43% more, for splitting the digits
and the bigger program; it also holds less at the peak (40k strands and 16k sites, against 45k
and 21k). On the player's example, three 3-bit lists, `saved` is the slower (50.4k clocks against
33.9k): three numbers do not pay for the final lookahead.

The eager evaluator (`src/run.ts --stats`, steps beyond reading the inputs) needs 25–60× fewer
steps than the lattice needs rewrites: equal trees are one node and equal applications one memo
entry, and nothing is copied. On 2^n − 1 plus 1 a tree's halves are equal trees, so it adds 16
bits in 0.7–1.0k steps on a tree against 1.8k on a list; on 51566 + 22995 it takes 1.5–1.8k
either way. Unary 255 + 1 takes 8.5k.

The tests run with `npx tsx src/run.ts evaluators/rust-ic-strands/programs/programs.disp`.

## Superpositions

A *superposition* `&ℓ{a,b}` is one value that is `a` in one universe and `b` in another, as in
HVM's [Interaction Calculus](https://raw.githubusercontent.com/HigherOrderCO/HVM3/HEAD/IC.md).
Whatever reads it splits in two from there on, one copy reading `a` and one reading `b`, and its
result is a superposition of the two; what was done before anything looked at the superposed
value was done once for both. The label ℓ (1 to 255) names the choice: the same label picks the
same side everywhere, so a term holding n labels stands for 2ⁿ plain terms, its universes, and
its answer collapses to theirs. That is how a search can run many candidates as one.

**The rules** (rust-ca-lattice `rules.rs` `SUP_RULES`). A new value `Sup` [value, first side,
second side], and a label on every agent: a superposition's, or a duplicator's; 0 for everything
else, and a duplicator of label 0 is a plain copy. A rule says the label of each agent it makes
(none, the consumer's or the producer's). Writing `Dn_ℓ` for a duplicator of label ℓ:
- `A·Sup`: `&ℓ{a,b} x` becomes `&ℓ{a x₀, b x₁}`, x copied by a `Dn_ℓ`;
- `T1·Sup` and `Sel·Sup`: the same, the arms copied by a `Dn_ℓ`, so a duplicator can now meet a
  pair: `Dn·Pair` copies it as `Dn·F` copies a fork;
- `Nrm·Sup` normalizes both sides; `Eps·Sup` erases both;
- `Dn·Sup` with the same label hands its first copy the first side and its second copy the
  second (it makes nothing); with another label, 0 included, it copies both sides and gives each
  copy a superposition of its copies;
- the duplicators a duplicator makes copying a value (`Dn·S`, `Dn·F`, `Dn·P`, `Dn·Pair`) take its
  label, so a labelled copy stays labelled all the way down.

`Unp·Sup` cannot happen: an unpair reads a triage's or dispatch's arms, and those only ever carry a
pair that `A·F` or `T1·F` made, or a duplicator's copy of one. The 8 rules are kept apart from the
other 26, which are all that the cascade's 5-bit rule numbers, the chip and the GPU hold. A net
with superpositions runs on the abstract net and on the lattice's CPU; the GPU refuses it
(`tables.rs` `gpu_refuses`: its 40-byte site has no room for labels), so the player stays on the
CPU for it.

**On the lattice** every seat keeps its agent's label, 8 bits, through steps, exchanges, rewrites,
saved states (`held_labels` beside `held_sites`) and hand-overs. A superposition is read whole: a
rewrite that makes one feeding a wanted reader also wants the computations feeding its two sides
(`fresh_wanted_in`), so both universes run at once.

**Writing them** (`term::parse_sup`; the player's input box takes the same): `&1{a,b}` anywhere a
term goes, a and b in any of the term notations (`L`, `S(x)`, `F(x,y)`, `@(f,x)`, ternary digits).
Terms side by side apply left to right, at the top and now inside any bracket too. So `<add's tree>
20200 &1{2020200,202020200}` is disp's `add 2` applied to 3 or to 4, which the player writes `add 2
&1{3,4}` (a list holding one, `[&1{1,3}, 2]`, goes in as `F(&1{…},…)`); `fib:&1{2,3}` is a
benchmark program on a superposed argument. An answer is shown collapsed: a superposition for each
of the input's labels whose universes differ (`sup::collapse`), so it equals the oracle's answers,
collapsed the same way, exactly when every universe agrees; `-> nat` and the other kinds decode
each side.

**Checked** (rust-ca-lattice `tests/sup.rs`, `crate/tests/sup.rs`): random terms with
superpositions in random places, with 1 to 3 labels so that labels repeat (and must stay
correlated), run to the answer and collapsed, against the oracle run on every choice. On the
abstract net 2384 terms, 7710 universes; on the lattice in the current design 120 terms, 354
universes, 10 of them re-checking every invariant after every move. Every new rule fires on both.
The root read back as a superposed term, reduced by the oracle in each universe, is the answer at
every 12th clock; a saved state put back with its labels runs on exactly; terms without
superpositions run exactly as before.

**How much the universes share** (`strands-sup`): each call superposed, against its two universes
run one at a time. Rewrites on the abstract net to quiescence (`Net::reduce_listed`; eager, so
9–150× the lattice's), then clocks and rewrites to the answer on the lattice in the current design,
grids sized as the player sizes them, 2 seeds, every answer checked universe by universe. *Shared*
is what the superposed run saves on the abstract net, over the smaller universe's rewrites;
*first fork* is how far into the lattice run (in rewrites) the first `A·Sup`, `T1·Sup` or `Sel·Sup`
fires, the first time anything looks at the superposed value:

| superposed | abstract net: one by one → superposed | shared | lattice clocks: one by one, longest → superposed | lattice rewrites: one by one → superposed | first fork |
|---|---|---|---|---|---|
| disp `add 2 &1{3,4}` | 65.5k → 33.0k | 99% | 21.3k, 10.8k → 11.0k | 4326 → 2269 | never |
| disp `size [&1{5,9}, 6, 7]` | 49.2k → 25.1k | 98% | 26.8k, 13.6k → 14.3k | 5546 → 2800 | never |
| disp `rev [&1{1,3}, 2]` | 140.2k → 70.7k | 99% | 33.8k, 17.1k → 17.2k | 10.4k → 5263 | never |
| disp `greet &1{"a","b"}` | 142.9k → 90.8k | 73% | 32.1k, 16.2k → 17.1k | 8203 → 4421 | never |
| disp `isort [&1{2,0}, 1]` | 429.7k → 258.4k | 97% | 51.4k, 33.5k → 34.5k | 14.9k → 9730 | 50% |
| disp `sum [&1{1,2}, 2]` | 193.6k → 116.5k | 85% | 36.5k, 18.4k → 18.8k | 13.9k → 8498 | 17% |
| disp `doubled [&1{1,3}, 2]` | 245.5k → 148.6k | 88% | 40.5k, 20.5k → 21.4k | 15.4k → 9942 | 12% |
| disp `mul 2 &1{2,3}` | 311.3k → 220.0k | 64% | 40.4k, 21.8k → 25.6k | 16.8k → 13.5k | 11% |
| disp `add &1{2,3} 3` | 77.8k → 70.7k | 22% | 24.6k, 14.1k → 17.1k | 5174 → 5016 | 7% |
| disp `is_even &1{3,4}` | 58.6k → 54.3k | 17% | 28.6k, 15.9k → 19.1k | 5653 → 5266 | 3% |
| disp `mul &1{2,3} 2` | 348.9k → 332.9k | 11% | 41.0k, 22.2k → 25.8k | 18.8k → 18.9k | 2% |
| disp `fib &1{2,3}` | 244.3k → 234.7k | 11% | 39.5k, 24.1k → 27.4k | 13.7k → 13.6k | 1% |
| disp `&1{add,mul} 2 2` | 175.9k → 175.9k | 0% | 29.1k, 18.6k → 19.2k | 9791 → 10.2k | at once |
| `fib:&1{1,2}` | 223.0k → 177.5k | 83% | 13.6k, 10.4k → 11.3k | 6542 → 6420 | 4% |
| `sort:&1{1,2}` | 1904k → 1713k | 65% | 22.3k, 21.0k → 20.9k | 11.2k → 11.4k | 1% |

- **A value only passed along is shared whole.** disp's `add` walks its first argument and puts the
  second at the bottom; `size` never looks at the items, `rev` and `greet` only move them. Then
  nothing forks, the superposition rides through to the answer (only `Nrm·Sup` at the end, and
  duplicators copying it as plain copies do), and the run costs one run: half the rewrites, the
  clocks of the longer universe.
- **A value looked at at once is shared hardly at all.** Where the program triages on the
  superposed value first thing (`add`'s and `mul`'s first argument, `fib`, `is_even`, the lambada
  programs, two different programs), the run forks within the first 1–7% of its rewrites and from
  there each universe does all its own work: as many rewrites as the two apart, −7% to +4%.
- **In between, it splits where it looks.** `isort` compares the superposed item halfway through,
  `sum`, `doubled` and `mul`'s second argument a sixth or a tenth of the way in, and walking the
  list or the recursion up to there is done once: 20–39% fewer rewrites. Each later step that
  looks at the value forks again (`mul 2 &1{2,3}`: 10 forks, 40 copies of a superposition).
- **The universes run side by side.** Even with nothing shared, the superposed run takes 1.0–1.2×
  the clocks of the longer universe alone, 0.6–0.95× the two one after the other: two computations
  on one lattice at once, as fork runs both halves of the S rule.
- The eager abstract net shares more than the lattice (lambada `sort`: 65% against none):
  evaluated eagerly, a program also works on its own code, which does not depend on the input
  and so is done once.

**A search over candidates.** Superposing candidates, each choice its own label, runs them as one;
which universes give the wanted answer names the candidates that do:
- *Four adders* (programs.disp): `&1{&2{ripple_add, select_add}, &2{lookahead_add, add}}` applied
  to 13 and 6 as 4-bit number trees. Three universes give 19 and the fourth, unary `add` on binary
  numbers, 48. Different programs share nothing (32.1k rewrites either way), but the four run side by
  side: 24.4k clocks, against 20.1k for the slowest alone and 69.5k for the four one after another.
- *Every tree of depth 2 as the function*, the search space as one superposition: `f = &1{L, &2{S(g₁), F(g₂, g₃)}}`,
  each `gᵢ = &{L, &{S(L), F(L,L)}}` with labels of its own, 8 labels in all, so 256 universes and 13
  distinct trees. `F(f L, f S(L))` uses f twice with the same labels, so each universe applies one
  tree to both examples. Wanting `F(F(L,L), F(L,S(L)))` (f x = F(L, x) on both), 32 universes give it,
  all with f = `S(L)`, K. Superposed: 204 rewrites and 458 clocks on the lattice, against 261 rewrites
  and 1264 clocks for the 13 trees one after another (147 for the slowest). Applying a stem does not
  look at its child, so `S(g₁) x = F(g₁, x)` applies the three stems at once.

**Copying forces.** A duplicator copying for a superposition is still the need-duplicator: copying
a suspension forces it (`Dn·P`). So an argument a universe never reads is computed anyway when a
superposed function is applied to it: `K L (fib 3)` throws `fib 3` away in 728 rewrites on the
abstract net, and `&1{K,K} L (fib 3)` (both sides K) computes it, 158.5k. On the lattice it costs
little here (24 rewrites against 7), as the universes throw their copies away before the duplicator
gets far and erasers collect it, but an argument that does not terminate could stop a run the oracle
finishes. None of the random terms did. A triage or dispatch on a superposition copies its arms the
same way, both arms though only one is taken.

## Reading back

`crate/src/readback.rs` reads what any piece of the net means right now, for the player's
reduction-state panel. It only reads; a run goes exactly as without it.
- **Meaning.** Each output means a term, read off the abstract net by the rules: a value is a
  tree; a suspension and an apply's result are applications *f x*; a triage on *a* with arms
  ⟨*b*, *c*⟩ is *t a b c* (what `A·F` made it from); a dispatch on *z* with arms ⟨*w*, ⟨*x*,
  *b*⟩⟩ is *t (t w x) b z*; an unpair's outputs are its pair's parts; a normalizer is its input;
  a duplicator's two copies are one value, read once and shared. `tests/readback.rs` checks that
  at every 12th clock of 64 runs the root's term, reduced by the oracle, is the answer.
- **A computation** is an agent and, recursively, every agent feeding its inputs. A duplicator's
  input belongs to every computation reading either copy.
- **Following a pick.** When a picked agent is rewritten, whatever reads its outputs now reads
  what it became, so the pick moves there. When the reader only passed the value on (a
  duplicator, a normalizer, a pair) and went too, the pick moves on past it: to both copies, to
  the normalized value, to the pair's reader. A forced suspension moves to the apply computing it.
  A pick whose value a rewrite uses up drops out. Checked: whatever a pick follows still reduces to
  what it did.
- **Across a GPU.** A GPU hands its stretch back as bare sites, so the abstract net is rebuilt
  with every agent renumbered. Picks are found again by neighbourhood: tags and ports out to 8
  wires that no other agent has in either net (then 6, 4, 3 within a clock's reach), and from
  those, neighbours whose own surroundings are unchanged. Seats alone fail, as look-alike agents
  move into each other's seats within a stretch. Against a CPU run that keeps its ids, after
  stretches of 50 clocks, 89% of picks are found again and none is mistaken for another.
- **The details drawer** writes each pick as disp: applications as *f x y* (a fork *t a b* is an
  application of *t* too), numbers, lists and strings as literals, code equal to a known
  definition by its name (matched by a hash of its ternary form), other code over 6 nodes as
  ‹the definition it is a piece of› or ‹its size›, computations shared by duplicators as *x₁* in
  a *where*, and what is being computed underlined in blue. A program's answer, as far as it is
  built, reads `succ (…)` or `cons x (…)`. Clicking part of a term lights its agents.

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
- **Adders are bound by work, not depth** (Adding binary numbers, above): two fifths of their
  rewrites are duplicators copying. With less copying a tree's O(log n) depth might show.
- **Superpositions that copy lazily** (Superpositions, above): a duplicator copying for one forces
  what it copies, arms a universe will not take included. A copy that leaves a suspension
  suspended would keep the oracle's laziness, at the price of computing it once per universe.
  Labels on the chip and the GPU (8 bits a seat) are untried. An eraser collecting a labelled
  duplicator hands its other copy the whole superposition: the answer collapses the same, but the
  side that universe will never pick is carried along and worked on.

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
cargo test --release                                  # ~9 s: corpus in 6 configurations (one with the demand field, one keeping the parts apart), per-move invariants, collection, handover, reading back, merging equal computations, superpositions
cargo run --release --bin strands-run -- disp-t --k 2 --lanes 3 --block --lazy --temp 2
cargo run --release --bin strands-run -- fib:0 --grid 256 --depth 8 --k 2 --lanes 4 --block --lazy --temp 2 --swap 1 --agents 0.8 --pulse --gc --idle-crowd 10 --board 0.5 --pairs 8
cargo run --release --bin strands-run -- fib:0 --grid 256 --chip                   # the chip's schedule (blocks)
cargo run --release --bin strands-run -- fib:0 --grid 256 --chip --demand          # with the demand field (field.rs; --field SPEC for others)
cargo run --release --bin strands-run -- fib:0 --grid 256 --latest                 # the current design: the demand field and fork
cargo run --release --bin strands-run -- fib:2 --grid 300 --depth 8 --k 2 --lanes 4 --block --lazy --temp 2 --swap 1 --agents 0.8 --pulse --gc --idle-crowd 10 --board 0.5 --pairs 8 --budget 5000000000 --clean 100000
cargo run --release --bin strands-run -- 'fib:&1{1,2}' --grid 300 --latest        # a superposed argument (Superpositions, above)
cargo run --release --bin strands-sup -- CASES                                     # lines name|term[|want]: superposed against its universes alone
cargo run --release --bin strands-sweep -- "k=2 lanes=2 temp=2.0 grid=48 block=1" "k=2 lanes=3 temp=2.0 grid=48 depth=6 block=1 lazy=1 pulse=1 swap=1 agents=0.8 gc=1 idlecrowd=10 board=0.5 pairs=8"
```

`--chip` is the configuration `hw/` builds; `hw/validate.sh` checks that the chip and the GPU
version match it (see [`hw/README.md`](hw/README.md)). `crate/gpu.sh TERM --grid N [--depth D]`
runs a term on the GPU (`--check` in lockstep with the simulator, `--dense` without the tiles,
`--vectors FILE...` replays recorded turns, `--latest` runs the current design, `key=value` changes
a setting as `strands-sweep` spells it); `strands-hw --wgsl` prints its generated tables.
`player/dawn.sh check 'src=sort:1'` checks the browser's GPU path on the design the player shows
(`player/firefox.sh check` the same in Firefox; `player/firefox.sh player 'src=add 3 4'` times
the player itself there, `player/firefox.sh input` checks its input box and compiler, and
`player/firefox.sh pick 'p=disp:fib&a=2'` checks its picks and their reading back, followed to the
answer), and `player/dawn.sh bench 'src=fib:1'` times it against the browser's CPU engine
(`copies=16&clocks=512` for many copies side by side, `demand=0&fork=0` for the chip's schedule alone). `TRACE=file strands-run ...` writes when each rewrite's pair
first existed, when its reader was first wanted, when it fired and how far apart the pair was.
`strands-run --clean N` runs on past the answer (up to N clocks) and reports when only the
answer is left; `PIECES=1` lists the connected pieces of the net at the end.
`strands-run --profile` prints what wanted readers spend their time waiting on, how crowded it
is where walkers go, and what switchboards hold; `--progress N` prints the wanted readers and
their wire lengths every N proposals, and `WHO=1` adds what each one is waiting on, which is
how the 2D jam was diagnosed. `strands-run --mix N` prints how mixed the root's segments are,
sampled every N clocks, and how many agents carry the tree label their reader gives them (Keeping
the term's parts apart, above); `strands-run` takes `key=value` settings as `strands-sweep` spells
them, applied after the flags (`--latest trees=4`). `strands-memo TERM --every N --keys --ideal`
prints the census of equal computations (Memo radius, above) on a lattice sized as the player sizes
it, and `strands-memo TERM --run memo=R` one line of counts for a run (`@file` reads the term from a
file; `MEMO_LOG=1` lists the merges).
