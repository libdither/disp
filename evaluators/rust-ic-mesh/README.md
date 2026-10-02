# rust-ic-mesh

disp's tree-calculus interaction net (the 26-rule table in `rust-ca-lattice/crate/src/rules.rs`)
running on a flat grid of identical tiles that only talk to their four neighbours. It is
meant to be buildable as a chip: every tile is a fixed amount of state plus a small router,
and every step a tile takes reads and writes only that tile.

Open `player/index.html` to watch it run. The page runs this crate's engine, compiled to
WebAssembly, so what you see is the engine itself, not a recording.

## The idea

The previous spatial machine (`rust-ca-lattice`, "the cascade") made wires physical: a wire
was a path of cells. Every rewrite then had to route new wires through a crowded 3D grid, and
the machine's hard problems all came from that: the relief and ring rules, the stuck docks,
and the 30 of 160 random terms it never finished.

Here wires are not matter. **A wire is an address held by the one agent that reads it.**
- Every wire joins a value *source* (a producer's principal port, a consumer's result port) to
  a *sink*. This is the polarity check in `polarity.rs`, run over the whole rule table.
- The sink stores the source's (tile, slot, port) address. Nothing is stored at the source.
- Each source has exactly one reader. So every request that ever reaches a source comes from
  one place. The only races left are between a source's own rewrite and that one request, and
  the source's tile settles those by handling one event at a time.

The result needs no locks, no arbitration between tiles, and no rules against cycles.

## What a tile is

A tile has `K` agent slots (default 8), a five-port router (four neighbours plus local) with
small input buffers, and a protocol engine that handles one event per tick. Messages move one
tile per tick, X first and then Y, which is the standard deadlock-free routing for a mesh.

There are eight messages:

| message | what it does |
|---|---|
| pull | a reader asks its source for the value |
| ship | a producer travels to the consumer that pulled it; the rewrite fires on arrival |
| moved | a waiting reader learns its source's new address |
| spawn | a fresh agent lands in a slot reserved for it in another tile |
| drop | a source's reader is gone |
| reserve / grant / release | a rewrite that doesn't fit in its tile finds room elsewhere |

A rewrite fires in the consumer's tile:
- **Placement.** Fresh agents take the consumer's own slot first, then free slots in the same
  tile.
- **Room.** If that isn't enough, the consumer holds the producer ("docks"), keeps the free
  slots it has, and sends a reserve. The reserve follows a free-space field (each tile knows its
  distance to the nearest tile with at least n free slots, for n = 1..6) to exactly the room the
  rule needs.
- **Results.** A result that someone is already waiting for is shipped straight to the reader
  when it's a producer, or subscribed on the reader's behalf when it's a computation.
- **Heir.** A result nobody has asked for yet goes into the consumer's own slot, permuted so it
  sits on the very port the eventual reader already holds. So no forwarding pointer is left
  behind in the common case.

## Demand and drop

- **Lazy by default.** Nothing runs until something asks. A consumer starts when someone
  subscribes to its result. The normalizer and the root always want their input, so asking for
  the answer pulls exactly the work the answer needs.
- **Erasure is a message.** An eraser doesn't wait for a value. It sends drop:
  - at a producer, the eraser rule runs where the producer stands and drops its children;
  - at a computation nobody can read any more, the computation is cancelled before it runs and
    drops its inputs.
- **Garbage collection is complete.** At the end of every run only the answer is left on the
  grid (e.g. 22 agents for a sorted 3-element list).
- **Speculation is a dial.** A computation nobody has asked for yet may start anyway when its
  tile has at least `speculate` free slots. Idle space and time buy parallelism; cancellation
  cleans up after wrong guesses.

## Why it is correct, and why it finishes

- **Correct.** Every interaction the mesh performs is replayed on the abstract net (`--check`,
  on in all tests) and must be a pair that net actually has. At the end, the whole mesh is
  compared with the abstract net, wire by wire. Cancellation is the one extra step, and it is
  mirrored there as "erase a computation whose results are all erased".
- **Finishes.** Routing can't deadlock: X-then-Y routing has no cycles of buffers waiting on
  each other, and a tile always accepts what is delivered to it. A rewrite that has docked never
  waits for anything but free slots, and slots are freed by other rewrites that never wait for
  it. So a run stops only when it is done, or when the grid is genuinely too full to fire. That
  case is reported as *out of space*, never guessed around.

## Hardware budget

For a 64×64 grid of 8-slot tiles, an address is 17 bits.
- **Slot:** about 96 bits (tag, phase, a 3-bit port permutation, three port fields with
  reader/dropped flags, and the docking fields).
- **Message:** at most about 80 bits, one flit.
- **Tile:** roughly 5 kbit of state. That is 8 slots, four 4-flit input buffers, a 32-flit
  outbox, and the 6-field free-space distance. The largest outbox and event queue seen in the
  benchmarks below are 21 and 13 entries.
- **Rewrite logic:** the rule table is 26 rows of at most 6 fresh agents and 9 wires; no tile
  ever searches.

For scale, that is about twice the RAM and ROM of one GreenArrays GA144 node. The tile's
logic is not yet written as gates (that is what `evaluators/gated-ca` set up).

## Results

`cargo run --release --bin mesh-bench` (every answer checked against the independent oracle;
96×96 grid, 8 slots per tile):

- **Old lattice's corpus.** The 160-term random corpus the cascade completes 130 of: **160 of
  160** here.
- **Old lattice's frontier.** Its five deep terms all complete; `disp-t` takes the same 18
  rewrites.

| program | speculation | rewrites | ticks | rewrites/tick | hops/rewrite | in own tile | peak live |
|---|---|---|---|---|---|---|---|
| fib(4) | off | 118k | 1.07M | 0.11 | 8.7 | 82% | 3.5k |
| fib(4) | ≥4 free | 260k | 234k | 1.1 | 8.2 | 86% | 3.6k |
| fib(4) | ≥1 free | 443k | 22k | 20 | 11.5 | 82% | 6.0k |
| sort(3) | off | 505k | 8.27M | 0.06 | 16.4 | 81% | 17.5k |
| sort(3) | ≥4 free | 922k | 656k | 1.4 | 10.5 | 85% | 10.8k |
| sort(3) | ≥1 free | 2.70M | 74k | 36 | 15.0 | 82% | 13.7k |
| size(size) | off | 209k | 1.10M | 0.19 | 4.3 | 86% | 3.1k |
| size(size) | ≥1 free | 362k | 33k | 11 | 7.5 | 81% | 3.3k |

For comparison, firing every active pair as soon as it exists (what the abstract net does)
takes fib(5) 845k rewrites, 41k ticks and 17k live agents. Full speculation here does it in
753k rewrites, 29k ticks and 9.4k live agents.

The simulator does about 0.5–0.9 million rewrites (7–9 million message hops) per second on
one core, in native code and in the browser alike.

## Running it

From `crate/` (wrap long runs in a memory cap, see `AGENTS.md`):

```sh
cargo test --release                           # ~10 s: soak, differential, tight grids, shapes
cargo run --release --bin mesh-run -- fib:4    # one program; also `@(F(L,L),L)` or ternary
cargo run --release --bin mesh-run -- sort:3 --spec 1 --grid 96x96
cargo run --release --bin mesh-bench           # the tables above
```

`../build-player.sh` rebuilds `player/engine.js` (it borrows a wasm linker through
`nix shell` when none is installed). The player's URL fragment holds the setup, for example
`player/index.html#p=sort&n=3&spec=1&heat=1`. Hovering a tile shows each slot's tag, phase
and the addresses its ports hold.

## Layout

- `crate/src/mesh.rs`: the tile, the router, every message handler, readback and the
  projection check.
- `crate/src/polarity.rs`: which ports are sources, checked against the rule table.
- `crate/src/term.rs`: term syntax, ternary programs, the benchmark workloads.
- `crate/src/run.rs`: load, run, report. `crate/src/wasm.rs` is the browser interface.
- `crate/tests/`: the cascade's soak corpus, squeezed grids, random terms, router shapes
  against speculation levels.
- `player/`: the visualizer.

## Open

- **Time to answer.** Pure demand is sequential along its demand chain, at about 9 ticks per
  rewrite. Speculation buys time with work. A smarter policy (e.g. speculate only where
  duplication will want both copies) is untried.
- **Gates.** The tile logic as a gate netlist, with measured gate count and depth.
- **Outbox overflow.** The outbox and event queue are unbounded in the simulator, so the
  numbers above are measured maxima, not proven bounds. A hardware tile needs either a proof of
  the bound or overflow into free slots.
- **One remaining forwarder case.** Indirections still appear when a rewrite fuses two outside
  wires (unpair meeting pair) before either end is read.
