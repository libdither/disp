# The strand lattice as a chip

`rtl/` is a synchronous chip for the block schedule of `crate/src/lattice.rs` (`--chip`: blocks
with a turn for every site, demand pulses, collection, switchboard crowding, at most 8 pairings).
It does exactly what the simulator does, bit for bit; `validate.sh` checks that, and with
`--layout` lays the chip's tile out on an open, manufacturable process (IHP SG13G2, 130 nm).

```sh
hw/validate.sh            # ~4 min: tables, vectors, every turn and block, whole lattice clock by clock
hw/validate.sh --layout   # + synthesis, place and route, design rules, layout vs schematic, timing
```

Run it after any change to the simulator's block schedule: if the simulator and the chip
disagree, the script says which move, at which clock, which site, and which bits.

**Status.** The logic is checked: the block unit matches the simulator on every recorded turn
and block (about 600,000 records over 15 workloads, every move and all 26 rules), and the whole
lattice matches clock by clock. The layout flow is proven on OpenROAD's example design (IHP's
design-rule and layout-vs-schematic decks both clean), but the block unit itself is not laid out
yet: synthesized as written it is about 400,000 cells, most of them multiplexers moving 219-bit
sites around, and it is being made smaller first.

## What the chip is

- **Sites** hold 219 bits: a switchboard of 33 ends at 6 bits (the 9 port ends include the
  transient slot an exchange uses), three slots' tags and wanted bits, and the pulse.
- **Block units** own the 8 sites of a 2×2×2 block. Each clock the simulator picks a random
  offset for the partition; the chip instead keeps the units still and shifts the lattice's
  contents up by the offset (a cycle per axis), runs every unit, and shifts back. The physical
  array is one site larger than the lattice along each axis, and the extra sites stay empty.
- **A unit's clock**: it asks each site whether a reaction is ready, then gives its sites their
  turns: ready sites, then sites with agents, then bare wire, each group in block order rotated
  by the block's random word, skipping sites an earlier turn changed. A turn runs as stages:
  collect, find a pair, rewrite, then infect and step, exchange, fold or flip, then drop pulses
  whose strand was rewired.
- **Rewrites** set up the rule's links in one cycle, then try candidate squares in the
  simulator's order and, within a square, every seating in the simulator's search order, one per
  cycle, keeping the cheapest that fits; then apply. This is the slow part: a rewrite with five
  fresh agents searches 1024 seatings per square.
- **Pulses** cross one strand per clock in a phase of their own, between neighbouring sites.
- **Randomness** has no state: a turn's 64 random bits are a hash (eight add-rotate-xor rounds)
  of the site's coordinates and the clock; the offset and a block's rotation are hashes too.
- **Energies** are integers in quarter units; acceptance compares 16 random bits with a table.

Everything the chip needs from the rules (the 26 rules, which fresh agents start wanted, the
energies, the acceptance table, the probabilities) is generated from the simulator by
`cargo run --bin strands-hw` into `rtl/strands_tables.vh`.

## How it is checked

- **Turns and blocks**: the simulator writes test vectors (`VECTORS=file strands-run ... --chip`):
  for every rare move (rewrite, collection, exchange, fold) and a sample of steps, flips and idle
  turns, the block's 8 sites before and after, the sites written and whether the turn was dropped;
  and whole blocks before and after. `sim/tb_block.cpp` replays them through the block unit. The
  workloads cover every move and all 26 rules (`rules`, small terms, three terms written to reach
  the rules that force suspended applications, fib(0) and sort(1)).
- **The lattice**: the simulator dumps every site after every clock (`DUMPS=file`), and
  `sim/tb_lattice.cpp` runs the whole chip (shifting, 50 block units, pulses) on an 8×8×2 lattice
  clock by clock from the same start, comparing every site.
- **Layout** (`flow/layout.sh`): OpenROAD-flow-scripts places and routes one block unit; then IHP's
  own KLayout decks check the result: design rules on the final GDS, and layout versus schematic
  against the routed netlist.

`eda.sh` runs any command with the tools (from nixpkgs) and fetches the pinned flow scripts and
IHP's decks into `~/.cache/rust-ic-strands-eda` on first use.
