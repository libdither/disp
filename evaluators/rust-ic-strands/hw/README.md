# The strand lattice as a chip

`rtl/` is a synchronous chip for the block schedule of `crate/src/lattice.rs` (`--chip`: blocks
with a turn for every site, demand pulses, collection, switchboard crowding, at most 8 pairings).
It does exactly what the simulator does, bit for bit; `validate.sh` checks that, and with
`--layout` lays the chip's tile out on an open, manufacturable process (IHP SG13G2, 130 nm).

```sh
hw/validate.sh            # ~4 min: tables, vectors, every turn and block, whole lattice clock by clock, the GPU version
hw/validate.sh --layout   # + synthesis, place and route, design rules, antenna, layout vs schematic
hw/flow/size.sh           # ~2 min: cells per stage, to see what a change costs before laying it out
```

Run it after any change to the simulator's block schedule: if the simulator and the chip
disagree, the script says which move, at which clock, which site, and which bits.

**Status.** The logic is checked: the block unit matches the simulator on every recorded turn
and block (about 680,000 records over 16 workloads, every move, all 26 rules and every rarer
path of a turn), and the whole lattice matches clock by clock. The block unit synthesizes to
87,000 standard cells, 0.96 mm² (1.2 mm² once placed and buffered, a 1.65 mm square at 35%
use), places, and meets timing at 50 ns with 22 ns to spare after the clock tree (2 ns once
global routing's wires are counted); global routing finishes with no congestion. It is not
laid out to the end yet: the detailed router starts with about 170,000 short circuits and
spacing errors, almost all on Metal2, and does not get them down; what is left and what has
been tried is in [`TODO.md`](TODO.md).

## What the chip is

- **Sites** hold 219 bits: a switchboard of 33 ends at 6 bits (the 9 port ends include the
  transient slot an exchange uses), three slots' tags and wanted bits, and the pulse.
- **Block units** own the 8 sites of a 2×2×2 block. Each clock the simulator picks a random
  offset for the partition; the chip instead keeps the units still and shifts the lattice's
  contents up by the offset (a cycle per axis), runs every unit, and shifts back. The physical
  array is one site larger than the lattice along each axis, and the extra sites stay empty.
- **A unit's clock**: it asks each site whether a reaction is ready, then gives its sites their
  turns: ready sites, then sites with agents, then bare wire, each group in block order rotated
  by the block's random word, skipping sites an earlier turn changed. A turn runs as stages, a
  cycle each: collect (an eraser per cycle), find a pair, rewrite, then infect and step,
  exchange (two cycles: the agent into the spare slot, then the resident back), fold or flip,
  then drop pulses whose strand was rewired.
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
- **Coverage**: the testbench is built with `-DCOVER`, and `validate.sh` fails unless the
  recorded turns reach every rarer path of a turn at least once: collecting here and across a
  strand, a site's second eraser collecting, rewrites here and across either axis, with no links
  and with six fresh agents, moves writing two and four sites, exchanges done and refused.
- **The lattice**: the simulator dumps every site after every clock (`DUMPS=file`), and
  `sim/tb_lattice.cpp` runs the whole chip (shifting, 50 block units, pulses) on an 8×8×2 lattice
  clock by clock from the same start, comparing every site.
- **The GPU version** (`crate/src/gpu`, the same schedule ported from `rtl/`): every recorded turn
  and block replayed through it, and the lattice run in lockstep with the simulator, including
  fib(0) on 232×232×8 with pulse phases fused into the next clock and only live tiles running.
- **Layout** (`flow/layout.sh`): OpenROAD-flow-scripts places and routes one block unit, from
  scratch; then IHP's own KLayout decks check the result: design rules on the final GDS (the main
  tables, and the antenna rules IHP's runner leaves off unless asked), and layout versus schematic
  against the routed netlist. Where the flow's defaults fail on this design, `flow/` departs from
  them and says why at the spot: ABC's newer mapper, each stage on its own, buffering left to
  OpenROAD (`synth.tcl`; the usual mapper crashes on the move stage, and ABC's buffering adds
  50,000 cells); IHP's corrected routing tracks from upstream (`make_tracks.tcl`; the pinned flow
  gives Metal5 a seventh of its tracks); and no antenna repair in OpenROAD, which does not converge
  here (`config.mk`), so IHP's antenna deck has the last word.

Most of a block unit is the logic of a turn, and what made it big was moving whole 219-bit sites
through choices: a write to a site picked at run time, or a write done only under some condition,
copies all 219 bits through a multiplexer. So the unit reads and writes sites through ports, like
a register file: four read ports (the turn's site, and three the stage under way names) and four
write ports, each one multiplexer shared by every stage, and the stages work on what the ports
hand them. Within a stage, a write that should not happen becomes a write to no end, a site's few
changing fields are changed in place rather than the site copied, and moves that never run in the
same cycle share one copy of their logic (a step and both halves of an exchange share one hop).
What a stage works out in one cycle and later stages still need (the rewrite's rule, links and
candidate squares) is kept in registers, since the read ports move on.

`eda.sh` runs any command with the tools (from nixpkgs) and fetches the pinned flow scripts and
IHP's decks into `~/.cache/rust-ic-strands-eda` on first use.
