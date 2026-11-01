# rust-ic-linear-strands

A copy of `evaluators/rust-ic-strands` (the strand lattice: wires physical, nothing addressed) with its own
copy of `tree-net`, running the website's two-way programs (website/src/lib/evaluators/twoway) as a **linear net**:
no agent is ever copied or erased. The copy is so the lattice can be changed for this without touching
strands; everything strands does still runs here.

- **The design** (`lin/rules.ts`, 218 rules over 62 new kinds of agent, every one of at most 4 ports): the
  program is compiled into a tree of agents (a gate for each move; `a; b`; a tri for each shape at the
  value's left; map on a stem and on a fork's halves). The value's top node carries the run (a root, with
  a front and a back port and its children), and every program node's principal port faces the run, so a
  node's facing is all it knows of where the run is. Flip relabels single agents: a root, head or reader
  swaps its line ports and direction, a tri reading the value finishes and then turns, a map holding a
  finished half sends it back down.
- **Recursion is unrolled before the run** (`lin/compile.ts`): the interpreter's run says which fix and
  self each call reaches, and those are laid out as a static circuit for this input; a self never reached
  is a `Gap`. The run back goes through the same calls.
- **On the lattice** a program node is wanted (walks, reacts) only while it faces a run
  (`Lattice::lin_want`); the rest of the circuit is idle matter that walkers trade places with. With every
  program node wanted, as two-way programs' walkers are, a large circuit gridlocks.

Running it:
- `npx --prefix website tsx evaluators/rust-ic-linear-strands/lin/check.mts [quick]`: the rules on the
  abstract net against core.ts's interpreter, and flipped after every round back to the net as loaded.
- `lin/gen-rust.mts` writes the rules into `tree-net/src/lin.rs` and the player's names (`player/lin-kinds.js`);
  `lin/fixtures.mts` writes `crate/tests/lin-fixtures.txt`; `cargo test --release --test linear` runs each
  case on the lattice and flips it halfway (about 6 minutes, nearly all of it `addw` on 2-bit words).
- `./build-player.sh`, then open `player/index.html` and type `lin: notw @ 0b10110010` (examples under ▾);
  ⇆ or X flips every run.
