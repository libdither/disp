# Linear tree calculus and linear interaction nets

Design analysis with prototypes, 2026-11-06 to 2026-11-10. Nothing here is in an evaluator. The numbers
for `nets/` and `lean/` were rerun from this folder on 2026-11-10, the others on 2026-11-08.

Names used in file names:
- **TC**: ordinary tree calculus.
- **LTC**: linear tree calculus, a tree calculus where exactly one rule drops (K) and one copies (W).
- **V5**: the final 9-rule LTC design (earlier drafts are V1–V4, and the 7- and 8-rule variants).

## The threads

1. **`tree-calculus/`: the 9-rule linear tree calculus.** One node, △; a redex is △ a b x, and the shape of a
   plus one more node picks the rule. Each rule eats three △. Triage hands back the arms it didn't pick,
   so nothing is dropped except by K. `final.py` checks everything in one run. See its README for the
   rules, the compiler from λ-terms and a map of the files.
2. **`reversible/`: reversible programs in it.** Programs that drop nothing and hand themselves back.
   The best reversible binary adder runs at 29.3 steps per bit, against 67 for the ordinary LTC add and
   183 for TC. See `RESULTS.md`.
3. **`nets/`: reflective linear nets.** Trees blow up because every place an argument goes has to be
   spelled out by routing nodes (B, C, W) on the path to that use; λx1…xn. k xn…x1 compiles to 1,134
   nodes in LTC at n = 16, and 6,533 in TC. As an interaction net, variables are wires, and it's 50
   agents. The calculus here keeps function bodies frozen until called, which makes a single kind of
   copier correct, and lets programs read any function's code, their own included. An interpreter for
   the calculus written in the calculus runs programs, and itself, to the same answers. Programs can
   also be written linearly: a unary and a binary add that erase nothing, in 43 rewrites a successor
   and 287 a bit against 73 and 390 for the ordinary ones, and an add on trees of bits in 300 a bit
   against 851. A parallel add on the same trees, still linear, takes 85 more rounds each time the
   numbers double where the others take twice the rounds: 946 rounds against 110,598 at 1,024 bits,
   for 2.6 times the rewrites. Linear is not reversible: a linear add erases nothing and still gives
   one sum for many pairs of numbers. A reversible add hands the first number back beside the sum, and
   a subtract undoes it, at 316 and 353 rewrites a bit. The adds are named for the kind of program, the
   shape of the numbers and the way they add (`linear_tree_add`, `ordinary_list_add`,
   `reversible_tree_sub_parallel`, …); `5u`, `5` and `5t` write linear numbers, and `5s`, `5c`, `5a`,
   `5p` the other shapes. `scaling.html` draws how the unary and list adds scale beside tree calculus,
   and `adders.html` compares every add: work, rounds and room. See `DESIGN.md`.
4. **`lean/`: theorems that constrain net designs.** The nets' rule table is written as data in Lean, and
   the properties a good design needs are proven by evaluation: well formed, polarized, deterministic,
   covered, code inert, only erasing drops, only copying copies, every value readable, and the copy and
   erase rules forced by polarity. Flawed designs fail them. `Size.lean` proves compiled nets have at most
   2 agents per term node. `lake exe forced` tries every other wiring of every rule against the test
   programs: only the table's own passes. See `PATHWAY.md` for the catalog, including the open theorems
   about reduction.

5. **`player/`: watching the nets reduce.** A page that plays a program's run one rewrite or one round
   at a time, forward and back: the net hanging from its root, a gold wire on each pair about to
   rewrite, each copy in its own colour, each rewrite shown as it happens, and a bar of the whole run
   to scrub. Clicking a gold wire fires that pair, so the order is yours to choose; the rest of the run
   is planned again and keeps its length. Every port is a pin on its own side of the icon, wires join
   pin tips, and an icon turns and flips to face what it is wired to. The code of a definition that
   nothing is acting on is drawn folded, as its name on a pill, and opens when something reaches it.
   Code that hangs together is one structure: a tidy tree on a faint band of its own, kept apart from
   the others and labelled with the definition it comes from or the start of its term. A number is
   tagged with which number it is, wherever it is and however it was built. A pair that can
   fire is a bright wire in the colour of what its rewrite will do. Pointing at an agent names its
   ports, reads the subnet under it as a λ-term (the whole net is read as one too, at every moment),
   and links to a picture of each rule it can take part in; holding Shift lights that subnet up. Run
   the physical way, pairs are drawn together and rewrite when they touch, so what is close goes first.
   `icons.html` says what each kind of agent does and offers four ways to draw it. It is the agent
   view of the strand lattice's player (`evaluators/rust-ic-strands/player/`), rebuilt for this
   calculus; there is no lattice under it. `engine.js` is the rule table in JavaScript, and `check.mjs`
   holds it to the Python nets: the same answers and the same number of rewrites on 671 programs, in
   three orders of steps, with pairs fired by hand, and with pairs fired live and nothing planned. Numbers can be typed
   (`linear_unary_add 40u 2u`, `linear_list_add 1000 2000`, `linear_tree_add 1014t 2413t`), and a value that is a
   number is shown as one. The side panel lists every add, linear, ordinary and reversible, to pick one and play it.

The visualizer for the 9-rule calculus lives in the website (`website/src/lib/evaluators/linear/`, page
`/evaluators/linear/`), not here.

Related: `../LINEAR_TREE_NETS.md` (earlier net work for disp's tree-net), and
`../interaction-combinator/EMBEDDING_THEOREM.md` (the hand-written polarity lemma for the tree-net,
which `lean/` could mechanize).

## Rerunning

Every run is memory-capped. From this folder:

```sh
cap() { systemd-run --user --scope -q -p MemoryMax=3G -p MemorySwapMax=1G timeout 1500 "$@"; }
(cd tree-calculus && cap python final.py)         # static checks, confluence, 3000 random programs, benchmarks
(cd reversible && ./run_all.sh)                   # every reversible measurement; addx6.py alone is the final adder
(cd nets && cap python check.py)                  # 12000 random programs against the reference, 47 hand-written ones (about 15 minutes)
(cd nets && cap python selfinterp.py --quick)     # the interpreter on programs and on itself, on three rule tables
(cd nets && cap python linear.py)                 # the adds: right sums, no eraser in the linear ones, copies counted, the reversible pair undoing each other, and their cost
(cd nets && cap python scaling.py)                # the adds at growing sizes, in the nets and in the tree calculi; fills scaling.html
(cd nets && cap python adders.py)                 # every add compared: work, rounds, room, to 1,024 bits; fills adders.html (about 20 minutes; --quick stops at 64 bits)
(cd nets && cap python blowup.py && cap python costs.py)
(cd player && cap python expected.py && cap node check.mjs)   # the player's engine against the Python nets
(cd lean && PATH=~/.elan/toolchains/leanprover--lean4---v4.34.0/bin:$PATH cap lake build)   # every theorem; no errors means proven
(cd lean && PATH=~/.elan/toolchains/leanprover--lean4---v4.34.0/bin:$PATH cap lake build Long)   # the long interpreter runs (about a minute)
(cd lean && cap python gen_programs.py && PATH=~/.elan/toolchains/leanprover--lean4---v4.34.0/bin:$PATH systemd-run --user --scope -q -p MemoryMax=12G -p MemorySwapMax=1G timeout 1500 lake exe forced)   # every rewiring of every rule
```

Lean is core Lean 4.34 with no Mathlib, as a small Lake project; the toolchain isn't on PATH.

## Open

- Simulation for all programs, as a proof. Proven so far (`lean/Net.lean`, `lean/Runs.lean`,
  `lean/Moves.lean`, `lean/Runner.lean`): polarity, the run invariants, confluence and unique results
  for real runs, which fire pairs and remove forwarders in any order; no order runs longer than a
  finished run; and so one finished run with the right answer makes every order right
  (`every_order_of_one`). Checked per program: every order reads back the reference's answer for every
  program up to size 9. Left: that the call-by-value run of every program ends with the reference's
  value, and that the compiler only emits good nets.
- Copies and erasures are 97% of the nets' work in recursive reflective programs, and each level of
  self-interpretation costs about 80×. Frozen code never changes, so it could be hash-consed, which
  would make copying it O(1). Not built.
- Linear programs in the nets are adds written by hand (`nets/check.py`, held to arithmetic by
  `nets/linear.py`). Not tried: a linear program that reads code, a multiply built from the adder, and
  a check that a program is linear, or reversible, before it runs. The reversible add is reversible in
  its answers only: no rewrite of a net can be undone (see "Reversible adds" in `nets/DESIGN.md`). Half of a linear add's rewrites are still copies of frozen code. Counted in steps,
  tree calculus adds in binary with fewer steps than the nets take rewrites (182 a bit against 287);
  it is ahead only while its trees are shared (`nets/scaling.html`).
- The tree-net's own rule table (`evaluators/tree-net/crate/src/rules.rs`) could go through the same Lean checks.
