# research/reversible-core

Working files of the review in research/REVERSIBLE_CORE.md (archived: no design adopted). Scratch
Python models of the abstract tree-calculus net, with no lattice and their own round scheduler, so
only their ratios carry over. Run them with `python` from their own folder.

- `residual.py`: keeping history, the design the lattice now runs (`resid=1`, rust-ic-strands README
  "Keeping history"). An apply leaves what it would have dropped as a residual tree, and un-apply
  takes the residual and the result back to the function and the argument. It has the oracle, the
  forward and backward rule tables sized for the lattice, an interpreter (eager, and lazy with the
  undo-by-meeting moves), and the checks: forward against the oracle, back to the very agents
  loaded, both in one net, history erased. `Run` drives it as the player does: the term as a tree at
  a root, a click turning the root into the walker that runs it (`E`) or runs it back (`E⁻`); and
  forgetting what the applies drop, which brings the term back with holes that any trees fill
  without changing the answer. `python residual.py [--quick]` runs the checks (`--quick`: 24 s);
  `--rust` writes the lattice's tables (rules.rs `RESID_RULES`, `FORGET_RULES`).
- `rule_audit.py`: which rules of a table could run backward with nothing beyond the net, by
  Salikhmetov's test (connected right sides, no two alike); `residual.py` imports it for its tables.

- `footprints/`: the footprint net (a rewrite fuses its pair into a footprint that what it made is
  wired back to; undo is a product meeting its footprint) beside today's 26-rule table under the same
  scheduler. `python run.py 'add 2 3'`; `calls.txt` lists the 21 compiled disp calls (from
  research/linear-nets/calls.txt). `randtest.py` runs random terms there and back.
- `critic/`: the reader (copies a value without consuming it, so Bennett's cleanup runs inside the net)
  and the critics' probes: wire ends per footprint, clashes between collectors and duplicators, local
  garbage collection by running back, flicker of a finished answer. `python run_bennett.py
  ../footprints/calls.txt` runs every call forward, reads the answer and runs back to zero footprints.
- `riders/`: the Riders model (code read in place by pointer agents, never copied) and its runs; the
  critic showed it ties every caller's history together.
- `review/`: the review's raw outputs. `sources.json` is every source the literature sweep kept, by
  field, with its fetch-verified link and quote and what the sweep took from it; `scouts.json` the
  repo's earlier and current designs as the scouts measured them; `designs.json`, `judgements.json`,
  `scores.json` and `critiques.json` the five designs, the five judges, the scores, and the critiques
  of the two leaders.
