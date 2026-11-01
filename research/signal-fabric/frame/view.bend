# What frame.html calls, built to out/view.mjs by check.sh: the design's grid,
# its tick and still test, what it reads as, and the reference's answer. The
# page only draws what these give back, so it shows the design as Bend runs
# it. A new design keeps these names and imports itself as Design.

import Base
import ./twoway.bend as TW
import ./lattice.bend as Lat
import ./loopnet.bend as Design

# the packets program p sends in, run forward; None outside the design
def packets(+p: TW.Tree) -> +Maybe<+List<+List<Design.Sym>>>:
  Design.program(p)

def symbols(ps: +List<+List<Design.Sym>>) -> +List<Design.Sym>:
  Design.symbols(ps)

# the grid that starts symbols s on tree v
def lay(+v: TW.Tree, +s: +List<Design.Sym>) -> +List<+List<Design.Cell>>:
  Design.lay(v, s, Design.room(s, v))

def tick(+g: +List<+List<Design.Cell>>) -> +List<+List<Design.Cell>>:
  Lat.step(~Design.Cell, ~Design.rule, ~Design.Void{}, g)

def still(g: +List<+List<Design.Cell>>) -> Bool:
  Lat.still_grid(~Design.Cell, ~Design.still, g)

# the tree grid g holds, for a run of symbols s on v
def reading(+g: +List<+List<Design.Cell>>, +v: TW.Tree, +s: +List<Design.Sym>) -> +Maybe<TW.Tree>:
  Design.reading(g, v, s)

# the design's answer for p on v, once its grid is still
def answer(+p: TW.Tree, +v: TW.Tree) -> +Maybe<TW.Tree>:
  Design.result(p, v)

# the reference's answer
def reference(+p: TW.Tree, +v: TW.Tree) -> +Maybe<TW.Tree>:
  TW.run(256n, p, v, True{}, None{})
