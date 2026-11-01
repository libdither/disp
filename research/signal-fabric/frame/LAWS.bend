# The laws a design must keep, and the reference's own: yours to write and
# edit (this is a first draft). PROOF.bend proves them, and `bend PROOF.bend`
# prints ALL PROOFS CHECK once every one holds. The laws marked "small" are
# checked on every case listed, by running them: they say nothing past those.
#
# Locality is not a law here but the frame's type: a design changes the grid
# only through lattice.bend's step, whose rule sees a cell and the four beside
# it (Lat.Nb<C> -> C), and keeps nothing outside its cells.

import Base
import ./twoway.bend as TW
import ./lattice.bend as Lat
import ./loopnet.bend as Design
import ./cases.bend as Cases

# The reference
# -------------

# LAW: the Bend reference answers as core.ts does, on every case cases.mts
# ran core.ts on (library programs and swapnet's, both ways, small trees).
law port_is_core:
  {TW.Case.all_ok(Cases.core()) == True{} : Bool}

# LAW: swap, cross and sink each undo themselves, on every tree.
law swap_undoes:
  for x: TW.Tree
  {TW.swap(TW.swap(x)) == x : TW.Tree}

law cross_undoes:
  for x: TW.Tree
  {TW.cross(TW.cross(x)) == x : TW.Tree}

law sink_undoes:
  for x: TW.Tree
  {TW.sink(TW.sink(x)) == x : TW.Tree}

# LAW (small): a program run backward on what it gave forward gives the input
# back, for every case above run forward.
law two_way_small:
  {TW.Case.all_undo(Cases.core()) == True{} : Bool}

# The design
# ----------

# LAW: no tick turns matter into Void. (Void becomes matter only where a
# chain's leaf grows into it, when a stem is made and its chain has no slack.)
law matter_stays:
  for nb: Lat.Nb<Design.Cell>
  {Bool.or(Design.is_void(Lat.center(Design.Cell, nb)), Bool.not(Design.is_void(Design.rule(nb)))) == True{} : Bool}

# LAW (small): the design gives the reference's answer for every program of
# its kind in cases.bend on every tree there.
law sound_small:
  {Design.agrees_all(Cases.programs(), Cases.trees()) == True{} : Bool}

# LAW (small): the design gives the reference's answer for its loops and the
# programs that make and take away stems, on pairs of numbers and a few other
# trees: add and add† among them, add† stuck where it would go below 0, a
# loop whose bursts go into its left part, so its levels wait on R, and add
# followed by steps into its part, which wait in the fork's queue.
law loops_small:
  {Design.agrees_all(Cases.loops(), Cases.numbers()) == True{} : Bool}

# LAW (small): the design gives the reference's answer for its binary
# programs (not, a tri that flips a bit when another is 1, a fix that a fork
# runs, and binary add both ways) on bits, on every pair of numbers in 1 and
# 2 bits, and on trees with no bit where one should be, where not is stuck.
law binary_small:
  {Design.agrees_all(Cases.binary(), Cases.registers()) == True{} : Bool}

# Not a law yet: that a tick can be undone (the grid before it follows from
# the grid after). The designs so far do not keep it: a packet's X is gone once it has
# acted. A design that does would send each packet back up as its done signal.
