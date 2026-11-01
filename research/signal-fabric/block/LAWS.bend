# The laws a block design keeps, by level: the calculus (what programs
# mean), the rule (what a tick may do), the matter (what the grid reads as),
# the control (how a run starts and ends), and meaning (what a run gives).
# PROOF.bend proves them; `bend PROOF.bend` prints ALL PROOFS CHECK once
# every one holds. A law marked "small" holds on the cases it lists, by
# running them, and says nothing past them.
#
# Each law says what it restricts and what it leaves free, so a law can be
# loosened if it turns out to rule out more than it should. Fixed by the
# physics (blocks.bend), not by any law: one rule for every block, 2×2
# blocks whose cut shifts each tick, cells of a few small fields, and an
# empty plane outside the grid.

import Base
import ./calculus.bend as TW
import ./blocks.bend as B
import ./harness.bend as H
import ./design.bend as Design

# The cases
# ---------

# programs of swaps and their order: id, swap, ; and †
def swaps() -> +List<TW.Tree>:
  [TW.L{}, TW.p_swap(), TW.p_seq(TW.p_swap(), TW.p_swap()), TW.p_dag(TW.p_swap())]

# maps: a program sent into the parts of a fork or a stem
def maps() -> +List<TW.Tree>:
  [TW.p_first(TW.p_swap()), TW.p_second(TW.p_swap()), TW.p_map(TW.p_swap(), TW.p_swap(), TW.p_swap()),
   TW.p_seq(TW.p_first(TW.p_swap()), TW.p_swap()), TW.p_seq(TW.p_second(TW.p_first(TW.p_swap())), TW.p_swap()),
   TW.p_map(TW.p_map(TW.p_swap(), TW.L{}, TW.L{}), TW.p_swap(), TW.p_second(TW.p_swap())),
   TW.p_dag(TW.p_seq(TW.p_first(TW.p_swap()), TW.p_swap()))]

# matter made and taken back: stems, and forks with an empty left part
# (unit: x becomes (△, x)), at the top or in a part, and after or before
# other steps; stem† and unit† get stuck where there is nothing to take back
def stems() -> +List<TW.Tree>:
  [TW.p_stem(), TW.p_dag(TW.p_stem()), TW.p_first(TW.p_stem()), TW.p_second(TW.p_stem()),
   TW.p_seq(TW.p_stem(), TW.p_stem()), TW.p_seq(TW.p_second(TW.p_stem()), TW.p_swap()),
   TW.p_map(TW.p_stem(), TW.p_stem(), TW.p_stem()), TW.p_dag(TW.p_second(TW.p_stem()))]

def units() -> +List<TW.Tree>:
  [TW.p_unit(), TW.p_dag(TW.p_unit()), TW.p_first(TW.p_unit()), TW.p_second(TW.p_unit()),
   TW.p_seq(TW.p_unit(), TW.p_swap()), TW.p_seq(TW.p_unit(), TW.p_first(TW.p_stem())),
   TW.p_dag(TW.p_first(TW.p_unit()))]

def programs() -> +List<TW.Tree>:
  List.concat(&2, TW.Tree, [swaps(), maps(), stems(), units()])

# the trees runs are checked on: every tree of at most 4 nodes, and a full
# tree of 4 leaves, one marked by a stem so a swap shows
def run_trees() -> +List<TW.Tree>:
  List.append(&2, TW.Tree, TW.trees(4n), [TW.F{TW.F{TW.S{TW.L{}}, TW.L{}}, TW.F{TW.L{}, TW.L{}}}])

# The calculus
# ------------

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
# back, for the programs above on every tree of at most 5 nodes.
law two_way_small:
  {TW.undoes_all(programs(), TW.trees(5n)) == True{} : Bool}

# The rule
# --------
# These hold for every block: they are checks on the reactions, not on cases.

# LAW: every reaction's cells have the design's fields, its values are below
# their fields' ranges, and a variable moves a value only between fields of
# the same range. Restricts: a cell is one of finitely many (the product of
# the ranges); nothing can hold a list or a counter that grows. Leaves free:
# how many fields and how large (that size is a score, not a bound).
law fits_fields:
  {B.all_fit(Design.reactions(), Design.fields()) == True{} : Bool}

# LAW: each variable of a reaction is on both its sides, once each.
# Restricts: a reaction cannot copy a value it does not spell out, or lose
# one. Leaves free: where values move.
law linear:
  {B.all_linear(Design.reactions()) == True{} : Bool}

# LAW: two reaction sides that fit some block in common turn it into the
# same block. With linear, the rule undoes itself (the argument is at the end
# of blocks.bend): a tick is undone by the same tick, so a run is undone by
# the same physics with the ticks in the other order. Restricts: no
# information is ever lost, and the backward physics is the forward one;
# this rules out rules whose inverse is a different rule (a mover that keeps
# its heading, say, which a time-reversal flip would need). Leaves free: what
# reacts with what, and how many reactions.
law undoes_itself:
  {B.one_partner(Design.reactions()) == True{} : Bool}

# LAW: no reaction side fits four empty cells. Restricts: empty space does
# nothing by itself, so a grid far from any matter stays empty. Leaves free:
# matter acting on the empty cells next to it (growing, moving into them).
law void_stays:
  {B.no_void(Design.reactions(), H.void()) == True{} : Bool}

# The matter
# ----------

# LAW (small): every tree of at most 6 nodes has a layout whose cells are in
# range, that reads back as the tree, and that a tick of either kind leaves
# as it is. Restricts: every tree can be put down, read, and rests with no
# program. Leaves free: the layout, and how the reading finds the tree.
law lays_small:
  {H.lays(TW.trees(6n)) == True{} : Bool}

# LAW (small): on every run below (the programs above on run_trees()), the grid reads as the input tree at the
# start (program and all) and as some tree at every tick. Restricts: the tree is always physically there; the ticks in the middle of
# a move must read as before it or after it. Leaves free: how many ticks a
# move takes.
law always_reads_small:
  {H.check_all(H.Reads{}, programs(), run_trees()) == True{} : Bool}

# LAW (small): on every run below, from one tick to the next the reading
# changes only by the calculus's moves (swap, cross, sink, stem, unit, either
# way), any number at once at different nodes. Restricts: nothing that is
# not a move ever shows; the tree changes by local steps. Leaves free:
# which moves happen, when, and how many in a tick.
law moves_only_small:
  {H.check_all(H.Moves{}, programs(), run_trees()) == True{} : Bool}

# The control
# -----------
# A run starts with the program's cells in a column above the tree (decided
# from the program alone) and ends once a tick of each kind changes nothing
# and nothing leaves the grid. That it ends is part of the laws on meaning
# below, which read the grid only once it has.

# LAW (small): on every run below, things leave the grid only upward from the
# port column, the way the program came in. Restricts: no dumping what a run
# leaves behind out of the sides; it comes back out where the program went
# in. Leaves free: what comes back out.
law sealed_small:
  {H.check_all(H.Sealed{}, programs(), run_trees()) == True{} : Bool}

# Meaning
# -------

# LAW (small): the design runs every program in swaps() (its cells in range),
# and on every tree in run_trees() the calculus does not get stuck on, the run
# goes still within its ticks and then reads as the calculus's answer.
# Restricts: these programs work, and once a program is through nothing keeps
# moving inside the grid: in a reversible system a grid comes to rest from
# motion only if what moved leaves, so every signal must leave (or never have
# moved). Leaves free: how a program is written into cells (the column may be
# any length), and how long a run takes (both scores). Stuck runs are held
# only to the matter and control laws above.
law swaps_small:
  {Bool.and(Bool.and(H.supports(swaps()), H.tapes_in(swaps())), H.check_all(H.Agrees{}, swaps(), run_trees())) == True{} : Bool}

# LAW (small): the same for maps(), programs sent into a fork's or a stem's
# parts.
law maps_small:
  {Bool.and(Bool.and(H.supports(maps()), H.tapes_in(maps())), H.check_all(H.Agrees{}, maps(), run_trees())) == True{} : Bool}

# LAW (small): the same for stems(): stems made and taken back. Restricts: a
# tree can grow by a node and shrink back. Leaves free: where the new cells
# come from (empty cells next to matter, or slack kept in the layout).
law stems_small:
  {Bool.and(Bool.and(H.supports(stems()), H.tapes_in(stems())), H.check_all(H.Agrees{}, stems(), run_trees())) == True{} : Bool}

# LAW (small): the same for units(): a fork with an empty left part made
# above a part, and taken back. Restricts: a tree can grow sideways. Leaves
# free: where the fork and its leaf go.
law units_small:
  {Bool.and(Bool.and(H.supports(units()), H.tapes_in(units())), H.check_all(H.Agrees{}, units(), run_trees())) == True{} : Bool}

# Held back
# ---------
# Not laws yet, each with what it would restrict:
# - Next programs: cross and sink, tri, fix. Each gets a law like maps_small
#   once the design reaches it.
# - Stuck runs: a reversible design cannot jam the way loopnet's did. One
#   choice: the program comes back out unused and the tree is unchanged.
#   Would restrict how a design refuses.
# - The echo is the program: what leaves is the column that went in. Would
#   restrict a run to leave nothing behind but its answer (Bennett's clean
#   computation); leaves free the timing.
# - Rotations and reflections: turning or mirroring a block commutes with the
#   rule (with a cell's directions turned too). Would restrict the physics to
#   no favoured direction; costs fields for directions.
# - Matter is counted: each reaction keeps the number of cells that are not
#   empty. Would restrict stem and unit to take matter from somewhere.
# - Speed: ticks per run below some bound in the tree's size. Would restrict
#   layouts and signalling to be efficient; worth stating once a design works.
