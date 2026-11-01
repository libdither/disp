# A second design inside the frame: swapnet, plus stems made and taken away,
# loops, recursion copied at forks or run by a fork, bits flipped, and chains
# that pack themselves up.
#
# Making a stem spends wire: a plain wire cell that takes in Sp at the end of
# its packet's path becomes a stem node, and Sm turns a stem back into wire.
# Where Sp finds no plain wire before the leaf a chain ends in, the leaf
# grows on into the empty cell past it (the chain gets longer at its foot,
# which is the same as a stem anywhere in it, stems being alike); where it
# finds a fork, or Sm finds no stem, the node jams. No tick turns matter into
# Void (matter_stays), and Void becomes matter only where a leaf grows.
#
# Chains pack themselves: a still plain wire just above a still free stem
# trades places with it, so the stem floats up and the slack sinks to the
# chain's foot. This never changes what the grid reads as.
#
# A loop is fix(tri(a, j ; self ; k, id)), or tri(a, b, id) with no self. The
# node it is sent to (the loop's node) sends a walker down its left wire. The
# walker holds each stem it meets, as tri lowers it, and a held stem passes
# everything through, so the recursion's stack is the left part itself.
# Wires carry a second lane, going up. Each burst a level owes (j on the way
# down, a at the bottom, k on the way back) goes up that lane to the loop's
# node, which runs it as packets. If a burst goes into the left part, the
# node answers it with R down the left wire, so a level moves on only once
# its burst has gone past it; if none does (a fast loop, add among them),
# nothing waits, and a burst may start a loop of its own further down. a, j
# and k may not act on the loop's node itself, and c must be id.
#
# A fork runs what reaches it in order: while a loop of its own is out (from
# Tri until the E that ends it comes back up), what comes from above waits in
# its queue, so whatever follows a loop finds it done.
#
# fix(map(x, y, z)), with each of x, y, z self or id and maybe a swap before
# or after, goes in as one symbol, Each, that a fork copies down both wires
# at once: every node does its swap as Each passes it. Any other fix(b) whose
# packets all go into a fork's halves (or are a loop or a swap there) goes in
# as Fix, carrying b's packets: the fork it reaches puts them first in its
# queue, each self (Me) among them being the Fix again, sent on down.
#
# not, on a bit (0 is △, 1 is △△), goes in as Flip: the wire just above a
# leaf becomes its stem, and a stem becomes wire, sending Chk down to see
# that a leaf was under it (on anything but a bit not is stuck: a jam).

import Base
import ./twoway.bend as TW
import ./lattice.bend as Lat

type Dir is Data:
  Up{}
  Rt{}
  Dn{}
  Lf{}

# when an Each swaps a fork: not at all, before it copies itself down, after
type Turn is Data:
  NoTurn{}
  TurnFirst{}
  TurnLast{}

# Q nothing; X swap; Sp, Sm make or take away a stem; M0, M1, M2 a step down
# a path; Tri starts a loop and Walk walks down its left part; N ends a burst
# sent up, R answers it, H says the stem below is held, E that it let go (or,
# in a fast loop, that the levels below are done); Each a copied recursion;
# Flip a bit's not, and Chk that a stem it took off stood on a leaf; Fix a
# recursion a fork runs, Me its self
type Sym is Data:
  Q{}
  X{}
  Sp{}
  Sm{}
  M0{}
  M1{}
  M2{}
  Tri{a: +List<Sym>, j: +List<Sym>, k: +List<Sym>, rec: Bool, fast: Bool}
  Walk{a: +List<Sym>, j: +List<Sym>, k: +List<Sym>, rec: Bool, fast: Bool}
  N{}
  R{}
  H{}
  E{}
  Each{k0: Bool, k1: Bool, k2: Bool, turn: Turn}
  Flip{}
  Chk{}
  Fix{body: +List<Sym>}
  Me{}

type Latch is Data:
  Idle{}
  Pass{to: Dir}
  Drop{}

# where a held stem is in its loop: waiting for R to send the walker w on
# (Down) or to let go (Once, a tri with no self; Sent, once k has gone up),
# waiting for the levels below (Waits), or, in a fast loop, for their E
type Phase is Data:
  Down{w: Sym}
  Once{}
  Waits{}
  Sent{}
  Fast{}

type Hold is Data:
  Free{}
  Held{k: +List<Sym>, phase: Phase, below: Bool}

# matter: a wire, or a node with its parts' wires, its latch, the symbol it
# sends toward `to` (a fork sends it down both its wires when wide), the
# symbol it sends up and those still to go up; a leaf growing toward `to`.
# A fork also keeps the symbols waiting their turn (todo) and whether a loop
# of its own is out (busy)
type Part is Data:
  Wire{to: Dir, sym: Sym, up: Sym}
  Leaf{latch: Latch, up: Sym, out: +List<Sym>}
  Stem{kid: Dir, latch: Latch, sym: Sym, to: Dir, hold: Hold, up: Sym, out: +List<Sym>}
  Fork{l: Dir, r: Dir, latch: Latch, sym: Sym, to: Dir, wide: Bool, up: Sym, out: +List<Sym>, todo: +List<Sym>, busy: Bool}
  Grow{to: Dir}
  Jam{}

type Cell is Data:
  Void{}
  Thing{p: Part}

# The rule
# --------

def Dir.is_eq(a: Dir, b: Dir) -> Bool:
  match a b:
    case Up{} Up{}:
      True{}
    case Rt{} Rt{}:
      True{}
    case Dn{} Dn{}:
      True{}
    case Lf{} Lf{}:
      True{}
    case _ _:
      False{}

def if_sym(b: Bool, s: Sym) -> Sym:
  match b:
    case True{}:
      s
    case False{}:
      Q{}

# what cell c sends toward d this tick
def sends(c: Cell, +d: Dir) -> Sym:
  match c:
    case Thing{Wire{to, sym, up}}:
      if_sym(Dir.is_eq(to, d), sym)
    case Thing{Stem{kid, latch, sym, to, hold, up, out}}:
      if_sym(Dir.is_eq(to, d), sym)
    case Thing{Fork{l, r, latch, sym, to, wide, up, out, todo, busy}}:
      if_sym(Bool.or(Dir.is_eq(to, d), Bool.and(wide, Bool.or(Dir.is_eq(l, d), Dir.is_eq(r, d)))), sym)
    case _:
      Q{}

def or_sym(a: Sym, b: Sym) -> Sym:
  match a:
    case Q{}:
      b
    case s:
      s

# what reaches a cell going down: whatever a cell beside it sends its way
def incoming(n: Cell, e: Cell, s: Cell, w: Cell) -> Sym:
  or_sym(sends(n, Dn{}), or_sym(sends(e, Lf{}), or_sym(sends(s, Up{}), sends(w, Rt{}))))

# what a cell sends up, to the cell whose wire leads to it
def ups(c: Cell) -> Sym:
  match c:
    case Thing{Wire{to, sym, up}}:
      up
    case Thing{Leaf{latch, up, out}}:
      up
    case Thing{Stem{kid, latch, sym, to, hold, up, out}}:
      up
    case Thing{Fork{l, r, latch, sym, to, wide, up, out, todo, busy}}:
      up
    case _:
      Q{}

# c's wire (or one of them) leads toward d
def points(c: Cell, +d: Dir) -> Bool:
  match c:
    case Thing{Wire{to, sym, up}}:
      Dir.is_eq(to, d)
    case Thing{Stem{kid, latch, sym, to, hold, up, out}}:
      Dir.is_eq(kid, d)
    case Thing{Fork{l, r, latch, sym, to, wide, up, out, todo, busy}}:
      Bool.or(Dir.is_eq(l, d), Dir.is_eq(r, d))
    case Thing{Grow{to}}:
      Dir.is_eq(to, d)
    case _:
      False{}

def is_void(c: Cell) -> Bool:
  match c:
    case Void{}:
      True{}
    case Thing{p}:
      False{}

def growing(c: Cell, +d: Dir) -> Bool:
  match c:
    case Thing{Grow{to}}:
      Dir.is_eq(to, d)
    case _:
      False{}

def toward(n: Cell, e: Cell, s: Cell, w: Cell, d: Dir) -> Cell:
  match d:
    case Up{}:
      n
    case Rt{}:
      e
    case Dn{}:
      s
    case Lf{}:
      w

def is_q(s: Sym) -> Bool:
  match s:
    case Q{}:
      True{}
    case _:
      False{}

def is_path(s: Sym) -> Bool:
  match s:
    case M0{}:
      True{}
    case M1{}:
      True{}
    case M2{}:
      True{}
    case _:
      False{}

# a latch passing a packet on, or eating it, until the packet's action
def until_act(l: Latch, s: Sym) -> Latch:
  match s:
    case X{}:
      Idle{}
    case Sp{}:
      Idle{}
    case Sm{}:
      Idle{}
    case Tri{a, j, k, rec, fast}:
      Idle{}
    case Each{k0, k1, k2, turn}:
      Idle{}
    case Flip{}:
      Idle{}
    case Chk{}:
      Idle{}
    case Fix{body}:
      Idle{}
    case _:
      l

def up_of(u: Sym) -> +List<Sym>:
  match u:
    case Q{}:
      Nil{}
    case v:
      [v]

def cat(a: +List<Sym>, b: +List<Sym>) -> +List<Sym>:
  List.append(&2, Sym, a, b)

def end_of(fast: Bool) -> Sym:
  match fast:
    case True{}:
      E{}
    case False{}:
      N{}

# a node's up lane: the symbol it sends up now, and the rest still to go
type Out is Data:
  Out{up: Sym, rest: +List<Sym>}

def pop(xs: +List<Sym>) -> Out:
  match xs:
    case Nil{}:
      Out{Q{}, Nil{}}
    case h <> t:
      Out{h, t}

def leaf_of(lt: Latch, o: Out) -> Part:
  match o:
    case Out{u, rest}:
      Leaf{lt, u, rest}

def grow_or_jam(m: +Maybe<Dir>) -> Part:
  match m:
    case Some{d}:
      Grow{d}
    case None{}:
      Jam{}

# a leaf: Sp (or a Flip the wire above did not take) finds no slack above
# it, so the leaf grows on past itself if there is room (else jams); Sm
# finds no stem and jams it; a walker finds the bottom of its loop's left
# part and sends a up, R the loop's end
def leaf(lt: Latch, +out: +List<Sym>, i: Sym, way: +Maybe<Dir>) -> Part:
  match lt:
    case Idle{}:
      match i:
        case Sp{}:
          grow_or_jam(way)
        case Flip{}:
          grow_or_jam(way)
        case Sm{}:
          Jam{}
        case M0{}:
          leaf_of(Drop{}, pop(out))
        case M1{}:
          leaf_of(Drop{}, pop(out))
        case M2{}:
          leaf_of(Drop{}, pop(out))
        case Walk{a, j, k, rec, fast}:
          leaf_of(Idle{}, pop(cat(out, cat(a, [end_of(fast)]))))
        case R{}:
          leaf_of(Idle{}, pop(cat(out, [E{}])))
        case _:
          leaf_of(Idle{}, pop(out))
    case Drop{}:
      leaf_of(until_act(Drop{}, i), pop(out))
    case Pass{d}:
      leaf_of(Pass{d}, pop(out))

def way_if(b: Bool, d: Dir, rest: +Maybe<Dir>) -> +Maybe<Dir>:
  match b:
    case True{}:
      Some{d}
    case False{}:
      rest

# the way a leaf grows: on from the wire that leads to it, if that cell is empty
def grow_way(+n: Cell, +e: Cell, +s: Cell, +w: Cell) -> +Maybe<Dir>:
  way_if(Bool.and(points(n, Dn{}), is_void(s)), Dn{}, way_if(Bool.and(points(s, Up{}), is_void(n)), Up{}, way_if(Bool.and(points(e, Lf{}), is_void(w)), Lf{}, way_if(Bool.and(points(w, Rt{}), is_void(e)), Rt{}, None{}))))

def stem_of(kid: Dir, lt: Latch, sym: Sym, to: Dir, hold: Hold, o: Out) -> Part:
  match o:
    case Out{u, rest}:
      Stem{kid, lt, sym, to, hold, u, rest}

def wire_of(to: Dir, o: Out) -> Part:
  match o:
    case Out{u, rest}:
      Wire{to, Q{}, u}

# a stem a Flip takes off: wire now, sending Chk on to make sure a leaf is next
def checked(to: Dir, o: Out) -> Part:
  match o:
    case Out{u, rest}:
      Wire{to, Chk{}, u}

def start(+a: +List<Sym>, +j: +List<Sym>, +k: +List<Sym>, rec: Bool) -> Phase:
  match rec:
    case True{}:
      Down{Walk{a, j, k, True{}, False{}}}
    case False{}:
      Once{}

# a walker reaching a free stem: in a fast loop the stem sends j up and the
# walker on at once, keeping k for the way back (or, with no self, lets go
# with an E); else it holds itself and sends H and j up, then waits for R
def walked(+kid: Dir, +out: +List<Sym>, +a: +List<Sym>, +j: +List<Sym>, +k: +List<Sym>, rec: Bool, fast: Bool) -> Part:
  match rec fast:
    case True{} True{}:
      stem_of(kid, Idle{}, Walk{a, j, k, True{}, True{}}, kid, Held{k, Fast{}, False{}}, pop(cat(out, j)))
    case False{} True{}:
      stem_of(kid, Idle{}, Q{}, kid, Free{}, pop(cat(out, cat(j, [E{}]))))
    case r False{}:
      stem_of(kid, Idle{}, Q{}, kid, Held{k, start(a, j, k, r), False{}}, pop(cat(out, H{} <> cat(j, [N{}]))))

# a waiting free stem: Sp goes on below it (a stem below a stem is a stem
# above it), Sm turns it back into wire, and so does Flip (1 is △△, so the
# stem must stand on a leaf: Chk goes down to see), a walker holds it, Each
# goes on down; Chk finds a stem where a leaf should be and jams it
def free_idle(+kid: Dir, +to: Dir, +out: +List<Sym>, i: Sym) -> Part:
  match i:
    case Flip{}:
      checked(kid, pop(out))
    case Chk{}:
      Jam{}
    case M0{}:
      stem_of(kid, Pass{kid}, Q{}, to, Free{}, pop(out))
    case M1{}:
      stem_of(kid, Drop{}, Q{}, to, Free{}, pop(out))
    case M2{}:
      stem_of(kid, Drop{}, Q{}, to, Free{}, pop(out))
    case Sp{}:
      stem_of(kid, Idle{}, Sp{}, kid, Free{}, pop(out))
    case Sm{}:
      wire_of(kid, pop(out))
    case R{}:
      stem_of(kid, Idle{}, R{}, kid, Free{}, pop(out))
    case Walk{a, j, k, rec, fast}:
      walked(kid, out, a, j, k, rec, fast)
    case Each{True{}, k1, k2, turn}:
      stem_of(kid, Idle{}, Each{True{}, k1, k2, turn}, kid, Free{}, pop(out))
    case _:
      stem_of(kid, Idle{}, Q{}, to, Free{}, pop(out))

def free(+kid: Dir, lt: Latch, +to: Dir, +out: +List<Sym>, +i: Sym) -> Part:
  match lt:
    case Idle{}:
      free_idle(kid, to, out, i)
    case Pass{+d}:
      stem_of(kid, until_act(Pass{d}, i), i, d, Free{}, pop(out))
    case Drop{}:
      stem_of(kid, until_act(Drop{}, i), Q{}, to, Free{}, pop(out))

# the lowest held stem answers R: it sends the walker on, or k up, or lets go
def on_r(+kid: Dir, +k: +List<Sym>, phase: Phase, +out: +List<Sym>) -> Part:
  match phase:
    case Down{w}:
      stem_of(kid, Idle{}, w, kid, Held{k, Waits{}, False{}}, pop(out))
    case Once{}:
      stem_of(kid, Idle{}, Q{}, kid, Free{}, pop(cat(out, [E{}])))
    case Waits{}:
      stem_of(kid, Idle{}, Q{}, kid, Held{k, Sent{}, False{}}, pop(cat(out, cat(k, [N{}]))))
    case Sent{}:
      stem_of(kid, Idle{}, Q{}, kid, Free{}, pop(cat(out, [E{}])))
    case Fast{}:
      stem_of(kid, Idle{}, Q{}, kid, Held{k, Fast{}, False{}}, pop(out))

def held_go(+kid: Dir, +k: +List<Sym>, +phase: Phase, +out: +List<Sym>, below: Bool, i: Sym) -> Part:
  match below i:
    case False{} R{}:
      on_r(kid, k, phase, out)
    case b j:
      stem_of(kid, Idle{}, j, kid, Held{k, phase, b}, pop(out))

# E from below: if the walker went on from here, this level's k goes up now;
# in a fast loop the stem lets go at once, and its E follows its k
def held_e(+kid: Dir, +k: +List<Sym>, phase: Phase, +out: +List<Sym>, +i: Sym) -> Part:
  match phase:
    case Waits{}:
      held_go(kid, k, Sent{}, cat(out, cat(k, [N{}])), False{}, i)
    case Fast{}:
      stem_of(kid, Idle{}, i, kid, Free{}, pop(cat(out, cat(k, [E{}]))))
    case p:
      held_go(kid, k, p, out, False{}, i)

# a held stem passes everything through, but R, which the lowest held stem
# answers, and H and E from the held stem below
def held(+kid: Dir, +k: +List<Sym>, phase: Phase, below: Bool, +out: +List<Sym>, +i: Sym, u: Sym) -> Part:
  match u:
    case H{}:
      held_go(kid, k, phase, out, True{}, i)
    case E{}:
      held_e(kid, k, phase, out, i)
    case v:
      held_go(kid, k, phase, cat(out, up_of(v)), below, i)

def stem_go(+kid: Dir, lt: Latch, +to: Dir, hold: Hold, +out: +List<Sym>, +i: Sym, +u: Sym) -> Part:
  match hold:
    case Free{}:
      free(kid, lt, to, cat(out, up_of(u)), i)
    case Held{k, phase, below}:
      held(kid, k, phase, below, out, i, u)

# a still free stem under a still plain wire sinks the wire: it becomes the
# wire, and the wire becomes it
def stem(sink: Bool, +kid: Dir, lt: Latch, +to: Dir, hold: Hold, +out: +List<Sym>, +i: Sym, +u: Sym) -> Part:
  match sink:
    case True{}:
      Wire{kid, Q{}, u}
    case False{}:
      stem_go(kid, lt, to, hold, out, i, u)

def stem_still(lt: Latch, sym: Sym, hold: Hold, up: Sym, out: +List<Sym>) -> Bool:
  match lt sym hold up out:
    case Idle{} Q{} Free{} Q{} Nil{}:
      True{}
    case _ _ _ _ _:
      False{}

def free_still(c: Cell) -> Bool:
  match c:
    case Thing{Stem{kid, Idle{}, Q{}, to, Free{}, Q{}, Nil{}}}:
      True{}
    case _:
      False{}

def plain_to(c: Cell, +d: Dir) -> Bool:
  match c:
    case Thing{Wire{to, Q{}, Q{}}}:
      Dir.is_eq(to, d)
    case _:
      False{}

# the cell whose wire leads here is a still plain wire
def under_plain(+n: Cell, +e: Cell, +s: Cell, +w: Cell) -> Bool:
  Bool.or(Bool.or(plain_to(n, Dn{}), plain_to(e, Lf{})), Bool.or(plain_to(s, Up{}), plain_to(w, Rt{})))

# a plain wire passes its symbols on; one that takes in Sp at the end of its
# packet's path (the symbol before it was no step down a path) becomes a stem
def becomes(mid: Bool, +to: Dir, +u: Sym) -> Part:
  match mid:
    case True{}:
      Wire{to, Sp{}, u}
    case False{}:
      Stem{to, Idle{}, Q{}, to, Free{}, u, Nil{}}

def is_leaf(c: Cell) -> Bool:
  match c:
    case Thing{Leaf{latch, up, out}}:
      True{}
    case _:
      False{}

# a plain wire passing a Flip whose packet ends at the leaf just past it
# becomes that bit's stem (0, △, turns 1, △△)
def flipped(pass: Bool, +to: Dir, +u: Sym) -> Part:
  match pass:
    case True{}:
      Wire{to, Flip{}, u}
    case False{}:
      Stem{to, Idle{}, Q{}, to, Free{}, u, Nil{}}

def wire_on(+to: Dir, prev: Sym, i: Sym, +below: Cell) -> Part:
  match i:
    case Sp{}:
      becomes(is_path(prev), to, ups(below))
    case Flip{}:
      flipped(Bool.or(is_path(prev), Bool.not(is_leaf(below))), to, ups(below))
    case j:
      Wire{to, j, ups(below)}

# a still plain wire over a still free stem becomes that stem (and the stem
# the wire): whatever reaches it now, the stem would have met next tick
def sinks(b: Bool, +to: Dir, prev: Sym, +i: Sym, +below: Cell) -> Part:
  match b:
    case True{}:
      free_idle(to, to, Nil{}, i)
    case False{}:
      wire_on(to, prev, i, below)

def wire(+to: Dir, +prev: Sym, up: Sym, +i: Sym, +below: Cell) -> Part:
  sinks(Bool.and(Bool.and(is_q(prev), is_q(up)), free_still(below)), to, prev, i, below)

def fork_of(l: Dir, r: Dir, lt: Latch, sym: Sym, to: Dir, wide: Bool, o: Out) -> Part:
  match o:
    case Out{u, rest}:
      Fork{l, r, lt, sym, to, wide, u, rest, Nil{}, False{}}

# Each at a fork: it goes on down the parts it recurs into (both at once, if
# both), and the fork swaps before or after, as the program says
def each_at(turn: Turn, k1: Bool, k2: Bool, +l: Dir, +r: Dir, +to: Dir, +out: +List<Sym>, +s: Sym) -> Part:
  match turn k1 k2:
    case TurnFirst{} True{} True{}:
      fork_of(r, l, Idle{}, s, l, True{}, pop(out))
    case TurnFirst{} True{} False{}:
      fork_of(r, l, Idle{}, s, r, False{}, pop(out))
    case TurnFirst{} False{} True{}:
      fork_of(r, l, Idle{}, s, l, False{}, pop(out))
    case TurnFirst{} False{} False{}:
      fork_of(r, l, Idle{}, Q{}, to, False{}, pop(out))
    case TurnLast{} True{} True{}:
      fork_of(r, l, Idle{}, s, l, True{}, pop(out))
    case TurnLast{} True{} False{}:
      fork_of(r, l, Idle{}, s, l, False{}, pop(out))
    case TurnLast{} False{} True{}:
      fork_of(r, l, Idle{}, s, r, False{}, pop(out))
    case TurnLast{} False{} False{}:
      fork_of(r, l, Idle{}, Q{}, to, False{}, pop(out))
    case NoTurn{} True{} True{}:
      fork_of(l, r, Idle{}, s, l, True{}, pop(out))
    case NoTurn{} True{} False{}:
      fork_of(l, r, Idle{}, s, l, False{}, pop(out))
    case NoTurn{} False{} True{}:
      fork_of(l, r, Idle{}, s, r, False{}, pop(out))
    case NoTurn{} False{} False{}:
      fork_of(l, r, Idle{}, Q{}, to, False{}, pop(out))

# a waiting fork: a walker or R that reaches it ends the walk (c is id); as a
# loop's node it sends the walker down its left wire, runs each burst that
# comes up as packets, and answers the burst's N with R
def fork_idle(+l: Dir, +r: Dir, +to: Dir, +out: +List<Sym>, i: Sym) -> Part:
  match i:
    case M1{}:
      fork_of(l, r, Pass{l}, Q{}, to, False{}, pop(out))
    case M2{}:
      fork_of(l, r, Pass{r}, Q{}, to, False{}, pop(out))
    case M0{}:
      fork_of(l, r, Drop{}, Q{}, to, False{}, pop(out))
    case X{}:
      fork_of(r, l, Idle{}, Q{}, to, False{}, pop(out))
    case Sp{}:
      Jam{}
    case Sm{}:
      Jam{}
    case Flip{}:
      Jam{}
    case Chk{}:
      Jam{}
    case Tri{a, j, k, rec, fast}:
      fork_of(l, r, Idle{}, Walk{a, j, k, rec, fast}, l, False{}, pop(out))
    case Walk{a, j, k, rec, fast}:
      fork_of(l, r, Idle{}, Q{}, to, False{}, pop(cat(out, [end_of(fast)])))
    case R{}:
      fork_of(l, r, Idle{}, Q{}, to, False{}, pop(cat(out, [E{}])))
    case N{}:
      fork_of(l, r, Idle{}, R{}, l, False{}, pop(out))
    case Each{k0, +k1, +k2, +turn}:
      each_at(turn, k1, k2, l, r, to, out, Each{k0, k1, k2, turn})
    case _:
      fork_of(l, r, Idle{}, Q{}, to, False{}, pop(out))

def fork(+l: Dir, +r: Dir, lt: Latch, +to: Dir, +out: +List<Sym>, +i: Sym) -> Part:
  match lt:
    case Idle{}:
      fork_idle(l, r, to, out, i)
    case Pass{+d}:
      fork_of(l, r, until_act(Pass{d}, i), i, d, False{}, pop(out))
    case Drop{}:
      fork_of(l, r, until_act(Drop{}, i), Q{}, to, False{}, pop(out))

def with_work(p: Part, todo: +List<Sym>, busy: Bool) -> Part:
  match p:
    case Fork{l, r, lt, sym, to, wide, up, out, t, b}:
      Fork{l, r, lt, sym, to, wide, up, out, todo, busy}
    case q:
      q

def is_e(s: Sym) -> Bool:
  match s:
    case E{}:
      True{}
    case _:
      False{}

def starts_loop(lt: Latch, s: Sym) -> Bool:
  match lt s:
    case Idle{} Tri{a, j, k, rec, fast}:
      True{}
    case _ _:
      False{}

# Me in a recursion's body is the recursion again
def expand(xs: +List<Sym>, +f: Sym) -> +List<Sym>:
  match xs:
    case Nil{}:
      Nil{}
    case Me{} <> t:
      f <> expand(t, f)
    case h <> t:
      h <> expand(t, f)

# a Fix that acts here puts its body, self and all, first in the queue
def queued(lt: Latch, s: Sym, rest: +List<Sym>) -> +List<Sym>:
  match lt s:
    case Idle{} Fix{+body}:
      cat(expand(body, Fix{body}), rest)
    case _ _:
      rest

def fork_pop(+l: Dir, +r: Dir, +lt: Latch, +to: Dir, +out: +List<Sym>, o: Out) -> Part:
  match o:
    case Out{+h, rest}:
      with_work(fork(l, r, lt, to, out, h), queued(lt, h, rest), starts_loop(lt, h))

def fork_next(+l: Dir, +r: Dir, lt: Latch, +to: Dir, +out: +List<Sym>, +q: +List<Sym>, busy: Bool) -> Part:
  match busy:
    case True{}:
      with_work(fork(l, r, lt, to, out, Q{}), q, True{})
    case False{}:
      fork_pop(l, r, lt, to, out, pop(q))

# a fork takes what comes up its wires first (its loop's bursts and answers;
# E ends the loop); what comes from above joins the end of its queue, and
# the queue moves on a symbol a tick while no loop of its own is out
def fork_tick(+l: Dir, +r: Dir, lt: Latch, +to: Dir, +out: +List<Sym>, +todo: +List<Sym>, busy: Bool, +i: Sym, u: Sym) -> Part:
  match u:
    case Q{}:
      fork_next(l, r, lt, to, out, cat(todo, up_of(i)), busy)
    case +v:
      with_work(fork(l, r, lt, to, out, v), cat(todo, up_of(i)), Bool.and(busy, Bool.not(is_e(v))))

# a part's next state from what reaches it from above (i) and the cells
# beside it: what its wires' cells send up, and, for a wire, a stem or a
# leaf, whether it trades places with or grows into one of them
def step(p: Part, +i: Sym, +n: Cell, +e: Cell, +s: Cell, +w: Cell) -> Part:
  match p:
    case Wire{+to, sym, up}:
      wire(to, sym, up, i, toward(n, e, s, w, to))
    case Leaf{lt, up, out}:
      leaf(lt, out, i, grow_way(n, e, s, w))
    case Stem{+kid, +lt, +sym, to, +hold, +up, +out}:
      stem(Bool.and(stem_still(lt, sym, hold, up, out), under_plain(n, e, s, w)), kid, lt, to, hold, out, i, ups(toward(n, e, s, w, kid)))
    case Fork{+l, +r, lt, sym, to, wide, up, out, todo, busy}:
      fork_tick(l, r, lt, to, out, todo, busy, i, or_sym(ups(toward(n, e, s, w, l)), ups(toward(n, e, s, w, r))))
    case Grow{+to}:
      Stem{to, Idle{}, Q{}, to, Free{}, Q{}, Nil{}}
    case Jam{}:
      Jam{}

# Void becomes a leaf where a leaf grows into it, and stays Void otherwise
def grown(b: Bool) -> Cell:
  match b:
    case True{}:
      Thing{Leaf{Idle{}, Q{}, Nil{}}}
    case False{}:
      Void{}

def void_next(n: Cell, e: Cell, s: Cell, w: Cell) -> Cell:
  grown(Bool.or(Bool.or(growing(n, Dn{}), growing(e, Lf{})), Bool.or(growing(s, Up{}), growing(w, Rt{}))))

# matter stays matter, whatever reaches it
def next(c: Cell, +i: Sym, +n: Cell, +e: Cell, +s: Cell, +w: Cell) -> Cell:
  match c:
    case Void{}:
      void_next(n, e, s, w)
    case Thing{p}:
      Thing{step(p, i, n, e, s, w)}

def rule(nb: Lat.Nb<Cell>) -> Cell:
  match nb:
    case Lat.Nb{c, +n, +e, +s, +w}:
      next(c, incoming(n, e, s, w), n, e, s, w)

# a cell with nothing to do: no symbol in it either way, nothing left to send
# up, no packet half passed on, no loop holding it (a plain wire may still
# sink past a stem below it: that changes nothing the grid reads as)
def still(c: Cell) -> Bool:
  match c:
    case Thing{Wire{to, Q{}, Q{}}}:
      True{}
    case Thing{Leaf{Idle{}, Q{}, Nil{}}}:
      True{}
    case Thing{Stem{kid, Idle{}, Q{}, to, Free{}, Q{}, Nil{}}}:
      True{}
    case Thing{Fork{l, r, Idle{}, Q{}, to, wide, Q{}, Nil{}, Nil{}, False{}}}:
      True{}
    case Thing{Jam{}}:
      True{}
    case Void{}:
      True{}
    case _:
      False{}

# Compiling: a program to its packets
# -----------------------------------

def then(a: +Maybe<+List<+List<Sym>>>, b: +Maybe<+List<+List<Sym>>>) -> +Maybe<+List<+List<Sym>>>:
  match a b:
    case Some{x} Some{y}:
      Some{List.append(&2, +List<Sym>, x, y)}
    case _ _:
      None{}

def prefix(+m: Sym, ps: +List<+List<Sym>>) -> +List<+List<Sym>>:
  match ps:
    case Nil{}:
      Nil{}
    case h <> t:
      (m <> h) <> prefix(m, t)

def under(+m: Sym, a: +Maybe<+List<+List<Sym>>>) -> +Maybe<+List<+List<Sym>>>:
  match a:
    case Some{x}:
      Some{prefix(m, x)}
    case None{}:
      None{}

def has_tri(p: +List<Sym>) -> Bool:
  match p:
    case Nil{}:
      False{}
    case Tri{a, j, k, rec, fast} <> t:
      True{}
    case h <> t:
      has_tri(t)

def starts_down(p: +List<Sym>) -> Bool:
  match p:
    case Nil{}:
      False{}
    case h <> t:
      is_path(h)

def starts_left(p: +List<Sym>) -> Bool:
  match p:
    case M1{} <> t:
      True{}
    case _:
      False{}

def has_fix(p: +List<Sym>) -> Bool:
  match p:
    case Nil{}:
      False{}
    case Me{} <> t:
      True{}
    case Fix{body} <> t:
      True{}
    case h <> t:
      has_fix(t)

# packets a loop's node can run as a burst: each takes a step down first (so
# none acts on the loop's node) and holds no recursion; it starts a loop of
# its own only in a fast loop (in a slow one the levels' R would queue at it)
def static(ps: +List<+List<Sym>>, +fast: Bool) -> Bool:
  match ps:
    case Nil{}:
      True{}
    case +p <> t:
      Bool.and(Bool.and(starts_down(p), Bool.and(Bool.or(fast, Bool.not(has_tri(p))), Bool.not(has_fix(p)))), static(t, fast))

# some packet goes into the loop's left part, so its levels must wait on R
def lefts(ps: +List<+List<Sym>>) -> Bool:
  match ps:
    case Nil{}:
      False{}
    case p <> t:
      Bool.or(starts_left(p), lefts(t))

def flat(ps: +List<+List<Sym>>) -> +List<Sym>:
  List.concat(&2, Sym, ps)

def keep_loop(ok: Bool, s: Sym) -> +Maybe<+List<+List<Sym>>>:
  match ok:
    case True{}:
      Some{[[s]]}
    case False{}:
      None{}

def loop_if(+fast: Bool, +x: +List<+List<Sym>>, +y: +List<+List<Sym>>, +z: +List<+List<Sym>>, rec: Bool) -> +Maybe<+List<+List<Sym>>>:
  keep_loop(Bool.and(static(x, fast), Bool.and(static(y, fast), static(z, fast))), Tri{flat(x), flat(y), flat(z), rec, fast})

def loop_of(a: +Maybe<+List<+List<Sym>>>, j: +Maybe<+List<+List<Sym>>>, k: +Maybe<+List<+List<Sym>>>, +rec: Bool) -> +Maybe<+List<+List<Sym>>>:
  match a j k:
    case Some{+x} Some{+y} Some{+z}:
      loop_if(Bool.not(Bool.or(lefts(x), Bool.or(lefts(y), lefts(z)))), x, y, z, rec)
    case _ _ _:
      None{}

# run backward, a loop's bursts before and after self trade places
def loop_way(+fwd: Bool, a: +Maybe<+List<+List<Sym>>>, x: +Maybe<+List<+List<Sym>>>, y: +Maybe<+List<+List<Sym>>>) -> +Maybe<+List<+List<Sym>>>:
  match fwd:
    case True{}:
      loop_of(a, x, y, True{})
    case False{}:
      loop_of(a, y, x, True{})

def stem_way(fwd: Bool) -> +Maybe<+List<+List<Sym>>>:
  match fwd:
    case True{}:
      Some{[[Sp{}]]}
    case False{}:
      Some{[[Sm{}]]}

def recurs(t: TW.Tree) -> +Maybe<Bool>:
  match t:
    case TW.S{TW.L{}}:
      Some{True{}}
    case TW.L{}:
      Some{False{}}
    case _:
      None{}

def each_of(x: +Maybe<Bool>, y: +Maybe<Bool>, z: +Maybe<Bool>, turn: Turn) -> +Maybe<+List<+List<Sym>>>:
  match x y z:
    case Some{a} Some{b} Some{c}:
      Some{[[Each{a, b, c, turn}]]}
    case _ _ _:
      None{}

# a swap after the copies, run backward, comes before them
def turned(fwd: Bool) -> Turn:
  match fwd:
    case True{}:
      TurnLast{}
    case False{}:
      TurnFirst{}

# a recursion a fork runs as one Fix: each of its packets goes into one of
# the fork's halves, or is a loop or a swap at the fork, so on a leaf or a
# stem (which have no halves) it does nothing, as the reference's does
def halves(p: +List<Sym>) -> Bool:
  match p:
    case M1{} <> t:
      True{}
    case M2{} <> t:
      True{}
    case Tri{a, j, k, rec, fast} <> Nil{}:
      True{}
    case X{} <> Nil{}:
      True{}
    case _:
      False{}

def at_forks(ps: +List<+List<Sym>>) -> Bool:
  match ps:
    case Nil{}:
      True{}
    case p <> t:
      Bool.and(halves(p), at_forks(t))

def fix_of(m: +Maybe<+List<+List<Sym>>>) -> +Maybe<+List<+List<Sym>>>:
  match m:
    case Some{+ps}:
      keep_loop(at_forks(ps), Fix{flat(ps)})
    case None{}:
      None{}

def stem_n(n: Nat) -> TW.Tree:
  match n:
    case 0n:
      TW.L{}
    case 1n+k:
      TW.S{stem_n(k)}

# the library's not: unit ; swap ; tri(stem, id, id) ; swap ; tri(stem, id, id)† ; swap ; unit†
def not_tree() -> TW.Tree:
  TW.p_seq(stem_n(6n), TW.p_seq(TW.p_swap(), TW.p_seq(TW.F{TW.F{stem_n(5n), TW.L{}}, TW.L{}}, TW.p_seq(TW.p_swap(), TW.p_seq(TW.p_dag(TW.F{TW.F{stem_n(5n), TW.L{}}, TW.L{}}), TW.p_seq(TW.p_swap(), TW.p_dag(stem_n(6n))))))))

def flip_or(b: Bool, other: +Maybe<+List<+List<Sym>>>) -> +Maybe<+List<+List<Sym>>>:
  match b:
    case True{}:
      Some{[[Flip{}]]}
    case False{}:
      other

def seq_way(fwd: Bool, a: +Maybe<+List<+List<Sym>>>, b: +Maybe<+List<+List<Sym>>>) -> +Maybe<+List<+List<Sym>>>:
  match fwd:
    case True{}:
      then(a, b)
    case False{}:
      then(b, a)

# the packets of p run forward or backward; None outside the design's
# programs. not goes in as one Flip, either way. The loops: tri(a, b, id),
# and fix(tri(a, b, id)) with b one of self, self ; y, x ; self and x ; self
# ; y. At a leaf tri runs a on the right part, so a's packets take a step
# right first. The copied recursions: fix(map(x, y, z)) and fix(map(x, y, z)
# ; swap), each of x, y, z self or id. Any other fix(b) is a Fix, if b's
# packets suit one
def packets(p: TW.Tree, +fwd: Bool) -> +Maybe<+List<+List<Sym>>>:
  match p:
    case TW.L{}:
      Some{Nil{}}
    case TW.F{TW.L{}, b}:
      packets(b, Bool.not(fwd))
    case TW.F{TW.S{+a}, +b}:
      flip_or(TW.Tree.is_eq(TW.F{TW.S{a}, b}, not_tree()), seq_way(fwd, packets(a, fwd), packets(b, fwd)))
    case TW.F{TW.F{a, b}, TW.L{}}:
      loop_of(under(M2{}, packets(a, fwd)), packets(b, fwd), Some{Nil{}}, False{})
    case TW.S{TW.F{TW.L{}, TW.F{TW.F{a, TW.S{TW.L{}}}, TW.L{}}}}:
      loop_way(fwd, under(M2{}, packets(a, fwd)), Some{Nil{}}, Some{Nil{}})
    case TW.S{TW.F{TW.L{}, TW.F{TW.F{a, TW.F{TW.S{TW.S{TW.L{}}}, y}}, TW.L{}}}}:
      loop_way(fwd, under(M2{}, packets(a, fwd)), Some{Nil{}}, packets(y, fwd))
    case TW.S{TW.F{TW.L{}, TW.F{TW.F{a, TW.F{TW.S{x}, TW.S{TW.L{}}}}, TW.L{}}}}:
      loop_way(fwd, under(M2{}, packets(a, fwd)), packets(x, fwd), Some{Nil{}})
    case TW.S{TW.F{TW.L{}, TW.F{TW.F{a, TW.F{TW.S{x}, TW.F{TW.S{TW.S{TW.L{}}}, y}}}, TW.L{}}}}:
      loop_way(fwd, under(M2{}, packets(a, fwd)), packets(x, fwd), packets(y, fwd))
    case TW.S{TW.F{TW.L{}, TW.S{TW.F{TW.F{x, y}, z}}}}:
      each_of(recurs(x), recurs(y), recurs(z), NoTurn{})
    case TW.S{TW.F{TW.L{}, TW.F{TW.S{TW.S{TW.F{TW.F{x, y}, z}}}, TW.S{TW.S{TW.L{}}}}}}:
      each_of(recurs(x), recurs(y), recurs(z), turned(fwd))
    case TW.S{TW.F{TW.L{}, b}}:
      fix_of(packets(b, fwd))
    case TW.S{TW.F{TW.F{a, b}, c}}:
      then(then(under(M0{}, packets(a, fwd)), under(M1{}, packets(b, fwd))), under(M2{}, packets(c, fwd)))
    case TW.S{TW.L{}}:
      Some{[[Me{}]]}
    case TW.S{TW.S{TW.L{}}}:
      Some{[[X{}]]}
    case TW.S{TW.S{TW.S{TW.S{TW.S{TW.L{}}}}}}:
      stem_way(fwd)
    case _:
      None{}

def keep_all(ok: Bool, +ps: +List<+List<Sym>>) -> +Maybe<+List<+List<Sym>>>:
  match ok:
    case True{}:
      Some{ps}
    case False{}:
      None{}

def has_me(p: +List<Sym>) -> Bool:
  match p:
    case Nil{}:
      False{}
    case Me{} <> t:
      True{}
    case h <> t:
      has_me(t)

def any_me(ps: +List<+List<Sym>>) -> Bool:
  match ps:
    case Nil{}:
      False{}
    case p <> t:
      Bool.or(has_me(p), any_me(t))

# self with no fix around it is stuck
def guard(m: +Maybe<+List<+List<Sym>>>) -> +Maybe<+List<+List<Sym>>>:
  match m:
    case Some{+ps}:
      keep_all(Bool.not(any_me(ps)), ps)
    case None{}:
      None{}

# p's packets, run forward; None outside the design
def program(+p: TW.Tree) -> +Maybe<+List<+List<Sym>>>:
  guard(packets(p, True{}))

def symbols(ps: +List<+List<Sym>>) -> +List<Sym>:
  List.concat(&2, Sym, ps)

# Laying out: a tree in rows under a column of symbols
# ----------------------------------------------------

def width(t: TW.Tree) -> Nat:
  match t:
    case TW.L{}:
      1n
    case TW.S{c}:
      width(c)
    case TW.F{l, r}:
      Nat.add(width(l), width(r))

def stems(t: TW.Tree) -> Nat:
  match t:
    case TW.L{}:
      0n
    case TW.S{c}:
      1n+stems(c)
    case TW.F{l, r}:
      Nat.add(stems(l), stems(r))

# the slack in the wire down to t: `gap` cells if t is a fork, which a chain
# cannot grow past, and none otherwise
def edge(t: TW.Tree, +gap: Nat) -> Nat:
  match t:
    case TW.F{l, r}:
      gap
    case _:
      0n

# the rows under t's top: two a level, and the slack over each fork
def tall(t: TW.Tree, +gap: Nat) -> Nat:
  match t:
    case TW.L{}:
      0n
    case TW.S{+c}:
      Nat.add(2n+edge(c, gap), tall(c, gap))
    case TW.F{+l, +r}:
      2n+Nat.max(Nat.add(edge(l, gap), tall(l, gap)), Nat.add(edge(r, gap), tall(r, gap)))

def sub1(n: Nat) -> Nat:
  Nat.sub(n, 1n)

def makes(s: +List<Sym>) -> Bool:
  match s:
    case Nil{}:
      False{}
    case Sp{} <> t:
      True{}
    case Tri{a, j, k, rec, fast} <> t:
      Bool.or(Bool.or(makes(a), makes(j)), Bool.or(makes(k), makes(t)))
    case Fix{body} <> t:
      Bool.or(makes(body), makes(t))
    case h <> t:
      makes(t)

def room_if(b: Bool, +v: TW.Tree) -> Nat:
  match b:
    case True{}:
      1n+stems(v)
    case False{}:
      0n

# the room a program that makes stems gets: as slack over each fork, and as
# empty rows under the tree for chains to grow into; on one wire it makes no
# more than one more than the stems already in v
def room(+s: +List<Sym>, +v: TW.Tree) -> Nat:
  room_if(makes(s), v)

# a cell at (x, y)
type Put is Data:
  Put{x: Nat, y: Nat, c: Cell}

def plain(d: Dir) -> Cell:
  Thing{Wire{d, Q{}, Q{}}}

def run_of.go(n: Nat, +r: Nat, +d: Dir, +x: Nat) -> +List<Put>:
  match n:
    case 0n:
      Nil{}
    case 1n+k:
      Put{x, r, plain(d)} <> run_of.go(k, r, d, 1n+x)

# the cells from x up to (not including) y on row r, each a wire toward d
def run_of(+r: Nat, +d: Dir, +x: Nat, y: Nat) -> +List<Put>:
  run_of.go(Nat.sub(y, x), r, d, x)

# n wire cells down from (x, y)
def column(n: Nat, +x: Nat, +y: Nat) -> +List<Put>:
  match n:
    case 0n:
      Nil{}
    case 1n+k:
      Put{x, y, plain(Dn{})} <> column(k, x, 1n+y)

# t's cells, its leftmost leaf in column 2·left and its top in row y
def place(t: TW.Tree, +left: Nat, +y: Nat, +gap: Nat) -> +List<Put>:
  match t:
    case TW.L{}:
      [Put{Nat.double(left), y, Thing{Leaf{Idle{}, Q{}, Nil{}}}}]
    case TW.S{+c}:
      +x = sub1(Nat.add(Nat.double(left), width(c)))
      Put{x, y, Thing{Stem{Dn{}, Idle{}, Q{}, Dn{}, Free{}, Q{}, Nil{}}}} <> List.append(&2, Put, column(1n+edge(c, gap), x, 1n+y), place(c, left, Nat.add(2n+y, edge(c, gap)), gap))
    case TW.F{+l, +r}:
      +x = sub1(Nat.add(Nat.double(left), Nat.add(width(l), width(r))))
      +xl = sub1(Nat.add(Nat.double(left), width(l)))
      +xr = sub1(Nat.add(Nat.double(Nat.add(left, width(l))), width(r)))
      List.concat(&2, Put, [
        [Put{x, y, Thing{Fork{Lf{}, Rt{}, Idle{}, Q{}, Lf{}, False{}, Q{}, Nil{}, Nil{}, False{}}}}],
        run_of(y, Lf{}, 1n+xl, x),
        [Put{xl, y, plain(Dn{})}],
        column(1n+edge(l, gap), xl, 1n+y),
        run_of(y, Rt{}, 1n+x, xr),
        [Put{xr, y, plain(Dn{})}],
        column(1n+edge(r, gap), xr, 1n+y),
        place(l, left, Nat.add(2n+y, edge(l, gap)), gap),
        place(r, Nat.add(left, width(l)), Nat.add(2n+y, edge(r, gap)), gap)])

# a column of symbols ending just above row y
def tape(s: +List<Sym>, +x: Nat, +y: Nat) -> +List<Put>:
  match s:
    case Nil{}:
      Nil{}
    case h <> t:
      Put{x, sub1(y), Thing{Wire{Dn{}, h, Q{}}}} <> tape(t, x, sub1(y))

def pick_cell(b: Bool, c: Cell, rest: Unit -> Cell) -> Cell:
  match b:
    case True{}:
      c
    case False{}:
      rest(Unit{})

def find(ps: +List<Put>, +x: Nat, +y: Nat) -> Cell:
  match ps:
    case Nil{}:
      Void{}
    case Put{px, py, c} <> t:
      pick_cell(Bool.and(Nat.is_eq(px, x), Nat.is_eq(py, y)), c, u => find(t, x, y))

def row_of(n: Nat, +ps: +List<Put>, +x: Nat, +y: Nat) -> +List<Cell>:
  match n:
    case 0n:
      Nil{}
    case 1n+k:
      find(ps, x, y) <> row_of(k, ps, 1n+x, y)

def grid_of(h: Nat, +ps: +List<Put>, +w: Nat, +y: Nat) -> +List<+List<Cell>>:
  match h:
    case 0n:
      Nil{}
    case 1n+k:
      row_of(w, ps, 0n, y) <> grid_of(k, ps, w, 1n+y)

def width_of(+v: TW.Tree) -> Nat:
  sub1(Nat.double(width(v)))

# n rows of symbols, a plain wire, the tree, and `gap` empty rows to grow into
def height_of(+v: TW.Tree, +n: Nat, +gap: Nat) -> Nat:
  Nat.add(Nat.add(2n+n, tall(v, gap)), gap)

# the grid that starts a run of symbols s on v: s above, a plain wire (so Sp
# first in s finds slack), v below
def lay(+v: TW.Tree, +s: +List<Sym>, +gap: Nat) -> +List<+List<Cell>>:
  +n = List.length(&2, Sym, s)
  +x = sub1(width(v))
  grid_of(height_of(v, n, gap), List.append(&2, Put, tape(s, x, n), Put{x, n, plain(Dn{})} <> place(v, 0n, 1n+n, gap)), width_of(v), 0n)

# Reading a tree back off the grid
# --------------------------------

def nth(~A: Data, xs: +List<A>, n: Nat, d: A) -> A:
  match xs n:
    case Nil{} _:
      d
    case h <> t 0n:
      h
    case h <> t 1n+k:
      nth(~A, t, k, d)

def get(+g: +List<+List<Cell>>, +x: Nat, +y: Nat) -> Cell:
  nth(~Cell, nth(~+List<Cell>, g, y, Nil{}), x, Void{})

def go_x(+x: Nat, d: Dir) -> Nat:
  match d:
    case Rt{}:
      1n+x
    case Lf{}:
      sub1(x)
    case _:
      x

def go_y(+y: Nat, d: Dir) -> Nat:
  match d:
    case Dn{}:
      1n+y
    case Up{}:
      sub1(y)
    case _:
      y

# a growing leaf reads as the stem over a leaf it is about to be
def on_cell(c: Cell, wire: Dir -> +Maybe<TW.Tree>, stem: Dir -> +Maybe<TW.Tree>, fork: Dir -> Dir -> +Maybe<TW.Tree>) -> +Maybe<TW.Tree>:
  match c:
    case Thing{Wire{to, sym, up}}:
      wire(to)
    case Thing{Leaf{latch, up, out}}:
      Some{TW.L{}}
    case Thing{Stem{kid, latch, sym, to, hold, up, out}}:
      stem(kid)
    case Thing{Fork{l, r, latch, sym, to, wide, up, out, todo, busy}}:
      fork(l, r)
    case Thing{Grow{to}}:
      Some{TW.S{TW.L{}}}
    case _:
      None{}

# the tree whose node or wire is at (x, y), following each wire to its node;
# None through a jam
def read(fuel: Nat, +g: +List<+List<Cell>>, +x: Nat, +y: Nat) -> +Maybe<TW.Tree>:
  match fuel:
    case 0n:
      None{}
    case 1n++k:
      on_cell(get(g, x, y),
        +d => read(k, g, go_x(x, d), go_y(y, d)),
        +d => TW.then_stem(read(k, g, go_x(x, d), go_y(y, d))),
        +l => +r => TW.then_fork(read(k, g, go_x(x, l), go_y(y, l)), read(k, g, go_x(x, r), go_y(y, r))))

# the tree grid g holds, for a run of symbols s on v, read from the top of
# the column of symbols, which a stem made at the top may have taken over
def reading(+g: +List<+List<Cell>>, +v: TW.Tree, +s: +List<Sym>) -> +Maybe<TW.Tree>:
  +n = List.length(&2, Sym, s)
  read(Nat.mul(4n, Nat.mul(width_of(v), height_of(v, n, room(s, v)))), g, sub1(width(v)), 0n)

# Running
# -------

def rest(st: Lat.Settled<Cell>) -> +List<+List<Cell>>:
  match st:
    case Lat.Settled{t, g}:
      g

def ticks(st: Lat.Settled<Cell>) -> Nat:
  match st:
    case Lat.Settled{t, g}:
      t

def forks(t: TW.Tree) -> Nat:
  match t:
    case TW.L{}:
      0n
    case TW.S{c}:
      forks(c)
    case TW.F{l, r}:
      1n+Nat.add(forks(l), forks(r))

# a loop that waits on R takes a trip to its node and back a level, and a
# Fix may run a few loops at each fork
def limit(+v: TW.Tree, +s: +List<Sym>) -> Nat:
  +n = List.length(&2, Sym, s)
  Nat.mul(Nat.add(n, Nat.mul(4n, Nat.add(width_of(v), height_of(v, n, room(s, v))))), 1n+Nat.add(stems(v), forks(v)))

# symbols s run on v until the grid is still (or out of ticks)
def settle(+v: TW.Tree, +s: +List<Sym>) -> Lat.Settled<Cell>:
  Lat.settle(~Cell, ~rule, ~Void{}, ~still, limit(v, s), 0n, lay(v, s, room(s, v)))

def on_packets(m: +Maybe<+List<+List<Sym>>>, f: +List<Sym> -> +Maybe<TW.Tree>) -> +Maybe<TW.Tree>:
  match m:
    case Some{ps}:
      f(symbols(ps))
    case None{}:
      None{}

# what the lattice makes of program p on v, read back off the grid; None for
# a program outside the design
def result(+p: TW.Tree, +v: TW.Tree) -> +Maybe<TW.Tree>:
  on_packets(program(p), +s => reading(rest(settle(v, s)), v, s))

# the lattice gives the reference's answer for p on v
def agrees(+p: TW.Tree, +v: TW.Tree) -> Bool:
  TW.Tree.is_eq_maybe(result(p, v), TW.run(256n, p, v, True{}, None{}))

def agrees_on(+p: TW.Tree, vs: +List<TW.Tree>) -> Bool:
  match vs:
    case Nil{}:
      True{}
    case v <> t:
      Bool.and(agrees(p, v), agrees_on(p, t))

def agrees_all(ps: +List<TW.Tree>, +vs: +List<TW.Tree>) -> Bool:
  match ps:
    case Nil{}:
      True{}
    case p <> t:
      Bool.and(agrees_on(p, vs), agrees_all(t, vs))

# add on (2, 1)
def main() -> +Maybe<TW.Tree>:
  result(TW.S{TW.F{TW.L{}, TW.F{TW.F{TW.L{}, TW.F{TW.S{TW.S{TW.L{}}}, TW.S{TW.F{TW.F{TW.L{}, TW.L{}}, TW.S{TW.S{TW.S{TW.S{TW.S{TW.L{}}}}}}}}}}, TW.L{}}}}, TW.F{TW.S{TW.S{TW.L{}}}, TW.S{TW.L{}}})
