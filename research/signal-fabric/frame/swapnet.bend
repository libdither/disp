# A first design inside the frame: a tree laid out on the lattice in rows,
# wires as chains of cells, and a program made of swap, map, ; and † sent in
# at the top as a stream of symbols, one cell a tick.
#
# A program of that kind is a list of swaps, each at a path down the tree, so
# it goes in as packets: a path (M0 into a stem's part, M1 into a fork's left
# part, M2 into its right part) and then X. A node waiting for a packet reads
# its first symbol: X swaps a fork's two wires (which wire is left is part of
# the fork's state); a path symbol that fits sends the rest of the packet on
# down that wire; one that does not (M1 at a stem, say: map does nothing
# there) eats the rest. A packet ends at its X, so a node needs only to
# remember where the packet it is passing on goes. Packets follow one another
# down the wires in order, so a later swap at a node finds every earlier
# swap above it already done, as the program's order says, and no done
# signals are needed.
#
# Not in the design: tri, fix and self, and the moves other than swap (which
# make, take away or move nodes).

import Base
import ./twoway.bend as TW
import ./lattice.bend as Lat

type Dir is Data:
  Up{}
  Rt{}
  Dn{}
  Lf{}

type Sym is Data:
  Q{}
  X{}
  M0{}
  M1{}
  M2{}

type Latch is Data:
  Idle{}
  Pass{to: Dir}
  Drop{}

# what a cell holds: nothing, a wire (passing its symbol on toward `to`), or
# a node with its parts' wires, its latch, and the symbol it sends toward `to`
type Cell is Data:
  Void{}
  Wire{to: Dir, sym: Sym}
  Leaf{}
  Stem{kid: Dir, latch: Latch, sym: Sym, to: Dir}
  Fork{l: Dir, r: Dir, latch: Latch, sym: Sym, to: Dir}

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
def sends(c: Cell, d: Dir) -> Sym:
  match c:
    case Wire{to, sym}:
      if_sym(Dir.is_eq(to, d), sym)
    case Stem{kid, latch, sym, to}:
      if_sym(Dir.is_eq(to, d), sym)
    case Fork{l, r, latch, sym, to}:
      if_sym(Dir.is_eq(to, d), sym)
    case _:
      Q{}

def or_sym(a: Sym, b: Sym) -> Sym:
  match a:
    case Q{}:
      b
    case s:
      s

# what reaches a cell: whatever a cell beside it sends its way
def incoming(n: Cell, e: Cell, s: Cell, w: Cell) -> Sym:
  or_sym(sends(n, Dn{}), or_sym(sends(e, Lf{}), or_sym(sends(s, Up{}), sends(w, Rt{}))))

def is_x(s: Sym) -> Bool:
  match s:
    case X{}:
      True{}
    case _:
      False{}

# a latch passing a packet on, or eating it, until the packet's X
def until_x(l: Latch, s: Sym) -> Latch:
  match s:
    case X{}:
      Idle{}
    case _:
      l

def stem_latch(+kid: Dir, l: Latch, +i: Sym) -> Latch:
  match l:
    case Idle{}:
      match i:
        case M0{}:
          Pass{kid}
        case M1{}:
          Drop{}
        case M2{}:
          Drop{}
        case _:
          Idle{}
    case Pass{d}:
      until_x(Pass{d}, i)
    case Drop{}:
      until_x(Drop{}, i)

def fork_latch(+l: Dir, +r: Dir, lt: Latch, +i: Sym) -> Latch:
  match lt:
    case Idle{}:
      match i:
        case M1{}:
          Pass{l}
        case M2{}:
          Pass{r}
        case M0{}:
          Drop{}
        case _:
          Idle{}
    case Pass{d}:
      until_x(Pass{d}, i)
    case Drop{}:
      until_x(Drop{}, i)

# what a node passes on, and which way: only while passing a packet on
def out_sym(l: Latch, i: Sym) -> Sym:
  match l:
    case Pass{d}:
      i
    case _:
      Q{}

def out_to(l: Latch, d: Dir) -> Dir:
  match l:
    case Pass{e}:
      e
    case _:
      d

# a waiting fork that reads X trades its wires
def swaps(lt: Latch, i: Sym) -> Bool:
  match lt:
    case Idle{}:
      is_x(i)
    case _:
      False{}

def pick_dir(b: Bool, x: Dir, y: Dir) -> Dir:
  match b:
    case True{}:
      x
    case False{}:
      y

# one cell's next state from what it holds and what reaches it; every arm
# gives back a cell of the same kind
def next(c: Cell, +i: Sym) -> Cell:
  match c:
    case Void{}:
      Void{}
    case Wire{to, sym}:
      Wire{to, i}
    case Leaf{}:
      Leaf{}
    case Stem{+kid, +latch, sym, +to}:
      Stem{kid, stem_latch(kid, latch, i), out_sym(latch, i), out_to(latch, to)}
    case Fork{+l, +r, +latch, sym, +to}:
      Fork{pick_dir(swaps(latch, i), r, l), pick_dir(swaps(latch, i), l, r), fork_latch(l, r, latch, i), out_sym(latch, i), out_to(latch, to)}

def rule(nb: Lat.Nb<Cell>) -> Cell:
  match nb:
    case Lat.Nb{c, n, e, s, w}:
      next(c, incoming(n, e, s, w))

# a cell with nothing to do: no symbol in it, no packet half passed on
def still(c: Cell) -> Bool:
  match c:
    case Wire{to, Q{}}:
      True{}
    case Wire{to, sym}:
      False{}
    case Stem{kid, Idle{}, Q{}, to}:
      True{}
    case Fork{l, r, Idle{}, Q{}, to}:
      True{}
    case Stem{kid, latch, sym, to}:
      False{}
    case Fork{l, r, latch, sym, to}:
      False{}
    case _:
      True{}

# what kind of matter a cell holds: 0 nothing, 1 wire, 2 leaf, 3 stem, 4 fork
def kind(c: Cell) -> Nat:
  match c:
    case Void{}:
      0n
    case Wire{to, sym}:
      1n
    case Leaf{}:
      2n
    case Stem{kid, latch, sym, to}:
      3n
    case Fork{l, r, latch, sym, to}:
      4n

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

# the packets of p run forward or backward; None outside the design's programs
def packets(p: TW.Tree, +fwd: Bool) -> +Maybe<+List<+List<Sym>>>:
  match p:
    case TW.L{}:
      Some{Nil{}}
    case TW.F{TW.L{}, b}:
      packets(b, Bool.not(fwd))
    case TW.F{TW.S{a}, b}:
      match fwd:
        case True{}:
          then(packets(a, True{}), packets(b, True{}))
        case False{}:
          then(packets(b, False{}), packets(a, False{}))
    case TW.S{TW.F{TW.F{a, b}, c}}:
      then(then(under(M0{}, packets(a, fwd)), under(M1{}, packets(b, fwd))), under(M2{}, packets(c, fwd)))
    case TW.S{TW.S{TW.L{}}}:
      Some{[[X{}]]}
    case _:
      None{}

# Laying out: a tree in rows under a column of symbols
# ----------------------------------------------------

# leaves under t (a stem's part is under it, a fork's side by side)
def width(t: TW.Tree) -> Nat:
  match t:
    case TW.L{}:
      1n
    case TW.S{c}:
      width(c)
    case TW.F{l, r}:
      Nat.add(width(l), width(r))

def depth(t: TW.Tree) -> Nat:
  match t:
    case TW.L{}:
      0n
    case TW.S{c}:
      1n+depth(c)
    case TW.F{l, r}:
      1n+Nat.max(depth(l), depth(r))

def Nat.sub1(n: Nat) -> Nat:
  Nat.sub(n, 1n)

# a cell at (x, y)
type Put is Data:
  Put{x: Nat, y: Nat, c: Cell}

# the cells from x up to (not including) y on row r, each a wire toward d
def run_of.go(n: Nat, +r: Nat, +d: Dir, +x: Nat) -> +List<Put>:
  match n:
    case 0n:
      Nil{}
    case 1n+k:
      Put{x, r, Wire{d, Q{}}} <> run_of.go(k, r, d, 1n+x)

def run_of(+r: Nat, +d: Dir, +x: Nat, y: Nat) -> +List<Put>:
  run_of.go(Nat.sub(y, x), r, d, x)

# t's cells, its leftmost leaf in column 2·left and its top in row y: a node
# sits above the middle of its leaves; a fork's left wire runs west along its
# row and down to its left part, its right wire east and down
def place(t: TW.Tree, +left: Nat, +y: Nat) -> +List<Put>:
  match t:
    case TW.L{}:
      [Put{Nat.add(Nat.double(left), 0n), y, Leaf{}}]
    case TW.S{+c}:
      +x = Nat.sub1(Nat.add(Nat.double(left), width(c)))
      Put{x, y, Stem{Dn{}, Idle{}, Q{}, Dn{}}} <> Put{x, 1n+y, Wire{Dn{}, Q{}}} <> place(c, left, 2n+y)
    case TW.F{+l, +r}:
      +x = Nat.sub1(Nat.add(Nat.double(left), Nat.add(width(l), width(r))))
      +xl = Nat.sub1(Nat.add(Nat.double(left), width(l)))
      +xr = Nat.sub1(Nat.add(Nat.double(Nat.add(left, width(l))), width(r)))
      List.concat(&2, Put, [
        [Put{x, y, Fork{Lf{}, Rt{}, Idle{}, Q{}, Lf{}}}],
        run_of(y, Lf{}, 1n+xl, x),
        [Put{xl, y, Wire{Dn{}, Q{}}}, Put{xl, 1n+y, Wire{Dn{}, Q{}}}],
        run_of(y, Rt{}, 1n+x, xr),
        [Put{xr, y, Wire{Dn{}, Q{}}}, Put{xr, 1n+y, Wire{Dn{}, Q{}}}],
        place(l, left, 2n+y),
        place(r, Nat.add(left, width(l)), 2n+y)])

# a column of symbols over the top node, the first just above it
def tape(s: +List<Sym>, +x: Nat, +y: Nat) -> +List<Put>:
  match s:
    case Nil{}:
      Nil{}
    case h <> t:
      Put{x, Nat.sub1(y), Wire{Dn{}, h}} <> tape(t, x, Nat.sub1(y))

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

def symbols(ps: +List<+List<Sym>>) -> +List<Sym>:
  List.concat(&2, Sym, ps)

def width_of(+v: TW.Tree) -> Nat:
  Nat.sub1(Nat.double(width(v)))

def height_of(+v: TW.Tree, +n: Nat) -> Nat:
  Nat.add(n, 1n+Nat.double(depth(v)))

# the grid that starts a run of symbols s on v: s above, v below
def lay(+v: TW.Tree, +s: +List<Sym>) -> +List<+List<Cell>>:
  grid_of(height_of(v, List.length(&2, Sym, s)), List.append(&2, Put, tape(s, Nat.sub1(width(v)), List.length(&2, Sym, s)), place(v, 0n, List.length(&2, Sym, s))), width_of(v), 0n)

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
      Nat.sub1(x)
    case _:
      x

def go_y(+y: Nat, d: Dir) -> Nat:
  match d:
    case Dn{}:
      1n+y
    case Up{}:
      Nat.sub1(y)
    case _:
      y

def on_cell(c: Cell, wire: Dir -> +Maybe<TW.Tree>, stem: Dir -> +Maybe<TW.Tree>, fork: Dir -> Dir -> +Maybe<TW.Tree>) -> +Maybe<TW.Tree>:
  match c:
    case Wire{to, sym}:
      wire(to)
    case Leaf{}:
      Some{TW.L{}}
    case Stem{kid, latch, sym, to}:
      stem(kid)
    case Fork{l, r, latch, sym, to}:
      fork(l, r)
    case Void{}:
      None{}

# the tree whose node or wire is at (x, y), following each wire to its node
def read(fuel: Nat, +g: +List<+List<Cell>>, +x: Nat, +y: Nat) -> +Maybe<TW.Tree>:
  match fuel:
    case 0n:
      None{}
    case 1n++k:
      on_cell(get(g, x, y),
        +d => read(k, g, go_x(x, d), go_y(y, d)),
        +d => TW.then_stem(read(k, g, go_x(x, d), go_y(y, d))),
        +l => +r => TW.then_fork(read(k, g, go_x(x, l), go_y(y, l)), read(k, g, go_x(x, r), go_y(y, r))))

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

# symbols s run on v until the grid is still (or out of ticks)
def settle(+v: TW.Tree, +s: +List<Sym>) -> Lat.Settled<Cell>:
  Lat.settle(~Cell, ~rule, ~Void{}, ~still, Nat.add(List.length(&2, Sym, s), Nat.mul(4n, Nat.add(width_of(v), height_of(v, List.length(&2, Sym, s))))), 0n, lay(v, s))

def on_packets(m: +Maybe<+List<+List<Sym>>>, f: +List<Sym> -> +Maybe<TW.Tree>) -> +Maybe<TW.Tree>:
  match m:
    case Some{ps}:
      f(symbols(ps))
    case None{}:
      None{}

# what the lattice makes of program p on v, read back off the grid; None for
# a program outside the design
def result(+p: TW.Tree, +v: TW.Tree) -> +Maybe<TW.Tree>:
  on_packets(packets(p, True{}), +s => read(Nat.mul(4n, Nat.mul(width_of(v), height_of(v, List.length(&2, Sym, s)))), rest(settle(v, s)), Nat.sub1(width(v)), List.length(&2, Sym, s)))

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

def main() -> +Maybe<TW.Tree>:
  result(TW.p_seq(TW.p_first(TW.p_swap()), TW.p_swap()), TW.F{TW.F{TW.L{}, TW.S{TW.L{}}}, TW.S{TW.S{TW.L{}}}})
