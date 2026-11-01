# Runs a design (design.bend) in the block physics and checks a run as the
# laws need: what it reads as at every tick, whether it went still, and what
# left the grid. A run starts with the program's cells (Design.inject, which
# sees only the program) in a column above the tree (Design.lay), and ends
# once a whole tick of each kind changes nothing and nothing leaves.
#
# A design module provides:
#   fields() -> +List<Nat>                    each field's range
#   reactions() -> +List<B.Reaction>          the rule
#   lay(x: TW.Tree) -> B.Layout               a tree on a grid
#   inject(p: TW.Tree) -> +Maybe<+List<+List<Nat>>>
#                                             a program's cells, top to bottom;
#                                             None: outside the design
#   read(g, port: Nat) -> +Maybe<TW.Tree>     the tree a grid holds

import Base
import ./calculus.bend as TW
import ./blocks.bend as B
import ./design.bend as Design

def zeros(rs: +List<Nat>) -> +List<Nat>:
  match rs:
    case Nil{}:
      Nil{}
    case r <> t:
      0n <> zeros(t)

def void() -> +List<Nat>:
  zeros(Design.fields())

def first_range(rs: +List<Nat>) -> Nat:
  match rs:
    case Nil{}:
      1n
    case r <> t:
      r

# the design's reactions, indexed for ticking
def rule() -> +List<+List<B.Reaction>>:
  B.index(Design.reactions(), first_range(Design.fields()))

# Laying a run out
# ----------------

def even_up(+n: Nat) -> Nat:
  Nat.add(n, Nat.mod(n, 2n))

def pad_row(+r: +List<+List<Nat>>, +w: Nat, +v: +List<Nat>) -> +List<+List<Nat>>:
  List.append(&2, +List<Nat>, r, B.row_of(Nat.sub(w, List.length(&2, +List<Nat>, r)), v))

def pad_rows(g: +List<+List<+List<Nat>>>, +w: Nat, +v: +List<Nat>) -> +List<+List<+List<Nat>>>:
  match g:
    case Nil{}:
      Nil{}
    case r <> t:
      pad_row(r, w, v) <> pad_rows(t, w, v)

# a row of empty cells with c in column x
def row_with(n: Nat, x: Nat, +c: +List<Nat>, +v: +List<Nat>) -> +List<+List<Nat>>:
  match n:
    case 0n:
      Nil{}
    case 1n+k:
      match x:
        case 0n:
          c <> B.row_of(k, v)
        case 1n+j:
          v <> row_with(k, j, c, v)

def tape_rows(cs: +List<+List<Nat>>, +w: Nat, +x: Nat, +v: +List<Nat>) -> +List<+List<+List<Nat>>>:
  match cs:
    case Nil{}:
      Nil{}
    case c <> t:
      row_with(w, x, c, v) <> tape_rows(t, w, x, v)

def rows_of(n: Nat, +r: +List<+List<Nat>>) -> +List<+List<+List<Nat>>>:
  match n:
    case 0n:
      Nil{}
    case 1n+k:
      r <> rows_of(k, r)

def widest(g: +List<+List<+List<Nat>>>) -> Nat:
  match g:
    case Nil{}:
      0n
    case r <> t:
      Nat.max(List.length(&2, +List<Nat>, r), widest(t))

# the tape above the layout, both made as wide as the widest row (and even),
# and an empty row below if that makes the height even
def grid_of(tape: +List<+List<Nat>>, l: B.Layout, +v: +List<Nat>) -> B.Layout:
  match l:
    case B.Layout{+g, +port}:
      +w = even_up(Nat.max(widest(g), 1n+port))
      +rows = List.append(&2, +List<+List<Nat>>, tape_rows(tape, w, port, v), pad_rows(g, w, v))
      B.Layout{List.append(&2, +List<+List<Nat>>, rows, rows_of(Nat.mod(List.length(&2, +List<+List<Nat>>, rows), 2n), B.row_of(w, v))), port}

def start_of(m: +Maybe<+List<+List<Nat>>>, x: TW.Tree) -> +Maybe<B.Layout>:
  match m:
    case Some{tape}:
      Some{grid_of(tape, Design.lay(x), void())}
    case None{}:
      None{}

# the grid a run of p on x starts from; None outside the design
def start(p: TW.Tree, x: TW.Tree) -> +Maybe<B.Layout>:
  start_of(Design.inject(p), x)

# Running
# -------

def cells_eq(a: +List<Nat>, b: +List<Nat>) -> Bool:
  match a b:
    case Nil{} Nil{}:
      True{}
    case x <> s y <> t:
      Bool.and(Nat.is_eq(x, y), cells_eq(s, t))
    case _ _:
      False{}

def row_eq(a: +List<+List<Nat>>, b: +List<+List<Nat>>) -> Bool:
  match a b:
    case Nil{} Nil{}:
      True{}
    case x <> s y <> t:
      Bool.and(cells_eq(x, y), row_eq(s, t))
    case _ _:
      False{}

def grid_eq(a: +List<+List<+List<Nat>>>, b: +List<+List<+List<Nat>>>) -> Bool:
  match a b:
    case Nil{} Nil{}:
      True{}
    case x <> s y <> t:
      Bool.and(row_eq(x, y), grid_eq(s, t))
    case _ _:
      False{}

def is_some(m: +Maybe<TW.Tree>) -> Bool:
  match m:
    case Some{t}:
      True{}
    case None{}:
      False{}

# the reading changed only by a step of moves (a missing reading is
# always_reads' business)
def moved_maybe(a: +Maybe<TW.Tree>, b: +Maybe<TW.Tree>) -> Bool:
  match a b:
    case Some{+x} Some{+y}:
      TW.moved(x, y)
    case _ _:
      True{}

# everything that left went up the port column
def up_port(os: +List<B.Out>, +port: Nat) -> Bool:
  match os:
    case Nil{}:
      True{}
    case B.Out{x, y, c} <> t:
      Bool.and(Bool.and(Nat.is_eq(x, 1n+port), Nat.is_eq(y, 0n)), up_port(t, port))

def no_outs(os: +List<B.Out>) -> Bool:
  match os:
    case Nil{}:
      True{}
    case o <> t:
      False{}

# a run: what it read as at first, the ticks it took, its last grid, what
# left, whether it went still, and whether, at every tick, it read as a
# tree, changed only by moves, and let things out only up the port
type Run is Data:
  Run{first: +Maybe<TW.Tree>, t: Nat, g: +List<+List<+List<Nat>>>, outs: +List<B.Out>, still: Bool, reads: Bool, moves: Bool, sealed: Bool}

def stop_or(b: Bool, r: Run, more: Unit -> Run) -> Run:
  match b:
    case True{}:
      r
    case False{}:
      more(Unit{})

def quiet_after(changed: Bool, +q: Nat) -> Nat:
  match changed:
    case True{}:
      0n
    case False{}:
      1n+q

# what g reads as, if the run is watched tick by tick (else what it read before)
def read_if(watch: Bool, +g: +List<+List<+List<Nat>>>, +port: Nat, +was: +Maybe<TW.Tree>) -> +Maybe<TW.Tree>:
  match watch:
    case True{}:
      Design.read(g, port)
    case False{}:
      was

# c is what the tick just taken from g gave
def go(fuel: Nat, +watch: Bool, +first: +Maybe<TW.Tree>, +rs: +List<+List<B.Reaction>>, +v: +List<Nat>, +port: Nat, +shifted: Bool, +quiet: Nat, +t: Nat, +g: +List<+List<+List<Nat>>>, +was: +Maybe<TW.Tree>, +outs: +List<B.Out>, +reads: Bool, +moves: Bool, +sealed: Bool, c: B.Cut) -> Run:
  match fuel:
    case 0n:
      Run{first, t, g, outs, False{}, reads, moves, sealed}
    case 1n++k:
      match c:
        case B.Cut{+h, +os}:
          +now = read_if(watch, h, port, was)
          +q = quiet_after(Bool.or(Bool.not(grid_eq(g, h)), Bool.not(no_outs(os))), quiet)
          +all = List.append(&2, B.Out, outs, os)
          +rd = Bool.and(reads, is_some(now))
          +mv = Bool.and(moves, moved_maybe(now, was))
          +sl = Bool.and(sealed, up_port(os, port))
          stop_or(Nat.is_le(2n, q), Run{first, 1n+t, h, all, True{}, rd, mv, sl}, u => go(k, watch, first, rs, v, port, Bool.not(shifted), q, 1n+t, h, now, all, rd, mv, sl, B.tick(rs, v, Bool.not(shifted), h)))

def rows_cols(+g: +List<+List<+List<Nat>>>) -> Nat:
  Nat.add(List.length(&2, +List<+List<Nat>>, g), widest(g))

# the ticks a run gets: plenty for a design that goes still
def limit(+g: +List<+List<+List<Nat>>>) -> Nat:
  Nat.add(256n, Nat.mul(Nat.mul(8n, rows_cols(g)), rows_cols(g)))

def run_from(+watch: Bool, l: B.Layout) -> Run:
  match l:
    case B.Layout{+g, +port}:
      +rs = rule()
      +v = void()
      +r0 = Design.read(g, port)
      go(limit(g), watch, r0, rs, v, port, False{}, 0n, 0n, g, r0, Nil{}, is_some(r0), True{}, True{}, B.tick(rs, v, False{}, g))

# watch: read the grid at every tick (for always_reads and moves_only)
def run_of(+watch: Bool, m: +Maybe<B.Layout>) -> +Maybe<Run>:
  match m:
    case Some{l}:
      Some{run_from(watch, l)}
    case None{}:
      None{}

# p run on x in the design; None outside it
def run(p: TW.Tree, x: TW.Tree) -> +Maybe<Run>:
  run_of(True{}, start(p, x))

def port_of(m: +Maybe<B.Layout>) -> Nat:
  match m:
    case Some{B.Layout{g, port}}:
      port
    case None{}:
      0n

# what a still run reads as at the end; None if it never went still
def answer_of(r: Run, +port: Nat) -> +Maybe<TW.Tree>:
  match r:
    case Run{f, t, g, outs, True{}, rd, mv, sl}:
      Design.read(g, port)
    case _:
      None{}

# Checks over cases
# -----------------

def is_supported(m: +Maybe<+List<+List<Nat>>>) -> Bool:
  match m:
    case Some{x}:
      True{}
    case None{}:
      False{}

def supports(ps: +List<TW.Tree>) -> Bool:
  match ps:
    case Nil{}:
      True{}
    case p <> t:
      Bool.and(is_supported(Design.inject(p)), supports(t))

type Check is Data:
  Reads{}
  Moves{}
  Sealed{}
  Halts{}
  Agrees{}

def stuck(m: +Maybe<TW.Tree>) -> Bool:
  match m:
    case None{}:
      True{}
    case Some{t}:
      False{}

def same_answer(a: +Maybe<TW.Tree>, b: +Maybe<TW.Tree>) -> Bool:
  match b:
    case None{}:
      True{}
    case Some{+y}:
      TW.Tree.is_eq_maybe(a, Some{y})

def flag(k: Check, r: Run, +x: TW.Tree, +want: +Maybe<TW.Tree>) -> Bool:
  match k r:
    case Reads{} Run{f, t, g, outs, still, rd, mv, sl}:
      Bool.and(TW.Tree.is_eq_maybe(f, Some{x}), rd)
    case Moves{} Run{f, t, g, outs, still, rd, mv, sl}:
      mv
    case Sealed{} Run{f, t, g, outs, still, rd, mv, sl}:
      sl
    case Halts{} Run{f, t, g, outs, still, rd, mv, sl}:
      Bool.or(stuck(want), still)
    case Agrees{} _:
      False{}

def check_one(+k: Check, +r: Run, +x: TW.Tree, +want: +Maybe<TW.Tree>, +port: Nat) -> Bool:
  match k:
    case Agrees{}:
      same_answer(answer_of(r, port), want)
    case _:
      flag(k, r, x, want)

# one run's check; a program outside the design passes (supports asks for it)
def check_run(m: +Maybe<Run>, +k: Check, +x: TW.Tree, +want: +Maybe<TW.Tree>, +port: Nat) -> Bool:
  match m:
    case None{}:
      True{}
    case Some{r}:
      check_one(k, r, x, want, port)

def watched(k: Check) -> Bool:
  match k:
    case Reads{}:
      True{}
    case Moves{}:
      True{}
    case _:
      False{}

def check_on(+k: Check, +p: TW.Tree, xs: +List<TW.Tree>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case +x <> t:
      +m = start(p, x)
      Bool.and(check_run(run_of(watched(k), m), k, x, TW.run(256n, p, x, True{}, None{}), port_of(m)), check_on(k, p, t))

# check k holds for every run of every program in ps on every tree in xs
def check_all(+k: Check, ps: +List<TW.Tree>, +xs: +List<TW.Tree>) -> Bool:
  match ps:
    case Nil{}:
      True{}
    case p <> t:
      Bool.and(check_on(k, p, xs), check_all(k, t, xs))

# Layouts
# -------

def in_ranges(c: +List<Nat>, rs: +List<Nat>) -> Bool:
  match c rs:
    case Nil{} Nil{}:
      True{}
    case x <> s r <> t:
      Bool.and(Nat.is_lt(x, r), in_ranges(s, t))
    case _ _:
      False{}

def cells_in(r: +List<+List<Nat>>, +ranges: +List<Nat>) -> Bool:
  match r:
    case Nil{}:
      True{}
    case c <> t:
      Bool.and(in_ranges(c, ranges), cells_in(t, ranges))

def grid_in(g: +List<+List<+List<Nat>>>, +ranges: +List<Nat>) -> Bool:
  match g:
    case Nil{}:
      True{}
    case r <> t:
      Bool.and(cells_in(r, ranges), grid_in(t, ranges))

def tape_in(m: +Maybe<+List<+List<Nat>>>, +ranges: +List<Nat>) -> Bool:
  match m:
    case Some{cs}:
      cells_in(cs, ranges)
    case None{}:
      True{}

def rests(c: B.Cut, +g: +List<+List<+List<Nat>>>) -> Bool:
  match c:
    case B.Cut{h, os}:
      Bool.and(grid_eq(h, g), no_outs(os))

# a tick of each kind leaves g as it is, and lets nothing out
def at_rest(+g: +List<+List<+List<Nat>>>) -> Bool:
  +rs = rule()
  +v = void()
  Bool.and(rests(B.tick(rs, v, False{}, g), g), rests(B.tick(rs, v, True{}, g), g))

def lay_check(+x: TW.Tree, l: B.Layout) -> Bool:
  match l:
    case B.Layout{+g, +port}:
      Bool.and(Bool.and(TW.Tree.is_eq_maybe(Design.read(g, port), Some{x}), grid_in(g, Design.fields())), at_rest(g))

def lay_ok(+x: TW.Tree) -> Bool:
  lay_check(x, Design.lay(x))

def lays(xs: +List<TW.Tree>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case x <> t:
      Bool.and(lay_ok(x), lays(t))

def tapes_in(ps: +List<TW.Tree>) -> Bool:
  match ps:
    case Nil{}:
      True{}
    case p <> t:
      Bool.and(tape_in(Design.inject(p), Design.fields()), tapes_in(t))
