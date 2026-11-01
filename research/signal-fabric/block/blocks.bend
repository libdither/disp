# The physics every block design shares. A grid of cells is cut into 2×2
# blocks, and one rule turns each block into a new block, all at once; the
# next tick the cut shifts by one cell down and right, and back again the
# tick after (Margolus's neighbourhood). A design picks only the cells and
# the rule, so locality is the physics, not a law.
#
# A cell is a few small numbers, its fields, each below its range: so a
# design has finitely many cells, however it reads them. The cell of all
# zeros is empty.
#
# A rule is a list of reactions. A reaction pairs two block patterns, and a
# block that fits one side turns into the other side, values moved along by
# the variables the two sides share; a block that fits no side stays as it
# is. LAWS.bend asks of the reactions that the rule undoes itself: then a
# tick is undone by the same tick, and a run is undone by running its ticks
# again in the other order, the cut shifted the other way first.
#
# The grid is a window onto an empty plane. On a shifted tick the blocks
# along the edges take in a border of empty cells, and whatever the rule
# puts in that border leaves the window: a run keeps a list of it.

import Base

type Quad<-A: Data> is Data:
  Quad{nw: A, ne: A, sw: A, se: A}

# a field in a pattern: a value, or a variable naming whatever is there
type Field is Data:
  Is{n: Nat}
  Any{v: Nat}

type Reaction is Data:
  Reaction{l: Quad<+List<Field>>, r: Quad<+List<Field>>}

type Bind is Data:
  Bind{v: Nat, n: Nat}

# Matching
# --------

def when_eq(b: Bool, m: Unit -> +Maybe<+List<Bind>>) -> +Maybe<+List<Bind>>:
  match b:
    case True{}:
      m(Unit{})
    case False{}:
      None{}

def bound(+v: Nat, s: +Maybe<+List<Bind>>, +n: Nat) -> +Maybe<+List<Bind>>:
  match s:
    case Some{bs}:
      Some{Bind{v, n} <> bs}
    case None{}:
      None{}

# a cell against a cell pattern, stopping at the first value that differs
def fits(ps: +List<Field>, cs: +List<Nat>) -> +Maybe<+List<Bind>>:
  match ps cs:
    case Nil{} Nil{}:
      Some{Nil{}}
    case Is{n} <> pt +c <> ct:
      when_eq(Nat.is_eq(n, c), u => fits(pt, ct))
    case Any{v} <> pt +c <> ct:
      bound(v, fits(pt, ct), c)
    case _ _:
      None{}

def prepend(x: +List<Bind>, m: +Maybe<+List<Bind>>) -> +Maybe<+List<Bind>>:
  match m:
    case Some{y}:
      Some{List.append(&2, Bind, x, y)}
    case None{}:
      None{}

# m's bindings and then rest's, or None at the first cell that does not fit
def and_then(m: +Maybe<+List<Bind>>, rest: Unit -> +Maybe<+List<Bind>>) -> +Maybe<+List<Bind>>:
  match m:
    case Some{x}:
      prepend(x, rest(Unit{}))
    case None{}:
      None{}

def fits_quad(p: Quad<+List<Field>>, q: Quad<+List<Nat>>) -> +Maybe<+List<Bind>>:
  match p q:
    case Quad{a, b, c, d} Quad{w, x, y, z}:
      and_then(fits(a, w), u => and_then(fits(b, x), u => and_then(fits(c, y), u => fits(d, z))))

def pick(b: Bool, +x: Nat, +y: Nat) -> Nat:
  match b:
    case True{}:
      x
    case False{}:
      y

def lookup(bs: +List<Bind>, +v: Nat) -> Nat:
  match bs:
    case Nil{}:
      0n
    case Bind{u, +n} <> t:
      pick(Nat.is_eq(u, v), n, lookup(t, v))

def fill(ps: +List<Field>, +bs: +List<Bind>) -> +List<Nat>:
  match ps:
    case Nil{}:
      Nil{}
    case Is{n} <> t:
      n <> fill(t, bs)
    case Any{v} <> t:
      lookup(bs, v) <> fill(t, bs)

def fill_quad(p: Quad<+List<Field>>, +bs: +List<Bind>) -> Quad<+List<Nat>>:
  match p:
    case Quad{a, b, c, d}:
      Quad{fill(a, bs), fill(b, bs), fill(c, bs), fill(d, bs)}

def zero(c: +List<Nat>) -> Bool:
  match c:
    case Nil{}:
      True{}
    case 0n <> t:
      zero(t)
    case _:
      False{}

def zero_quad(q: Quad<+List<Nat>>) -> Bool:
  match q:
    case Quad{a, b, c, d}:
      Bool.and(Bool.and(zero(a), zero(b)), Bool.and(zero(c), zero(d)))

def other_side(m: +Maybe<+List<Bind>>, +p: Quad<+List<Field>>, rest: Unit -> Quad<+List<Nat>>) -> Quad<+List<Nat>>:
  match m:
    case Some{bs}:
      fill_quad(p, bs)
    case None{}:
      rest(Unit{})

# the rule: the first reaction a side of which the block fits
def react.go(rs: +List<Reaction>, +q: Quad<+List<Nat>>) -> Quad<+List<Nat>>:
  match rs:
    case Nil{}:
      q
    case Reaction{+l, +r} <> t:
      other_side(fits_quad(l, q), r, u => other_side(fits_quad(r, q), l, w => react.go(t, q)))

def keep_or(b: Bool, +q: Quad<+List<Nat>>, rest: Unit -> Quad<+List<Nat>>) -> Quad<+List<Nat>>:
  match b:
    case True{}:
      q
    case False{}:
      rest(Unit{})

# The rule by the block's top-left cell: for each value of a cell's first
# field, the reactions with a side whose top-left cell could have it, in
# their order. A reaction left out of a group fits no block of that group,
# so the rule picks as it would from the whole list, only faster.

def head_is(p: Quad<+List<Field>>, +k: Nat) -> Bool:
  match p:
    case Quad{Is{n} <> t, b, c, d}:
      Nat.is_eq(n, k)
    case _:
      True{}

def may_fit(+k: Nat, x: Reaction) -> Bool:
  match x:
    case Reaction{l, r}:
      Bool.or(head_is(l, k), head_is(r, k))

def keep_r(b: Bool, +x: Reaction, rest: +List<Reaction>) -> +List<Reaction>:
  match b:
    case True{}:
      x <> rest
    case False{}:
      rest

def group(rs: +List<Reaction>, +k: Nat) -> +List<Reaction>:
  match rs:
    case Nil{}:
      Nil{}
    case +x <> t:
      keep_r(may_fit(k, x), x, group(t, k))

def index.go(n: Nat, +rs: +List<Reaction>, +k: Nat) -> +List<+List<Reaction>>:
  match n:
    case 0n:
      Nil{}
    case 1n+j:
      group(rs, k) <> index.go(j, rs, 1n+k)

# rs grouped by the first field of the block's top-left cell, which ranges
# below n
def index(+rs: +List<Reaction>, n: Nat) -> +List<+List<Reaction>>:
  index.go(n, rs, 0n)

def nth_group(gs: +List<+List<Reaction>>, n: Nat) -> +List<Reaction>:
  match gs n:
    case Nil{} _:
      Nil{}
    case +h <> t 0n:
      h
    case h <> t 1n+k:
      nth_group(t, k)

def head_of(q: Quad<+List<Nat>>) -> Nat:
  match q:
    case Quad{+n <> t, b, c, d}:
      n
    case _:
      0n

# four empty cells stay empty (LAWS.bend's void_stays), so they are skipped
def react(+idx: +List<+List<Reaction>>, +q: Quad<+List<Nat>>) -> Quad<+List<Nat>>:
  keep_or(zero_quad(q), q, u => react.go(nth_group(idx, head_of(q)), q))

# A tick
# ------

type Rows is Data:
  Rows{top: +List<+List<Nat>>, bot: +List<+List<Nat>>}

def put(q: Quad<+List<Nat>>, rest: Rows) -> Rows:
  match q rest:
    case Quad{a, b, c, d} Rows{t, u}:
      Rows{a <> b <> t, c <> d <> u}

# the blocks along two rows
def pair(+rs: +List<+List<Reaction>>, top: +List<+List<Nat>>, bot: +List<+List<Nat>>) -> Rows:
  match top bot:
    case a <> b <> t c <> d <> u:
      put(react(rs, Quad{a, b, c, d}), pair(rs, t, u))
    case _ _:
      Rows{Nil{}, Nil{}}

def unrows(r: Rows, rest: +List<+List<+List<Nat>>>) -> +List<+List<+List<Nat>>>:
  match r:
    case Rows{t, b}:
      t <> b <> rest

# every block whose corner is at an even row and column
def even(+rs: +List<+List<Reaction>>, g: +List<+List<+List<Nat>>>) -> +List<+List<+List<Nat>>>:
  match g:
    case a <> b <> t:
      unrows(pair(rs, a, b), even(rs, t))
    case _:
      Nil{}

def pick_cell(b: Bool, +x: +List<Nat>, +y: +List<Nat>) -> +List<Nat>:
  match b:
    case True{}:
      x
    case False{}:
      y

def row_of(n: Nat, +v: +List<Nat>) -> +List<+List<Nat>>:
  match n:
    case 0n:
      Nil{}
    case 1n+k:
      v <> row_of(k, v)

def last(+v: +List<Nat>, r: +List<+List<Nat>>) -> +List<+List<Nat>>:
  List.append(&2, +List<Nat>, r, [v])

def frame_rows(g: +List<+List<+List<Nat>>>, +v: +List<Nat>) -> +List<+List<+List<Nat>>>:
  match g:
    case Nil{}:
      Nil{}
    case r <> t:
      (v <> last(v, r)) <> frame_rows(t, v)

# g inside a border of empty cells v, w + 2 wide
def framed(g: +List<+List<+List<Nat>>>, +w: Nat, +v: +List<Nat>) -> +List<+List<+List<Nat>>>:
  row_of(2n+w, v) <> List.append(&2, +List<+List<Nat>>, frame_rows(g, v), [row_of(2n+w, v)])

# what leaves: a cell that is not empty in the border, where it is
type Out is Data:
  Out{x: Nat, y: Nat, c: +List<Nat>}

def out_if(b: Bool, +o: Out, rest: +List<Out>) -> +List<Out>:
  match b:
    case True{}:
      rest
    case False{}:
      o <> rest

def outs_row(r: +List<+List<Nat>>, +x: Nat, +y: Nat) -> +List<Out>:
  match r:
    case Nil{}:
      Nil{}
    case +c <> t:
      out_if(zero(c), Out{x, y, c}, outs_row(t, 1n+x, y))

def first_cell(r: +List<+List<Nat>>) -> +List<+List<Nat>>:
  match r:
    case Nil{}:
      Nil{}
    case c <> t:
      [c]

def last_cell(r: +List<+List<Nat>>) -> +List<+List<Nat>>:
  match r:
    case Nil{}:
      Nil{}
    case c <> Nil{}:
      [c]
    case c <> t:
      last_cell(t)

def inner(r: +List<+List<Nat>>) -> +List<+List<Nat>>:
  match r:
    case Nil{}:
      Nil{}
    case c <> +t:
      List.take(&2, +List<Nat>, t, Nat.sub(List.length(&2, +List<Nat>, t), 1n))

type Cut is Data:
  Cut{g: +List<+List<+List<Nat>>>, outs: +List<Out>}

def add_row(r: +List<+List<Nat>>, o: +List<Out>, c: Cut) -> Cut:
  match c:
    case Cut{g, outs}:
      Cut{r <> g, List.append(&2, Out, o, outs)}

def cut_rows(rows: +List<+List<+List<Nat>>>, +w: Nat, +y: Nat) -> Cut:
  match rows:
    case Nil{}:
      Cut{Nil{}, Nil{}}
    case +r <> Nil{}:
      Cut{Nil{}, outs_row(r, 0n, y)}
    case +r <> t:
      add_row(inner(r), List.append(&2, Out, outs_row(first_cell(r), 0n, y), outs_row(last_cell(r), 1n+w, y)), cut_rows(t, w, 1n+y))

def cut_more(o: +List<Out>, c: Cut) -> Cut:
  match c:
    case Cut{g, outs}:
      Cut{g, List.append(&2, Out, o, outs)}

# the framed grid's inside, and what the border holds now
def uncut(rows: +List<+List<+List<Nat>>>, +w: Nat) -> Cut:
  match rows:
    case Nil{}:
      Cut{Nil{}, Nil{}}
    case +r <> t:
      cut_more(outs_row(r, 0n, 0n), cut_rows(t, w, 1n))

def width(g: +List<+List<+List<Nat>>>) -> Nat:
  match g:
    case Nil{}:
      0n
    case r <> t:
      List.length(&2, +List<Nat>, r)

# one tick (rs the rule, indexed): the blocks at even corners, or (shifted) at odd ones, with the
# border taken in and handed back as what leaves
def tick(+rs: +List<+List<Reaction>>, +v: +List<Nat>, shifted: Bool, +g: +List<+List<+List<Nat>>>) -> Cut:
  match shifted:
    case False{}:
      Cut{even(rs, g), Nil{}}
    case True{}:
      uncut(even(rs, framed(g, width(g), v)), width(g))

# Laws on reactions
# -----------------

def range_of(rs: +List<Nat>, n: Nat) -> Nat:
  match rs n:
    case Nil{} _:
      0n
    case +h <> t 0n:
      h
    case h <> t 1n+k:
      range_of(t, k)

# every value below its field's range, and as many fields as the design has
def cell_ok(ps: +List<Field>, rs: +List<Nat>) -> Bool:
  match ps rs:
    case Nil{} Nil{}:
      True{}
    case Is{n} <> pt +r <> rt:
      Bool.and(Nat.is_lt(n, r), cell_ok(pt, rt))
    case Any{v} <> pt r <> rt:
      cell_ok(pt, rt)
    case _ _:
      False{}

def quad_ok(p: Quad<+List<Field>>, +rs: +List<Nat>) -> Bool:
  match p:
    case Quad{a, b, c, d}:
      Bool.and(Bool.and(cell_ok(a, rs), cell_ok(b, rs)), Bool.and(cell_ok(c, rs), cell_ok(d, rs)))

def flat(p: Quad<+List<Field>>) -> +List<Field>:
  match p:
    case Quad{a, b, c, d}:
      List.append(&2, Field, List.append(&2, Field, a, b), List.append(&2, Field, c, d))

def flat_cells(q: Quad<+List<Nat>>) -> +List<Nat>:
  match q:
    case Quad{a, b, c, d}:
      List.append(&2, Nat, List.append(&2, Nat, a, b), List.append(&2, Nat, c, d))

type VarAt is Data:
  VarAt{v: Nat, at: Nat}

# a side's variables, each with the place it sits in
def vars_at(ps: +List<Field>, +i: Nat) -> +List<VarAt>:
  match ps:
    case Nil{}:
      Nil{}
    case Is{n} <> t:
      vars_at(t, 1n+i)
    case Any{v} <> t:
      VarAt{v, i} <> vars_at(t, 1n+i)

def count_var(xs: +List<VarAt>, +v: Nat) -> Nat:
  match xs:
    case Nil{}:
      0n
    case VarAt{u, at} <> t:
      Nat.add(pick(Nat.is_eq(u, v), 1n, 0n), count_var(t, v))

def place_of(xs: +List<VarAt>, +v: Nat) -> Nat:
  match xs:
    case Nil{}:
      0n
    case VarAt{u, +at} <> t:
      pick(Nat.is_eq(u, v), at, place_of(t, v))

def once_each(xs: +List<VarAt>, +ls: +List<VarAt>, +rs: +List<VarAt>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case VarAt{+v, at} <> t:
      Bool.and(Bool.and(Nat.is_eq(count_var(ls, v), 1n), Nat.is_eq(count_var(rs, v), 1n)), once_each(t, ls, rs))

def same_ranges(xs: +List<VarAt>, +ls: +List<VarAt>, +rs: +List<VarAt>, +ranges: +List<Nat>, +k: Nat) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case VarAt{+v, at} <> t:
      Bool.and(Nat.is_eq(range_of(ranges, Nat.mod(place_of(ls, v), k)), range_of(ranges, Nat.mod(place_of(rs, v), k))), same_ranges(t, ls, rs, ranges, k))

def reaction_fits(x: Reaction, +ranges: +List<Nat>) -> Bool:
  match x:
    case Reaction{+l, +r}:
      +k = List.length(&2, Nat, ranges)
      +ls = vars_at(flat(l), 0n)
      +rs = vars_at(flat(r), 0n)
      Bool.and(Bool.and(quad_ok(l, ranges), quad_ok(r, ranges)), same_ranges(ls, ls, rs, ranges, k))

# each cell of each side has the design's fields, every value below its
# field's range, and a variable moves only between fields of the same range
def all_fit(xs: +List<Reaction>, +ranges: +List<Nat>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case x <> t:
      Bool.and(reaction_fits(x, ranges), all_fit(t, ranges))

def reaction_linear(x: Reaction) -> Bool:
  match x:
    case Reaction{l, r}:
      +ls = vars_at(flat(l), 0n)
      +rs = vars_at(flat(r), 0n)
      Bool.and(Nat.is_eq(List.length(&2, VarAt, ls), List.length(&2, VarAt, rs)), Bool.and(once_each(ls, ls, rs), once_each(rs, ls, rs)))

# each variable of a reaction once on each side, so no value is copied or
# dropped unseen
def all_linear(xs: +List<Reaction>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case x <> t:
      Bool.and(reaction_linear(x), all_linear(t))

def is_none(m: +Maybe<+List<Bind>>) -> Bool:
  match m:
    case None{}:
      True{}
    case Some{x}:
      False{}

# no side fits four empty cells
def no_void(xs: +List<Reaction>, +v: +List<Nat>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case Reaction{+l, +r} <> t:
      Bool.and(Bool.and(is_none(fits_quad(l, Quad{v, v, v, v})), is_none(fits_quad(r, Quad{v, v, v, v}))), no_void(t, v))

# One partner: two sides that fit a block in common turn it into the same
# block. Both sides of a pair are laid flat, field by field; the block they
# share has, at each place, the value either side names there, or a free
# value if both name a variable. The two other sides are then compared on
# that block, a variable standing for its place.

type Val is Data:
  Lit{n: Nat}
  Free{at: Nat}

def Val.is_eq(a: Val, b: Val) -> Bool:
  match a b:
    case Lit{x} Lit{y}:
      Nat.is_eq(x, y)
    case Free{x} Free{y}:
      Nat.is_eq(x, y)
    case _ _:
      False{}

def meet_if(b: Bool, +x: Val, m: +Maybe<+List<Val>>) -> +Maybe<+List<Val>>:
  match b m:
    case True{} Some{xs}:
      Some{x <> xs}
    case _ _:
      None{}

# the shared block, if the two flat sides can meet
def meet(a: +List<Field>, b: +List<Field>, +i: Nat) -> +Maybe<+List<Val>>:
  match a b:
    case Nil{} Nil{}:
      Some{Nil{}}
    case Is{+x} <> s Is{y} <> t:
      meet_if(Nat.is_eq(x, y), Lit{x}, meet(s, t, 1n+i))
    case Is{x} <> s Any{y} <> t:
      meet_if(True{}, Lit{x}, meet(s, t, 1n+i))
    case Any{x} <> s Is{y} <> t:
      meet_if(True{}, Lit{y}, meet(s, t, 1n+i))
    case Any{x} <> s Any{y} <> t:
      meet_if(True{}, Free{i}, meet(s, t, 1n+i))
    case _ _:
      None{}

def nth_val(xs: +List<Val>, n: Nat) -> Val:
  match xs n:
    case Nil{} _:
      Lit{0n}
    case +h <> t 0n:
      h
    case h <> t 1n+k:
      nth_val(t, k)

# a side's partner on the shared block: each variable becomes what the
# shared block holds where the side's own pattern has it
def partner(ts: +List<Field>, +vs: +List<VarAt>, +shared: +List<Val>) -> +List<Val>:
  match ts:
    case Nil{}:
      Nil{}
    case Is{n} <> t:
      Lit{n} <> partner(t, vs, shared)
    case Any{v} <> t:
      nth_val(shared, place_of(vs, v)) <> partner(t, vs, shared)

def vals_eq(a: +List<Val>, b: +List<Val>) -> Bool:
  match a b:
    case Nil{} Nil{}:
      True{}
    case x <> s y <> t:
      Bool.and(Val.is_eq(x, y), vals_eq(s, t))
    case _ _:
      False{}

def agree_on(m: +Maybe<+List<Val>>, +s1: +List<Field>, +t1: +List<Field>, +s2: +List<Field>, +t2: +List<Field>) -> Bool:
  match m:
    case None{}:
      True{}
    case Some{+shared}:
      vals_eq(partner(t1, vars_at(s1, 0n), shared), partner(t2, vars_at(s2, 0n), shared))

# a side of a reaction, which partner it turns into, and which entry it is
type Side is Data:
  Side{s: +List<Field>, t: +List<Field>, id: Nat}

def sides(xs: +List<Reaction>, +i: Nat) -> +List<Side>:
  match xs:
    case Nil{}:
      Nil{}
    case Reaction{l, r} <> t:
      +fl = flat(l)
      +fr = flat(r)
      Side{fl, fr, i} <> Side{fr, fl, 1n+i} <> sides(t, 2n+i)

def pair_ok(a: Side, b: Side) -> Bool:
  match a b:
    case Side{+s1, +t1, i} Side{+s2, +t2, j}:
      Bool.or(Nat.is_eq(i, j), agree_on(meet(s1, s2, 0n), s1, t1, s2, t2))

def side_ok(+a: Side, bs: +List<Side>) -> Bool:
  match bs:
    case Nil{}:
      True{}
    case b <> t:
      Bool.and(pair_ok(a, b), side_ok(a, t))

def sides_ok(xs: +List<Side>, +all: +List<Side>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case a <> t:
      Bool.and(side_ok(a, all), sides_ok(t, all))

def one_partner(+xs: +List<Reaction>) -> Bool:
  +ss = sides(xs, 0n)
  sides_ok(ss, ss)

# Why these make the rule undo itself: say a block q fits side s of some
# reaction and turns into q', the partner t filled with what s's variables
# matched. Each variable is once on each side, so q' fits t, matching each
# variable to the same value, and t's partner s, so filled, is q. Any other
# side q' fits gives q' the same partner (one_partner), so the rule turns
# q' back into q. A block that fits no side stays put.

# What a design lays out: its grid, and the column the tree's top is in (a
# program's cells go in above it, and anything a run sends back leaves there)
type Layout is Data:
  Layout{g: +List<+List<+List<Nat>>>, port: Nat}
