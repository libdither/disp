# The physics every design shares: a grid of cells, all updated at once,
# each from itself and the four cells beside it by one rule. A design picks
# the cell type C, the rule, and the cell that stands beyond the edge; this
# file is the only place a grid changes, so a design cannot see further than
# one cell or keep anything outside its cells. The rule's type, Nb<C> -> C,
# is the locality law: no proof needed.

import Base

# a cell and the four beside it: north, east, south, west
type Nb<-C: Data> is Data:
  Nb{c: C, n: C, e: C, s: C, w: C}

def center(-C: Data, nb: Nb<C>) -> C:
  match nb:
    case Nb{c, n, e, s, w}:
      c

def first(~C: Data, xs: +List<C>, d: C) -> C:
  match xs:
    case Nil{}:
      d
    case h <> t:
      h

def first_row(~C: Data, rs: +List<+List<C>>, d: +List<C>) -> +List<C>:
  match rs:
    case Nil{}:
      d
    case h <> t:
      h

# a row of edge cells as long as r
def blank(~C: Data, ~edge: C, r: +List<C>) -> +List<C>:
  match r:
    case Nil{}:
      Nil{}
    case h <> t:
      edge <> blank(~C, ~edge, t)

# one row of the next grid, from the rows above and below it; w is the cell
# west of the first
def row(~C: Data, ~rule: Nb<C> -> C, ~edge: C, up: +List<C>, here: +List<C>, down: +List<C>, w: C) -> +List<C>:
  match up here down:
    case u <> us +h <> +hs d <> ds:
      rule(Nb{h, u, first(~C, hs, edge), d, w}) <> row(~C, ~rule, ~edge, us, hs, ds, h)
    case _ _ _:
      Nil{}

def rows(~C: Data, ~rule: Nb<C> -> C, ~edge: C, g: +List<+List<C>>, up: +List<C>) -> +List<+List<C>>:
  match g:
    case Nil{}:
      Nil{}
    case +r <> +rs:
      row(~C, ~rule, ~edge, up, r, first_row(~C, rs, blank(~C, ~edge, r)), edge) <> rows(~C, ~rule, ~edge, rs, r)

# one tick: every cell at once
def step(~C: Data, ~rule: Nb<C> -> C, ~edge: C, +g: +List<+List<C>>) -> +List<+List<C>>:
  rows(~C, ~rule, ~edge, g, blank(~C, ~edge, first_row(~C, g, Nil{})))

# n ticks
def steps(~C: Data, ~rule: Nb<C> -> C, ~edge: C, n: Nat, g: +List<+List<C>>) -> +List<+List<C>>:
  match n:
    case 0n:
      g
    case 1n+k:
      steps(~C, ~rule, ~edge, k, step(~C, ~rule, ~edge, g))

def still_row(~C: Data, ~still: C -> Bool, r: +List<C>) -> Bool:
  match r:
    case Nil{}:
      True{}
    case h <> t:
      Bool.and(still(h), still_row(~C, ~still, t))

def still_grid(~C: Data, ~still: C -> Bool, g: +List<+List<C>>) -> Bool:
  match g:
    case Nil{}:
      True{}
    case r <> rs:
      Bool.and(still_row(~C, ~still, r), still_grid(~C, ~still, rs))

def choose(~A: Data, b: Bool, x: Unit -> A, y: Unit -> A) -> A:
  match b:
    case True{}:
      x(Unit{})
    case False{}:
      y(Unit{})

# a grid that came to rest, and the tick it did
type Settled<-C: Data> is Data:
  Settled{t: Nat, g: +List<+List<C>>}

# the ticks until every cell is still (giving up n ticks after t), and the grid then
def settle(~C: Data, ~rule: Nb<C> -> C, ~edge: C, ~still: C -> Bool, n: Nat, +t: Nat, +g: +List<+List<C>>) -> Settled<C>:
  match n:
    case 0n:
      Settled{t, g}
    case 1n+k:
      choose(~Settled<C>, still_grid(~C, ~still, g), u => Settled{t, g}, u => settle(~C, ~rule, ~edge, ~still, k, 1n+t, step(~C, ~rule, ~edge, g)))
