# The two-way calculus, as this spec defines it: the meaning a block design's
# runs are held to. It stands on its own (no other implementation is
# consulted). A value and a program are both trees. As a program a tree reads
# as:
#
#   △            id             (△, b)        b†, b run backward
#   (△a, b)      a ; b          ((a, b), c)   tri(a, b, c)
#   △(△, b)      fix(b)         △((a, b), c)  map(a, b, c)
#   △△ … △       the moves, by the number of stems on the leaf: 1 self,
#                2 swap, 3 cross, 4 sink, 5 stem, 6 unit (7 and up are stuck)
#
# `run` takes fuel, a call deep: out of fuel and stuck both answer None.

import Base

type Tree is Data:
  L{}
  S{c: Tree}
  F{l: Tree, r: Tree}

def Tree.is_eq(a: Tree, b: Tree) -> Bool:
  match a b:
    case L{} L{}:
      True{}
    case S{x} S{y}:
      Tree.is_eq(x, y)
    case F{x, y} F{u, v}:
      Bool.and(Tree.is_eq(x, u), Tree.is_eq(y, v))
    case _ _:
      False{}

def Tree.is_eq_maybe(a: +Maybe<Tree>, b: +Maybe<Tree>) -> Bool:
  match a b:
    case None{} None{}:
      True{}
    case Some{x} Some{y}:
      Tree.is_eq(x, y)
    case _ _:
      False{}

# Moves
# -----

def swap(x: Tree) -> Tree:
  match x:
    case F{l, r}:
      F{r, l}
    case y:
      y

def cross(x: Tree) -> Tree:
  match x:
    case F{F{a, b}, c}:
      F{F{a, c}, b}
    case y:
      y

# sink and its inverse in one: △(a, b) ⇄ (△a, b)
def sink(x: Tree) -> Tree:
  match x:
    case S{F{a, b}}:
      F{S{a}, b}
    case F{S{a}, b}:
      S{F{a, b}}
    case y:
      y

def stem(x: Tree, fwd: Bool) -> +Maybe<Tree>:
  match x fwd:
    case y True{}:
      Some{S{y}}
    case S{c} False{}:
      Some{c}
    case _ False{}:
      None{}

def unit(x: Tree, fwd: Bool) -> +Maybe<Tree>:
  match x fwd:
    case y True{}:
      Some{F{L{}, y}}
    case F{L{}, r} False{}:
      Some{r}
    case _ False{}:
      None{}

# Running
# -------

def bind(y: +Maybe<Tree>, f: Tree -> +Maybe<Tree>) -> +Maybe<Tree>:
  match y:
    case Some{z}:
      f(z)
    case None{}:
      None{}

def then_stem(y: +Maybe<Tree>) -> +Maybe<Tree>:
  match y:
    case Some{z}:
      Some{S{z}}
    case None{}:
      None{}

def then_fork(y: +Maybe<Tree>, z: +Maybe<Tree>) -> +Maybe<Tree>:
  match y z:
    case Some{u} Some{v}:
      Some{F{u, v}}
    case _ _:
      None{}

# tri's stem back on: (s, r) -> (△s, r)
def then_lower(y: +Maybe<Tree>) -> +Maybe<Tree>:
  match y:
    case Some{F{l, r}}:
      Some{F{S{l}, r}}
    case _:
      None{}

# tri's rotation back: (l, (u, v)) -> ((l, u), v)
def then_unrotate(y: +Maybe<Tree>) -> +Maybe<Tree>:
  match y:
    case Some{F{l, F{u, v}}}:
      Some{F{F{l, u}, v}}
    case _:
      None{}

# p run on x, forward or backward; me is the innermost fix around it. tri
# reads the left part: a leaf runs a on the right; a stem comes off, b runs,
# and it goes back on; a fork rotates, c runs, and it rotates back.
def run(fuel: Nat, p: Tree, x: Tree, +fwd: Bool, +me: +Maybe<Tree>) -> +Maybe<Tree>:
  match fuel:
    case 0n:
      None{}
    case 1n++k:
      match p:
        case L{}:
          Some{x}
        case F{L{}, b}:
          run(k, b, x, Bool.not(fwd), me)
        case F{S{a}, b}:
          match fwd:
            case True{}:
              bind(run(k, a, x, True{}, me), y => run(k, b, y, True{}, me))
            case False{}:
              bind(run(k, b, x, False{}, me), y => run(k, a, y, False{}, me))
        case F{F{a, b}, c}:
          match x:
            case F{L{}, r}:
              then_fork(Some{L{}}, run(k, a, r, fwd, me))
            case F{S{s}, r}:
              then_lower(run(k, b, F{s, r}, fwd, me))
            case F{F{u, v}, r}:
              then_unrotate(run(k, c, F{u, F{v, r}}, fwd, me))
            case y:
              Some{y}
        case S{F{L{}, +b}}:
          run(k, b, x, fwd, Some{S{F{L{}, b}}})
        case S{F{F{a, b}, c}}:
          match x:
            case S{y}:
              then_stem(run(k, a, y, fwd, me))
            case F{l, r}:
              then_fork(run(k, b, l, fwd, me), run(k, c, r, fwd, me))
            case L{}:
              Some{L{}}
        case S{F{S{_}, _}}:
          None{}
        case S{L{}}:
          match me:
            case Some{S{F{L{}, +b}}}:
              run(k, b, x, fwd, Some{S{F{L{}, b}}})
            case _:
              None{}
        case S{S{L{}}}:
          Some{swap(x)}
        case S{S{S{L{}}}}:
          Some{cross(x)}
        case S{S{S{S{L{}}}}}:
          Some{sink(x)}
        case S{S{S{S{S{L{}}}}}}:
          stem(x, fwd)
        case S{S{S{S{S{S{L{}}}}}}}:
          unit(x, fwd)
        case _:
          None{}

# Programs by name
# ----------------

def p_swap() -> Tree:
  S{S{L{}}}

def p_cross() -> Tree:
  S{S{S{L{}}}}

def p_sink() -> Tree:
  S{S{S{S{L{}}}}}

def p_stem() -> Tree:
  S{S{S{S{S{L{}}}}}}

def p_unit() -> Tree:
  S{S{S{S{S{S{L{}}}}}}}

def p_dag(b: Tree) -> Tree:
  F{L{}, b}

def p_seq(a: Tree, b: Tree) -> Tree:
  F{S{a}, b}

def p_map(a: Tree, b: Tree, c: Tree) -> Tree:
  S{F{F{a, b}, c}}

def p_first(f: Tree) -> Tree:
  p_map(L{}, f, L{})

def p_second(f: Tree) -> Tree:
  p_map(L{}, L{}, f)


# Two ways
# --------

def undoes.go(+p: Tree, +x: Tree, y: +Maybe<Tree>) -> Bool:
  match y:
    case None{}:
      True{}
    case Some{z}:
      Tree.is_eq_maybe(run(256n, p, z, False{}, None{}), Some{x})

# run backward on what came out forward gives x back, wherever forward gives anything
def undoes(+p: Tree, +x: Tree) -> Bool:
  undoes.go(p, x, run(256n, p, x, True{}, None{}))

def undoes_on(+p: Tree, xs: +List<Tree>) -> Bool:
  match xs:
    case Nil{}:
      True{}
    case x <> t:
      Bool.and(undoes(p, x), undoes_on(p, t))

def undoes_all(ps: +List<Tree>, +xs: +List<Tree>) -> Bool:
  match ps:
    case Nil{}:
      True{}
    case p <> t:
      Bool.and(undoes_on(p, xs), undoes_all(t, xs))

# A step of moves
# ---------------

def size(t: Tree) -> Nat:
  match t:
    case L{}:
      1n
    case S{c}:
      1n+size(c)
    case F{l, r}:
      1n+Nat.add(size(l), size(r))

def unstem(t: Tree) -> +List<Tree>:
  match t:
    case S{c}:
      [c]
    case _:
      Nil{}

def ununit(t: Tree) -> +List<Tree>:
  match t:
    case F{L{}, r}:
      [r]
    case _:
      Nil{}

# t, and t after one move at its top, either way
def tops(+t: Tree) -> +List<Tree>:
  t <> swap(t) <> cross(t) <> sink(t) <> S{t} <> F{L{}, t} <> List.append(&2, Tree, unstem(t), ununit(t))

def head(u: Tree, c: Tree, fs: Tree -> Tree -> Bool, ff: Tree -> Tree -> Tree -> Tree -> Bool) -> Bool:
  match u c:
    case L{} L{}:
      True{}
    case S{d} S{x}:
      fs(d)(x)
    case F{a, b} F{x, y}:
      ff(a)(b)(x)(y)
    case _ _:
      False{}

# u is one of cs after a step of moves inside its parts (fuel bounds the search)
def mv(fuel: Nat, +u: Tree, cs: +List<Tree>) -> Bool:
  match fuel:
    case 0n:
      False{}
    case 1n++k:
      match cs:
        case Nil{}:
          False{}
        case c <> rest:
          Bool.or(head(u, c, d => x => mv(k, d, tops(x)), a => b => x => y => Bool.and(mv(k, a, tops(x)), mv(k, b, tops(y)))), mv(k, u, rest))

# u is t after a step of moves: one move (swap, cross, sink, stem or unit,
# either way) or none at t's top, then a step of moves in each part of what
# that gives, all at once
def moved(+u: Tree, +t: Tree) -> Bool:
  mv(1n+Nat.mul(16n, size(u)), u, tops(t))

# Small trees and programs
# ------------------------

def stems(ts: +List<Tree>) -> +List<Tree>:
  match ts:
    case Nil{}:
      Nil{}
    case t <> rest:
      S{t} <> stems(rest)

def keep_if(b: Bool, +t: Tree, rest: +List<Tree>) -> +List<Tree>:
  match b:
    case True{}:
      t <> rest
    case False{}:
      rest

def forks_with(+a: Tree, bs: +List<Tree>, +k: Nat) -> +List<Tree>:
  match bs:
    case Nil{}:
      Nil{}
    case +b <> rest:
      keep_if(Nat.is_le(Nat.add(size(a), size(b)), k), F{a, b}, forks_with(a, rest, k))

def forks(xs: +List<Tree>, +bs: +List<Tree>, +k: Nat) -> +List<Tree>:
  match xs:
    case Nil{}:
      Nil{}
    case a <> rest:
      List.append(&2, Tree, forks_with(a, bs, k), forks(rest, bs, k))

# every tree of at most n nodes
def trees(n: Nat) -> +List<Tree>:
  match n:
    case 0n:
      Nil{}
    case 1n++k:
      +ts = trees(k)
      L{} <> List.append(&2, Tree, stems(ts), forks(ts, ts, k))
