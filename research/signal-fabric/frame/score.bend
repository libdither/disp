# The design's score: the ticks until the grid is still, for a program that
# mirrors a whole tree (every fork swaps its two parts), on full trees of
# 2 to 32 leaves. Every swap is a packet of its own, and every packet goes
# in at the top one symbol a tick, so the time grows with the program's
# length, about n log n symbols for n leaves: a design that sent one map
# down and copied it at each fork would not pay that.
#
# Then add, in stems and in binary (binary.ts's addb, ripple carry), on
# numbers whose carry goes all the way up: 2^n - 1 + 1 in n bits.
#
#   check.sh runs it (compiled to JS: bun …/bend2/main.ts score.bend -o score.js)

import Base
import ./twoway.bend as TW
import ./loopnet.bend as Design
import ./binary.bend as Bin

# a full tree of d levels of forks
def full(d: Nat) -> TW.Tree:
  match d:
    case 0n:
      TW.L{}
    case 1n++k:
      TW.F{full(k), full(k)}

# mirror unrolled d levels, with no fix: map(id, m, m) ; swap
def mirror(d: Nat) -> TW.Tree:
  match d:
    case 0n:
      TW.L{}
    case 1n++k:
      TW.p_seq(TW.p_map(TW.L{}, mirror(k), mirror(k)), TW.p_swap())

def symbols.go(m: +Maybe<+List<+List<Design.Sym>>>) -> Nat:
  match m:
    case Some{ps}:
      List.length(&2, Design.Sym, Design.symbols(ps))
    case None{}:
      0n

def symbols(+p: TW.Tree) -> Nat:
  symbols.go(Design.packets(p, True{}))

def symbols.list.go(m: +Maybe<+List<+List<Design.Sym>>>) -> +List<Design.Sym>:
  match m:
    case Some{ps}:
      Design.symbols(ps)
    case None{}:
      Nil{}

def symbols.list(+p: TW.Tree) -> +List<Design.Sym>:
  symbols.list.go(Design.packets(p, True{}))

def line(+d: Nat) -> String:
  String.concat([Nat.show(Nat.pow(2n, d)), " leaves: ", Nat.show(symbols(mirror(d))), " symbols, ", Nat.show(Design.ticks(Design.settle(full(d), symbols.list(mirror(d))))), " ticks"])

# n counted in stems
def nat(n: Nat) -> TW.Tree:
  match n:
    case 0n:
      TW.L{}
    case 1n+k:
      TW.S{nat(k)}

# add: fix(tri(id, self ; second(stem), id))
def p_add() -> TW.Tree:
  TW.S{TW.F{TW.L{}, TW.F{TW.F{TW.L{}, TW.p_seq(TW.S{TW.L{}}, TW.p_second(TW.S{TW.S{TW.S{TW.S{TW.S{TW.L{}}}}}}))}, TW.L{}}}}

def ticks(+p: TW.Tree, +v: TW.Tree) -> Nat:
  Design.ticks(Design.settle(v, symbols.list(p)))

# add on (n, n), and add† on (n, 2n)
def add_line(+n: Nat) -> String:
  String.concat(["add ", Nat.show(n), " + ", Nat.show(n), ": ", Nat.show(ticks(p_add(), TW.F{nat(n), nat(n)})), " ticks; add† back: ", Nat.show(ticks(TW.p_dag(p_add()), TW.F{nat(n), nat(Nat.double(n))})), " ticks"])

# the library's mirror, fix(map(self, self, self) ; swap): one symbol, copied at each fork
def p_mirror() -> TW.Tree:
  TW.S{TW.F{TW.L{}, TW.p_seq(TW.p_map(TW.S{TW.L{}}, TW.S{TW.L{}}, TW.S{TW.L{}}), TW.p_swap())}}

def mirror_line(+d: Nat) -> String:
  String.concat([Nat.show(Nat.pow(2n, d)), " leaves, mirror copied at forks: ", Nat.show(symbols(p_mirror())), " symbol, ", Nat.show(ticks(p_mirror(), full(d))), " ticks"])

def bit(m: Nat) -> TW.Tree:
  match m:
    case 0n:
      TW.L{}
    case 1n+k:
      TW.S{TW.L{}}

# a and b in n bits, lowest first, a position (carry slot, a's bit, b's bit)
def register(n: Nat, +a: Nat, +b: Nat) -> TW.Tree:
  match n:
    case 0n:
      TW.L{}
    case 1n+k:
      TW.F{TW.L{}, TW.F{bit(Nat.mod(a, 2n)), TW.F{bit(Nat.mod(b, 2n)), register(k, Nat.div(a, 2n), Nat.div(b, 2n))}}}

def binary_line(+n: Nat) -> String:
  +a = Nat.sub(Nat.pow(2n, n), 1n)
  String.concat(["binary add ", Nat.show(a), " + 1 in ", Nat.show(n), " bits: ", Nat.show(ticks(Bin.p_addb(), register(n, a, 1n))), " ticks"])

def unary_line(+n: Nat) -> String:
  +a = Nat.sub(Nat.pow(2n, n), 1n)
  String.concat(["add in stems, ", Nat.show(a), " + 1: ", Nat.show(ticks(p_add(), TW.F{nat(a), nat(1n)})), " ticks"])

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- IO.print(line(1n))
    Unit <- IO.print(line(2n))
    Unit <- IO.print(line(3n))
    Unit <- IO.print(line(4n))
    Unit <- IO.print(line(5n))
    Unit <- IO.print(mirror_line(1n))
    Unit <- IO.print(mirror_line(3n))
    Unit <- IO.print(mirror_line(5n))
    Unit <- IO.print(add_line(1n))
    Unit <- IO.print(add_line(2n))
    Unit <- IO.print(add_line(4n))
    Unit <- IO.print(add_line(8n))
    Unit <- IO.print(add_line(16n))
    Unit <- IO.print(binary_line(1n))
    Unit <- IO.print(binary_line(2n))
    Unit <- IO.print(binary_line(4n))
    Unit <- IO.print(binary_line(8n))
    Unit <- IO.print(unary_line(4n))
    IO.print(unary_line(8n))
