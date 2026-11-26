import Statement

/-! The quote-and-table net and its kit as data, written by `../ideal/kexport.py`; checked and
priced in `RecordQuote.lean`. -/

namespace QuoteRec
open Statement

/-- The 14 kinds: code (lam app var rho tab), running (O apply, A open-apply, T thaw, C a table
reading), resources (sig sharer, dupI/dupO copiers, eraI/eraO erasers). -/
inductive Q
  | lam | app | var | rho | tab | O | A | T | C | sig | dupI | dupO | eraI | eraO
  deriving DecidableEq, Repr

def Q.all : List Q := [.lam, .app, .var, .rho, .tab, .O, .A, .T, .C, .sig, .dupI, .dupO, .eraI, .eraO]

/-- Port directions, principal first. -/
def Q.ports : Q → List Pol
  | .lam => [.o, .o, .i]
  | .app => [.o, .i, .i]
  | .var => [.o, .i]
  | .rho => [.o]
  | .tab => [.o, .i, .i, .i, .i, .i]
  | .O => [.i, .i, .o]
  | .A => [.i, .i, .o]
  | .T => [.i, .o]
  | .C => [.i, .i, .i, .i, .i, .i, .o]
  | .sig => [.i, .o, .o]
  | .dupI => [.i, .o, .o]
  | .dupO => [.o, .i, .i]
  | .eraI => [.i]
  | .eraO => [.o]

def qsig : Sig Q := ⟨Q.ports⟩

/-- The 35 rules (`ktable.py`). -/
def qtable : List (GRule Q) := [
  ⟨.O, .lam, [.T], [(.left 1, .right 1), (.new 0 0, .right 2), (.new 0 1, .left 2)]⟩,
  ⟨.O, .var, [.A], [(.new 0 0, .right 1), (.new 0 1, .left 1), (.new 0 2, .left 2)]⟩,
  ⟨.A, .lam, [], [(.left 1, .right 1), (.left 2, .right 2)]⟩,
  ⟨.O, .tab, [.C, .T], [(.new 0 0, .left 1), (.new 0 1, .right 1), (.new 0 2, .right 2), (.new 0 3, .right 3), (.new 0 4, .right 4), (.new 0 5, .right 5), (.new 0 6, .new 1 0), (.new 1 1, .left 2)]⟩,
  ⟨.A, .tab, [.C], [(.new 0 0, .left 1), (.new 0 1, .right 1), (.new 0 2, .right 2), (.new 0 3, .right 3), (.new 0 4, .right 4), (.new 0 5, .right 5), (.new 0 6, .left 2)]⟩,
  ⟨.C, .lam, [.dupI, .tab, .lam, .var, .var, .app, .var, .app], [(.new 0 0, .left 1), (.new 0 1, .new 1 1), (.new 1 2, .left 2), (.new 1 3, .left 3), (.new 1 4, .left 4), (.new 1 5, .left 5), (.new 2 1, .right 1), (.new 2 2, .right 2), (.new 3 1, .new 0 2), (.new 4 1, .new 1 0), (.new 5 1, .new 3 0), (.new 5 2, .new 4 0), (.new 6 1, .new 2 0), (.new 7 1, .new 5 0), (.new 7 2, .new 6 0), (.new 7 0, .left 6)]⟩,
  ⟨.C, .app, [.dupI, .tab, .var, .var, .app, .var, .app, .var, .app], [(.new 0 0, .left 2), (.new 0 1, .new 1 2), (.new 1 1, .left 1), (.new 1 3, .left 3), (.new 1 4, .left 4), (.new 1 5, .left 5), (.new 2 1, .new 0 2), (.new 3 1, .new 1 0), (.new 4 1, .new 2 0), (.new 4 2, .new 3 0), (.new 5 1, .right 1), (.new 6 1, .new 4 0), (.new 6 2, .new 5 0), (.new 7 1, .right 2), (.new 8 1, .new 6 0), (.new 8 2, .new 7 0), (.new 8 0, .left 6)]⟩,
  ⟨.C, .var, [.dupI, .tab, .var, .var, .app, .var, .app], [(.new 0 0, .left 3), (.new 0 1, .new 1 3), (.new 1 1, .left 1), (.new 1 2, .left 2), (.new 1 4, .left 4), (.new 1 5, .left 5), (.new 2 1, .new 0 2), (.new 3 1, .new 1 0), (.new 4 1, .new 2 0), (.new 4 2, .new 3 0), (.new 5 1, .right 1), (.new 6 1, .new 4 0), (.new 6 2, .new 5 0), (.new 6 0, .left 6)]⟩,
  ⟨.C, .rho, [.dupI, .tab, .var, .var, .app], [(.new 0 0, .left 4), (.new 0 1, .new 1 4), (.new 1 1, .left 1), (.new 1 2, .left 2), (.new 1 3, .left 3), (.new 1 5, .left 5), (.new 2 1, .new 0 2), (.new 3 1, .new 1 0), (.new 4 1, .new 2 0), (.new 4 2, .new 3 0), (.new 4 0, .left 6)]⟩,
  ⟨.C, .tab, [.dupI, .tab, .var, .var, .app, .var, .app, .var, .app, .var, .app, .var, .app, .var, .app], [(.new 0 0, .left 5), (.new 0 1, .new 1 5), (.new 1 1, .left 1), (.new 1 2, .left 2), (.new 1 3, .left 3), (.new 1 4, .left 4), (.new 2 1, .new 0 2), (.new 3 1, .new 1 0), (.new 4 1, .new 2 0), (.new 4 2, .new 3 0), (.new 5 1, .right 1), (.new 6 1, .new 4 0), (.new 6 2, .new 5 0), (.new 7 1, .right 2), (.new 8 1, .new 6 0), (.new 8 2, .new 7 0), (.new 9 1, .right 3), (.new 10 1, .new 8 0), (.new 10 2, .new 9 0), (.new 11 1, .right 4), (.new 12 1, .new 10 0), (.new 12 2, .new 11 0), (.new 13 1, .right 5), (.new 14 1, .new 12 0), (.new 14 2, .new 13 0), (.new 14 0, .left 6)]⟩,
  ⟨.T, .app, [.O, .T, .T], [(.new 1 0, .right 1), (.new 1 1, .new 0 0), (.new 2 0, .right 2), (.new 2 1, .new 0 1), (.new 0 2, .left 1)]⟩,
  ⟨.T, .var, [], [(.left 1, .right 1)]⟩,
  ⟨.T, .lam, [.lam], [(.new 0 0, .left 1), (.new 0 1, .right 1), (.new 0 2, .right 2)]⟩,
  ⟨.T, .rho, [.rho], [(.new 0 0, .left 1)]⟩,
  ⟨.T, .tab, [.tab, .T, .T, .T, .T, .T], [(.new 0 0, .left 1), (.new 1 0, .right 1), (.new 1 1, .new 0 1), (.new 2 0, .right 2), (.new 2 1, .new 0 2), (.new 3 0, .right 3), (.new 3 1, .new 0 3), (.new 4 0, .right 4), (.new 4 1, .new 0 4), (.new 5 0, .right 5), (.new 5 1, .new 0 5)]⟩,
  ⟨.sig, .lam, [.lam, .lam, .dupO, .dupI], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1), (.new 3 0, .right 2), (.new 3 1, .new 0 2), (.new 3 2, .new 1 2)]⟩,
  ⟨.sig, .app, [.app, .app, .dupI, .dupI], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1), (.new 3 0, .right 2), (.new 3 1, .new 0 2), (.new 3 2, .new 1 2)]⟩,
  ⟨.sig, .var, [.var, .var, .dupI], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1)]⟩,
  ⟨.sig, .rho, [.rho, .rho], [(.new 0 0, .left 1), (.new 1 0, .left 2)]⟩,
  ⟨.sig, .tab, [.tab, .tab, .dupI, .dupI, .dupI, .dupI, .dupI], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1), (.new 3 0, .right 2), (.new 3 1, .new 0 2), (.new 3 2, .new 1 2), (.new 4 0, .right 3), (.new 4 1, .new 0 3), (.new 4 2, .new 1 3), (.new 5 0, .right 4), (.new 5 1, .new 0 4), (.new 5 2, .new 1 4), (.new 6 0, .right 5), (.new 6 1, .new 0 5), (.new 6 2, .new 1 5)]⟩,
  ⟨.dupI, .lam, [.lam, .lam, .dupO, .dupI], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1), (.new 3 0, .right 2), (.new 3 1, .new 0 2), (.new 3 2, .new 1 2)]⟩,
  ⟨.dupI, .app, [.app, .app, .dupI, .dupI], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1), (.new 3 0, .right 2), (.new 3 1, .new 0 2), (.new 3 2, .new 1 2)]⟩,
  ⟨.dupI, .var, [.var, .var, .dupI], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1)]⟩,
  ⟨.dupI, .rho, [.rho, .rho], [(.new 0 0, .left 1), (.new 1 0, .left 2)]⟩,
  ⟨.dupI, .tab, [.tab, .tab, .dupI, .dupI, .dupI, .dupI, .dupI], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1), (.new 3 0, .right 2), (.new 3 1, .new 0 2), (.new 3 2, .new 1 2), (.new 4 0, .right 3), (.new 4 1, .new 0 3), (.new 4 2, .new 1 3), (.new 5 0, .right 4), (.new 5 1, .new 0 4), (.new 5 2, .new 1 4), (.new 6 0, .right 5), (.new 6 1, .new 0 5), (.new 6 2, .new 1 5)]⟩,
  ⟨.dupO, .sig, [.sig, .sig, .dupO, .dupO], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1), (.new 3 0, .right 2), (.new 3 1, .new 0 2), (.new 3 2, .new 1 2)]⟩,
  ⟨.dupI, .dupO, [], [(.left 1, .right 1), (.left 2, .right 2)]⟩,
  ⟨.eraI, .lam, [.eraO, .eraI], [(.new 0 0, .right 1), (.new 1 0, .right 2)]⟩,
  ⟨.eraI, .app, [.eraI, .eraI], [(.new 0 0, .right 1), (.new 1 0, .right 2)]⟩,
  ⟨.eraI, .var, [.eraI], [(.new 0 0, .right 1)]⟩,
  ⟨.eraI, .rho, [], []⟩,
  ⟨.eraI, .tab, [.eraI, .eraI, .eraI, .eraI, .eraI], [(.new 0 0, .right 1), (.new 1 0, .right 2), (.new 2 0, .right 3), (.new 3 0, .right 4), (.new 4 0, .right 5)]⟩,
  ⟨.eraI, .dupO, [.eraI, .eraI], [(.new 0 0, .right 1), (.new 1 0, .right 2)]⟩,
  ⟨.eraO, .sig, [.eraO, .eraO], [(.new 0 0, .right 1), (.new 1 0, .right 2)]⟩,
  ⟨.eraO, .eraI, [], []⟩]

def qstuck : List (Q × Q) := [(.O, .app), (.O, .rho), (.O, .dupO), (.O, .eraO), (.A, .app), (.A, .var), (.A, .rho), (.A, .dupO), (.A, .eraO), (.T, .dupO), (.T, .eraO), (.C, .dupO), (.C, .eraO), (.dupI, .eraO)]

/-- `reify` of `kexport.py`, compiled by `knet.py`: 107 agents. -/
def reifyProg : Prog Q :=
  { agents := [.lam, .app, .app, .app, .tab, .lam, .lam, .lam, .lam, .app, .lam, .app, .lam, .lam, .lam, .lam, .app, .var, .var, .eraI, .eraI, .app, .app, .app, .var, .app, .var, .var, .var, .lam, .lam, .app, .var, .var, .eraI, .var, .sig, .lam, .app, .app, .var, .rho, .var, .lam, .lam, .lam, .lam, .lam, .app, .app, .lam, .lam, .lam, .lam, .lam, .app, .app, .var, .var, .var, .eraI, .eraI, .app, .app, .app, .var, .var, .var, .var, .app, .app, .app, .var, .var, .var, .var, .sig, .sig, .sig, .lam, .lam, .lam, .lam, .app, .lam, .lam, .lam, .lam, .app, .var, .var, .eraI, .eraI, .app, .var, .var, .eraI, .eraI, .rho, .rho, .var, .lam, .lam, .var, .eraI, .lam, .var],
    wires := [(.out, .port 0 0), (.port 16 1, .port 17 0), (.port 16 2, .port 18 0), (.port 15 2, .port 16 0), (.port 15 1, .port 17 1), (.port 14 2, .port 15 0), (.port 14 1, .port 19 0), (.port 13 2, .port 14 0), (.port 13 1, .port 20 0), (.port 12 2, .port 13 0), (.port 12 1, .port 18 1), (.port 11 1, .port 12 0), (.port 23 1, .port 24 0), (.port 26 1, .port 27 0), (.port 25 1, .port 26 0), (.port 25 2, .port 28 0), (.port 23 2, .port 25 0), (.port 22 1, .port 23 0), (.port 31 1, .port 32 0), (.port 31 2, .port 33 0), (.port 30 2, .port 31 0), (.port 30 1, .port 32 1), (.port 29 2, .port 30 0), (.port 29 1, .port 34 0), (.port 22 2, .port 29 0), (.port 21 1, .port 22 0), (.port 21 2, .port 35 0), (.port 11 2, .port 21 0), (.port 10 2, .port 11 0), (.port 10 1, .port 36 0), (.port 36 1, .port 28 1), (.port 36 2, .port 35 1), (.port 9 1, .port 10 0), (.port 39 1, .port 40 0), (.port 39 2, .port 41 0), (.port 38 1, .port 39 0), (.port 38 2, .port 42 0), (.port 37 2, .port 38 0), (.port 37 1, .port 40 1), (.port 9 2, .port 37 0), (.port 8 2, .port 9 0), (.port 8 1, .port 42 1), (.port 7 2, .port 8 0), (.port 7 1, .port 33 1), (.port 6 2, .port 7 0), (.port 6 1, .port 27 1), (.port 5 2, .port 6 0), (.port 5 1, .port 24 1), (.port 4 1, .port 5 0), (.port 56 1, .port 57 0), (.port 56 2, .port 58 0), (.port 55 1, .port 56 0), (.port 55 2, .port 59 0), (.port 54 2, .port 55 0), (.port 54 1, .port 60 0), (.port 53 2, .port 54 0), (.port 53 1, .port 57 1), (.port 52 2, .port 53 0), (.port 52 1, .port 61 0), (.port 51 2, .port 52 0), (.port 51 1, .port 59 1), (.port 50 2, .port 51 0), (.port 50 1, .port 58 1), (.port 49 1, .port 50 0), (.port 64 1, .port 65 0), (.port 64 2, .port 66 0), (.port 63 1, .port 64 0), (.port 63 2, .port 67 0), (.port 62 1, .port 63 0), (.port 62 2, .port 68 0), (.port 49 2, .port 62 0), (.port 48 1, .port 49 0), (.port 71 1, .port 72 0), (.port 71 2, .port 73 0), (.port 70 1, .port 71 0), (.port 70 2, .port 74 0), (.port 69 1, .port 70 0), (.port 69 2, .port 75 0), (.port 48 2, .port 69 0), (.port 47 2, .port 48 0), (.port 47 1, .port 76 0), (.port 76 1, .port 68 1), (.port 76 2, .port 75 1), (.port 46 2, .port 47 0), (.port 46 1, .port 77 0), (.port 77 1, .port 67 1), (.port 77 2, .port 74 1), (.port 45 2, .port 46 0), (.port 45 1, .port 73 1), (.port 44 2, .port 45 0), (.port 44 1, .port 66 1), (.port 43 2, .port 44 0), (.port 43 1, .port 78 0), (.port 78 1, .port 65 1), (.port 78 2, .port 72 1), (.port 4 2, .port 43 0), (.port 88 1, .port 89 0), (.port 88 2, .port 90 0), (.port 87 2, .port 88 0), (.port 87 1, .port 91 0), (.port 86 2, .port 87 0), (.port 86 1, .port 92 0), (.port 85 2, .port 86 0), (.port 85 1, .port 89 1), (.port 84 2, .port 85 0), (.port 84 1, .port 90 1), (.port 83 1, .port 84 0), (.port 93 1, .port 94 0), (.port 93 2, .port 95 0), (.port 83 2, .port 93 0), (.port 82 2, .port 83 0), (.port 82 1, .port 96 0), (.port 81 2, .port 82 0), (.port 81 1, .port 95 1), (.port 80 2, .port 81 0), (.port 80 1, .port 94 1), (.port 79 2, .port 80 0), (.port 79 1, .port 97 0), (.port 4 3, .port 79 0), (.port 4 4, .port 98 0), (.port 4 5, .port 99 0), (.port 3 1, .port 4 0), (.port 3 2, .port 100 0), (.port 2 1, .port 3 0), (.port 102 2, .port 103 0), (.port 102 1, .port 104 0), (.port 101 2, .port 102 0), (.port 101 1, .port 103 1), (.port 2 2, .port 101 0), (.port 1 1, .port 2 0), (.port 105 2, .port 106 0), (.port 105 1, .port 106 1), (.port 1 2, .port 105 0), (.port 0 2, .port 1 0), (.port 0 1, .port 100 1)] }

/-- `run` of `kexport.py`, the thaw, λq. T q, written out: 2 agents. -/
def runProg : Prog Q :=
  { agents := [.lam, .T],
    wires := [(.out, .port 0 0), (.port 0 1, .port 1 0), (.port 1 1, .port 0 2)] }

/-- `interp` of `kexport.py`, compiled by `knet.py`; v0.1's interpreter, a `run` to compare: 29 agents. -/
def interpProg : Prog Q :=
  { agents := [.tab, .lam, .lam, .var, .eraI, .lam, .lam, .lam, .app, .lam, .app, .var, .app, .var, .var, .app, .var, .var, .app, .var, .var, .sig, .sig, .lam, .lam, .var, .eraI, .rho, .rho],
    wires := [(.out, .port 0 0), (.port 2 2, .port 3 0), (.port 2 1, .port 3 1), (.port 1 2, .port 2 0), (.port 1 1, .port 4 0), (.port 0 1, .port 1 0), (.port 10 1, .port 11 0), (.port 13 1, .port 14 0), (.port 12 1, .port 13 0), (.port 15 1, .port 16 0), (.port 15 2, .port 17 0), (.port 12 2, .port 15 0), (.port 10 2, .port 12 0), (.port 9 2, .port 10 0), (.port 9 1, .port 14 1), (.port 8 1, .port 9 0), (.port 18 1, .port 19 0), (.port 18 2, .port 20 0), (.port 8 2, .port 18 0), (.port 7 2, .port 8 0), (.port 7 1, .port 17 1), (.port 6 2, .port 7 0), (.port 6 1, .port 20 1), (.port 5 2, .port 6 0), (.port 5 1, .port 21 0), (.port 21 1, .port 11 1), (.port 21 2, .port 22 0), (.port 22 1, .port 16 1), (.port 22 2, .port 19 1), (.port 0 2, .port 5 0), (.port 24 2, .port 25 0), (.port 24 1, .port 25 1), (.port 23 2, .port 24 0), (.port 23 1, .port 26 0), (.port 0 3, .port 23 0), (.port 0 4, .port 27 0), (.port 0 5, .port 28 0)] }

/-- `reifyK` of `kexport.py`, compiled by `knet.py`; the continuation-passing `reify`, to compare: 122 agents. -/
def reifyKProg : Prog Q :=
  { agents := [.lam, .app, .app, .app, .app, .tab, .lam, .lam, .lam, .lam, .lam, .app, .lam, .app, .app, .app, .app, .var, .app, .var, .var, .var, .lam, .lam, .app, .var, .var, .eraI, .var, .lam, .app, .var, .lam, .lam, .lam, .app, .var, .var, .eraI, .eraI, .sig, .lam, .app, .app, .var, .rho, .var, .lam, .lam, .lam, .lam, .lam, .lam, .app, .app, .app, .app, .var, .var, .var, .var, .lam, .lam, .app, .app, .app, .app, .var, .var, .var, .var, .lam, .app, .var, .lam, .lam, .lam, .app, .app, .var, .var, .var, .eraI, .eraI, .sig, .sig, .lam, .lam, .lam, .lam, .lam, .app, .lam, .app, .app, .var, .lam, .lam, .lam, .app, .var, .var, .eraI, .eraI, .var, .app, .var, .var, .eraI, .rho, .rho, .var, .lam, .lam, .var, .eraI, .lam, .var, .lam, .lam, .var, .eraI],
    wires := [(.out, .port 0 0), (.port 16 1, .port 17 0), (.port 19 1, .port 20 0), (.port 18 1, .port 19 0), (.port 18 2, .port 21 0), (.port 16 2, .port 18 0), (.port 15 1, .port 16 0), (.port 24 1, .port 25 0), (.port 24 2, .port 26 0), (.port 23 2, .port 24 0), (.port 23 1, .port 25 1), (.port 22 2, .port 23 0), (.port 22 1, .port 27 0), (.port 15 2, .port 22 0), (.port 14 1, .port 15 0), (.port 14 2, .port 28 0), (.port 13 1, .port 14 0), (.port 30 1, .port 31 0), (.port 35 1, .port 36 0), (.port 35 2, .port 37 0), (.port 34 2, .port 35 0), (.port 34 1, .port 36 1), (.port 33 2, .port 34 0), (.port 33 1, .port 38 0), (.port 32 2, .port 33 0), (.port 32 1, .port 39 0), (.port 30 2, .port 32 0), (.port 29 2, .port 30 0), (.port 29 1, .port 37 1), (.port 13 2, .port 29 0), (.port 12 2, .port 13 0), (.port 12 1, .port 40 0), (.port 40 1, .port 21 1), (.port 40 2, .port 28 1), (.port 11 1, .port 12 0), (.port 43 1, .port 44 0), (.port 43 2, .port 45 0), (.port 42 1, .port 43 0), (.port 42 2, .port 46 0), (.port 41 2, .port 42 0), (.port 41 1, .port 44 1), (.port 11 2, .port 41 0), (.port 10 2, .port 11 0), (.port 10 1, .port 31 1), (.port 9 2, .port 10 0), (.port 9 1, .port 46 1), (.port 8 2, .port 9 0), (.port 8 1, .port 26 1), (.port 7 2, .port 8 0), (.port 7 1, .port 20 1), (.port 6 2, .port 7 0), (.port 6 1, .port 17 1), (.port 5 1, .port 6 0), (.port 56 1, .port 57 0), (.port 56 2, .port 58 0), (.port 55 1, .port 56 0), (.port 55 2, .port 59 0), (.port 54 1, .port 55 0), (.port 54 2, .port 60 0), (.port 53 1, .port 54 0), (.port 66 1, .port 67 0), (.port 66 2, .port 68 0), (.port 65 1, .port 66 0), (.port 65 2, .port 69 0), (.port 64 1, .port 65 0), (.port 64 2, .port 70 0), (.port 63 1, .port 64 0), (.port 72 1, .port 73 0), (.port 78 1, .port 79 0), (.port 78 2, .port 80 0), (.port 77 1, .port 78 0), (.port 77 2, .port 81 0), (.port 76 2, .port 77 0), (.port 76 1, .port 82 0), (.port 75 2, .port 76 0), (.port 75 1, .port 79 1), (.port 74 2, .port 75 0), (.port 74 1, .port 83 0), (.port 72 2, .port 74 0), (.port 71 2, .port 72 0), (.port 71 1, .port 81 1), (.port 63 2, .port 71 0), (.port 62 2, .port 63 0), (.port 62 1, .port 67 1), (.port 61 2, .port 62 0), (.port 61 1, .port 80 1), (.port 53 2, .port 61 0), (.port 52 2, .port 53 0), (.port 52 1, .port 73 1), (.port 51 2, .port 52 0), (.port 51 1, .port 84 0), (.port 84 1, .port 60 1), (.port 84 2, .port 70 1), (.port 50 2, .port 51 0), (.port 50 1, .port 85 0), (.port 85 1, .port 59 1), (.port 85 2, .port 69 1), (.port 49 2, .port 50 0), (.port 49 1, .port 68 1), (.port 48 2, .port 49 0), (.port 48 1, .port 58 1), (.port 47 2, .port 48 0), (.port 47 1, .port 57 1), (.port 5 2, .port 47 0), (.port 94 1, .port 95 0), (.port 99 1, .port 100 0), (.port 99 2, .port 101 0), (.port 98 2, .port 99 0), (.port 98 1, .port 102 0), (.port 97 2, .port 98 0), (.port 97 1, .port 103 0), (.port 96 2, .port 97 0), (.port 96 1, .port 100 1), (.port 94 2, .port 96 0), (.port 93 1, .port 94 0), (.port 93 2, .port 104 0), (.port 92 2, .port 93 0), (.port 92 1, .port 101 1), (.port 91 1, .port 92 0), (.port 105 1, .port 106 0), (.port 105 2, .port 107 0), (.port 91 2, .port 105 0), (.port 90 2, .port 91 0), (.port 90 1, .port 95 1), (.port 89 2, .port 90 0), (.port 89 1, .port 108 0), (.port 88 2, .port 89 0), (.port 88 1, .port 107 1), (.port 87 2, .port 88 0), (.port 87 1, .port 106 1), (.port 86 2, .port 87 0), (.port 86 1, .port 104 1), (.port 5 3, .port 86 0), (.port 5 4, .port 109 0), (.port 5 5, .port 110 0), (.port 4 1, .port 5 0), (.port 4 2, .port 111 0), (.port 3 1, .port 4 0), (.port 113 2, .port 114 0), (.port 113 1, .port 115 0), (.port 112 2, .port 113 0), (.port 112 1, .port 114 1), (.port 3 2, .port 112 0), (.port 2 1, .port 3 0), (.port 116 2, .port 117 0), (.port 116 1, .port 117 1), (.port 2 2, .port 116 0), (.port 1 1, .port 2 0), (.port 119 2, .port 120 0), (.port 119 1, .port 121 0), (.port 118 2, .port 119 0), (.port 118 1, .port 120 1), (.port 1 2, .port 118 0), (.port 0 2, .port 1 0), (.port 0 1, .port 111 1)] }

end QuoteRec
