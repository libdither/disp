import Search

/-! The candidate net ("quote-and-table net") as a rule table, written by `../ideal/ktable.py --lean`.
Not in the default build: check with `lake env lean QuoteNet.lean`. The table is checked by `decide`
against the same predicates as the older table (`Search.lean`, `Ideal.lean`): well formed, polarized, one
rule per pair, opposite principal ports (`TableOk`), no end wired twice (`RunOk`), every pair that can meet
has a rule or is listed stuck (`Total`), closed over one fixed list of kinds. So it gets the general
theorems of `Net.lean`, `Runs.lean` and `Moves.lean`: steps keep nets polarized, runs are strongly
confluent, and two finished runs end in the same net.
`Honest` is stated twice: with tables counted as copiers (a table is where a program asks for one more copy
of a slot) it holds; with the three plain copiers only it fails, on exactly the five reading rules. -/

/-! ## The constraints and the score, copied from `Ideal.lean` (which is not a built module, so it cannot
be imported) -/

open End

variable {κ : Type} [DecidableEq κ]

def Sig.arity (S : Sig κ) (k : κ) : Nat := (S.ports k).length - 1

namespace GRule
variable (S : Sig κ) (r : GRule κ)

def other (e : End) : Option End :=
  r.wires.findSome? fun w => if w.1 == e then some w.2 else if w.2 == e then some w.1 else none

/-- Annihilation: nothing made, every wire joins a port of one agent to a port of the other. -/
def isAnn : Bool :=
  r.made.isEmpty && S.arity r.a == S.arity r.b &&
  r.wires.all fun w => match w with
    | (.left _, .right _) | (.right _, .left _) => true
    | _ => false

/-- Commutation (Lafont): one copy of b per auxiliary port of a, one copy of a per port of b, the copies
wired as a grid. The copies may be other kinds (an oriented copier); `comSubs` counts those. -/
def isComAt (ra rb : Nat → End) (p q : Nat) : Bool :=
  r.made.length == p + q &&
  (List.range p).all (fun i => match r.other (ra (i + 1)) with
    | some (.new j 0) => (r.made[j]?.map (S.arity ·)) == some q &&
        (List.range q).all fun l => match r.other (rb (l + 1)) with
          | some (.new j' 0) => r.other (.new j' (i + 1)) == some (.new j (l + 1))
          | _ => false
    | _ => false) &&
  (List.range q).all fun l => match r.other (rb (l + 1)) with
    | some (.new j' 0) => (r.made[j']?.map (S.arity ·)) == some p
    | _ => false

def isCom : Bool :=
  r.isComAt S .left .right (S.arity r.a) (S.arity r.b)

/-- Pass on: a one-port agent meets b and b is made again, its principal port where the agent's port led. -/
def isPass : Bool :=
  S.arity r.a == 1 && r.made.length == 1 && r.other (.new 0 0) == some (.left 1) &&
  (List.range (S.arity r.b)).all fun n => r.other (.new 0 (n + 1)) == some (.right (n + 1))
end GRule

/-! ## Hard constraints -/

/-- Every pair that can meet has a rule, or is a declared stuck pair. -/
def Total (ks : List κ) (S : Sig κ) (T : List (GRule κ)) (stuck : List (κ × κ)) : Prop :=
  ∀ a ∈ ks, ∀ b ∈ ks, S.pol a 0 ≠ S.pol b 0 →
    (∃ r ∈ T, (r.a = a ∧ r.b = b) ∨ (r.a = b ∧ r.b = a)) ∨ (a, b) ∈ stuck ∨ (b, a) ∈ stuck

/-- Resources are honest: only a rule with a copier on its left makes copiers, only one with an eraser makes
erasers. A case that copies the arm it takes (the kept table of `../nets/tables.py`) fails this. -/
def Honest (copier eraser : κ → Bool) (T : List (GRule κ)) : Prop :=
  ∀ r ∈ T, (r.made.any copier → copier r.a ∨ copier r.b) ∧ (r.made.any eraser → eraser r.a ∨ eraser r.b)

/-- Everything the table checks of `Net.lean` and `Tables.lean` already give, plus totality and honesty. The
uniformity constraint is the type: `ks` is one finite list, fixed before any program, and every rule's
right side is a fixed finite net, so its size is bounded independently of the program. -/
structure Admissible (ks : List κ) (S : Sig κ) (T : List (GRule κ)) (stuck : List (κ × κ))
    (copier eraser : κ → Bool) : Prop where
  table : TableOk S T
  total : Total ks S T stuck
  honest : Honest copier eraser T
  closed : ∀ r ∈ T, r.a ∈ ks ∧ r.b ∈ ks ∧ ∀ m ∈ r.made, m ∈ ks

/-! ## The score (bits) -/

def clog (n : Nat) : Nat := if n ≤ 1 then 0 else Nat.log2 (n - 1) + 1

/-- log2 of the number of ways to pair n out ends with n in ends, rounded up per factor. -/
def wiringBits (n : Nat) : Nat := ((List.range n).map fun i => clog (i + 1)).sum

def literalBits (ks : List κ) (S : Sig κ) (r : GRule κ) : Nat :=
  let ends := S.arity r.a + S.arity r.b + (r.made.map fun m => S.arity m + 1).sum
  2 * clog (r.made.length + 1) + 1 + r.made.length * clog ks.length + wiringBits (ends / 2)

/-- A rule costs a tag and its shape's few parameters when it has a generic shape, else its literal. -/
def ruleBits (ks : List κ) (S : Sig κ) (r : GRule κ) : Nat :=
  2 * clog ks.length + 2 +
  (if r.isAnn S || r.isCom S || r.isPass S then 1 else literalBits ks S r)

def sigBits (ks : List κ) (S : Sig κ) : Nat :=
  ((ks.map fun k => 2 * clog (S.arity k + 2) + 1 + (S.arity k + 1))).sum

/-- The score of a system and of the programs it must be able to write: a reader and a self-interpreter,
written as nets of the system, given here by their sizes in bits (`../ideal/score.py` measures them). -/
def score (ks : List κ) (S : Sig κ) (T : List (GRule κ)) (programBits : Nat) : Nat :=
  sigBits ks S + (T.map (ruleBits ks S)).sum + programBits

/-! ## The table -/

inductive Q
  | lam | app | var | rho | tab | O | A | T | C | sig | dupI | dupO | eraI | eraO
  deriving DecidableEq, Repr

namespace Q
open Pol
def all : List Q := [lam, app, var, rho, tab, O, A, T, C, sig, dupI, dupO, eraI, eraO]
def ports : Q → List Pol
  | .lam => [o, o, i]
  | .app => [o, i, i]
  | .var => [o, i]
  | .rho => [o]
  | .tab => [o, i, i, i, i, i]
  | .O => [i, i, o]
  | .A => [i, i, o]
  | .T => [i, o]
  | .C => [i, i, i, i, i, i, o]
  | .sig => [i, o, o]
  | .dupI => [i, o, o]
  | .dupO => [o, i, i]
  | .eraI => [i]
  | .eraO => [o]
end Q

def qsig : Sig Q := ⟨Q.ports⟩

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

def copierQ : Q → Bool | .sig | .dupI | .dupO => true | _ => false
def tableOrCopierQ : Q → Bool | .sig | .dupI | .dupO | .tab | .C => true | _ => false
def eraserQ : Q → Bool | .eraI | .eraO => true | _ => false

theorem q_rules_polarized : ∀ r ∈ qtable, r.Polarized qsig := by
  show ∀ r ∈ qtable, ∀ w ∈ r.wires, r.wireOk qsig w = true; decide
theorem q_rules_madeOk : ∀ r ∈ qtable, r.MadeOk := by
  show ∀ r ∈ qtable, ∀ w ∈ r.wires, r.EndOk w.1 ∧ r.EndOk w.2; decide
theorem q_deterministic : Deterministic qtable := by
  show ∀ r ∈ qtable, ∀ r' ∈ qtable, ((r.a = r'.a ∧ r.b = r'.b) ∨ (r.a = r'.b ∧ r.b = r'.a)) → r = r'
  decide
theorem q_principals : PrincipalsOpposite qsig qtable := by
  show ∀ r ∈ qtable, qsig.pol r.a 0 ≠ qsig.pol r.b 0; decide
theorem q_tableOk : TableOk qsig qtable := ⟨q_rules_polarized, q_rules_madeOk, q_deterministic, q_principals⟩
theorem q_nodup : ∀ r ∈ qtable, (ends r.wires).Nodup := by decide
theorem q_runOk : RunOk qsig qtable := ⟨q_tableOk, q_nodup⟩

/-- Runs of the candidate are strongly confluent. -/
theorem q_strong_confluence {N N1 N2 : Net Q} (hinv : N.Involutive) (hfr : N.Fresh)
    (h1 : Step qsig qtable N N1) (h2 : Step qsig qtable N N2) :
    N1 = N2 ∨ ∃ M, Step qsig qtable N1 M ∧ Step qsig qtable N2 M :=
  q_tableOk.confluent hinv hfr h1 h2

/-- Two finished runs of a candidate program end in the same net, firing pairs and removing forwarders in
any order. -/
theorem q_finished_unique {keep : Ident → Prop} {N N1 N2 : Net Q} (hg : N.Good qsig)
    (h1 : Moves qsig qtable keep N N1) (h2 : Moves qsig qtable keep N N2)
    (f1 : Finished qsig qtable keep N1) (f2 : Finished qsig qtable keep N2) : N1 = N2 :=
  q_runOk.finished_unique hg h1 h2 f1 f2

theorem q_total : Total Q.all qsig qtable qstuck := by unfold Total; decide
theorem q_closed : ∀ r ∈ qtable, r.a ∈ Q.all ∧ r.b ∈ Q.all ∧ ∀ m ∈ r.made, m ∈ Q.all := by decide
/-- Only erasers erase; only copiers and tables (a table, and a table reading) copy. -/
theorem q_honest : Honest tableOrCopierQ eraserQ qtable := by unfold Honest; decide
theorem q_admissible : Admissible Q.all qsig qtable qstuck tableOrCopierQ eraserQ :=
  ⟨q_tableOk, q_total, q_honest, q_closed⟩
/-- With the reader not counted as a copier, the rules that break honesty are the five reading rules. -/
theorem q_reads_copy :
    (qtable.filter fun r => r.made.any copierQ && !(copierQ r.a || copierQ r.b)).map (fun r => (r.a, r.b))
      = [(.C, .lam), (.C, .app), (.C, .var), (.C, .rho), (.C, .tab)] := by decide
/-- No rule but an erase rule makes an eraser. -/
theorem q_only_erasing_drops : ∀ r ∈ qtable, r.made.any eraserQ → eraserQ r.a ∨ eraserQ r.b := by decide
/-- Code is inert: every code agent's principal port faces out, toward its user. -/
theorem q_code_faces_up : ∀ k ∈ [Q.lam, .app, .var, .rho, .tab], qsig.pol k 0 = .o := by decide
/-- Every kind of code has a reading rule, and the five take five different slots: the slot the copier
is put on is read off each rule's wiring. -/
theorem q_reads_tell_kinds_apart :
    (qtable.filter fun r => r.a == .C).map (fun r => (r.b, r.other (.new 0 0)))
      = [(.lam, some (.left 1)), (.app, some (.left 2)), (.var, some (.left 3)), (.rho, some (.left 4)),
         (.tab, some (.left 5))] := by decide

-- rules with a generic shape, all rules, stuck pairs, and the table's score in bits by `Ideal.lean`'s code
#eval ((qtable.filter fun r => r.isAnn qsig || r.isCom qsig || r.isPass qsig).length, qtable.length,
  qstuck.length, score Q.all qsig qtable 0)
