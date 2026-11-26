import Search

/-! What "an ideal interaction system" means, as definitions over the generic tables of `Net.lean`.
Not in the default build: check with `lake env lean Ideal.lean`.

A system is a finite list of kinds fixed before any program (uniform), a signature, and a rule table.
It is admissible when it passes hard constraints; among admissible systems an ideal one has the least
score. The score is description length in bits, with the rule table coded by generic shapes where it can
be (annihilate, commute, pass on), so a table made of one or two shapes costs almost nothing per rule.
The reference semantics (universality, reflection, a self-interpreter written in the system) enter as an
abstract predicate `Spec`, discharged per design by the run checks of `Check.lean`/`Checked.lean`. -/

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

/-- Ideal for a specification: admissible, meets the spec, and no admissible system of any kinds meeting
the spec scores less. `Spec` holds the semantic constraints (compiles λ-terms correctly, has a reader whose
views tell kinds apart, has a correct self-interpreter) and the bits of the programs it needs. -/
def Ideal (Spec : ∀ (κ : Type) [DecidableEq κ], List κ → Sig κ → List (GRule κ) → Nat → Prop)
    (ks : List κ) (S : Sig κ) (T : List (GRule κ)) (stuck : List (κ × κ)) (cp er : κ → Bool) (bits : Nat) : Prop :=
  Admissible ks S T stuck cp er ∧ Spec κ ks S T bits ∧
  ∀ (K' : Type) [DecidableEq K'] (ks' : List K') (S' : Sig K') (T' : List (GRule K')) (st' : List (K' × K'))
    (cp' er' : K' → Bool) (bits' : Nat),
    Admissible ks' S' T' st' cp' er' → Spec K' ks' S' T' bits' → score ks S T bits ≤ score ks' S' T' bits'

/-! ## The reflective-nets table -/

def copierK : K → Bool | .sig | .dupI | .dupO => true | _ => false
def eraserK : K → Bool | .eraI | .eraO => true | _ => false

theorem rln_total : Total K.all sig gtable stuck := by
  unfold Total; decide

theorem rln_honest : Honest copierK eraserK gtable := by
  unfold Honest; decide

theorem rln_admissible : Admissible K.all sig gtable stuck copierK eraserK :=
  ⟨rln_tableOk, rln_total, rln_honest, by decide⟩

-- How the table is coded: rules with a generic shape, of all rules, and the score of the table alone.
#eval ((gtable.filter fun r => r.isAnn sig || r.isCom sig || r.isPass sig).length, gtable.length,
  score K.all sig gtable 0)
