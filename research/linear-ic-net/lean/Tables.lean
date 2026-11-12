import Basic

/-! Reflective linear nets: the rule table as data, and theorems about the table that any good
design should satisfy. Every theorem here is checked by evaluation (`decide`), so it doubles as a
linter: change the table and the theorems say what broke. -/


inductive K
  | lam | Lam | app | var | rho      -- code; lam, Lam and rho are also values
  | O | T | R                        -- running: apply, thaw, reflect
  | sig                              -- a variable's split, in code
  | dupI | dupO                      -- a copy in flight, principal facing in or out
  | eraI | eraO                      -- an eraser, principal facing in or out
  deriving DecidableEq, Repr

namespace K
def all : List K := [lam, Lam, app, var, rho, O, T, R, sig, dupI, dupO, eraI, eraO]

/-- Polarities of the ports, principal first. -/
def ports : K → List Pol
  | lam | Lam => [.o, .o, .i]     -- p; x (hands the argument to the uses), b (takes the body)
  | app => [.o, .i, .i]           -- p; f, a
  | var => [.o, .i]               -- p; s (from the binder, or a captured value)
  | rho => [.o]
  | O => [.i, .i, .o]             -- p (the function); a, r
  | T | R => [.i, .o]             -- p; o
  | sig | dupI => [.i, .o, .o]
  | dupO => [.o, .i, .i]
  | eraI => [.i]
  | eraO => [.o]

def arity (k : K) : Nat := k.ports.length - 1
def pol (k : K) (n : Nat) : Pol := k.ports.getD n .i
def isCode : K → Bool | lam | Lam | app | var | rho => true | _ => false
def isCopier : K → Bool | dupI | dupO => true | _ => false
def isEraser : K → Bool | eraI | eraO => true | _ => false
end K


structure Rule where
  a : K
  b : K
  made : List K
  wires : List (End × End)
  deriving Repr

namespace Rule
def polEnd (r : Rule) : End → Pol
  | .left n => r.a.pol n
  | .right n => r.b.pol n
  | .new j n => (r.made.getD j .eraI).pol n

def isSlot : End → Bool := End.isSlot

/-- A made port takes a slot's place, so it has the slot's polarity; two slots joined, or two made
ports joined, must have opposite polarities. -/
def wireOk (r : Rule) (w : End × End) : Bool :=
  if isSlot w.1 == isSlot w.2 then r.polEnd w.1 != r.polEnd w.2 else r.polEnd w.1 == r.polEnd w.2

def ends (r : Rule) : List End := r.wires.flatMap fun w => [w.1, w.2]

/-- Every port the rule must account for: the two agents' auxiliary ports and every port it makes. -/
def expected (r : Rule) : List End :=
  (List.range r.a.arity).map (fun n => End.left (n + 1)) ++
  (List.range r.b.arity).map (fun n => End.right (n + 1)) ++
  (List.range r.made.length).flatMap fun j =>
    (List.range ((r.made.getD j .eraI).arity + 1)).map fun n => End.new j n

def wellFormed (r : Rule) : Bool :=
  r.ends.length == r.expected.length && r.expected.all fun e => r.ends.count e == 1

def polarized (r : Rule) : Bool :=
  r.a.pol 0 != r.b.pol 0 && r.wires.all r.wireOk

def samePair (r s : Rule) : Bool := (r.a == s.a && r.b == s.b) || (r.a == s.b && r.b == s.a)
end Rule

open K End

/-! ## The rules -/

/-- O ⋈ lam: the argument goes to the uses; the body is thawed into the result. -/
def run : Rule := ⟨O, lam, [T], [(left 1, right 1), (new 0 0, right 2), (new 0 1, left 2)]⟩
/-- O ⋈ Lam: the same, but the body comes back as code. -/
def openR : Rule := ⟨O, Lam, [], [(left 1, right 1), (left 2, right 2)]⟩
/-- O ⋈ rho: the argument is read. -/
def reflectStart : Rule := ⟨O, rho, [R], [(new 0 0, left 1), (new 0 1, left 2)]⟩
/-- T ⋈ app: a running application, with both parts thawed. -/
def thawApp : Rule :=
  ⟨T, app, [O, T, T], [(new 1 0, right 1), (new 1 1, new 0 0), (new 2 0, right 2), (new 2 1, new 0 1),
    (new 0 2, left 1)]⟩
/-- T ⋈ var: the use resolves to whatever it leads to, unthawed. -/
def thawVar : Rule := ⟨T, var, [], [(left 1, right 1)]⟩
/-- T ⋈ a value: the value itself. -/
def thawValue (k : K) : Rule :=
  ⟨T, k, [k], (new 0 0, left 1) :: (List.range k.arity).map fun n => (new 0 (n + 1), right (n + 1))⟩

/-- How many arms a view takes: one per kind of value. -/
def arms : Nat := 5

/-- A view: λk1 … k5. k_arm (the other k's) (the parts), built as code. `parts` are the ends the
splices lead to; `extra` are agents made besides the view (the function handed back). -/
def view (b : K) (arm : Nat) (parts : List End) (extra : List K) (extraWires : List (End × End)) : Rule :=
  let items := arm :: ((List.range arms).map (· + 1)).filter (· != arm)   -- bound uses, by binder number
  let m := items.length + parts.length
  let nApps := m - 1
  let appAt := fun k => arms + k          -- the k-th application of the spine
  let varAt := fun k => arms + nApps + k  -- the k-th spine item's var
  let made := List.replicate arms lam ++ List.replicate nApps app ++ List.replicate m var ++ extra
  let lams := (new 0 0, left 1) :: (List.range (arms - 1)).map (fun j => (new j 2, new (j + 1) 0)) ++
               [(new (arms - 1) 2, if nApps = 0 then new (varAt 0) 0 else new (appAt (nApps - 1)) 0)]
  let spine := (List.range nApps).flatMap fun k =>
    [(new (appAt k) 1, if k = 0 then new (varAt 0) 0 else new (appAt (k - 1)) 0),
     (new (appAt k) 2, new (varAt (k + 1)) 0)]
  let bound := items.zipIdx.map fun (k, idx) => (new (varAt idx) 1, new (k - 1) 1)
  let splices := parts.zipIdx.map fun (e, idx) => (new (varAt (items.length + idx)) 1, e)
  ⟨R, b, made, lams ++ spine ++ bound ++ splices ++ extraWires⟩

/-- Where a view with one part puts its extra agent: after the binders, applications and uses. -/
def extraAt : Nat := arms + arms + (arms + 1)

/-- R ⋈ lam: hands the function back opened (as a Lam), to the first arm. -/
def viewFun : Rule :=
  view lam 1 [new extraAt 0] [Lam] [(new extraAt 1, right 1), (new extraAt 2, right 2)]
/-- R ⋈ Lam: hands the opened function back closed (as a lam), to the second arm. Reading a
function twice gives it back unchanged. -/
def viewOpened : Rule :=
  view Lam 2 [new extraAt 0] [lam] [(new extraAt 1, right 1), (new extraAt 2, right 2)]
def viewApp : Rule := view app 3 [right 1, right 2] [] []
def viewVar : Rule := view var 4 [right 1] [] []
def viewPrim : Rule := view rho 5 [] [] []

/-- c ⋈ x: two copies of x; each auxiliary port of x gets a copy in flight facing the same way. -/
def copyRule (c x : K) : Rule :=
  let ds := (List.range x.arity).map fun n => if x.pol (n + 1) = .i then dupI else dupO
  ⟨c, x, [x, x] ++ ds,
    [(new 0 0, left 1), (new 1 0, left 2)] ++ (List.range x.arity).flatMap fun n =>
      [(new (n + 2) 0, right (n + 1)), (new (n + 2) 1, new 0 (n + 1)), (new (n + 2) 2, new 1 (n + 1))]⟩

/-- dupI ⋈ dupO: the two ends of one copy meet. -/
def annihilate : Rule := ⟨dupI, dupO, [], [(left 1, right 1), (left 2, right 2)]⟩

/-- e ⋈ x: x goes; each auxiliary port of x gets an eraser facing the same way. -/
def eraseRule (e x : K) : Rule :=
  ⟨e, x, (List.range x.arity).map (fun n => if x.pol (n + 1) = .i then eraI else eraO),
    (List.range x.arity).map fun n => (new n 0, right (n + 1))⟩

def codeKinds : List K := [lam, Lam, app, var, rho]

def table : List Rule :=
  [run, openR, reflectStart, thawApp, thawVar] ++ [lam, Lam, rho].map thawValue ++
  [viewFun, viewOpened, viewApp, viewVar, viewPrim] ++
  codeKinds.map (copyRule sig) ++ codeKinds.map (copyRule dupI) ++ [copyRule dupO sig, annihilate] ++
  (codeKinds ++ [dupO]).map (eraseRule eraI) ++ [sig, eraI].map (eraseRule eraO)

/-- Pairs that meet with opposite polarities but have no rule, on purpose. Applying code is an
error. The others never form (tested, not proven): running agents never face an out-facing copier
or eraser, and an out-facing eraser, which follows an erased binder's variable to its uses, never
meets a copier coming up from a use, since a body is only copied through its binder. -/
def stuck : List (K × K) :=
  [(O, app), (O, var)] ++ ([O, T, R].flatMap fun k => [(k, dupO), (k, eraO)]) ++ [(dupI, eraO)]

def hasRule (x y : K) : Bool := table.any fun r => (r.a == x && r.b == y) || (r.a == y && r.b == x)

/-! ## Theorems about the table -/

/-- Every rule accounts for every port exactly once: nothing dangles, nothing is wired twice. -/
theorem wellFormed : ∀ r ∈ table, r.wellFormed = true := by decide

/-- Every wire joins an out port to an in port, before and after every rule. -/
theorem polarized : ∀ r ∈ table, r.polarized = true := by decide

/-- At most one rule per pair of agents, so reduction is deterministic up to order. -/
theorem deterministic : table.Pairwise (fun r s => r.samePair s = false) := by decide

/-- Every pair that can meet (opposite principal polarities) has a rule, or is one of the
deliberate stuck pairs, and none of those has a rule. -/
theorem covered : ∀ x ∈ K.all, ∀ y ∈ K.all, x.pol 0 = .i → y.pol 0 = .o →
    hasRule x y = true ∨ (x, y) ∈ stuck := by decide
theorem stuck_has_no_rule : ∀ p ∈ stuck, hasRule p.1 p.2 = false := by decide

/-- Code always faces up, so two pieces of code never react: a function body is inert. -/
theorem code_faces_up : ∀ k ∈ K.all, k.isCode = true → k.pol 0 = .o := by decide

/-- Only erase rules make erasers, and only copy rules make copiers or sharers: every other rule
neither drops nor duplicates anything. -/
theorem only_erasing_drops : ∀ r ∈ table, r.made.any K.isEraser = true → r.a.isEraser = true := by decide
theorem only_copying_copies : ∀ r ∈ table, (r.made.any fun k => k.isCopier || k == sig) = true →
    (r.a == sig || r.a.isCopier) = true := by decide

/-- Every value and every piece of code can be read. -/
theorem readable : ∀ k ∈ codeKinds, hasRule R k = true := by decide

/-! ## Reading tells the kinds of value apart -/

namespace Rule
/-- The end a rule wires to e. -/
def other (r : Rule) (e : End) : Option End :=
  r.wires.findSome? fun w => if w.1 == e then some w.2 else if w.2 == e then some w.1 else none

/-- Walk a made term from its top: through binders into their bodies, down the function side of
applications, to the head variable; the answer is which binder (first, second, …) it belongs to. -/
def armFrom (r : Rule) : Nat → List Nat → End → Option Nat
  | 0, _, _ => none
  | n + 1, binders, .new j 0 =>
    match r.made.getD j .eraI with
    | .lam => (r.other (.new j 2)).bind (armFrom r n (binders ++ [j]))
    | .app => (r.other (.new j 1)).bind (armFrom r n binders)
    | .var => match r.other (.new j 1) with
      | some (.new i 1) => (binders.findIdx? (· == i)).map (· + 1)
      | _ => none
    | _ => none
  | _, _, _ => none

/-- The arm a reflection rule runs: read off the view it builds at the reader's output. -/
def arm (r : Rule) : Option Nat := (r.other (.left 1)).bind (armFrom r 30 [])
end Rule

def readers (T : List Rule) : List Rule := T.filter (·.a == R)

/-- Every kind of value has its own arm, so a program can always tell which kind it holds. -/
theorem views_tell_kinds_apart :
    (readers table).map Rule.arm = [some 1, some 2, some 3, some 4, some 5] := by decide

/-! ## The theorems have teeth: they reject broken rules and flawed designs -/

/-- A rule that forgets a port is not well formed; one that joins two in ports is not polarized. -/
example : ({ run with wires := run.wires.drop 1 } : Rule).wellFormed = false := by decide
example : ({ openR with wires := [(left 1, right 2), (left 2, right 1)] } : Rule).polarized = false := by decide

/-- Design flaw 1: make the variable split in code the same agent as a copy in flight. Copying a
function that uses its variable twice needs the out-facing copier to pass through the split
(commute), while the two ends of one copy must cancel (annihilate): two rules for one pair. -/
def merge (k : K) : K := if k = sig then dupI else k
def mergedTable : List Rule :=
  table.map fun r => { r with a := merge r.a, b := merge r.b, made := r.made.map merge }
theorem merged_sharer_conflicts :
    let rs := mergedTable.filter (·.samePair annihilate)
    rs.any (·.made.isEmpty) = true ∧ rs.any (fun r => !r.made.isEmpty) = true := by decide

/-- Design flaw 2: a single copier that always faces in. Copying a function puts a copier on its
binder's x port, which hands values out, so that copier must face out. -/
def copyRuleOneWay (c x : K) : Rule :=
  { copyRule c x with made := (copyRule c x).made.map fun k => if k = dupO then dupI else k }
theorem one_way_copier_breaks_polarity : (copyRuleOneWay sig lam).polarized = false := by decide

/-- Design flaw 3: an opened function that reads exactly like a closed one (the first design). The
two behave differently when applied, one runs its body and the other hands it back as code, so an
interpreter written in the calculus cannot tell which it must do (`../nets/selfinterp.py`: 6 of
14 test programs go wrong). Every other table theorem passes this design; this one rejects it. -/
def conflatedTable : List Rule :=
  table.map fun r => if r.a == R && r.b == Lam then { viewFun with b := Lam } else r
theorem conflated_readers_share_an_arm :
    ((readers conflatedTable).map Rule.arm).Nodup = false ∧
    (∀ r ∈ conflatedTable, r.wellFormed = true ∧ r.polarized = true) := by decide

/-! ## The resource rules are forced -/

/-- Every way to choose, port by port, which way a copier or eraser faces. -/
def choices : Nat → List (List Bool)
  | 0 => [[]]
  | n + 1 => (choices n).flatMap fun c => [true :: c, false :: c]

def copyWith (c x : K) (ch : List Bool) : Rule :=
  { copyRule c x with made := [x, x] ++ ch.map fun b => if b then dupI else dupO }
def eraseWith (e x : K) (ch : List Bool) : Rule :=
  { eraseRule e x with made := ch.map fun b => if b then eraI else eraO }

/-- Of all the ways to orient the copiers a copy rule leaves behind, exactly one is polarized, and
it is the table's. The same holds for erasers. So once the code agents are fixed, polarity alone
decides every copy and erase rule. -/
theorem copy_forced : ∀ c ∈ [sig, dupI], ∀ x ∈ codeKinds, ∀ ch ∈ choices x.arity,
    (copyWith c x ch).polarized = true ↔ (copyWith c x ch).made = (copyRule c x).made := by decide
theorem erase_forced : ∀ x ∈ codeKinds ++ [dupO], ∀ ch ∈ choices x.arity,
    (eraseWith eraI x ch).polarized = true ↔ (eraseWith eraI x ch).made = (eraseRule eraI x).made := by decide

example : table.length = 33 ∧ stuck.length = 9 ∧ K.all.length = 13 := by decide
