import Checked
import Readers

/-! Is the design forced? For each rule, every other way to wire the same agents that the table
checks accept is run against the test programs. A rule is forced when only its own wiring, up to
renaming the agents it makes (and swapping the two outputs of a split), gets every program right. Run with `lake exe forced`.

The five reflection rules are left out: a view is a λ-term, not a free wiring, and which arm goes
with which kind of value is a naming choice (`views_tell_kinds_apart` is what the design needs). -/

open K End

/-- Every way to pair up xs with pairs that ok accepts. -/
def pairings {α : Type} (ok : α → α → Bool) : Nat → List α → List (List (α × α))
  | _, [] => [[]]
  | 0, _ => []
  | n + 1, x :: xs =>
    (List.range xs.length).flatMap fun i =>
      match xs[i]? with
      | some y => if ok x y then (pairings ok n (xs.eraseIdx i)).map ((x, y) :: ·) else []
      | none => []

/-- Every wiring of the rule's ports that is well formed and polarized. -/
def Rule.rewirings (r : Rule) : List (List (End × End)) :=
  pairings (fun x y => r.wireOk (x, y)) r.expected.length r.expected

def End.key : End → Nat
  | .left n => n
  | .right n => 100 + n
  | .new j n => 1000 + 100 * j + n

def insertSorted (x : Nat) : List Nat → List Nat
  | [] => [x]
  | y :: ys => if x ≤ y then x :: y :: ys else y :: insertSorted x ys

/-- A wiring as a sorted list of numbers, one per wire, whichever way round the wire is written. -/
def wiringKey (ws : List (End × End)) : List Nat :=
  ws.foldl (fun acc w =>
    let a := w.1.key; let b := w.2.key
    insertSorted (if a ≤ b then a * 10000 + b else b * 10000 + a) acc) []

def listLt : List Nat → List Nat → Bool
  | [], [] => false
  | [], _ => true
  | _, [] => false
  | x :: xs, y :: ys => x < y || (x == y && listLt xs ys)

def perms : List Nat → List (List Nat)
  | [] => [[]]
  | x :: xs => (perms xs).flatMap fun p => (List.range (p.length + 1)).map fun i => p.insertIdx i x

/-- A way to rewrite a wiring that cannot change what a rule does: rename the made agents among
those of the same kind, and swap the two outputs of a split (`sig`), the one the rule consumes or
ones it makes. The two uses of a variable get equal copies, so which use hangs on which output of
a split cannot matter. -/
structure Sym where
  perm : List Nat
  swapLeft : Bool
  swapMade : List Nat

def sublists : List Nat → List (List Nat)
  | [] => [[]]
  | x :: xs => (sublists xs).flatMap fun l => [l, x :: l]

def Rule.renamings (r : Rule) : List Sym :=
  let ps := (perms (List.range r.made.length)).filter fun p =>
    (List.range r.made.length).all fun j => r.made[p.getD j j]? == r.made[j]?
  let sigs := (List.range r.made.length).filter fun j => r.made[j]? == some K.sig
  ps.flatMap fun p => (sublists sigs).flatMap fun sm =>
    if r.a == K.sig then [⟨p, false, sm⟩, ⟨p, true, sm⟩] else [⟨p, false, sm⟩]

def swap12 (n : Nat) : Nat := if n = 1 then 2 else if n = 2 then 1 else n

def End.rename (y : Sym) : End → End
  | .new j n => let j' := y.perm.getD j j; .new j' (if y.swapMade.contains j' then swap12 n else n)
  | .left n => .left (if y.swapLeft then swap12 n else n)
  | e => e

/-- The smallest key among a wiring's renamings: equal for two wirings exactly when one is the
other renamed. -/
def Rule.canon (r : Rule) (rens : List Sym) (ws : List (End × End)) : List Nat :=
  rens.foldl (fun best p =>
    let k := wiringKey (ws.map fun w => (w.1.rename p, w.2.rename p))
    if best.isEmpty || listLt k best then k else best) []

/-- One wiring per class of renamings. -/
def Rule.classes (r : Rule) : List (List (End × End)) :=
  let rens := r.renamings
  let (out, _) := r.rewirings.foldl (fun (acc : List (List (End × End)) × Std.HashSet (List Nat)) ws =>
    let c := r.canon rens ws
    if acc.2.contains c then acc else (ws :: acc.1, acc.2.insert c)) ([], {})
  out.reverse

/-! ## The test programs, with what the table itself does on each -/

structure Case where
  source : String
  t : Tm
  want : Outcome
  /-- steps allowed: a few times what the table itself needs -/
  limit : Nat

def Tm.size : Tm → Nat
  | .app f a => f.size + a.size + 1
  | .lam b | .Lam b => b.size + 1
  | .splice v => v.size + 1
  | _ => 1

/-- Read a term back, giving up after `budget` agents: a broken net can have cycles. -/
def readbackB : Nat → Net K → List Port → Port → Nat → Option Tm × Nat
  | 0, _, _, _, budget => (none, budget)
  | _, _, _, _, 0 => (none, 0)
  | n + 1, N, bs, p, budget + 1 =>
    match N.kind p.1 with
    | some (.fwd _) =>
      match N.mate (p.1, if p.2 = 0 then 1 else 0) with
      | some q => readbackB n N bs q budget
      | none => (none, budget)
    | some (.base k) =>
      if p.2 ≠ 0 then (none, budget) else
      match k with
      | .lam | .Lam =>
        match N.mate (p.1, 2) with
        | some q =>
          let (b, budget) := readbackB n N ((p.1, 1) :: bs) q budget
          (b.map (if k == .lam then Tm.lam else Tm.Lam), budget)
        | none => (none, budget)
      | .app =>
        match N.mate (p.1, 1), N.mate (p.1, 2) with
        | some qf, some qa =>
          match readbackB n N bs qf budget with
          | (some f, budget) =>
            let (a, budget) := readbackB n N bs qa budget
            (a.map (Tm.app f), budget)
          | (none, budget) => (none, budget)
        | _, _ => (none, budget)
      | .rho => (some .rho, budget)
      | .var =>
        match (N.mate (p.1, 1)).bind (walk n N) with
        | some src =>
          match bs.findIdx? (· == src) with
          | some i => (some (.var i), budget)
          | none =>
            if src.2 = 0 then
              let (v, budget) := readbackB n N [] src budget
              (v.map .splice, budget)
            else (none, budget)
        | none => (none, budget)
      | _ => (none, budget)
    | none => (none, budget)

/-- The checks of `fastCheckWith`, against a reference outcome worked out beforehand. -/
def Case.passes (T : List (GRule K)) (c : Case) : Bool :=
  match c.want with
  | .fuel => true
  | .stuck =>
    let (q, _) := QRun.runFor T c.limit (qstart c.t)
    stuckPair T q.final.spec && !q.run.crossed
  | .value v =>
    let (q, fin) := QRun.runFor T c.limit (qstart c.t)
    let st := q.final
    if !fin || st.crossed then false else
    let N := st.net.toNet
    match N.mate (st.root, 0) with
    | none => false
    | some p =>
      (readbackB (4 * v.size + 1000) N [] p (4 * v.size + 1000)).1 == some v &&
      !stuckPair T st.spec &&
      (let seen := reachable N 10000000 [st.root] []; st.ids.all seen.contains)

/-- How many times the runner goes round (firing, removing a forwarder, or finding nothing to do at
an agent) before the run ends, up to n. -/
def rounds (T : List (GRule K)) : Nat → QRun → Nat → Nat
  | 0, _, acc => acc
  | n + 1, q, acc => match q.step T with
    | (q', true) => rounds T n q' (acc + 1)
    | (_, false) => acc

/-- A program with the reference's outcome and a limit of four times the rounds the table's own run
takes. The net is run first: a program it cannot finish in `most` rounds is left out, so the
reference (whose fuel bounds depth, not work) is only asked about runs known to be short. -/
def mkCase (source : String) (fuel most : Nat) (t : Tm) : Option Case :=
  let n := rounds gtable most (qstart t) 0
  if n ≥ most then none else
  match eval fuel t with
  | .fuel => none
  | want => some ⟨source, t, want, 4 * n + 200⟩

/-- Cheap programs first, so a wrong wiring is caught early. -/
def cases (_ : Unit) : List Case :=
  let small := (upTo 8).filterMap (mkCase "small" 300 60000)
  let opened := (upToO 7).filterMap (mkCase "small, with an opened function" 300 60000)
  let hand := (programs.filter fun p => !(p.1.startsWith "interpret") && p.1 != "binders in size itself").filterMap
    fun p => mkCase "hand-written" 100000 20000000 p.2
  let rand := randomReaders.filterMap (mkCase "random reader" 2000 20000)
  let all := small ++ opened ++ hand ++ rand
  (all.toArray.qsort fun a b => a.limit < b.limit).toList

/-- The programs kept for the wirings that pass everything else. -/
def heavyCases (_ : Unit) : List Case :=
  ((programs.filter fun p => p.1.startsWith "interpret" || p.1 == "binders in size itself") ++
    longPrograms.filter (·.1 == "interpret the interpreter")).filterMap
    fun p => mkCase "hand-written (long)" 100000 20000000 p.2

/-! ## Testing every rewiring of a rule -/

/-- Does the rule ever fire when the table runs this program? Found by running the table without
the rule: its pair then sits in the net, stuck, if it ever forms. The runner is deterministic, so
a rewiring of a rule that never fires on a program runs that program exactly as the table does:
those programs are skipped for it. -/
def fires (r : Rule) (c : Case) : Bool :=
  let T := gtable.filter fun g => !(g.a == r.a && g.b == r.b)
  let (q, fin) := QRun.runFor T c.limit (qstart c.t)
  let N := q.run.net.toNet
  !fin || q.final.ids.any fun x =>
    match N.kind x, N.mate (x, 0) with
    | some (.base kx), some (y, 0) =>
      match N.kind y with
      | some (.base ky) => (kx == r.a && ky == r.b) || (kx == r.b && ky == r.a)
      | _ => false
    | _, _ => false

def withWires (r : Rule) (ws : List (End × End)) : List (GRule K) :=
  gtable.map fun g => if g.a == r.a && g.b == r.b then { g with wires := ws } else g

structure Report where
  rule : Rule
  /-- how many test programs the rule fires on -/
  uses : Nat
  wirings : Nat
  classes : Nat
  /-- how many classes each source of programs caught first -/
  caught : List (String × Nat)
  survivors : List (List (End × End))

def isView (r : Rule) : Bool := r.a == R

def examine (cs heavy : List Case) (r : Rule) : Report :=
  let cs := cs.filter (fires r)
  let heavy := heavy.filter (fires r)
  let classes := r.classes
  let (caught, survivors) := classes.foldl (fun (acc : Std.HashMap String Nat × List (List (End × End))) ws =>
    let T := withWires r ws
    match cs.find? (fun c => !c.passes T) with
    | some c => (acc.1.insert c.source (acc.1.getD c.source 0 + 1), acc.2)
    | none =>
      match heavy.find? (fun c => !c.passes T) with
      | some c => (acc.1.insert c.source (acc.1.getD c.source 0 + 1), acc.2)
      | none => (acc.1, ws :: acc.2)) ({}, [])
  ⟨r, cs.length + heavy.length, r.rewirings.length, classes.length, caught.toList, survivors.reverse⟩

def showEnd : End → String
  | .left n => s!"a{n}"
  | .right n => s!"b{n}"
  | .new j n => s!"m{j}.{n}"

def showWires (ws : List (End × End)) : String :=
  " ".intercalate (ws.map fun w => s!"{showEnd w.1}–{showEnd w.2}")

def Report.show (rep : Report) : String :=
  let r := rep.rule
  let rens := r.renamings
  let own := r.canon rens r.wires
  let forced := rep.survivors.length == 1 && rep.survivors.all fun ws => r.canon rens ws == own
  let head := s!"{repr r.a} ⋈ {repr r.b} makes {repr r.made}: fires on {rep.uses} programs; {rep.wirings} wirings, {rep.classes} up to renaming, " ++
    s!"{rep.survivors.length} pass" ++
    (if rep.uses == 0 then " (no program uses this rule)" else if forced then " (the table's own: forced)" else " (a spare wiring)")
  let caught := if rep.caught.isEmpty then "" else
    "\n    caught first by: " ++ ", ".intercalate (rep.caught.map fun (s, n) => s!"{s} {n}")
  let extra := if forced then "" else
    "\n    table:    " ++ showWires r.wires ++
    "".intercalate (rep.survivors.map fun ws =>
      "\n    survivor: " ++ showWires ws ++ (if r.canon rens ws == own then "   (the table's)" else ""))
  head ++ caught ++ extra

def main (args : List String) : IO Unit := do
  let cs := cases ()
  let heavy := heavyCases ()
  let failing := (cs ++ heavy).filter fun c => !c.passes gtable
  let count := fun (src : String) => (cs.filter (·.source == src)).length
  IO.println s!"{cs.length} test programs ({count "small"} small, {count "small, with an opened function"} small with an opened function, {count "hand-written"} hand-written, {count "random reader"} random readers), plus {heavy.length} long ones for the wirings that pass those; the table itself fails {failing.length}"
  let rules := table.filter fun r => !isView r && (args.isEmpty || args.contains s!"{repr r.a}⋈{repr r.b}")
  (← IO.getStdout).flush
  let tasks := rules.map fun r => Task.spawn fun _ => examine cs heavy r
  let mut spare := 0
  let mut unused := 0
  for t in tasks do
    let rep := t.get
    IO.println rep.show
    (← IO.getStdout).flush
    if rep.survivors.length != 1 then spare := spare + 1
    if rep.uses == 0 then unused := unused + 1
  IO.println s!"{rules.length} rules examined: {spare} with a spare wiring, {unused} that no program uses"
