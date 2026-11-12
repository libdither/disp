import Search
import Std.Data.HashMap

/-! The per-design checks, run inside Lean on the same `fire` the theorems are about: compile a
program, run it, read the answer back, and compare with a reference interpreter; check every
cancel joins the two ends of one copy, and that a finished run leaves no garbage. -/

/-! ## Terms and the reference interpreter -/

/-- Programs and values. Variables are de Bruijn indices. `Lam` is an opened function; `splice v`
is a captured closed value inside code. -/
inductive Tm
  | var (i : Nat)
  | app (f a : Tm)
  | lam (b : Tm)
  | Lam (b : Tm)
  | rho
  | splice (v : Tm)
  deriving DecidableEq, Repr, Inhabited

namespace Tm
/-- Replace variable k (and nothing else) by a splice of the closed value v. -/
def subst (v : Tm) : Tm → Nat → Tm
  | var i, k => if i = k then splice v else var i
  | app f a, k => app (subst v f k) (subst v a k)
  | lam b, k => lam (subst v b (k + 1))
  | Lam b, k => Lam (subst v b (k + 1))
  | t, _ => t

/-- A view: λk1 … k5. (the arm for this kind) (the other arms) (the parts). -/
def view5 (arm : Nat) (parts : List Tm) : Tm :=
  let spine := (([1, 2, 3, 4, 5].filter (· != arm)).foldl (fun t k => app t (var (5 - k))) (var (5 - arm)))
  lam (lam (lam (lam (lam (parts.foldl (fun t p => app t (splice p)) spine)))))
end Tm

inductive Outcome
  | value (v : Tm)
  | stuck
  | fuel
  deriving DecidableEq, Repr

open Tm in
/-- Call by value, never under a binder. -/
def eval : Nat → Tm → Outcome
  | 0, _ => .fuel
  | n + 1, t =>
    match t with
    | lam b => .value (lam b)
    | Lam b => .value (Lam b)
    | rho => .value rho
    | splice v => .value v
    | var _ => .stuck
    | app f a =>
      match eval n f with
      | .value fv =>
        match eval n a with
        | .value av =>
          match fv with
          | lam b => eval n (subst av b 0)
          | Lam b => .value (subst av b 0)
          | rho =>
            match av with
            | lam b => .value (view5 1 [Lam b])
            | Lam b => .value (view5 2 [lam b])
            | app c1 c2 => .value (view5 3 [c1, c2])
            | splice w => .value (view5 4 [w])
            | rho => .value (view5 5 [])
            | var _ => .stuck
          | _ => .stuck
        | o => o
      | o => o

/-! ## Compiling a term to a net -/

structure Build where
  kinds : List (Ident × Ag K) := []
  wires : List (Port × Port) := []
  next : Nat := 0

def Build.add (b : Build) (k : Ag K) : Build × Ident :=
  let x := Ident.root b.next
  ({ b with kinds := (x, k) :: b.kinds, next := b.next + 1 }, x)

def Build.link (b : Build) (p q : Port) : Build := { b with wires := (p, q) :: b.wires }

def modAt {α : Type} (f : α → α) : List α → Nat → List α
  | [], _ => []
  | x :: xs, 0 => f x :: xs
  | x :: xs, n + 1 => x :: modAt f xs n

/-- Wire a binder's port to its uses: an eraser for none, sharers for several. -/
def bindUses (b : Build) (port : Port) : List Port → Build
  | [] => let (b, e) := b.add (.base .eraI); b.link port (e, 0)
  | [u] => b.link port u
  | u :: us =>
    let (b, s) := b.add (.base .sig)
    bindUses ((b.link port (s, 0)).link (s, 1) u) (s, 2) us

/-- Emit code for a term; env collects each enclosing binder's uses. -/
def emit : Tm → Build → List (List Port) → Build × Port × List (List Port)
  | .var i, b, env =>
    let (b, v) := b.add (.base .var)
    (b, (v, 0), modAt (fun us => us ++ [(v, 1)]) env i)
  | .app f a, b, env =>
    let (b, x) := b.add (.base .app)
    let (b, pf, env) := emit f b env
    let (b, pa, env) := emit a b env
    ((b.link (x, 1) pf).link (x, 2) pa, (x, 0), env)
  | .lam body, b, env =>
    let (b, l) := b.add (.base .lam)
    let (b, pb, env') := emit body b ([] :: env)
    (bindUses (b.link (l, 2) pb) (l, 1) env'.head!, (l, 0), env'.tail)
  | .Lam body, b, env =>
    let (b, l) := b.add (.base .Lam)
    let (b, pb, env') := emit body b ([] :: env)
    (bindUses (b.link (l, 2) pb) (l, 1) env'.head!, (l, 0), env'.tail)
  | .rho, b, env =>
    let (b, x) := b.add (.base .rho)
    (b, (x, 0), env)
  | .splice v, b, env =>
    let (b, x) := b.add (.base .var)
    let (b, pv, _) := emit v b []
    (b.link (x, 1) pv, (x, 0), env)

def lookupP (ws : List (Port × Port)) (p : Port) : Option Port :=
  match ws.find? (fun w => w.1 == p) with
  | some w => some w.2
  | none => (ws.find? (fun w => w.2 == p)).map (·.1)

/-- A program under a thaw, with its answer arriving at port 0 of a root forwarder. -/
def loadB (t : Tm) : Build × Ident :=
  let (b, pr, _) := emit t {} []
  let (b, th) := b.add (.base .T)
  let (b, root) := b.add (.fwd .i)
  ((b.link (th, 0) pr).link (th, 1) (root, 0), root)

def Build.net (b : Build) : Net K := ⟨fun x => (b.kinds.find? (·.1 == x)).map (·.2), lookupP b.wires⟩

/-! ## Running -/

def Ag.arity : Ag K → Nat
  | .base k => k.arity
  | .fwd _ => 1

structure Run where
  net : Net K
  ids : List Ident
  root : Ident
  ops : List (Ident × Nat) := []
  nextOp : Nat := 0
  crossed : Bool := false
  steps : Nat := 0

def ruleFor (T : List (GRule K)) (x y : K) : Option (GRule K) :=
  T.find? (fun r => r.a == x && r.b == y)

/-- The first active pair with a rule, oriented the rule's way. -/
def findPair (T : List (GRule K)) (st : Run) : Option (Ident × Ident × GRule K) :=
  st.ids.findSome? fun x =>
    match st.net.kind x, st.net.mate (x, 0) with
    | some (.base kx), some (y, 0) =>
      match st.net.kind y with
      | some (.base ky) =>
        match ruleFor T kx ky with
        | some r => some (x, y, r)
        | none => none
      | _ => none
    | _, _ => none

/-- An active pair that no rule matches: the run is stuck there. -/
def stuckPair (T : List (GRule K)) (st : Run) : Bool :=
  st.ids.any fun x =>
    match st.net.kind x, st.net.mate (x, 0) with
    | some (.base kx), some (y, 0) =>
      match st.net.kind y with
      | some (.base ky) => (ruleFor T kx ky).isNone && (ruleFor T ky kx).isNone
      | _ => false
    | _, _ => false

def opOf (st : Run) (x : Ident) : Nat := ((st.ops.find? (·.1 == x)).map (·.2)).getD 0

def isCopy (r : GRule K) : Bool :=
  r.a == .sig || (r.a == .dupI && r.b != .dupO) || (r.a == .dupO && r.b == .sig)

def fireStep (st : Run) (a b : Ident) (r : GRule K) : Run :=
  let made := (List.range (r.made.length + r.wires.length)).filterMap fun k =>
    (madeKind sig r k).map fun g => (Ident.made a b k, g)
  let newIds := made.map (·.1)
  let crossed := st.crossed || (r.a == .dupI && r.b == .dupO && opOf st a != opOf st b)
  let (op, nextOp) := if r.a == .sig then (st.nextOp, st.nextOp + 1) else (opOf st a, st.nextOp)
  let newOps := if isCopy r then
      made.filterMap fun (x, g) => match g with
        | .base .dupI | .base .dupO => some (x, op)
        | _ => none
    else []
  { st with net := fire sig r a b st.net, ids := st.ids.filter (fun x => x != a && x != b) ++ newIds,
            ops := newOps ++ st.ops, nextOp := nextOp, crossed := crossed, steps := st.steps + 1 }

def isFwd (st : Run) (x : Ident) : Bool :=
  x != st.root && match st.net.kind x with | some (.fwd _) => true | _ => false

/-- Remove a forwarder if there is one, else fire a pair; none when the run has finished. -/
def step (T : List (GRule K)) (st : Run) : Option Run :=
  match st.ids.find? (isFwd st) with
  | some x => some { st with net := unfwd x st.net, ids := st.ids.filter (· != x) }
  | none =>
    match findPair T st with
    | some (a, b, r) => some (fireStep st a b r)
    | none => none

def runFor (T : List (GRule K)) : Nat → Run → Run × Bool
  | 0, st => (st, false)
  | n + 1, st => match step T st with
    | some st' => runFor T n st'
    | none => (st, true)

/-! ## Reading the answer back, and the checks -/

/-- Follow a variable's wire back through sharers and forwarders to its binder or captured value. -/
def walk : Nat → Net K → Port → Option Port
  | 0, _, _ => none
  | n + 1, N, s =>
    match N.kind s.1 with
    | some (.base .sig) => if s.2 = 1 ∨ s.2 = 2 then (N.mate (s.1, 0)).bind (walk n N) else some s
    | some (.fwd _) => (N.mate (s.1, if s.2 = 0 then 1 else 0)).bind (walk n N)
    | _ => some s

def readback : Nat → Net K → List Port → Port → Option Tm
  | 0, _, _, _ => none
  | n + 1, N, bs, p =>
    match N.kind p.1 with
    | some (.fwd _) => (N.mate (p.1, if p.2 = 0 then 1 else 0)).bind (readback n N bs)
    | some (.base k) =>
      if p.2 ≠ 0 then none else
      match k with
      | .lam => (N.mate (p.1, 2)).bind (readback n N ((p.1, 1) :: bs)) |>.map .lam
      | .Lam => (N.mate (p.1, 2)).bind (readback n N ((p.1, 1) :: bs)) |>.map .Lam
      | .app => do
        let f ← (N.mate (p.1, 1)).bind (readback n N bs)
        let a ← (N.mate (p.1, 2)).bind (readback n N bs)
        pure (.app f a)
      | .rho => some .rho
      | .var => do
        let src ← (N.mate (p.1, 1)).bind (walk n N)
        match bs.findIdx? (· == src) with
        | some i => pure (.var i)
        | none => if src.2 = 0 then (readback n N [] src).map .splice else none
      | _ => none
    | none => none

def reachable (N : Net K) : Nat → List Ident → List Ident → List Ident
  | 0, _, seen => seen
  | _ + 1, [], seen => seen
  | n + 1, x :: todo, seen =>
    if seen.contains x then reachable N n todo seen else
    let ar := ((N.kind x).map Ag.arity).getD 0
    let next := (List.range (ar + 1)).filterMap fun i => (N.mate (x, i)).map (·.1)
    reachable N n (next ++ todo) (x :: seen)

def start (t : Tm) : Run :=
  let lb := loadB t
  { net := lb.1.net, ids := lb.1.kinds.map (·.1), root := lb.2 }

/-- One program: the net agrees with the reference (same answer, or stuck where it is stuck), no
cancel joins two different copies, and a finished run leaves only its answer. Programs the
reference can't finish within its fuel are skipped. -/
def checkWith (T : List (GRule K)) (fuel : Nat) (t : Tm) : Bool :=
  match eval fuel t with
  | .fuel => true
  | .stuck =>
    let (st, _) := runFor T (20 * fuel) (start t)
    stuckPair T st && !st.crossed
  | .value v =>
    let (st, fin) := runFor T (20 * fuel) (start t)
    let ans := (st.net.mate (st.root, 0)).bind (readback 10000 st.net [])
    let seen := reachable st.net 100000 [st.root] []
    fin && !stuckPair T st && ans == some v && !st.crossed && st.ids.all seen.contains

def checkOne := checkWith gtable

/-- Every closed term of a given size, with ρ allowed as a leaf. -/
partial def terms (size depth : Nat) : List Tm :=
  if size = 0 then [] else
  if size = 1 then .rho :: (List.range depth).map .var else
  (terms (size - 1) (depth + 1)).map .lam ++
  (List.range (size - 2)).flatMap fun k =>
    (terms (k + 1) depth).flatMap fun f => (terms (size - 2 - k) depth).map fun a => .app f a

def upTo (n : Nat) : List Tm := (List.range n).flatMap fun s => terms (s + 1) 0

/-- The same, with opened functions written out as well (applying one hands back its body as
code, so a written-out opened function is a quote). -/
partial def termsO (size depth : Nat) : List Tm :=
  if size = 0 then [] else
  if size = 1 then .rho :: (List.range depth).map .var else
  (termsO (size - 1) (depth + 1)).flatMap (fun b => [.lam b, .Lam b]) ++
  (List.range (size - 2)).flatMap fun k =>
    (termsO (k + 1) depth).flatMap fun f => (termsO (size - 2 - k) depth).map fun a => .app f a

def Tm.hasOpened : Tm → Bool
  | .Lam _ => true
  | .lam b => b.hasOpened
  | .app f a => f.hasOpened || a.hasOpened
  | _ => false

/-- Every closed program up to a size that writes out at least one opened function. -/
def upToO (n : Nat) : List Tm := ((List.range n).flatMap fun s => termsO (s + 1) 0).filter Tm.hasOpened


/-! ## A faster runner for long programs

The same step, on hash maps: only the ports a step touches are recomputed, each with the very
`fire` (or `unfwd`) the theorems are about. On good nets, where wires are symmetric, those are
the only ports whose wiring changes; `agreesWithSpec` checks the two runners in lockstep. -/

deriving instance Hashable for Ident
deriving instance DecidableEq for Ag

structure FNet where
  kinds : Std.HashMap Ident (Ag K)
  mates : Std.HashMap Port Port

def FNet.toNet (F : FNet) : Net K := ⟨fun x => F.kinds[x]?, fun p => F.mates[p]?⟩

def portsOf (N : Net K) (x : Ident) : List Port :=
  (List.range ((((N.kind x).map Ag.arity).getD 0) + 1)).map fun i => (x, i)

/-- Recompute the given ports from M (the net after the step), dropping ports of removed agents. -/
def FNet.patch (F : FNet) (gone : List Port) (redo : List Port) (M : Net K) : FNet :=
  let mates := gone.foldl (fun m p => m.erase p) F.mates
  let mates := redo.foldl (fun m p => match M.mate p with
    | some q => m.insert p q
    | none => m.erase p) mates
  { F with mates }

def FNet.fire (F : FNet) (a b : Ident) (r : GRule K) : FNet :=
  let N := F.toNet
  let M := _root_.fire sig r a b N
  let made := (List.range (r.made.length + r.wires.length)).filterMap fun k =>
    (madeKind sig r k).map fun g => (Ident.made a b k, g)
  let kinds := made.foldl (fun m (x, g) => m.insert x g) ((F.kinds.erase a).erase b)
  let gone := portsOf N a ++ portsOf N b
  let nbrs := (gone.filterMap N.mate).filter fun q => q.1 != a && q.1 != b
  let newPorts := made.flatMap fun (x, g) => (List.range (g.arity + 1)).map fun i => (x, i)
  ({ F with kinds } : FNet).patch gone (nbrs ++ newPorts) M

def FNet.unfwd (F : FNet) (x : Ident) : FNet :=
  let N := F.toNet
  let gone := portsOf N x
  let nbrs := (gone.filterMap N.mate).filter fun q => q.1 != x
  ({ F with kinds := F.kinds.erase x } : FNet).patch gone nbrs (_root_.unfwd x N)

structure FRun where
  net : FNet
  ids : List Ident
  root : Ident
  ops : Std.HashMap Ident Nat := {}
  nextOp : Nat := 0
  crossed : Bool := false
  steps : Nat := 0

/-- The spec runner's state, seen through the fast one. -/
def FRun.spec (st : FRun) : Run :=
  { net := st.net.toNet, ids := st.ids, root := st.root, ops := st.ops.toList,
    nextOp := st.nextOp, crossed := st.crossed, steps := st.steps }

def FRun.step (T : List (GRule K)) (st : FRun) : Option FRun :=
  let s : Run := { net := st.net.toNet, ids := st.ids, root := st.root }
  match st.ids.find? (isFwd s) with
  | some x => some { st with net := st.net.unfwd x, ids := st.ids.filter (· != x) }
  | none =>
    match findPair T s with
    | some (a, b, r) =>
      let made := (List.range (r.made.length + r.wires.length)).filterMap fun k =>
        (madeKind sig r k).map fun g => (Ident.made a b k, g)
      let opA := st.ops.getD a 0
      let crossed := st.crossed || (r.a == .dupI && r.b == .dupO && opA != st.ops.getD b 0)
      let (op, nextOp) := if r.a == .sig then (st.nextOp, st.nextOp + 1) else (opA, st.nextOp)
      let ops := (st.ops.erase a).erase b
      let ops := if isCopy r then
          made.foldl (fun m (x, g) => match g with
            | .base .dupI | .base .dupO => m.insert x op
            | _ => m) ops
        else ops
      some { st with net := st.net.fire a b r, ids := st.ids.filter (fun x => x != a && x != b) ++ made.map (·.1),
                     ops, nextOp, crossed, steps := st.steps + 1 }
    | none => none

def FRun.runFor (T : List (GRule K)) : Nat → FRun → FRun × Bool
  | 0, st => (st, false)
  | n + 1, st => match st.step T with
    | some st' => FRun.runFor T n st'
    | none => (st, true)

def fstart (t : Tm) : FRun :=
  let (b, root) := loadB t
  let mates := b.wires.foldl (fun m (p, q) => (m.insert p q).insert q p) {}
  { net := ⟨Std.HashMap.ofList b.kinds, mates⟩, ids := b.kinds.map (·.1), root }

/-! The work-list runner: after a step only the agents it touched can have formed an active pair,
so only those are looked at. It fires pairs in a different order than the spec runner, which the
confluence theorems say doesn't change the result. -/

structure QRun where
  run : FRun
  todo : List Ident
  fwds : List Ident
  /-- the next unused root name: made agents are renamed to short names, since names built
  from their parents' names grow exponentially and hashing them would too -/
  fresh : Nat

def touched (N : Net K) (xs : List Ident) : List Ident :=
  (xs.flatMap (portsOf N)).filterMap fun p => (N.mate p).map (·.1)

/-- One step, and whether there was one to take. The state is handed back either way, and taken
apart before anything is read from it, so the caller never holds a second reference and the maps are
updated in place instead of copied: everything a step needs from the net is read before the maps
change. `run.ids` is left alone (`QRun.final` rebuilds it). -/
def QRun.step (T : List (GRule K)) (q : QRun) : QRun × Bool :=
  match q with
  | ⟨⟨⟨kinds, mates⟩, ids, root, ops, nextOp, crossed, steps⟩, todo, fwds, fresh⟩ =>
  match fwds with
  | x :: rest =>
    if x != root && (match kinds[x]? with | some (.fwd _) => true | _ => false) then
      let N : Net K := ⟨fun x => kinds[x]?, fun p => mates[p]?⟩
      let gone := portsOf N x
      let nbrs := (gone.filterMap N.mate).filter fun p => p.1 != x
      let M := _root_.unfwd x N
      let ups := nbrs.map fun p => (p, M.mate p)
      let near := touched N [x]
      let mates := gone.foldl (fun m p => m.erase p) mates
      let mates := ups.foldl (fun m (p, u) => match u with | some v => m.insert p v | none => m.erase p) mates
      (⟨⟨⟨kinds.erase x, mates⟩, ids, root, ops, nextOp, crossed, steps⟩, near ++ todo, rest, fresh⟩, true)
    else (⟨⟨⟨kinds, mates⟩, ids, root, ops, nextOp, crossed, steps⟩, todo, rest, fresh⟩, true)
  | [] =>
    match todo with
    | [] => (⟨⟨⟨kinds, mates⟩, ids, root, ops, nextOp, crossed, steps⟩, [], [], fresh⟩, false)
    | x :: rest =>
      let pair := match kinds[x]?, mates[(x, 0)]? with
        | some (.base kx), some (y, 0) =>
          match kinds[y]? with
          | some (.base ky) =>
            match ruleFor T kx ky, ruleFor T ky kx with
            | some r, _ => some (x, y, r)
            | none, some r => some (y, x, r)
            | none, none => none
          | _ => none
        | _, _ => none
      match pair with
      | none => (⟨⟨⟨kinds, mates⟩, ids, root, ops, nextOp, crossed, steps⟩, rest, [], fresh⟩, true)
      | some (a, b, r) =>
        let N : Net K := ⟨fun x => kinds[x]?, fun p => mates[p]?⟩
        let near := (touched N [a, b]).filter fun z => z != a && z != b
        let made := (List.range (r.made.length + r.wires.length)).filterMap fun k =>
          (madeKind sig r k).map fun g => (k, g)
        let short := fun (k : Nat) => Ident.root (fresh + k)
        let ren : Port → Port := fun p => match madeIdx a b p.1 with
          | some k => (short k, p.2)
          | none => p
        let M := _root_.fire sig r a b N
        let gone := portsOf N a ++ portsOf N b
        let upsNear : List (Port × Option Port) := near.flatMap fun z => (portsOf N z).filterMap fun p =>
          match N.mate p with
          | some t => if t.1 == a || t.1 == b then some (p, (M.mate p).map ren) else none
          | none => none
        let upsMade : List (Port × Port) := made.flatMap fun (k, g) =>
          (List.range (g.arity + 1)).filterMap fun i =>
            (M.mate (Ident.made a b k, i)).map fun u => ((short k, i), ren u)
        let mates := gone.foldl (fun m p => m.erase p) mates
        let mates := upsNear.foldl (fun m (p, u) => match u with | some v => m.insert p v | none => m.erase p) mates
        let mates := upsMade.foldl (fun m (p, v) => m.insert p v) mates
        let kinds := made.foldl (fun m (k, g) => m.insert (short k) g) ((kinds.erase a).erase b)
        let opA := ops.getD a 0
        let crossed := crossed || (r.a == .dupI && r.b == .dupO && opA != ops.getD b 0)
        let (op, nextOp) := if r.a == .sig then (nextOp, nextOp + 1) else (opA, nextOp)
        let ops := (ops.erase a).erase b
        let ops := if isCopy r then
            made.foldl (fun m (k, g) => match g with
              | .base .dupI | .base .dupO => m.insert (short k) op
              | _ => m) ops
          else ops
        let newIds := made.map fun (k, _) => short k
        let fw := made.filterMap fun (k, g) => match g with | .fwd _ => some (short k) | _ => none
        (⟨⟨⟨kinds, mates⟩, ids, root, ops, nextOp, crossed, steps + 1⟩, newIds ++ near ++ rest, fw,
              fresh + r.made.length + r.wires.length⟩, true)

/-- The run's state with the list of live agents rebuilt from the net. -/
def QRun.final (q : QRun) : FRun := { q.run with ids := q.run.net.kinds.keys }

def QRun.runFor (T : List (GRule K)) : Nat → QRun → QRun × Bool
  | 0, q => (q, false)
  | n + 1, q => match q.step T with
    | (q', true) => QRun.runFor T n q'
    | (q', false) => (q', true)

def qstart (t : Tm) : QRun :=
  let st := fstart t
  { run := st, todo := st.ids, fresh := st.ids.length,
    fwds := st.ids.filter fun x => match (st.net.toNet).kind x with | some (.fwd _) => true | _ => false }

/-- Same checks as `checkWith`, on the work-list runner. -/
def fastCheckWith (T : List (GRule K)) (fuel : Nat) (t : Tm) : Bool :=
  match eval fuel t with
  | .fuel => true
  | .stuck =>
    let (q, _) := QRun.runFor T (200 * fuel) (qstart t)
    stuckPair T q.final.spec && !q.run.crossed
  | .value v =>
    let (q, fin) := QRun.runFor T (200 * fuel) (qstart t)
    let st := q.final
    let N := st.net.toNet
    let ans := (N.mate (st.root, 0)).bind (readback 1000000 N [])
    let seen := reachable N 10000000 [st.root] []
    fin && !stuckPair T st.spec && ans == some v && !st.crossed && st.ids.all seen.contains

/-- The two runners agree on every agent and wire after every step. -/
def agreesWithSpec (T : List (GRule K)) (fuel : Nat) (t : Tm) : Bool :=
  let rec go : Nat → Run → FRun → Bool
    | 0, _, _ => true
    | n + 1, s, f =>
      let fn := f.net.toNet
      let same := s.ids == f.ids && s.ids.all fun x =>
        s.net.kind x == fn.kind x && (portsOf s.net x).all fun p => s.net.mate p == fn.mate p
      same && match step T s, f.step T with
        | some s', some f' => go n s' f'
        | none, none => true
        | _, _ => false
  go fuel (start t) (fstart t)
