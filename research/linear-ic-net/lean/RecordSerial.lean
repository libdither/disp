import Statement

/-! # The serializer record: a plain λ-machine with two kinds that turn code into data (v0.2)

What this is: a plain λ-machine (10 of the quote net's kinds and 17 of its rules) with two kinds bolted on
whose 8 rules serialise frozen code into its `encode` data. It was built by the third adversarial review
of the statement (its probes `P5`, `P7`, ported here) to show what v0.2 rewards, and it beats
both earlier records in every column. It is kept as the current best record under v0.2 and as a regression
case for later versions of the statement. Whether it counts as a system that reads its own code is the
owner's call: no program reads anything, the reading is all in the rules.

- Kinds: lam app var, O (apply), T (thaw), sig (sharer), dupI/dupO (copiers), eraI/eraO (erasers), all as
  in the quote net (`QuoteKit.lean`), and two new ones: R, the reader (principal, code out, depth in), and
  N, a printer from a depth chain to a Scott numeral's code. A depth is a chain of `var` agents ending in
  an `eraO`, shared by the quote net's own sig ⋈ var and copied at its end by the new dupI ⋈ eraO.
- `reify` (3 agents): λq. R q eraO, reading at depth 0. `run` (2 agents): λq. T q, the thaw, which
  annihilates the quote's `var` wrapper and starts the code.
- The 8 new rules:
  - R ⋈ lam writes λa b c. c ⌜body⌝ (two erasers for a, b); the depth gains one `var`; that chain is
    shared between the binder's uses (the token) and a new R reading the body.
  - R ⋈ app writes λa b c. b ⌜f⌝ ⌜x⌝, the depth shared between two new R's.
  - R ⋈ var (a use, whose wire leads to its binder's token) writes λa b c. a ⌜i⌝ with an O agent subtracting the token
    from the depth: O ⋈ var peels one link and swaps the two chains, O ⋈ eraO hands the chain that is left
    to N. Both pairs were stuck in the quote net, so O does double duty. The answer is the de Bruijn index.
  - N ⋈ eraO writes λz s. z; N ⋈ var writes λz s. s ⌜rest⌝ and prints the rest.
  - dupI ⋈ eraO copies a chain's end.
- Bits: signature 69, rule count and rules 634, gadget 27, `reify` 21, `run` 13; one ceiling: 762.

Proved here (layout as `RecordQuote.lean`):
- `serial_ok : Ok ssys` by `decide`; `serial_gadget_ok`, `serial_kit_ok` by `decide`.
- `serial_bit_terms`, `serial_bits` by `decide +kernel`.
- `serial_jobs`, `serial_runs` (every battery job's moves, rounds, records, and the totals) and
  `serial_tested : MeetsTested ssys sg skit 9` by `native_decide`.
- `serial_record : Achieved (.tested 9) ⟨762, 22682, 1775, 26139⟩`.
- With `StatementSound.lean`'s `Record.tested_spec`, the test gives the spec's `Answers` on every tested term.

`#print axioms serial_record`:
  [propext, Classical.choice, Quot.sound, SerialRec.serial_runs._native.native_decide.ax_1_1,
   SerialRec.serial_tested._native.native_decide.ax_1_1]
(the last two are the two `native_decide` facts, trusted to the compiler; nothing else is assumed.)

Measured by the review, not proved here: every closed term of sizes 10 and 11 passes; about 20,440 of the
26,139 record bits come from three rules that make no agent. Not proved: `Meets`.
-/

namespace SerialRec
open Statement

/-- 12 kinds: the quote net's lam app var O T sig dupI dupO eraI eraO, the reader R, the printer N. -/
inductive Sk
  | lam | app | var | O | T | sig | dupI | dupO | eraI | eraO | R | N
  deriving DecidableEq, Repr

def Sk.all : List Sk := [.lam, .app, .var, .O, .T, .sig, .dupI, .dupO, .eraI, .eraO, .R, .N]

/-- Port directions, principal first. -/
def Sk.ports : Sk → List Pol
  | .lam => [.o, .o, .i]
  | .app => [.o, .i, .i]
  | .var => [.o, .i]
  | .O => [.i, .i, .o]
  | .T => [.i, .o]
  | .sig => [.i, .o, .o]
  | .dupI => [.i, .o, .o]
  | .dupO => [.o, .i, .i]
  | .eraI => [.i]
  | .eraO => [.o]
  | .R => [.i, .o, .i]
  | .N => [.i, .o]

def ssig : Sig Sk := ⟨Sk.ports⟩

/-- The 17 kept rules of the quote net, then the 8 new ones. -/
def stable : List (GRule Sk) := [
  ⟨.O, .lam, [.T], [(.left 1, .right 1), (.new 0 0, .right 2), (.new 0 1, .left 2)]⟩,
  ⟨.T, .app, [.O, .T, .T], [(.new 1 0, .right 1), (.new 1 1, .new 0 0), (.new 2 0, .right 2), (.new 2 1, .new 0 1), (.new 0 2, .left 1)]⟩,
  ⟨.T, .var, [], [(.left 1, .right 1)]⟩,
  ⟨.T, .lam, [.lam], [(.new 0 0, .left 1), (.new 0 1, .right 1), (.new 0 2, .right 2)]⟩,
  ⟨.sig, .lam, [.lam, .lam, .dupO, .dupI], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1), (.new 3 0, .right 2), (.new 3 1, .new 0 2), (.new 3 2, .new 1 2)]⟩,
  ⟨.sig, .var, [.var, .var, .dupI], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1)]⟩,
  ⟨.dupI, .lam, [.lam, .lam, .dupO, .dupI], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1), (.new 3 0, .right 2), (.new 3 1, .new 0 2), (.new 3 2, .new 1 2)]⟩,
  ⟨.dupI, .app, [.app, .app, .dupI, .dupI], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1), (.new 3 0, .right 2), (.new 3 1, .new 0 2), (.new 3 2, .new 1 2)]⟩,
  ⟨.dupI, .var, [.var, .var, .dupI], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1)]⟩,
  ⟨.dupO, .sig, [.sig, .sig, .dupO, .dupO], [(.new 0 0, .left 1), (.new 1 0, .left 2), (.new 2 0, .right 1), (.new 2 1, .new 0 1), (.new 2 2, .new 1 1), (.new 3 0, .right 2), (.new 3 1, .new 0 2), (.new 3 2, .new 1 2)]⟩,
  ⟨.dupI, .dupO, [], [(.left 1, .right 1), (.left 2, .right 2)]⟩,
  ⟨.eraI, .lam, [.eraO, .eraI], [(.new 0 0, .right 1), (.new 1 0, .right 2)]⟩,
  ⟨.eraI, .app, [.eraI, .eraI], [(.new 0 0, .right 1), (.new 1 0, .right 2)]⟩,
  ⟨.eraI, .var, [.eraI], [(.new 0 0, .right 1)]⟩,
  ⟨.eraI, .dupO, [.eraI, .eraI], [(.new 0 0, .right 1), (.new 1 0, .right 2)]⟩,
  ⟨.eraO, .sig, [.eraO, .eraO], [(.new 0 0, .right 1), (.new 1 0, .right 2)]⟩,
  ⟨.eraO, .eraI, [], []⟩,
  -- R ⋈ lam: λa b c. c ⌜body⌝; the depth gains a `var`, shared between the binder's uses and the body's R
  ⟨.R, .lam, [.lam, .lam, .lam, .app, .var, .R, .eraI, .eraI, .var, .sig],
    [(.left 1, .new 0 0), (.new 0 1, .new 6 0), (.new 0 2, .new 1 0), (.new 1 1, .new 7 0), (.new 1 2, .new 2 0),
     (.new 2 1, .new 4 1), (.new 2 2, .new 3 0), (.new 3 1, .new 4 0), (.new 3 2, .new 5 1), (.new 5 0, .right 2),
     (.new 5 2, .new 9 1), (.new 9 2, .right 1), (.new 9 0, .new 8 0), (.new 8 1, .left 2)]⟩,
  -- R ⋈ app: λa b c. b ⌜f⌝ ⌜x⌝, the depth shared
  ⟨.R, .app, [.lam, .lam, .lam, .app, .app, .var, .R, .R, .eraI, .eraI, .sig],
    [(.left 1, .new 0 0), (.new 0 1, .new 8 0), (.new 0 2, .new 1 0), (.new 1 1, .new 5 1), (.new 1 2, .new 2 0),
     (.new 2 1, .new 9 0), (.new 2 2, .new 3 0), (.new 3 1, .new 4 0), (.new 3 2, .new 7 1), (.new 4 1, .new 5 0),
     (.new 4 2, .new 6 1), (.new 6 0, .right 1), (.new 7 0, .right 2), (.new 10 0, .left 2), (.new 10 1, .new 6 2),
     (.new 10 2, .new 7 2)]⟩,
  -- R ⋈ var: λa b c. a ⌜depth − token⌝, the subtraction left to an O
  ⟨.R, .var, [.lam, .lam, .lam, .app, .var, .O, .eraI, .eraI],
    [(.left 1, .new 0 0), (.new 0 1, .new 4 1), (.new 0 2, .new 1 0), (.new 1 1, .new 6 0), (.new 1 2, .new 2 0),
     (.new 2 1, .new 7 0), (.new 2 2, .new 3 0), (.new 3 1, .new 4 0), (.new 3 2, .new 5 2), (.new 5 0, .right 1),
     (.new 5 1, .left 2)]⟩,
  -- O ⋈ var: peel one link, swap the chains; O ⋈ eraO: the other chain is the answer, printed by N
  ⟨.O, .var, [.O], [(.new 0 0, .left 1), (.new 0 1, .right 1), (.new 0 2, .left 2)]⟩,
  ⟨.O, .eraO, [.N], [(.new 0 0, .left 1), (.new 0 1, .left 2)]⟩,
  -- N: a chain to a Scott numeral's code
  ⟨.N, .eraO, [.lam, .lam, .var, .eraI],
    [(.left 1, .new 0 0), (.new 0 1, .new 2 1), (.new 0 2, .new 1 0), (.new 1 1, .new 3 0), (.new 1 2, .new 2 0)]⟩,
  ⟨.N, .var, [.lam, .lam, .app, .var, .eraI, .N],
    [(.left 1, .new 0 0), (.new 0 1, .new 4 0), (.new 0 2, .new 1 0), (.new 1 1, .new 3 1), (.new 1 2, .new 2 0),
     (.new 2 1, .new 3 0), (.new 2 2, .new 5 1), (.new 5 0, .right 1)]⟩,
  -- dupI ⋈ eraO: copy a chain's end
  ⟨.dupI, .eraO, [.eraO, .eraO], [(.new 0 0, .left 1), (.new 1 0, .left 2)]⟩]

def sstuck : List (Sk × Sk) :=
  [(.O, .app), (.O, .dupO), (.T, .dupO), (.T, .eraO), (.sig, .app), (.R, .dupO), (.R, .eraO), (.N, .lam),
   (.N, .app), (.N, .dupO)]

def ssys : System Sk := { kinds := Sk.all, sig := ssig, rules := stable, stuck := sstuck }

def sg : Gadget Sk :=
  { lam := .lam, app := .app, var := .var, share := .sig, erase := .eraI, thaw := .T, quote := .native .var }

/-- λq. R q eraO: 3 agents. -/
def reifyProg : Prog Sk :=
  { agents := [.lam, .R, .eraO],
    wires := [(.out, .port 0 0), (.port 0 1, .port 1 0), (.port 1 1, .port 0 2), (.port 1 2, .port 2 0)] }

/-- λq. T q: 2 agents. -/
def runProg : Prog Sk :=
  { agents := [.lam, .T],
    wires := [(.out, .port 0 0), (.port 0 1, .port 1 0), (.port 1 1, .port 0 2)] }

def skit : Kit Sk := { reify := reifyProg, run := runProg }

theorem s_tableOk : TableOk ssig stable :=
  ⟨by show ∀ r ∈ stable, ∀ w ∈ r.wires, r.wireOk ssig w = true; decide,
   by show ∀ r ∈ stable, ∀ w ∈ r.wires, r.EndOk w.1 ∧ r.EndOk w.2; decide,
   by show ∀ r ∈ stable, ∀ r' ∈ stable, ((r.a = r'.a ∧ r.b = r'.b) ∨ (r.a = r'.b ∧ r.b = r'.a)) → r = r'
      decide,
   by show ∀ r ∈ stable, ssig.pol r.a 0 ≠ ssig.pol r.b 0; decide⟩

theorem serial_ok : Ok ssys where
  admissible :=
    { table := s_tableOk
      total := by decide
      closed := by decide }
  run := ⟨s_tableOk, by decide⟩
  complete := by decide

theorem serial_gadget_ok : sg.ok ssys = true := by decide
theorem serial_kit_ok : skit.ok ssys = true := by decide

/-- The bits, term by term: signature, table (rule count and rules), gadget, reify, run. -/
theorem serial_bit_terms :
    (clog2 (sigAlts ssys), clog2 (tableAlts ssys), clog2 (gadgetAlts ssys sg),
      clog2 (reifyProg.alts ssys), clog2 (runProg.alts ssys)) = (69, 634, 27, 21, 13) := by decide +kernel

theorem serial_bits : bitsOf ssys sg skit = 762 := by decide +kernel

/-- Moves, rounds and records of each battery job, in `batteryJobs` order: the eight plain jobs, `reify`
on sComb, succ3, add, then `normNat (run ·)` on succ3, add21, add33, mul33. -/
def jobCost (j : JobTm Sk) : Option (Nat × Nat × Nat) :=
  (runJob ssys sg Battery.budget j).map fun (_, t) => (t.fires + t.cuts, t.rounds, recordsOf ssys t)

theorem serial_jobs : (batteryJobs skit).map (jobCost ·.1) =
      [some (820, 120, 983), some (3902, 194, 4656), some (6699, 244, 8053), some (165, 33, 152),
       some (445, 90, 497), some (1395, 283, 1399), some (35, 11, 42), some (83, 21, 100),
       some (71, 17, 30), some (137, 19, 72), some (299, 31, 129),
       some (591, 106, 657), some (1163, 126, 1333), some (1728, 195, 1991), some (5149, 285, 6045)] := by
  native_decide

theorem serial_runs : batteryCost ssys sg skit = some (22682, 1775, 26139) := by native_decide

theorem serial_tested : MeetsTested ssys sg skit 9 := by native_decide

def serialRecord : Record where
  K := Sk
  sys := ssys
  gadget := sg
  kit := skit
  strength := .tested 9
  cost := ⟨762, 22682, 1775, 26139⟩
  ok := serial_ok
  gadgetOk := serial_gadget_ok
  kitOk := serial_kit_ok
  meets := ⟨by decide, serial_tested⟩
  bits := serial_bits.symm
  runs := serial_runs

theorem serial_record : Achieved (.tested 9) ⟨762, 22682, 1775, 26139⟩ := ⟨serialRecord, by decide⟩

end SerialRec
#print axioms SerialRec.serial_record
