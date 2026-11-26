import Statement
import Check

set_option maxRecDepth 100000

/-! # The first record: the older 13-kind reflective net

The older net (`Tables.lean`, `Search.lean`: 13 kinds, 33 rules, 9 stuck pairs), with the gadget its own
compiler uses (`Check.lean`'s `emit` is exactly the frozen `Statement.emitSrc` with this gadget on pure
terms) and its kit, made into a `Statement.Record` (`Statement.lean`, v0.2 draft).

## The kit
- `reify` (142 agents), in `../nets/check.py` syntax (ρ c hands arm k the other four arms, then the
  parts; arms 2 and 5, never taken on a quote, hold spare copies of arms 1 and 3):
  `reify := \c. (\L A. ρ c (L (\i a b c. a i)) L A (\L Ls A As w. w) A (\z s. z)) Lam App`, with
  `Lam := \p Ls A V As F n. (\w. (\E a b c. c E) (ρ (F w) (Ls w) Ls A V As (\z s. s n))) (\m. m ρ p)`,
  `App := \L Ls V As f x n. (\d. (\F X a b c. b F X) (d f) (d x)) (\y. ρ y L Ls As V As n)`.
  Each dispatch carries n, the number of binders opened above it, as a Scott numeral. A λ opened at
  depth d gets the splice `w_d = λm. m ρ p_d`, where `p_0 = λi a b c. a i` and `p_(d+1) = w_d` (arm 1 of
  the body's dispatch is `Lam w_d`). A variable answers `w n`; since `p_d m` strips d successors from m
  and then builds a variable code, `w_d n` is the variable code of index n - d - 1, its de Bruijn index.
- `run` (1 agent): one λ whose bound port is wired straight to its body port, the identity. The quote
  is a `var` agent over the term's code, which the net reads as a captured value: the identity's body is
  thawed when it is applied, and that thaw starts the quoted code. A native `run`, as v0.2 allows.

## Why `reify` is right on every closed term (argued, not proved)
Induction on t: if each free variable of index i in t is a splice of `w_(n-1-i)`, then the dispatch
`ρ t (Lam p_n) Lam App V App ⌜n⌝` answers `encode t`. Variable: arm 4 answers `w_(n-1-i) ⌜n⌝` =
`p_(n-i) ⌜n⌝` = the code of index i. Application: arm 3 dispatches f and x at the same depth with the
same arms, and builds `λa b c. b ⌜f⌝ ⌜x⌝` from the two finished codes. λ: arm 1 opens the body on
`w_n` (index 0 inside becomes `w_n` = `w_((n+1)-1-0)`, every other index shifts by one with the depth)
and dispatches it at n + 1 with arm 1 `Lam w_n` = `Lam p_(n+1)`. Children are built before parents and
every answer is a value, so the readback is `encode t` literally. It relies on: ρ handing a λ to arm 1
with its opened function, an application to arm 3 and a splice to arm 4, and opening (O ⋈ Lam)
substituting the splice, as `Check.eval` models; and on the net copying the arms and closures right
(copies cross freely here, which `rln_finished_unique` makes order-independent but nothing proves
correct). The test checks all of it on 2,622 terms.

## Proved by `decide` (kernel)
- `ok`: admissible (the table checks reuse `rln_tableOk`), total, closed, every rule complete; the run
  checks reuse `rln_runOk`.
- `gadget_ok`.
- `bits`: `bitsOf` is 2904 (signature 71.0, rule count and rules 1170.7, gadget 26.9, reify 1626.8
  with 142 agents, run 7.7 with 1 agent; one ceiling over the product).
- `minTestSize ≤ 9`, the strength order and the cost bound of `older_record`.

## Proved by `decide +kernel`
- `kit_ok`, `kit_agents`: both programs are valid nets of the system; 142 and 1 agents.

## Proved by `native_decide` (trusts the compiler and the precompiled libraries)
- `battery`: `batteryCost` is (54580, 2961, 81730), every job checked against the reference.
- `tested`: `MeetsTested … 9`, every closed term up to size 9 and `numFamily` (2,640 terms), about
  45 s compiled; the whole file builds in about 90 s, under 1 GB.

## Only tested, not proved
- The spec `Meets`: the record's strength is `.tested 9`. The argument above for `reify`.

## Not proved here
- `Meets` itself, for any size beyond 9.
- That the runner's counts are `Path` lengths (see `Statement.lean`'s own list).

## Axioms (`#print axioms older_record`)
`propext` and the two `native_decide` axioms
`RecordOld.battery._native.native_decide.ax_1_1`, `RecordOld.tested._native.native_decide.ax_1_1`.
-/

namespace RecordOld

/-- The older net as a system: its real table (`gtable`), stuck pairs and signature. -/
def olderSystem : Statement.System K := ⟨K.all, sig, gtable, stuck⟩

def sys : Statement.System K := olderSystem

/-- λ, application, variable, sharer (`sig`), eraser (`eraI`) and thaw (`T`) of `Check.emit`; a quote
is a `var` agent over the code, as `Check.emit` writes a captured value (`Tm.splice`). -/
def gadget : Statement.Gadget K :=
  { lam := .lam, app := .app, var := .var, share := .sig, erase := .eraI, thaw := .T, quote := .native .var }

/-! ## The kit

`reify` is a literal term, printed from the source above by `../nets/ref.py`'s parser and a de Bruijn
printer, since Lean has no parser for it; its `Prog` is computed here by `Check.lean`'s `emit`, so the
counted net is the compiled term, nothing else. -/

/-- `reify` of the header. -/
def reifyTm : Tm := (Tm.lam (Tm.app (Tm.app (Tm.lam (Tm.lam (Tm.app (Tm.app (Tm.app (Tm.app (Tm.app (Tm.app (Tm.app Tm.rho (Tm.var 2)) (Tm.app (Tm.var 1) (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.app (Tm.var 2) (Tm.var 3)))))))) (Tm.var 1)) (Tm.var 0)) (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.var 0))))))) (Tm.var 0)) (Tm.lam (Tm.lam (Tm.var 1)))))) (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.app (Tm.lam (Tm.app (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.app (Tm.var 0) (Tm.var 3)))))) (Tm.app (Tm.app (Tm.app (Tm.app (Tm.app (Tm.app (Tm.app Tm.rho (Tm.app (Tm.var 2) (Tm.var 0))) (Tm.app (Tm.var 6) (Tm.var 0))) (Tm.var 6)) (Tm.var 5)) (Tm.var 4)) (Tm.var 3)) (Tm.lam (Tm.lam (Tm.app (Tm.var 0) (Tm.var 3))))))) (Tm.lam (Tm.app (Tm.app (Tm.var 0) Tm.rho) (Tm.var 7)))))))))))) (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.app (Tm.lam (Tm.app (Tm.app (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.lam (Tm.app (Tm.app (Tm.var 1) (Tm.var 4)) (Tm.var 3))))))) (Tm.app (Tm.var 0) (Tm.var 3))) (Tm.app (Tm.var 0) (Tm.var 2)))) (Tm.lam (Tm.app (Tm.app (Tm.app (Tm.app (Tm.app (Tm.app (Tm.app Tm.rho (Tm.var 0)) (Tm.var 7)) (Tm.var 6)) (Tm.var 4)) (Tm.var 5)) (Tm.var 4)) (Tm.var 1)))))))))))))

/-- `Check.emit` of a closed term, as a `Prog`: agent i is root i, the out end is the term's port. -/
def progOf (t : Tm) : Statement.Prog K :=
  let (b, p, _) := emit t {} []
  let agents := (List.range b.next).map fun i => match b.kinds.find? (·.1 == Ident.root i) with
    | some (_, .base k) => k
    | _ => .rho
  let pe : Port → Statement.PEnd := fun q => match q.1 with | .root i => .port i q.2 | _ => .out
  { agents, wires := (.out, pe p) :: b.wires.map fun (x, y) => (pe x, pe y) }

/-- The identity as one λ agent: its bound port wired to its body port. -/
def runProg : Statement.Prog K := { agents := [.lam], wires := [(.out, .port 0 0), (.port 0 1, .port 0 2)] }

def kit : Statement.Kit K := { reify := progOf reifyTm, run := runProg }

/-! ## The facts -/

theorem ok : Statement.Ok sys :=
  ⟨⟨rln_tableOk, by decide, by decide⟩, rln_runOk, by decide⟩

theorem gadget_ok : gadget.ok sys = true := by decide

/-- Both programs are well-wired nets of the system. -/
theorem kit_ok : kit.ok sys = true := by decide +kernel

theorem kit_agents : kit.reify.agents.length = 142 ∧ kit.run.agents.length = 1 := by decide +kernel

/-- Description length: signature, table, gadget and both programs. -/
theorem bits : Statement.bitsOf sys gadget kit = 2904 := by decide

/-- The battery: moves, rounds, record bits, summed over its 15 jobs. -/
theorem battery : Statement.batteryCost sys gadget kit = some (54580, 2961, 81730) := by native_decide

/-- Every closed term up to size 9, and the number family, passes every job. -/
theorem tested : Statement.MeetsTested sys gadget kit 9 := by native_decide

/-- The record: tested to size 9, its costs the computed ones. -/
def record : Statement.Record where
  K := K
  sys := sys
  gadget := gadget
  kit := kit
  strength := .tested 9
  cost := ⟨2904, 54580, 2961, 81730⟩
  ok := ok
  gadgetOk := gadget_ok
  kitOk := kit_ok
  meets := ⟨by decide, tested⟩
  bits := bits.symm
  runs := battery

end RecordOld

/-- **The first record**: the older net, tested to size 9, costs 2904 bits, and over the battery 54580
moves, 2961 rounds and 81730 record bits. -/
theorem older_record : Statement.Achieved (.tested 9) ⟨2904, 54580, 2961, 81730⟩ :=
  ⟨RecordOld.record, by decide, Nat.le_refl _, Nat.le_refl _, Nat.le_refl _, Nat.le_refl _⟩

#print axioms older_record
