import Programs
import Runner

/-! The per-design checks as theorems, and the filter a design search calls. The run checks are
decided by `native_decide`, which trusts Lean's compiler (the theorems in the other files don't). -/

instance (T : List (GRule K)) : Decidable (RunOk sig T) :=
  decidable_of_iff
    ((∀ r ∈ T, ∀ w ∈ r.wires, r.wireOk sig w = true) ∧ (∀ r ∈ T, ∀ w ∈ r.wires, r.EndOk w.1 ∧ r.EndOk w.2) ∧
     (∀ r ∈ T, ∀ r' ∈ T, ((r.a = r'.a ∧ r.b = r'.b) ∨ (r.a = r'.b ∧ r.b = r'.a)) → r = r') ∧
     (∀ r ∈ T, sig.pol r.a 0 ≠ sig.pol r.b 0) ∧ (∀ r ∈ T, (ends r.wires).Nodup))
    ⟨fun ⟨a, b, c, d, e⟩ => ⟨⟨a, b, c, d⟩, e⟩, fun ⟨⟨a, b, c, d⟩, e⟩ => ⟨a, b, c, d, e⟩⟩

/-- A candidate rule table for these agent kinds passes: the table checks the run theorems need,
and every program up to size 8, and every one up to size 7 that writes out an opened function,
runs to the reference's answer (or gets stuck where it does), every cancel joins the two ends of
one copy, and no garbage is left. -/
def candidateOk (T : List (GRule K)) : Bool :=
  decide (RunOk sig T) && (upTo 8).all (fastCheckWith T 300) && (upToO 7).all (fastCheckWith T 300)

/-- A table the filter accepts gets every run theorem: polarity kept, confluence, unique results. -/
theorem candidateOk_sound {T : List (GRule K)} (h : candidateOk T = true) : RunOk sig T := by
  simp only [candidateOk, Bool.and_eq_true] at h
  exact of_decide_eq_true h.1.1

def I : Tm := .lam (.var 0)
/-- Opens λx. x on I, getting the code `~I`, then applies that code, which is an error. -/
def stuckProg : Tm :=
  .app (.app (.app (.app (.app (.app (.app .rho I) (.lam (.lam (.lam (.lam (.lam (.app (.var 0) I))))))) I) I) I) I) I
/-- Broken: the two ends of a copy cancel crossed over. -/
def crossedCancel : List (GRule K) := gtable.map fun r =>
  if r.a == .dupI && r.b == .dupO then { r with wires := [(.left 1, .right 2), (.left 2, .right 1)] } else r
/-- Broken: no rule for copying a function. -/
def noCopyLam : List (GRule K) := gtable.filter fun r => !(r.a == .sig && r.b == .lam)

theorem rln_candidate : candidateOk gtable = true := by native_decide
/-- All 9,419 closed programs up to size 9. -/
theorem rln_small_programs : (upTo 9).all (fastCheckWith gtable 300) = true := by native_decide
/-- All 26,258 closed programs up to size 8 that write out an opened function (57 of them get
stuck in the reference, applying a piece of code; the nets get stuck there too). -/
theorem rln_small_programs_opened : (upToO 8).all (fastCheckWith gtable 300) = true := by native_decide
/-- The reflective programs from `../nets/check.py`, `size size` and the self-interpreter among them. -/
theorem rln_reflective_programs : programs.all (fun p => fastCheckWith gtable 100000 p.2) = true := by
  native_decide
/-- None of them is skipped: the reference gives every one a value. Among them an interpreter for
the calculus, written in the calculus, run on programs and on itself. -/
theorem rln_reflective_programs_have_values :
    programs.all (fun p => match eval 100000 p.2 with | .value _ => true | _ => false) = true := by
  native_decide
theorem rln_stuck_program : eval 100 stuckProg = .stuck ∧ fastCheckWith gtable 100 stuckProg = true := by
  native_decide
/-- The fast runner does exactly what the spec runner (`fire`, `unfwd`) does, step by step. -/
theorem fast_runner_matches_spec : (upTo 7).all (agreesWithSpec gtable 400) = true := by native_decide
theorem crossed_cancel_rejected : candidateOk crossedCancel = false := by native_decide
theorem missing_copy_rejected : candidateOk noCopyLam = false := by native_decide


/-- The spec runner's check, and `Build.ok` on the compiled net, for all 9,419 closed programs up
to size 9. -/
theorem rln_spec_checks : (upTo 9).all (fun t => (loadB t).1.ok && checkWith gtable 300 t) = true := by
  native_decide

/-- **Every order of steps gives the reference's answer**, for every closed program up to size 9
that the reference evaluates to a value: no order of steps runs forever, and every finished run,
whatever order it fired pairs and removed forwarders in, holds that value at the root. -/
theorem rln_every_order {t : Tm} (ht : t ∈ upTo 9) {v : Tm} (he : eval 300 t = .value v) :
    (∃ k, ∀ m M, Path (Move sig gtable (· = (loadB t).2)) m (start t).net M → m ≤ k) ∧
    (∀ M, ProgRuns t (start t).net M → Finished sig gtable (· = (loadB t).2) M →
      (M.mate ((loadB t).2, 0)).bind (readback 10000 M []) = some v) := by
  have h := List.all_eq_true.mp rln_spec_checks t ht
  simp only [Bool.and_eq_true] at h
  exact every_order_answers h.1 h.2 he

/-- How many programs up to size n the reference evaluates to a value, gets stuck on, or runs out
of fuel on (those are skipped). -/
def outcomeCounts (n : Nat) : Nat × Nat × Nat :=
  (upTo n).foldl (fun (v, s, f) t => match eval 300 t with
    | .value _ => (v + 1, s, f) | .stuck => (v, s + 1, f) | .fuel => (v, s, f + 1)) (0, 0, 0)

/-- `rln_every_order` covers 9,418 programs: all but one up to size 9 have a value. -/
theorem rln_every_order_count : outcomeCounts 9 = (9418, 0, 1) := by native_decide
