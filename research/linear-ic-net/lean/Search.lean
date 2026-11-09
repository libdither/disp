import Tables
import Net
import Moves

/-! The reflective-nets table meets every hypothesis of the general theorems in `Net.lean`, so its
steps keep nets polarized and its runs are strongly confluent. A candidate design gets the same
corollaries by passing the same four `decide` checks. -/

deriving instance DecidableEq for GRule

def sig : Sig K := ⟨K.ports⟩
def Rule.toG (r : Rule) : GRule K := ⟨r.a, r.b, r.made, r.wires⟩
def gtable : List (GRule K) := table.map Rule.toG

theorem rln_rules_polarized : ∀ r ∈ gtable, r.Polarized sig := by
  show ∀ r ∈ gtable, ∀ w ∈ r.wires, r.wireOk sig w = true; decide
theorem rln_rules_madeOk : ∀ r ∈ gtable, r.MadeOk := by
  show ∀ r ∈ gtable, ∀ w ∈ r.wires, r.EndOk w.1 ∧ r.EndOk w.2; decide
theorem rln_deterministic : Deterministic gtable := by
  show ∀ r ∈ gtable, ∀ r' ∈ gtable, ((r.a = r'.a ∧ r.b = r'.b) ∨ (r.a = r'.b ∧ r.b = r'.a)) → r = r'
  decide
theorem rln_principals : PrincipalsOpposite sig gtable := by
  show ∀ r ∈ gtable, sig.pol r.a 0 ≠ sig.pol r.b 0; decide

theorem rln_tableOk : TableOk sig gtable :=
  ⟨rln_rules_polarized, rln_rules_madeOk, rln_deterministic, rln_principals⟩

/-- Every step of a reflective-nets run keeps the net polarized. -/
theorem rln_step_polarized {N N' : Net K} (hN : N.Polarized sig) (hP : N.Present) (hfr : N.Fresh)
    (hs : Step sig gtable N N') : N'.Polarized sig :=
  rln_tableOk.step_polarized hN hP hfr hs

/-- Reflective-nets runs are strongly confluent. -/
theorem rln_strong_confluence {N N1 N2 : Net K} (hinv : N.Involutive) (hfr : N.Fresh)
    (h1 : Step sig gtable N N1) (h2 : Step sig gtable N N2) :
    N1 = N2 ∨ ∃ M, Step sig gtable N1 M ∧ Step sig gtable N2 M :=
  rln_tableOk.confluent hinv hfr h1 h2

theorem rln_nodup : ∀ r ∈ gtable, (ends r.wires).Nodup := by decide

theorem rln_runOk : RunOk sig gtable := ⟨rln_tableOk, rln_nodup⟩

/-- Any two finished runs of a reflective-nets program end in the same net, whatever order they
fire in. A compiled program is a good starting net when it is polarized, present and symmetric
and uses root names (`good_of_roots`). -/
theorem rln_unique_result {N N1 N2 : Net K} (hg : N.Good sig)
    (h1 : Reach sig gtable N N1) (h2 : Reach sig gtable N N2)
    (n1 : Normal sig gtable N1) (n2 : Normal sig gtable N2) : N1 = N2 :=
  rln_runOk.unique_result hg h1 h2 n1 n2

/-- Any two finished runs of a reflective-nets program end in the same net, firing pairs and
removing forwarders in any order; `keep` names the forwarders a run leaves in place, such as the
one its answer arrives at. -/
theorem rln_finished_unique {keep : Ident → Prop} {N N1 N2 : Net K} (hg : N.Good sig)
    (h1 : Moves sig gtable keep N N1) (h2 : Moves sig gtable keep N N2)
    (f1 : Finished sig gtable keep N1) (f2 : Finished sig gtable keep N2) : N1 = N2 :=
  rln_runOk.finished_unique hg h1 h2 f1 f2
