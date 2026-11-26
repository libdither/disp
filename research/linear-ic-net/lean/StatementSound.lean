import Statement

/-!
# The executable runner makes real runs

`Statement.lean` checks a record with a fast engine (`Eng`: hash maps, short names for made agents). This
file proves that what the engine does is a run of the proved theory (`Net`, `Move`, `Path`, `Finished`).

The engine names a made agent `root (fresh + k)` where the theory names it `made a b k`, so the two agree
only up to a one-to-one renaming of agents (`Iso σ F M`), never literally.

**Proved** (using only Lean's three standard axioms):
1. One step: `fire_view`, `cut_view` (an engine fire or forwarder removal is the theory's, renamed).
2. The loop: `rounds_sim`, `rounds_run` (a `Trace`, hence a `Path` of `fires + removals` moves, with
   per-rule counts as tallied).
3. Stopping: `rounds_finished` (when the loop's work list empties, the theory's net is `Finished`).
4. A job: `runJob_run`, `runJob_answers` (a real finished run, readback on the theory's net gives the
   answer, and its length is the one `work_well_defined` fixes); `runJob_answers'`, `testTerm_answers`,
   `meetsTested_answers` (the spec's `Answers` for every term `testTerms n` lists: `reify`, the plain job,
   the halting and numeral observers of `run`, each under the condition `testTerm` checks it), given the
   two transfers below.
5. The transfers: `loadOk_of_ok` (every test job's loaded net meets `LoadOk`, from `Gadget.ok`'s arities and
   `Kit.ok`; an invariant `BInv` on `Build` kept by every emitter step) and `cleanTransfer` (the engine's
   `clean` means `Clean`; `reach` only ever adds agents joined to the root, and the size check then leaves no
   agent out).
6. So `meetsTested_spec` and `Record.tested_spec`: a passed test gives the spec's `Answers` on every tested
   term, assuming only `Ok`, `Gadget.ok` and `Kit.ok`, which a `Record` carries.

**Invariant** (`Sim`): the theory's net renamed by `σ` is the engine's view; it is `Good`, wires sit only
on real ports (`InRange`), every engine name is `root n` below `fresh`, `σ` keeps the root, and the root is a
forwarder. For stopping, the work list covers every live active pair and stray forwarder (`Covered`,
`FwdRoot`).

## Remaining obligations

None for the test. `loadOk_of_ok` needs neither a closed term nor anything about the jobs beyond their shape:
it holds for every `JobTm` whose programs pass `Prog.ok` (`loadOk_of_jobOk`).
-/

namespace Statement
open Std
attribute [local instance] identHashable

variable {K : Type}

/-! ## Nets equal up to renaming -/

/-- Rename the agent of a port. -/
def pmap (σ : Ident → Ident) (p : Port) : Port := (σ p.1, p.2)

/-- Wires sit only on ports an agent has. -/
def InRange (S : Sig K) (N : Net K) : Prop :=
  ∀ x n q, N.mate (x, n) = some q → ∃ g, N.kind x = some g ∧ n ≤ agArity S g

/-- `σ` renames the agents of `N` one-to-one onto those of `M`, kinds and wires included; `M` has no
wire on a missing agent. -/
structure Iso (σ : Ident → Ident) (N M : Net K) : Prop where
  inj : ∀ x y, N.kind x ≠ none → N.kind y ≠ none → σ x = σ y → x = y
  kind : ∀ x, N.kind x ≠ none → M.kind (σ x) = N.kind x
  onto : ∀ y, M.kind y ≠ none → ∃ x, N.kind x ≠ none ∧ σ x = y
  mate : ∀ x n, N.kind x ≠ none → M.mate (σ x, n) = (N.mate (x, n)).map (pmap σ)
  dead : ∀ y n, M.kind y = none → M.mate (y, n) = none

theorem Iso.refl {N : Net K} (h : ∀ y n, N.kind y = none → N.mate (y, n) = none) : Iso id N N :=
  ⟨fun _ _ _ _ e => e, fun _ _ => rfl, fun y hy => ⟨y, hy, rfl⟩,
    fun x n _ => by show N.mate (x, n) = _; cases N.mate (x, n) <;> simp [pmap], h⟩

theorem Iso.trans {σ τ : Ident → Ident} {N M P : Net K} (I : Iso σ N M) (J : Iso τ M P) :
    Iso (τ ∘ σ) N P where
  inj x y hx hy e := I.inj x y hx hy (J.inj _ _ (by rw [I.kind x hx]; exact hx) (by rw [I.kind y hy]; exact hy) e)
  kind x hx := by
    show P.kind (τ (σ x)) = _
    rw [J.kind _ (by rw [I.kind x hx]; exact hx), I.kind x hx]
  onto y hy := by
    obtain ⟨z, hz, rfl⟩ := J.onto y hy
    obtain ⟨x, hx, rfl⟩ := I.onto z hz
    exact ⟨x, hx, rfl⟩
  mate x n hx := by
    show P.mate (τ (σ x), n) = _
    rw [J.mate _ _ (by rw [I.kind x hx]; exact hx), I.mate x n hx, Option.map_map]; rfl
  dead := J.dead

section IsoFacts
variable {σ : Ident → Ident} {N M : Net K}

theorem Iso.kind_ne (I : Iso σ N M) {x : Ident} (hx : N.kind x ≠ none) : M.kind (σ x) ≠ none := by
  rw [I.kind x hx]; exact hx

/-- A wire of `M` comes from a wire of `N`. -/
theorem Iso.mate_some (I : Iso σ N M) {p q : Port} (h : M.mate p = some q) :
    ∃ x q0, N.kind x ≠ none ∧ σ x = p.1 ∧ N.mate (x, p.2) = some q0 ∧ q = pmap σ q0 := by
  obtain ⟨y, n⟩ := p
  have hy : M.kind y ≠ none := fun e => by rw [I.dead y n e] at h; cases h
  obtain ⟨x, hx, rfl⟩ := I.onto y hy
  rw [I.mate x n hx] at h
  cases hm : N.mate (x, n) with
  | none => rw [hm] at h; cases h
  | some q0 => rw [hm] at h; simp only [Option.map_some, Option.some.injEq] at h; exact ⟨x, q0, hx, rfl, hm, h.symm⟩

theorem Iso.present (I : Iso σ N M) (hP : N.Present) : M.Present := by
  intro p q h
  obtain ⟨x, q0, hx, hpx, hm, rfl⟩ := I.mate_some h
  exact ⟨by rw [← hpx]; exact I.kind_ne hx, I.kind_ne (hP _ _ hm).2⟩

theorem Iso.involutive (I : Iso σ N M) (hP : N.Present) (hi : N.Involutive) : M.Involutive := by
  intro p q h
  obtain ⟨x, q0, hx, hpx, hm, rfl⟩ := I.mate_some h
  obtain ⟨z, m⟩ := q0
  have back := hi _ _ hm
  show M.mate (σ z, m) = some p
  rw [I.mate z m (hP _ _ hm).2, back]
  obtain ⟨y, n⟩ := p
  simp only at hpx; subst hpx; rfl

theorem Iso.inRange (I : Iso σ N M) {S : Sig K} (hR : InRange S N) : InRange S M := by
  intro y n q h
  obtain ⟨x, q0, hx, hpx, hm, rfl⟩ := I.mate_some h
  obtain ⟨g, hg, hn⟩ := hR x n q0 hm
  exact ⟨g, by simp only at hpx; rw [← hpx, I.kind x hx, hg], hn⟩

end IsoFacts

/-! ## Renaming commutes with firing a pair and with removing a forwarder -/

/-- After firing a and b: agents the step made are renamed after the renamed pair, the rest by σ. -/
def ext (σ : Ident → Ident) (a b a' b' : Ident) (y : Ident) : Ident :=
  match madeIdx a b y with
  | some k => .made a' b' k
  | none => σ y

theorem ext_made (σ : Ident → Ident) (a b a' b' : Ident) (k : Nat) :
    ext σ a b a' b' (.made a b k) = .made a' b' k := by simp [ext, madeIdx_made]

theorem ext_old {σ : Ident → Ident} {a b a' b' y : Ident} (h : madeIdx a b y = none) :
    ext σ a b a' b' y = σ y := by simp [ext, h]

theorem madeIdx_none_of_fresh {N : Net K} {a b x : Ident} (hf : ∀ k, N.kind (.made a b k) = none)
    (hx : N.kind x ≠ none) : madeIdx a b x = none := by
  cases hk : madeIdx a b x with
  | none => rfl
  | some k => rw [madeIdx_some hk] at hx; exact absurd (hf k) hx

theorem madeMate_none {S : Sig K} {r : GRule K} {a b : Ident} {N : Net K} {k n : Nat}
    (h : madeKind S r k = none) : madeMate r a b N k n = none := by
  unfold madeKind at h; unfold madeMate
  by_cases hlt : k < r.made.length
  · rw [if_pos hlt] at h
    have : r.made[k]? ≠ none := by simp [hlt]
    simp at h; omega
  · rw [if_neg hlt] at h ⊢
    cases hw : r.wires[k - r.made.length]? with
    | none => rfl
    | some e12 =>
      obtain ⟨e1, e2⟩ := e12
      rw [hw] at h; simp only at h ⊢
      split at h
      · cases h
      · rename_i hs; simp [hs]

section FireIso
variable {S : Sig K} {r : GRule K} {σ : Ident → Ident} {N M : Net K} {a b : Ident}

/-- The hypotheses for firing the same pair on both sides of a renaming. -/
structure FireIsoOk (σ : Ident → Ident) (N M : Net K) (a b : Ident) : Prop where
  iso : Iso σ N M
  ha : N.kind a ≠ none
  hb : N.kind b ≠ none
  freshN : ∀ k, N.kind (.made a b k) = none
  freshM : ∀ k, M.kind (.made (σ a) (σ b) k) = none
  present : N.Present

local notation "τ" => ext σ a b (σ a) (σ b)

theorem serve_iso (e : End) : serve r (σ a) (σ b) e = (serve r a b e).map (pmap τ) := by
  unfold serve
  cases findEnd e r.wires 0 with
  | none => rfl
  | some t =>
    obtain ⟨w, f, first⟩ := t
    cases f <;> simp [pmap, ext_made]

variable (h : FireIsoOk σ N M a b)
include h

theorem relink_iso_some {q : Port} (hq : N.kind q.1 ≠ none) :
    relink r (σ a) (σ b) (some (pmap σ q)) = (relink r a b (some q)).map (pmap τ) := by
  obtain ⟨y, n⟩ := q
  simp only [relink, pmap]
  by_cases hya : y = a
  · subst hya; simp only [if_true]; exact serve_iso _
  have : σ y ≠ σ a := fun e => hya (h.iso.inj _ _ hq h.ha e)
  simp only [hya, this, if_false]
  by_cases hyb : y = b
  · subst hyb; simp only [if_true]; exact serve_iso _
  have : σ y ≠ σ b := fun e => hyb (h.iso.inj _ _ hq h.hb e)
  simp only [hyb, this, if_false]
  simp [pmap, ext_old (madeIdx_none_of_fresh h.freshN hq)]

theorem relink_iso {o : Option Port} (ho : ∀ q, o = some q → N.kind q.1 ≠ none) :
    relink r (σ a) (σ b) (o.map (pmap σ)) = (relink r a b o).map (pmap τ) := by
  cases o with
  | none => rfl
  | some q => exact relink_iso_some h (ho q rfl)

theorem slot_mate_iso {e : End} (he : e.isSlot = true) :
    relink r (σ a) (σ b) (M.mate (slotPort (σ a) (σ b) e)) =
      (relink r a b (N.mate (slotPort a b e))).map (pmap τ) := by
  have key : ∀ x n, N.kind x ≠ none →
      relink r (σ a) (σ b) (M.mate (σ x, n)) = (relink r a b (N.mate (x, n))).map (pmap τ) := by
    intro x n hx
    rw [h.iso.mate x n hx]
    exact relink_iso h (fun q hq => (h.present _ _ hq).2)
  cases e with
  | left m => exact key a m h.ha
  | right m => exact key b m h.hb
  | new j m => simp [End.isSlot] at he

theorem partner_iso (e : End) :
    partner r (σ a) (σ b) M e = (partner r a b N e).map (pmap τ) := by
  unfold partner
  cases findEnd e r.wires 0 with
  | none => rfl
  | some t =>
    obtain ⟨w, f, first⟩ := t
    cases f with
    | new j m => simp [pmap, ext_made]
    | left m => exact slot_mate_iso h (e := .left m) rfl
    | right m => exact slot_mate_iso h (e := .right m) rfl

theorem madeMate_iso (k n : Nat) :
    madeMate r (σ a) (σ b) M k n = (madeMate r a b N k n).map (pmap τ) := by
  unfold madeMate
  by_cases hlt : k < r.made.length
  · rw [if_pos hlt, if_pos hlt]; exact partner_iso h _
  rw [if_neg hlt, if_neg hlt]
  cases r.wires[k - r.made.length]? with
  | none => rfl
  | some e12 =>
    obtain ⟨e1, e2⟩ := e12
    simp only
    by_cases hs : (e1.isSlot && e2.isSlot) = true
    · simp only [hs, if_true]
      simp only [Bool.and_eq_true] at hs
      by_cases hn0 : n = 0
      · simp only [hn0, if_true]; exact slot_mate_iso h hs.1
      · simp only [hn0, if_false]
        by_cases hn1 : n = 1
        · simp only [hn1, if_true]; exact slot_mate_iso h hs.2
        · simp [hn1]
    · simp [hs]

theorem old_away {x : Ident} (hx : N.kind x ≠ none) (hab : ¬(x = a ∨ x = b)) :
    ¬(σ x = σ a ∨ σ x = σ b) ∧ madeIdx (σ a) (σ b) (σ x) = none := by
  refine ⟨?_, madeIdx_none_of_fresh h.freshM (h.iso.kind_ne hx)⟩
  rintro (e | e)
  · exact hab (Or.inl (h.iso.inj _ _ hx h.ha e))
  · exact hab (Or.inr (h.iso.inj _ _ hx h.hb e))

/-- **Firing commutes with renaming**: firing a and b, then renaming, is firing their new names. -/
theorem fire_iso : Iso τ (fire S r a b N) (fire S r (σ a) (σ b) M) where
  inj x y hx hy e := by
    rcases fire_kind_some S r a b N hx with ⟨k, rfl⟩ | ⟨hxab, hxm, hxN⟩ <;>
    rcases fire_kind_some S r a b N hy with ⟨k', rfl⟩ | ⟨hyab, hym, hyN⟩
    · rw [ext_made, ext_made] at e; cases e; rfl
    · rw [ext_made, ext_old hym] at e
      exact absurd (h.freshM k) (by rw [e]; exact h.iso.kind_ne hyN)
    · rw [ext_made, ext_old hxm] at e
      exact absurd (h.freshM k') (by rw [← e]; exact h.iso.kind_ne hxN)
    · rw [ext_old hxm, ext_old hym] at e; exact h.iso.inj _ _ hxN hyN e
  kind x hx := by
    rcases fire_kind_some S r a b N hx with ⟨k, rfl⟩ | ⟨hxab, hxm, hxN⟩
    · rw [ext_made, fire_kind_made, fire_kind_made]
    · obtain ⟨h1, h2⟩ := old_away h hxN hxab
      rw [ext_old hxm, fire_kind_other S r _ _ M h1 h2, fire_kind_other S r a b N hxab hxm, h.iso.kind x hxN]
  onto y hy := by
    rcases fire_kind_some S r (σ a) (σ b) M hy with ⟨k, rfl⟩ | ⟨hyab, hym, hyM⟩
    · refine ⟨.made a b k, ?_, ext_made _ _ _ _ _ k⟩
      rw [fire_kind_made]; rw [fire_kind_made] at hy; exact hy
    · obtain ⟨x, hx, rfl⟩ := h.iso.onto y hyM
      have hxab : ¬(x = a ∨ x = b) := by rintro (rfl | rfl) <;> simp at hyab
      have hxm := madeIdx_none_of_fresh h.freshN hx
      exact ⟨x, by rw [fire_kind_other S r a b N hxab hxm]; exact hx, ext_old hxm⟩
  mate x n hx := by
    rcases fire_kind_some S r a b N hx with ⟨k, rfl⟩ | ⟨hxab, hxm, hxN⟩
    · rw [ext_made, fire_mate_made, fire_mate_made]; exact madeMate_iso h k n
    · obtain ⟨h1, h2⟩ := old_away h hxN hxab
      rw [ext_old hxm, fire_mate_other S r _ _ M h1 h2 n, fire_mate_other S r a b N hxab hxm n,
        h.iso.mate x n hxN]
      exact relink_iso h (fun q hq => (h.present _ _ hq).2)
  dead y n hy := by
    by_cases hab : y = σ a ∨ y = σ b
    · exact fire_mate_pair S r _ _ M hab n
    cases hk : madeIdx (σ a) (σ b) y with
    | some k =>
      have := madeIdx_some hk; subst this
      rw [fire_kind_made] at hy
      rw [fire_mate_made]; exact madeMate_none hy
    | none =>
      rw [fire_kind_other S r _ _ M hab hk] at hy
      rw [fire_mate_other S r _ _ M hab hk, h.iso.dead y n hy]; rfl

end FireIso

section UnfwdIso
variable {σ : Ident → Ident} {N M : Net K} {x : Ident}

theorem via_iso (I : Iso σ N M) (hP : N.Present) (hx : N.kind x ≠ none) {o : Option Port}
    (ho : ∀ q, o = some q → N.kind q.1 ≠ none) :
    via (σ x) M (o.map (pmap σ)) = (via x N o).map (pmap σ) := by
  cases o with
  | none => rfl
  | some q =>
    have hq := ho q rfl
    simp only [Option.map_some, via, pmap]
    by_cases hqx : q.1 = x
    · rw [if_pos (by rw [hqx]), if_pos hqx]
      have sub : ∀ i, (M.mate (σ x, i)) = (N.mate (x, i)).map (pmap σ) := fun i => I.mate x i hx
      have fin : ∀ o : Option Port, (∀ t, o = some t → N.kind t.1 ≠ none) →
          (match o.map (pmap σ) with
            | some t => if t.1 = σ x then none else some t
            | none => none) =
          (match o with
            | some t => if t.1 = x then none else some t
            | none => none).map (pmap σ) := by
        intro o ho
        cases o with
        | none => rfl
        | some t =>
          simp only [Option.map_some, pmap]
          by_cases htx : t.1 = x
          · simp [htx]
          · have : σ t.1 ≠ σ x := fun e => htx (I.inj _ _ (ho t rfl) hx e)
            simp [htx, this]; rfl
      have hpres : ∀ i t, N.mate (x, i) = some t → N.kind t.1 ≠ none := fun i t ht => (hP _ _ ht).2
      by_cases h0 : q.2 = 0
      · simp only [h0, if_true, sub]; exact fin _ (hpres 1)
      · simp only [h0, if_false]
        by_cases h1 : q.2 = 1
        · simp only [h1, if_true, sub]; exact fin _ (hpres 0)
        · simp [h1]
    · have : σ q.1 ≠ σ x := fun e => hqx (I.inj _ _ hq hx e)
      simp [this, hqx]; rfl

/-- **Removing a forwarder commutes with renaming.** -/
theorem unfwd_iso (I : Iso σ N M) (hP : N.Present) (hx : N.kind x ≠ none) :
    Iso σ (unfwd x N) (unfwd (σ x) M) where
  inj y z hy hz e := by
    rw [unfwd_kind] at hy hz
    by_cases hyx : y = x
    · simp [hyx] at hy
    by_cases hzx : z = x
    · simp [hzx] at hz
    rw [if_neg hyx] at hy; rw [if_neg hzx] at hz
    exact I.inj _ _ hy hz e
  kind y hy := by
    rw [unfwd_kind] at hy
    by_cases hyx : y = x
    · simp [hyx] at hy
    rw [if_neg hyx] at hy
    have : σ y ≠ σ x := fun e => hyx (I.inj _ _ hy hx e)
    rw [unfwd_kind, unfwd_kind, if_neg this, if_neg hyx, I.kind y hy]
  onto y hy := by
    rw [unfwd_kind] at hy
    by_cases hyx : y = σ x
    · simp [hyx] at hy
    rw [if_neg hyx] at hy
    obtain ⟨z, hz, rfl⟩ := I.onto y hy
    have : z ≠ x := by rintro rfl; exact hyx rfl
    exact ⟨z, by rw [unfwd_kind, if_neg this]; exact hz, rfl⟩
  mate y n hy := by
    rw [unfwd_kind] at hy
    by_cases hyx : y = x
    · simp [hyx] at hy
    rw [if_neg hyx] at hy
    have : σ y ≠ σ x := fun e => hyx (I.inj _ _ hy hx e)
    rw [unfwd_mate, unfwd_mate]
    simp only [this, hyx, if_false]
    rw [I.mate y n hy]
    exact via_iso I hP hx (fun q hq => (hP _ _ hq).2)
  dead y n hy := by
    rw [unfwd_kind] at hy
    rw [unfwd_mate]
    by_cases hyx : y = σ x
    · simp [hyx]
    rw [if_neg hyx] at hy ⊢
    rw [I.dead y n hy]; rfl

end UnfwdIso

/-! ## Nothing left to do, read off the net -/

/-- No active pair the table has a rule for, and no forwarder outside `keep`. -/
def Idle (S : Sig K) (T : List (GRule K)) (keep : Ident → Prop) (N : Net K) : Prop :=
  (∀ a b r, r ∈ T → N.mate (a, 0) = some (b, 0) → N.kind a = some (.base r.a) →
    N.kind b = some (.base r.b) → False) ∧
  ∀ x P, ¬ keep x → N.kind x = some (.fwd P) → False

theorem Idle.finished {S : Sig K} {T : List (GRule K)} {keep : Ident → Prop} {N : Net K}
    (h : Idle S T keep N) : Finished S T keep N := by
  intro M hm
  rcases hm with ⟨a, b, r, hr, hab, hka, hkb, _⟩ | ⟨x, P, hk, hx, _⟩
  · exact h.1 a b r hr hab hka hkb
  · exact h.2 x P hk hx

theorem Iso.idle {σ : Ident → Ident} {N M : Net K} {S : Sig K} {T : List (GRule K)}
    {keep keep' : Ident → Prop} (I : Iso σ N M) (hk : ∀ x, N.kind x ≠ none → keep' (σ x) → keep x)
    (h : Idle S T keep' M) : Idle S T keep N := by
  refine ⟨fun a b r hr hab hka hkb => ?_, fun x P hx hkx => ?_⟩
  · have pa : N.kind a ≠ none := by simp [hka]
    have := I.mate a 0 pa
    rw [hab] at this
    exact h.1 _ _ r hr this (by rw [I.kind a pa, hka]) (by rw [I.kind b (by simp [hkb]), hkb])
  · have px : N.kind x ≠ none := by simp [hkx]
    exact h.2 (σ x) P (fun e => hx (hk x px e)) (by rw [I.kind x px, hkx])

/-! ## Folds over hash maps -/

open Classical in
/-- A fold that writes, at each key, a value fixed by the key: the map ends with that value at every
key the list names, and the old value elsewhere. -/
theorem hm_foldl {α β γ : Type} [BEq β] [Hashable β] [DecidableEq β]
    (f : Std.HashMap β γ → α → Std.HashMap β γ) (key : α → β) (v : β → Option γ) :
    ∀ (l : List α) (m : Std.HashMap β γ),
      (∀ x ∈ l, ∀ (m : Std.HashMap β γ) q, (f m x)[q]? = if key x = q then v q else m[q]?) →
      ∀ q, (l.foldl f m)[q]? = if q ∈ l.map key then v q else m[q]?
  | [], m, _, q => by simp
  | x :: l, m, hf, q => by
    rw [List.foldl_cons, hm_foldl f key v l (f m x) (fun y hy => hf y (List.mem_cons_of_mem _ hy)) q,
      hf x List.mem_cons_self m q]
    by_cases h1 : q ∈ l.map key
    · simp [h1]
    · by_cases h2 : key x = q
      · simp [h1, h2]
      · have : q ∉ (x :: l).map key := by simp only [List.map_cons, List.mem_cons, not_or]; exact ⟨Ne.symm h2, h1⟩
        rw [if_neg this, if_neg h1, if_neg h2]

open Classical in
theorem hm_erase_fold {β γ : Type} [BEq β] [Hashable β] [LawfulBEq β] [DecidableEq β]
    (l : List β) (m : Std.HashMap β γ) (q : β) :
    (l.foldl (fun m p => m.erase p) m)[q]? = if q ∈ l then none else m[q]? := by
  rw [hm_foldl _ id (fun _ => none) l m (fun x _ m q => by simp [Std.HashMap.getElem?_erase])]
  simp

/-! ## The engine against the theory: one fire -/

/-- The engine's view of a state. -/
def vw (kinds : Std.HashMap Ident (Ag K)) (mates : Std.HashMap Port Port) : Net K :=
  ⟨fun x => kinds[x]?, fun p => mates[p]?⟩

/-- Every agent of the engine has a root name below its counter. -/
def Eng.Roots (e : Eng K) : Prop := ∀ x, e.kinds[x]? ≠ none → ∃ n, x = Ident.root n ∧ n < e.fresh

/-- The engine's names for what a fire of a and b makes: the k-th made agent is `root (fresh + k)`. -/
def sren (fresh : Nat) (a b : Ident) (y : Ident) : Ident :=
  match madeIdx a b y with
  | some k => Ident.root (fresh + k)
  | none => y

theorem madeKind_lt' {S : Sig K} {r : GRule K} {k : Nat} (h : madeKind S r k ≠ none) :
    k < r.made.length + r.wires.length := by
  unfold madeKind at h
  split at h
  · omega
  · cases hw : r.wires[k - r.made.length]? with
    | none => simp [hw] at h
    | some _ =>
      have := (List.getElem?_eq_some_iff.mp hw).1
      omega

theorem mem_portsOf {S : Sig K} {N : Net K} {x : Ident} {p : Port} :
    p ∈ Eng.portsOf S N x ↔ p.1 = x ∧ p.2 < ((N.kind x).map (agArity S)).getD 0 + 1 := by
  obtain ⟨y, n⟩ := p
  simp only [Eng.portsOf, List.mem_map, List.mem_range, Prod.mk.injEq]
  constructor
  · rintro ⟨i, hi, rfl, rfl⟩; exact ⟨rfl, hi⟩
  · rintro ⟨rfl, hi⟩; exact ⟨n, hi, rfl, rfl⟩

section EngFire
variable (S : Sig K) (r : GRule K) (kinds : Std.HashMap Ident (Ag K)) (mates : Std.HashMap Port Port)
  (fresh : Nat) (a b : Ident)

def fRen : Port → Port := fun p => match madeIdx a b p.1 with
  | some k => (Ident.root (fresh + k), p.2)
  | none => p

def fGone : List Port := Eng.portsOf S (vw kinds mates) a ++ Eng.portsOf S (vw kinds mates) b

def fNear : List Ident :=
  (((fGone S kinds mates a b).filterMap (vw kinds mates).mate).map (·.1)).filter fun z => z != a && z != b

def fMade : List (Nat × Ag K) :=
  (List.range (r.made.length + r.wires.length)).filterMap fun k => (madeKind S r k).map fun g => (k, g)

def fUpsNear : List (Port × Option Port) :=
  (fNear S kinds mates a b).flatMap fun z => (Eng.portsOf S (vw kinds mates) z).filterMap fun p =>
    match (vw kinds mates).mate p with
    | some t => if t.1 == a || t.1 == b then
        some (p, ((fire S r a b (vw kinds mates)).mate p).map (fRen fresh a b)) else none
    | none => none

def fUpsMade : List (Port × Port) :=
  (fMade S r).flatMap fun (k, g) =>
    (List.range (agArity S g + 1)).filterMap fun i =>
      ((fire S r a b (vw kinds mates)).mate (Ident.made a b k, i)).map fun u =>
        ((Ident.root (fresh + k), i), fRen fresh a b u)

theorem fire_mates_eq : (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.mates =
    (fUpsMade S r kinds mates fresh a b).foldl (fun m (p, v) => m.insert p v)
      ((fUpsNear S r kinds mates fresh a b).foldl
        (fun m (p, u) => match u with | some v => m.insert p v | none => m.erase p)
        ((fGone S kinds mates a b).foldl (fun m p => m.erase p) mates)) := rfl

theorem fire_kinds_eq : (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.kinds =
    (fMade S r).foldl (fun m (k, g) => m.insert (Ident.root (fresh + k)) g) ((kinds.erase a).erase b) := rfl

theorem fire_fresh_eq : (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.fresh =
    fresh + r.made.length + r.wires.length := rfl

theorem fRen_eq : fRen fresh a b = pmap (sren fresh a b) := by
  funext p; obtain ⟨y, n⟩ := p
  simp only [fRen, pmap, sren]
  cases madeIdx a b y <;> rfl

theorem mem_fMade {k : Nat} {g : Ag K} : (k, g) ∈ fMade S r ↔ madeKind S r k = some g := by
  simp only [fMade, List.mem_filterMap, List.mem_range, Option.map_eq_some_iff, Prod.mk.injEq]
  constructor
  · rintro ⟨k', _, g', h, rfl, rfl⟩; exact h
  · intro h; exact ⟨k, madeKind_lt' (S := S) (by simp [h]), g, h, rfl, rfl⟩

open Classical in
/-- The kinds after a fire: what it made at the new names, the pair gone, the rest as before. -/
theorem fire_kinds_get (y : Ident) :
    (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.kinds[y]? =
      if ∃ k, madeKind S r k ≠ none ∧ y = .root (fresh + k) then
        (match y with | Ident.root n => madeKind S r (n - fresh) | _ => none)
      else if y = a ∨ y = b then none else kinds[y]? := by
  have hf : ∀ x ∈ fMade S r, ∀ (m : Std.HashMap Ident (Ag K)) q,
      ((fun m (kg : Nat × Ag K) => match kg with | (k, g) => m.insert (Ident.root (fresh + k)) g) m x)[q]? =
        if Ident.root (fresh + x.1) = q then
          (match q with | Ident.root n => madeKind S r (n - fresh) | _ => none) else m[q]? := by
    rintro ⟨k, g⟩ hx m q
    simp only [Std.HashMap.getElem?_insert, beq_iff_eq]
    split
    · rename_i e; subst e; simp [(mem_fMade S r).mp hx]
    · rfl
  rw [fire_kinds_eq, hm_foldl _ _ _ _ _ hf]
  have hm : (y ∈ (fMade S r).map fun kg : Nat × Ag K => Ident.root (fresh + kg.1)) ↔
      ∃ k, madeKind S r k ≠ none ∧ y = .root (fresh + k) := by
    simp only [List.mem_map]
    constructor
    · rintro ⟨⟨k, g⟩, hk, rfl⟩; exact ⟨k, by rw [(mem_fMade S r).mp hk]; simp, rfl⟩
    · rintro ⟨k, hk, rfl⟩
      obtain ⟨g, hg⟩ := Option.ne_none_iff_exists'.mp hk
      exact ⟨(k, g), (mem_fMade S r).mpr hg, rfl⟩
  by_cases h : ∃ k, madeKind S r k ≠ none ∧ y = .root (fresh + k)
  · rw [if_pos (hm.mpr h), if_pos h]
  · rw [if_neg (fun e => h (hm.mp e)), if_neg h]
    simp only [Std.HashMap.getElem?_erase, beq_iff_eq]
    by_cases hb : b = y
    · subst hb; simp
    · by_cases ha : a = y
      · subst ha; simp
      · simp [ha, hb, Ne.symm ha, Ne.symm hb]


def wM (q : Port) : Option Port := match q.1 with
  | Ident.root n => ((fire S r a b (vw kinds mates)).mate (Ident.made a b (n - fresh), q.2)).map (fRen fresh a b)
  | _ => none

theorem fUpsNear_val {x : Port × Option Port} (hx : x ∈ fUpsNear S r kinds mates fresh a b) :
    x.2 = ((fire S r a b (vw kinds mates)).mate x.1).map (fRen fresh a b) := by
  simp only [fUpsNear, List.mem_flatMap, List.mem_filterMap] at hx
  obtain ⟨z, _, p, _, hpx⟩ := hx
  split at hpx
  · split at hpx
    · cases hpx; rfl
    · cases hpx
  · cases hpx

theorem fUpsMade_val {x : Port × Port} (hx : x ∈ fUpsMade S r kinds mates fresh a b) :
    wM S r kinds mates fresh a b x.1 = some x.2 := by
  simp only [fUpsMade, List.mem_flatMap, List.mem_filterMap, Option.map_eq_some_iff] at hx
  obtain ⟨⟨k, g⟩, _, i, _, u, hu, rfl⟩ := hx
  simp [wM, hu]

open Classical in
/-- The wires after a fire: made agents' wires, then the rewired neighbours', then the pair's
ports cleared, the rest as before. -/
theorem fire_mates_get (q : Port) :
    (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.mates[q]? =
      if q ∈ (fUpsMade S r kinds mates fresh a b).map Prod.fst then wM S r kinds mates fresh a b q
      else if q ∈ (fUpsNear S r kinds mates fresh a b).map Prod.fst then ((fire S r a b (vw kinds mates)).mate q).map (fRen fresh a b)
      else if q ∈ fGone S kinds mates a b then none else mates[q]? := by
  have h1 : ∀ x ∈ fUpsMade S r kinds mates fresh a b, ∀ (m : Std.HashMap Port Port) q,
      ((fun m (pv : Port × Port) => match pv with | (p, v) => m.insert p v) m x)[q]? =
        if x.1 = q then wM S r kinds mates fresh a b q else m[q]? := by
    rintro ⟨p, v⟩ hx m q
    simp only [Std.HashMap.getElem?_insert, beq_iff_eq]
    split
    · rename_i e; subst e; exact (fUpsMade_val S r kinds mates fresh a b hx).symm
    · rfl
  have h2 : ∀ x ∈ fUpsNear S r kinds mates fresh a b, ∀ (m : Std.HashMap Port Port) q,
      ((fun m (pu : Port × Option Port) => match pu with
        | (p, u) => match u with | some v => m.insert p v | none => m.erase p) m x)[q]? =
        if x.1 = q then ((fire S r a b (vw kinds mates)).mate q).map (fRen fresh a b) else m[q]? := by
    rintro ⟨p, u⟩ hx m q
    have hv := fUpsNear_val S r kinds mates fresh a b hx
    simp only at hv
    cases u with
    | some v =>
      simp only [Std.HashMap.getElem?_insert, beq_iff_eq]
      split
      · rename_i e; subst e; exact hv
      · rfl
    | none =>
      simp only [Std.HashMap.getElem?_erase, beq_iff_eq]
      split
      · rename_i e; subst e; exact hv
      · rfl
  rw [fire_mates_eq, hm_foldl _ _ _ _ _ h1, hm_foldl _ _ _ _ _ h2, hm_erase_fold]
  congr

theorem mem_fGone {q : Port} : q ∈ fGone S kinds mates a b ↔
    (q.1 = a ∨ q.1 = b) ∧ q.2 < (((vw kinds mates).kind q.1).map (agArity S)).getD 0 + 1 := by
  simp only [fGone, List.mem_append, mem_portsOf]
  constructor
  · rintro (⟨h, h'⟩ | ⟨h, h'⟩) <;> subst h <;> simp_all
  · rintro ⟨h | h, h'⟩ <;> subst h <;> simp_all

theorem fGone_of {q : Port} {g : Ag K} (hk : (vw kinds mates).kind q.1 = some g) (hn : q.2 ≤ agArity S g)
    (hab : q.1 = a ∨ q.1 = b) : q ∈ fGone S kinds mates a b :=
  (mem_fGone S kinds mates a b).mpr ⟨hab, by rw [hk]; simp; omega⟩

theorem fNear_spec {z : Ident} (hz : z ∈ fNear S kinds mates a b) :
    z ≠ a ∧ z ≠ b ∧ ∃ p t, p ∈ fGone S kinds mates a b ∧ (vw kinds mates).mate p = some t ∧ t.1 = z := by
  simp only [fNear, List.mem_filter, List.mem_map, List.mem_filterMap, Bool.and_eq_true, bne_iff_ne,
    ne_eq] at hz
  obtain ⟨⟨t, ⟨p, hp, ht⟩, rfl⟩, h1, h2⟩ := hz
  exact ⟨h1, h2, p, t, hp, ht, rfl⟩

theorem near_key {q : Port} (hq : q ∈ (fUpsNear S r kinds mates fresh a b).map Prod.fst) :
    q.1 ∈ fNear S kinds mates a b := by
  simp only [fUpsNear, List.mem_map, List.mem_flatMap, List.mem_filterMap] at hq
  obtain ⟨x, ⟨z, hz, p, hp, hpx⟩, rfl⟩ := hq
  have : x.1 = p := by
    split at hpx
    · split at hpx
      · cases hpx; rfl
      · cases hpx
    · cases hpx
  rw [this, (mem_portsOf.mp hp).1]; exact hz

theorem near_key_of (hi : (vw kinds mates).Involutive) (hR : InRange S (vw kinds mates)) {q t : Port} (hqa : q.1 ≠ a) (hqb : q.1 ≠ b)
    (hm : (vw kinds mates).mate q = some t) (ht : t.1 = a ∨ t.1 = b) :
    q ∈ (fUpsNear S r kinds mates fresh a b).map Prod.fst := by
  have back := hi _ _ hm
  obtain ⟨g, hg, hn⟩ := hR t.1 t.2 q back
  have tg : t ∈ fGone S kinds mates a b := fGone_of S kinds mates a b hg hn ht
  have hz : q.1 ∈ fNear S kinds mates a b := by
    simp only [fNear, List.mem_filter, List.mem_map, List.mem_filterMap, Bool.and_eq_true, bne_iff_ne, ne_eq]
    exact ⟨⟨q, ⟨t, tg, back⟩, rfl⟩, hqa, hqb⟩
  obtain ⟨g', hg', hn'⟩ := hR q.1 q.2 t hm
  simp only [fUpsNear, List.mem_map, List.mem_flatMap, List.mem_filterMap]
  refine ⟨(q, ((fire S r a b (vw kinds mates)).mate q).map (fRen fresh a b)), ⟨q.1, hz, q, ?_, ?_⟩, rfl⟩
  · exact mem_portsOf.mpr ⟨rfl, by rw [hg']; simp; omega⟩
  · show (match (vw kinds mates).mate q with
      | some t => if (t.1 == a || t.1 == b) = true then some (q, ((fire S r a b (vw kinds mates)).mate q).map (fRen fresh a b)) else none
      | none => none) = _
    rw [hm]
    rcases ht with h | h <;> simp [h]

theorem made_key {q : Port} (hq : q ∈ (fUpsMade S r kinds mates fresh a b).map Prod.fst) :
    ∃ k, q.1 = Ident.root (fresh + k) ∧ madeKind S r k ≠ none := by
  simp only [fUpsMade, List.mem_map, List.mem_flatMap, List.mem_filterMap, Option.map_eq_some_iff] at hq
  obtain ⟨x, ⟨⟨k, g⟩, hkg, i, _, u, _, rfl⟩, rfl⟩ := hq
  exact ⟨k, rfl, by rw [(mem_fMade S r).mp hkg]; simp⟩

theorem made_key_of {k n : Nat} {g : Ag K} {u : Port} (hk : madeKind S r k = some g) (hn : n ≤ agArity S g)
    (hu : (fire S r a b (vw kinds mates)).mate (Ident.made a b k, n) = some u) :
    (Ident.root (fresh + k), n) ∈ (fUpsMade S r kinds mates fresh a b).map Prod.fst := by
  simp only [fUpsMade, List.mem_map, List.mem_flatMap, List.mem_filterMap, Option.map_eq_some_iff]
  exact ⟨_, ⟨(k, g), (mem_fMade S r).mpr hk, n, List.mem_range.mpr (Nat.lt_succ_of_le hn), u, hu, rfl⟩, rfl⟩

/-- **One fire of the engine is the theory's fire**, up to the engine's short names for what it made:
`sren` renames the theory's net after the fire onto the engine's state after the fire. -/
theorem fire_view (roots : ∀ x, kinds[x]? ≠ none → ∃ n, x = Ident.root n ∧ n < fresh)
    (hP : (vw kinds mates).Present) (hi : (vw kinds mates).Involutive) (hR : InRange S (vw kinds mates)) (hRM : InRange S (fire S r a b (vw kinds mates)))
    (ha : kinds[a]? ≠ none) (hb : kinds[b]? ≠ none) :
    Iso (sren fresh a b) (fire S r a b (vw kinds mates)) (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.view := by
  have old : ∀ x, kinds[x]? ≠ none → madeIdx a b x = none ∧ ∀ k, x ≠ Ident.root (fresh + k) := by
    intro x hx
    obtain ⟨n, rfl, hn⟩ := roots x hx
    exact ⟨rfl, fun k e => by cases e; omega⟩
  have notKK : ∀ x, kinds[x]? ≠ none → ¬ ∃ k, madeKind S r k ≠ none ∧ x = Ident.root (fresh + k) := by
    rintro x hx ⟨k, _, rfl⟩; exact (old _ hx).2 k rfl
  have fresh0 : ∀ k, kinds[Ident.root (fresh + k)]? = none := by
    intro k
    cases h : kinds[Ident.root (fresh + k)]? with
    | none => rfl
    | some g => exact absurd rfl ((old _ (by simp [h])).2 k)
  have dead0 : ∀ (y : Ident) (n : Nat), kinds[y]? = none → mates[(y, n)]? = none := by
    intro y n hy
    cases hm : mates[(y, n)]? with
    | none => rfl
    | some t => exact absurd hy (hP (y, n) t hm).1
  have sr_made : ∀ k, sren fresh a b (.made a b k) = Ident.root (fresh + k) := by
    intro k; simp [sren, madeIdx_made]
  have sr_old : ∀ x, kinds[x]? ≠ none → sren fresh a b x = x := by
    intro x hx; simp [sren, (old x hx).1]
  have kget := fire_kinds_get S r kinds mates fresh a b
  have mget := fire_mates_get S r kinds mates fresh a b
  have rootK : ∀ k, (∃ k', madeKind S r k' ≠ none ∧ Ident.root (fresh + k) = Ident.root (fresh + k')) ↔
      madeKind S r k ≠ none := by
    intro k; constructor
    · rintro ⟨k', h, e⟩; injection e with e; have : k = k' := by omega
      subst this; exact h
    · intro h; exact ⟨k, h, rfl⟩
  refine ⟨?inj, ?kind, ?onto, ?mate, ?dead⟩
  case inj =>
    intro x y hx hy e
    rcases fire_kind_some S r a b (vw kinds mates) hx with ⟨k, rfl⟩ | ⟨_, hxm, hxN⟩ <;>
    rcases fire_kind_some S r a b (vw kinds mates) hy with ⟨k', rfl⟩ | ⟨_, hym, hyN⟩
    · rw [sr_made, sr_made] at e; injection e with e; have : k = k' := by omega
      subst this; rfl
    · rw [sr_made, sr_old y hyN] at e; exact absurd e.symm ((old y hyN).2 k)
    · rw [sr_made, sr_old x hxN] at e; exact absurd e ((old x hxN).2 k')
    · rwa [sr_old x hxN, sr_old y hyN] at e
  case kind =>
    intro x hx
    rcases fire_kind_some S r a b (vw kinds mates) hx with ⟨k, rfl⟩ | ⟨hxab, hxm, hxN⟩
    · rw [fire_kind_made] at hx ⊢
      show (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.kinds[sren fresh a b (.made a b k)]? = _
      rw [sr_made, kget, if_pos ((rootK k).mpr hx)]
      simp
    · rw [fire_kind_other S r a b (vw kinds mates) hxab hxm]
      show (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.kinds[sren fresh a b x]? = _
      rw [sr_old x hxN, kget, if_neg (notKK x hxN), if_neg hxab]; rfl
  case onto =>
    intro y hy
    change (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.kinds[y]? ≠ none at hy
    rw [kget] at hy
    by_cases h : ∃ k, madeKind S r k ≠ none ∧ y = Ident.root (fresh + k)
    · obtain ⟨k, hk, rfl⟩ := h
      exact ⟨.made a b k, by rw [fire_kind_made]; exact hk, sr_made k⟩
    · rw [if_neg h] at hy
      by_cases hab : y = a ∨ y = b
      · rw [if_pos hab] at hy; exact absurd rfl hy
      rw [if_neg hab] at hy
      exact ⟨y, by rw [fire_kind_other S r a b (vw kinds mates) hab (old y hy).1]; exact hy, sr_old y hy⟩
  case mate =>
    intro x n hx
    show (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.mates[(sren fresh a b x, n)]? = _
    rw [mget, ← fRen_eq]
    rcases fire_kind_some S r a b (vw kinds mates) hx with ⟨k, rfl⟩ | ⟨hxab, hxm, hxN⟩
    · rw [sr_made]
      by_cases hK : (Ident.root (fresh + k), n) ∈ (fUpsMade S r kinds mates fresh a b).map Prod.fst
      · rw [if_pos hK]; simp [wM]
      rw [if_neg hK]
      have nN : (Ident.root (fresh + k), n) ∉ (fUpsNear S r kinds mates fresh a b).map Prod.fst := by
        intro e
        obtain ⟨_, _, p, t, _, ht, htz⟩ := fNear_spec S kinds mates a b (near_key S r kinds mates fresh a b e)
        have := (hP p t ht).2
        rw [htz] at this
        exact this (fresh0 k)
      have nG : (Ident.root (fresh + k), n) ∉ fGone S kinds mates a b := by
        intro e
        rcases ((mem_fGone S kinds mates a b).mp e).1 with e | e
        · exact (old a ha).2 k e.symm
        · exact (old b hb).2 k e.symm
      rw [if_neg nN, if_neg nG, dead0 _ _ (fresh0 k)]
      cases hu : (fire S r a b (vw kinds mates)).mate (.made a b k, n) with
      | none => rfl
      | some u =>
        exfalso
        rw [fire_kind_made] at hx
        obtain ⟨g, hg⟩ := Option.ne_none_iff_exists'.mp hx
        obtain ⟨g', hg', hn⟩ := hRM _ _ _ hu
        rw [fire_kind_made, hg] at hg'; cases hg'
        exact hK (made_key_of S r kinds mates fresh a b hg hn hu)
    · have hxN' : kinds[x]? ≠ none := hxN
      rw [sr_old x hxN']
      have nM : (x, n) ∉ (fUpsMade S r kinds mates fresh a b).map Prod.fst := by
        intro e
        obtain ⟨k, hk, _⟩ := made_key S r kinds mates fresh a b e
        exact (old x hxN').2 k hk
      rw [if_neg nM]
      by_cases hN : (x, n) ∈ (fUpsNear S r kinds mates fresh a b).map Prod.fst
      · rw [if_pos hN]
      rw [if_neg hN]
      have nG : (x, n) ∉ fGone S kinds mates a b := fun e => hxab ((mem_fGone S kinds mates a b).mp e).1
      rw [if_neg nG, fire_mate_other S r a b (vw kinds mates) hxab hxm n]
      change mates[(x, n)]? = Option.map (fRen fresh a b) (relink r a b ((vw kinds mates).mate (x, n)))
      cases hm : (vw kinds mates).mate (x, n) with
      | none => exact hm
      | some t =>
        by_cases ht : t.1 = a ∨ t.1 = b
        · exact absurd (near_key_of S r kinds mates fresh a b hi hR (fun e => hxab (Or.inl e))
            (fun e => hxab (Or.inr e)) hm ht) hN
        · have htN : kinds[t.1]? ≠ none := (hP _ _ hm).2
          rw [relink_keep r (fun e => ht (Or.inl e)) (fun e => ht (Or.inr e)), fRen_eq]
          simp only [Option.map_some, pmap, sr_old _ htN]
          exact hm
  case dead =>
    intro y n hy
    change (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.kinds[y]? = none at hy
    show (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.mates[(y, n)]? = none
    rw [kget] at hy
    have nKK : ¬ ∃ k, madeKind S r k ≠ none ∧ y = Ident.root (fresh + k) := by
      intro h; rw [if_pos h] at hy
      obtain ⟨k, hk, rfl⟩ := h
      simp at hy; exact hk hy
    rw [if_neg nKK] at hy
    rw [mget]
    have nM : (y, n) ∉ (fUpsMade S r kinds mates fresh a b).map Prod.fst := by
      intro e
      obtain ⟨k, hk, hmk⟩ := made_key S r kinds mates fresh a b e
      exact nKK ⟨k, hmk, hk⟩
    have nN : (y, n) ∉ (fUpsNear S r kinds mates fresh a b).map Prod.fst := by
      intro e
      obtain ⟨h1, h2, p, t, _, ht, htz⟩ := fNear_spec S kinds mates a b (near_key S r kinds mates fresh a b e)
      have hyN : kinds[y]? ≠ none := by have := (hP p t ht).2; rw [htz] at this; exact this
      rw [if_neg (by rintro (h | h) <;> simp_all)] at hy
      exact hyN hy
    rw [if_neg nM, if_neg nN]
    by_cases hG : (y, n) ∈ fGone S kinds mates a b
    · rw [if_pos hG]
    rw [if_neg hG]
    by_cases hab : y = a ∨ y = b
    · cases hm : mates[(y, n)]? with
      | none => rfl
      | some t =>
        obtain ⟨g, hg, hn⟩ := hR y n t hm
        exact absurd (fGone_of S kinds mates a b (q := (y, n)) hg hn hab) hG
    · rw [if_neg hab] at hy; exact dead0 y n hy

theorem fire_roots (hRo : ∀ x, kinds[x]? ≠ none → ∃ n, x = Ident.root n ∧ n < fresh) :
    (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.Roots := by
  intro y hy
  change (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1.kinds[y]? ≠ none at hy
  rw [fire_kinds_get S r kinds mates fresh a b y] at hy
  rw [fire_fresh_eq]
  by_cases h : ∃ k, madeKind S r k ≠ none ∧ y = Ident.root (fresh + k)
  · obtain ⟨k, hk, rfl⟩ := h
    have := madeKind_lt' hk
    exact ⟨_, rfl, by omega⟩
  · rw [if_neg h] at hy
    by_cases hab : y = a ∨ y = b
    · rw [if_pos hab] at hy; exact absurd rfl hy
    rw [if_neg hab] at hy
    obtain ⟨n, rfl, hn⟩ := hRo y hy
    exact ⟨n, rfl, by omega⟩

end EngFire

/-! ## The engine against the theory: one forwarder removal -/

section EngCut
variable (S : Sig K) (kinds : Std.HashMap Ident (Ag K)) (mates : Std.HashMap Port Port) (fresh : Nat) (x : Ident)

def cGone : List Port := Eng.portsOf S (vw kinds mates) x
def cNbrs : List Port := ((cGone S kinds mates x).filterMap (vw kinds mates).mate).filter fun p => p.1 != x

theorem cut_mates_eq : (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.mates =
    ((cNbrs S kinds mates x).map fun p => (p, (unfwd x (vw kinds mates)).mate p)).foldl
      (fun m (p, u) => match u with | some v => m.insert p v | none => m.erase p)
      ((cGone S kinds mates x).foldl (fun m p => m.erase p) mates) := rfl

theorem cut_kinds_eq : (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.kinds = kinds.erase x := rfl
theorem cut_fresh_eq : (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.fresh = fresh := rfl

open Classical in
theorem cut_mates_get (q : Port) : (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.mates[q]? =
    if q ∈ cNbrs S kinds mates x then (unfwd x (vw kinds mates)).mate q
    else if q ∈ cGone S kinds mates x then none else mates[q]? := by
  have h : ∀ y ∈ (cNbrs S kinds mates x).map (fun p => (p, (unfwd x (vw kinds mates)).mate p)),
      ∀ (m : Std.HashMap Port Port) q,
      ((fun m (pu : Port × Option Port) => match pu with
        | (p, u) => match u with | some v => m.insert p v | none => m.erase p) m y)[q]? =
        if y.1 = q then (unfwd x (vw kinds mates)).mate q else m[q]? := by
    intro y hy m q
    obtain ⟨p, _, rfl⟩ := List.mem_map.mp hy
    cases hu : (unfwd x (vw kinds mates)).mate p with
    | some v =>
      simp only [hu, Std.HashMap.getElem?_insert, beq_iff_eq]
      split
      · rename_i e; subst e; exact hu.symm
      · rfl
    | none =>
      simp only [hu, Std.HashMap.getElem?_erase, beq_iff_eq]
      split
      · rename_i e; subst e; exact hu.symm
      · rfl
  rw [cut_mates_eq, hm_foldl _ _ _ _ _ h, hm_erase_fold, List.map_map]
  have : (Prod.fst ∘ fun p => (p, (unfwd x (vw kinds mates)).mate p)) = id := rfl
  rw [this, List.map_id]
  congr

theorem pmap_id_map (o : Option Port) : o.map (pmap id) = o := by cases o <;> rfl

/-- **One forwarder removal of the engine is the theory's `unfwd`**, names unchanged. -/
theorem cut_view (hP : (vw kinds mates).Present) (hi : (vw kinds mates).Involutive)
    (hR : InRange S (vw kinds mates)) :
    Iso id (unfwd x (vw kinds mates)) (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.view := by
  have kget : ∀ y, (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.view.kind y = if y = x then none else kinds[y]? := by
    intro y
    show (kinds.erase x)[y]? = _
    simp only [Std.HashMap.getElem?_erase, beq_iff_eq]
    by_cases h : y = x
    · subst h; simp
    · simp [h, Ne.symm h]
  have mget := cut_mates_get S kinds mates fresh x
  have nbr : ∀ q, q ∈ cNbrs S kinds mates x → q.1 ≠ x ∧ kinds[q.1]? ≠ none := by
    intro q hq
    simp only [cNbrs, List.mem_filter, List.mem_filterMap, bne_iff_ne, ne_eq] at hq
    obtain ⟨⟨p, _, hp⟩, hx⟩ := hq
    exact ⟨hx, (hP p q hp).2⟩
  have gone : ∀ q, q ∈ cGone S kinds mates x → q.1 = x := fun q hq => (mem_portsOf.mp hq).1
  refine ⟨fun _ _ _ _ e => e, ?_, ?_, ?_, ?_⟩
  · intro y hy
    rw [unfwd_kind] at hy ⊢
    show (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.view.kind y = _
    rw [kget]; rfl
  · intro y hy
    rw [kget] at hy
    refine ⟨y, ?_, rfl⟩
    rw [unfwd_kind]; exact hy
  · intro y n hy
    rw [unfwd_kind] at hy
    by_cases hyx : y = x
    · simp [hyx] at hy
    rw [if_neg hyx] at hy
    show (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.mates[(y, n)]? = _
    rw [mget, pmap_id_map]
    by_cases hN : (y, n) ∈ cNbrs S kinds mates x
    · rw [if_pos hN]
    rw [if_neg hN, if_neg (fun e => hyx (gone _ e)), unfwd_mate, if_neg hyx]
    change mates[(y, n)]? = via x (vw kinds mates) ((vw kinds mates).mate (y, n))
    cases hm : (vw kinds mates).mate (y, n) with
    | none => exact hm
    | some t =>
      by_cases htx : t.1 = x
      · exfalso
        have back := hi _ _ hm
        obtain ⟨g, hg, hn⟩ := hR t.1 t.2 (y, n) back
        apply hN
        simp only [cNbrs, List.mem_filter, List.mem_filterMap, bne_iff_ne, ne_eq]
        refine ⟨⟨t, mem_portsOf.mpr ⟨htx, ?_⟩, back⟩, hyx⟩
        rw [← htx, hg]; simp; omega
      · rw [via_keep htx]; exact hm
  · intro y n hy
    rw [kget] at hy
    show (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.mates[(y, n)]? = none
    rw [mget]
    have nN : (y, n) ∉ cNbrs S kinds mates x := by
      intro e
      obtain ⟨h1, h2⟩ := nbr _ e
      rw [if_neg h1] at hy; exact h2 hy
    rw [if_neg nN]
    by_cases hG : (y, n) ∈ cGone S kinds mates x
    · rw [if_pos hG]
    rw [if_neg hG]
    cases hm : mates[(y, n)]? with
    | none => rfl
    | some t =>
      exfalso
      obtain ⟨g, hg, hn⟩ := hR y n t hm
      by_cases hyx : y = x
      · subst hyx
        exact hG (mem_portsOf.mpr ⟨rfl, by show n < _; rw [hg]; simp; omega⟩)
      · rw [if_neg hyx] at hy
        change kinds[y]? = some g at hg
        rw [hy] at hg; cases hg

theorem cut_roots (hRo : ∀ y, kinds[y]? ≠ none → ∃ n, y = Ident.root n ∧ n < fresh) :
    (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.Roots := by
  intro y hy
  change (kinds.erase x)[y]? ≠ none at hy
  rw [Std.HashMap.getElem?_erase] at hy
  split at hy
  · exact absurd rfl hy
  · exact hRo y hy

end EngCut

/-! ## Keeping wires on real ports -/

/-- Every made port a rule wires is a port its agent has. -/
def RuleInRange (S : Sig K) (r : GRule K) : Prop :=
  ∀ w ∈ r.wires, ∀ j n, (w.1 = .new j n ∨ w.2 = .new j n) → ∃ k, r.made[j]? = some k ∧ n ≤ arity S k

theorem fire_inRange {S : Sig K} {r : GRule K} {a b : Ident} {N : Net K} (hR : InRange S N)
    (hr : RuleInRange S r) : InRange S (fire S r a b N) := by
  intro y n q h
  by_cases hab : y = a ∨ y = b
  · rw [fire_mate_pair S r a b N hab] at h; cases h
  cases hk : madeIdx a b y with
  | none =>
    rw [fire_mate_other S r a b N hab hk] at h
    cases hm : N.mate (y, n) with
    | none => rw [hm] at h; cases h
    | some t =>
      obtain ⟨g, hg, hn⟩ := hR y n t hm
      exact ⟨g, by rw [fire_kind_other S r a b N hab hk, hg], hn⟩
  | some k =>
    have := madeIdx_some hk; subst this
    rw [fire_mate_made] at h
    rw [fire_kind_made]
    unfold madeMate at h
    unfold madeKind
    by_cases hlt : k < r.made.length
    · rw [if_pos hlt] at h; rw [if_pos hlt]
      unfold partner at h
      cases hf : findEnd (.new k n) r.wires 0 with
      | none => rw [hf] at h; cases h
      | some t =>
        obtain ⟨w, f, first⟩ := t
        have ⟨hmem, _⟩ := findEnd_mem hf
        obtain ⟨kk, hkk, hn⟩ := hr _ hmem k n (by cases first <;> simp)
        exact ⟨.base kk, by rw [hkk]; rfl, hn⟩
    · rw [if_neg hlt] at h; rw [if_neg hlt]
      cases hw : r.wires[k - r.made.length]? with
      | none => rw [hw] at h; cases h
      | some e12 =>
        obtain ⟨e1, e2⟩ := e12
        rw [hw] at h
        simp only at h ⊢
        by_cases hs : (e1.isSlot && e2.isSlot) = true
        · rw [if_pos hs] at h; rw [if_pos hs]
          refine ⟨_, rfl, ?_⟩
          show n ≤ 1
          split at h
          · omega
          · split at h
            · omega
            · cases h
        · rw [if_neg hs] at h; cases h

theorem unfwd_inRange {S : Sig K} {x : Ident} {N : Net K} (hR : InRange S N) : InRange S (unfwd x N) := by
  intro y n q h
  rw [unfwd_mate] at h
  by_cases hyx : y = x
  · subst hyx; rw [if_pos rfl] at h; cases h
  simp only [show ¬ (y, n).1 = x from hyx, if_false] at h
  cases hm : N.mate (y, n) with
  | none => rw [hm] at h; cases h
  | some t =>
    obtain ⟨g, hg, hn⟩ := hR y n t hm
    exact ⟨g, by rw [unfwd_kind, if_neg hyx, hg], hn⟩

/-! ## The simulation: an engine state is a renamed net of the theory -/

/-- The engine state `e` is the theory's net `F` renamed by `σ`; `F` is good with wires on real
ports, the engine's names are roots below its counter, and the kept root keeps its name. -/
structure Sim (S : Sig K) (root : Ident) (F : Net K) (e : Eng K) (σ : Ident → Ident) : Prop where
  iso : Iso σ F e.view
  good : F.Good S
  range : InRange S F
  roots : e.Roots
  rootFix : σ root = root
  rootIn : ∃ P, F.kind root = some (.fwd P)

theorem Sim.present {S : Sig K} {root : Ident} {F : Net K} {e : Eng K} {σ : Ident → Ident}
    (h : Sim S root F e σ) : e.view.Present := h.iso.present h.good.present
theorem Sim.involutive {S : Sig K} {root : Ident} {F : Net K} {e : Eng K} {σ : Ident → Ident}
    (h : Sim S root F e σ) : e.view.Involutive := h.iso.involutive h.good.present h.good.involutive
theorem Sim.inRange {S : Sig K} {root : Ident} {F : Net K} {e : Eng K} {σ : Ident → Ident}
    (h : Sim S root F e σ) : InRange S e.view := h.iso.inRange h.range

/-- At the start: the loaded net itself, names unchanged. -/
theorem Sim.start {S : Sig K} {root : Ident} {e : Eng K} (hg : e.view.Good S) (hR : InRange S e.view)
    (hr : e.Roots) (hk : ∃ P, e.view.kind root = some (.fwd P)) : Sim S root e.view e id :=
  ⟨Iso.refl (fun y n hy => by
      cases hm : e.view.mate (y, n) with
      | none => rfl
      | some t => exact absurd hy (hg.present _ _ hm).1), hg, hR, hr, rfl, hk⟩

section SimSteps
variable [DecidableEq K] {S : Sig K} {T : List (GRule K)} {root : Ident}

/-- **An engine fire is a step of the theory**: the pair it fires, read back through the renaming,
is an active pair of the theory's net with the rule's kinds, and the states stay related. -/
theorem Sim.fire (hT : RunOk S T) (hRR : ∀ r ∈ T, RuleInRange S r) (hroot : ∃ n, root = Ident.root n)
    {F : Net K} {e : Eng K} {σ : Ident → Ident} (sim : Sim S root F e σ) {a' b' : Ident} {r : GRule K}
    (hr : r ∈ T) (hab : e.view.mate (a', 0) = some (b', 0)) (hka : e.view.kind a' = some (.base r.a))
    (hkb : e.view.kind b' = some (.base r.b)) :
    ∃ a b, F.mate (a, 0) = some (b, 0) ∧ F.kind a = some (.base r.a) ∧ F.kind b = some (.base r.b) ∧
      Sim S root (_root_.fire S r a b F) (e.fire S a' b' r).1 (sren e.fresh a' b' ∘ ext σ a b a' b') := by
  obtain ⟨kinds, mates, fresh⟩ := e
  have I := sim.iso
  have pa' : (vw kinds mates).kind a' ≠ none := by rw [show (vw kinds mates).kind a' = _ from hka]; simp
  have pb' : (vw kinds mates).kind b' ≠ none := by rw [show (vw kinds mates).kind b' = _ from hkb]; simp
  obtain ⟨a, ha, rfl⟩ := I.onto a' pa'
  obtain ⟨b, hb, rfl⟩ := I.onto _ pb'
  have hkaF : F.kind a = some (.base r.a) := by rw [← I.kind a ha]; exact hka
  have hkbF : F.kind b = some (.base r.b) := by rw [← I.kind b hb]; exact hkb
  have hmF : F.mate (a, 0) = some (b, 0) := by
    have := I.mate a 0 ha
    rw [hab] at this
    cases hm : F.mate (a, 0) with
    | none => rw [hm] at this; cases this
    | some t =>
      rw [hm] at this
      obtain ⟨z, m⟩ := t
      simp only [Option.map_some, Option.some.injEq, pmap, Prod.mk.injEq] at this
      obtain ⟨e1, rfl⟩ := this
      have hz : F.kind z ≠ none := (sim.good.present _ _ hm).2
      rw [I.inj _ _ hb hz e1]
  have ok : FireIsoOk σ F (Eng.view ⟨kinds, mates, fresh⟩) a b := by
    refine ⟨I, ha, hb, fun k => sim.good.tidy.fresh a b k ha hb, fun k => ?_, sim.good.present⟩
    cases h : (Eng.view ⟨kinds, mates, fresh⟩).kind (.made (σ a) (σ b) k) with
    | none => rfl
    | some g =>
      obtain ⟨n, hn, _⟩ := sim.roots _ (by change (Eng.view ⟨kinds, mates, fresh⟩).kind _ ≠ none; rw [h]; simp)
      cases hn
  have I1 := fire_iso (S := S) (r := r) ok
  have I2 := fire_view S r kinds mates fresh (σ a) (σ b) sim.roots sim.present sim.involutive sim.inRange
    (fire_inRange sim.inRange (hRR r hr)) pa' pb'
  refine ⟨a, b, hmF, hkaF, hkbF, I1.trans I2, ?_, fire_inRange sim.range (hRR r hr),
    fire_roots S r kinds mates fresh _ _ sim.roots, ?_, ?_⟩
  · exact hT.step_good sim.good ⟨a, b, r, hr, hmF, hkaF, hkbF, rfl⟩
  · obtain ⟨n, rfl⟩ := hroot
    show sren fresh (σ a) (σ b) (ext σ a b (σ a) (σ b) (Ident.root n)) = Ident.root n
    simp only [ext, sren, madeIdx]
    rw [sim.rootFix]
  · obtain ⟨P, hP⟩ := sim.rootIn
    obtain ⟨n, rfl⟩ := hroot
    have hra : Ident.root n ≠ a := by rintro rfl; rw [hP] at hkaF; cases hkaF
    have hrb : Ident.root n ≠ b := by rintro rfl; rw [hP] at hkbF; cases hkbF
    have hn : ¬(Ident.root n = a ∨ Ident.root n = b) := by
      rintro (h | h)
      · exact hra h
      · exact hrb h
    exact ⟨P, by rw [fire_kind_other S r a b F hn rfl, hP]⟩

/-- **An engine forwarder removal is a forwarder removal of the theory**, not of the kept root. -/
theorem Sim.cut {F : Net K} {e : Eng K} {σ : Ident → Ident} (sim : Sim S root F e σ) {x' : Ident} {P : Pol}
    (hx : e.view.kind x' = some (.fwd P)) (hroot : x' ≠ root) :
    ∃ x, F.kind x = some (.fwd P) ∧ x ≠ root ∧ Sim S root (unfwd x F) (e.cut S x').1 σ := by
  obtain ⟨kinds, mates, fresh⟩ := e
  have I := sim.iso
  have px' : (Eng.view ⟨kinds, mates, fresh⟩).kind x' ≠ none := by rw [hx]; simp
  obtain ⟨x, hxF, rfl⟩ := I.onto x' px'
  have hkx : F.kind x = some (.fwd P) := by rw [← I.kind x hxF]; exact hx
  have hxr : x ≠ root := by rintro rfl; exact hroot sim.rootFix
  have I1 := unfwd_iso I sim.good.present hxF
  have I2 := cut_view S kinds mates fresh (σ x) sim.present sim.involutive sim.inRange
  refine ⟨x, hkx, hxr, I1.trans I2, cut_good (keep := (· = root)) sim.good ⟨x, P, hxr, hkx, rfl⟩, unfwd_inRange sim.range,
    cut_roots S kinds mates fresh _ sim.roots, sim.rootFix, ?_⟩
  obtain ⟨Q, hQ⟩ := sim.rootIn
  exact ⟨Q, by rw [unfwd_kind, if_neg (Ne.symm hxr), hQ]⟩

end SimSteps


/-! ## Labelled runs and what the tally counts -/

/-- A run of the theory from `N`, each move labelled: `some i` fires with rule `T[i]`, `none` removes
a forwarder. -/
inductive Trace (S : Sig K) (T : List (GRule K)) (keep : Ident → Prop) (N : Net K) :
    List (Option Nat) → Net K → Prop
  | nil : Trace S T keep N [] N
  | fire {l : List (Option Nat)} {M : Net K} {i : Nat} {r : GRule K} (a b : Ident) :
      Trace S T keep N l M → T[i]? = some r → M.mate (a, 0) = some (b, 0) →
      M.kind a = some (.base r.a) → M.kind b = some (.base r.b) →
      Trace S T keep N (l ++ [some i]) (_root_.fire S r a b M)
  | cut {l : List (Option Nat)} {M : Net K} (x : Ident) (P : Pol) :
      Trace S T keep N l M → ¬ keep x → M.kind x = some (.fwd P) →
      Trace S T keep N (l ++ [none]) (unfwd x M)

theorem _root_.Path.snoc {α : Type} {R : α → α → Prop} {n : Nat} {a b c : α} (h : Path R n a b) (hs : R b c) :
    Path R (n + 1) a c := by
  induction h with
  | refl a => exact .step hs (.refl _)
  | step h1 _ ih => exact .step h1 (ih hs)

section Traces
variable {S : Sig K} {T : List (GRule K)} {keep : Ident → Prop}

/-- A labelled run is a run of `Move`s of its length. -/
theorem Trace.path {N M : Net K} {l : List (Option Nat)} (h : Trace S T keep N l M) :
    Path (Move S T keep) l.length N M := by
  induction h with
  | nil => exact .refl _
  | fire a b _ hi hab hka hkb ih =>
    rw [List.length_append]; exact ih.snoc (Or.inl ⟨a, b, _, List.mem_of_getElem? hi, hab, hka, hkb, rfl⟩)
  | cut x P _ hk hx ih => rw [List.length_append]; exact ih.snoc (Or.inr ⟨x, P, hk, hx, rfl⟩)

theorem Trace.append {N M P : Net K} {l l' : List (Option Nat)} (h : Trace S T keep N l M)
    (h' : Trace S T keep M l' P) : Trace S T keep N (l ++ l') P := by
  induction h' with
  | nil => simpa using h
  | fire a b _ hi hab hka hkb ih => rw [← List.append_assoc]; exact .fire a b ih hi hab hka hkb
  | cut x P _ hk hx ih => rw [← List.append_assoc]; exact .cut x P ih hk hx

theorem length_eq_some_none (l : List (Option Nat)) :
    l.length = (l.filter Option.isSome).length + l.count none := by
  induction l with
  | nil => rfl
  | cons x l ih => cases x <;> simp [List.filter_cons, List.count_cons, ih] <;> omega

end Traces

/-- What the tally adds along a labelled run: a fire per `some i`, counted for rule i, and a removal
per `none`. -/
def Tal (t t' : Tally) (l : List (Option Nat)) : Prop :=
  t'.fires = t.fires + (l.filter Option.isSome).length ∧ t'.cuts = t.cuts + l.count none ∧
  ∀ i, t'.perRule[i]? = (t.perRule[i]?).map (· + l.count (some i))

theorem Tal.refl (t : Tally) : Tal t t [] := ⟨by simp, by simp, fun i => by cases t.perRule[i]? <;> simp⟩

theorem Tal.trans {t1 t2 t3 : Tally} {l1 l2 : List (Option Nat)} (h1 : Tal t1 t2 l1) (h2 : Tal t2 t3 l2) :
    Tal t1 t3 (l1 ++ l2) := by
  refine ⟨by rw [h2.1, h1.1, List.filter_append, List.length_append]; omega,
    by rw [h2.2.1, h1.2.1, List.count_append]; omega, fun i => ?_⟩
  rw [h2.2.2, h1.2.2, Option.map_map, List.count_append]
  cases t1.perRule[i]? <;> simp; omega

/-! ## The round loop -/

def fireF (S : Sig K) (x : Eng K × List Ident × Tally) (y : Ident × Ident × GRule K × Nat) :
    Eng K × List Ident × Tally :=
  if (x.1.kinds[y.1]?.isSome && x.1.kinds[y.2.1]?.isSome) = true then
    ((Eng.fire S x.1 y.1 y.2.1 y.2.2.1).1, (Eng.fire S x.1 y.1 y.2.1 y.2.2.1).2 ++ x.2.1,
      { fires := x.2.2.fires + 1, cuts := x.2.2.cuts, rounds := x.2.2.rounds,
        perRule := x.2.2.perRule.modify y.2.2.2 fun x => x + 1 })
  else (x.1, x.2.1, x.2.2)

def cutF (S : Sig K) (root : Ident) (x : Eng K × List Ident × Tally) (y : Ident) : Eng K × List Ident × Tally :=
  if x.1.isFwd root y = true then
    ((Eng.cut S x.1 y).1, (Eng.cut S x.1 y).2 ++ x.2.1,
      { fires := x.2.2.fires, cuts := x.2.2.cuts + 1, rounds := x.2.2.rounds, perRule := x.2.2.perRule })
  else (x.1, x.2.1, x.2.2)

/-- One turn of the loop, unfolded. -/
theorem rounds_succ [DecidableEq K] (S : Sig K) (T : List (GRule K)) (root : Ident) (budget n : Nat) (e : Eng K)
    (cands : List Ident) (t : Tally) :
    Eng.rounds S T root budget (n + 1) e cands t =
      if (cands.filterMap (Eng.activeAt T e)).isEmpty then some (e, t) else
      if ((((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t)).2.1.filter
          (((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t)).1.isFwd root)).foldl (cutF S root)
          ((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t))).2.2.fires +
        ((((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t)).2.1.filter
          (((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t)).1.isFwd root)).foldl (cutF S root)
          ((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t))).2.2.cuts > budget then none
      else
        let B := (((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t)).2.1.filter
          (((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t)).1.isFwd root)).foldl (cutF S root)
          ((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t))
        Eng.rounds S T root budget n B.1 B.2.1 { B.2.2 with rounds := B.2.2.rounds + 1 } := by
  simp only [Eng.rounds]; rfl


section Loop
variable [DecidableEq K] {S : Sig K} {T : List (GRule K)} {root : Ident}

theorem findRule_spec {x y : K} : ∀ (T : List (GRule K)) (i0 : Nat) {i : Nat} {r : GRule K},
    Eng.findRule x y T i0 = some (i, r) →
      i0 ≤ i ∧ T[i - i0]? = some r ∧ ((r.a = x ∧ r.b = y) ∨ (r.a = y ∧ r.b = x))
  | [], _, _, _, h => by simp [Eng.findRule] at h
  | s :: T, i0, i, r, h => by
    simp only [Eng.findRule] at h
    split at h
    · simp only [Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      rename_i hs
      exact ⟨Nat.le_refl _, by simp, hs⟩
    · obtain ⟨h1, h2, h3⟩ := findRule_spec T (i0 + 1) h
      refine ⟨by omega, ?_, h3⟩
      have : i - i0 = (i - (i0 + 1)) + 1 := by omega
      rw [this, List.getElem?_cons_succ]; exact h2

/-- A pair in a round's list: its rule is `T[i]`, its names are old, and while both its agents are
still there, they still face each other with the rule's kinds. -/
def PairOk (T : List (GRule K)) (e : Eng K) (p : Ident × Ident × GRule K × Nat) : Prop :=
  T[p.2.2.2]? = some p.2.2.1 ∧ (∃ n, p.1 = Ident.root n ∧ n < e.fresh) ∧
  (∃ n, p.2.1 = Ident.root n ∧ n < e.fresh) ∧
  (e.kinds[p.1]? ≠ none → e.kinds[p.2.1]? ≠ none →
    e.view.mate (p.1, 0) = some (p.2.1, 0) ∧ e.view.kind p.1 = some (.base p.2.2.1.a) ∧
      e.view.kind p.2.1 = some (.base p.2.2.1.b))

theorem activeAt_ok {e : Eng K} (hr : e.Roots) (hi : e.view.Involutive) {x : Ident}
    {p : Ident × Ident × GRule K × Nat} (h : Eng.activeAt T e x = some p) : PairOk T e p := by
  unfold Eng.activeAt at h
  split at h
  · next kx y hkx hmx =>
    split at h
    · next ky hky =>
      cases hf : Eng.findRule kx ky T 0 with
      | none => rw [hf] at h; cases h
      | some ir =>
        obtain ⟨i, r⟩ := ir
        rw [hf] at h
        obtain ⟨_, hT, hab⟩ := findRule_spec T 0 hf
        simp only [Nat.sub_zero] at hT
        have px : e.kinds[x]? ≠ none := by rw [hkx]; simp
        have py : e.kinds[y]? ≠ none := by rw [hky]; simp
        obtain ⟨nx, rfl, hnx⟩ := hr _ px
        obtain ⟨ny, rfl, hny⟩ := hr _ py
        have hmy : e.view.mate (Ident.root ny, 0) = some (Ident.root nx, 0) := hi _ _ hmx
        simp only [Option.map_some, Option.some.injEq] at h
        split at h
        · next hra =>
          subst h
          have hrb : r.b = ky := by
            rcases hab with ⟨_, h⟩ | ⟨h1, h2⟩
            · exact h
            · rw [h2, ← hra, h1]
          refine ⟨hT, ⟨nx, rfl, hnx⟩, ⟨ny, rfl, hny⟩, fun _ _ => ⟨hmx, ?_, ?_⟩⟩
          · show e.kinds[Ident.root nx]? = some (.base r.a); rw [hra]; exact hkx
          · show e.kinds[Ident.root ny]? = some (.base r.b); rw [hrb]; exact hky
        · next hra =>
          subst h
          have h2 : r.a = ky ∧ r.b = kx := by
            rcases hab with ⟨h, _⟩ | h
            · exact absurd h hra
            · exact h
          refine ⟨hT, ⟨ny, rfl, hny⟩, ⟨nx, rfl, hnx⟩, fun _ _ => ⟨hmy, ?_, ?_⟩⟩
          · show e.kinds[Ident.root ny]? = some (.base r.a); rw [h2.1]; exact hky
          · show e.kinds[Ident.root nx]? = some (.base r.b); rw [h2.2]; exact hkx
    · cases h
  · cases h

/-- Firing one pair of the round leaves every other listed pair as it was. -/
theorem pairOk_fire (hRR : ∀ r ∈ T, RuleInRange S r) {F : Net K} {e : Eng K} {σ : Ident → Ident}
    (sim : Sim S root F e σ) {c d : Ident} {r' : GRule K} (hr' : r' ∈ T)
    (hc : e.kinds[c]? ≠ none) (hd : e.kinds[d]? ≠ none) {p : Ident × Ident × GRule K × Nat}
    (hp : PairOk T e p) : PairOk T (e.fire S c d r').1 p := by
  obtain ⟨kinds, mates, fresh⟩ := e
  obtain ⟨a, b, r, i⟩ := p
  obtain ⟨hT, ⟨na, rfl, hna⟩, ⟨nb, rfl, hnb⟩, hact⟩ := hp
  change na < fresh at hna
  change nb < fresh at hnb
  have hf : (Eng.fire S ⟨kinds, mates, fresh⟩ c d r').1.fresh = fresh + r'.made.length + r'.wires.length :=
    fire_fresh_eq S r' kinds mates fresh c d
  refine ⟨hT, ⟨na, rfl, by rw [hf]; omega⟩, ⟨nb, rfl, by rw [hf]; omega⟩, fun pa pb => ?_⟩
  have kget := fire_kinds_get S r' kinds mates fresh c d
  have notNew : ∀ n, n < fresh → ¬ ∃ k, madeKind S r' k ≠ none ∧ Ident.root n = Ident.root (fresh + k) := by
    rintro n hn ⟨k, _, e⟩; injection e with e; omega
  have back : ∀ n, n < fresh → (Eng.fire S ⟨kinds, mates, fresh⟩ c d r').1.kinds[Ident.root n]? ≠ none →
      ¬ (Ident.root n = c ∨ Ident.root n = d) ∧ kinds[Ident.root n]? ≠ none := by
    intro n hn h
    rw [kget, if_neg (notNew n hn)] at h
    by_cases hcd : Ident.root n = c ∨ Ident.root n = d
    · rw [if_pos hcd] at h; exact absurd rfl h
    · rw [if_neg hcd] at h; exact ⟨hcd, h⟩
  obtain ⟨acd, pa0⟩ := back na hna pa
  obtain ⟨bcd, pb0⟩ := back nb hnb pb
  obtain ⟨hab, hka, hkb⟩ := hact pa0 pb0
  have I2 := fire_view S r' kinds mates fresh c d sim.roots sim.present sim.involutive sim.inRange
    (fire_inRange sim.inRange (hRR r' hr')) hc hd
  have srn : ∀ n, sren fresh c d (Ident.root n) = Ident.root n := fun n => rfl
  have kfa : (fire S r' c d (vw kinds mates)).kind (Ident.root na) = some (.base r.a) :=
    (fire_kind_other S r' c d _ acd rfl).trans hka
  have kfb : (fire S r' c d (vw kinds mates)).kind (Ident.root nb) = some (.base r.b) :=
    (fire_kind_other S r' c d _ bcd rfl).trans hkb
  refine ⟨?_, ?_, ?_⟩
  · have := I2.mate (Ident.root na) 0 (by rw [kfa]; simp)
    rw [srn] at this
    rw [this, fire_mate_other S r' c d _ acd rfl, show (vw kinds mates).mate (Ident.root na, 0) = _ from hab,
      relink_keep r' (fun e => bcd (Or.inl e)) (fun e => bcd (Or.inr e))]
    rfl
  · have := I2.kind (Ident.root na) (by rw [kfa]; simp)
    rw [srn] at this; rw [this, kfa]
  · have := I2.kind (Ident.root nb) (by rw [kfb]; simp)
    rw [srn] at this; rw [this, kfb]

theorem tal_fire (t : Tally) (i : Nat) :
    Tal t ({ fires := t.fires + 1, cuts := t.cuts, rounds := t.rounds,
             perRule := t.perRule.modify i (fun x => x + 1) } : Tally) [some i] := by
  refine ⟨by simp, by simp, fun j => ?_⟩
  rw [Array.getElem?_modify]
  by_cases h : i = j
  · subst h; simp
  · rw [if_neg h]
    have : [some i].count (some j) = 0 := by simp [List.count_cons]; omega
    rw [this]; cases t.perRule[j]? <;> rfl

theorem tal_cut (t : Tally) :
    Tal t ({ fires := t.fires, cuts := t.cuts + 1, rounds := t.rounds, perRule := t.perRule } : Tally) [none] := by
  refine ⟨by simp, by simp, fun j => ?_⟩
  have : [(none : Option Nat)].count (some j) = 0 := by simp [List.count_cons]
  rw [this]; cases t.perRule[j]? <;> rfl

/-- The fire half of a round makes a labelled run of the theory. -/
theorem fires_fold (hT : RunOk S T) (hRR : ∀ r ∈ T, RuleInRange S r) (hroot : ∃ n, root = Ident.root n)
    {F0 : Net K} : ∀ (ps : List (Ident × Ident × GRule K × Nat)) (st : Eng K × List Ident × Tally)
      (F : Net K) (σ : Ident → Ident) (l0 : List (Option Nat)),
      Trace S T (· = root) F0 l0 F → Sim S root F st.1 σ → (∀ p ∈ ps, PairOk T st.1 p) →
      ∃ F' σ' l, Trace S T (· = root) F0 (l0 ++ l) F' ∧ Sim S root F' (ps.foldl (fireF S) st).1 σ' ∧
        Tal st.2.2 (ps.foldl (fireF S) st).2.2 l
  | [], st, F, σ, l0, ht, sim, _ => ⟨F, σ, [], by simpa using ht, sim, Tal.refl _⟩
  | p :: ps, st, F, σ, l0, ht, sim, hps => by
    rw [List.foldl_cons]
    obtain ⟨a, b, r, i⟩ := p
    by_cases hc : (st.1.kinds[a]?.isSome && st.1.kinds[b]?.isSome) = true
    · have hst : fireF S st (a, b, r, i) = ((Eng.fire S st.1 a b r).1, (Eng.fire S st.1 a b r).2 ++ st.2.1,
          { fires := st.2.2.fires + 1, cuts := st.2.2.cuts, rounds := st.2.2.rounds,
            perRule := st.2.2.perRule.modify i fun x => x + 1 }) := if_pos hc
      simp only [Bool.and_eq_true, Option.isSome_iff_ne_none] at hc
      obtain ⟨hTi, _, _, hact⟩ := hps _ List.mem_cons_self
      obtain ⟨hab, hka, hkb⟩ := hact hc.1 hc.2
      have hr : r ∈ T := List.mem_of_getElem? hTi
      obtain ⟨a0, b0, hab0, hka0, hkb0, sim'⟩ := sim.fire hT hRR hroot hr hab hka hkb
      have ht' := Trace.fire a0 b0 ht hTi hab0 hka0 hkb0
      have hps' : ∀ q ∈ ps, PairOk T (fireF S st (a, b, r, i)).1 q := by
        rw [hst]; intro q hq; exact pairOk_fire hRR sim hr hc.1 hc.2 (hps q (List.mem_cons_of_mem _ hq))
      have sim'' : Sim S root (_root_.fire S r a0 b0 F) (fireF S st (a, b, r, i)).1
          (sren st.1.fresh a b ∘ ext σ a0 b0 a b) := by rw [hst]; exact sim'
      obtain ⟨F', σ', l, ht2, sim2, tal2⟩ := fires_fold hT hRR hroot ps _ _ _ _ ht' sim'' hps'
      refine ⟨F', σ', some i :: l, by simpa using ht2, sim2, ?_⟩
      rw [hst] at tal2 ⊢
      exact (tal_fire st.2.2 i).trans tal2
    · have hst : fireF S st (a, b, r, i) = (st.1, st.2.1, st.2.2) := if_neg hc
      rw [hst]
      exact fires_fold hT hRR hroot ps st F σ l0 ht sim (fun q hq => hps q (List.mem_cons_of_mem _ hq))

/-- The forwarder half of a round makes a labelled run of the theory. -/
theorem cuts_fold {F0 : Net K} : ∀ (xs : List Ident) (st : Eng K × List Ident × Tally)
      (F : Net K) (σ : Ident → Ident) (l0 : List (Option Nat)),
      Trace S T (· = root) F0 l0 F → Sim S root F st.1 σ →
      ∃ F' σ' l, Trace S T (· = root) F0 (l0 ++ l) F' ∧ Sim S root F' (xs.foldl (cutF S root) st).1 σ' ∧
        Tal st.2.2 (xs.foldl (cutF S root) st).2.2 l
  | [], st, F, σ, l0, ht, sim => ⟨F, σ, [], by simpa using ht, sim, Tal.refl _⟩
  | x :: xs, st, F, σ, l0, ht, sim => by
    rw [List.foldl_cons]
    by_cases hc : st.1.isFwd root x = true
    · have hst : cutF S root st x = ((Eng.cut S st.1 x).1, (Eng.cut S st.1 x).2 ++ st.2.1,
          { fires := st.2.2.fires, cuts := st.2.2.cuts + 1, rounds := st.2.2.rounds,
            perRule := st.2.2.perRule }) := if_pos hc
      simp only [Eng.isFwd, Bool.and_eq_true, bne_iff_ne, ne_eq] at hc
      obtain ⟨hxr, hk⟩ := hc
      have : ∃ P, st.1.view.kind x = some (.fwd P) := by
        show ∃ P, st.1.kinds[x]? = some (.fwd P)
        revert hk; cases st.1.kinds[x]? with
        | none => intro h; cases h
        | some g => cases g with
          | base _ => intro h; cases h
          | fwd P => intro _; exact ⟨P, rfl⟩
      obtain ⟨P, hP⟩ := this
      obtain ⟨x0, hx0, hx0r, sim'⟩ := sim.cut hP hxr
      have ht' := Trace.cut x0 P ht hx0r hx0
      have sim'' : Sim S root (unfwd x0 F) (cutF S root st x).1 σ := by rw [hst]; exact sim'
      obtain ⟨F', σ', l, ht2, sim2, tal2⟩ := cuts_fold xs _ _ _ _ ht' sim''
      refine ⟨F', σ', none :: l, by simpa using ht2, sim2, ?_⟩
      rw [hst] at tal2 ⊢
      exact (tal_cut st.2.2).trans tal2
    · have hst : cutF S root st x = (st.1, st.2.1, st.2.2) := if_neg hc
      rw [hst]
      exact cuts_fold xs st F σ l0 ht sim

/-- **The round loop makes a run of the theory.** If `rounds` answers, there is a labelled run of the
theory from the net the engine started from to a net the engine's final state renames, and the tally
grew by exactly that run: one fire per `some i` (counted under rule i) and one removal per `none`. -/
theorem rounds_sim (hT : RunOk S T) (hRR : ∀ r ∈ T, RuleInRange S r) (hroot : ∃ n, root = Ident.root n)
    (budget : Nat) : ∀ (n : Nat) (e : Eng K) (cands : List Ident) (t : Tally) (res : Eng K × Tally),
      Eng.rounds S T root budget n e cands t = some res → ∀ F σ, Sim S root F e σ →
      ∃ F' σ' l, Trace S T (· = root) F l F' ∧ Sim S root F' res.1 σ' ∧ Tal t res.2 l
  | 0, _, _, _, _, h, _, _, _ => by simp [Eng.rounds] at h
  | n + 1, e, cands, t, res, h, F, σ, sim => by
    rw [rounds_succ] at h
    split at h
    · cases h; exact ⟨F, σ, [], .nil, sim, Tal.refl _⟩
    split at h
    · cases h
    have hps : ∀ p ∈ cands.filterMap (Eng.activeAt T e), PairOk T (e, ([] : List Ident), t).1 p := by
      intro p hp
      obtain ⟨x, _, hx⟩ := List.mem_filterMap.mp hp
      exact activeAt_ok sim.roots sim.involutive hx
    obtain ⟨F1, σ1, l1, ht1, sim1, tal1⟩ := fires_fold hT hRR hroot _ (e, [], t) F σ [] .nil sim hps
    obtain ⟨F2, σ2, l2, ht2, sim2, tal2⟩ := cuts_fold (S := S) (T := T)
      ((((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t)).2.1.filter
          (((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t)).1.isFwd root)))
      ((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t)) F1 σ1 _ ht1 sim1
    obtain ⟨F3, σ3, l3, ht3, sim3, tal3⟩ := rounds_sim hT hRR hroot budget n _ _ _ res h F2 σ2 sim2
    refine ⟨F3, σ3, (l1 ++ l2) ++ l3, ht2.append ht3, sim3, ?_⟩
    have tal3' : Tal ((((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t)).2.1.filter
          (((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t)).1.isFwd root)).foldl (cutF S root)
          ((cands.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t))).2.2 res.2 l3 := tal3
    simp only [List.nil_append] at tal1
    exact (tal1.trans tal2).trans tal3'


/-- **Layer 2, packaged.** From an engine state that is a good net with wires on real ports and root
names: if the loop answers `(e', t')`, the theory has a run of exactly `(fires + removals)` moves (as the
tally counts them) from the start net to a net that `e'` renames, labelled so that rule i fires exactly
as often as the tally's count for rule i grew. -/
theorem rounds_run (hT : RunOk S T) (hRR : ∀ r ∈ T, RuleInRange S r) (hroot : ∃ n, root = Ident.root n)
    {budget n : Nat} {e e' : Eng K} {cands : List Ident} {t t' : Tally}
    (hg : e.view.Good S) (hR : InRange S e.view) (hr : e.Roots) (hk : ∃ P, e.view.kind root = some (.fwd P))
    (h : Eng.rounds S T root budget n e cands t = some (e', t')) :
    ∃ F σ l, Trace S T (· = root) e.view l F ∧ Iso σ F e'.view ∧ Tal t t' l ∧
      Path (Move S T (· = root)) ((t'.fires - t.fires) + (t'.cuts - t.cuts)) e.view F := by
  obtain ⟨F, σ, l, ht, sim, tal⟩ := rounds_sim hT hRR hroot budget n e cands t _ h e.view id (Sim.start hg hR hr hk)
  refine ⟨F, σ, l, ht, sim.iso, tal, ?_⟩
  have := ht.path
  rw [length_eq_some_none] at this
  rw [tal.1, tal.2.1]
  simpa using this

end Loop


/-! ## When the loop stops, nothing is left to do -/

section Frames
variable (S : Sig K) (r : GRule K) (kinds : Std.HashMap Ident (Ag K)) (mates : Std.HashMap Port Port)
  (fresh : Nat)

theorem fire_touched_eq (c d : Ident) : (Eng.fire S ⟨kinds, mates, fresh⟩ c d r).2 =
    (fMade S r).map (fun (k, _) => Ident.root (fresh + k)) ++ fNear S kinds mates c d := rfl

theorem cut_touched_eq (x : Ident) : (Eng.cut S ⟨kinds, mates, fresh⟩ x).2 = (cNbrs S kinds mates x).map (·.1) := rfl

/-- An old agent a fire does not report is untouched: same kind, same principal wire. -/
theorem fire_frame {c d : Ident} (roots : ∀ x, kinds[x]? ≠ none → ∃ n, x = Ident.root n ∧ n < fresh)
    (hP : (vw kinds mates).Present) (hi : (vw kinds mates).Involutive) (hR : InRange S (vw kinds mates))
    (hRM : InRange S (fire S r c d (vw kinds mates))) (hc : kinds[c]? ≠ none) (hd : kinds[d]? ≠ none)
    (y : Ident) (hy : (Eng.fire S ⟨kinds, mates, fresh⟩ c d r).1.kinds[y]? ≠ none)
    (hyt : y ∉ (Eng.fire S ⟨kinds, mates, fresh⟩ c d r).2) :
    kinds[y]? = (Eng.fire S ⟨kinds, mates, fresh⟩ c d r).1.kinds[y]? ∧
      (Eng.fire S ⟨kinds, mates, fresh⟩ c d r).1.view.mate (y, 0) = (vw kinds mates).mate (y, 0) := by
  have kget := fire_kinds_get S r kinds mates fresh c d
  rw [fire_touched_eq, List.mem_append, not_or] at hyt
  obtain ⟨hyM, hyN⟩ := hyt
  have hy' := hy
  rw [kget] at hy'
  by_cases hnew : ∃ k, madeKind S r k ≠ none ∧ y = Ident.root (fresh + k)
  · exfalso
    obtain ⟨k, hk, rfl⟩ := hnew
    obtain ⟨g, hg⟩ := Option.ne_none_iff_exists'.mp hk
    exact hyM (List.mem_map.mpr ⟨(k, g), (mem_fMade S r).mpr hg, rfl⟩)
  rw [if_neg hnew] at hy'
  by_cases hcd : y = c ∨ y = d
  · rw [if_pos hcd] at hy'; exact absurd rfl hy'
  rw [if_neg hcd] at hy'
  refine ⟨by rw [kget, if_neg hnew, if_neg hcd], ?_⟩
  obtain ⟨n, rfl, _⟩ := roots _ hy'
  have I2 := fire_view S r kinds mates fresh c d roots hP hi hR hRM hc hd
  have hyF : (fire S r c d (vw kinds mates)).kind (Ident.root n) ≠ none := by
    rw [fire_kind_other S r c d _ hcd rfl]; exact hy'
  have := I2.mate (Ident.root n) 0 hyF
  rw [show sren fresh c d (Ident.root n) = Ident.root n from rfl] at this
  rw [this, fire_mate_other S r c d _ hcd rfl]
  cases hm : (vw kinds mates).mate (Ident.root n, 0) with
  | none => rfl
  | some t =>
    by_cases ht : t.1 = c ∨ t.1 = d
    · exact absurd (near_key S r kinds mates fresh c d
        (near_key_of S r kinds mates fresh c d hi hR (fun e => hcd (Or.inl e)) (fun e => hcd (Or.inr e)) hm ht)) hyN
    · rw [relink_keep r (fun e => ht (Or.inl e)) (fun e => ht (Or.inr e))]
      obtain ⟨m, hm', _⟩ := roots t.1 (hP _ _ hm).2
      obtain ⟨z, j⟩ := t
      simp only at hm'; subst hm'
      rfl

theorem fire_absent {c d : Ident} {n : Nat} (hn : n < fresh) (h : kinds[Ident.root n]? = none) :
    (Eng.fire S ⟨kinds, mates, fresh⟩ c d r).1.kinds[Ident.root n]? = none := by
  rw [fire_kinds_get, if_neg]
  · split
    · rfl
    · exact h
  · rintro ⟨k, _, e⟩; injection e with e; omega

theorem fire_kills {c d : Ident} {n : Nat} (hn : n < fresh) (hc : c = Ident.root n ∨ d = Ident.root n) :
    (Eng.fire S ⟨kinds, mates, fresh⟩ c d r).1.kinds[Ident.root n]? = none := by
  rw [fire_kinds_get, if_neg, if_pos (by rcases hc with h | h <;> simp [h])]
  rintro ⟨k, _, e⟩; injection e with e; omega

theorem nbr_of {x : Ident} (hi : (vw kinds mates).Involutive) (hR : InRange S (vw kinds mates)) {y : Ident} {n : Nat}
    {t : Port} (hyx : y ≠ x) (hm : (vw kinds mates).mate (y, n) = some t) (htx : t.1 = x) :
    (y, n) ∈ cNbrs S kinds mates x := by
  have back := hi _ _ hm
  obtain ⟨g, hg, hn⟩ := hR t.1 t.2 (y, n) back
  simp only [cNbrs, List.mem_filter, List.mem_filterMap, bne_iff_ne, ne_eq]
  refine ⟨⟨t, mem_portsOf.mpr ⟨htx, ?_⟩, back⟩, hyx⟩
  rw [← htx, hg]; simp; omega

theorem cut_frame {x : Ident} (hP : (vw kinds mates).Present) (hi : (vw kinds mates).Involutive)
    (hR : InRange S (vw kinds mates)) (y : Ident) (hy : (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.kinds[y]? ≠ none)
    (hyt : y ∉ (Eng.cut S ⟨kinds, mates, fresh⟩ x).2) :
    kinds[y]? = (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.kinds[y]? ∧
      (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.view.mate (y, 0) = (vw kinds mates).mate (y, 0) := by
  rw [cut_kinds_eq] at hy ⊢
  rw [Std.HashMap.getElem?_erase] at hy ⊢
  have hyx : y ≠ x := by rintro rfl; simp at hy
  have hxy : (x == y) = false := by simp [Ne.symm hyx]
  rw [hxy] at hy ⊢
  refine ⟨rfl, ?_⟩
  have I := cut_view S kinds mates fresh x hP hi hR
  have := I.mate y 0 (by rw [unfwd_kind, if_neg hyx]; exact hy)
  rw [show (id y : Ident) = y from rfl, pmap_id_map, unfwd_mate, if_neg hyx] at this
  rw [this]
  cases hm : (vw kinds mates).mate (y, 0) with
  | none => rfl
  | some t =>
    by_cases htx : t.1 = x
    · exfalso; apply hyt
      rw [cut_touched_eq]
      exact List.mem_map.mpr ⟨(y, 0), nbr_of S kinds mates hi hR hyx hm htx, rfl⟩
    · exact via_keep htx

end Frames

theorem isFwd_cut [DecidableEq K] {S : Sig K} {root z x : Ident} {e : Eng K} (h : e.isFwd root x = false ∨ x = z) :
    (e.cut S z).1.isFwd root x = false := by
  obtain ⟨kinds, mates, fresh⟩ := e
  simp only [Eng.isFwd] at h ⊢
  rw [cut_kinds_eq, Std.HashMap.getElem?_erase]
  rcases h with h | rfl
  · by_cases hzx : z = x
    · subst hzx; simp
    · simp only [beq_iff_eq, hzx, if_false]; exact h
  · simp

section Finish
variable [DecidableEq K] {S : Sig K} {T : List (GRule K)} {root : Ident}

/-- Every active pair with a rule has an agent in the candidate list. -/
def Covered (T : List (GRule K)) (e : Eng K) (C : List Ident) : Prop :=
  ∀ a b (r : GRule K), r ∈ T → e.view.mate (a, 0) = some (b, 0) → e.view.kind a = some (.base r.a) →
    e.view.kind b = some (.base r.b) → a ∈ C ∨ b ∈ C

/-- The only forwarder is the root. -/
def FwdRoot (root : Ident) (e : Eng K) : Prop := ∀ x P, e.view.kind x = some (.fwd P) → x = root

/-- Every agent not reported in `acc` has its kind and principal wire from `e0`. -/
def Frame (e0 e : Eng K) (acc : List Ident) : Prop :=
  ∀ y, e.kinds[y]? ≠ none → y ∉ acc → e0.kinds[y]? = e.kinds[y]? ∧ e.view.mate (y, 0) = e0.view.mate (y, 0)

def Killed (e : Eng K) (p : Ident × Ident × GRule K × Nat) : Prop :=
  e.kinds[p.1]? = none ∨ e.kinds[p.2.1]? = none

def Old (e : Eng K) (p : Ident × Ident × GRule K × Nat) : Prop :=
  (∃ n, p.1 = Ident.root n ∧ n < e.fresh) ∧ (∃ n, p.2.1 = Ident.root n ∧ n < e.fresh)

theorem findRule_some_of {x y : K} : ∀ (T : List (GRule K)) (i0 : Nat) (r : GRule K), r ∈ T →
    ((r.a = x ∧ r.b = y) ∨ (r.a = y ∧ r.b = x)) → Eng.findRule x y T i0 ≠ none
  | [], _, _, h, _ => by cases h
  | s :: T, i0, r, h, hr => by
    simp only [Eng.findRule]
    split
    · simp
    · rename_i hs
      rcases List.mem_cons.mp h with rfl | h
      · exact absurd hr hs
      · exact findRule_some_of T _ r h hr

theorem activeAt_some {e : Eng K} {x y : Ident} {kx ky : K} {r : GRule K} (hkx : e.kinds[x]? = some (.base kx))
    (hm : e.mates[(x, 0)]? = some (y, 0)) (hky : e.kinds[y]? = some (.base ky)) (hr : r ∈ T)
    (hab : (r.a = kx ∧ r.b = ky) ∨ (r.a = ky ∧ r.b = kx)) : Eng.activeAt T e x ≠ none := by
  unfold Eng.activeAt
  rw [hkx, hm]
  simp only
  rw [hky]
  simp only
  cases hf : Eng.findRule kx ky T 0 with
  | none => exact absurd hf (findRule_some_of T 0 r hr hab)
  | some ir => simp

theorem activeAt_pair {e : Eng K} {x : Ident} {p : Ident × Ident × GRule K × Nat}
    (h : Eng.activeAt T e x = some p) :
    ∃ y, e.mates[(x, 0)]? = some (y, 0) ∧ ((p.1 = x ∧ p.2.1 = y) ∨ (p.1 = y ∧ p.2.1 = x)) := by
  unfold Eng.activeAt at h
  split at h
  · next kx y hkx hmx =>
    split at h
    · next ky hky =>
      cases hf : Eng.findRule kx ky T 0 with
      | none => rw [hf] at h; cases h
      | some ir =>
        obtain ⟨i, r⟩ := ir
        rw [hf] at h
        simp only [Option.map_some, Option.some.injEq] at h
        refine ⟨y, hmx, ?_⟩
        split at h
        · subst h; exact Or.inl ⟨rfl, rfl⟩
        · subst h; exact Or.inr ⟨rfl, rfl⟩
    · cases h
  · cases h

theorem idle_of_stop {e : Eng K} {C : List Ident} (hi : e.view.Involutive) (hcov : Covered T e C)
    (hfw : FwdRoot root e) (hstop : (C.filterMap (Eng.activeAt T e)).isEmpty = true) :
    Idle S T (· = root) e.view := by
  have none_of : ∀ x ∈ C, Eng.activeAt T e x = none := by
    have := List.isEmpty_iff.mp hstop
    exact List.filterMap_eq_nil_iff.mp this
  refine ⟨fun a b r hr hab hka hkb => ?_, fun x P hx hk => hx (hfw x P hk)⟩
  rcases hcov a b r hr hab hka hkb with h | h
  · exact activeAt_some hka hab hkb hr (Or.inl ⟨rfl, rfl⟩) (none_of a h)
  · exact activeAt_some hkb (hi _ _ hab) hka hr (Or.inr ⟨rfl, rfl⟩) (none_of b h)

/-- The fire half of a round: pairs fired are killed, unreported agents untouched. -/
theorem fires_frame (hT : RunOk S T) (hRR : ∀ r ∈ T, RuleInRange S r) (hroot : ∃ n, root = Ident.root n)
    {e0 : Eng K} : ∀ (ps : List (Ident × Ident × GRule K × Nat)) (st : Eng K × List Ident × Tally)
      (F : Net K) (σ : Ident → Ident) (D : List (Ident × Ident × GRule K × Nat)),
      Sim S root F st.1 σ → (∀ p ∈ ps, PairOk T st.1 p) → (∀ p ∈ D, Old st.1 p ∧ Killed st.1 p) →
      Frame e0 st.1 st.2.1 →
      ∃ F' σ', Sim S root F' (ps.foldl (fireF S) st).1 σ' ∧ Frame e0 (ps.foldl (fireF S) st).1 (ps.foldl (fireF S) st).2.1 ∧
        ∀ p ∈ D ++ ps, Old (ps.foldl (fireF S) st).1 p ∧ Killed (ps.foldl (fireF S) st).1 p
  | [], st, F, σ, D, sim, _, hD, hfr => ⟨F, σ, sim, hfr, by simpa using hD⟩
  | p :: ps, st, F, σ, D, sim, hps, hD, hfr => by
    rw [List.foldl_cons]
    obtain ⟨a, b, r, i⟩ := p
    have hp := hps _ List.mem_cons_self
    have hps' := fun q (hq : q ∈ ps) => hps q (List.mem_cons_of_mem _ hq)
    obtain ⟨st1, acc, t⟩ := st
    obtain ⟨kinds, mates, fresh⟩ := st1
    by_cases hc : ((kinds[a]?).isSome && (kinds[b]?).isSome) = true
    · have hst : fireF S (⟨kinds, mates, fresh⟩, acc, t) (a, b, r, i) =
          ((Eng.fire S ⟨kinds, mates, fresh⟩ a b r).1, (Eng.fire S ⟨kinds, mates, fresh⟩ a b r).2 ++ acc,
          { fires := t.fires + 1, cuts := t.cuts, rounds := t.rounds,
            perRule := t.perRule.modify i fun x => x + 1 }) := if_pos hc
      rw [hst]
      simp only [Bool.and_eq_true, Option.isSome_iff_ne_none] at hc
      obtain ⟨hTi, ⟨na, rfl, hna⟩, ⟨nb, rfl, hnb⟩, hact⟩ := hp
      change na < fresh at hna; change nb < fresh at hnb
      obtain ⟨hab, hka, hkb⟩ := hact hc.1 hc.2
      have hr : r ∈ T := List.mem_of_getElem? hTi
      obtain ⟨_, _, _, _, _, sim'⟩ := sim.fire hT hRR hroot hr hab hka hkb
      have hf : (Eng.fire S ⟨kinds, mates, fresh⟩ (Ident.root na) (Ident.root nb) r).1.fresh =
          fresh + r.made.length + r.wires.length := rfl
      have hOld : ∀ n, n < fresh →
          n < (Eng.fire S ⟨kinds, mates, fresh⟩ (Ident.root na) (Ident.root nb) r).1.fresh := by
        intro n hn; rw [hf]; omega
      have hD2 : ∀ q ∈ D ++ [(Ident.root na, Ident.root nb, r, i)],
          Old (Eng.fire S ⟨kinds, mates, fresh⟩ (Ident.root na) (Ident.root nb) r).1 q ∧
            Killed (Eng.fire S ⟨kinds, mates, fresh⟩ (Ident.root na) (Ident.root nb) r).1 q := by
        intro q hq
        rcases List.mem_append.mp hq with hq | hq
        · obtain ⟨⟨⟨n1, h1, l1⟩, ⟨n2, h2, l2⟩⟩, hk⟩ := hD q hq
          change n1 < fresh at l1; change n2 < fresh at l2
          refine ⟨⟨⟨n1, h1, hOld n1 l1⟩, ⟨n2, h2, hOld n2 l2⟩⟩, ?_⟩
          rcases hk with hk | hk
          · left
            change kinds[q.1]? = none at hk
            show (Eng.fire S ⟨kinds, mates, fresh⟩ (Ident.root na) (Ident.root nb) r).1.kinds[q.1]? = none
            rw [h1] at hk ⊢; exact fire_absent S r kinds mates fresh l1 hk
          · right
            change kinds[q.2.1]? = none at hk
            show (Eng.fire S ⟨kinds, mates, fresh⟩ (Ident.root na) (Ident.root nb) r).1.kinds[q.2.1]? = none
            rw [h2] at hk ⊢; exact fire_absent S r kinds mates fresh l2 hk
        · simp only [List.mem_singleton] at hq; subst hq
          exact ⟨⟨⟨na, rfl, hOld na hna⟩, ⟨nb, rfl, hOld nb hnb⟩⟩,
            Or.inl (fire_kills S r kinds mates fresh hna (Or.inl rfl))⟩
      have hfr2 : Frame e0 (Eng.fire S ⟨kinds, mates, fresh⟩ (Ident.root na) (Ident.root nb) r).1
          ((Eng.fire S ⟨kinds, mates, fresh⟩ (Ident.root na) (Ident.root nb) r).2 ++ acc) := by
        intro y hy hyt
        simp only [List.mem_append, not_or] at hyt
        obtain ⟨h1, h2⟩ := fire_frame S r kinds mates fresh sim.roots sim.present sim.involutive sim.inRange
          (fire_inRange sim.inRange (hRR r hr)) hc.1 hc.2 y hy hyt.1
        obtain ⟨h3, h4⟩ := hfr y (by change kinds[y]? ≠ none; rw [h1]; exact hy) hyt.2
        exact ⟨h3.trans h1, h2.trans h4⟩
      obtain ⟨F', σ', s2, f2, k2⟩ := fires_frame hT hRR hroot ps
        ((Eng.fire S ⟨kinds, mates, fresh⟩ (Ident.root na) (Ident.root nb) r).1,
          (Eng.fire S ⟨kinds, mates, fresh⟩ (Ident.root na) (Ident.root nb) r).2 ++ acc,
          { fires := t.fires + 1, cuts := t.cuts, rounds := t.rounds,
            perRule := t.perRule.modify i fun x => x + 1 }) _ _ (D ++ [(Ident.root na, Ident.root nb, r, i)]) sim'
        (fun q hq => pairOk_fire hRR sim hr hc.1 hc.2 (hps' q hq)) hD2 hfr2
      exact ⟨F', σ', s2, f2, fun q hq => k2 q (by simpa using hq)⟩
    · have hst : fireF S (⟨kinds, mates, fresh⟩, acc, t) (a, b, r, i) = (⟨kinds, mates, fresh⟩, acc, t) := if_neg hc
      rw [hst]
      obtain ⟨_, hO1, hO2, _⟩ := hp
      have hk : Killed ⟨kinds, mates, fresh⟩ (a, b, r, i) := by
        show kinds[a]? = none ∨ kinds[b]? = none
        cases ha : kinds[a]? with
        | none => exact Or.inl rfl
        | some _ =>
          cases hb : kinds[b]? with
          | none => exact Or.inr rfl
          | some _ => simp [ha, hb] at hc
      have hD2 : ∀ q ∈ D ++ [(a, b, r, i)], Old (Eng.mk kinds mates fresh) q ∧ Killed (Eng.mk kinds mates fresh) q := by
        intro q hq
        rcases List.mem_append.mp hq with hq | hq
        · exact hD q hq
        · simp only [List.mem_singleton] at hq; subst hq; exact ⟨⟨hO1, hO2⟩, hk⟩
      obtain ⟨F', σ', s2, f2, k2⟩ := fires_frame hT hRR hroot ps (⟨kinds, mates, fresh⟩, acc, t) F σ
        (D ++ [(a, b, r, i)]) sim hps' hD2 hfr
      exact ⟨F', σ', s2, f2, fun q hq => k2 q (by simpa using hq)⟩

/-- The forwarder half of a round: kinds only disappear, forwarders it was given are gone, unreported
agents untouched. -/
theorem cuts_frame {e0 : Eng K} : ∀ (xs : List Ident) (st : Eng K × List Ident × Tally)
      (F : Net K) (σ : Ident → Ident) (D : List (Ident × Ident × GRule K × Nat)) (Dx : List Ident),
      Sim S root F st.1 σ → (∀ p ∈ D, Old st.1 p ∧ Killed st.1 p) → (∀ x ∈ Dx, st.1.isFwd root x = false) →
      Frame e0 st.1 st.2.1 →
      ∃ F' σ', Sim S root F' (xs.foldl (cutF S root) st).1 σ' ∧
        Frame e0 (xs.foldl (cutF S root) st).1 (xs.foldl (cutF S root) st).2.1 ∧
        (∀ p ∈ D, Old (xs.foldl (cutF S root) st).1 p ∧ Killed (xs.foldl (cutF S root) st).1 p) ∧
        (∀ x ∈ Dx ++ xs, (xs.foldl (cutF S root) st).1.isFwd root x = false) ∧
        (∀ y : Ident, (xs.foldl (cutF S root) st).1.kinds[y]? ≠ none →
          (xs.foldl (cutF S root) st).1.kinds[y]? = st.1.kinds[y]?)
  | [], st, F, σ, D, Dx, sim, hD, hDx, hfr => ⟨F, σ, sim, hfr, hD, by simpa using hDx, fun _ _ => rfl⟩
  | x :: xs, st, F, σ, D, Dx, sim, hD, hDx, hfr => by
    rw [List.foldl_cons]
    obtain ⟨st1, acc, t⟩ := st
    obtain ⟨kinds, mates, fresh⟩ := st1
    have kc : ∀ y, (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.kinds[y]? = if y = x then none else kinds[y]? := by
      intro y; rw [cut_kinds_eq, Std.HashMap.getElem?_erase]
      by_cases h : y = x
      · subst h; simp
      · simp [h, Ne.symm h]
    by_cases hc : (Eng.isFwd ⟨kinds, mates, fresh⟩ root x) = true
    · have hst : cutF S root (⟨kinds, mates, fresh⟩, acc, t) x =
          ((Eng.cut S ⟨kinds, mates, fresh⟩ x).1, (Eng.cut S ⟨kinds, mates, fresh⟩ x).2 ++ acc,
          { fires := t.fires, cuts := t.cuts + 1, rounds := t.rounds, perRule := t.perRule }) := if_pos hc
      rw [hst]
      have hc' := hc
      simp only [Eng.isFwd, Bool.and_eq_true, bne_iff_ne, ne_eq] at hc'
      obtain ⟨hxr, hk⟩ := hc'
      have : ∃ P, (Eng.view ⟨kinds, mates, fresh⟩).kind x = some (.fwd P) := by
        show ∃ P, kinds[x]? = some (.fwd P)
        revert hk; cases kinds[x]? with
        | none => intro h; cases h
        | some g => cases g with
          | base _ => intro h; cases h
          | fwd P => intro _; exact ⟨P, rfl⟩
      obtain ⟨P, hP⟩ := this
      obtain ⟨_, _, _, sim'⟩ := sim.cut hP hxr
      have hD2 : ∀ q ∈ D, Old (Eng.cut S ⟨kinds, mates, fresh⟩ x).1 q ∧ Killed (Eng.cut S ⟨kinds, mates, fresh⟩ x).1 q := by
        intro q hq
        obtain ⟨hO, hk⟩ := hD q hq
        refine ⟨hO, ?_⟩
        rcases hk with hk | hk
        · left; show (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.kinds[q.1]? = none; rw [kc]; split
          · rfl
          · exact hk
        · right; show (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.kinds[q.2.1]? = none; rw [kc]; split
          · rfl
          · exact hk
      have hDx2 : ∀ z ∈ Dx ++ [x], (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.isFwd root z = false := by
        intro z hz
        rcases List.mem_append.mp hz with hz | hz
        · exact isFwd_cut (Or.inl (hDx z hz))
        · simp only [List.mem_singleton] at hz; exact isFwd_cut (Or.inr hz)
      have hfr2 : Frame e0 (Eng.cut S ⟨kinds, mates, fresh⟩ x).1 ((Eng.cut S ⟨kinds, mates, fresh⟩ x).2 ++ acc) := by
        intro y hy hyt
        simp only [List.mem_append, not_or] at hyt
        obtain ⟨h1, h2⟩ := cut_frame S kinds mates fresh sim.present sim.involutive sim.inRange y hy hyt.1
        obtain ⟨h3, h4⟩ := hfr y (by change kinds[y]? ≠ none; rw [h1]; exact hy) hyt.2
        exact ⟨h3.trans h1, h2.trans h4⟩
      obtain ⟨F', σ', s2, f2, k2, x2, sh2⟩ := cuts_frame xs ((Eng.cut S ⟨kinds, mates, fresh⟩ x).1,
          (Eng.cut S ⟨kinds, mates, fresh⟩ x).2 ++ acc,
          { fires := t.fires, cuts := t.cuts + 1, rounds := t.rounds, perRule := t.perRule }) _ _ D (Dx ++ [x])
        sim' hD2 hDx2 hfr2
      refine ⟨F', σ', s2, f2, k2, fun z hz => x2 z (by simpa using hz), fun y hy => ?_⟩
      rw [sh2 y hy]
      have hy' := sh2 y hy ▸ hy
      change (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.kinds[y]? ≠ none at hy'
      change (Eng.cut S ⟨kinds, mates, fresh⟩ x).1.kinds[y]? = kinds[y]?
      rw [kc] at hy' ⊢
      by_cases hyx : y = x
      · rw [if_pos hyx] at hy'; exact absurd rfl hy'
      · rw [if_neg hyx]
    · have hst : cutF S root (⟨kinds, mates, fresh⟩, acc, t) x = (⟨kinds, mates, fresh⟩, acc, t) := if_neg hc
      rw [hst]
      have hDx2 : ∀ z ∈ Dx ++ [x], (Eng.mk kinds mates fresh).isFwd root z = false := by
        intro z hz
        rcases List.mem_append.mp hz with hz | hz
        · exact hDx z hz
        · simp only [List.mem_singleton] at hz; subst hz; simpa using hc
      obtain ⟨F', σ', s2, f2, k2, x2, sh2⟩ := cuts_frame xs (⟨kinds, mates, fresh⟩, acc, t) F σ D (Dx ++ [x])
        sim hD hDx2 hfr
      exact ⟨F', σ', s2, f2, k2, fun z hz => x2 z (by simpa using hz), sh2⟩


/-- The state after a round's fires. -/
def roundA (S : Sig K) (T : List (GRule K)) (e : Eng K) (C : List Ident) (t : Tally) : Eng K × List Ident × Tally :=
  (C.filterMap (Eng.activeAt T e)).foldl (fireF S) (e, [], t)

/-- The state after a round's fires and forwarder removals. -/
def roundB (S : Sig K) (T : List (GRule K)) (root : Ident) (e : Eng K) (C : List Ident) (t : Tally) :
    Eng K × List Ident × Tally :=
  ((roundA S T e C t).2.1.filter ((roundA S T e C t).1.isFwd root)).foldl (cutF S root) (roundA S T e C t)

theorem rounds_succ' (budget n : Nat) (e : Eng K) (cands : List Ident) (t : Tally) :
    Eng.rounds S T root budget (n + 1) e cands t =
      if (cands.filterMap (Eng.activeAt T e)).isEmpty then some (e, t) else
      if (roundB S T root e cands t).2.2.fires + (roundB S T root e cands t).2.2.cuts > budget then none
      else Eng.rounds S T root budget n (roundB S T root e cands t).1 (roundB S T root e cands t).2.1
        { (roundB S T root e cands t).2.2 with rounds := (roundB S T root e cands t).2.2.rounds + 1 } := by
  rw [rounds_succ]; rfl

/-- **A round keeps the work list complete**: after it, every active pair with a rule has an agent
among those the round reported, and the only forwarder is still the root. -/
theorem round_cov (hT : RunOk S T) (hRR : ∀ r ∈ T, RuleInRange S r) (hroot : ∃ n, root = Ident.root n)
    {F : Net K} {σ : Ident → Ident} {e : Eng K} {C : List Ident} {t : Tally} (sim : Sim S root F e σ)
    (hcov : Covered T e C) (hfw : FwdRoot root e) :
    (∃ F' σ', Sim S root F' (roundB S T root e C t).1 σ') ∧
      Covered T (roundB S T root e C t).1 (roundB S T root e C t).2.1 ∧ FwdRoot root (roundB S T root e C t).1 := by
  have hps : ∀ p ∈ C.filterMap (Eng.activeAt T e), PairOk T (e, ([] : List Ident), t).1 p := by
    intro p hp
    obtain ⟨x, _, hx⟩ := List.mem_filterMap.mp hp
    exact activeAt_ok sim.roots sim.involutive hx
  have fr0 : Frame e (e, ([] : List Ident), t).1 (e, ([] : List Ident), t).2.1 := fun _ _ _ => ⟨rfl, rfl⟩
  obtain ⟨F1, σ1, s1, f1, k1⟩ := fires_frame hT hRR hroot (e0 := e) _ (e, [], t) F σ [] sim hps (by simp) fr0
  obtain ⟨F2, σ2, s2, f2, k2, x2, sh2⟩ := cuts_frame (S := S) (e0 := e)
    ((roundA S T e C t).2.1.filter ((roundA S T e C t).1.isFwd root)) (roundA S T e C t) F1 σ1
    (C.filterMap (Eng.activeAt T e)) [] s1 (fun p hp => k1 p (by simpa using hp)) (by simp) f1
  refine ⟨⟨F2, σ2, s2⟩, ?_, ?_⟩
  · intro a b r hr hab hka hkb
    by_cases ha : a ∈ (roundB S T root e C t).2.1
    · exact Or.inl ha
    by_cases hb : b ∈ (roundB S T root e C t).2.1
    · exact Or.inr hb
    exfalso
    have pa : (roundB S T root e C t).1.kinds[a]? ≠ none := by
      change (roundB S T root e C t).1.view.kind a ≠ none; rw [hka]; simp
    have pb : (roundB S T root e C t).1.kinds[b]? ≠ none := by
      change (roundB S T root e C t).1.view.kind b ≠ none; rw [hkb]; simp
    obtain ⟨ka, ma⟩ := f2 a pa ha
    obtain ⟨kb, _⟩ := f2 b pb hb
    have hab0 : e.mates[(a, 0)]? = some (b, 0) := ma.symm.trans hab
    have hka0 : e.kinds[a]? = some (.base r.a) := ka.trans hka
    have hkb0 : e.kinds[b]? = some (.base r.b) := kb.trans hkb
    have kill : ∀ p ∈ C.filterMap (Eng.activeAt T e), ∀ x y, e.mates[(x, 0)]? = some (y, 0) →
        Eng.activeAt T e x = some p → (x = a ∨ x = b) → (y = a ∨ y = b) → False := by
      intro p hp x y hxy hpx hx hy
      obtain ⟨y', hmy', hpq⟩ := activeAt_pair hpx
      rw [hxy] at hmy'; injection hmy' with hmy'; injection hmy' with hmy'; subst hmy'
      have := (k2 p hp).2
      have hn : ∀ z, (z = a ∨ z = b) → (roundB S T root e C t).1.kinds[z]? ≠ none := by
        rintro z (rfl | rfl)
        · exact pa
        · exact pb
      rcases this with h | h <;> rcases hpq with ⟨h1, h2⟩ | ⟨h1, h2⟩
      · exact hn _ (h1 ▸ hx) h
      · exact hn _ (h1 ▸ hy) h
      · exact hn _ (h2 ▸ hy) h
      · exact hn _ (h2 ▸ hx) h
    rcases hcov a b r hr hab0 hka0 hkb0 with hC | hC
    · have hact := activeAt_some (T := T) hka0 hab0 hkb0 hr (Or.inl ⟨rfl, rfl⟩)
      obtain ⟨p, hp⟩ := Option.ne_none_iff_exists'.mp hact
      exact kill p (List.mem_filterMap.mpr ⟨a, hC, hp⟩) a b hab0 hp (Or.inl rfl) (Or.inr rfl)
    · have hba0 : e.mates[(b, 0)]? = some (a, 0) := sim.involutive _ _ hab0
      have hact := activeAt_some (T := T) hkb0 hba0 hka0 hr (Or.inr ⟨rfl, rfl⟩)
      obtain ⟨p, hp⟩ := Option.ne_none_iff_exists'.mp hact
      exact kill p (List.mem_filterMap.mpr ⟨b, hC, hp⟩) b a hba0 hp (Or.inr rfl) (Or.inl rfl)
  · intro x P hx
    by_cases hxr : x = root
    · exact hxr
    exfalso
    have hxB : (roundB S T root e C t).1.kinds[x]? = some (.fwd P) := hx
    have hxA : (roundA S T e C t).1.kinds[x]? = some (.fwd P) :=
      (sh2 x (by change (roundB S T root e C t).1.kinds[x]? ≠ none; rw [hxB]; simp)).symm.trans hxB
    have fwdB : (roundB S T root e C t).1.isFwd root x = true := by
      simp only [Eng.isFwd, hxB, Bool.and_true, bne_iff_ne, ne_eq]; exact hxr
    by_cases hxt : x ∈ (roundA S T e C t).2.1
    · have hmem : x ∈ (roundA S T e C t).2.1.filter ((roundA S T e C t).1.isFwd root) := by
        refine List.mem_filter.mpr ⟨hxt, ?_⟩
        simp only [Eng.isFwd, hxA, Bool.and_true, bne_iff_ne, ne_eq]; exact hxr
      have := x2 x (by simpa using hmem)
      change (roundB S T root e C t).1.isFwd root x = false at this
      rw [fwdB] at this; cases this
    · obtain ⟨h1, _⟩ := f1 x (by change (roundA S T e C t).1.kinds[x]? ≠ none; rw [hxA]; simp) hxt
      exact hxr (hfw x P (h1.trans hxA))

/-- When the loop answers, nothing is left to do in the engine's state. -/
theorem rounds_idle (hT : RunOk S T) (hRR : ∀ r ∈ T, RuleInRange S r) (hroot : ∃ n, root = Ident.root n)
    (budget : Nat) : ∀ (n : Nat) (e : Eng K) (cands : List Ident) (t : Tally) (res : Eng K × Tally),
      Eng.rounds S T root budget n e cands t = some res → ∀ F σ, Sim S root F e σ →
      Covered T e cands → FwdRoot root e → Idle S T (· = root) res.1.view
  | 0, _, _, _, _, h, _, _, _, _, _ => by simp [Eng.rounds] at h
  | n + 1, e, cands, t, res, h, F, σ, sim, hcov, hfw => by
    rw [rounds_succ'] at h
    split at h
    · cases h; exact idle_of_stop sim.involutive hcov hfw ‹_›
    split at h
    · cases h
    obtain ⟨⟨F', σ', sim'⟩, cov', fw'⟩ := round_cov (t := t) hT hRR hroot sim hcov hfw
    exact rounds_idle hT hRR hroot budget n _ _ _ res h F' σ' sim' cov' fw'

/-- **Layer 3: the loop's answer is a finished run.** From a good start where every active pair has an
agent in the candidate list and the only forwarder is the root: if the loop answers `(e', t')`, the
theory has a labelled run, counted by the tally, from the start net to a net `F` that `e'` renames, and
`F` is `Finished` (no pair to fire, no forwarder but the root). -/
theorem rounds_finished (hT : RunOk S T) (hRR : ∀ r ∈ T, RuleInRange S r) (hroot : ∃ n, root = Ident.root n)
    {budget n : Nat} {e e' : Eng K} {cands : List Ident} {t t' : Tally}
    (hg : e.view.Good S) (hR : InRange S e.view) (hr : e.Roots) (hk : ∃ P, e.view.kind root = some (.fwd P))
    (hcov : Covered T e cands) (hfw : FwdRoot root e)
    (h : Eng.rounds S T root budget n e cands t = some (e', t')) :
    ∃ F σ l, Trace S T (· = root) e.view l F ∧ Iso σ F e'.view ∧ σ root = root ∧ Tal t t' l ∧
      Path (Move S T (· = root)) ((t'.fires - t.fires) + (t'.cuts - t.cuts)) e.view F ∧
      Finished S T (· = root) F ∧ F.Good S ∧ (∃ P, F.kind root = some (.fwd P)) := by
  obtain ⟨F, σ, l, ht, sim, tal⟩ := rounds_sim hT hRR hroot budget n e cands t _ h e.view id (Sim.start hg hR hr hk)
  have idle := rounds_idle hT hRR hroot budget n e cands t _ h e.view id (Sim.start hg hR hr hk) hcov hfw
  refine ⟨F, σ, l, ht, sim.iso, sim.rootFix, tal, ?_, Idle.finished (sim.iso.idle ?_ idle), sim.good, sim.rootIn⟩
  · have := ht.path
    rw [length_eq_some_none] at this
    rw [tal.1, tal.2.1]
    simpa using this
  · intro x hx hσ
    obtain ⟨P, hP⟩ := sim.rootIn
    exact sim.iso.inj x root hx (by rw [hP]; simp) (hσ.trans sim.rootFix.symm)

end Finish



/-! ## Complete rules wire only real ports -/

theorem nodup_range_map {β : Type} (f : Nat → β) (hf : ∀ a b, f a = f b → a = b) (n : Nat) :
    ((List.range n).map f).Nodup :=
  List.pairwise_map.mpr (List.nodup_range.imp fun h e => h (hf _ _ e))

theorem expected_nodup (S : Sig K) (r : GRule K) : (expected S r).Nodup := by
  unfold expected
  rw [List.nodup_append, List.nodup_append]
  refine ⟨⟨nodup_range_map _ (fun a b e => by injection e; omega) _,
    nodup_range_map _ (fun a b e => by injection e; omega) _, ?_⟩, ?_, ?_⟩
  · intro x hx y hy e; subst e
    simp only [List.mem_map] at hx hy
    obtain ⟨_, _, rfl⟩ := hx; obtain ⟨_, _, h⟩ := hy; cases h
  · rw [List.Nodup, List.pairwise_flatMap]
    refine ⟨fun j _ => nodup_range_map _ (fun a b e => by injection e) _, ?_⟩
    refine List.nodup_range.imp fun hne x hx y hy e => ?_
    subst e
    simp only [List.mem_map] at hx hy
    obtain ⟨_, _, rfl⟩ := hx; obtain ⟨_, _, h⟩ := hy
    injection h with h; exact hne h.symm
  · intro x hx y hy e; subst e
    simp only [List.mem_append, List.mem_map, List.mem_flatMap] at hx hy
    obtain ⟨_, _, ⟨_, _, h⟩⟩ := hy
    rcases hx with ⟨_, _, rfl⟩ | ⟨_, _, rfl⟩ <;> cases h

/-- A complete rule wires only ends it must wire. -/
theorem ends_sub_expected {S : Sig K} {r : GRule K} (h : Complete S r) : ∀ e ∈ ends r.wires, e ∈ expected S r := by
  intro x hx
  apply Classical.byContradiction
  intro hne
  have hsub : expected S r ⊆ (ends r.wires).filter (fun e => decide (e ∈ expected S r)) := by
    intro e he
    refine List.mem_filter.mpr ⟨List.count_pos_iff.mp ?_, by simp [he]⟩
    rw [h.2 e he]; omega
  have h1 := (expected_nodup S r).length_le_of_subset hsub
  have h2 := (List.length_filter_lt_length_iff_exists (p := fun e => decide (e ∈ expected S r))).mpr
    ⟨x, hx, by simp [hne]⟩
  rw [h.1] at h2
  omega

theorem ruleInRange_of_complete {S : Sig K} {r : GRule K} (h : Complete S r) : RuleInRange S r := by
  intro w hw j n hj
  have hmem : End.new j n ∈ ends r.wires := by
    have := mem_ends_of_mem (x := w.1) (y := w.2) hw
    rcases hj with e | e
    · rw [← e]; exact this.1
    · rw [← e]; exact this.2
  have := ends_sub_expected h _ hmem
  unfold expected at this
  simp only [List.mem_append, List.mem_map, List.mem_flatMap, List.mem_range] at this
  rcases this with (⟨_, _, e⟩ | ⟨_, _, e⟩) | ⟨j', hj', n', hn', e⟩
  · cases e
  · cases e
  · injection e with e1 e2; subst e1; subst e2
    refine ⟨r.made[j'], by simp [hj'], ?_⟩
    have : r.made[j']? = some r.made[j'] := by simp [hj']
    rw [this] at hn'; simp at hn'; omega

theorem Ok.ruleInRange {sys : System K} (h : Ok sys) : ∀ r ∈ sys.rules, RuleInRange sys.sig r :=
  fun r hr => ruleInRange_of_complete (h.complete r hr)


/-! ## Reading the answer back does not see the renaming -/

section Readback
variable [DecidableEq K] {σ : Ident → Ident} {F M : Net K} (g : Gadget K)

theorem walk_iso (I : Iso σ F M) (hP : F.Present) : ∀ (n : Nat) (s : Port), F.kind s.1 ≠ none →
    walk g n M (pmap σ s) = (walk g n F s).map (pmap σ) ∧ ∀ q, walk g n F s = some q → F.kind q.1 ≠ none
  | 0, s, _ => ⟨rfl, fun q h => by simp [walk] at h⟩
  | n + 1, s, hs => by
    have hk := I.kind s.1 hs
    have hrec : ∀ i, (M.mate (σ s.1, i)).bind (walk g n M) = ((F.mate (s.1, i)).bind (walk g n F)).map (pmap σ) ∧
        ∀ q, (F.mate (s.1, i)).bind (walk g n F) = some q → F.kind q.1 ≠ none := by
      intro i
      rw [I.mate s.1 i hs]
      cases ht : F.mate (s.1, i) with
      | none => exact ⟨rfl, fun q h => by cases h⟩
      | some t => exact walk_iso I hP n t (hP _ _ ht).2
    simp only [walk, pmap]
    rw [hk]
    cases hks : F.kind s.1 with
    | none => exact absurd hks hs
    | some ag =>
      cases ag with
      | base k =>
        simp only
        by_cases hc : k = g.share ∧ (s.2 = 1 ∨ s.2 = 2)
        · simp only [hc, and_self, if_true]; exact hrec 0
        · simp only [hc, if_false]
          exact ⟨rfl, fun q h => by cases h; exact hs⟩
      | fwd P =>
        simp only
        exact hrec _

theorem findIdx_map {f : Port → Port} (x : Port) : ∀ (bs : List Port), (∀ b ∈ bs, f b = f x → b = x) →
    (bs.map f).findIdx? (· == f x) = bs.findIdx? (· == x)
  | [], _ => rfl
  | b :: bs, hinj => by
    rw [List.map_cons, List.findIdx?_cons, List.findIdx?_cons,
      findIdx_map x bs (fun b' hb' => hinj b' (List.mem_cons_of_mem _ hb'))]
    by_cases h : b = x
    · subst h; simp
    · have : f b ≠ f x := fun e => h (hinj b List.mem_cons_self e)
      simp [h, this]

/-- `readback` on a renamed net, from a renamed port with renamed binders, reads the same term. -/
theorem readback_iso (I : Iso σ F M) (hP : F.Present) : ∀ (n : Nat) (bs : List Port) (p : Port),
    F.kind p.1 ≠ none → (∀ b ∈ bs, F.kind b.1 ≠ none) →
    readback g n M (bs.map (pmap σ)) (pmap σ p) = readback g n F bs p
  | 0, _, _, _, _ => rfl
  | n + 1, bs, p, hp, hbs => by
    have hk := I.kind p.1 hp
    have hrec : ∀ i (bs' : List Port), (∀ b ∈ bs', F.kind b.1 ≠ none) →
        (M.mate (σ p.1, i)).bind (readback g n M (bs'.map (pmap σ))) = (F.mate (p.1, i)).bind (readback g n F bs') := by
      intro i bs' hbs'
      rw [I.mate p.1 i hp]
      cases ht : F.mate (p.1, i) with
      | none => rfl
      | some t => exact readback_iso I hP n bs' t (hP _ _ ht).2 hbs'
    have inj : ∀ a b : Port, F.kind a.1 ≠ none → F.kind b.1 ≠ none → pmap σ a = pmap σ b → a = b := by
      rintro ⟨x, i⟩ ⟨y, j⟩ ha hb e
      simp only [pmap, Prod.mk.injEq] at e
      rw [I.inj x y ha hb e.1, e.2]
    simp only [readback]
    rw [show (pmap σ p).1 = σ p.1 from rfl, hk]
    cases hks : F.kind p.1 with
    | none => exact absurd hks hp
    | some ag =>
      cases ag with
      | fwd P => exact hrec _ bs hbs
      | base k =>
        simp only [show (pmap σ p).2 = p.2 from rfl]
        split
        · rfl
        split
        · have := hrec 2 ((p.1, 1) :: bs) (by
            intro b hb
            rcases List.mem_cons.mp hb with rfl | hb
            · exact hp
            · exact hbs b hb)
          simp only [List.map_cons] at this
          exact congrArg (Option.map Src.lam) this
        split
        · rw [hrec 1 bs hbs, hrec 2 bs hbs]
        split
        · rw [I.mate p.1 1 hp]
          cases ht : F.mate (p.1, 1) with
          | none => rfl
          | some t =>
            simp only [Option.map_some, Option.bind_some]
            obtain ⟨hw, hwp⟩ := walk_iso g I hP n t (hP _ _ ht).2
            rw [hw]
            cases hws : walk g n F t with
            | none => rfl
            | some src =>
              have hsrc := hwp src hws
              change (match List.findIdx? (· == pmap σ src) (bs.map (pmap σ)) with
                  | some i => pure (Src.var i)
                  | none => if (pmap σ src).2 = 0 then readback g n M [] (pmap σ src) else none) =
                (match List.findIdx? (· == src) bs with
                  | some i => pure (Src.var i)
                  | none => if src.2 = 0 then readback g n F [] src else none)
              rw [findIdx_map src bs (fun b hb e => inj b src (hbs b hb) hsrc e)]
              cases bs.findIdx? (· == src) with
              | some i => rfl
              | none =>
                change (if src.2 = 0 then readback g n M [] (pmap σ src) else none) =
                  (if src.2 = 0 then readback g n F [] src else none)
                have := readback_iso I hP n [] src hsrc (by simp)
                simp only [List.map_nil] at this
                rw [this]
        · rfl

end Readback

/-! ## The start state, and a whole job -/

section Start
variable [DecidableEq K] {S : Sig K}

open Classical in
theorem wires_fold (p q : Port) : ∀ (l : List (Port × Port)) (m : Std.HashMap Port Port),
    (∀ w ∈ l, (w.1 = p → w.2 = q) ∧ (w.2 = p → w.1 = q)) →
    (l.foldl (fun m (w : Port × Port) => match w with | (p, q) => (m.insert p q).insert q p) m)[p]? =
      if ∃ w ∈ l, w.1 = p ∨ w.2 = p then some q else m[p]?
  | [], m, _ => by simp
  | w :: l, m, hc => by
    rw [List.foldl_cons, wires_fold p q l _ (fun w hw => hc w (List.mem_cons_of_mem _ hw))]
    obtain ⟨w1, w2⟩ := w
    have h0 := hc _ List.mem_cons_self
    by_cases hl : ∃ w ∈ l, w.1 = p ∨ w.2 = p
    · rw [if_pos hl, if_pos (let ⟨w, hw, h⟩ := hl; ⟨w, List.mem_cons_of_mem _ hw, h⟩)]
    rw [if_neg hl]
    simp only [Std.HashMap.getElem?_insert, beq_iff_eq]
    by_cases h2 : w2 = p
    · rw [if_pos h2, if_pos ⟨(w1, w2), List.mem_cons_self, Or.inr h2⟩]; exact congrArg some (h0.2 h2)
    rw [if_neg h2]
    by_cases h1 : w1 = p
    · rw [if_pos h1, if_pos ⟨(w1, w2), List.mem_cons_self, Or.inl h1⟩]; exact congrArg some (h0.1 h1)
    rw [if_neg h1, if_neg]
    rintro ⟨w, hw, h⟩
    rcases List.mem_cons.mp hw with rfl | hw
    · rcases h with h | h
      · exact h1 h
      · exact h2 h
    · exact hl ⟨w, hw, h⟩

/-- **The engine starts on the very net the job built**, when the build passes `Build.ok` and names
each agent once. -/
theorem ofBuild_view {b : Build K} (hok : b.ok S = true) (hnd : (b.kinds.map Prod.fst).Nodup) :
    (Eng.ofBuild b).view = b.net := by
  simp only [Build.ok, Bool.and_eq_true, List.all_eq_true, beq_iff_eq, bne_iff_ne, ne_eq] at hok
  obtain ⟨_, hw⟩ := hok
  apply Net.ext'
  · funext x
    show (Std.HashMap.ofList b.kinds)[x]? = (b.kinds.find? (·.1 == x)).map (·.2)
    cases hf : b.kinds.find? (·.1 == x) with
    | none =>
      apply Std.HashMap.getElem?_ofList_of_contains_eq_false
      simp only [List.contains_eq_any_beq, List.any_eq_false, List.mem_map, beq_iff_eq]
      rintro y ⟨e, he, rfl⟩ rfl
      have := List.find?_eq_none.mp hf e he
      simp at this
    | some e =>
      have hex := List.find?_some hf; simp only [beq_iff_eq] at hex
      apply Std.HashMap.getElem?_ofList_of_mem (k := e.1) (by simp [hex])
      · have := List.pairwise_map.mp hnd
        exact this.imp fun h => by simpa using h
      · exact List.mem_of_find?_eq_some hf
  · funext p
    show (b.wires.foldl (fun (m : Std.HashMap Port Port) (w : Port × Port) => match w with
      | (p, q) => (m.insert p q).insert q p) ({} : Std.HashMap Port Port))[p]? = lookup b.wires p
    cases hl : lookup b.wires p with
    | none =>
      rw [wires_fold p p b.wires {} ?_]
      · rw [if_neg]
        · simp
        rintro ⟨w, hwm, h⟩
        obtain ⟨⟨⟨⟨h1, h2⟩, _⟩, _⟩, _⟩ := hw w hwm
        rcases h with rfl | rfl
        · rw [hl] at h1; cases h1
        · rw [hl] at h2; cases h2
      · intro w hwm
        obtain ⟨⟨⟨⟨h1, h2⟩, _⟩, _⟩, _⟩ := hw w hwm
        exact ⟨fun e => (by rw [e, hl] at h1; cases h1), fun e => (by rw [e, hl] at h2; cases h2)⟩
    | some q =>
      rw [wires_fold p q b.wires {} ?_]
      · rcases lookup_mem hl with hm | hm
        · rw [if_pos ⟨_, hm, Or.inl rfl⟩]
        · rw [if_pos ⟨_, hm, Or.inr rfl⟩]
      · intro w hwm
        obtain ⟨⟨⟨⟨h1, h2⟩, _⟩, _⟩, _⟩ := hw w hwm
        refine ⟨fun e => ?_, fun e => ?_⟩
        · rw [e, hl] at h1; exact (Option.some.inj h1).symm
        · rw [e, hl] at h2; exact (Option.some.inj h2).symm

end Start

/-- What a loaded job must satisfy for the soundness theorem below; see "Remaining obligations" for
proving it from `Gadget.ok` and `Kit.ok`. Each agent is named once, by a root name below `next`; wires
sit on real ports; the only forwarder is the answer's root. -/
structure LoadOk (sys : System K) (g : Gadget K) (j : JobTm K) : Prop where
  nodup : ((load g j).1.kinds.map Prod.fst).Nodup
  below : ∀ x, (load g j).1.net.kind x ≠ none → ∃ n, x = Ident.root n ∧ n < (load g j).1.next
  range : InRange sys.sig (load g j).1.net
  fwds : ∀ x P, (load g j).1.net.kind x = some (.fwd P) → x = (load g j).2
  rootFwd : ∃ P, (load g j).1.net.kind (load g j).2 = some (.fwd P)
  rootName : ∃ n, (load g j).2 = Ident.root n

/-- **Layer 4, up to two transfers.** If `runJob` answers `v` with tally `t`, the loaded net has a run of
the theory of exactly `t.fires + t.cuts` moves, keeping the root, to a `Finished` net `F` with every
rule's fire count as tallied; the engine's final state is `F` renamed (root kept), the engine judged
it clean, and read `v` back from it. So every finishing run of the loaded net has that length
(`work_well_defined`). `runJob_answers` moves the readback onto `F`. -/
theorem runJob_run [DecidableEq K] {sys : System K} {g : Gadget K} {budget : Nat} {j : JobTm K} {v : Src}
    {t : Tally} (hok : Ok sys) (hL : LoadOk sys g j) (h : runJob sys g budget j = some (v, t)) :
    ∃ F σ l, ∃ e' : Eng K, Trace sys.sig sys.rules (· = (load g j).2) (load g j).1.net l F ∧
      Path (Move sys.sig sys.rules (· = (load g j).2)) (t.fires + t.cuts) (load g j).1.net F ∧
      Finished sys.sig sys.rules (· = (load g j).2) F ∧
      (∀ i, t.perRule[i]? = if i < sys.rules.length then some (l.count (some i)) else none) ∧
      Iso σ F e'.view ∧ σ (load g j).2 = (load g j).2 ∧ e'.clean sys.sig (load g j).2 = true ∧
      (e'.view.mate ((load g j).2, 0)).bind (readback g readFuel e'.view []) = some v ∧
      (∀ n F', Path (Move sys.sig sys.rules (· = (load g j).2)) n (load g j).1.net F' →
        Finished sys.sig sys.rules (· = (load g j).2) F' → n = t.fires + t.cuts) ∧
      F.Good sys.sig ∧ (∃ P, F.kind (load g j).2 = some (.fwd P)) := by
  have hRR := hok.ruleInRange
  unfold runJob at h
  obtain ⟨nd, bl, rg, fw, rf, rn⟩ := hL
  generalize hlj : load g j = lj at h nd bl rg fw rf rn ⊢
  obtain ⟨b, root⟩ := lj
  simp only at h nd bl rg fw rf rn ⊢
  split at h
  · cases h
  rename_i hb
  have hb' : b.ok sys.sig = true := by simpa using hb
  split at h
  · cases h
  rename_i e' t' hr
  split at h
  · rename_i hcl
    cases ho : (e'.view.mate (root, 0)).bind (readback g readFuel e'.view []) with
    | none => rw [ho] at h; cases h
    | some w =>
      rw [ho] at h
      simp only [Option.map_some, Option.some.injEq, Prod.mk.injEq] at h
      obtain ⟨rfl, rfl⟩ := h
      have hview := ofBuild_view hb' nd
      have hg : (Eng.ofBuild b).view.Good sys.sig := hview ▸ Build.ok_good hb'
      have hR : InRange sys.sig (Eng.ofBuild b).view := hview ▸ rg
      have hroots : (Eng.ofBuild b).Roots := by
        intro x hx
        have : b.net.kind x ≠ none := by rw [← hview]; exact hx
        exact bl x this
      have hk : ∃ P, (Eng.ofBuild b).view.kind root = some (.fwd P) := hview ▸ rf
      have hcov : Covered sys.rules (Eng.ofBuild b) (b.kinds.map (·.1)) := by
        intro a _ _ _ _ hka _
        left
        have : b.net.kind a ≠ none := by rw [← hview]; show (Eng.ofBuild b).view.kind a ≠ none; rw [hka]; simp
        cases hf : b.kinds.find? (·.1 == a) with
        | none => exact absurd (by simp [Build.net, hf]) this
        | some e =>
          have hex := List.find?_some hf; simp only [beq_iff_eq] at hex
          exact List.mem_map.mpr ⟨e, List.mem_of_find?_eq_some hf, hex⟩
      have hfw : FwdRoot root (Eng.ofBuild b) := by
        intro x P hx
        exact fw x P (by rw [← hview]; exact hx)
      obtain ⟨F, σ, l, ht, I, hσr, tal, hp, hfin, hgF, hrF⟩ := rounds_finished hok.run hRR rn hg hR hroots hk hcov hfw hr
      rw [hview] at ht hp
      simp only [Nat.sub_zero] at hp
      refine ⟨F, σ, l, e', ht, hp, hfin, ?_, I, ?_, hcl, ho, ?_, hgF, hrF⟩
      · intro i
        rw [tal.2.2 i]
        by_cases hi : i < sys.rules.length
        · simp [hi]
        · simp [hi]
      · exact hσr
      · intro n F' hp' hf'
        exact work_well_defined hok.run (Build.ok_good hb') hp' hf' hp hfin
  · cases h

theorem root_readback_iso [DecidableEq K] (g : Gadget K) {σ : Ident → Ident} {F M : Net K} {root : Ident}
    (I : Iso σ F M) (hP : F.Present) (hσr : σ root = root) (hroot : F.kind root ≠ none) (fuel : Nat) :
    (M.mate (root, 0)).bind (readback g fuel M []) = (F.mate (root, 0)).bind (readback g fuel F []) := by
  have hm := I.mate root 0 hroot
  rw [hσr] at hm
  rw [hm]
  cases ht : F.mate (root, 0) with
  | none => rfl
  | some q =>
    have := readback_iso g I hP fuel [] q (hP _ _ ht).2 (by simp)
    simpa using this

/-- **What `runJob` proves.** A run that answers `v` is a real finished run of the theory's moves, whose
readback (on the theory's net, not the engine's renamed one) is `v`; it is a full `Answers` once that net is
`Clean`. -/
theorem runJob_answers [DecidableEq K] {sys : System K} {g : Gadget K} {budget : Nat} {j : JobTm K} {v : Src}
    {t : Tally} (hok : Ok sys) (hL : LoadOk sys g j) (h : runJob sys g budget j = some (v, t)) :
    ∃ F, Moves sys.sig sys.rules (· = (load g j).2) (load g j).1.net F ∧
      Finished sys.sig sys.rules (· = (load g j).2) F ∧
      (F.mate ((load g j).2, 0)).bind (readback g readFuel F []) = some v ∧
      (Clean F (load g j).2 → Answers sys g j (· = v)) := by
  have hb : (load g j).1.ok sys.sig = true := by
    unfold runJob at h
    revert h
    generalize load g j = lj
    obtain ⟨b, root⟩ := lj
    simp only
    split
    · intro h; cases h
    · rename_i hb; intro _; simpa using hb
  obtain ⟨F, σ, l, e', _, hp, hfin, _, I, hσr, _, ho, _, hgF, P, hrF⟩ := runJob_run hok hL h
  have hroot : F.kind (load g j).2 ≠ none := by rw [hrF]; simp
  have hrd : (F.mate ((load g j).2, 0)).bind (readback g readFuel F []) = some v := by
    rw [← ho, root_readback_iso g I hgF.present hσr hroot]
  exact ⟨F, hp.star, hfin, hrd, fun hc => ⟨hb, F, hp.star, hfin, hc, readFuel, v, hrd, rfl⟩⟩

/-- The one transfer still unproved (see the header): the engine's `clean` on a renamed copy means `Clean`. -/
def CleanTransfer (S : Sig K) : Prop :=
  ∀ (e : Eng K) (F : Net K) (σ : Ident → Ident) (root : Ident), Iso σ F e.view → σ root = root →
    F.Good S → (∃ P, F.kind root = some (.fwd P)) → e.clean S root = true → Clean F root

theorem runJob_answers' [DecidableEq K] {sys : System K} {g : Gadget K} {budget : Nat} {j : JobTm K} {w : Src}
    (hok : Ok sys) (hL : LoadOk sys g j) (hC : CleanTransfer sys.sig)
    (h : ((runJob sys g budget j).map (·.1) == some w) = true) : Answers sys g j (· = w) := by
  cases hr : runJob sys g budget j with
  | none => rw [hr] at h; cases h
  | some vt =>
    obtain ⟨v, t⟩ := vt
    rw [hr] at h
    have hv : v = w := by simpa using h
    subst hv
    obtain ⟨F, σ, l, e', _, hp, hfin, _, I, hσr, hcl, ho, _, hgF, hrF⟩ := runJob_run hok hL hr
    have hroot : F.kind (load g j).2 ≠ none := by obtain ⟨P, hP⟩ := hrF; rw [hP]; simp
    have hrd : (F.mate ((load g j).2, 0)).bind (readback g readFuel F []) = some v := by
      rw [← ho, root_readback_iso g I hgF.present hσr hroot]
    have hb : (load g j).1.ok sys.sig = true := by
      unfold runJob at hr
      revert hr
      generalize load g j = lj
      obtain ⟨b, root⟩ := lj
      simp only
      split
      · intro h; cases h
      · rename_i hb; intro _; simpa using hb
    exact ⟨hb, F, hp.star, hfin, hC e' F σ _ I hσr hgF hrF hcl, readFuel, v, hrd, rfl⟩

/-- The jobs `testTerm` runs on t. -/
def testJobs (kit : Kit K) (t : Src) : List (JobTm K) :=
  [reifyJob kit t, plainJob t, haltJob kit t, natJob kit t]

/-- **The test, up to the two transfers.** A passing `testTerm` gives the spec's `Answers` for that term:
`reify` always, the plain job and the halting observer when the reference finishes within `testFuel`,
the numeral observer when the term stands for a number within `testFuel`. -/
theorem testTerm_answers [DecidableEq K] {sys : System K} {g : Gadget K} {kit : Kit K} {t : Src}
    (hok : Ok sys) (hL : ∀ j ∈ testJobs kit t, LoadOk sys g j)
    (hC : CleanTransfer sys.sig) (ht : testTerm sys g kit t = true) :
    Answers sys g (reifyJob kit t) (· = encode t) ∧
      (∀ v, eval testFuel t = some v →
        Answers sys g (plainJob t) (· = v) ∧ Answers sys g (haltJob kit t) (· = scott 0)) ∧
      (∀ k, natWant testFuel t = some k → Answers sys g (natJob kit t) (· = scott k)) := by
  unfold testTerm at ht
  simp only [Bool.and_eq_true] at ht
  obtain ⟨⟨h1, h2⟩, h3⟩ := ht
  refine ⟨runJob_answers' hok (hL _ (by simp [testJobs])) hC h1, fun v hv => ?_, fun k hk => ?_⟩
  · rw [hv] at h2
    simp only [Bool.and_eq_true] at h2
    exact ⟨runJob_answers' hok (hL _ (by simp [testJobs])) hC h2.1,
      runJob_answers' hok (hL _ (by simp [testJobs])) hC h2.2⟩
  · rw [hk] at h3
    exact runJob_answers' hok (hL _ (by simp [testJobs])) hC h3

/-- `MeetsTested` gives the spec on every term `testTerms n` lists, up to the two transfers. -/
theorem meetsTested_answers [DecidableEq K] {sys : System K} {g : Gadget K} {kit : Kit K} {n : Nat}
    (hok : Ok sys) (hL : ∀ t, ∀ j ∈ testJobs kit t, LoadOk sys g j)
    (hC : CleanTransfer sys.sig) (hm : MeetsTested sys g kit n) :
    ∀ t ∈ testTerms n, Answers sys g (reifyJob kit t) (· = encode t) ∧
      (∀ v, eval testFuel t = some v →
        Answers sys g (plainJob t) (· = v) ∧ Answers sys g (haltJob kit t) (· = scott 0)) ∧
      (∀ k, natWant testFuel t = some k → Answers sys g (natJob kit t) (· = scott k)) :=
  fun t ht => testTerm_answers hok (hL t) hC (List.all_eq_true.mp hm t ht)

section CleanProof
variable {S : Sig K}

/-- Everything `reach` has seen or still has to visit hangs off the start. -/
theorem reach_go_linked (e : Eng K) (x : Ident) : ∀ (n : Nat) (todo : List Ident) (seen : HashSet Ident),
    (∀ y ∈ todo, Linked e.view x y) → (∀ y ∈ seen, Linked e.view x y) →
    ∀ y ∈ Eng.reach.go S e n todo seen, Linked e.view x y
  | 0, _, seen, _, hs => by simpa only [Eng.reach.go] using hs
  | _ + 1, [], seen, _, hs => by simpa only [Eng.reach.go] using hs
  | n + 1, z :: todo, seen, ht, hs => by
    simp only [Eng.reach.go]
    split
    · exact reach_go_linked e x n todo seen (fun y hy => ht y (List.mem_cons_of_mem _ hy)) hs
    · apply reach_go_linked e x n _ _ _ _
      · intro y hy
        rcases List.mem_append.mp hy with hy | hy
        · simp only [List.mem_map, List.mem_filterMap] at hy
          obtain ⟨⟨y', m⟩, ⟨p, _, hm⟩, rfl⟩ := hy
          obtain ⟨z', i⟩ := p
          have hz : z' = z := by
            simp only [Eng.portsOf, List.mem_map] at *
            rename_i hp
            obtain ⟨_, _, h⟩ := hp
            exact (Prod.mk.inj h).1.symm
          subst hz
          exact Linked.step (ht z' List.mem_cons_self) hm
        · exact ht y (List.mem_cons_of_mem _ hy)
      · intro y hy
        rcases HashSet.mem_insert.mp hy with h | h
        · have : z = y := by simpa using h
          subst this; exact ht z List.mem_cons_self
        · exact hs y h

theorem reach_linked (e : Eng K) (x : Ident) : ∀ y ∈ Eng.reach S e x, Linked e.view x y :=
  reach_go_linked e x _ [x] {} (by simp; exact Linked.refl) (by simp)

/-- A path in the renamed net comes from a path in the theory's net. -/
theorem Iso.linked_back {σ : Ident → Ident} {F M : Net K} (I : Iso σ F M) (hP : F.Present) {root : Ident}
    (hσ : σ root = root) (hr : F.kind root ≠ none) {y' : Ident} (h : Linked M root y') :
    ∃ y, σ y = y' ∧ F.kind y ≠ none ∧ Linked F root y := by
  induction h with
  | refl => exact ⟨root, hσ, hr, .refl⟩
  | step _ hm ih =>
    obtain ⟨y, rfl, hy, hl⟩ := ih
    rename_i n z m
    rw [I.mate y n hy] at hm
    cases hf : F.mate (y, n) with
    | none => rw [hf] at hm; cases hm
    | some q =>
      rw [hf] at hm
      simp only [Option.map_some, Option.some.injEq, pmap] at hm
      obtain ⟨q1, q2⟩ := q
      simp only [Prod.mk.injEq] at hm
      exact ⟨q1, hm.1, (hP _ _ hf).2, .step hl hf⟩

/-- A set inside a map's keys, of the map's size, holds every key. -/
theorem all_of_size {β : Type} {s : HashSet Ident} {m : HashMap Ident β} (hsub : ∀ y ∈ s, y ∈ m)
    (hsz : s.size = m.size) : ∀ x ∈ m, x ∈ s := by
  intro x hx
  apply Classical.byContradiction
  intro hn
  have nd : s.toList.Nodup := HashSet.distinct_toList.imp fun h => by simpa using h
  have sub : s.toList ⊆ m.keys.erase x := by
    intro y hy
    have hy' := HashSet.mem_toList.mp hy
    have hxy : y ≠ x := fun e => hn (e ▸ hy')
    exact (List.mem_erase_of_ne hxy).2 (HashMap.mem_keys.mpr (hsub y hy'))
  have hl := nd.length_le_of_subset sub
  rw [HashSet.length_toList, List.length_erase_of_mem (HashMap.mem_keys.mpr hx), HashMap.length_keys] at hl
  have : 0 < m.size := by
    have := List.length_pos_of_mem (HashMap.mem_keys.mpr hx); rwa [HashMap.length_keys] at this
  omega

/-- **Obligation 2.** The engine's `clean` on a renamed copy of a good net whose root is a forwarder means
`Clean`: no two base agents meet on principal ports, and every agent hangs off the root. Only that `reach`
finds agents joined to the root is needed: the size check then leaves no agent out. -/
theorem cleanTransfer (S : Sig K) : CleanTransfer S := by
  intro e F σ root I hσ hG hroot hc
  unfold Eng.clean at hc
  simp only [Bool.and_eq_true, beq_iff_eq, List.all_eq_true] at hc
  obtain ⟨hpairs, hsize⟩ := hc
  have hr : F.kind root ≠ none := by obtain ⟨P, hP⟩ := hroot; rw [hP]; simp
  refine ⟨?_, ?_⟩
  · intro x y k k' hx hy hm
    have hkx : e.kinds[σ x]? = some (.base k) := by
      have := I.kind x (by rw [hx]; simp); rw [hx] at this; exact this
    have hky : e.kinds[σ y]? = some (.base k') := by
      have := I.kind y (by rw [hy]; simp); rw [hy] at this; exact this
    have hm' : e.mates[(σ x, 0)]? = some (σ y, 0) := by
      have := I.mate x 0 (by rw [hx]; simp); rw [hm] at this; exact this
    have h1 := hpairs (σ x, .base k) (HashMap.mem_toList_iff_getElem?_eq_some.mpr hkx)
    simp [hm', hky] at h1
  · have hin : ∀ y ∈ Eng.reach S e root, y ∈ e.kinds := by
      intro y hy
      obtain ⟨z, rfl, hz, _⟩ := I.linked_back hG.present hσ hr (reach_linked e root y hy)
      have := I.kind_ne hz
      exact HashMap.mem_iff_isSome_getElem?.mpr (Option.isSome_iff_ne_none.mpr this)
    intro x hx
    have hxm : σ x ∈ e.kinds :=
      HashMap.mem_iff_isSome_getElem?.mpr (Option.isSome_iff_ne_none.mpr (I.kind_ne hx))
    have := all_of_size hin hsize (σ x) hxm
    obtain ⟨z, hz, hzn, hl⟩ := I.linked_back hG.present hσ hr (reach_linked e root _ this)
    rwa [I.inj z x hzn hx hz] at hl

end CleanProof

section LoadProof
variable {S : Sig K}

/-- A port of an agent the build has, within that agent's ports. -/
def PortIn (S : Sig K) (b : Build K) (p : Port) : Prop := ∃ g, (p.1, g) ∈ b.kinds ∧ p.2 ≤ agArity S g

/-- What the emitters keep: names distinct, root names below `next`, base agents only, wires on real ports. -/
structure BInv (S : Sig K) (b : Build K) : Prop where
  nodup : (b.kinds.map Prod.fst).Nodup
  below : ∀ e ∈ b.kinds, ∃ n, e.1 = .root n ∧ n < b.next
  base : ∀ e ∈ b.kinds, ∃ k, e.2 = .base k
  wires : ∀ w ∈ b.wires, PortIn S b w.1 ∧ PortIn S b w.2

/-- b' keeps every agent of b. -/
def Grows (b b' : Build K) : Prop := ∀ e ∈ b.kinds, e ∈ b'.kinds

theorem Grows.trans {b b' b'' : Build K} (h : Grows b b') (h' : Grows b' b'') : Grows b b'' :=
  fun e he => h' e (h e he)

theorem PortIn.mono {b b' : Build K} (h : Grows b b') {p : Port} (hp : PortIn S b p) : PortIn S b' p :=
  let ⟨g, hg, hn⟩ := hp; ⟨g, h _ hg, hn⟩

theorem BInv.add {b : Build K} (h : BInv S b) (k : K) :
    BInv S (b.add (.base k)).1 ∧ Grows b (b.add (.base k)).1 ∧ (b.add (.base k)).2 = .root b.next ∧
      ∀ i ≤ arity S k, PortIn S (b.add (.base k)).1 ((b.add (.base k)).2, i) := by
  have hg : Grows b (b.add (.base k)).1 := fun e he => List.mem_cons_of_mem _ he
  refine ⟨⟨?_, ?_, ?_, ?_⟩, hg, rfl, fun i hi => ⟨.base k, List.mem_cons_self, hi⟩⟩
  · simp only [Build.add, List.map_cons, List.nodup_cons, List.mem_map]
    refine ⟨?_, h.nodup⟩
    rintro ⟨e, he, hx⟩
    obtain ⟨n, hn, hlt⟩ := h.below e he
    rw [hn] at hx; cases hx; omega
  · intro e he
    simp only [Build.add, List.mem_cons] at he
    rcases he with rfl | he
    · exact ⟨b.next, rfl, by simp [Build.add]⟩
    · obtain ⟨n, hn, hlt⟩ := h.below e he; exact ⟨n, hn, by simp [Build.add]; omega⟩
  · intro e he
    simp only [Build.add, List.mem_cons] at he
    rcases he with rfl | he
    · exact ⟨k, rfl⟩
    · exact h.base e he
  · intro w hw
    exact ⟨(h.wires w hw).1.mono hg, (h.wires w hw).2.mono hg⟩

theorem BInv.link {b : Build K} (h : BInv S b) {p q : Port} (hp : PortIn S b p) (hq : PortIn S b q) :
    BInv S (b.link p q) where
  nodup := h.nodup
  below := h.below
  base := h.base
  wires w hw := by
    simp only [Build.link, List.mem_cons] at hw
    rcases hw with rfl | hw
    · exact ⟨hp, hq⟩
    · exact h.wires w hw

/-- The gadget's arities, as `Gadget.ok` checks them. -/
structure GArity (S : Sig K) (g : Gadget K) : Prop where
  lam : arity S g.lam = 2
  app : arity S g.app = 2
  var : arity S g.var = 1
  share : arity S g.share = 2
  thaw : arity S g.thaw = 1
  quote : ∀ w, g.quote = .native w → arity S w = 1

theorem Gadget.ok_arity [DecidableEq K] {sys : System K} {g : Gadget K} (h : g.ok sys = true) :
    GArity sys.sig g := by
  simp only [Gadget.ok, Bool.and_eq_true, beq_iff_eq] at h
  obtain ⟨⟨⟨⟨⟨⟨⟨⟨_, _⟩, hl⟩, ha⟩, hv⟩, hs⟩, _⟩, ht⟩, hq⟩ := h
  refine ⟨hl, ha, hv, hs, ht, fun w hw => ?_⟩
  rw [hw] at hq
  simp only [Bool.and_eq_true, beq_iff_eq] at hq
  exact hq.1.2

theorem bindUses_inv {g : Gadget K} (hs : arity S g.share = 2) : ∀ (uses : List Port) (b : Build K) (port : Port),
    BInv S b → PortIn S b port → (∀ u ∈ uses, PortIn S b u) →
    BInv S (bindUses g b port uses) ∧ Grows b (bindUses g b port uses)
  | [], b, port, hb, hp, _ => by
    obtain ⟨hb1, hg1, hx, hp1⟩ := hb.add g.erase
    simp only [bindUses]
    generalize b.add (.base g.erase) = A at hb1 hg1 hx hp1 ⊢
    obtain ⟨b1, e⟩ := A
    exact ⟨hb1.link (hp.mono hg1) (hp1 0 (Nat.zero_le _)), hg1⟩
  | [u], b, port, hb, hp, hu => by
    simp only [bindUses]
    exact ⟨hb.link hp (hu u List.mem_cons_self), fun _ h => h⟩
  | u :: v :: us, b, port, hb, hp, hu => by
    obtain ⟨hb1, hg1, hx, hp1⟩ := hb.add g.share
    simp only [bindUses]
    generalize b.add (.base g.share) = A at hb1 hg1 hx hp1 ⊢
    obtain ⟨b1, s⟩ := A
    dsimp only at *
    have hb2 := (hb1.link (hp.mono hg1) (hp1 0 (Nat.zero_le _))).link (hp1 1 (by omega))
      ((hu u List.mem_cons_self).mono hg1)
    have := bindUses_inv hs (v :: us) _ (s, 2) hb2 (hp1 2 (by omega))
      (fun w hw => (hu w (List.mem_cons_of_mem _ hw)).mono hg1)
    exact ⟨this.1, hg1.trans this.2⟩

/-- Every pending use is a real port. -/
def EnvIn (S : Sig K) (b : Build K) (env : List (List Port)) : Prop := ∀ us ∈ env, ∀ u ∈ us, PortIn S b u

theorem EnvIn.mono {b b' : Build K} (h : Grows b b') {env : List (List Port)} (he : EnvIn S b env) :
    EnvIn S b' env := fun us hus u hu => (he us hus u hu).mono h

theorem modAt_mem {α : Type} (f : α → α) : ∀ (l : List α) (i : Nat) (x : α), x ∈ modAt f l i →
    x ∈ l ∨ ∃ y ∈ l, x = f y
  | [], _, _, h => by simp [modAt] at h
  | y :: l, 0, x, h => by
    simp only [modAt, List.mem_cons] at h
    rcases h with rfl | h
    · exact .inr ⟨y, List.mem_cons_self, rfl⟩
    · exact .inl (List.mem_cons_of_mem _ h)
  | y :: l, i + 1, x, h => by
    simp only [modAt, List.mem_cons] at h
    rcases h with rfl | h
    · exact .inl List.mem_cons_self
    · rcases modAt_mem f l i x h with h | ⟨z, hz, rfl⟩
      · exact .inl (List.mem_cons_of_mem _ h)
      · exact .inr ⟨z, List.mem_cons_of_mem _ hz, rfl⟩

theorem emitSrc_inv {g : Gadget K} (ga : GArity S g) : ∀ (t : Src) (b : Build K) (env : List (List Port)),
    BInv S b → EnvIn S b env →
    BInv S (emitSrc g t b env).1 ∧ Grows b (emitSrc g t b env).1 ∧
      PortIn S (emitSrc g t b env).1 (emitSrc g t b env).2.1 ∧ EnvIn S (emitSrc g t b env).1 (emitSrc g t b env).2.2
  | .var i, b, env, hb, he => by
    obtain ⟨hb1, hg1, hx, hp1⟩ := hb.add g.var
    simp only [emitSrc]
    generalize b.add (.base g.var) = A at hb1 hg1 hx hp1 ⊢
    obtain ⟨b1, v⟩ := A
    dsimp only at *
    refine ⟨hb1, hg1, hp1 0 (Nat.zero_le _), fun us hus u hu => ?_⟩
    rcases modAt_mem _ env i us hus with h | ⟨us0, h0, rfl⟩
    · exact (he us h u hu).mono hg1
    · rcases List.mem_append.mp hu with hu | hu
      · exact (he us0 h0 u hu).mono hg1
      · simp only [List.mem_singleton] at hu; subst hu; exact hp1 1 (by rw [ga.var]; omega)
  | .app f a, b, env, hb, he => by
    obtain ⟨hb1, hg1, hx, hp1⟩ := hb.add g.app
    simp only [emitSrc]
    generalize b.add (.base g.app) = A at hb1 hg1 hx hp1 ⊢
    obtain ⟨b1, x⟩ := A
    dsimp only at *
    obtain ⟨hb2, hg2, hpf, he2⟩ := emitSrc_inv ga f b1 env hb1 (he.mono hg1)
    generalize emitSrc g f b1 env = R at hb2 hg2 hpf he2 ⊢
    obtain ⟨b2, pf, env2⟩ := R
    dsimp only at *
    obtain ⟨hb3, hg3, hpa, he3⟩ := emitSrc_inv ga a b2 env2 hb2 he2
    generalize emitSrc g a b2 env2 = R at hb3 hg3 hpa he3 ⊢
    obtain ⟨b3, pa, env3⟩ := R
    dsimp only at *
    have g13 := hg2.trans hg3
    exact ⟨(hb3.link ((hp1 1 (by rw [ga.app]; omega)).mono g13) (hpf.mono hg3)).link
        ((hp1 2 (by rw [ga.app]; omega)).mono g13) hpa, hg1.trans g13, (hp1 0 (Nat.zero_le _)).mono g13, he3⟩
  | .lam body, b, env, hb, he => by
    obtain ⟨hb1, hg1, hx, hp1⟩ := hb.add g.lam
    simp only [emitSrc]
    generalize b.add (.base g.lam) = A at hb1 hg1 hx hp1 ⊢
    obtain ⟨b1, l⟩ := A
    dsimp only at *
    have he1 : EnvIn S b1 ([] :: env) := by
      intro us hus u hu
      rcases List.mem_cons.mp hus with rfl | hus
      · cases hu
      · exact (he us hus u hu).mono hg1
    obtain ⟨hb2, hg2, hpb, he2⟩ := emitSrc_inv ga body b1 ([] :: env) hb1 he1
    generalize emitSrc g body b1 ([] :: env) = R at hb2 hg2 hpb he2 ⊢
    obtain ⟨b2, pb, env2⟩ := R
    dsimp only at *
    have hb3 := hb2.link ((hp1 2 (by rw [ga.lam]; omega)).mono hg2) hpb
    have hhead : ∀ u ∈ env2.headD [], PortIn S b2 u := by
      cases env2 with
      | nil => simp
      | cons h t => exact he2 h List.mem_cons_self
    obtain ⟨hb4, hg4⟩ := bindUses_inv ga.share (env2.headD []) _ _ hb3 ((hp1 1 (by rw [ga.lam]; omega)).mono hg2) hhead
    refine ⟨hb4, hg1.trans (hg2.trans hg4), (hp1 0 (Nat.zero_le _)).mono (hg2.trans hg4), ?_⟩
    intro us hus u hu
    have : us ∈ env2 := by
      cases env2 with
      | nil => cases hus
      | cons h t => exact List.mem_cons_of_mem _ hus
    exact (he2 us this u hu).mono hg4


theorem mergeBy_perm {α : Type} (le : α → α → Bool) : ∀ (f : Nat) (xs ys : List α),
    (mergeBy le f xs ys).Perm (xs ++ ys)
  | 0, xs, ys => by simp only [mergeBy]; exact .refl _
  | _ + 1, [], ys => by simp only [mergeBy, List.nil_append]; exact .refl _
  | _ + 1, x :: xs, [] => by simp only [mergeBy, List.append_nil]; exact .refl _
  | f + 1, x :: xs, y :: ys => by
    simp only [mergeBy]
    split
    · exact (mergeBy_perm le f xs (y :: ys)).cons x
    · exact ((mergeBy_perm le f (x :: xs) ys).cons y).trans List.perm_middle.symm

theorem sortBy_perm {α : Type} (le : α → α → Bool) : ∀ (f : Nat) (xs : List α), (sortBy le f xs).Perm xs
  | 0, xs => by simp only [sortBy]; exact .refl _
  | f + 1, xs => by
    simp only [sortBy]
    split
    · exact .refl _
    · refine (mergeBy_perm le _ _ _).trans ?_
      refine ((sortBy_perm le f _).append (sortBy_perm le f _)).trans ?_
      rw [List.take_append_drop]

theorem Prog.ok_perm [DecidableEq K] {sys : System K} {P : Prog K} (h : P.ok sys = true) :
    P.endList.Perm (P.expected sys.sig) := by
  simp only [Prog.ok, Bool.and_eq_true, beq_iff_eq] at h
  rw [← h.1.2]; exact (sortBy_perm _ _ _).symm

theorem Prog.mem_expected {P : Prog K} {j n : Nat} (h : PEnd.port j n ∈ P.expected S) :
    ∃ k, P.agents[j]? = some k ∧ n ≤ arity S k := by
  simp only [Prog.expected, List.mem_cons, reduceCtorEq, false_or, List.mem_flatMap, List.mem_map,
    List.mem_range] at h
  obtain ⟨⟨k, j'⟩, hk, n', hn, he⟩ := h
  cases he
  dsimp only at hn
  exact ⟨k, List.mem_zipIdx_iff_getElem?.mp hk, by omega⟩

theorem Prog.endList_length (P : Prog K) : P.endList.length = 2 * P.wires.length := by
  unfold Prog.endList
  generalize P.wires = ws
  induction ws with
  | nil => rfl
  | cons w ws ih => simp only [List.flatMap_cons, List.length_append, ih]; simp; omega

theorem Prog.agents_ne_nil [DecidableEq K] {sys : System K} {P : Prog K} (h : P.ok sys = true) :
    P.agents ≠ [] := by
  intro hn
  have := (Prog.ok_perm h).length_eq
  rw [Prog.endList_length] at this
  simp [Prog.expected, hn] at this
  omega

theorem foldAdd : ∀ (ks : List K) (b : Build K), BInv S b →
    BInv S (ks.foldl (fun b k => (b.add (.base k)).1) b) ∧ Grows b (ks.foldl (fun b k => (b.add (.base k)).1) b) ∧
    (ks.foldl (fun b k => (b.add (.base k)).1) b).wires = b.wires ∧
    ∀ j k, ks[j]? = some k → (Ident.root (b.next + j), Ag.base k) ∈ (ks.foldl (fun b k => (b.add (.base k)).1) b).kinds
  | [], b, hb => ⟨hb, fun _ h => h, rfl, fun j k h => by simp at h⟩
  | k :: ks, b, hb => by
    obtain ⟨hb1, hg1, _, _⟩ := hb.add k
    obtain ⟨h1, h2, h3, h4⟩ := foldAdd ks _ hb1
    simp only [List.foldl_cons]
    refine ⟨h1, hg1.trans h2, h3, fun j k' hj => ?_⟩
    cases j with
    | zero =>
      simp only [List.getElem?_cons_zero, Option.some.injEq] at hj; subst hj
      exact h2 _ (by simp [Build.add])
    | succ j =>
      have := h4 j k' (by simpa using hj)
      simp only [Build.add] at this
      rwa [show b.next + 1 + j = b.next + (j + 1) by omega] at this

theorem foldLinks {α : Type} (f : Build K → α → Build K) : ∀ (ws : List α) (b : Build K), BInv S b →
    (∀ w ∈ ws, ∀ b' : Build K, b'.kinds = b.kinds →
      f b' w = b' ∨ ∃ p q, PortIn S b p ∧ PortIn S b q ∧ f b' w = b'.link p q) →
    BInv S (ws.foldl f b) ∧ (ws.foldl f b).kinds = b.kinds
  | [], b, hb, _ => ⟨hb, rfl⟩
  | w :: ws, b, hb, hf => by
    simp only [List.foldl_cons]
    have step : BInv S (f b w) ∧ (f b w).kinds = b.kinds := by
      rcases hf w List.mem_cons_self b rfl with h | ⟨p, q, hp, hq, h⟩
      · rw [h]; exact ⟨hb, rfl⟩
      · rw [h]; exact ⟨hb.link hp hq, rfl⟩
    have := foldLinks f ws (f b w) step.1 (fun w' hw' b' hk => by
      rcases hf w' (List.mem_cons_of_mem _ hw') b' (hk.trans step.2) with h | ⟨p, q, hp, hq, h⟩
      · exact .inl h
      · exact .inr ⟨p, q, by unfold PortIn; rw [step.2]; exact hp, by unfold PortIn; rw [step.2]; exact hq, h⟩)
    exact ⟨this.1, this.2.trans step.2⟩

theorem getD_findSome {α β : Type} (Q : β → Prop) {l : List α} {F : α → Option β} {d : β} (hd : Q d)
    (h : ∀ w ∈ l, ∀ x, F w = some x → Q x) : Q ((l.findSome? F).getD d) := by
  cases hs : l.findSome? F with
  | none => exact hd
  | some x =>
    obtain ⟨w, hw, hx⟩ := List.exists_of_findSome?_eq_some hs
    exact h w hw x hx

theorem addProg_inv [DecidableEq K] {sys : System K} {P : Prog K} (hP : P.ok sys = true) {b : Build K}
    (hb : BInv sys.sig b) :
    BInv sys.sig (addProg b P).1 ∧ Grows b (addProg b P).1 ∧ PortIn sys.sig (addProg b P).1 (addProg b P).2 := by
  obtain ⟨h1, h2, _, h4⟩ := foldAdd (S := sys.sig) P.agents b hb
  unfold addProg
  dsimp only
  generalize P.agents.foldl (fun b k => (b.add (.base k)).1) b = b1 at h1 h2 h4
  have hpj : ∀ j n, PEnd.port j n ∈ P.expected sys.sig → PortIn sys.sig b1 (Ident.root (b.next + j), n) := by
    intro j n h
    obtain ⟨k, hk, hn⟩ := Prog.mem_expected h
    exact ⟨.base k, h4 j k hk, hn⟩
  have hout : PortIn sys.sig b1 (Ident.root b.next, 0) := by
    cases ha : P.agents with
    | nil => exact absurd ha (Prog.agents_ne_nil hP)
    | cons k _ => exact ⟨.base k, by simpa using h4 0 k (by simp [ha]), Nat.zero_le _⟩
  have hend : ∀ w ∈ P.wires, w.1 ∈ P.expected sys.sig ∧ w.2 ∈ P.expected sys.sig := by
    intro w hw
    have hs := (Prog.ok_perm hP).subset
    exact ⟨hs (List.mem_flatMap.mpr ⟨w, hw, by simp⟩), hs (List.mem_flatMap.mpr ⟨w, hw, by simp⟩)⟩
  let port : PEnd → Port := fun
    | .out => (Ident.root b.next, 0)
    | .port j n => (Ident.root (b.next + j), n)
  let F : Build K → PEnd × PEnd → Build K := fun b w => match w with
    | (.out, _) | (_, .out) => b
    | (e, f) => b.link (port e) (port f)
  obtain ⟨hf1, hf2⟩ := foldLinks (S := sys.sig) F P.wires b1 h1 (by
    intro w hw b' _
    obtain ⟨he1, he2⟩ := hend w hw
    rcases w with ⟨_ | ⟨j, n⟩, _ | ⟨j', n'⟩⟩
    · exact .inl rfl
    · exact .inl rfl
    · exact .inl rfl
    · exact .inr ⟨_, _, hpj j n he1, hpj j' n' he2, rfl⟩)
  refine ⟨hf1, fun e he => ?_, ?_⟩
  · show e ∈ (P.wires.foldl F b1).kinds
    rw [hf2]; exact h2 e he
  · show ∃ g, (_, g) ∈ (P.wires.foldl F b1).kinds ∧ _
    rw [hf2]
    show PortIn sys.sig b1 _
    apply getD_findSome (PortIn sys.sig b1) hout
    intro w hw x hx
    obtain ⟨he1, he2⟩ := hend w hw
    rcases w with ⟨_ | ⟨j, n⟩, _ | ⟨j', n'⟩⟩ <;> simp only [Option.some.injEq, reduceCtorEq] at hx <;> subst hx
    · exact hout
    · exact hpj j' n' he2
    · exact hpj j n he1

/-- Every program a job holds passes `Prog.ok`. -/
def JobOk [DecidableEq K] (sys : System K) : JobTm K → Prop
  | .src _ => True
  | .app f a => JobOk sys f ∧ JobOk sys a
  | .prog P => P.ok sys = true
  | .quote _ => True

theorem emitJob_inv [DecidableEq K] {sys : System K} {g : Gadget K} (ga : GArity sys.sig g) :
    ∀ (j : JobTm K) (b : Build K), JobOk sys j → BInv sys.sig b →
    BInv sys.sig (emitJob g j b).1 ∧ Grows b (emitJob g j b).1 ∧ PortIn sys.sig (emitJob g j b).1 (emitJob g j b).2
  | .src t, b, _, hb => by
    obtain ⟨h1, h2, h3, _⟩ := emitSrc_inv ga t b [] hb (by intro _ h; cases h)
    simp only [emitJob]
    generalize emitSrc g t b [] = R at h1 h2 h3 ⊢
    obtain ⟨b1, p, env⟩ := R
    exact ⟨h1, h2, h3⟩
  | .app f a, b, hj, hb => by
    obtain ⟨hb1, hg1, _, hp1⟩ := hb.add g.app
    simp only [emitJob]
    generalize b.add (.base g.app) = A at hb1 hg1 hp1 ⊢
    obtain ⟨b1, x⟩ := A
    dsimp only at *
    obtain ⟨hb2, hg2, hpf⟩ := emitJob_inv ga f b1 hj.1 hb1
    generalize emitJob g f b1 = R at hb2 hg2 hpf ⊢
    obtain ⟨b2, pf⟩ := R
    dsimp only at *
    obtain ⟨hb3, hg3, hpa⟩ := emitJob_inv ga a b2 hj.2 hb2
    generalize emitJob g a b2 = R at hb3 hg3 hpa ⊢
    obtain ⟨b3, pa⟩ := R
    dsimp only at *
    have g13 := hg2.trans hg3
    exact ⟨(hb3.link ((hp1 1 (by rw [ga.app]; omega)).mono g13) (hpf.mono hg3)).link
        ((hp1 2 (by rw [ga.app]; omega)).mono g13) hpa, hg1.trans g13, (hp1 0 (Nat.zero_le _)).mono g13⟩
  | .prog P, b, hj, hb => by
    simp only [emitJob]
    exact addProg_inv (show P.ok sys = true from hj) hb
  | .quote t, b, _, hb => by
    simp only [emitJob]
    cases hq : g.quote with
    | native w =>
      dsimp only
      obtain ⟨hb1, hg1, _, hp1⟩ := hb.add w
      generalize b.add (.base w) = A at hb1 hg1 hp1 ⊢
      obtain ⟨b1, q⟩ := A
      dsimp only at *
      obtain ⟨h1, h2, h3, _⟩ := emitSrc_inv ga t b1 [] hb1 (by intro _ h; cases h)
      generalize emitSrc g t b1 [] = R at h1 h2 h3 ⊢
      obtain ⟨b2, p, env⟩ := R
      dsimp only at *
      exact ⟨h1.link ((hp1 1 (by rw [ga.quote w hq]; omega)).mono h2) h3, hg1.trans h2,
        (hp1 0 (Nat.zero_le _)).mono h2⟩
    | encoded =>
      dsimp only
      obtain ⟨h1, h2, h3, _⟩ := emitSrc_inv ga (encode t) b [] hb (by intro _ h; cases h)
      generalize emitSrc g (encode t) b [] = R at h1 h2 h3 ⊢
      obtain ⟨b1, p, env⟩ := R
      exact ⟨h1, h2, h3⟩

theorem fst_unique {α β : Type} {a : α} {b c : β} (l : List (α × β)) (hnd : (l.map Prod.fst).Nodup)
    (h1 : (a, b) ∈ l) (h2 : (a, c) ∈ l) : b = c := by
  induction l with
  | nil => cases h1
  | cons e l ih =>
    obtain ⟨e1, e2⟩ := e
    simp only [List.map_cons, List.nodup_cons, List.mem_map] at hnd
    simp only [List.mem_cons, Prod.mk.injEq] at h1 h2
    rcases h1 with ⟨rfl, rfl⟩ | h1 <;> rcases h2 with ⟨h2a, rfl⟩ | h2
    · rfl
    · exact absurd ⟨(a, c), h2, rfl⟩ hnd.1
    · subst h2a; exact absurd ⟨(a, b), h1, rfl⟩ hnd.1
    · exact ih hnd.2 h1 h2

theorem net_kind_iff {b : Build K} (hnd : (b.kinds.map Prod.fst).Nodup) {x : Ident} {gk : Ag K} :
    b.net.kind x = some gk ↔ (x, gk) ∈ b.kinds := by
  constructor
  · intro h
    simp only [Build.net, Option.map_eq_some_iff] at h
    obtain ⟨e, he, rfl⟩ := h
    have hx := List.find?_some he; simp only [beq_iff_eq] at hx
    rw [← hx]; exact List.mem_of_find?_eq_some he
  · intro h
    show (b.kinds.find? (·.1 == x)).map (·.2) = some gk
    cases hf : b.kinds.find? (·.1 == x) with
    | none => exact absurd (List.find?_eq_none.mp hf _ h) (by simp)
    | some e =>
      have hx := List.find?_some hf; simp only [beq_iff_eq] at hx
      have he := List.mem_of_find?_eq_some hf
      obtain ⟨e1, e2⟩ := e
      simp only at hx; subst hx
      simp only [Option.map_some, Option.some.injEq]
      exact fst_unique _ hnd he h

/-- **The loaded net of a job meets the start conditions**, when its gadget has the right arities and
every program in it passes `Prog.ok`. -/
theorem loadOk_of_jobOk [DecidableEq K] {sys : System K} {g : Gadget K} (ga : GArity sys.sig g) {j : JobTm K}
    (hj : JobOk sys j) : LoadOk sys g j := by
  obtain ⟨h1, _, h3⟩ := emitJob_inv ga j {} hj
    ⟨by simp, fun _ h => (by cases h), fun _ h => (by cases h), fun _ h => (by cases h)⟩
  have hload : ∃ b0 p, BInv sys.sig b0 ∧ PortIn sys.sig b0 p ∧ load g j =
      (((((b0.add (.base g.thaw)).1.add (.fwd .i)).1.link (Ident.root b0.next, 0) p).link
        (Ident.root b0.next, 1) (Ident.root (b0.next + 1), 0)), Ident.root (b0.next + 1)) := by
    unfold load
    generalize emitJob g j {} = R at h1 h3
    obtain ⟨b0, p⟩ := R
    exact ⟨b0, p, h1, h3, rfl⟩
  obtain ⟨b0, p, hb, hp, hl⟩ := hload
  generalize hB : ((((b0.add (.base g.thaw)).1.add (.fwd .i)).1.link (Ident.root b0.next, 0) p).link
        (Ident.root b0.next, 1) (Ident.root (b0.next + 1), 0)) = B at hl
  have hk : B.kinds = (Ident.root (b0.next + 1), .fwd .i) :: (Ident.root b0.next, .base g.thaw) :: b0.kinds := by
    rw [← hB]; rfl
  have hw : B.wires = ((Ident.root b0.next, 1), (Ident.root (b0.next + 1), 0)) ::
      ((Ident.root b0.next, 0), p) :: b0.wires := by
    rw [← hB]; rfl
  have hnx : B.next = b0.next + 2 := by rw [← hB]; rfl
  have hnd : (B.kinds.map Prod.fst).Nodup := by
    rw [hk]
    simp only [List.map_cons, List.nodup_cons, List.mem_cons, List.mem_map, Ident.root.injEq]
    refine ⟨?_, ?_, hb.nodup⟩
    · rintro (h | ⟨e, he, hx⟩)
      · omega
      · obtain ⟨n, hn, hlt⟩ := hb.below e he
        rw [hn] at hx; cases hx; omega
    · rintro ⟨e, he, hx⟩
      obtain ⟨n, hn, hlt⟩ := hb.below e he
      rw [hn] at hx; cases hx; omega
  have hgr : Grows b0 B := fun e he => by rw [hk]; exact List.mem_cons_of_mem _ (List.mem_cons_of_mem _ he)
  have hmem : ∀ x gk, B.net.kind x = some gk → (x, gk) ∈ B.kinds := fun x gk h => (net_kind_iff hnd).mp h
  refine ⟨?_, ?_, ?_, ?_, ?_, ?_⟩ <;> rw [hl] <;> dsimp only
  · exact hnd
  · intro x hx
    obtain ⟨gk, hgk⟩ := Option.ne_none_iff_exists'.mp hx
    have := hmem x gk hgk
    rw [hk] at this
    rcases List.mem_cons.mp this with h | h
    · cases h; exact ⟨_, rfl, by omega⟩
    rcases List.mem_cons.mp h with h | h
    · cases h; exact ⟨_, rfl, by omega⟩
    · obtain ⟨n, hn, hlt⟩ := hb.below _ h; exact ⟨n, hn, by omega⟩
  · intro x i q hm
    have hpin : PortIn sys.sig B (x, i) := by
      have hw' : ∀ w ∈ B.wires, PortIn sys.sig B w.1 ∧ PortIn sys.sig B w.2 := by
        have h0 : PortIn sys.sig B (Ident.root b0.next, 0) := ⟨.base g.thaw, by rw [hk]; simp, Nat.zero_le _⟩
        have h1' : PortIn sys.sig B (Ident.root b0.next, 1) :=
          ⟨.base g.thaw, by rw [hk]; simp, by show 1 ≤ arity sys.sig g.thaw; rw [ga.thaw]; omega⟩
        have h2' : PortIn sys.sig B (Ident.root (b0.next + 1), 0) := ⟨.fwd .i, by rw [hk]; simp, Nat.zero_le _⟩
        intro w hwm
        rw [hw] at hwm
        rcases List.mem_cons.mp hwm with rfl | hwm
        · exact ⟨h1', h2'⟩
        rcases List.mem_cons.mp hwm with rfl | hwm
        · exact ⟨h0, hp.mono hgr⟩
        · exact ⟨(hb.wires w hwm).1.mono hgr, (hb.wires w hwm).2.mono hgr⟩
      rcases lookup_mem hm with h | h
      · exact (hw' _ h).1
      · exact (hw' _ h).2
    obtain ⟨gk, hgk, hi⟩ := hpin
    exact ⟨gk, (net_kind_iff hnd).mpr hgk, hi⟩
  · intro x P hx
    have := hmem x _ hx
    rw [hk] at this
    rcases List.mem_cons.mp this with h | h
    · cases h; rfl
    rcases List.mem_cons.mp h with h | h
    · cases h
    · obtain ⟨k, hk'⟩ := hb.base _ h; cases hk'
  · exact ⟨.i, (net_kind_iff hnd).mpr (by rw [hk]; simp)⟩
  · exact ⟨_, rfl⟩

/-- **Obligation 1.** Every job the test runs on a term loads into a net meeting the start conditions,
given `Gadget.ok` and `Kit.ok`. -/
theorem loadOk_of_ok [DecidableEq K] {sys : System K} {g : Gadget K} {kit : Kit K} (hg : g.ok sys = true)
    (hk : kit.ok sys = true) (t : Src) : ∀ j ∈ testJobs kit t, LoadOk sys g j := by
  have ga := Gadget.ok_arity hg
  simp only [Kit.ok, Bool.and_eq_true] at hk
  intro j hj
  simp only [testJobs, reifyJob, plainJob, haltJob, natJob, runQuote, List.mem_cons, List.not_mem_nil,
    or_false] at hj
  apply loadOk_of_jobOk ga
  rcases hj with rfl | rfl | rfl | rfl <;> simp [JobOk, hk.1, hk.2]

end LoadProof

/-! ## The test, with nothing assumed beyond a record's own checks -/

/-- **A passed test gives the spec on every tested term**, from what a record carries (`Ok`, `Gadget.ok`,
`Kit.ok`) and nothing else. -/
theorem meetsTested_spec [DecidableEq K] {sys : System K} {g : Gadget K} {kit : Kit K} {n : Nat}
    (hok : Ok sys) (hg : g.ok sys = true) (hk : kit.ok sys = true) (hm : MeetsTested sys g kit n) :
    ∀ t ∈ testTerms n, Answers sys g (reifyJob kit t) (· = encode t) ∧
      (∀ v, eval testFuel t = some v →
        Answers sys g (plainJob t) (· = v) ∧ Answers sys g (haltJob kit t) (· = scott 0)) ∧
      (∀ k, natWant testFuel t = some k → Answers sys g (natJob kit t) (· = scott k)) :=
  meetsTested_answers hok (fun t => loadOk_of_ok hg hk t) (cleanTransfer sys.sig) hm

/-- A tested record does the job on every term its test lists. -/
theorem Record.tested_spec (R : Record) {n : Nat} (h : R.strength = .tested n) :
    letI := R.deq
    ∀ t ∈ testTerms n, Answers R.sys R.gadget (reifyJob R.kit t) (· = encode t) ∧
      (∀ v, eval testFuel t = some v →
        Answers R.sys R.gadget (plainJob t) (· = v) ∧ Answers R.sys R.gadget (haltJob R.kit t) (· = scott 0)) ∧
      (∀ k, natWant testFuel t = some k → Answers R.sys R.gadget (natJob R.kit t) (· = scott k)) := by
  letI := R.deq
  have hm := R.meets
  rw [h] at hm
  exact meetsTested_spec R.ok R.gadgetOk R.kitOk hm.2

end Statement

