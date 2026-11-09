import Runs

/-! Removing forwarders commutes with firing and with itself, so runs that mix both kinds of step
are confluent too, and every finished run ends in the same net. -/

section Filter
variable {K : Type}

/-- Drop a connection that lands on x. -/
def filt (x : Ident) : Option Port → Option Port
  | some t => if t.1 = x then none else some t
  | none => none

/-- A wire into port n of forwarder x leads out of its other port, unless that leads back into x. -/
theorem via_at (x : Ident) (N : Net K) (n : Nat) :
    via x N (some (x, n)) = if n < 2 then filt x (N.mate (x, 1 - n)) else none := by
  rcases (show n = 0 ∨ n = 1 ∨ n ≥ 2 by omega) with rfl | rfl | hn
  · simp only [via, if_pos rfl, ite_true, show (0 : Nat) < 2 by decide]
    cases N.mate (x, 1) <;> rfl
  · simp only [via, if_pos rfl, Nat.one_ne_zero, ite_false, ite_true, show (1 : Nat) < 2 by decide]
    cases N.mate (x, 0) <;> rfl
  · simp [via, show n ≠ 0 by omega, show n ≠ 1 by omega, show ¬ n < 2 by omega]
end Filter

/-! ## Removing a forwarder commutes with firing -/

section FireUnfwd
variable {K : Type} {S : Sig K} {r : GRule K} {a b x : Ident} {N : Net K}

/-- x is outside the pair and isn't one of the agents the step makes. -/
structure Away (a b x : Ident) : Prop where
  xa : x ≠ a
  xb : x ≠ b
  made : ∀ k, Ident.made a b k ≠ x

theorem Away.not_pair (h : Away a b x) : ¬(x = a ∨ x = b) := by
  rintro (e | e)
  · exact h.xa e
  · exact h.xb e

theorem relink_lands (h : Away a b x) {t q : Port} (ht : t.1 ≠ x)
    (hq : relink r a b (some t) = some q) : q.1 ≠ x := by
  simp only [relink] at hq
  split at hq
  · obtain ⟨k, hk⟩ := serve_made r hq; rw [hk]; exact h.made k
  · split at hq
    · obtain ⟨k, hk⟩ := serve_made r hq; rw [hk]; exact h.made k
    · simp at hq; subst hq; exact ht

theorem relink_filt (h : Away a b x) (o : Option Port) :
    relink r a b (filt x o) = filt x (relink r a b o) := by
  cases o with
  | none => rfl
  | some t =>
    by_cases ht : t.1 = x
    · rw [relink_keep r (by rw [ht]; exact h.xa) (by rw [ht]; exact h.xb)]
      simp [filt, ht, relink]
    · simp only [filt, ht, if_false]
      cases hq : relink r a b (some t) with
      | none => rfl
      | some q => simp [filt, relink_lands h ht hq]

theorem fire_mate_away (h : Away a b x) (n : Nat) :
    (fire S r a b N).mate (x, n) = relink r a b (N.mate (x, n)) :=
  fire_mate_other S r a b N h.not_pair (madeIdx_none_of_ne h.made) n

/-- Rewiring by one hop and skipping forwarder x can go in either order. -/
theorem relink_via (h : Away a b x) (o : Option Port) :
    relink r a b (via x N o) = via x (fire S r a b N) (relink r a b o) := by
  cases o with
  | none => rfl
  | some q =>
    obtain ⟨y, n⟩ := q
    by_cases hy : y = x
    · subst hy
      rw [relink_keep r (q := (y, n)) h.xa h.xb, via_at, via_at, fire_mate_away h]
      split
      · exact relink_filt h _
      · rfl
    · rw [via_keep (q := (y, n)) hy]
      cases hq : relink r a b (some (y, n)) with
      | none => rfl
      | some q => rw [via_keep (relink_lands h hy hq)]

theorem unfwd_slot (h : Away a b x) (e : End) :
    (unfwd x N).mate (slotPort a b e) = via x N (N.mate (slotPort a b e)) := by
  rw [unfwd_mate, if_neg]
  cases e <;> simp [slotPort, Ne.symm h.xa, Ne.symm h.xb, h.made]

theorem madeMate_unfwd (h : Away a b x) (k n : Nat) :
    madeMate r a b (unfwd x N) k n = via x (fire S r a b N) (madeMate r a b N k n) := by
  unfold madeMate
  by_cases hlt : k < r.made.length
  · simp only [if_pos hlt, partner]
    cases hf : findEnd (.new k n) r.wires 0 with
    | none => rfl
    | some t =>
      obtain ⟨w, f, first⟩ := t
      cases f with
      | new j m => exact (via_keep (h.made j)).symm
      | left m => simp only; rw [unfwd_slot h, relink_via h]
      | right m => simp only; rw [unfwd_slot h, relink_via h]
  · simp only [if_neg hlt]
    cases hw : r.wires[k - r.made.length]? with
    | none => rfl
    | some e12 =>
      obtain ⟨e1, e2⟩ := e12
      simp only
      by_cases hs : (e1.isSlot && e2.isSlot) = true
      · simp only [hs, if_true]
        by_cases hn0 : n = 0
        · simp only [hn0, if_true]; rw [unfwd_slot h, relink_via h]
        · simp only [hn0, if_false]
          by_cases hn1 : n = 1
          · simp only [hn1, if_true]; rw [unfwd_slot h, relink_via h]
          · simp only [hn1, if_false]; rfl
      · simp only [hs, Bool.false_eq_true, if_false]; rfl

variable (S r N) in
/-- **Removing a forwarder commutes with firing a pair** it isn't part of, names included. -/
theorem fire_unfwd (h : Away a b x) : fire S r a b (unfwd x N) = unfwd x (fire S r a b N) := by
  apply Net.ext'
  · funext y
    rw [unfwd_kind]
    by_cases hy : y = x
    · subst hy
      rw [if_pos rfl, fire_kind_other S r a b _ h.not_pair (madeIdx_none_of_ne h.made), unfwd_kind, if_pos rfl]
    · rw [if_neg hy]
      simp only [fire, unfwd_kind, hy, if_false]
  · funext p
    obtain ⟨y, n⟩ := p
    rw [unfwd_mate]
    by_cases hy : y = x
    · subst hy
      rw [if_pos rfl, fire_mate_away h, unfwd_mate, if_pos rfl]; rfl
    rw [if_neg hy]
    by_cases hab : y = a ∨ y = b
    · rw [fire_mate_pair S r a b _ hab, fire_mate_pair S r a b _ hab]; rfl
    cases hk : madeIdx a b y with
    | some k =>
      have := madeIdx_some hk; subst this
      rw [fire_mate_made, fire_mate_made]
      exact madeMate_unfwd h k n
    | none =>
      rw [fire_mate_other S r a b _ hab hk, fire_mate_other S r a b _ hab hk, unfwd_mate, if_neg hy]
      exact relink_via h _
end FireUnfwd

/-! ## Two forwarder removals commute -/

section UnfwdComm
variable {K : Type} {x y : Ident} {N : Net K}

theorem unfwd_mate_ne (h : x ≠ y) (n : Nat) : (unfwd y N).mate (x, n) = via y N (N.mate (x, n)) := by
  rw [unfwd_mate, if_neg h]

/-- Following a wire into x then on through y, or the other way: the same, on symmetric wires. -/
theorem via_via_at (hxy : x ≠ y) (hinv : N.Involutive) {p : Port} (hpx : p.1 ≠ x) (hpy : p.1 ≠ y)
    {n : Nat} (hm : N.mate p = some (x, n)) :
    via y (unfwd x N) (via x N (some (x, n))) = via x (unfwd y N) (via y N (some (x, n))) := by
  rw [via_keep (q := (x, n)) hxy, via_at, via_at, unfwd_mate_ne hxy]
  by_cases hn : ¬ n < 2
  · simp only [hn, ite_false]; rfl
  simp only [Classical.not_not.mp hn, ite_true]
  cases hs : N.mate (x, 1 - n) with
  | none => rfl
  | some t =>
    obtain ⟨z, j⟩ := t
    by_cases hzx : z = x
    · subst hzx; simp [filt, via_keep (q := (z, j)) hxy]; rfl
    by_cases hzy : z ≠ y
    · simp [filt, hzx, via_keep (q := (z, j)) hzy]
    obtain rfl : z = y := Classical.not_not.mp hzy
    simp only [filt, hzx, if_false]
    rw [via_at, via_at, unfwd_mate_ne (Ne.symm hxy)]
    by_cases hj : ¬ j < 2
    · simp [hj, filt]
    simp only [Classical.not_not.mp hj, ite_true]
    cases hu : N.mate (z, 1 - j) with
    | none => rfl
    | some v =>
      obtain ⟨w, i⟩ := v
      by_cases hwz : w = z
      · subst hwz; simp [filt, via_keep (q := (w, i)) (Ne.symm hxy)]
      by_cases hwx : w ≠ x
      · simp [filt, hwz, hwx, via_keep (q := (w, i)) hwx]
      obtain rfl : w = x := Classical.not_not.mp hwx
      simp only [filt, hwz, if_true, if_false]
      rw [via_at]
      by_cases hi : ¬ i < 2
      · simp [hi, filt]
      have hi := Classical.not_not.mp hi
      simp only [hi, ite_true]
      by_cases hin : i = n
      · subst hin; simp [hs, filt, hzx]
      · -- then i = 1 - n, and symmetry would join (x, 1 - n) to both (z, j) and (z, 1 - j)
        have hi' : i = 1 - n := by omega
        have := hinv _ _ hu
        rw [hi', hs] at this
        simp at this
        omega

theorem via_via (hxy : x ≠ y) (hinv : N.Involutive) {p : Port} (hpx : p.1 ≠ x) (hpy : p.1 ≠ y) :
    via y (unfwd x N) (via x N (N.mate p)) = via x (unfwd y N) (via y N (N.mate p)) := by
  cases hm : N.mate p with
  | none => rfl
  | some q =>
    obtain ⟨z, n⟩ := q
    by_cases hz : z = x
    · subst hz; exact via_via_at hxy hinv hpx hpy hm
    by_cases hz' : z = y
    · subst hz'; exact (via_via_at (Ne.symm hxy) hinv hpy hpx hm).symm
    simp only [via_keep (q := (z, n)) hz, via_keep (q := (z, n)) hz']

/-- **Two forwarder removals commute**, on symmetric wires. -/
theorem unfwd_comm (hxy : x ≠ y) (hinv : N.Involutive) : unfwd y (unfwd x N) = unfwd x (unfwd y N) := by
  apply Net.ext'
  · funext z
    simp only [unfwd_kind]
    by_cases h1 : z = x <;> by_cases h2 : z = y <;> simp [h1, h2]
  · funext p
    simp only [unfwd_mate]
    by_cases hpx : p.1 = x
    · simp [hpx, hxy, via]
    by_cases hpy : p.1 = y
    · simp [hpy, Ne.symm hxy, via]
    simp only [hpx, hpy, if_false]
    exact via_via hxy hinv hpx hpy
end UnfwdComm

/-! ## Runs that mix both kinds of step -/

section Moves
variable {K : Type} (S : Sig K) (T : List (GRule K)) (keep : Ident → Prop)

/-- Remove a forwarder, other than those kept (a run keeps the one its answer arrives at). -/
def Cut (N N' : Net K) : Prop := ∃ x P, ¬ keep x ∧ N.kind x = some (.fwd P) ∧ N' = unfwd x N

/-- One step of a run: fire an active pair, or remove a forwarder. -/
def Move (N N' : Net K) : Prop := Step S T N N' ∨ Cut keep N N'

/-- Runs that fire pairs and remove forwarders in any order. -/
abbrev Moves : Net K → Net K → Prop := Star (Move S T keep)

/-- Nothing left to fire and no forwarder left to remove. -/
def Finished (N : Net K) : Prop := ∀ M, ¬ Move S T keep N M

variable {S T keep}

theorem cut_good {N N' : Net K} (hg : N.Good S) (h : Cut keep N N') : N'.Good S := by
  obtain ⟨x, P, _, hx, rfl⟩ := h
  exact ⟨unfwd_polarized S N x P hx hg.polarized, unfwd_present hg.present,
    unfwd_involutive hg.involutive, unfwd_tidy hg.tidy⟩

/-- Firing a pair and removing a forwarder, from the same good net: each can follow the other. -/
theorem step_cut {N N1 N2 : Net K} (hg : N.Good S) (h1 : Step S T N N1) (h2 : Cut keep N N2) :
    ∃ M, Cut keep N1 M ∧ Step S T N2 M := by
  obtain ⟨a, b, r, hr, hab, hka, hkb, rfl⟩ := h1
  obtain ⟨x, P, kx, hx, rfl⟩ := h2
  have pa : N.kind a ≠ none := by simp [hka]
  have pb : N.kind b ≠ none := by simp [hkb]
  have px : N.kind x ≠ none := by simp [hx]
  have xa : x ≠ a := by intro e; rw [e, hka] at hx; cases hx
  have xb : x ≠ b := by intro e; rw [e, hkb] at hx; cases hx
  have away : Away a b x := ⟨xa, xb, fun k e => px (e ▸ hg.tidy.fresh a b k pa pb)⟩
  refine ⟨unfwd x (fire S r a b N), ⟨x, P, kx, ?_, rfl⟩, ⟨a, b, r, hr, ?_, ?_, ?_, (fire_unfwd S r N away).symm⟩⟩
  · rw [fire_kind_other S r a b N away.not_pair (madeIdx_none_of_ne away.made)]; exact hx
  · rw [unfwd_mate, if_neg (Ne.symm away.xa), hab, via_keep (Ne.symm away.xb)]
  · rw [unfwd_kind, if_neg (Ne.symm away.xa)]; exact hka
  · rw [unfwd_kind, if_neg (Ne.symm away.xb)]; exact hkb

/-- Removing two forwarders: the same removal, or each can follow the other. -/
theorem cut_cut {N N1 N2 : Net K} (hinv : N.Involutive) (h1 : Cut keep N N1) (h2 : Cut keep N N2) :
    N1 = N2 ∨ ∃ M, Cut keep N1 M ∧ Cut keep N2 M := by
  obtain ⟨x, P, kx, hx, rfl⟩ := h1
  obtain ⟨y, Q, ky, hy, rfl⟩ := h2
  by_cases hxy : x = y
  · subst hxy; exact Or.inl rfl
  refine Or.inr ⟨unfwd y (unfwd x N), ⟨y, Q, ky, ?_, rfl⟩, ⟨x, P, kx, ?_, unfwd_comm hxy hinv⟩⟩
  · rw [unfwd_kind, if_neg (Ne.symm hxy)]; exact hy
  · rw [unfwd_kind, if_neg hxy]; exact hx

theorem RunOk.move_good (h : RunOk S T) {N N' : Net K} (hg : N.Good S) (hm : Move S T keep N N') :
    N'.Good S :=
  hm.elim (h.step_good hg) (cut_good hg)

/-- **Strong confluence for both kinds of step.** -/
theorem RunOk.move_diamond (h : RunOk S T) : Diamond (Move S T keep) (Net.Good S) := by
  intro N N1 N2 hg h1 h2
  rcases h1 with s1 | c1 <;> rcases h2 with s2 | c2
  · rcases h.diamond _ _ _ hg s1 s2 with e | ⟨M, m1, m2⟩
    · exact Or.inl e
    · exact Or.inr ⟨M, Or.inl m1, Or.inl m2⟩
  · obtain ⟨M, m1, m2⟩ := step_cut hg s1 c2
    exact Or.inr ⟨M, Or.inr m1, Or.inl m2⟩
  · obtain ⟨M, m2, m1⟩ := step_cut hg s2 c1
    exact Or.inr ⟨M, Or.inl m1, Or.inr m2⟩
  · rcases cut_cut hg.involutive c1 c2 with e | ⟨M, m1, m2⟩
    · exact Or.inl e
    · exact Or.inr ⟨M, Or.inr m1, Or.inr m2⟩

theorem RunOk.moves_good (h : RunOk S T) {N M : Net K} (hg : N.Good S) (hr : Moves S T keep N M) :
    M.Good S :=
  Star.keeps (G := Net.Good S) (fun _ _ hg hm => h.move_good hg hm) hg hr

/-- **Runs are confluent**, firing and removing forwarders in any order. -/
theorem RunOk.moves_confluent (h : RunOk S T) {N N1 N2 : Net K} (hg : N.Good S)
    (h1 : Moves S T keep N N1) (h2 : Moves S T keep N N2) : ∃ M, Moves S T keep N1 M ∧ Moves S T keep N2 M :=
  Star.confluent (G := Net.Good S) (fun _ _ hg hm => h.move_good hg hm) h.move_diamond hg h1 h2

/-- **Every finished run ends in the same net**, names included, whatever order it fired pairs
and removed forwarders in. -/
theorem RunOk.finished_unique (h : RunOk S T) {N N1 N2 : Net K} (hg : N.Good S)
    (h1 : Moves S T keep N N1) (h2 : Moves S T keep N N2)
    (f1 : Finished S T keep N1) (f2 : Finished S T keep N2) : N1 = N2 :=
  Star.unique (G := Net.Good S) (fun _ _ hg hm => h.move_good hg hm) h.move_diamond hg h1 h2 f1 f2
end Moves
