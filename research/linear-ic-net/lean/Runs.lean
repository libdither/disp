import Net

/-! Steps keep the invariants the one-step theorems assume, so they hold along whole runs; and
every run from a good net ends in the same net, whatever order it fires in. -/

/-! ## Names -/

/-- x is part of y's name: y was made from x, or from something made from x. -/
def Ident.Within (x : Ident) : Ident → Prop
  | .root _ => False
  | .made a b _ => x = a ∨ x = b ∨ x.Within a ∨ x.Within b

theorem Ident.within_size {x : Ident} : ∀ {y : Ident}, x.Within y → sizeOf x < sizeOf y := by
  intro y
  induction y with
  | root n => intro h; exact h.elim
  | made a b k iha ihb =>
    intro h
    simp only [Ident.made.sizeOf_spec]
    rcases h with rfl | rfl | h | h
    · omega
    · omega
    · have := iha h; omega
    · have := ihb h; omega

theorem Ident.within_trans {x y : Ident} (h1 : x.Within y) : ∀ {z : Ident}, y.Within z → x.Within z := by
  intro z
  induction z with
  | root n => intro h; exact h.elim
  | made a b k iha ihb =>
    intro h
    rcases h with rfl | rfl | h | h
    · exact Or.inr (Or.inr (Or.inl h1))
    · exact Or.inr (Or.inr (Or.inr h1))
    · exact Or.inr (Or.inr (Or.inl (iha h)))
    · exact Or.inr (Or.inr (Or.inr (ihb h)))

section
variable {K : Type}

/-- No agent's name is part of another present agent's name. Nets whose agents all have root
names are tidy, and steps keep them tidy. -/
def Net.Tidy (N : Net K) : Prop := ∀ x y, N.kind x ≠ none → N.kind y ≠ none → ¬ x.Within y

theorem Net.Tidy.fresh {N : Net K} (h : N.Tidy) : N.Fresh := by
  intro x y k hx _
  cases hm : N.kind (.made x y k) with
  | none => rfl
  | some g => exact absurd (show x.Within (.made x y k) from Or.inl rfl) (h x _ hx (by simp [hm]))

theorem tidy_of_roots {N : Net K} (h : ∀ x, N.kind x ≠ none → ∃ n, x = .root n) : N.Tidy := by
  intro x y _ hy hw
  obtain ⟨n, rfl⟩ := h y hy
  exact hw

variable (S : Sig K) (r : GRule K) (a b : Ident) (N : Net K)

theorem fire_kind_some {x : Ident} (hx : (fire S r a b N).kind x ≠ none) :
    (∃ k, x = .made a b k) ∨ (¬(x = a ∨ x = b) ∧ madeIdx a b x = none ∧ N.kind x ≠ none) := by
  by_cases hab : x = a ∨ x = b
  · exact absurd (fire_kind_pair S r a b N hab) hx
  cases hm : madeIdx a b x with
  | some k => exact Or.inl ⟨k, madeIdx_some hm⟩
  | none => exact Or.inr ⟨hab, rfl, by rwa [fire_kind_other S r a b N hab hm] at hx⟩

/-- A step keeps the net tidy. -/
theorem fire_tidy (h : N.Tidy) (pa : N.kind a ≠ none) (pb : N.kind b ≠ none) :
    (fire S r a b N).Tidy := by
  intro x y hx hy hw
  rcases fire_kind_some S r a b N hx with ⟨k, rfl⟩ | ⟨hxab, _, hxN⟩ <;>
  rcases fire_kind_some S r a b N hy with ⟨k', rfl⟩ | ⟨hyab, _, hyN⟩
  · rcases hw with e | e | e | e
    · exact Ident.made_ne_left a b k e
    · exact Ident.made_ne_right a b k e
    · have := Ident.within_size e; simp at this; omega
    · have := Ident.within_size e; simp at this; omega
  · -- a made agent inside an old one: then a is inside it too
    exact h a y pa hyN (Ident.within_trans (show a.Within (.made a b k) from Or.inl rfl) hw)
  · -- an old agent inside a made one: it is a or b, or inside a or b
    rcases hw with rfl | rfl | hw | hw
    · exact hxab (Or.inl rfl)
    · exact hxab (Or.inr rfl)
    · exact h x a hxN pa hw
    · exact h x b hxN pb hw
  · exact h x y hxN hyN hw
end

/-! ## Each wire end used once -/

/-- Every end the rule's wires mention, in order. -/
def ends (ws : List (End × End)) : List End := ws.flatMap fun w => [w.1, w.2]

theorem ends_cons (x y : End) (ws : List (End × End)) : ends ((x, y) :: ws) = x :: y :: ends ws := by
  simp [ends]

theorem mem_ends_of_mem {ws : List (End × End)} {x y : End} (h : (x, y) ∈ ws) : x ∈ ends ws ∧ y ∈ ends ws :=
  ⟨List.mem_flatMap.mpr ⟨(x, y), h, by simp⟩, List.mem_flatMap.mpr ⟨(x, y), h, by simp⟩⟩

/-- With every end used once, either end of a wire finds that wire. -/
theorem findEnd_of_get : ∀ (ws : List (End × End)) (i w : Nat) (x y : End),
    (ends ws).Nodup → ws[w]? = some (x, y) →
    findEnd x ws i = some (i + w, y, true) ∧ findEnd y ws i = some (i + w, x, false) := by
  intro ws
  induction ws with
  | nil => intro i w x y _ h; simp at h
  | cons hd rest ih =>
    intro i w x y hnd h
    obtain ⟨x0, y0⟩ := hd
    rw [ends_cons] at hnd
    have hx0 : x0 ∉ y0 :: ends rest := (List.nodup_cons.mp hnd).1
    have hy0 : y0 ∉ ends rest := (List.nodup_cons.mp (List.nodup_cons.mp hnd).2).1
    have hrest : (ends rest).Nodup := (List.nodup_cons.mp (List.nodup_cons.mp hnd).2).2
    cases w with
    | zero =>
      simp at h; obtain ⟨rfl, rfl⟩ := h
      have hne : x0 ≠ y0 := fun e => hx0 (by simp [e])
      simp [findEnd, hne]
    | succ w' =>
      simp only [List.getElem?_cons_succ] at h
      have ⟨h1, h2⟩ := ih (i + 1) w' x y hrest h
      have ⟨mx, my⟩ := mem_ends_of_mem (List.mem_of_getElem? h)
      have nx0 : x0 ≠ x := fun e => hx0 (by simp [e, mx])
      have ny0 : y0 ≠ x := fun e => hy0 (e ▸ mx)
      have nx0' : x0 ≠ y := fun e => hx0 (by simp [e, my])
      have ny0' : y0 ≠ y := fun e => hy0 (e ▸ my)
      have : i + 1 + w' = i + (w' + 1) := by omega
      simp only [findEnd, nx0, ny0, nx0', ny0', if_false, ↓reduceIte]
      rw [← this]; exact ⟨h1, h2⟩

theorem findEnd_back {ws : List (End × End)} (hnd : (ends ws).Nodup) {e f : End} {w : Nat} {first : Bool}
    (h : findEnd e ws 0 = some (w, f, first)) : findEnd f ws 0 = some (w, e, !first) := by
  have ⟨_, hget⟩ := findEnd_mem h
  cases first
  · have := (findEnd_of_get ws 0 w f e hnd (by simpa using hget)).1
    simpa using this
  · have := (findEnd_of_get ws 0 w e f hnd (by simpa using hget)).2
    simpa using this

/-! ## A step keeps wires present and symmetric -/

section Invariants
variable {K : Type} {S : Sig K} {r : GRule K} {a b : Ident} {N : Net K}

/-- What a step needs to keep wires present and symmetric. -/
structure StepOk (S : Sig K) (r : GRule K) (a b : Ident) (N : Net K) : Prop where
  ab : a ≠ b
  present : N.Present
  fresh : ∀ k, N.kind (.made a b k) = none
  madeOk : r.MadeOk
  nodup : (ends r.wires).Nodup

theorem StepOk.madeIdx_none (h : StepOk S r a b N) {x : Ident} (hx : N.kind x ≠ none) : madeIdx a b x = none := by
  cases hk : madeIdx a b x with
  | none => rfl
  | some k => rw [madeIdx_some hk] at hx; exact absurd (h.fresh k) hx

theorem made_kind_ne {j : Nat} (hj : j < r.made.length) : (fire S r a b N).kind (.made a b j) ≠ none := by
  rw [fire_kind_made]; simp [madeKind, hj]

theorem fwd_kind_ne {w : Nat} {e1 e2 : End} (hget : r.wires[w]? = some (e1, e2))
    (h1 : e1.isSlot = true) (h2 : e2.isSlot = true) :
    (fire S r a b N).kind (.made a b (r.made.length + w)) ≠ none := by
  have : ¬ (r.made.length + w < r.made.length) := by omega
  rw [fire_kind_made]; simp [madeKind, this, hget, h1, h2]

theorem serve_kind_ne (h : StepOk S r a b N) {e : End} (he : e.isSlot = true) {q : Port}
    (hs : serve r a b e = some q) : (fire S r a b N).kind q.1 ≠ none := by
  unfold serve at hs
  cases hf : findEnd e r.wires 0 with
  | none => simp [hf] at hs
  | some t =>
    obtain ⟨w, f, first⟩ := t
    have ⟨hmem, hget⟩ := findEnd_mem hf
    rw [hf] at hs
    cases f with
    | new j m =>
      simp at hs; subst hs
      cases first
      · exact made_kind_ne ((h.madeOk _ hmem).1)
      · exact made_kind_ne ((h.madeOk _ hmem).2)
    | left m | right m =>
      simp at hs; subst hs
      cases first
      · exact fwd_kind_ne (by simpa using hget) rfl he
      · exact fwd_kind_ne (by simpa using hget) he rfl

theorem relink_kind_ne (h : StepOk S r a b N) {p q0 q : Port} (hm : N.mate p = some q0)
    (hq : relink r a b (some q0) = some q) : (fire S r a b N).kind q.1 ≠ none := by
  obtain ⟨x, n⟩ := q0
  simp only [relink] at hq
  by_cases hxa : x = a
  · rw [if_pos hxa] at hq; exact serve_kind_ne h rfl hq
  rw [if_neg hxa] at hq
  by_cases hxb : x = b
  · rw [if_pos hxb] at hq; exact serve_kind_ne h rfl hq
  rw [if_neg hxb] at hq
  simp at hq; subst hq
  have hx := (h.present _ _ hm).2
  rw [fire_kind_other S r a b N (by simp [hxa, hxb]) (h.madeIdx_none hx)]; exact hx

/-- A step keeps every wire on agents that are there. -/
theorem fire_present (h : StepOk S r a b N) : (fire S r a b N).Present := by
  intro p q hpq
  obtain ⟨x, n⟩ := p
  simp only [fire] at hpq
  by_cases hab : x = a ∨ x = b
  · simp [hab] at hpq
  rw [if_neg hab] at hpq
  cases hk : madeIdx a b x with
  | none =>
    rw [hk] at hpq
    cases hm : N.mate (x, n) with
    | none => simp [relink, hm] at hpq
    | some q0 =>
      rw [hm] at hpq
      have hx := (h.present _ _ hm).1
      refine ⟨?_, relink_kind_ne h hm hpq⟩
      rw [fire_kind_other S r a b N hab hk]; exact hx
  | some k =>
    have hx := madeIdx_some hk; subst hx
    rw [hk] at hpq
    simp only [madeMate] at hpq
    by_cases hlt : k < r.made.length
    · rw [if_pos hlt] at hpq
      refine ⟨made_kind_ne hlt, ?_⟩
      simp only [partner] at hpq
      cases hf : findEnd (.new k n) r.wires 0 with
      | none => simp [hf] at hpq
      | some t =>
        obtain ⟨w, f, first⟩ := t
        have ⟨hmem, _⟩ := findEnd_mem hf
        rw [hf] at hpq
        by_cases hfs : f.isSlot = true
        · have hpq' : relink r a b (N.mate (slotPort a b f)) = some q := by
            cases f with
            | new j m => simp [End.isSlot] at hfs
            | left m => simpa using hpq
            | right m => simpa using hpq
          cases hm : N.mate (slotPort a b f) with
          | none => simp [relink, hm] at hpq'
          | some q0 => rw [hm] at hpq'; exact relink_kind_ne h hm hpq'
        · cases f with
          | left m => simp [End.isSlot] at hfs
          | right m => simp [End.isSlot] at hfs
          | new j m =>
            simp at hpq; subst hpq
            cases first
            · exact made_kind_ne ((h.madeOk _ hmem).1)
            · exact made_kind_ne ((h.madeOk _ hmem).2)
    · rw [if_neg hlt] at hpq
      have hk' : k = r.made.length + (k - r.made.length) := by omega
      cases hw : r.wires[k - r.made.length]? with
      | none => simp [hw] at hpq
      | some e12 =>
        obtain ⟨e1, e2⟩ := e12
        rw [hw] at hpq
        by_cases hs : e1.isSlot = true ∧ e2.isSlot = true
        · simp only [hs.1, hs.2, Bool.and_self, ite_true] at hpq
          refine ⟨by rw [hk']; exact fwd_kind_ne hw hs.1 hs.2, ?_⟩
          by_cases hn0 : n = 0
          · simp only [hn0, ite_true] at hpq
            cases hm : N.mate (slotPort a b e1) with
            | none => simp [relink, hm] at hpq
            | some q0 => rw [hm] at hpq; exact relink_kind_ne h hm hpq
          · rw [if_neg hn0] at hpq
            by_cases hn1 : n = 1
            · simp only [hn1, ite_true] at hpq
              cases hm : N.mate (slotPort a b e2) with
              | none => simp [relink, hm] at hpq
              | some q0 => rw [hm] at hpq; exact relink_kind_ne h hm hpq
            · rw [if_neg hn1] at hpq; cases hpq
        · have : (e1.isSlot && e2.isSlot) = false := by
            revert hs; cases e1.isSlot <;> cases e2.isSlot <;> simp
          simp [this] at hpq

theorem relink_slotPort (hab : a ≠ b) {e : End} (he : e.isSlot = true) :
    relink r a b (some (slotPort a b e)) = serve r a b e := by
  cases e with
  | left m => simp [relink, slotPort]
  | right m => simp [relink, slotPort, Ne.symm hab]
  | new j m => simp [End.isSlot] at he

/-- What stands in for a slot connects to wherever the slot's outside neighbour went. -/
theorem serve_back (h : StepOk S r a b N) {e : End} (he : e.isSlot = true) {t : Port}
    (hs : serve r a b e = some t) : (fire S r a b N).mate t = relink r a b (N.mate (slotPort a b e)) := by
  unfold serve at hs
  cases hf : findEnd e r.wires 0 with
  | none => simp [hf] at hs
  | some u =>
    obtain ⟨w, f, first⟩ := u
    have ⟨hmem, hget⟩ := findEnd_mem hf
    have hback := findEnd_back h.nodup hf
    rw [hf] at hs
    by_cases hfs : f.isSlot = true
    · have hs' : ((Ident.made a b (r.made.length + w)), if first then 0 else 1) = t := by
        cases f with
        | new j m => simp [End.isSlot] at hfs
        | left m => simpa using hs
        | right m => simpa using hs
      subst hs'
      rw [fire_mate_made]
      have : ¬ (r.made.length + w < r.made.length) := by omega
      cases first
      · simp only [Bool.false_eq_true, ite_false] at hget
        simp only [Bool.false_eq_true, ↓reduceIte, madeMate, this, Nat.add_sub_cancel_left, hget, hfs, he,
          Bool.and_self, Nat.one_ne_zero]
      · simp only [ite_true] at hget
        simp only [↓reduceIte, madeMate, this, Nat.add_sub_cancel_left, hget, hfs, he, Bool.and_self]
    · cases f with
      | left m => simp [End.isSlot] at hfs
      | right m => simp [End.isSlot] at hfs
      | new j m =>
        simp at hs; subst hs
        have hj : j < r.made.length := by
          cases first
          · exact (h.madeOk _ hmem).1
          · exact (h.madeOk _ hmem).2
        rw [fire_mate_made]
        simp only [madeMate, hj, ite_true, partner, hback]
        cases e with
        | left m' => rfl
        | right m' => rfl
        | new j' m' => simp [End.isSlot] at he

theorem mate_old (h : StepOk S r a b N) {q : Port} (hq : N.kind q.1 ≠ none) (hab : ¬(q.1 = a ∨ q.1 = b)) :
    (fire S r a b N).mate q = relink r a b (N.mate q) := by
  obtain ⟨x, n⟩ := q
  exact fire_mate_other S r a b N hab (h.madeIdx_none hq) n

/-- If what stands in for slot e is p, whatever p connects to connects back to p. -/
theorem slot_sym (h : StepOk S r a b N) (hinv : N.Involutive) {e : End} (he : e.isSlot = true)
    {p q0 q : Port} (hs : serve r a b e = some p) (hm : N.mate (slotPort a b e) = some q0)
    (hq : relink r a b (some q0) = some q) : (fire S r a b N).mate q = some p := by
  have back := hinv _ _ hm
  obtain ⟨x, m⟩ := q0
  simp only [relink] at hq
  by_cases hxa : x = a
  · subst hxa
    rw [if_pos rfl] at hq
    rw [serve_back h rfl hq]
    show relink r x b (N.mate (x, m)) = some p
    rw [back, relink_slotPort h.ab he, hs]
  rw [if_neg hxa] at hq
  by_cases hxb : x = b
  · subst hxb
    rw [if_pos rfl] at hq
    rw [serve_back h rfl hq]
    show relink r a x (N.mate (x, m)) = some p
    rw [back, relink_slotPort h.ab he, hs]
  rw [if_neg hxb] at hq
  simp at hq; subst hq
  rw [mate_old h (h.present _ _ hm).2 (by simp [hxa, hxb]), back, relink_slotPort h.ab he, hs]

theorem serve_fwd (h : StepOk S r a b N) {w : Nat} {e1 e2 : End} (hw : r.wires[w]? = some (e1, e2))
    (h1 : e1.isSlot = true) (h2 : e2.isSlot = true) :
    serve r a b e1 = some (.made a b (r.made.length + w), 0) ∧
    serve r a b e2 = some (.made a b (r.made.length + w), 1) := by
  have ⟨f1, f2⟩ := findEnd_of_get r.wires 0 w e1 e2 h.nodup hw
  constructor
  · unfold serve; rw [f1]
    cases e2 with
    | new j m => simp [End.isSlot] at h2
    | left m => simp
    | right m => simp
  · unfold serve; rw [f2]
    cases e1 with
    | new j m => simp [End.isSlot] at h1
    | left m => simp
    | right m => simp

/-- A step keeps every wire symmetric. -/
theorem fire_involutive (h : StepOk S r a b N) (hinv : N.Involutive) : (fire S r a b N).Involutive := by
  intro p q hpq
  obtain ⟨x, n⟩ := p
  simp only [fire] at hpq
  by_cases hab : x = a ∨ x = b
  · simp [hab] at hpq
  rw [if_neg hab] at hpq
  have hxa : x ≠ a := fun e => hab (Or.inl e)
  have hxb : x ≠ b := fun e => hab (Or.inr e)
  cases hk : madeIdx a b x with
  | none =>
    rw [hk] at hpq
    cases hm : N.mate (x, n) with
    | none => simp [relink, hm] at hpq
    | some q0 =>
      rw [hm] at hpq
      have back := hinv _ _ hm
      obtain ⟨y, m⟩ := q0
      simp only [relink] at hpq
      by_cases hya : y = a
      · rw [if_pos hya] at hpq
        rw [serve_back h rfl hpq]
        show relink r a b (N.mate (a, m)) = some (x, n)
        rw [← hya, back, hya]; exact relink_keep r (q := (x, n)) hxa hxb
      rw [if_neg hya] at hpq
      by_cases hyb : y = b
      · rw [if_pos hyb] at hpq
        rw [serve_back h rfl hpq]
        show relink r a b (N.mate (b, m)) = some (x, n)
        rw [← hyb, back, hyb]; exact relink_keep r (q := (x, n)) hxa hxb
      rw [if_neg hyb] at hpq
      simp at hpq; subst hpq
      rw [mate_old h (h.present _ _ hm).2 (by simp [hya, hyb]), back]
      exact relink_keep r (q := (x, n)) hxa hxb
  | some k =>
    have hx := madeIdx_some hk; subst hx
    rw [hk] at hpq
    simp only [madeMate] at hpq
    by_cases hlt : k < r.made.length
    · rw [if_pos hlt] at hpq
      simp only [partner] at hpq
      cases hf : findEnd (.new k n) r.wires 0 with
      | none => simp [hf] at hpq
      | some t =>
        obtain ⟨w, f, first⟩ := t
        have ⟨hmem, _⟩ := findEnd_mem hf
        have hback := findEnd_back h.nodup hf
        rw [hf] at hpq
        by_cases hfs : f.isSlot = true
        · have hpq' : relink r a b (N.mate (slotPort a b f)) = some q := by
            cases f with
            | new j m => simp [End.isSlot] at hfs
            | left m => simpa using hpq
            | right m => simpa using hpq
          have hs : serve r a b f = some (.made a b k, n) := by unfold serve; rw [hback]
          cases hm : N.mate (slotPort a b f) with
          | none => simp [relink, hm] at hpq'
          | some q0 => rw [hm] at hpq'; exact slot_sym h hinv hfs hs hm hpq'
        · cases f with
          | left m => simp [End.isSlot] at hfs
          | right m => simp [End.isSlot] at hfs
          | new j m =>
            simp at hpq; subst hpq
            have hj : j < r.made.length := by
              cases first
              · exact (h.madeOk _ hmem).1
              · exact (h.madeOk _ hmem).2
            rw [fire_mate_made]
            simp only [madeMate, hj, ite_true, partner, hback]
    · rw [if_neg hlt] at hpq
      have hk' : r.made.length + (k - r.made.length) = k := by omega
      cases hw : r.wires[k - r.made.length]? with
      | none => simp [hw] at hpq
      | some e12 =>
        obtain ⟨e1, e2⟩ := e12
        rw [hw] at hpq
        by_cases hs : e1.isSlot = true ∧ e2.isSlot = true
        · simp only [hs.1, hs.2, Bool.and_self, ite_true] at hpq
          have ⟨sv1, sv2⟩ := serve_fwd h hw hs.1 hs.2
          rw [hk'] at sv1 sv2
          by_cases hn0 : n = 0
          · subst hn0
            simp only [ite_true] at hpq
            cases hm : N.mate (slotPort a b e1) with
            | none => simp [relink, hm] at hpq
            | some q0 => rw [hm] at hpq; exact slot_sym h hinv hs.1 sv1 hm hpq
          · rw [if_neg hn0] at hpq
            by_cases hn1 : n = 1
            · subst hn1
              simp only [ite_true] at hpq
              cases hm : N.mate (slotPort a b e2) with
              | none => simp [relink, hm] at hpq
              | some q0 => rw [hm] at hpq; exact slot_sym h hinv hs.2 sv2 hm hpq
            · rw [if_neg hn1] at hpq; cases hpq
        · have : (e1.isSlot && e2.isSlot) = false := by
            revert hs; cases e1.isSlot <;> cases e2.isSlot <;> simp
          simp [this] at hpq
end Invariants

/-! ## Whole runs -/

/-- Zero or more R-steps. -/
inductive Star {α : Type} (R : α → α → Prop) : α → α → Prop
  | refl (a : α) : Star R a a
  | step {a b c : α} : R a b → Star R b c → Star R a c

section Abstract
variable {α : Type} {R : α → α → Prop} {G : α → Prop}

/-- From a state where G holds, two steps are the same step or can each be followed by one more
to meet. -/
def Diamond (R : α → α → Prop) (G : α → Prop) : Prop :=
  ∀ a b c, G a → R a b → R a c → b = c ∨ ∃ d, R b d ∧ R c d

theorem Star.keeps (pres : ∀ a b, G a → R a b → G b) {a b : α} (hg : G a) (h : Star R a b) : G b := by
  induction h with
  | refl => exact hg
  | step hs _ ih => exact ih (pres _ _ hg hs)

theorem Star.strip (pres : ∀ a b, G a → R a b → G b) (dia : Diamond R G) {a b c : α} (hg : G a)
    (h1 : R a b) (h2 : Star R a c) : ∃ d, Star R b d ∧ (c = d ∨ R c d) := by
  revert hg h1 b
  induction h2 with
  | refl a => intro b _ h1; exact ⟨b, .refl b, Or.inr h1⟩
  | step hs hr ih =>
    intro b hg h1
    rcases dia _ _ _ hg h1 hs with e | ⟨P, hP1, hP2⟩
    · subst e; exact ⟨_, hr, Or.inl rfl⟩
    · obtain ⟨M, hM1, hM2⟩ := ih (pres _ _ hg hs) hP2
      exact ⟨M, .step hP1 hM1, hM2⟩

/-- Any two runs from a state where G holds can be continued to a common state. -/
theorem Star.confluent (pres : ∀ a b, G a → R a b → G b) (dia : Diamond R G) {a b c : α}
    (hg : G a) (h1 : Star R a b) (h2 : Star R a c) : ∃ d, Star R b d ∧ Star R c d := by
  revert hg h2 c
  induction h1 with
  | refl a => intro c _ h2; exact ⟨c, h2, .refl c⟩
  | step hs hr ih =>
    intro c hg h2
    obtain ⟨Q, hQ1, hQ2⟩ := Star.strip pres dia hg hs h2
    obtain ⟨M, hM1, hM2⟩ := ih (pres _ _ hg hs) hQ1
    refine ⟨M, hM1, ?_⟩
    rcases hQ2 with rfl | hq
    · exact hM2
    · exact .step hq hM2

/-- Exactly n R-steps. -/
inductive Path {α : Type} (R : α → α → Prop) : Nat → α → α → Prop
  | refl (a : α) : Path R 0 a a
  | step {n : Nat} {a b c : α} : R a b → Path R n b c → Path R (n + 1) a c

theorem Path.star {n : Nat} {a b : α} (h : Path R n a b) : Star R a b := by
  induction h with
  | refl => exact .refl _
  | step hs _ ih => exact .step hs ih

theorem Star.path {a b : α} (h : Star R a b) : ∃ n, Path R n a b := by
  induction h with
  | refl => exact ⟨0, .refl _⟩
  | step hs _ ih => obtain ⟨n, hp⟩ := ih; exact ⟨n + 1, .step hs hp⟩

/-- On the way to a state with no step, any step taken first is still on a way there, one shorter. -/
theorem Path.after (pres : ∀ a b, G a → R a b → G b) (dia : Diamond R G) {n : Nat} {a b : α}
    (hp : Path R n a b) (nb : ∀ d, ¬ R b d) : ∀ {c}, G a → R a c → ∃ m, n = m + 1 ∧ Path R m c b := by
  induction hp with
  | refl => intro c _ hs; exact absurd hs (nb c)
  | step h1 hr ih =>
    intro c hg hs
    rcases dia _ _ _ hg h1 hs with rfl | ⟨d, hd1, hd2⟩
    · exact ⟨_, rfl, hr⟩
    · obtain ⟨m, rfl, hm⟩ := ih nb (pres _ _ hg h1) hd1
      exact ⟨m + 1, rfl, .step hd2 hm⟩

/-- **Every run has the same length**: if some run of n steps reaches a state with no step, every
run is at most n steps long and can be continued to that state in exactly n steps in all. -/
theorem Path.bound (pres : ∀ a b, G a → R a b → G b) (dia : Diamond R G) {m : Nat} {a c : α}
    (hq : Path R m a c) : ∀ {n b}, G a → Path R n a b → (∀ d, ¬ R b d) → m ≤ n ∧ Path R (n - m) c b := by
  induction hq with
  | refl => intro n b _ hp _; exact ⟨Nat.zero_le _, by simpa using hp⟩
  | step hs hr ih =>
    intro n b hg hp nb
    obtain ⟨k, rfl, hk⟩ := Path.after pres dia hp nb hg hs
    obtain ⟨hle, hrest⟩ := ih (pres _ _ hg hs) hk nb
    exact ⟨by omega, by simpa [Nat.add_sub_add_right] using hrest⟩

theorem Star.stays {a b : α} (hn : ∀ c, ¬ R a c) (h : Star R a b) : b = a := by
  cases h with
  | refl => rfl
  | step hs _ => exact absurd hs (hn _)

/-- Two runs from a state where G holds that both reach a state with no step end in the same state. -/
theorem Star.unique (pres : ∀ a b, G a → R a b → G b) (dia : Diamond R G) {a b c : α}
    (hg : G a) (h1 : Star R a b) (h2 : Star R a c) (nb : ∀ d, ¬ R b d) (nc : ∀ d, ¬ R c d) : b = c := by
  obtain ⟨d, hd1, hd2⟩ := Star.confluent pres dia hg h1 h2
  rw [← Star.stays nb hd1, Star.stays nc hd2]
end Abstract

section Runs
variable {K : Type} (S : Sig K) (T : List (GRule K))

/-- The table checks for whole runs: `TableOk`, plus every rule uses each wire end once. -/
structure RunOk : Prop extends TableOk S T where
  nodup : ∀ r ∈ T, (ends r.wires).Nodup

/-- The invariants a run keeps: wires join out to in, touch only agents that are there, are
symmetric, and no agent's name is part of another's. -/
structure Net.Good (N : Net K) : Prop where
  polarized : N.Polarized S
  present : N.Present
  involutive : N.Involutive
  tidy : N.Tidy

/-- A net built with root names only (as a compiler builds one) is good if it is polarized,
present and symmetric. -/
theorem good_of_roots {N : Net K} (hp : N.Polarized S) (hP : N.Present) (hi : N.Involutive)
    (hr : ∀ x, N.kind x ≠ none → ∃ n, x = .root n) : N.Good S :=
  ⟨hp, hP, hi, tidy_of_roots hr⟩

/-- Runs of firing steps. -/
abbrev Reach : Net K → Net K → Prop := Star (Step S T)

/-- A net no step applies to. -/
def Normal (N : Net K) : Prop := ∀ M, ¬ Step S T N M

variable {S T}

/-- A step keeps a good net good. -/
theorem RunOk.step_good (h : RunOk S T) {N N' : Net K} (hg : N.Good S) (hs : Step S T N N') : N'.Good S := by
  obtain ⟨a, b, r, hr, _, hka, hkb, rfl⟩ := hs
  have pa : N.kind a ≠ none := by simp [hka]
  have pb : N.kind b ≠ none := by simp [hkb]
  have hab : a ≠ b := by
    intro e; subst e; rw [hka] at hkb
    exact h.principals r hr (by simp at hkb; rw [hkb])
  have fresh : ∀ k, N.kind (.made a b k) = none := fun k => hg.tidy.fresh a b k pa pb
  have ok : StepOk S r a b N := ⟨hab, hg.present, fresh, h.madeOk r hr, h.nodup r hr⟩
  exact ⟨fire_polarized ⟨hka, hkb, h.polarized r hr, h.madeOk r hr, fresh, hg.present⟩ hg.polarized,
    fire_present ok, fire_involutive ok hg.involutive, fire_tidy S r a b N hg.tidy pa pb⟩

theorem RunOk.reach_good (h : RunOk S T) {N M : Net K} (hg : N.Good S) (hr : Reach S T N M) : M.Good S :=
  Star.keeps (G := Net.Good S) (fun _ _ hg hs => h.step_good hg hs) hg hr

theorem RunOk.diamond (h : RunOk S T) : Diamond (Step S T) (Net.Good S) := fun _ _ _ hg h1 h2 =>
  strong_confluence h.deterministic h.principals hg.involutive hg.tidy.fresh h1 h2

/-- **Runs are confluent**: any two runs from a good net can be continued to a common net. -/
theorem RunOk.confluent_runs (h : RunOk S T) {N N1 N2 : Net K} (hg : N.Good S)
    (h1 : Reach S T N N1) (h2 : Reach S T N N2) : ∃ M, Reach S T N1 M ∧ Reach S T N2 M :=
  Star.confluent (G := Net.Good S) (fun _ _ hg hs => h.step_good hg hs) h.diamond hg h1 h2

/-- **The result doesn't depend on the order**: two runs from a good net that both finish end in
the same net, names included. -/
theorem RunOk.unique_result (h : RunOk S T) {N N1 N2 : Net K} (hg : N.Good S)
    (h1 : Reach S T N N1) (h2 : Reach S T N N2) (n1 : Normal S T N1) (n2 : Normal S T N2) : N1 = N2 :=
  Star.unique (G := Net.Good S) (fun _ _ hg hs => h.step_good hg hs) h.diamond hg h1 h2 n1 n2
end Runs

/-! ## Removing a forwarder keeps a net good -/

section Unfwd
variable {K : Type}

/-- Where a wire that led into forwarder x leads once x is gone. -/
def via (x : Ident) (N : Net K) : Option Port → Option Port
  | some q =>
    if q.1 = x then
      match (if q.2 = 0 then N.mate (x, 1) else if q.2 = 1 then N.mate (x, 0) else none) with
      | some t => if t.1 = x then none else some t
      | none => none
    else some q
  | none => none

theorem unfwd_mate (x : Ident) (N : Net K) (p : Port) :
    (unfwd x N).mate p = if p.1 = x then none else via x N (N.mate p) := by
  simp only [unfwd]
  split
  · rfl
  · cases N.mate p <;> rfl

theorem unfwd_kind (x : Ident) (N : Net K) (y : Ident) : (unfwd x N).kind y = if y = x then none else N.kind y := rfl

theorem via_keep {x : Ident} {N : Net K} {q : Port} (h : q.1 ≠ x) : via x N (some q) = some q := by
  simp [via, h]

/-- The result of `via` never lands on x, and lands on a port that some wire of x led to. -/
theorem via_some {x : Ident} {N : Net K} {q t : Port} (h : via x N (some q) = some t) :
    t.1 ≠ x ∧ (t = q ∨ (q.1 = x ∧ ((q.2 = 0 ∧ N.mate (x, 1) = some t) ∨ (q.2 = 1 ∧ N.mate (x, 0) = some t)))) := by
  simp only [via] at h
  by_cases hq : q.1 = x
  · rw [if_pos hq] at h
    by_cases h0 : q.2 = 0
    · rw [if_pos h0] at h
      cases hm : N.mate (x, 1) with
      | none => simp [hm] at h
      | some u =>
        rw [hm] at h; by_cases hu : u.1 = x
        · simp [hu] at h
        · simp [hu] at h; subst h; exact ⟨hu, Or.inr ⟨hq, Or.inl ⟨h0, rfl⟩⟩⟩
    · rw [if_neg h0] at h
      by_cases h1 : q.2 = 1
      · rw [if_pos h1] at h
        cases hm : N.mate (x, 0) with
        | none => simp [hm] at h
        | some u =>
          rw [hm] at h; by_cases hu : u.1 = x
          · simp [hu] at h
          · simp [hu] at h; subst h; exact ⟨hu, Or.inr ⟨hq, Or.inr ⟨h1, rfl⟩⟩⟩
      · simp [h1] at h
  · rw [if_neg hq] at h; simp at h; subst h; exact ⟨hq, Or.inl rfl⟩

theorem unfwd_present {x : Ident} {N : Net K} (hP : N.Present) : (unfwd x N).Present := by
  intro p q h
  rw [unfwd_mate] at h
  by_cases hp : p.1 = x
  · simp [hp] at h
  rw [if_neg hp] at h
  cases hm : N.mate p with
  | none => simp [hm, via] at h
  | some q0 =>
    rw [hm] at h
    obtain ⟨hqx, hq⟩ := via_some h
    refine ⟨by rw [unfwd_kind, if_neg hp]; exact (hP _ _ hm).1, ?_⟩
    rw [unfwd_kind, if_neg hqx]
    rcases hq with rfl | ⟨_, ⟨_, ht⟩ | ⟨_, ht⟩⟩
    · exact (hP _ _ hm).2
    · exact (hP _ _ ht).2
    · exact (hP _ _ ht).2

theorem unfwd_involutive {x : Ident} {N : Net K} (hinv : N.Involutive) : (unfwd x N).Involutive := by
  intro p q h
  rw [unfwd_mate] at h
  by_cases hp : p.1 = x
  · simp [hp] at h
  rw [if_neg hp] at h
  cases hm : N.mate p with
  | none => simp [hm, via] at h
  | some q0 =>
    rw [hm] at h
    obtain ⟨hqx, hq⟩ := via_some h
    rw [unfwd_mate, if_neg hqx]
    rcases hq with rfl | ⟨hq0x, ⟨h0, ht⟩ | ⟨h1, ht⟩⟩
    · rw [hinv _ _ hm]; exact via_keep hp
    · -- p → (x, 0), and (x, 1) → q
      have back := hinv _ _ ht
      have hp0 : N.mate (x, 0) = some p := by
        obtain ⟨y, m⟩ := q0; simp at hq0x h0; subst hq0x; subst h0; exact hinv _ _ hm
      rw [back]; simp [via, hp0, hp]
    · have back := hinv _ _ ht
      have hp1 : N.mate (x, 1) = some p := by
        obtain ⟨y, m⟩ := q0; simp at hq0x h1; subst hq0x; subst h1; exact hinv _ _ hm
      rw [back]; simp [via, hp1, hp]

theorem unfwd_tidy {x : Ident} {N : Net K} (h : N.Tidy) : (unfwd x N).Tidy := by
  intro y z hy hz
  rw [unfwd_kind] at hy hz
  by_cases hyx : y = x
  · simp [hyx] at hy
  by_cases hzx : z = x
  · simp [hzx] at hz
  rw [if_neg hyx] at hy; rw [if_neg hzx] at hz
  exact h y z hy hz
end Unfwd
