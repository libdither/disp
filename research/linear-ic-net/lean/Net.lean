import Basic

/-! Nets and interaction steps, for any agent kinds and any rule table, and theorems about
reduction that hold for every table passing the table checks (`Tables.lean`).

Two choices keep a step local. Agents a step makes are named after the pair it replaced
(`made a b k`), so names never clash. And when a rule joins two of the pair's outside neighbours
directly, the step puts a forwarder between them, a two-port stand-in for the wire, so every
outside neighbour is rewired by one hop. A separate step removes forwarders. -/

structure Sig (K : Type) where
  ports : K → List Pol

namespace Sig
variable {K : Type}
def pol (S : Sig K) (k : K) (n : Nat) : Pol := (S.ports k).getD n .i
end Sig

inductive Ident
  | root (n : Nat)
  | made (a b : Ident) (k : Nat)
  deriving DecidableEq, Repr

abbrev Port := Ident × Nat

inductive Ag (K : Type)
  | base (k : K)
  | fwd (p : Pol)

def Ag.pol {K : Type} (S : Sig K) : Ag K → Nat → Pol
  | .base k, n => S.pol k n
  | .fwd p, n => if n = 0 then p else p.flip

structure GRule (K : Type) where
  a : K
  b : K
  made : List K
  wires : List (End × End)

structure Net (K : Type) where
  kind : Ident → Option (Ag K)
  mate : Port → Option Port

section
variable {K : Type} (S : Sig K)

def Net.pol (N : Net K) (p : Port) : Pol :=
  match N.kind p.1 with
  | some g => g.pol S p.2
  | none => .i

/-- Every wire joins an out port to an in port. -/
def Net.Polarized (N : Net K) : Prop := ∀ p q, N.mate p = some q → N.pol S p ≠ N.pol S q

/-- Wires only touch agents that are there. -/
def Net.Present (N : Net K) : Prop := ∀ p q, N.mate p = some q → N.kind p.1 ≠ none ∧ N.kind q.1 ≠ none

def GRule.polEnd (r : GRule K) : End → Pol
  | .left n => S.pol r.a n
  | .right n => S.pol r.b n
  | .new j n => match r.made[j]? with | some k => S.pol k n | none => .i

/-- The table check: a made port has its slot's polarity, joined made ports or joined slots differ. -/
def GRule.wireOk (r : GRule K) (w : End × End) : Bool :=
  if w.1.isSlot == w.2.isSlot then r.polEnd S w.1 != r.polEnd S w.2 else r.polEnd S w.1 == r.polEnd S w.2

def GRule.Polarized (r : GRule K) : Prop := ∀ w ∈ r.wires, r.wireOk S w = true

/-- An end names a port of the pair, or of an agent the rule makes. -/
def GRule.EndOk (r : GRule K) : End → Prop
  | .new j _ => j < r.made.length
  | _ => True

instance (r : GRule K) (e : End) : Decidable (r.EndOk e) := by
  cases e <;> simp only [GRule.EndOk] <;> infer_instance

/-- Every agent a wire mentions is one the rule makes. -/
def GRule.MadeOk (r : GRule K) : Prop := ∀ w ∈ r.wires, r.EndOk w.1 ∧ r.EndOk w.2
end

/-- The wire holding end e: its index (counting from i), the other end, and whether e comes first. -/
def findEnd (e : End) : List (End × End) → Nat → Option (Nat × End × Bool)
  | [], _ => none
  | (x, y) :: ws, i =>
    if x = e then some (i, y, true) else if y = e then some (i, x, false) else findEnd e ws (i + 1)

theorem findEnd_spec (e : End) : ∀ (ws : List (End × End)) (i w : Nat) (f : End) (first : Bool),
    findEnd e ws i = some (w, f, first) →
      i ≤ w ∧ ws[w - i]? = some (if first then (e, f) else (f, e)) := by
  intro ws
  induction ws with
  | nil => intro i w f first h; simp [findEnd] at h
  | cons x ws ih =>
    intro i w f first h
    obtain ⟨x1, x2⟩ := x
    simp only [findEnd] at h
    split at h
    · simp at h; obtain ⟨rfl, rfl, rfl⟩ := h; subst_vars; simp
    · split at h
      · simp at h; obtain ⟨rfl, rfl, rfl⟩ := h; subst_vars; simp
      · have ⟨h1, h2⟩ := ih (i + 1) w f first h
        refine ⟨by omega, ?_⟩
        have : w - i = (w - (i + 1)) + 1 := by omega
        rw [this, List.getElem?_cons_succ]
        exact h2

theorem findEnd_mem {e : End} {ws : List (End × End)} {w : Nat} {f : End} {first : Bool}
    (h : findEnd e ws 0 = some (w, f, first)) :
    (if first then (e, f) else (f, e)) ∈ ws ∧ ws[w]? = some (if first then (e, f) else (f, e)) := by
  have ⟨_, h2⟩ := findEnd_spec e ws 0 w f first h
  simp only [Nat.sub_zero] at h2
  exact ⟨List.mem_of_getElem? h2, h2⟩

section Fire
variable {K : Type} (S : Sig K) (r : GRule K) (a b : Ident)

/-- The port of the pair a slot names. -/
def slotPort : End → Port
  | .left n => (a, n)
  | .right n => (b, n)
  | .new j n => (.made a b j, n)

/-- The port that stands in for a slot after the step: the made port the rule joins it to, or
the forwarder's port when the rule joins it to another slot. -/
def serve (e : End) : Option Port :=
  match findEnd e r.wires 0 with
  | some (_, .new j n, _) => some (.made a b j, n)
  | some (w, _, first) => some (.made a b (r.made.length + w), if first then 0 else 1)
  | none => none

/-- Where a port connects after the step, given where it connected before: ports of the pair
are replaced by what stands in for them; anything else stays. -/
def relink (q : Option Port) : Option Port :=
  match q with
  | some q => if q.1 = a then serve r a b (.left q.2) else if q.1 = b then serve r a b (.right q.2) else some q
  | none => none

/-- What the other end of a made agent's wire connects to. -/
def partner (N : Net K) (e : End) : Option Port :=
  match findEnd e r.wires 0 with
  | some (_, .new j n, _) => some (.made a b j, n)
  | some (_, f, _) => relink r a b (N.mate (slotPort a b f))
  | none => none

/-- The kind of the k-th agent a step makes: one the rule makes, then one forwarder per wire
that joins two slots. -/
def madeKind (k : Nat) : Option (Ag K) :=
  if k < r.made.length then (r.made[k]?).map .base
  else match r.wires[k - r.made.length]? with
    | some (e1, e2) => if e1.isSlot && e2.isSlot then some (.fwd (r.polEnd S e1)) else none
    | none => none

def madeMate (N : Net K) (k n : Nat) : Option Port :=
  if k < r.made.length then partner r a b N (.new k n)
  else match r.wires[k - r.made.length]? with
    | some (e1, e2) =>
      if e1.isSlot && e2.isSlot then
        if n = 0 then relink r a b (N.mate (slotPort a b e1))
        else if n = 1 then relink r a b (N.mate (slotPort a b e2)) else none
      else none
    | none => none

/-- Is x an agent this step makes, and which one. -/
def madeIdx : Ident → Option Nat
  | .made a' b' k => if a' = a ∧ b' = b then some k else none
  | .root _ => none

/-- One interaction: a and b go; what the rule makes, and the forwarders, come. -/
def fire (N : Net K) : Net K where
  kind x :=
    if x = a ∨ x = b then none else
    match madeIdx a b x with
    | some k => madeKind S r k
    | none => N.kind x
  mate p :=
    if p.1 = a ∨ p.1 = b then none else
    match madeIdx a b p.1 with
    | some k => madeMate r a b N k p.2
    | none => relink r a b (N.mate p)
end Fire

/-! ## Basic facts about names -/

theorem Ident.made_ne_left (a b : Ident) (k : Nat) : Ident.made a b k ≠ a := by
  intro h; have := congrArg sizeOf h; simp at this; omega
theorem Ident.made_ne_right (a b : Ident) (k : Nat) : Ident.made a b k ≠ b := by
  intro h; have := congrArg sizeOf h; simp at this; omega

theorem madeIdx_some {a b x : Ident} {k : Nat} (h : madeIdx a b x = some k) : x = .made a b k := by
  cases x with
  | root n => simp [madeIdx] at h
  | made a' b' k' => simp only [madeIdx] at h; split at h <;> simp_all

theorem madeIdx_made (a b : Ident) (k : Nat) : madeIdx a b (.made a b k) = some k := by simp [madeIdx]

theorem Pol.flip_of_ne {x y : Pol} (h : x ≠ y) : x.flip = y := by
  cases x <;> cases y <;> simp_all [Pol.flip]

/-- A forwarder's port n has its first slot's polarity at port 0, the other at port 1. -/
def fwdPolAt {K : Type} (S : Sig K) (r : GRule K) (e1 : End) (n : Nat) : Pol :=
  if n = 0 then r.polEnd S e1 else (r.polEnd S e1).flip

/-! ## A step keeps the net polarized -/

section Polarity
variable {K : Type} (S : Sig K) (r : GRule K) (a b : Ident) (N : Net K)

theorem fire_kind_made (k : Nat) : (fire S r a b N).kind (.made a b k) = madeKind S r k := by
  simp [fire, Ident.made_ne_left, Ident.made_ne_right, madeIdx_made]

theorem fire_kind_other {x : Ident} (hx : ¬(x = a ∨ x = b)) (hm : madeIdx a b x = none) :
    (fire S r a b N).kind x = N.kind x := by
  simp [fire, hx, hm]

/-- The hypotheses: the pair's kinds match the rule, the rule passes the table checks, and the
names the step makes are free. -/
structure FireOk : Prop where
  ka : N.kind a = some (.base r.a)
  kb : N.kind b = some (.base r.b)
  pol : r.Polarized S
  madeOk : r.MadeOk
  fresh : ∀ k, N.kind (.made a b k) = none
  present : N.Present

variable {S r a b N}

theorem wire_mixed (h : FireOk S r a b N) {w : End × End} (hw : w ∈ r.wires)
    (h12 : w.1.isSlot ≠ w.2.isSlot) : r.polEnd S w.1 = r.polEnd S w.2 := by
  have := h.pol w hw
  rw [GRule.wireOk, if_neg (by simpa using h12)] at this
  simpa using this

theorem wire_same (h : FireOk S r a b N) {w : End × End} (hw : w ∈ r.wires)
    (hs : w.1.isSlot = w.2.isSlot) : r.polEnd S w.1 ≠ r.polEnd S w.2 := by
  have := h.pol w hw
  rw [GRule.wireOk, if_pos (by simpa using hs)] at this
  simpa using this

theorem slot_pol (h : FireOk S r a b N) {e : End} (he : e.isSlot = true) :
    N.pol S (slotPort a b e) = r.polEnd S e := by
  cases e with
  | left n => simp [slotPort, Net.pol, h.ka, Ag.pol, GRule.polEnd]
  | right n => simp [slotPort, Net.pol, h.kb, Ag.pol, GRule.polEnd]
  | new j n => simp [End.isSlot] at he

theorem made_pol (h : FireOk S r a b N) {j : Nat} (hj : j < r.made.length) (n : Nat) :
    (fire S r a b N).pol S (.made a b j, n) = r.polEnd S (.new j n) := by
  simp only [Net.pol, fire_kind_made, madeKind, hj, ite_true, GRule.polEnd]
  cases hk : r.made[j]? with
  | none => simp at hk; omega
  | some k => simp [Ag.pol]

theorem fwd_pol (h : FireOk S r a b N) {w : Nat} {e1 e2 : End} (hget : r.wires[w]? = some (e1, e2))
    (h1 : e1.isSlot = true) (h2 : e2.isSlot = true) (n : Nat) :
    (fire S r a b N).pol S (.made a b (r.made.length + w), n) = (fwdPolAt S r e1 n) := by
  have : ¬ (r.made.length + w < r.made.length) := by omega
  simp [Net.pol, fire_kind_made, madeKind, this, hget, h1, h2, Ag.pol, fwdPolAt]

/-- What stands in for a slot has the slot's polarity. -/
theorem serve_pol (h : FireOk S r a b N) {e : End} (he : e.isSlot = true) {q : Port}
    (hq : serve r a b e = some q) : (fire S r a b N).pol S q = r.polEnd S e := by
  unfold serve at hq
  cases hf : findEnd e r.wires 0 with
  | none => simp [hf] at hq
  | some t =>
    obtain ⟨w, f, first⟩ := t
    have ⟨hmem, hget⟩ := findEnd_mem hf
    rw [hf] at hq
    cases first
    · simp only [Bool.false_eq_true, ite_false] at hmem hget
      cases f with
      | new j n =>
        simp at hq; subst hq
        rw [made_pol h ((h.madeOk _ hmem).1)]
        exact wire_mixed h hmem (by show (End.new j n).isSlot ≠ e.isSlot; rw [he]; simp [End.isSlot])
      | left m | right m =>
        simp at hq; subst hq
        rw [fwd_pol h hget (by rfl) he]
        simp only [fwdPolAt, Nat.one_ne_zero, ite_false]
        exact Pol.flip_of_ne (wire_same h hmem (by show End.isSlot _ = e.isSlot; rw [he]; rfl))
    · simp only [ite_true] at hmem hget
      cases f with
      | new j n =>
        simp at hq; subst hq
        rw [made_pol h ((h.madeOk _ hmem).2)]
        exact (wire_mixed h hmem (by show e.isSlot ≠ (End.new j n).isSlot; rw [he]; simp [End.isSlot])).symm
      | left m | right m =>
        simp at hq; subst hq
        rw [fwd_pol h hget he (by rfl)]
        simp [fwdPolAt]

theorem madeIdx_none_of_present (h : FireOk S r a b N) {x : Ident} (hx : N.kind x ≠ none) :
    madeIdx a b x = none := by
  cases hk : madeIdx a b x with
  | none => rfl
  | some k => rw [madeIdx_some hk] at hx; exact absurd (h.fresh k) hx

/-- A port that connected to q0 before the step connects to something with q0's polarity after. -/
theorem relink_pol (h : FireOk S r a b N) {p q0 q : Port} (hm : N.mate p = some q0)
    (hq : relink r a b (some q0) = some q) : (fire S r a b N).pol S q = N.pol S q0 := by
  obtain ⟨x, n⟩ := q0
  simp only [relink] at hq
  by_cases hxa : x = a
  · subst hxa; rw [if_pos rfl] at hq
    rw [serve_pol h rfl hq]; simp [Net.pol, h.ka, Ag.pol, GRule.polEnd]
  · rw [if_neg hxa] at hq
    by_cases hxb : x = b
    · subst hxb; rw [if_pos rfl] at hq
      rw [serve_pol h rfl hq]; simp [Net.pol, h.kb, Ag.pol, GRule.polEnd]
    · rw [if_neg hxb] at hq
      simp at hq; subst hq
      have hx : N.kind x ≠ none := (h.present _ _ hm).2
      simp [Net.pol, fire_kind_other S r a b N (by simp [hxa, hxb]) (madeIdx_none_of_present h hx)]

/-- **A step keeps the net polarized**, for any rule that passes the table's polarity check. -/
theorem fire_polarized (h : FireOk S r a b N) (hN : N.Polarized S) : (fire S r a b N).Polarized S := by
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
      rw [relink_pol h hm hpq]
      have : (fire S r a b N).pol S (x, n) = N.pol S (x, n) := by
        simp [Net.pol, fire_kind_other S r a b N hab hk]
      rw [this]; exact hN _ _ hm
  | some k =>
    have hx := madeIdx_some hk; subst hx
    rw [hk] at hpq
    simp only [madeMate] at hpq
    by_cases hlt : k < r.made.length
    · rw [if_pos hlt] at hpq
      rw [made_pol h hlt n]
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
          | some q0 =>
            rw [hm] at hpq'
            rw [relink_pol h hm hpq']
            have hs := hN _ _ hm
            rw [slot_pol h hfs] at hs
            cases first
            · simp only [Bool.false_eq_true, ite_false] at hmem
              rw [← wire_mixed h hmem (by show f.isSlot ≠ (End.new k n).isSlot; rw [hfs]; simp [End.isSlot])]; exact hs
            · simp only [ite_true] at hmem
              rw [wire_mixed h hmem (by show (End.new k n).isSlot ≠ f.isSlot; rw [hfs]; simp [End.isSlot])]; exact hs
        · cases f with
          | left m => simp [End.isSlot] at hfs
          | right m => simp [End.isSlot] at hfs
          | new j m =>
            simp at hpq; subst hpq
            cases first
            · simp only [Bool.false_eq_true, ite_false] at hmem
              rw [made_pol h ((h.madeOk _ hmem).1)]
              exact (wire_same h hmem rfl).symm
            · simp only [ite_true] at hmem
              rw [made_pol h ((h.madeOk _ hmem).2)]
              exact wire_same h hmem rfl
    · rw [if_neg hlt] at hpq
      have hk' : k = r.made.length + (k - r.made.length) := by omega
      cases hw : r.wires[k - r.made.length]? with
      | none => simp [hw] at hpq
      | some e12 =>
        obtain ⟨e1, e2⟩ := e12
        rw [hw] at hpq
        have hmem : (e1, e2) ∈ r.wires := List.mem_of_getElem? hw
        by_cases hs : e1.isSlot = true ∧ e2.isSlot = true
        · simp only [hs.1, hs.2, Bool.and_self, ite_true] at hpq
          rw [hk', fwd_pol h hw hs.1 hs.2 n]
          by_cases hn0 : n = 0
          · subst hn0
            simp only [ite_true] at hpq
            cases hm : N.mate (slotPort a b e1) with
            | none => simp [relink, hm] at hpq
            | some q0 =>
              rw [hm] at hpq
              rw [relink_pol h hm hpq]
              have := hN _ _ hm; rw [slot_pol h hs.1] at this
              simpa [fwdPolAt] using this
          · rw [if_neg hn0] at hpq
            by_cases hn1 : n = 1
            · subst hn1
              simp only [ite_true] at hpq
              cases hm : N.mate (slotPort a b e2) with
              | none => simp [relink, hm] at hpq
              | some q0 =>
                rw [hm] at hpq
                rw [relink_pol h hm hpq]
                have := hN _ _ hm; rw [slot_pol h hs.2] at this
                have hflip := Pol.flip_of_ne (wire_same h hmem (by simp [hs.1, hs.2]))
                simpa [fwdPolAt, hflip] using this
            · rw [if_neg hn1] at hpq; cases hpq
        · have : (e1.isSlot && e2.isSlot) = false := by
            revert hs; cases e1.isSlot <;> cases e2.isSlot <;> simp
          simp [this] at hpq
end Polarity

theorem Pol.flip_ne (x : Pol) : x.flip ≠ x := by cases x <;> simp [Pol.flip]
theorem Pol.flip_flip (x : Pol) : x.flip.flip = x := by cases x <;> rfl

/-! ## Removing a forwarder keeps the net polarized -/

/-- Remove forwarder x, joining its two neighbours. -/
def unfwd {K : Type} (x : Ident) (N : Net K) : Net K where
  kind y := if y = x then none else N.kind y
  mate p :=
    if p.1 = x then none else
    match N.mate p with
    | some q =>
      if q.1 = x then
        match (if q.2 = 0 then N.mate (x, 1) else if q.2 = 1 then N.mate (x, 0) else none) with
        | some t => if t.1 = x then none else some t
        | none => none
      else some q
    | none => none

theorem unfwd_polarized {K : Type} (S : Sig K) (N : Net K) (x : Ident) (P : Pol)
    (hx : N.kind x = some (.fwd P)) (hN : N.Polarized S) : (unfwd x N).Polarized S := by
  intro p q hpq
  have keep : ∀ y : Port, y.1 ≠ x → (unfwd x N).pol S y = N.pol S y := by
    intro y hy; simp [Net.pol, unfwd, hy]
  simp only [unfwd] at hpq
  by_cases hpx : p.1 = x
  · simp [hpx] at hpq
  rw [if_neg hpx] at hpq
  cases hm : N.mate p with
  | none => simp [hm] at hpq
  | some q0 =>
    rw [hm] at hpq
    dsimp only at hpq
    have h0 := hN _ _ hm
    by_cases hq0 : q0.1 = x
    · rw [if_pos hq0] at hpq
      have hpolq0 : N.pol S q0 = if q0.2 = 0 then P else P.flip := by
        obtain ⟨y, m⟩ := q0; simp at hq0; subst hq0; simp [Net.pol, hx, Ag.pol]
      by_cases hz : q0.2 = 0
      · rw [if_pos hz] at hpq
        cases ht : N.mate (x, 1) with
        | none => simp [ht] at hpq
        | some t =>
          rw [ht] at hpq
          by_cases htx : t.1 = x
          · simp [htx] at hpq
          simp [htx] at hpq; subst hpq
          have h1 := hN _ _ ht
          rw [show N.pol S (x, 1) = P.flip by simp [Net.pol, hx, Ag.pol]] at h1
          rw [hpolq0, if_pos hz] at h0
          rw [keep p hpx, keep _ htx]
          revert h0 h1
          generalize N.pol S p = u; generalize N.pol S t = v
          cases u <;> cases v <;> cases P <;> simp [Pol.flip]
      · rw [if_neg hz] at hpq
        by_cases h1z : q0.2 = 1
        · rw [if_pos h1z] at hpq
          cases ht : N.mate (x, 0) with
          | none => simp [ht] at hpq
          | some t =>
            rw [ht] at hpq
            by_cases htx : t.1 = x
            · simp [htx] at hpq
            simp [htx] at hpq; subst hpq
            have h1 := hN _ _ ht
            rw [show N.pol S (x, 0) = P by simp [Net.pol, hx, Ag.pol]] at h1
            rw [hpolq0, if_neg hz] at h0
            rw [keep p hpx, keep _ htx]
            revert h0 h1
            generalize N.pol S p = u; generalize N.pol S t = v
            cases u <;> cases v <;> cases P <;> simp [Pol.flip]
        · rw [if_neg h1z] at hpq; simp at hpq
    · rw [if_neg hq0] at hpq
      simp at hpq; subst hpq
      rw [keep p hpx, keep _ hq0]; exact h0

/-! ## Active pairs are disjoint -/

def Net.Involutive {K : Type} (N : Net K) : Prop := ∀ p q, N.mate p = some q → N.mate q = some p

/-- An agent's principal port has one mate, so it is in at most one active pair: two different
active pairs never share an agent, which is what lets them fire in either order. -/
theorem active_unique {K : Type} (N : Net K) {a b c : Ident}
    (h1 : N.mate (a, 0) = some (b, 0)) (h2 : N.mate (a, 0) = some (c, 0)) : b = c := by
  rw [h1] at h2; simpa using h2

/-! ## Steps on different pairs commute -/

section Comm
variable {K : Type} (S : Sig K) (r r' : GRule K) (a b c d : Ident) (N : Net K)

theorem fire_mate_pair {x : Ident} (hx : x = a ∨ x = b) (n : Nat) : (fire S r a b N).mate (x, n) = none := by
  simp [fire, hx]
theorem fire_mate_made (k n : Nat) : (fire S r a b N).mate (.made a b k, n) = madeMate r a b N k n := by
  simp [fire, Ident.made_ne_left, Ident.made_ne_right, madeIdx_made]
theorem fire_mate_other {x : Ident} (hx : ¬(x = a ∨ x = b)) (hm : madeIdx a b x = none) (n : Nat) :
    (fire S r a b N).mate (x, n) = relink r a b (N.mate (x, n)) := by
  simp [fire, hx, hm]
theorem fire_kind_pair {x : Ident} (hx : x = a ∨ x = b) : (fire S r a b N).kind x = none := by
  simp [fire, hx]

/-- Two pairs with no agent in common, where neither step's names could be an agent of the other. -/
structure Apart (a b c d : Ident) : Prop where
  ab : a ≠ b
  cd : c ≠ d
  ac : a ≠ c
  ad : a ≠ d
  bc : b ≠ c
  bd : b ≠ d
  ca : ∀ k, Ident.made c d k ≠ a
  cb : ∀ k, Ident.made c d k ≠ b
  ac' : ∀ k, Ident.made a b k ≠ c
  bd' : ∀ k, Ident.made a b k ≠ d

theorem Apart.symm {a b c d : Ident} (h : Apart a b c d) : Apart c d a b :=
  ⟨h.cd, h.ab, h.ac.symm, h.bc.symm, h.ad.symm, h.bd.symm, h.ac', h.bd', h.ca, h.cb⟩

variable {a b c d}

theorem madeIdx_none_of_ne {x : Ident} (hx : ∀ k, Ident.made a b k ≠ x) : madeIdx a b x = none := by
  cases hk : madeIdx a b x with
  | none => rfl
  | some k => exact absurd (madeIdx_some hk).symm (hx k)

theorem madeIdx_other_made (h : a ≠ c) (k : Nat) : madeIdx c d (.made a b k) = none := by
  simp [madeIdx, h]

theorem serve_made {e : End} {q : Port} (hq : serve r a b e = some q) : ∃ k, q.1 = .made a b k := by
  unfold serve at hq
  split at hq <;> simp at hq <;> subst hq <;> exact ⟨_, rfl⟩

theorem relink_keep {q : Port} (ha : q.1 ≠ a) (hb : q.1 ≠ b) : relink r a b (some q) = some q := by
  simp [relink, ha, hb]

theorem relink_serve (h : Apart a b c d) (e : End) : relink r' c d (serve r a b e) = serve r a b e := by
  cases hs : serve r a b e with
  | none => rfl
  | some q =>
    obtain ⟨k, hk⟩ := serve_made r hs
    exact relink_keep r' (by rw [hk]; exact h.ac' k) (by rw [hk]; exact h.bd' k)

/-- Rewiring by one hop for two disjoint steps commutes. -/
theorem relink_comm (h : Apart a b c d) (x : Option Port) :
    relink r' c d (relink r a b x) = relink r a b (relink r' c d x) := by
  cases x with
  | none => rfl
  | some q =>
    obtain ⟨y, n⟩ := q
    by_cases hya : y = a
    · rw [relink_keep r' (a := c) (b := d) (q := (y, n)) (by rw [hya]; exact h.ac) (by rw [hya]; exact h.ad),
        show relink r a b (some (y, n)) = serve r a b (.left n) by simp [relink, hya]]
      exact relink_serve r r' h _
    by_cases hyb : y = b
    · rw [relink_keep r' (a := c) (b := d) (q := (y, n)) (by rw [hyb]; exact h.bc) (by rw [hyb]; exact h.bd),
        show relink r a b (some (y, n)) = serve r a b (.right n) by simp [relink, hyb, Ne.symm h.ab]]
      exact relink_serve r r' h _
    by_cases hyc : y = c
    · rw [relink_keep r (q := (y, n)) (by rw [hyc]; exact Ne.symm h.ac) (by rw [hyc]; exact Ne.symm h.bc),
        show relink r' c d (some (y, n)) = serve r' c d (.left n) by simp [relink, hyc]]
      exact (relink_serve r' r h.symm _).symm
    by_cases hyd : y = d
    · rw [relink_keep r (q := (y, n)) (by rw [hyd]; exact Ne.symm h.ad) (by rw [hyd]; exact Ne.symm h.bd),
        show relink r' c d (some (y, n)) = serve r' c d (.right n) by simp [relink, hyd, Ne.symm h.cd]]
      exact (relink_serve r' r h.symm _).symm
    rw [relink_keep r (q := (y, n)) hya hyb, relink_keep r' (a := c) (b := d) (q := (y, n)) hyc hyd,
      relink_keep r (q := (y, n)) hya hyb]

theorem mate_slot_after (h : Apart a b c d) {e : End} (he : e.isSlot = true) :
    (fire S r' c d N).mate (slotPort a b e) = relink r' c d (N.mate (slotPort a b e)) := by
  cases e with
  | left m =>
    exact fire_mate_other S r' c d N (by simp [h.ac, h.ad]) (madeIdx_none_of_ne h.ca) m
  | right m =>
    exact fire_mate_other S r' c d N (by simp [h.bc, h.bd]) (madeIdx_none_of_ne h.cb) m
  | new j n => simp [End.isSlot] at he

/-- What a made agent's port connects to doesn't depend on whether the other step came first. -/
theorem madeMate_comm (h : Apart a b c d) (k n : Nat) :
    relink r' c d (madeMate r a b N k n) = madeMate r a b (fire S r' c d N) k n := by
  unfold madeMate
  by_cases hlt : k < r.made.length
  · simp only [if_pos hlt, partner]
    cases hf : findEnd (.new k n) r.wires 0 with
    | none => rfl
    | some t =>
      obtain ⟨w, f, first⟩ := t
      cases f with
      | new j m =>
        exact relink_keep r' (h.ac' j) (h.bd' j)
      | left m =>
        simp only
        rw [mate_slot_after S r' N h rfl, relink_comm r r' h]
      | right m =>
        simp only
        rw [mate_slot_after S r' N h rfl, relink_comm r r' h]
  · simp only [if_neg hlt]
    cases hw : r.wires[k - r.made.length]? with
    | none => rfl
    | some e12 =>
      obtain ⟨e1, e2⟩ := e12
      simp only
      by_cases hs : e1.isSlot = true ∧ e2.isSlot = true
      · simp only [hs.1, hs.2, Bool.and_self, ite_true]
        by_cases hn0 : n = 0
        · simp only [hn0, ite_true]
          rw [mate_slot_after S r' N h hs.1, relink_comm r r' h]
        · by_cases hn1 : n = 1
          · subst hn1
            simp only [Nat.one_ne_zero, ↓reduceIte]
            rw [mate_slot_after S r' N h hs.2, relink_comm r r' h]
          · simp [hn0, hn1, relink]
      · have : (e1.isSlot && e2.isSlot) = false := by
          revert hs; cases e1.isSlot <;> cases e2.isSlot <;> simp
        simp [this, relink]
theorem Net.ext' {N M : Net K} (hk : N.kind = M.kind) (hm : N.mate = M.mate) : N = M := by
  cases N; cases M; cases hk; cases hm; rfl

/-- **Steps on different pairs commute**: firing two disjoint pairs in either order gives the
same net, names included. With `active_unique`, this is strong confluence: every order of
firing reaches the same net in the same number of steps. -/
theorem fire_comm (h : Apart a b c d) :
    fire S r' c d (fire S r a b N) = fire S r a b (fire S r' c d N) := by
  apply Net.ext'
  · funext x
    by_cases hx : x = a ∨ x = b
    · have hx' : ¬(x = c ∨ x = d) := by rcases hx with rfl | rfl <;> simp [h.ac, h.ad, h.bc, h.bd]
      have hm : madeIdx c d x = none := by
        rcases hx with rfl | rfl
        · exact madeIdx_none_of_ne h.ca
        · exact madeIdx_none_of_ne h.cb
      rw [fire_kind_other S r' c d _ hx' hm, fire_kind_pair S r a b _ hx, fire_kind_pair S r a b _ hx]
    by_cases hy : x = c ∨ x = d
    · have hy' : ¬(x = a ∨ x = b) := hx
      have hm : madeIdx a b x = none := by
        rcases hy with rfl | rfl
        · exact madeIdx_none_of_ne h.ac'
        · exact madeIdx_none_of_ne h.bd'
      rw [fire_kind_pair S r' c d _ hy, fire_kind_other S r a b _ hy' hm, fire_kind_pair S r' c d _ hy]
    cases hk : madeIdx a b x with
    | some k =>
      have := madeIdx_some hk; subst this
      rw [fire_kind_other S r' c d _ hy (madeIdx_other_made h.ac k), fire_kind_made, fire_kind_made]
    | none =>
      cases hk' : madeIdx c d x with
      | some k =>
        have := madeIdx_some hk'; subst this
        rw [fire_kind_made, fire_kind_other S r a b _ hx (madeIdx_other_made (Ne.symm h.ac) k), fire_kind_made]
      | none =>
        rw [fire_kind_other S r' c d _ hy hk', fire_kind_other S r a b _ hx hk,
          fire_kind_other S r a b _ hx hk, fire_kind_other S r' c d _ hy hk']
  · funext p
    obtain ⟨x, n⟩ := p
    by_cases hx : x = a ∨ x = b
    · have hx' : ¬(x = c ∨ x = d) := by rcases hx with rfl | rfl <;> simp [h.ac, h.ad, h.bc, h.bd]
      have hm : madeIdx c d x = none := by
        rcases hx with rfl | rfl
        · exact madeIdx_none_of_ne h.ca
        · exact madeIdx_none_of_ne h.cb
      rw [fire_mate_other S r' c d _ hx' hm, fire_mate_pair S r a b _ hx, fire_mate_pair S r a b _ hx]
      rfl
    by_cases hy : x = c ∨ x = d
    · have hm : madeIdx a b x = none := by
        rcases hy with rfl | rfl
        · exact madeIdx_none_of_ne h.ac'
        · exact madeIdx_none_of_ne h.bd'
      rw [fire_mate_pair S r' c d _ hy, fire_mate_other S r a b _ hx hm, fire_mate_pair S r' c d _ hy]
      rfl
    cases hk : madeIdx a b x with
    | some k =>
      have := madeIdx_some hk; subst this
      rw [fire_mate_other S r' c d _ hy (madeIdx_other_made h.ac k), fire_mate_made, fire_mate_made]
      exact madeMate_comm S r r' N h k n
    | none =>
      cases hk' : madeIdx c d x with
      | some k =>
        have := madeIdx_some hk'; subst this
        rw [fire_mate_made, fire_mate_other S r a b _ hx (madeIdx_other_made (Ne.symm h.ac) k), fire_mate_made]
        exact (madeMate_comm S r' r N h.symm k n).symm
      | none =>
        rw [fire_mate_other S r' c d _ hy hk', fire_mate_other S r a b _ hx hk,
          fire_mate_other S r a b _ hx hk, fire_mate_other S r' c d _ hy hk']
        exact relink_comm r r' h _
end Comm

/-! ## Strong confluence -/

section Confluence
variable {K : Type} (S : Sig K) (T : List (GRule K))

/-- One step: some active pair whose kinds a rule of the table matches fires. -/
def Step (N N' : Net K) : Prop :=
  ∃ a b r, r ∈ T ∧ N.mate (a, 0) = some (b, 0) ∧ N.kind a = some (.base r.a) ∧
    N.kind b = some (.base r.b) ∧ N' = fire S r a b N

/-- At most one rule for a pair of kinds, either way round (the table's determinism check). -/
def Deterministic : Prop :=
  ∀ r ∈ T, ∀ r' ∈ T, ((r.a = r'.a ∧ r.b = r'.b) ∨ (r.a = r'.b ∧ r.b = r'.a)) → r = r'

/-- Every rule pairs an in-facing principal port with an out-facing one (the table's polarity check). -/
def PrincipalsOpposite : Prop := ∀ r ∈ T, S.pol r.a 0 ≠ S.pol r.b 0

/-- Names a step would make from two agents that are there are not in use. -/
def Net.Fresh (N : Net K) : Prop :=
  ∀ x y k, N.kind x ≠ none → N.kind y ≠ none → N.kind (.made x y k) = none

variable {S T}

theorem no_swapped_pair (hdet : Deterministic T) (hpo : PrincipalsOpposite S T) {r r' : GRule K}
    (hr : r ∈ T) (hr' : r' ∈ T) (h1 : r'.a = r.b) (h2 : r'.b = r.a) : False := by
  have := hdet r hr r' hr' (Or.inr ⟨h2.symm, h1.symm⟩)
  subst this
  exact hpo r hr (by rw [h1])

/-- **Strong confluence**: two steps from the same net are the same step, or each can be followed
by one more step to reach a common net. -/
theorem strong_confluence (hdet : Deterministic T) (hpo : PrincipalsOpposite S T) {N N1 N2 : Net K}
    (hinv : N.Involutive) (hfr : N.Fresh) (h1 : Step S T N N1) (h2 : Step S T N N2) :
    N1 = N2 ∨ ∃ M, Step S T N1 M ∧ Step S T N2 M := by
  obtain ⟨a, b, r, hr, hab, hka, hkb, rfl⟩ := h1
  obtain ⟨c, d, r', hr', hcd, hkc, hkd, rfl⟩ := h2
  have pa : N.kind a ≠ none := by simp [hka]
  have pb : N.kind b ≠ none := by simp [hkb]
  have pc : N.kind c ≠ none := by simp [hkc]
  have pd : N.kind d ≠ none := by simp [hkd]
  have hba := hinv _ _ hab
  have hdc := hinv _ _ hcd
  -- the same pair, or the same pair the other way round, or disjoint
  by_cases hac : a = c
  · subst hac
    have hbd : b = d := by rw [hab] at hcd; simpa using hcd
    subst hbd
    left
    have : r = r' := hdet r hr r' hr' (Or.inl ⟨by rw [hka] at hkc; simpa using hkc, by rw [hkb] at hkd; simpa using hkd⟩)
    rw [this]
  by_cases had : a = d
  · subst had
    have : b = c := by rw [hdc] at hab; simp at hab; exact hab.symm
    subst this
    exact (no_swapped_pair hdet hpo hr hr' (by rw [hkb] at hkc; simpa using hkc.symm)
      (by rw [hka] at hkd; simpa using hkd.symm)).elim
  by_cases hbc : b = c
  · subst hbc
    have : a = d := by rw [hba] at hcd; simpa using hcd
    exact (had this).elim
  by_cases hbd : b = d
  · subst hbd
    have : a = c := by rw [hba] at hdc; simpa using hdc
    exact (hac this).elim
  have hab' : a ≠ b := by
    intro e; subst e; rw [hka] at hkb
    exact hpo r hr (by simp at hkb; rw [hkb])
  have hcd' : c ≠ d := by
    intro e; subst e; rw [hkc] at hkd
    exact hpo r' hr' (by simp at hkd; rw [hkd])
  have absent : ∀ {x y z : Ident}, N.kind x ≠ none → N.kind y ≠ none → N.kind z ≠ none →
      ∀ k, Ident.made x y k ≠ z := by
    intro x y z hx hy hz k e; subst e; exact hz (hfr x y k hx hy)
  have hap : Apart a b c d :=
    ⟨hab', hcd', hac, had, hbc, hbd, absent pc pd pa, absent pc pd pb, absent pa pb pc, absent pa pb pd⟩
  right
  refine ⟨fire S r' c d (fire S r a b N), ⟨c, d, r', hr', ?_, ?_, ?_, rfl⟩, ⟨a, b, r, hr, ?_, ?_, ?_, ?_⟩⟩
  · rw [fire_mate_other S r a b N (by simp [Ne.symm hac, Ne.symm hbc]) (madeIdx_none_of_ne (absent pa pb pc)),
      hcd, relink_keep r (q := (d, 0)) (Ne.symm had) (Ne.symm hbd)]
  · rw [fire_kind_other S r a b N (by simp [Ne.symm hac, Ne.symm hbc]) (madeIdx_none_of_ne (absent pa pb pc)), hkc]
  · rw [fire_kind_other S r a b N (by simp [Ne.symm had, Ne.symm hbd]) (madeIdx_none_of_ne (absent pa pb pd)), hkd]
  · rw [fire_mate_other S r' c d N (by simp [hac, had]) (madeIdx_none_of_ne (absent pc pd pa)),
      hab, relink_keep r' (q := (b, 0)) hbc hbd]
  · rw [fire_kind_other S r' c d N (by simp [hac, had]) (madeIdx_none_of_ne (absent pc pd pa)), hka]
  · rw [fire_kind_other S r' c d N (by simp [hbc, hbd]) (madeIdx_none_of_ne (absent pc pd pb)), hkb]
  · exact fire_comm S r r' N hap
end Confluence

/-! ## What a candidate table must pass -/

section Candidate
variable {K : Type} (S : Sig K) (T : List (GRule K))

/-- The table checks the general theorems need: every rule's wiring is polarized, mentions only
agents it makes, at most one rule per pair, and each rule pairs an in-facing principal port with an
out-facing one. -/
structure TableOk : Prop where
  polarized : ∀ r ∈ T, r.Polarized S
  madeOk : ∀ r ∈ T, r.MadeOk
  deterministic : Deterministic T
  principals : PrincipalsOpposite S T

variable {S T}

/-- Any table passing the checks: every step keeps a polarized net polarized. -/
theorem TableOk.step_polarized (h : TableOk S T) {N N' : Net K} (hN : N.Polarized S) (hP : N.Present)
    (hfr : N.Fresh) (hs : Step S T N N') : N'.Polarized S := by
  obtain ⟨a, b, r, hr, _, hka, hkb, rfl⟩ := hs
  exact fire_polarized
    ⟨hka, hkb, h.polarized r hr, h.madeOk r hr, fun k => hfr a b k (by simp [hka]) (by simp [hkb]), hP⟩ hN

/-- Any table passing the checks: runs are strongly confluent. -/
theorem TableOk.confluent (h : TableOk S T) {N N1 N2 : Net K} (hinv : N.Involutive) (hfr : N.Fresh)
    (h1 : Step S T N N1) (h2 : Step S T N N2) : N1 = N2 ∨ ∃ M, Step S T N1 M ∧ Step S T N2 M :=
  strong_confluence h.deterministic h.principals hinv hfr h1 h2
end Candidate
