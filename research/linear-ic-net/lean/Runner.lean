import Check
import Moves

/-! The spec runner (`step`, `runFor` in `Check.lean`) makes runs in the theorems' sense: each of its
steps fires an active pair the table has a rule for, or removes a forwarder other than the root;
and when it stops, nothing is left to fire or remove. So when it finishes a program, every order
of steps finishes too, in the same number of steps, in the very net it read the answer from. -/

section Steps
variable {T : List (GRule K)}

theorem ruleFor_spec {x y : K} {r : GRule K} (h : ruleFor T x y = some r) : r ∈ T ∧ r.a = x ∧ r.b = y := by
  unfold ruleFor at h
  have hp := List.find?_some h
  simp only [Bool.and_eq_true, beq_iff_eq] at hp
  exact ⟨List.mem_of_find?_eq_some h, hp.1, hp.2⟩

theorem findPair_spec {st : Run} {a b : Ident} {r : GRule K} (h : findPair T st = some (a, b, r)) :
    a ∈ st.ids ∧ r ∈ T ∧ st.net.mate (a, 0) = some (b, 0) ∧ st.net.kind a = some (.base r.a) ∧
      st.net.kind b = some (.base r.b) := by
  unfold findPair at h
  obtain ⟨x, hx, hf⟩ := List.exists_of_findSome?_eq_some h
  cases hk : st.net.kind x with
  | none => simp [hk] at hf
  | some g =>
    cases g with
    | fwd p => simp [hk] at hf
    | base kx =>
      cases hm : st.net.mate (x, 0) with
      | none => simp [hk, hm] at hf
      | some q =>
        obtain ⟨y, n⟩ := q
        cases n with
        | succ n => simp [hk, hm] at hf
        | zero =>
          cases hky : st.net.kind y with
          | none => simp [hk, hm, hky] at hf
          | some g' =>
            cases g' with
            | fwd p => simp [hk, hm, hky] at hf
            | base ky =>
              cases hr : ruleFor T kx ky with
              | none => simp [hk, hm, hky, hr] at hf
              | some r' =>
                simp only [hk, hm, hky, hr, Option.some.injEq, Prod.mk.injEq] at hf
                obtain ⟨rfl, rfl, rfl⟩ := hf
                obtain ⟨hmem, rfl, rfl⟩ := ruleFor_spec hr
                exact ⟨hx, hmem, hm, hk, hky⟩

theorem fireStep_net (st : Run) (a b : Ident) (r : GRule K) : (fireStep st a b r).net = fire sig r a b st.net := by
  unfold fireStep; split <;> rfl

theorem fireStep_root (st : Run) (a b : Ident) (r : GRule K) : (fireStep st a b r).root = st.root := by
  unfold fireStep; split <;> rfl

theorem fireStep_ids (st : Run) (a b : Ident) (r : GRule K) : (fireStep st a b r).ids =
    st.ids.filter (fun x => x != a && x != b) ++
      ((List.range (r.made.length + r.wires.length)).filterMap fun k =>
        (madeKind sig r k).map fun g => (Ident.made a b k, g)).map (·.1) := by
  unfold fireStep; split <;> rfl

/-- **Each step of the spec runner is a step of the theorems**, keeping its root forwarder. -/
theorem step_move {st st' : Run} (h : step T st = some st') :
    Move sig T (· = st.root) st.net st'.net ∧ st'.root = st.root := by
  unfold step at h
  cases hf : st.ids.find? (isFwd st) with
  | some x =>
    rw [hf] at h; simp only [Option.some.injEq] at h; subst h
    have hx := List.find?_some hf
    simp only [isFwd, Bool.and_eq_true, bne_iff_ne, ne_eq] at hx
    obtain ⟨hroot, hk⟩ := hx
    cases hkx : st.net.kind x with
    | none => simp [hkx] at hk
    | some g =>
      cases g with
      | base k => simp [hkx] at hk
      | fwd P => exact ⟨Or.inr ⟨x, P, hroot, hkx, rfl⟩, rfl⟩
  | none =>
    rw [hf] at h
    cases hp : findPair T st with
    | none => simp [hp] at h
    | some t =>
      obtain ⟨a, b, r⟩ := t
      simp only [hp, Option.some.injEq] at h; subst h
      obtain ⟨_, hr, hab, hka, hkb⟩ := findPair_spec hp
      exact ⟨Or.inl ⟨a, b, r, hr, hab, hka, hkb, (fireStep_net st a b r)⟩, fireStep_root st a b r⟩

/-- The runner's list of agents holds every agent in its net. -/
def Covers (st : Run) : Prop := ∀ x, st.net.kind x ≠ none → x ∈ st.ids

theorem madeKind_lt {r : GRule K} {k : Nat} (h : madeKind sig r k ≠ none) :
    k < r.made.length + r.wires.length := by
  unfold madeKind at h
  split at h
  · omega
  · cases hw : r.wires[k - r.made.length]? with
    | none => simp [hw] at h
    | some _ =>
      have := (List.getElem?_eq_some_iff.mp hw).1
      omega

theorem step_covers {st st' : Run} (h : step T st = some st') (hc : Covers st) : Covers st' := by
  unfold step at h
  cases hf : st.ids.find? (isFwd st) with
  | some x =>
    rw [hf] at h; simp only [Option.some.injEq] at h; subst h
    intro y hy
    simp only [unfwd_kind] at hy
    by_cases hyx : y = x
    · simp [hyx] at hy
    simp only [hyx, if_false] at hy
    exact List.mem_filter.mpr ⟨hc y hy, by simpa using hyx⟩
  | none =>
    rw [hf] at h
    cases hp : findPair T st with
    | none => simp [hp] at h
    | some t =>
      obtain ⟨a, b, r⟩ := t
      simp only [hp, Option.some.injEq] at h; subst h
      intro y hy
      rw [fireStep_net] at hy
      rw [fireStep_ids]
      rcases fire_kind_some sig r a b st.net hy with ⟨k, rfl⟩ | ⟨hab, _, hyN⟩
      · rw [fire_kind_made] at hy
        cases hm : madeKind sig r k with
        | none => exact absurd hm hy
        | some g =>
          refine List.mem_append_right _ (List.mem_map.mpr ⟨(Ident.made a b k, g), ?_, rfl⟩)
          exact List.mem_filterMap.mpr ⟨k, List.mem_range.mpr (madeKind_lt hy), by simp [hm]⟩
      · refine List.mem_append_left _ (List.mem_filter.mpr ⟨hc y hyN, ?_⟩)
        simp only [not_or] at hab
        simp [hab.1, hab.2]

/-- **When the spec runner stops, nothing is left to do**: no pair to fire, no forwarder to
remove but the root. -/
theorem stop_finished {st : Run} (h : step T st = none) (hc : Covers st) :
    Finished sig T (· = st.root) st.net := by
  unfold step at h
  cases hf : st.ids.find? (isFwd st) with
  | some x => rw [hf] at h; cases h
  | none =>
    rw [hf] at h
    cases hp : findPair T st with
    | some t => rw [hp] at h; cases h
    | none =>
      intro M hm
      rcases hm with ⟨a, b, r, hr, hab, hka, hkb, _⟩ | ⟨x, P, hroot, hx, _⟩
      · have ha : a ∈ st.ids := hc a (by simp [hka])
        unfold findPair at hp
        have hnone := List.findSome?_eq_none_iff.mp hp a ha
        have hrule : ruleFor T r.a r.b ≠ none := by
          unfold ruleFor
          intro e
          have := List.find?_eq_none.mp e r hr
          simp at this
        cases hrf : ruleFor T r.a r.b with
        | none => exact hrule hrf
        | some r' => simp [hka, hab, hkb, hrf] at hnone
      · have hxid : x ∈ st.ids := hc x (by simp [hx])
        have := List.find?_eq_none.mp hf x hxid
        simp [isFwd, hroot, hx] at this

/-- When `runFor` reports a finished run, it made a run in the theorems' sense and ended where
nothing is left to do. -/
theorem runFor_finished : ∀ (n : Nat) {st st' : Run}, runFor T n st = (st', true) → Covers st →
    Moves sig T (· = st.root) st.net st'.net ∧ Finished sig T (· = st.root) st'.net ∧ st'.root = st.root
  | 0, _, _, h, _ => by simp [runFor] at h
  | n + 1, st, st', h, hc => by
    simp only [runFor] at h
    cases hs : step T st with
    | none =>
      rw [hs] at h; simp only [Prod.mk.injEq] at h; obtain ⟨rfl, -⟩ := h
      exact ⟨.refl _, stop_finished hs hc, rfl⟩
    | some s =>
      rw [hs] at h
      obtain ⟨hm, hroot⟩ := step_move hs
      obtain ⟨ih1, ih2, ih3⟩ := runFor_finished n h (step_covers hs hc)
      rw [hroot] at ih1 ih2 ih3
      exact ⟨.step hm ih1, ih2, ih3⟩
end Steps

/-! ## The compiled program is a good net -/

theorem lookupP_mem {ws : List (Port × Port)} {p q : Port} (h : lookupP ws p = some q) :
    (p, q) ∈ ws ∨ (q, p) ∈ ws := by
  unfold lookupP at h
  cases hf : ws.find? (fun w => w.1 == p) with
  | some w =>
    rw [hf] at h; simp only [Option.some.injEq] at h; subst h
    have := List.find?_some hf; simp only [beq_iff_eq] at this
    left; rw [← this]; exact List.mem_of_find?_eq_some hf
  | none =>
    rw [hf] at h
    cases hg : ws.find? (fun w => w.2 == p) with
    | none => rw [hg] at h; cases h
    | some w =>
      rw [hg] at h; simp only [Option.map_some, Option.some.injEq] at h; subst h
      have := List.find?_some hg; simp only [beq_iff_eq] at this
      right; rw [← this]; exact List.mem_of_find?_eq_some hg

/-- A check of the compiled net's finite lists: every agent has a root name, and every wire is
symmetric, touches agents that are there, and joins an out port to an in port. -/
def Build.ok (b : Build) : Bool :=
  b.kinds.all (fun e => match e.1 with | .root _ => true | .made .. => false) &&
  b.wires.all fun w =>
    lookupP b.wires w.1 == some w.2 && lookupP b.wires w.2 == some w.1 &&
    (b.net.kind w.1.1).isSome && (b.net.kind w.2.1).isSome && b.net.pol sig w.1 != b.net.pol sig w.2

theorem Build.ok_good {b : Build} (h : b.ok = true) : b.net.Good sig := by
  simp only [Build.ok, Bool.and_eq_true, List.all_eq_true, beq_iff_eq, bne_iff_ne, ne_eq] at h
  obtain ⟨hroots, hw⟩ := h
  have wire : ∀ p q, b.net.mate p = some q → b.net.mate q = some p ∧ b.net.kind p.1 ≠ none ∧
      b.net.kind q.1 ≠ none ∧ b.net.pol sig p ≠ b.net.pol sig q := by
    intro p q hm
    rcases lookupP_mem hm with hmem | hmem
    · obtain ⟨⟨⟨⟨_, h2⟩, h3⟩, h4⟩, h5⟩ := hw _ hmem
      exact ⟨h2, by simpa [Option.isSome_iff_ne_none] using h3, by simpa [Option.isSome_iff_ne_none] using h4, h5⟩
    · obtain ⟨⟨⟨⟨h1, _⟩, h3⟩, h4⟩, h5⟩ := hw _ hmem
      exact ⟨h1, by simpa [Option.isSome_iff_ne_none] using h4, by simpa [Option.isSome_iff_ne_none] using h3,
        fun e => h5 e.symm⟩
  refine ⟨fun p q hm => (wire p q hm).2.2.2, fun p q hm => ⟨(wire p q hm).2.1, (wire p q hm).2.2.1⟩,
    fun p q hm => (wire p q hm).1, tidy_of_roots ?_⟩
  intro x hx
  cases hf : b.kinds.find? (·.1 == x) with
  | none => exact absurd (by simp [Build.net, hf]) hx
  | some e =>
    have hex := List.find?_some hf; simp only [beq_iff_eq] at hex
    have hr := hroots e (List.mem_of_find?_eq_some hf)
    obtain ⟨y, g⟩ := e
    simp only at hex hr; subst hex
    cases y with
    | root n => exact ⟨n, rfl⟩
    | made _ _ _ => simp at hr

theorem start_covers (t : Tm) : Covers (start t) := by
  intro x hx
  show x ∈ (loadB t).1.kinds.map (·.1)
  cases hf : (loadB t).1.kinds.find? (·.1 == x) with
  | none => exact absurd (by simp [start, Build.net, hf]) hx
  | some e =>
    have hex := List.find?_some hf; simp only [beq_iff_eq] at hex
    exact List.mem_map.mpr ⟨e, List.mem_of_find?_eq_some hf, hex⟩

/-! ## Every order gives the checked answer -/

/-- Runs of a compiled program: fire pairs and remove forwarders other than the root, in any order. -/
abbrev ProgRuns (t : Tm) := Moves sig gtable (· = (loadB t).2)

/-- **One finished run decides every run.** If some run of a compiled program (one whose net passes
`Build.ok`) reaches a net with nothing left to do, then no order of steps runs longer than that
run, every order can still finish there, and every finished run ends in that very net. So to show
that every order gives the right answer, it is enough to show it for one order. -/
theorem one_run_decides {t : Tm} {F : Net K} (hok : (loadB t).1.ok = true)
    (hrun : ProgRuns t (start t).net F) (hfin : Finished sig gtable (· = (loadB t).2) F) :
    (∃ k, ∀ m M, Path (Move sig gtable (· = (loadB t).2)) m (start t).net M → m ≤ k) ∧
    (∀ M, ProgRuns t (start t).net M → ProgRuns t M F) ∧
    (∀ M, ProgRuns t (start t).net M → Finished sig gtable (· = (loadB t).2) M → M = F) := by
  have hg : (start t).net.Good sig := Build.ok_good hok
  have pres : ∀ a b, Net.Good sig a → Move sig gtable (· = (loadB t).2) a b → Net.Good sig b :=
    fun _ _ hg hm => rln_runOk.move_good hg hm
  obtain ⟨k, hk⟩ := hrun.path
  refine ⟨⟨k, fun m M hp => (Path.bound pres rln_runOk.move_diamond hp hg hk hfin).1⟩, ?_, ?_⟩
  · intro M hM
    obtain ⟨m, hm⟩ := hM.path
    exact (Path.bound pres rln_runOk.move_diamond hm hg hk hfin).2.star
  · intro M hM fM
    exact rln_runOk.finished_unique hg hM hrun fM hfin

/-- **When the spec runner finishes a program**, (compiled to a net passing `Build.ok`): no order
of steps runs longer than it did, every order can still finish, and every finished run ends in
the very net it ended in. -/
theorem runner_decides {t : Tm} {n : Nat} {st : Run} (hok : (loadB t).1.ok = true)
    (h : runFor gtable n (start t) = (st, true)) :
    (∃ k, ∀ m M, Path (Move sig gtable (· = (loadB t).2)) m (start t).net M → m ≤ k) ∧
    (∀ M, ProgRuns t (start t).net M → ProgRuns t M st.net) ∧
    (∀ M, ProgRuns t (start t).net M → Finished sig gtable (· = (loadB t).2) M → M = st.net) := by
  obtain ⟨hrun, hfin, _⟩ := runFor_finished n h (start_covers t)
  exact one_run_decides hok hrun hfin

/-- **Every order of steps reads back the reference's answer**, for a program the spec-runner
check passes (and whose compiled net passes `Build.ok`): no order runs forever, and every
finished run, whatever order it fired pairs and removed forwarders in, holds the reference
interpreter's value at the root. -/
theorem every_order_answers {t : Tm} {fuel : Nat} {v : Tm} (hok : (loadB t).1.ok = true)
    (hc : checkWith gtable fuel t = true) (he : eval fuel t = .value v) :
    (∃ k, ∀ m M, Path (Move sig gtable (· = (loadB t).2)) m (start t).net M → m ≤ k) ∧
    (∀ M, ProgRuns t (start t).net M → Finished sig gtable (· = (loadB t).2) M →
      (M.mate ((loadB t).2, 0)).bind (readback 10000 M []) = some v) := by
  unfold checkWith at hc
  rw [he] at hc
  cases hr : runFor gtable (20 * fuel) (start t) with
  | mk st fin =>
    rw [hr] at hc
    simp only [Bool.and_eq_true, beq_iff_eq] at hc
    obtain ⟨⟨⟨⟨hfin, _⟩, hans⟩, _⟩, _⟩ := hc
    subst hfin
    obtain ⟨hbound, _, huniq⟩ := runner_decides hok hr
    obtain ⟨_, _, hroot⟩ := runFor_finished _ hr (start_covers t)
    refine ⟨hbound, fun M hM fM => ?_⟩
    rw [huniq M hM fM]
    rw [hroot] at hans
    exact hans

/-! ## What is left for all programs at once -/

/-- Simulation for one program: whenever the reference gives it a value, some run of its net
finishes holding that value. One run, in one order of our choosing. -/
def Simulated (t : Tm) : Prop :=
  ∀ fuel v, eval fuel t = .value v →
    ∃ F, ProgRuns t (start t).net F ∧ Finished sig gtable (· = (loadB t).2) F ∧
      ∃ n, (F.mate ((loadB t).2, 0)).bind (readback n F []) = some v

/-- **One order is enough.** If a program is simulated by some run, then every order of steps
gives the reference's answer: no order runs forever, and every finished run holds the value. So
the copier never has to be reasoned about while two copies are under way at once: it is enough to
follow the one order, call by value, in which each copy finishes before anything else moves. -/
theorem every_order_of_one {t : Tm} {fuel : Nat} {v : Tm} (hok : (loadB t).1.ok = true)
    (hs : Simulated t) (he : eval fuel t = .value v) :
    (∃ k, ∀ m M, Path (Move sig gtable (· = (loadB t).2)) m (start t).net M → m ≤ k) ∧
    (∃ n, ∀ M, ProgRuns t (start t).net M → Finished sig gtable (· = (loadB t).2) M →
      (M.mate ((loadB t).2, 0)).bind (readback n M []) = some v) := by
  obtain ⟨F, hrun, hfin, n, hans⟩ := hs fuel v he
  obtain ⟨hbound, _, huniq⟩ := one_run_decides hok hrun hfin
  exact ⟨hbound, n, fun M hM fM => by rw [huniq M hM fM]; exact hans⟩
