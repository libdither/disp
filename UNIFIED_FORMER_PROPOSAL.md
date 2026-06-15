# Unified Dependent Former (Negative Telescopes) — Proposal

**Status: PROPOSAL (2026-06-14). Recognizer validated in scratch; respond
unvalidated; no migration started.** Companion reading: `TYPE_THEORY.typ`
(§2.6 "one cut for projection, match, and application"; §12 Telescope/Pi/Record/
Sigma), `KERNEL_DESIGN.md` (§ Telescopes), and the `mk_curried`/`constructor_type`
work (commit `7e1a11eb`).

## TL;DR

`Pi`, `Record`, and `Sigma` are three formers today. They are really **one**:
a *negative telescope* — a chain of dependent cells where each cell's value is

- **minted** (a fresh hypothesis — the ∀-introduction a function argument needs),
- **queried** (`v q` — the substrate cut, which *projects* when `q` is an
  accessor and *applies* when `q` is an argument), or
- **computed** (a recipe over prior cells — a transparent derived field).

A single recognizer (`neg_check`) walks such a telescope and recognizes
**existing lambdas and existing records as-is** — no new value representation.
`Pi A B` = `[mint x:A ; query-apply out : B x]`; `Record {a;b}` =
`[query-acc a ; query-acc b]`; `Sigma` = a 2-cell record; a transparent function
= `[mint/query inputs ; compute outputs]`.

The project-vs-apply distinction — the thing that made every prior attempt feel
like "squishing two unrelated recognizers together" — **dissolves**: both are
`v q` against the same cut. The only irreducibly distinct cell-source is `mint`,
and that is not a wart: it is **introduction** (binding a universally-quantified
variable) versus **elimination** (observing `v`). The genuine atoms are intro
and elim; everything else is shared.

This is a *principled* collapse (not the cosmetic "DepProd" squish that was
rejected earlier), but landing it is a conversion-identity migration. This doc
specifies the design, records the dead ends, states what's validated, and gives
a migration plan.

---

## 1. The question

Two long-standing irritations motivated this:

- **`Pi` is binary (`Pi A B`) but a telescope is n-ary.** Why the asymmetry?
- **Wait-forms feel like *both* Π and Σ.** A type is `wait recognizer meta`; a
  record is `prod F = wait annihilate F`; both are "an object awaiting one query
  `q`, then computing `f m q`." If the substrate has one mechanism (the cut),
  shouldn't the type formers unify?

The answer developed here: yes. The binary/​n-ary asymmetry is surface (`Pi A B`
is the *cons-step* of a Π-chain, exactly as `t entry (λx. rest)` conses a
telescope; multi-arg Pi nests). And the "both Π and Σ" feeling is real — they are
the elimination-by-application and elimination-by-projection faces of one cut.

---

## 2. Current encoding (ground truth)

So an implementer has the exact starting point. All in `lib/kernel/types.disp`
unless noted.

**Telescope spine.** A telescope is `t entry (λx. rest)` (a fork: `pair_fst` =
entry, `pair_snd` = the tail `λx. rest` binding *this* cell's value so later
cells see it); `nil = t`. An **entry** is the §2.6 record `{ name; ty; def }`:

- `name` — the field-name tag (a string).
- `ty` — the field's type (a recognizer; the check is `(entry.ty) x`).
- `def` — a **stem-option**: `t` (opaque / *input*) or `t recipe` (derived,
  *pinned* to `recipe` over the priors). Decoded by `opt_elim`.

**Recognizer** (`tele_check`, honest-lookup variant):

```disp
tele_check tele v =
  if (is_fork tele) then {
    let e = pair_fst tele
    match (lookup_field v (e.name)) {       // <-- ASSUMES v is a stored record
      Err _ => Ok FF                        //     (the field must be present)
      Ok x  => opt_elim e.def
                 ({_}     -> bind ((e.ty) x) ({ok} -> if ok then (self (tail x) v) else Ok FF))  // opaque: x : ty
                 ({recipe}-> if (tree_eq x recipe) then (self (tail x) v) else Ok FF)             // derived: x ≡ recipe
  } else Ok TT
```

**Respond** (`tele_field_at`) — projection on a *neutral* (a telescope-hyp):
walk to the named cell feeding each prior its projection-neutral (opaque) or
recipe (derived); opaque cell → `Extend ty`, derived → `Reduce recipe`; tails
instantiate under `param_walker`.

**The formers:**

```disp
Telescope := {tele} -> wait (make_recognizer {m,v} -> if (record? v) then (tele_check tele v) else Ok FF)
                            (make_meta tele ({p,self,frame} -> tele_field_at p self (pair_fst frame)))

Pi := {A,B} -> wait (make_recognizer {m,v} -> bind_hyp (m.recognizer_params.dom) ({h} -> ((m.recognizer_params.cod) h) (v h)))
                    (make_meta { dom:=A; cod:=B } ({p,self,frame} -> Extend (p.cod frame)))

Record := {fields} -> Telescope-over (fields_to_tele fields)   // constant-tail telescope
Sigma  := {A,B}    -> Telescope (t {fst:A} (λa. t {snd: B a} nil))
```

**Constructors (already landed, commit `7e1a11eb`):**

- `mk T given` — fill derived from recipes, opaque from `given`, emit the record.
- `mk_curried T` — the **curried** constructor: supply opaque cells one at a
  time, computing derived between; `mk_curried T : Π Δ_opaque. T`.
- `constructor_type T` — the Pi-chain type of `mk_curried T` (residual typing).

The crucial line for this proposal is the `lookup_field v name` in `tele_check`:
it assumes `v` is a stored record with named fields. That assumption is the only
thing excluding abstract functions.

---

## 3. Dead ends (recorded so they are not re-tried)

1. **DepProd — whole-chain polarity tag (`mode = Pi | Rec`).** Coherent: one
   former, the recognizer dispatches on a mode tag to either `tele_check`
   (project each field) or the Pi recognizer (mint + apply). Validated in
   scratch (5/5). **Rejected as a squish**: it is two recognizers under one tag
   with *no overlap* — the arms share nothing but the meta record. It also
   doesn't unify construction (a Pi value is a user lambda; there is no
   field-by-field constructor).

2. **Per-binder polarity (a flat chain mixing ∀/∃ binders).** *Incoherent.* It
   models a value that doesn't exist: an ∃-binder needs `v` to be a record
   (project), a ∀-binder needs `v` to be a function (apply), and a single tree
   can't be both (applying a `prod` to a non-accessor is garbage). Mixed
   quantifiers are always **nesting** (a record field whose *type* is a Pi; a Pi
   whose *codomain* is a record), never a flat polarity chain.

3. **`sat_check` — saturation-agnostic over *transparent* DAGs.** Validated in
   scratch (10/10): one walk where a cell is *supplied* (in the record),
   *minted* (an absent input → a fresh hyp), or *computed* (a derived recipe);
   `Record` = all-supplied, the "function" extreme = all-minted, partial =
   mixed. **But it only unifies Record with *transparent* functions** (whose
   body is a recipe living in the *type*). It does **not** subsume abstract
   `Pi A B` (opaque user lambdas), because an opaque function's output has *no
   recipe* — it is `v` applied to the input. Fixing exactly this gap is §4.

---

## 4. The key insight

`sat_check` had three cell-sources: supplied (project a stored field), minted
(a fresh hyp), computed (a recipe). It was missing the one source an opaque
function needs: **the output is `v` applied to the input.** And that is just the
cut `v q` again — the *same* operation as projecting a stored field
(`v (acc name)`), with a different query.

So: **replace `lookup_field v name` (which assumes a stored record) with `v q`
(the cut).** Then `v` may be a record (queried by accessors) *or* a function
(queried by arguments), uniformly. Projection and application stop being two
things; they are one cut with different queries.

The generalized cell-source:

| source | component | meaning |
|---|---|---|
| `mint` | `bind_hyp ty` (a fresh hyp) | **∀-introduction** — a function's argument / an unsupplied input |
| `qacc name` | `v (acc name)` — the cut | **projection** — a record field |
| `qapp` | `v prior` — the cut | **application** — a function's output (`v` applied to the prior input) |
| `compute` | `recipe(priors)` | **δ** — a transparent derived cell |

`qacc` and `qapp` are *both* `v q`. `mint` is the binder. `compute` is δ.

---

## 5. The design: negative telescopes

A **negative type** is a telescope whose entries carry a `src` (one of the four
sources above) instead of the opaque/derived stem-option. The formers become
source-patterns over one structure:

```
Pi A B            =  [ mint x:A ;  qapp out : B x ]              // intro, then observe v at it
(x:Type)->(x->x)  =  [ mint x:Type; qapp out : Arrow x x ]       // dependent, polymorphic
Record {a;b}      =  [ qacc a ;    qacc b ]                      // observe v at each accessor
Sigma A B         =  [ qacc fst:A ; qacc snd:(B fst) ]           // a 2-cell record
transparent fn    =  [ mint/qacc x ; compute r := not x ]        // body in the type
```

**The unified recognizer** (validated in scratch, 2026-06-14, 6/6 incl. opaque
and dependent/polymorphic Pi):

```disp
let neg_check = fix ({self, tele, v, prior} ->
  if (is_fork tele) then {
    let e = pair_fst tele
    if (tree_eq e.src "mint")
      then (bind_hyp (e.ty) ({h} -> self ((pair_snd tele) h) v h))                 // ∀-intro; h is the new prior
      else {
        let comp = if (tree_eq e.src "qapp") then (v prior) else (v (acc e.name))  // ELIM = the cut `v q`
        bind ((e.ty) comp) ({ok} -> if ok then (self ((pair_snd tele) comp) v comp) else (Ok FF))
      }
  } else (Ok TT))

NegT := {tele} -> wait (make_recognizer ({m, v} -> neg_check m.recognizer_params v t))
                       (make_meta tele neg_respond)   // neg_respond: §6
```

What this recognized, with the existing kernel and **unmodified lambda/record
values**:

```disp
PiBB := NegT ( t {ty:=Bool; src:="mint"} ({x} -> t {ty:=Bool; src:="qapp"} ({_} -> t)) )
test param_apply PiBB ({n} -> n)     = Ok TT   // identity : Bool->Bool  (an OPAQUE lambda)
test param_apply PiBB ({n} -> TT)    = Ok TT   // const
test param_apply PiBB ({n} -> zero)  = Ok FF   // returns Nat, not Bool

RecBB := NegT ( t {ty:=Bool; src:="qacc"; name:="a"} ({a} -> t {ty:=Bool; src:="qacc"; name:="b"} ({b} -> t)) )
test param_apply RecBB { a := TT; b := FF }   = Ok TT
test param_apply RecBB { a := TT; b := zero } = Ok FF

PiDep := NegT ( t {ty:=Type; src:="mint"} ({x} -> t {ty:=(Arrow x x); src:="qapp"} ({_} -> t)) )
test param_apply PiDep ({A} -> {z} -> z) = Ok TT   // poly id-returner : (A:Type)->(A->A)
```

The dependency threading is uniform: every cell's `ty` is the tail instantiated
at the prior *components*, whether those came from a mint, a projection, or an
application.

---

## 6. The respond (neutral elimination) — unified, **unvalidated**

Recognition is the introduction/checking side. The neutral-elimination side (the
`respond` in the meta, driven by `hyp_reduce`) must unify symmetrically, or the
collapse is only half done. The shape:

```disp
neg_respond p self frame =
  // `frame` is the elimination. Is it an accessor (project) or an argument (apply)?
  walk p to the cell this frame selects:
    qacc cell  -> Extend (cell.ty)            // projection lands at the field type   (≈ tele_field_at)
    qapp cell  -> Extend (cell.ty applied to the argument)   // application lands at the codomain (≈ Pi's Extend (cod frame))
    compute    -> Reduce (recipe)             // a transparent derived projection (δ)
  feeding priors their projection/argument neutrals, instantiating tails under param_walker.
```

This is `tele_field_at` (for `qacc`/`compute`) and Pi's `Extend (cod frame)`
(for `qapp`) under one dispatch — exactly mirroring `neg_check`. **It is not yet
built or tested.** The open risk is whether a single `Neg`-neutral can both
*project* (accessor frame) and *apply* (argument frame) through one respond
without the frame-kind ambiguity biting (an argument that happens to look like an
accessor). For *pure* patterns (a Pi-neutral only ever gets argument frames; a
Record-neutral only accessor frames) there is no ambiguity. Validating this is
the gating next step (§13).

---

## 7. Why this is a genuine unification (not the DepProd squish)

- **Elimination is one operation.** `qacc` (project) and `qapp` (apply) are both
  `v q`. The project-vs-apply split — which made DepProd two non-overlapping arms
  — collapses into *which query you send to the same cut*. That is the actual
  overlap.
- **Dependency is uniform.** One threading of prior components, regardless of
  source.
- **The only distinct atom is `mint`.** And `mint` vs `query` is **introduction
  vs elimination** — universal quantification (bind a variable) vs observation
  (look at `v`). That asymmetry is the real mathematical content of Π, now living
  *inside one former* rather than splitting it into two. `Pi` = intro-then-elim;
  `Record` = elim-only. Nothing reduces further because intro and elim don't.

---

## 8. The property that makes it land: no new value representation

`neg_check` recognized an **ordinary lambda** as a Pi and an **ordinary record**
as a record, with the *same* code, using only the existing substrate cut `v q`.
So unlike the self-describing partial-DAG idea (§9), this needs **no new value
model**: functions stay lambdas, records stay prods. The unification is entirely
at the **type / recognizer / respond** level. That is what makes it a realistic
migration rather than a runtime rewrite.

---

## 9. The DAG / dataflow reading (and its limit)

A telescope *is* a computational DAG: cells = nodes, `mint`/`qacc` inputs,
`qapp`/`compute` outputs, the tail binders = dependency edges. `mk_curried` is
"supply inputs left-to-right, computing derived as deps fill"; a partial
application is a partially-saturated DAG; reading a cell is projection/δ. This is
the substrate the GOALS roadmap wants: a typed dataflow graph where partial
application = partial evaluation / specialization.

Two limits to be honest about:

- **A self-describing *partial* value** (a record-under-construction you can
  `supply`/`read` out of order) is a *heavier, separate* value model (`{filled;
  residual}`), not needed by `neg_check` (which works on plain lambdas/records).
  It would be a library/layer if first-class dataflow values are wanted.
- **Linearity.** The telescope is a *topologically-sorted* DAG: cells fire in
  list order. True out-of-order supply (provide independent inputs in any order;
  compute a cell the instant its deps are ready regardless of position) needs
  **explicit dependency edges**, not linear order. The linear telescope is a
  pre-sorted DAG.

---

## 10. Honest residuals / non-goals

1. **Mixed values (callable records).** A value that is *both* projected-by-name
   and applied (an object with fields *and* an `apply`) needs a **dispatching
   value** (coherent as codata — `v q` triages whether `q` is an accessor or an
   argument — but a different value shape). Pure records and pure functions, all
   that is normally needed, are fine.
2. **Defining functions that eliminate their argument.** A function that *uses*
   its argument via raw `select`/`triage` (e.g. `not`, `add` written directly)
   does **not** inhabit its Pi type — eliminating a neutral via the Scott/raw
   path produces a malformed neutral. It must go through the proper recursor
   (`bool_rec`/`nat_rec`), which routes neutrals through the respond. This is
   unchanged by this proposal and orthogonal to it, but it means `qapp`
   recognition only succeeds for inhabitants that are parametric or recursor-
   defined — exactly as Pi recognition already requires.
3. **The respond is unvalidated** (§6).
4. **Performance.** Pure `Record` (O(1) tabulated projection) and pure `Pi`
   (direct lambda application) are the fast shapes; a generic `neg_check` walk is
   slower. Keep them as optimized representations / fast paths under the unified
   semantics (the `tree_eq` native-fast-path discipline).

---

## 11. Validation status

All in throwaway scratch against the live kernel (2026-06-14):

- `neg_check`: 6/6 — opaque `Bool->Bool` (identity, const, reject Nat-return),
  `Record {a;b}`, dependent/poly `(x:Type)->(x->x)`. **This is the core claim.**
- `sat_check` (transparent-DAG saturation-agnostic): 10/10 (the precursor; §3.3).
- `DepProd` (whole-chain polarity): 5/5 (the rejected squish; §3.1).
- `mk_curried` / `constructor_type`: landed and tested (`mk_curried.test.disp`).

Not yet validated: the unified **respond** (§6); behavior under genuinely
neutral `Neg` values; mixed/callable records.

---

## 12. Migration plan (concrete)

A principled but tree-changing collapse (conversion identity migration). Phasing:

**Phase 0 — validate the respond (no migration).** Build `neg_respond` in scratch
and confirm a `Neg`-neutral projects (accessor frame) *and* applies (argument
frame) correctly. If this fails or is ambiguous, stop — the collapse is not
viable as one former.

**Phase 1 — introduce `Neg` alongside the existing formers.** Add to
`lib/kernel/types.disp`:
- the cell-source encoding (entry gains `src ∈ {mint, qacc, qapp, compute}`;
  `qacc` carries `name`). This generalizes the current `{name; ty; def}` (opaque
  ≈ `qacc`/`mint`, derived ≈ `compute`).
- `neg_check` + `neg_respond`.
- `Neg := {tele} -> wait (make_recognizer …) (make_meta tele neg_respond)`.
Keep `Pi`/`Record`/`Sigma` unchanged. Pin `Neg`-instances tree-equal where
possible to the legacy formers (they will *not* be tree-identical — different
recognizers — so this is informational, not a no-op).

**Phase 2 — redefine `Pi`/`Record`/`Sigma` as `Neg` instances.** This changes
their trees → conversion-identity migration. Ripples:
- `mk` / `mk_curried` / `constructor_type` (cell-source aware).
- the **recType desugar** (`compile.ts` + the in-language `ast.disp` mirror):
  today it emits opaque/derived entries; it must emit `qacc`/`compute` (and the
  field-type `binderToPi`, already done). Pi-position (`->`) already desugars to
  `Pi`; if `Pi` becomes `Neg [mint; qapp]`, `binderToPi` targets that.
- the kernel's own structural types (`NeutralMeta`, `MetaShape`, `RespondShape`,
  `Action`) are `Record`/`Coproduct` instances — they move with `Record`.
- **golden suites**: `telescope`, `mk_curried`, `compile`, `desugar`, `ast`,
  `metashape`, `wrapping`, `kernel`, `use_raw`, `types` — all encode current
  former trees; every one migrates.

**Phase 3 — fast paths.** Reintroduce tabulated-`Record` and direct-`Pi`
recognizers as optimizations validated bit-identically against `neg_check`
(per the `tree_eq` native-fast-path discipline), if profiling needs them.

Blast radius: kernel + elaborator (host + in-language) + ~8 golden suites. Large,
but each step is mechanical once Phase 0 confirms the respond.

---

## 13. Open questions

- **Respond ambiguity** (the Phase-0 gate): can one `neg_respond` serve accessor
  *and* argument frames without misclassifying? (Pure patterns: yes; mixed: needs
  a frame tag.)
- **Frame tagging.** Should an elimination frame carry an explicit
  accessor-vs-argument tag, making mixed/callable records first-class? That is the
  difference between "unify pure Pi and pure Record" and "support objects with
  fields *and* apply."
- **Is the collapse worth the migration?** The unification is real and
  principled, but `Pi`/`Record` already share the telescope spine conceptually
  (`constructor_type`, `mk_curried`). Options: (a) full collapse to `Neg`;
  (b) keep `Pi`/`Record`/`Sigma` as the surface formers but **share `neg_check`
  internally** (define each as a thin `Neg` instance, no separate recognizer —
  smaller migration, same unity in code); (c) document the unification as the
  organizing principle and keep the current formers (zero migration). (b) is
  likely the sweet spot.
- **Relation to the spec.** `TYPE_THEORY.typ` §2.6 already states "one cut for
  projection, match, and application," and §12 frames `Record ≅ finite Pi`. This
  proposal is the *operational* realization of that slogan at the former level;
  the spec should reference it.
- **Dataflow layer.** Whether to build the self-describing partial-DAG value
  (§9) for first-class staged computation / out-of-order supply (needs explicit
  dep-edges), as a library over `Neg`.

---

## 14. Recommendation

The unification is genuine: **a type is a negative telescope of cells; each cell
is introduced (`mint`), observed (`v q` — the one cut, project or apply), or
computed; `Pi`/`Record`/`Sigma`/transparent-functions are source-patterns over
that one structure.** It recognizes existing lambdas and records unchanged, and
the project/apply distinction — the thing that defeated every prior attempt —
genuinely dissolves into the cut.

Proposed path: **Phase 0 (validate `neg_respond`) → option (b)** — keep
`Pi`/`Record`/`Sigma` as surface names but reimplement them as thin `Neg`
instances sharing `neg_check`/`neg_respond`, so the kernel has one recognizer and
one respond, the unity is visible in code, and the surface/trees churn is
bounded. Reserve the full collapse (option a) and the first-class dataflow value
(§9) for later, driven by the optimizer/staged-computation goals.
