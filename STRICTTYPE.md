# Completing §12.6: `StrictType`, structured Frames (Part C), and a real `InvalidType`

**Status:** design note (2026-06-08). What it takes to make the kernel's `respond`s
(and recognizers) inhabit `RespondShape` / `RecognizerShape` so the deep validator
`StrictType` (TYPE_THEORY §11.4 / §12.6) can check them — and the two concrete
problems Part A surfaced, with solution sketches.

> **Update (2026-06-11): the Part-C machinery now exists.** Telescopes landed
> (`Telescope` former; `KERNEL_DESIGN.md` § Telescopes): a structured per-former
> `Frame` type is now expressible as a `Telescope` (e.g. `{ motive : …, cases :
> … }`), and a frame-HYP's `frame.motive` projects through `tele_respond` with a
> real type instead of routing to Tree's inert respond. The `params` side still
> needs a per-former `Telescope`-params classifier (tele_respond itself triages
> its telescope, so it doesn't inhabit RespondShape under abstract params —
> pinned in `metashape.test.disp`). Steps 2–4 are now incremental, not blocked.

> **Update (2026-06-08): Step 1 (Problem 2) is LANDED.** `InvalidType` is now a real
> empty type (`make_recognizer` wait-form, MetaShape meta, respond `Extend (neutral_type
> self)`), distinct from `False`, and `Action`'s `Extend` payload is tightened `Tree →
> `Type`. The dead-state explosion is gone: under abstract `RespondShape` checking,
> `nat_respond` now returns a clean `Err` and `pi_respond` terminates (both previously
> exhausted the eval budget). Pinned in `metashape.test.disp`. Steps 2–4 (the per-former
> Frame buildout) remain deferred per §6.

Audience: anyone continuing the kernel-self-typing thread (Rung 1+2 landed; see
`lib/kernel/core.disp` end-of-file types, `metashape.test.disp`, and the memory note
`project_kernel_self_typing_metashape`).

---

## 0. Where we are

Three validators are intended (§11.4):

| Validator | Checks |
|---|---|
| `Type` (structural) | recognizer is `make_recognizer`-formed; meta fits `MetaShape`'s *layout*. H-rule on neutrals. **This is what the kernel ships today** (via `has_metashape_layout`). |
| `StrictType` (deep) | the above, **plus** recognizer inhabits `RecognizerShape`, `respond` inhabits `RespondShape`, meta typed field-by-field. **Not built.** |
| `BehavioralType` (coherence) | the above, **plus** the per-former computation laws (Π's `B a`, Σ's projection dependency, …) hold — encoded as `Path`s, run to `Ok TT`. **Needs cubical §13.** |

Rung 1 (landed) defined the vocabulary as first-class types: `Tree`, `Frame`,
`NeutralMeta`, `Action`, `RespondShape`, `MetaShape`. Rung 2 self-typed the parametric
parts: `make_meta : Tree -> Tree -> MetaShape`, `inert_respond : RespondShape`.

§12.6 **Part A** (landed) re-contracted every respond to `{params, self, frame} ->
Action`: `hyp_reduce` extracts the stored type's metadata *once* (its privileged raw
read) and passes `recognizer_params` + the reconstructed self-neutral, so a respond
never re-extracts `type_meta` from a possibly-abstract stored type. The one raw
metadata read now lives in the one raw Σ-op.

**The empirical wall after Part A** (`metashape.test.disp`), checking each respond
against `RespondShape = Arrow Tree (Arrow Tree (Arrow Frame Action))`:

| respond | `param_apply RespondShape ·` | cause |
|---|---|---|
| `inert_respond` | `Ok TT` | parametric — ignores all three args |
| `inductive_respond`, `eq_respond`, `record_respond` | `Err` | project off `frame`; abstract `frame : Tree` → `InvalidType`; applying it to the self-neutral trips the walker |
| `pi_respond`, `nat_respond` | **eval budget exhausted** | abstract inputs reach the `InvalidType` (= `FF`) dead-state elimination / the coherence machinery and explode |

So Part A removed the *metadata-read* obstacle but exposed two deeper ones. Neither is
the read-rewriting we already did; both are about the **shape of `frame`** and the
**representation of `InvalidType`**.

---

## 1. Problem 1 — `frame` is heterogeneous and structured (Part C proper)

The spec loosens `Frame := Tree` ("untagged; the stored type interprets it"). That is
fine at runtime (frames are concrete) but fatal under `RespondShape`-checking: an
abstract `frame : Tree` has the *inert* respond, so `frame.motive` routes to `Extend
InvalidType` (a stuck dead value), and `param_walker InvalidType self` applies that to
the self-neutral → triage-on-neutral → `Err`. The fix is to give each former a
**structured Frame type** so `frame.motive` is a real motive.

The frame each respond actually consumes (read off `core.disp`):

| former | respond reads | Frame type (proposed) |
|---|---|---|
| Π | `params.cod frame` | `frame : dom` — the argument. **Dependent** on the Π's `dom`. |
| inductive (Unit/Ord/Coproduct) | `frame.motive` | `Record [motive : Self -> Type; cases : …]` |
| Nat/Bool (gated) | `frame.motive`, `frame.cases` | as above; `cases` shape per former (Bool `{ct,cf}`, Nat `{base,step}`) |
| Σ | `tree_eq frame walker_pair_{fst,snd}` | a 2-element selector enum |
| Eq (J) | `frame.motive` | `Record [motive : A -> Type]` |
| Record | `pair_fst frame` | an accessor `acc name` (a tag from the field set) |
| Type (predicate side) | `is_neutral frame` | `Tree` — the candidate value (genuinely any) |

Two things follow:

**1a. `RespondShape` is not one type — it's per-former, and dependent.** With params,
self, *and* frame all varying, the respond type is a family. For Π it is genuinely
dependent: `pi_respond : (p : PiParams) -> Self -> (a : p.dom) -> Action` — the
frame's type `p.dom` depends on the params argument. The kernel *has* dependent Π
(`Pi A B`, `B : A -> Type`), so this is expressible — but `RespondShape` becomes a
function of the former (or each former declares its own `RespondShape_T`), not the
single `Arrow Tree (Arrow Tree (Arrow Frame Action))` we have now.

**1b. `motive : Self -> Type` is dependent on the eliminated type.** `Self` is the
neutral's type (Nat, Bool, …) — heterogeneous — so the inductive Frame type is itself
parameterized by `Self`, and `cases` must inhabit the motive at each constructor
(another dependency: Nat's `step : Π n. motive n -> motive (succ n)`). Typing `cases`
faithfully reproduces the very coherence check `nat_coherence` already runs at
runtime — i.e. the case-coherence gate becomes part of the Frame *type*.

### Solution sketch

- Define a per-former Frame type and a per-former `RespondShape_T`. Concretely, e.g.:
  - `PiFrame  p        := p.dom`  and  `RespondShape_Pi := Pi PiParams ({p} -> Arrow Self (Arrow (PiFrame p) Action))`.
  - `IndFrame Self M   := Record [motive : Arrow Self Type; cases : CaseShape Self M]`.
  - `SigmaFrame        := Coproduct [("fst", Unit); ("snd", Unit)]` (the two selectors).
  - `RecordFrame names := Coproduct [(n, Unit) for n in names]` (the accessors).
- The former's metadata declares its `RespondShape_T`; `StrictType` looks it up and
  checks `respond : RespondShape_T`.

### Residual difficulty inside 1

`PiParams = {dom : Type; cod : dom -> Type}` is a **dependent record** — `cod`'s type
mentions the `dom` field. The current `Record` former has *non-dependent* field types,
so this can't be a `Record`. Options: (i) encode params as a `Sigma` chain (`Sigma Type
({dom} -> Arrow dom Type)`) and read by projection rather than by §2.6 name — but the
kernel's `recognizer_params` is a name-keyed §2.6 record, so this means changing the
metadata representation; or (ii) add a *dependent-record* former (a `Record` whose
later field types may mention earlier fields). Either is real work and both stress the
type system's ability to express dependency in records.

---

## 2. Problem 2 — `InvalidType` must become a real (empty) type

`InvalidType := FF` today (`core.disp`): the Scott-`false` sentinel, chosen for O(1)
`tree_eq` and closedness. It is **not a type** — no recognizer, no `MetaShape` meta, no
`respond`. That is exactly why `param_apply Type InvalidType = Ok FF`, why `Action`'s
`Extend` payload had to be loosened to `Tree` (it carries `InvalidType`, a non-`Type`),
and why `pi_respond`/`nat_respond` blow the budget under abstract checking: they
produce a value stuck at `InvalidType` and then *eliminate* it, which makes
`hyp_reduce` look up `(type_meta InvalidType).respond` — i.e. project `.respond` out of
`FF` — yielding garbage that loops.

### Solution sketch

Promote `InvalidType` to a genuine empty type with an inert respond:

```disp
InvalidType := wait (make_recognizer ({m, v} -> Ok FF)) (make_meta unit_witness inert_respond)
```

Then: it *is* a `Type` (so `Action`'s `Extend` can tighten from `Tree` to `Type`);
nothing inhabits it (recognizer always `Ok FF`); and eliminating an `InvalidType`-
neutral runs its **inert respond → `Extend InvalidType`** (a well-defined absorbing
stuck form), so the budget blowup disappears — the dead state stays dead instead of
exploding.

### Residual difficulty inside 2

- **Audit every `InvalidType` site.** It's used as `Extend InvalidType` (responds) and
  `neutral_type stuck = InvalidType` (tests/downstream). Those compare/propagate the
  *value*, so a tree-identity change is mostly mechanical — but the comment at the
  current definition notes a property to preserve: as `FF`, an accidental `InvalidType
  v` is a harmless partial tree; as a recognizer wait-form, `InvalidType v` would *run
  the recognizer* (`Ok FF`). Confirm nothing relied on the `FF`-application behavior.
- **Ordering / bootstrap.** `InvalidType` is referenced by `inert_respond` and
  `hyp_reduce` (early in the file), but the real-type definition needs
  `make_recognizer`/`make_meta` (also early — fine) and *itself* uses `inert_respond`
  (mutual: `inert_respond` returns `Extend InvalidType`, `InvalidType`'s meta is
  `inert_respond`). This is a definitional cycle resolvable by the `use raw` layer (as
  Pi's self-reference is) or by a one-line `fix`, but it needs care.
- **`InvalidType` vs `False`.** Both are uninhabited. `False` is the *logical* empty
  proposition (a legitimate type; a `False`-neutral is a normal hypothesis). `InvalidType`
  is the *absorbing dead state* — a type-checking-failure marker. Conflating them
  (`InvalidType := False`) is tempting (one empty type) but conflates "ill-formed
  elimination" with "the empty proposition," and `False` is defined later in the file.
  Recommendation: keep them distinct types that happen to share the inert/empty shape.

---

## 3. Problem 3 — the precision ceiling (why this is bounded)

Even with Parts 1+2 done, `respond : RespondShape_T` verifies only that the respond is
a **total function into `Action`** with the right per-former argument shapes. It does
**not** verify the respond returns the *correct* `Action` — that Π's respond returns
`Extend (B a)` (not some other type), that Σ's `snd` preserves the dependency, that
inductive `respond` lands the motive at the reconstructed self. Those are the per-former
**computation laws**, and they are `BehavioralType`'s job — encoded as runnable `Path`s
(§13, cubical), which the kernel does not yet have.

So `StrictType` is a real but *bounded* gate: it rules out a respond that is partial,
ill-shaped, or returns a non-`Action`; it does **not** rule out a respond that returns
a well-formed but *wrong* `Action`. The genuine soundness guarantees (the elimination
rules actually compute correctly) live one tier up, in `BehavioralType`/§13.

---

## 4. The other half of `StrictType`: `RecognizerShape`

`StrictType` also checks `recognizer : RecognizerShape`. The recognizers
(`make_recognizer body`, body `{m, v} -> CheckerResult`) *triage* their candidate `v`
to recognize concrete inhabitants — which, on an abstract `v`-hyp, the walker would
reject, the same way responds fail. **But** there's a saving asymmetry: recognizers run
*inside* `recognizer_wrap`, whose H-rule short-circuits on `is_neutral v` *before*
running the body. So under `RecognizerShape`-checking, minting an abstract `v`-hyp would
trigger the H-rule path, not the structural body — the wrapper handles the abstract case
and the per-type body only ever sees concrete `v`. This suggests `RecognizerShape` may
be checkable through the wrapper where `RespondShape` is not through the bare respond —
worth a focused experiment (it's the mirror of Part A's finding, possibly with a better
outcome). Not yet investigated.

---

## 5. Suggested staging

1. **Real `InvalidType`** (Problem 2). ✅ **LANDED (2026-06-08).** The bootstrap cycle
   was sidestepped without a `fix`: `make_recognizer` moved up before `InvalidType`, and
   InvalidType's respond returns `Extend (neutral_type self)` (recovers InvalidType from
   the neutral via the sanctioned reader) instead of the literal `InvalidType` binding —
   so it never references its own meta's respond. Kept distinct from `False` via the
   differing respond. `Action`'s `Extend` tightened `Tree → Type`. 140/140 green.
2. **Inductive Frame type** (the biggest former group: Unit/Nat/Bool/Ord/Coproduct/Eq).
   Define `IndFrame`/`CaseShape`, make `inductive`/`gated`/`eq` responds typeable
   against per-former `RespondShape_T`. This is where the `cases`-inhabit-the-motive
   dependency (≈ `nat_coherence` as a type) lands.
3. **Π and Σ Frames** (the dependent-record / Sigma-encoding work, Problem 1's residual).
4. **Wire `StrictType`**: a validator that runs `param_apply MetaShape (type_meta v)`,
   `recognizer : RecognizerShape`, and `respond : RespondShape_T` (former looked up from
   the meta). Then `Type` stays the cheap structural gate and `StrictType` is the deep one.

Effort: step 1 small; step 2 medium (new dependent shapes); step 3 medium-large
(dependent records); step 4 small once 1–3 land. Risk concentrates in the bootstrap
cycles (real `InvalidType`, self-referential param/frame types) and in the
dependent-record representation choice.

---

## 6. The strategic question

`StrictType`'s guarantee is capped at "well-shaped, total, into `Action`" (§3); the
actual computation-rule correctness needs `BehavioralType`/§13. So the honest question
before building Parts 1–3 is: **is the bounded gate worth a per-former dependent-Frame
buildout, or is the effort better spent on §13 (cubical `Path`), which both subsumes
the correctness guarantee and is needed for univalence anyway?**

A reasonable middle path: do **step 1 (real `InvalidType`)** now — it's small, removes
a real sharp edge (the dead-state explosion), and tightens `Action` — and *defer* the
per-former Frame buildout (steps 2–3) until §13 makes the full `BehavioralType` gate
reachable, so the Frame types are designed once against the stronger target rather than
twice.

## References

- Code: `lib/kernel/core.disp` — `hyp_reduce` (the §12.6 Part-A contract), the responds
  (`inert_`/`inductive_`/`gated_inductive_`/`nat_`/`pi_`/`eq_`/`sigma`/`record_respond`,
  `type_predicate_h_rule`), `InvalidType`, and the end-of-file structural types
  (`Tree`/`Frame`/`NeutralMeta`/`Action`/`RespondShape`/`MetaShape`).
- Tests: `lib/tests/metashape.test.disp` — pins the inhabitation results and the wall.
- Spec: `TYPE_THEORY.typ` §11.2 (MetaShape convention), §11.4 (the three validators),
  §12.3 (InvalidType / the dead state), §12.6 (RespondShape / RecognizerShape).
- Memory: `project_kernel_self_typing_metashape`.
