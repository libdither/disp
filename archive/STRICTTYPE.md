# Completing §12.6: `StrictType`, structured Frames (Part C), and a real `InvalidType`

**Status: archived 2026-07-09, superseded by the landed universe.** Step 1
(`InvalidType`) and the recognize-side `StrictType` landed as designed (update
notes below). The coherence rung landed 2026-07-01 as `Type := BehavioralType`
via GoodRespond probe telescopes, without cubical (contra the validator table in
§0). The open remainder is the structural `respond : RespondShape` ascription in
MetaShape, tracked in TYPE_THEORY.typ §11-§12 and pinned in `metashape.test.disp`.
Kept for the Part B/C structured-Frame buildout sketches. (File references such as
`lib/kernel/core.disp` predate the seven-fragment kernel split.)

**Original status:** design note (2026-06-08). What it takes to make the kernel's `respond`s
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
| `StrictType` (deep) | the above, **plus** recognizer inhabits `RecognizerShape`, `respond` inhabits `RespondShape`, meta typed field-by-field. **Recognize-side LANDED** (2026-06, as a self-hosting telescope — see §7); the deep `respond : RespondShape` half is open (§7A/§7B). |
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

## 7. Next steps (2026-06) — the recognize side landed; the respond side is two research problems

**What changed since §0–§6.** Two developments reshape this doc. (a) The kernel migrated
to **one walker `at` over wait-form cells** (`lib/kernel/{cut,engine,types}.disp` — `core.disp`
was split): a telescope cell is a wait-form `wait op meta` returning a `Step`, the negative
formers' respond is now `at FF` (not the per-former `pi_respond`/`record_respond` the §0 table
names), and projections inside responds route through `param_walker` for GAP-2 policing. (b)
**The recognize-side metacircle LANDED as `StrictType`** (commit `466b76a`): instead of
`has_metashape_layout` (a structural header `tree_eq`), `StrictType` checks the *semantic* type
interface as a plain `Telescope` of two `qid` cells — recognize-face `v : Arrow Tree
CheckerResult` and meta-face `type_meta v : MetaShape` — walked by the same `at`. It recognizes
every kernel type, tells a type from a type *constructor*, and **`StrictType : StrictType`** —
the fixed point closes with no privileged kinding rule (`lib/tests/strict_type.test.disp`,
150/150, additive; legacy `Type` untouched).

So §0's "StrictType: not built" is now half-built: the `param_apply MetaShape (type_meta v)`
half (Problem-1's "wire StrictType", §5 step 4) is **done and self-hosting**. What remains is
the deep `respond : RespondShape` check, and Phase-B probing (2026-06) located it *below*
Problem 1 in two specific, independent walls.

**Problem 1 (structured frames) is VALIDATED — it is not the wall.** A structured frame type
*does* make a *projecting* respond checkable: against `params -> Self -> { motive : Tree ->
Type, cases } -> Action`, the hand-inlined `{p,s,f} -> Extend (f.motive s)` checks `Ok TT`,
where the flat `RespondShape` (frame : Tree) rejects it. So "Part C" works for the
data-projection half. The wall is one layer deeper, in *how the responds use that frame*.

### 7A. Typing privileged walker-policing

**The problem, precisely.** The real responds don't write `Extend (f.motive s)` — they write
```disp
match (param_walker frame.motive self) { Ok ty => Extend ty; Err _ => Extend InvalidType }
```
The `param_walker` is **load-bearing soundness**: it polices the motive so a motive that
raw-triages the self-neutral routes to `InvalidType` (the GAP-2 discipline — a respond must not
let a sub-term inspect a hyp). With a structured frame the *inlined* form (no `param_walker`)
type-checks; the *policed* form does not, though both compute the same value. (Validated 2026-06:
standalone, the `param_walker`/`match` pieces all check — scrutinee is `Ok (…)`, result is
`Extend (…)`, it inhabits `Action`; but under the Pi-chain recognition — nested `bind_hyp`,
ambient walker — the policed respond fails where the inlined one passes.)

**Why it's hard.** `param_walker` invoked *inside a respond that is itself being type-checked by
`param_walker`* is a **privileged operation typing against itself** — the same self-reference
that blocks typing `bind_hyp` and the raw meta-readers (the recurring privileged-vs-parametric
wall). You cannot give `param_walker` a parametric `(A -> B) -> A -> CheckerResult` type and have
the respond inherit it, because `param_walker`'s job is precisely to *break* parametricity safely
(it is the walker — the thing that *decides* soundness). Typing it strictly is circular.

**Attack vectors:**
- **Sealing / trusted-token (most promising).** `bind_hyp` is already a privileged op the kernel
  *trusts* (the `seal(Σ)` set; `bind_hyp_marker` fails closed). Ask whether `param_walker` — or
  one factored-out `apply_policed` combinator — can be admitted as a **sealed token with a
  *declared* (not *checked*) type** for the purpose of respond-checking, so a respond that calls
  it inherits a clean type without the walker re-deriving the walker. This is the same resolution
  that landed `bind_hyp` body-walking (the sealing-framework note): a privileged op gets a
  declared, trusted type. Open question: does declaring `param_walker`'s type preserve soundness
  (the step-indexed-LR "sealing preserves parametricity" conjecture), and do the projecting
  responds (`inductive`/`eq`) then inhabit `RespondShape`?
- **Factor policing into one typed helper.** If every `param_walker` use in responds is
  concentrated into a single sealed `apply_policed : (Self -> Type) -> Self -> Action`-shaped
  combinator, each respond body becomes *parametric modulo one trusted call*, and the trusted
  call carries the type. (Same shape as the sealing route, framed as a refactor.)
- **Honest fallback.** Keep `respond` at `MetaShape`'s loose `Tree` and have `StrictType` check
  only `recognizer : RecognizerShape` + `type_meta : MetaShape` strictly — which is what ships.
  Per §3/§6, respond *correctness* needs `BehavioralType`/§13 anyway, so a strict-but-shallow
  respond *shape* type buys little; this may be the right precision ceiling.

### 7B. Self-typing the spine

> **RESOLVED for the uniform formers (2026-06-19, commit `4d5f7bf`).** The wall described below was
> dissolved without the feared higher-order recursor: `at FF` tails route through `apply_policed` and
> `proj_op` threads via the sanctioned `source (acc name)` elimination, so Pi/Record/Sigma/dependent-Sigma
> responds inhabit per-former RespondShapes. The analysis below is retained as history; the only surviving
> open case is **branching dependent codomains** (see "Recommended next move").

**The problem, precisely.** The negative-former respond is `at FF params self frame t`, and
`at FF` walks the cell telescope `params` with **raw structural ops** — `is_fork params`,
`pair_fst params`, `(pair_snd params) x`. Under `RespondShape`-checking `params` is an abstract
hyp, so `is_fork params` triages it → the walker rejects. So the *negative-former* respond cannot
inhabit any `RespondShape` while it raw-walks the spine — **independent of frame structure (7A)**,
because the **spine itself is untyped**.

**Why it's hard.** The spine `t cell (λx. rest)` is a *dependent* structure: each tail is a real
λ binding the prior cell's value, so it is not a `List Cell` but a Σ-chain `Cell × (cellvalue ->
(Cell × …))`. Walking it safely (routing a neutral spine through a recursor instead of raw
`is_fork`) needs both an **`Entry`/`Cell` type** and a **`Telescope`-shape recursor** that routes
a neutral spine through `respond` — and that recursor is *higher-order* (the λ-tails range over
cell values, themselves typed).

**Attack vectors:**
- **`Cell` as a coproduct of op-signatures (the easy half).** The op set is closed-ish
  (`mint`/`proj`/`apply`/`deriv`/`qid`/`qid_meta`), so `Cell := Coproduct [(mint_sig, Type),
  (proj_sig, {name;ty}), …]` is expressible, and `is_derive`-style signature reads are
  *sanctioned* (they use `pair_fst`, a walker carve-out). This types the cell.
- **The spine recursor (the hard half).** A `tele_rec` that, given a neutral spine, routes
  through a *spine respond* rather than `is_fork` — i.e. make `at FF` itself **elim-routed over a
  typed spine, exactly as `nat_rec` is elim-routed over `Nat`**. Whether the dependent λ-tails can
  be typed without full first-class dependent-spine machinery is the open question (NEGATIVE_TYPES
  §4b's "self-description needs higher-order machinery").
- **Independence from 7A.** A fully self-typed spine still leaves the *projecting* responds on 7A
  (they `param_walker` the motive); a fully solved 7A still leaves the *negative-former* respond
  on 7B (it raw-walks the spine). **Both** are required for a fully strict `respond` across all
  formers; they are independent walls.

### Recommended next move

**Update (2026-06-19): 7A via `apply_policed` has LANDED as a validated PoC** (commit `4d5f7bf`,
merged to `main`). `apply_policed := wait param_walker M` is admitted as a trusted walker carve-out
keyed on `checker_sig self` (finite tree → no definition cycle; works raw at runtime *and* under
check-time emulation). The `inductive`/`gated`/`eq` responds were switched from raw `param_walker
frame.motive self` to `apply_policed`; with params **PINNED** per former (the universal quant was
over-general and also sidestepped the PiParams dependent-record residual), the projecting AND
negative-former responds (Pi/Record/Sigma/dependent-Sigma) now inhabit per-former RespondShapes —
the feared higher-order spine recursor of 7B was **not** needed for these. `at FF` spine tails also
route through `apply_policed`, and `proj_op` threads the no-match field value via the sanctioned
`source (acc name)` elimination (not a forged `wait hyp_reduce …` neutral, which the stem-forge guard
rejects under emulation). Full suite 150/150, soundness 19/19, metashape pins intact. **Residual:**
dependent codomains that *branch* on the bound value (the pre-existing neutral-branching limit), and
the StrictType *wiring* itself — building per-former `RespondShape_T` and checking `respond` against it
— is not yet done (the PoC is not wired into `StrictType`).

The original framing, for history: the cheap, high-leverage experiment was **7A via sealing** — declare
`param_walker` (or one `apply_policed` helper) a trusted, typed token, then check whether the projecting
responds inhabit `RespondShape`. That landed — **and so did 7B**: the spine self-types without the feared
higher-order recursor, via `apply_policed` on the `at FF` tails plus `proj_op` threading the no-match field
through the sanctioned `source (acc name)` elimination (not a forged neutral). So §7B as written below
(an open "self-typing the spine" problem) is **superseded** — it is solved for the uniform formers.

What actually remains is therefore **not a wall**: (a) the **StrictType wiring** — build per-former
`RespondShape_T` and make `StrictType` *check* `respond` against it (validated by the PoC, just not hooked
up); and (b) one **narrow residual** — codomains that *branch* on the bound value (`λa. if … then T1 else
T2`), a pre-existing neutral-branching limit the current uniform kernel formers don't trigger. Both are
engineering / a contained extension, not the higher-order spine recursor §7B feared. The recognize-side
metacircle is done, 7A+7B are validated, and the only open frontier is the branching-codomain case (defer
to §13/`BehavioralType` if you want the spine type designed once against the stronger target).

---

## References

- Code (NB: `core.disp` was SPLIT into `cut`/`engine`/`types`): `lib/kernel/engine.disp`
  — `hyp_reduce` (the §12.6 Part-A contract), `param_walker` (the policing op of §7A),
  `inert_`/`inductive_`/`gated_inductive_respond`, `make_recognizer`, `InvalidType`;
  `lib/kernel/types.disp` — the `at` walker + wait-form cell ops (the spine of §7B), the
  structural types (`Tree`/`Frame`/`NeutralMeta`/`Action`/`RespondShape`/`MetaShape`), and
  the landed `qid`/`qid_meta` ops + `StrictType`/`CheckerResult`.
- Tests: `lib/tests/metashape.test.disp` (the respond-inhabitation wall) and
  `lib/tests/strict_type.test.disp` (the landed recognize-side metacircle, `StrictType :
  StrictType`).
- Spec: `TYPE_THEORY.typ` §11.2 (MetaShape convention), §11.4 (the three validators),
  §12.3 (InvalidType / the dead state), §12.6 (RespondShape / RecognizerShape).
- Memory: `project_kernel_self_typing_metashape`.
