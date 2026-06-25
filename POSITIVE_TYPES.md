# Positive types as a coproduct of telescopes — the inductive-former merge

**Status: Track A LANDED (2026-06-24); Track B research-open.** How to present inductive types
(Bool, Nat, Ord, List, …) generically as a **constructor spec** — a coproduct of
argument-telescopes — so the recognizer, the case-eliminator, and the coherence gate are all
*derived* from one spec by reusing machinery that already exists (the telescope walker `at`, the
§2.6 cut `annihilate`), instead of being hand-written per type. The dual of
[`NEGATIVE_TYPES.md`](NEGATIVE_TYPES.md) (the telescope = the one *negative* n-ary former); this
note is the *positive* side. Companion to [`TYPE_NORMALIZATION.md`](TYPE_NORMALIZATION.md) §10e (the
StrictType/`Type` migration and the respond-shape residuals) and [`STRICTTYPE.md`](STRICTTYPE.md)
(per-former respond typing).

> **What landed (Track A — the recognizer/eliminator merge).** `pos_cell` is in the kernel
> (`types.disp`, a positional `at` op, §5a), and `Coproduct` is now THE generic positive former:
> `Coproduct [(tag, [arg-types])]` — a **plain n-ary sum** recognized by `at` over each variant's
> positional telescope (no hand-written recognizer). **Recursion is `Mu`**, a fixpoint binder
> (NOT a `REC` sentinel — see the §-note below): a recursive datatype is `Mu (λself. Coproduct
> […self…])`, so the recursive position is an ordinary cell whose type is `self`. The eliminator is
> the surface `match`/§2.6 cut. Single-arg sums (`Action`/`CheckerResult`/`Option`/`Result`) are the
> degenerate `[T]` case — unchanged, no regression. **§5b views landed too** (`Coproduct_viewed
> view variants`, where `view` maps a shape-encoded tree to inj-tagged form): **`Ord`** (kernel) and
> **`List`** (std, was a raw predicate → now a real wait-form `Type -> Type`) are migrated onto
> specs. See `lib/tests/positive_proto.test.disp`. **Deferred:** `Nat`/`Bool` — defined *before* the
> Coproduct machinery and used as values throughout `types.disp`, so in-place migration is blocked by
> forward-ref ordering; and their *gated* responds (`coh_nat`/`coh_bool`) are the §8 coherence-gate
> self-typing = **Track B** (the sealing program; see [`KERNEL_SELF_TYPING.md`](KERNEL_SELF_TYPING.md)).
>
> **On recursion (§-note answering "is `REC` the best way?").** No — recursion is now `Mu`, the least
> fixpoint, *not* a magic marker + substitution pass. `Coproduct` is a plain sum (no `self`-threading,
> no `fill_rec`); `Mu F` recognizes `v` by unfolding one level (`(F self) v`, `self = Mu F`, knot
> tied by `make_rec_recognizer`, neutrals via the H-rule). This is the μ-types / initial-algebra
> presentation: recursion is orthogonal to summing, `self` is a real λ-binder, and it composes
> (nested/mutual recursion, codata). So "rec on the telescope" is *not* a special cell op — the
> recursive cell is an ordinary one whose type happens to be the `Mu`-bound `self`.

## TL;DR

A coproduct value is literally a pair — `inj tag pay = t tag pay = pair tag pay` (`cut.disp:11`). So a
coproduct **is a dependent sum over its tag**: `Coproduct variants = Σ (tag : Tags). Payload(tag)`,
where `Payload = λtag. lookup_arm variants tag` is a *case-table* family. Since `Sigma` is already a
telescope (`Sigma A B = Telescope [proj "fst" A ; proj "snd" (B a)]`, `types.disp:320`), a coproduct's
**recognition** is a 2-cell telescope walk `[ tag : Tags ; payload : argTele(tag) ]`, and an inductive
is **nested telescopes** — an outer Σ over the tag whose payload cell's type is the inner
argument-telescope. The positive **case-eliminator** is the §2.6 cut you already have:
`annihilate cases v = (proj cases (pair_fst v)) (pair_snd v)` (`cut.disp:37`). Raw value representations
(Nat's `leaf`/`fork`) are kept via a per-type **view** (`from`/`to` iso). One spec → three derived
folds; the only genuinely new substrate is a **positional cell op** for `at` (so raw pairs are
walkable without boxing into named records).

**Validated this session** (`lib/tests`, scratch): `Sigma String (λtag. match (lookup_arm variants tag)
{ Ok T => T; Err => False })` recognizes `{fst:=tag; snd:=pay}` *iff* `pay` inhabits the tag's payload
type — `L→Nat`, `R→Bool`, dispatched correctly through `at`'s dependent second cell; an unknown tag
yields payload type `False` and is rejected. So the structural heart runs in the real kernel.

## 0. Where this fits: the polarity picture

`NEGATIVE_TYPES.md` established the telescope as the one negative former — Pi/Sigma/Record/⊤ — presented
by **projection** (`at` walks cells, the respond projects/applies). This note's claim is that the
*positive* formers (sums / inductives) need **no separate recognition machinery**: a coproduct's
*membership* is a Σ (hence a telescope), and its *elimination* is the §2.6 cut. Concretely an inductive
uses **both faces**:

| face | operation | machinery | role |
|---|---|---|---|
| negative (projection) | "which constructor? what are its args?" | telescope walk `at` | **recognition** |
| positive (case) | "do this per constructor" | the cut `annihilate` | **elimination** |

The cut is the bridge: `annihilate cases v` projects the case for `v`'s tag and applies it to the
payload — case-analysis *built from* projection. So the kernel already contains both halves; the merge
is to route inductives through them generically instead of hand-writing a recognizer/dispatcher/coh per
type.

## 1. The core construction — Coproduct is a Σ over the tag (validated)

`inj` is `pair`. A coproduct value `inj tag pay` is the pair `(tag, pay)`, eliminated by
`pair_fst`/`pair_snd`. So:

```
Coproduct variants  ≅  Σ (tag : Tags). Payload(tag)
   Tags     = the (finite) tag set                       -- see §1a; often just String + the Err→False trick
   Payload  = λ tag. match (lookup_arm variants tag) { Ok T => T ; Err _ => False }
```

`Payload` is a **case table**, not a smooth function: it triages the variant list on the concrete tag
(`lookup_arm`, `types.disp:325`) and returns that constructor's payload type, or `False` for an unknown
tag. The classic `A + B ≅ Σ(b:Bool). if b then A else B` generalized to an n-ary tag with `if`/`else`
replaced by `lookup_arm`.

**§1a. Tag validity comes for free.** Because `Payload(bogus) = False` (the empty type) and nothing
inhabits `False`, an ill-tagged value `inj "bogus" x` is rejected at the payload cell *even if* `Tags`
is the permissive `String`. So you may take `Tags = String` and let the family enforce validity, or use
a real finite enum `Tags = { c₁ … cₙ }` (rejects one cell earlier). Both are correct; the validation
used `String`.

**Validation (this session).** `EitherSig = Sigma String Payload` with `variants = [("L",Nat),("R",Bool)]`:
`typecheck EitherSig {fst:="L"; snd:=zero} = Ok TT`, `{fst:="L"; snd:=TT} = Ok FF`,
`{fst:="R"; snd:=TT} = Ok TT`, `{fst:="R"; snd:=zero} = Ok FF`. The dependent second cell dispatches the
payload type on the tag — exactly the load-bearing claim.

## 2. Multi-argument constructors → nested telescopes

A constructor with several arguments carries them as a **telescope** (its payload is a record of args).
Since `Sigma = Telescope` and `Payload(tag)` is itself a telescope, an inductive is **nested telescopes**:

```
inductive  =  Telescope [ tag : Tags ; payload : argTele(tag) ]      -- OUTER: which constructor (Σ over tag)
   each argTele(c)  =  Telescope [ a₁ : T₁ ; … ; aₖ : Tₖ ]            -- INNER: c's arguments
```

Examples (spec form):

```
Bool   = [ ("true", ⊤) , ("false", ⊤) ]
Nat    = [ ("zero", ⊤) , ("succ", [ pred : REC ]) ]
List A = [ ("nil",  ⊤) , ("cons", [ head : A , tail : REC ]) ]
```

`REC` marks "the type being defined" (resolved to `self` at recognition time, §4). The inner telescope
is a genuine `Telescope` (not a flat `Record`) precisely so that **dependent constructors** — a later
arg whose type mentions an earlier arg's value (Σ-shaped, e.g. `mk : (n:Nat) -> Vec n -> …`) — are
expressible. For Bool/Nat/List the inner telescopes happen to be non-dependent.

## 3. The three components, derived from one spec

### 3a. Recognizer — `at` over the nested telescope

The current `Coproduct` recognizer (`types.disp:331`) is `lookup_arm meta (pair_fst v)` then `handler
(pair_snd v)`. The generic recognizer is the **one-line generalization** — replace the single-type
payload check with a telescope walk:

```
{ self, meta, v } ->                                              -- make_rec_recognizer threads self (for REC)
  match (lookup_arm meta.recognizer_params (pair_fst v)) {        -- coproduct: dispatch on the tag
    Ok argTele => at TT (fill_rec argTele self) (pair_snd v) t t  -- telescope: check the args record
    Err _      => Ok FF                                            -- unknown tag
  }
```

- `lookup_arm` is **unchanged**; it just carries an `argTele` per variant instead of a single type.
- `at TT argTele payload t t` is the **same `at`** the Telescope recognizer calls (`types.disp:113`).
- A neutral value is recognized by the H-rule in `recognizer_wrap` (`engine.disp:30`) *before* the body
  runs, so the spec body only ever sees concrete values.
- Empty `argTele` (nullary constructors) → `at TT t … = Ok TT` for free.

The *whole* recognizer (tag + args) can equivalently be expressed as the §2 nested telescope walked by
`at` directly, once positional cells exist (§5): outer cell projects the tag, the dependent payload cell's
type is `argTele(tag)`, walked as an inner telescope.

### 3b. Case-eliminator — the §2.6 cut, generalized

For a single-payload constructor, the dispatcher is already `annihilate` (`cut.disp:37`):

```
annihilate cases v = (proj cases (pair_fst v)) (pair_snd v)   -- project the case for v's tag, apply to payload
```

For multi-arg constructors the payload is a *record*, so the dispatcher must **spread** it into the case's
arguments and **recurse** on the recursive positions (via `elim`, so a neutral sub-term stays a clean
stuck elimination):

```
dispatch motive cases v =
  let c = pair_fst v ; args = pair_snd v
  cases.c  (field args n₁) … (field args nₖ)  (elim … on each REC position)
```

`derive_dispatcher(spec)` reads the arg-names/positions from the spec's `argTele(c)` to know what to
project and which positions recurse. So `annihilate` is the degenerate (single-arg) case; the general
dispatcher is "project the case, spread the args, recurse."

### 3c. Coherence gate — a telescope of case-types

The cases must inhabit the motive at each constructor. Those case-*types* form a telescope, derived from
the spec + motive (§4 schema), and the gate is just `at TT` over it:

```
gate spec P cases  =  at TT (derive_case_telescope spec P) cases t t
```

Note: **no coproduct here** — the cases are one record, checked against one telescope. The coproduct only
appears in the recognizer/dispatcher, where a *value* is being decoded ("which constructor is this?").

## 4. The recursor schema — a telescope→telescope transform

`derive_case_telescope` is the one piece of real logic. For each constructor `c` with argument telescope
`[a₁:T₁, …, aₖ:Tₖ]` and recursive positions `R ⊆ {1..k}`:

```
case-type(c, P)  =  Π a₁:T₁. … Π aₖ:Tₖ.   (for each j ∈ R:  P aⱼ →)   P (c a₁…aₖ)
```

i.e. take the arg-telescope, Π-bind each argument, **insert one IH premise `P aⱼ` per recursive
position**, conclude `P (c args)`. Run across all constructors → the case-telescope. Worked:

```
Nat:    [ base : P zero ,  step : Π n. P n → P (succ n) ]
List A: [ nil  : P nil  ,  cons : Π h. Π t. P t → P (cons h t) ]
Tree:   [ leaf : P leaf ,  node : Π l. Π r. P l → P r → P (node l r) ]   -- two rec args → two IHs
```

`REC` markers in the arg-telescope are exactly the positions that get an IH premise. This is the standard
"recursor type computed from the constructor signatures," expressed as a map on telescopes.

## 5. Representation — keeping raw values

The structure above is the *logical* shape; values are stored as trees, and disp's encodings are compact
and non-uniform. Two mechanisms keep them:

**5a. Positional cells (the one new substrate op).** `inj tag pay = pair tag pay` is a *raw fork*, not a
`mk_record`; today's `proj_cell` looks up by name (`lookup_field`, needs names). A **positional cell**
`pos_cell i T` that observes `pair_fst`/`pair_snd` directly lets `at` walk a raw pair:

```
cons h tl = inj "cons" (pair h tl)
  recognized by  [ pos₀ : Tags ; pos₁ : [ pos₀ : A ; pos₁ : REC ] ]   -- all raw nested pairs, zero boxing
```

This is the `at` op CLAUDE.md already lists as planned ("positional → tuples"). It plugs in as a new
wait-form cell op with no `at` edit, exactly like the existing `mint`/`proj`/`apply`/`qid` ops.

**5b. Views for shape-tagged types (Nat).** Nat's tag lives in the *shape* — `zero = t` (leaf),
`succ n = fork(t, n)` — not as a pair component, so positional cells alone can't read it. The spec carries
a **view** (`from`/`to`, à la GHC `Generic`):

```
view_nat  v  = triage ("zero", ⊤-args)
                      (NOT_A_NAT)
                      ({l,r} -> if (tree_eq l t) then ("succ", { pred := r }) else NOT_A_NAT)  v
build_nat (tag, args) = (the inverse)
```

The generic recognizer/dispatcher run on `view v`; raw `leaf`/`fork` storage is untouched, and `view_nat`
costs a triage + a `tree_eq` — the same as today's hand-written recognizer. **Soundness obligation:**
`from`/`to` must be a bijection on the type's values. So: pair-tagged inductives use positional cells
directly (raw, no view); shape-tagged ones (Nat/Bool/Ord) supply a view. Either way the representation
stays as compact as the cut already makes it — no forced migration to boxed `inj`-records.

## 6. The respond — Coproduct is a *hybrid*, not a pure telescope

A crucial nuance. Recognition is a telescope walk (§3a), but a coproduct neutral's **elimination is
case-analysis** (positive), so its respond stays `inductive_respond` — `Extend (motive self)`,
`engine.disp:140` — *not* the negative former's `at FF` (projection) respond. So:

> **Coproduct = telescope RECOGNIZER + positive (`inductive_respond`) RESPOND** — a hybrid wait-form,
> exactly like `StrictType` (telescope recognizer + H-rule respond) or `Record` (shares the `at` walk,
> keeps its own wait-form).

A coproduct neutral *can* also support the negative projection face (project the tag — discriminate which
constructor without knowing it), but that's a *second* elimination mode on the same neutral. Exposing
both (project-tag and case-analyze) on one neutral is the **mixed-frame / callable-record frontier**
(the respond would dispatch on the frame: `acc name` → project, `{motive;cases}` → eliminate). Defer it;
the default respond is the positive eliminator. This is why the merge does **not** make Coproduct a pure
`Telescope` instance: only the recognize face is the telescope.

## 7. Implementation map

**The spec** (carried in the type's meta, `recognizer_params`):

```
spec = { variants : [ (tag, argTele-with-REC-marks) ]   -- §2; argTele built from proj/pos cells
       , view     : (Tree -> (tag, args)) option         -- §5b; identity for inj-tagged types
       }
```

**New substrate:** one positional cell op `pos_cell` for `at` (§5a). Nothing else.

**Generic derivations** (in-language, over the spec):

| function | builds | reuses |
|---|---|---|
| `derive_recognizer` | tag-dispatch + `at TT` per-branch argTele (+ view, + H-rule) | `lookup_arm`, `at`, `recognizer_wrap`, `fill_rec` |
| `derive_dispatcher` | project case + spread args + recurse | `annihilate`/`proj`, `field`, `elim` |
| `derive_case_telescope` | the §4 schema (Π-bind + IH insert) | telescope/`Pi` constructors |
| `fill_rec` | substitute `self` for `REC` in an argTele | structural tree rewrite |

**Reused as-is:** `at` (recognition), `annihilate`/the cut (case-elim), `recognizer_wrap`'s H-rule
(neutrals), `elim` (recursion routing), `Sigma`/`Pi`/`Telescope` (structure). The hand-written per-type
recognizer body, `coh_nat`/`coh_bool`, and `nat_dispatcher`/`bool_dispatcher`/`ord_dispatcher` all
collapse into the three folds.

## 8. Validation status (what's solid vs what's a bet)

- **Validated (runs in-kernel this session):** Coproduct = Σ over the tag with a `lookup_arm` case-table
  family; `at`'s dependent second cell dispatches the payload type on the tag; unknown tag → `False` →
  rejected.
- **Code facts:** `inj = pair`; `Sigma = Telescope`; `annihilate` = the cut case-eliminator;
  `recognizer_wrap` H-rule for neutrals.
- **To build (mechanical, low-risk):** the positional cell op; the spec representation; the four generic
  derivations; `fill_rec`; Nat's view.
- **Hypothesis (unvalidated, higher-risk):** the **coherence gate self-types** when expressed as `at TT
  (case-telescope) cases`, because each IH premise becomes a Pi-typed cell and the IH-minting is delegated
  to the (already strictly-typed) Pi recognizer instead of `coh_nat`'s hand-rolled `bind_hyp`. This is the
  proposed dissolution of the recursive-eliminator residual (TYPE_NORMALIZATION §10e / "move 3"). It is
  *independent* of the recognizer/dispatcher merge — those land regardless — so this bet can be tested
  separately and need not block the rest.

## 9. Staging

1. **Positional cell op.** Add `pos_cell` to `at`; validate that a raw `inj`-pair is recognized by a
   2-cell `[ pos₀ : Tags ; pos₁ : T ]` telescope. *Smallest, unblocks everything.*
2. **Coproduct as the Σ-telescope.** Re-express the existing `Coproduct` recognizer as the 2-cell Σ over
   the tag with the `lookup_arm` family (validated structurally); confirm extensional equivalence to the
   current `lookup_arm + handler` form.
3. **Multi-arg constructors + a fresh inj-tagged datatype** end to end (recognizer + dispatcher via the
   nested telescope + generalized `annihilate`). Avoids touching Nat. *Proves the merge.*
4. **Views.** Present Nat by spec + `view_nat`, keeping raw `leaf`/`fork`; validate extensionally against
   the current Nat recognizer/dispatcher.
5. **Coherence gate.** Build `derive_case_telescope`; test the §8 self-typing hypothesis (the
   make-or-break experiment for the recursive residual).
6. **Unify the zoo.** Move Bool/Nat/Ord/Coproduct (and std List/Option/…) onto specs; retire the bespoke
   recognizer bodies, `coh`s, and dispatchers.

## 10. Gaps and open questions

- **Strict positivity.** The §4 schema is only well-founded if recursive args occur strictly positively
  (no `(List A → Bool) → …`). `derive` should enforce (or the spec format should preclude) negative
  recursive occurrences.
- **Nested recursion.** A rose tree `node : { val : A, kids : List (Rose A) }` has its recursive occurrence
  *under* `List`, so the IH is not `P kids` but "`P` at every element" — the naive §4 schema needs `List`'s
  functoriality (a `map`/`All`). Parameters-only, single-level recursion first.
- **Indices vs parameters.** `List A` is parameterized (A fixed across constructors); indexed families
  (`Vec n`) have the motive `Π indices. I indices → Type` and constructors landing at specific indices — a
  strictly harder schema. Start parameter-only.
- **Mixed-frame respond (§6).** Exposing both project-tag and case-analyze on one coproduct neutral is the
  callable-record frontier; defer until a consumer needs the negative face.
- **Cost.** A spec-driven recognizer may be slower per check than a bespoke one (cf. the strict-universe
  lesson, TYPE_NORMALIZATION §10e — generality costs steps). Measure against `APPLY_BUDGET`; if it bites,
  optimize the *evaluator* (`apply`/hash-cons/memoization), never by special-casing away the generality.

## References

- Validated facts (this note): `Sigma String (λtag. lookup_arm-payload)` recognizes tagged values with
  per-tag payload dispatch (`L→Nat`, `R→Bool`); unknown tag → `False` → rejected.
- Code: `cut.disp` — `inj`/`pair`, `annihilate`/`proj`/`prod`, `lookup_field`; `types.disp` — `Coproduct`
  (`lookup_arm`), `Sigma`/`Telescope`/`Pi`, `at` + cell ops, the Nat/Bool/Ord recognizers + `coh`s +
  dispatchers/`nat_rec`; `engine.disp` — `recognizer_wrap` (H-rule), `inductive_respond`/
  `gated_inductive_respond`, `elim`.
- Companions: `NEGATIVE_TYPES.md` (the telescope / negative former — this note's dual), `TYPE_NORMALIZATION.md`
  §10e (StrictType/`Type` migration, the recursive-eliminator residual = the §8 hypothesis), `STRICTTYPE.md`
  (per-former respond shapes).
```
