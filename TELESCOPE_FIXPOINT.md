# The Self-Aware Telescope — recursion and corecursion as cells (Path B)

**Status: LANDED (2026-06-25).** The entire type system runs on one self-aware walker `at`, with a single
cell vocabulary: data fields, the negative-former observations, **and** recursion (μ) / corecursion
(ν) as cells. `Mu`/`Mu_resp` are **retired**; recursion is a cell, the fixpoint's polarity matches the
shape, and the respond follows. Companion to [`POSITIVE_TYPES.md`](POSITIVE_TYPES.md) (Track A, landed),
[`NEGATIVE_TYPES.md`](NEGATIVE_TYPES.md) (the telescope), and [`KERNEL_SELF_TYPING.md`](KERNEL_SELF_TYPING.md)
(Track B / sealing — orthogonal, unchanged by this).

> **What actually shipped (and the one refinement vs. this proposal).** Integration landed in six
> test-gated steps (`lib/kernel/types.disp`, `lib/std/stream.disp`, `lib/std/list.disp`):
> 1. `at` gained `rs` (the recursion environment), threaded to every cell op. The negative/positional
>    ops ignore it (`Pi`/`Sigma`/`Record` pass a dummy `t`); validated free — `metashape` self-typing
>    unchanged.
> 2. **Recursion is one functor-parameterized cell `RecUnder F`** (checks `acc source : F self`), with
>    `Rec := RecUnder Id`. This is the refinement: the original proposal's bare `rec` cell handled only
>    *direct* recursion; **`RecUnder F` also covers NESTED recursion** (`Call Expr (List Expr)` = arg
>    lists, ubiquitous in real ASTs), which `Mu`'s self-as-value binder could do and a bare `rec` marker
>    could not. So retiring `Mu` is no longer a downgrade. (Direct = `RecUnder Id`; nested = `RecUnder List`.)
> 3. The sum former is self-aware + respond-parametric: `Coproduct_resp`/`Coproduct_viewed_resp` thread
>    `rs = self` and take the respond directly (absorbing `Mu_resp`'s role — Nat/Bool keep their gated
>    coherence respond). `Nat`/`Bool`/`Ord`/`List` migrated to `Rec` cells; **`Mu` deleted**.
> 4. **Codata**: `Stream` (`lib/std/stream.disp`) = `gen` cell over one observation, respond delegating
>    to the observation `Record [head:A; tail:self]`. `unfold`/`out`/`stream_take`/`stream_map`.
> 5. **Mutual recursion**: `Coproduct_ctx` threads `rs = ctx` (a context function) and `RecAt i` cells
>    recurse into `ctx i` — validated on the *real* kernel (Tree/Forest + an Expr/Stmt parser AST).
> 6. The walker `at` is now exported (userland formers — e.g. `Stream` — build cells and walk them).
>
> Validation: `lib/tests/path_b_{inductive,codata,mutual}.test.disp` (12+13+15 green) plus the full
> suite green; the 6 `*_proto.test.disp` files are superseded and removed. **Still deferred** (§8):
> general `Nu F` over an arbitrary `functor` meta-field (Stream hardcodes head/tail); a typed
> corecursor for full coinductive soundness; gated-respond self-typing stays the Track B residual.

## TL;DR

Today (post Track A): recognition is unified — one `at` walks Pi/Sigma/Record (negative) and, via
tag-dispatch + positional cells, Coproduct (positive). But **recursion** lives in a separate wrapper
`Mu (λself. …)`, the formers are **self-less**, and **codata** doesn't exist. This proposal makes
`at` **self-aware** (it threads `self` = the type being defined). Then:

- **Recursion is a cell.** `rec acc` checks `acc source : self` (RECURSE — finite, terminates).
  The former ties its own knot (`make_rec_recognizer`); no `Mu` wrapper.
- **Mutual recursion is a cell too.** `rec_at i acc` indexes a threaded **context function**
  `ctx : i -> recognizer`; `rec` is the 1-entry case. This *must* resolve at walk time, which is also
  what dodges the build-time loop an eager tuple-fixpoint would hit.
- **Corecursion (codata) is a cell.** `gen acc` GUARDS (checks `acc source` is a generator) instead
  of descending. A codata value is a finite **coalgebra** (`unfold step seed`); you recognize the
  generator, never the infinite value. The respond OBSERVES (`out`) one step.
- **The respond carries polarity.** A sum/`rec` former eliminates by CASE (`inductive_respond`); a
  product/`gen` former eliminates by PROJECTION (`at FF`), the codata respond *delegating* to the
  observation `Record [head:A; tail:self]`. Both self-type.

So the whole basis is `{Σ sum, Π product} × {μ, ν}`, expressed as cells on **one** walker, with the
respond's polarity matching the corner. Adding a connective = adding a cell op.

## 0. What changed vs. today

| | today (landed) | this proposal |
|---|---|---|
| recognition engine | one `at` (self-less) | one `at` (self-aware: threads `self`) |
| negative formers | cells: `mint`/`apply`/`proj`/`pos`/`qid`/`deriv` | unchanged (ignore `self`) |
| sum | `Coproduct` (dispatch + `pos` cells) | unchanged |
| **direct recursion** | `Mu (λself. …)` wrapper | **`rec` cell** + self-aware former |
| **mutual recursion** | (would need a tuple `Mu`) | **`rec_at i` cell** + context function |
| **codata** | — | **`gen` cell** + coalgebra values + `unfold`/`out` |
| respond polarity | per former (case vs `at FF`) | per cell/former; codata delegates to a `Record` |

## 1. The self-aware walker

`at` gains one parameter, `self` (call it `rs` in code — the recursive type, or a context function
for mutual). It threads it to every cell op and through its own recursion. **Verbatim** the kernel
`at`, plus `rs`:

```
at = fix ({go, rs, mode, tele, source, frame, prior} ->
  if (is_fork tele) then {
    let cell = pair_fst tele
    match (cell mode source prior frame rs) {                 -- thread rs to the cell
      SMint ty  => (if mode then (bind_hyp ty ({h} -> go rs TT ((pair_snd tele) h) source frame h))
                            else (match (apply_policed (pair_snd tele) frame) { Ok rest => go rs FF rest source frame frame; Err _ => Extend InvalidType }))
      SThread x => (if mode then (go rs TT ((pair_snd tele) x) source frame x)
                            else (match (apply_policed (pair_snd tele) x)    { Ok rest => go rs FF rest source frame frame; Err _ => Extend InvalidType }))
      SReject _ => Ok FF
      SDone a   => a } } else (if mode then (Ok TT) else (Extend InvalidType)))
```

Every cell op gains `rs` as a trailing param. The existing **negative** ops ignore it (byte-identical
otherwise — `mint_op`/`apply_op`/`proj_op`/`pos_op`/`deriv_op`/`qid_op`). Two new ops **use** it.

## 2. The cell vocabulary (the whole type theory)

| cell | mode TT (recognize) | mode FF (respond / eliminate) | used by |
|---|---|---|---|
| `mint A` | mint a ∀-hyp | land codomain | Pi (∀-intro) |
| `apply ty` | check `source prior : ty` | `Extend ty` | Pi (codomain) |
| `proj name ty` | honest field lookup | project `name` | Record/Sigma |
| `pos acc ty` | check `acc source : ty` | `Extend ty` | constructor args (Track A) |
| `qid ty` / `qid_meta ty` | check `v` / `type_meta v : ty` | subsumption | Intersection, StrictType |
| `deriv name recipe` | present & pinned | `Reduce recipe` | derived fields |
| **`rec acc`** | check `acc source : self` (RECURSE) | `Extend self` | μ recursive position |
| **`rec_at i acc`** | check `acc source : (ctx i)` | `Extend (ctx i)` | mutual recursion |
| **`gen acc`** | check `is_gen (acc source)` (GUARD) | `Extend self` (observe) | ν corecursive position |

The new ops:

```
rec_op = {meta, mode, source, prior, frame, rs} ->
  if mode then (match (rs (meta.acc source)) { Ok b => (if b then (SThread (meta.acc source)) else SReject); Err _ => SReject })
          else (SDone (Extend rs))                                  -- self = the recursive type
gen_op = {meta, mode, source, prior, frame, rs} ->
  if mode then (if (is_gen (meta.acc source)) then (SThread (meta.acc source)) else SReject)
          else (SDone (Extend rs))
```

`rec` *recurses* (sound because μ is well-founded — finite). `gen` *guards* (it must not descend — ν
is infinite). That single difference is the whole μ/ν distinction.

## 3. The duality (`{Σ,Π} × {μ,ν}`)

| | **μ (inductive — `rec`)** | **ν (coinductive — `gen`)** |
|---|---|---|
| a value is | a finite **constructor-tree** | a finite **generator** (coalgebra) |
| introduce | constructors | `unfold step seed` |
| eliminate | fold (case + recurse) | `out` (observe one step) |
| recognized by | **structure** (walk it; terminates ∵ finite) | **construction** (a typed coalgebra; finite repr, infinite behavior) |
| respond | **case** (`inductive_respond`) | **projection** (`at FF`, via observation `Record`) |
| one-level shape | **Coproduct** (sum) | **Record** (product) |
| examples | Nat, List, Ord, Tree/Forest | Stream, processes |

The walker is shared across all four corners; the *fixpoint's polarity* (μ recurse / ν guard) and the
*respond's polarity* (case / projection) are what differ — and both are now driven by the cell.

## 4. Worked examples (all validated — see §6)

**Direct recursion (μ).** The former ties its own knot; `rec` is the recursive position:
```
NatList = SumRec [ pair "nil" [], pair "cons" [ pos pair_fst Nat, rec pair_snd ] ]
```

**Mutual recursion (μ).** A **context function** `ctx : i -> recognizer`; `rec_at i` indexes it. The
context is a `fix` *function* (driven by application — a `fix` over a data pair stays stuck), and the
recognizers *capture* `ctx` without forcing the others, so building doesn't loop:
```
mkctx A = fix (λself i. if i=0 then SumRec [leaf:[pos I A],  node:[rec_at 1 I]]
                                else SumRec [empty:[], cons:[rec_at 0 pair_fst, rec_at 1 pair_snd]])
Tree A = (mkctx A) 0 ;   Forest A = (mkctx A) 1
```

**Codata (ν).** A value is a coalgebra `unfold step seed`; recognition GUARDS (`gen`); the respond
OBSERVES, delegating to the observation `Record`:
```
unfold step seed = inj "gen" (pair step seed)
out v            = let (step,seed) = unpack v in { head := (step seed).head ; tail := unfold step (step seed).tail }
Stream A = recognize: if is_gen v then  at TT [ pos head A ; gen tail ] (out v)  else Ok FF
           respond:   {p,self,frame} -> delegate to (Record [head:A; tail: neutral_type self]).respond
```

**Negative formers are unchanged** — Pi/Sigma/Record are the same cells on the now-self-aware `at`
(they ignore `self`).

## 5. The respond (elimination), and why it self-types

Eliminating a neutral runs the former's respond (`hyp_reduce`). Polarity:

- **Sum / `rec` →** `inductive_respond`: `Extend (motive self)` (case-analysis). Unchanged.
- **Product / `gen` →** projection. The codata respond **delegates** to the observation
  `Record [head:A; tail:self]` (with `self = neutral_type self`), so observing `tail` lands
  `Extend (Stream)` and `head` lands `Extend A`. This is the *delegating-respond* idea: a fixpoint's
  respond can delegate to its unfolded body's respond, inheriting the body's polarity.

**Self-typing (the StrictType metacircle) survives, proven empirically:**
- *Recognize face* is the **H-rule** (`recognizer_wrap` short-circuits on the hyp candidate), so it
  never runs the walker body on a hyp — it's robust to `self`-threading.
- *Respond face*: `at FF` (negative) self-types; `inductive_respond` (case) self-types; and the
  **codata projection respond self-types** (it bottoms out in `Record`'s `at FF`). Crucially,
  **Pi/Record rebuilt on the self-threaded `at` still self-type on both faces** — threading `self`
  through the real walker is free.

## 6. What's validated (prototype ledger — all green, flip-verified)

| file | proves |
|---|---|
| `lib/tests/mu_delegate_proto.test.disp` | a fixpoint's respond can delegate to its body's polarity; self-types |
| `lib/tests/nu_proto.test.disp` | codata as a coalgebra — recognize the finite generator, observe forever, no loop |
| `lib/tests/telescope_fixpoint_proto.test.disp` | one self-aware walker; `pos`/`rec`/`gen` cells; μ + ν recognition, no wrappers |
| `lib/tests/path_b_respond_proto.test.disp` | `rec`→case respond, `gen`→projection respond (delegates to `Record`); both self-type |
| `lib/tests/at_self_threaded_proto.test.disp` | the **real `at`, self-threaded**, preserves Pi/Record self-typing (recognize + respond), and runs `rec`/`gen` |
| `lib/tests/mutual_rec_proto.test.disp` | mutual recursion via `rec_at i` + a context function; no build loop |

## 7. Problems it solves

- **No `Mu`/`Nu`/`Mu_resp` formers.** Recursion and corecursion are *cells*; the former ties its own
  knot. The fixpoint *operators* disappear for the common (direct/mutual/codata) cases.
- **One walker for the entire type theory** — positive, negative, inductive, coinductive. Any future
  meta-operation (type normalizer, the optimizer, NbE) walks *one* structure and gets recursion for
  free. This is the concrete payoff that justifies `self`-in-the-walker.
- **Codata, properly** — recognize the finite generator (not the infinite value), observe by `out`,
  with a projection respond. The dual of `Mu` completes the framework.
- **Mutual recursion**, which an eager explicit tuple-fixpoint *can't* express (it loops at build) —
  `rec_at` over a walk-time-resolved context handles it.
- **Maximal "types are open wait-forms."** Adding a connective = adding a cell op, no walker edit.

## 8. What's left (honest)

- **General `gen`/`fmap`.** The Stream prototype hardcodes "re-suspend `tail`". Arbitrary `Nu F`
  needs `F`'s map to know which positions recurse — exactly the dormant **`functor` meta-field**
  (`make_meta`'s `functor`, currently `trivial_functor`). Codata is what makes it load-bearing.
- **Codata soundness = typed introduction.** `gen`'s one-step guard is a sanity check; full
  coinductive soundness rests on a typed `unfold : (S -> F S) -> S -> Nu F` (correct-by-construction),
  not on structural recognition. Needs the corecursor typed.
- **Gated types stay Track B.** Nat/Bool's coherence-gated case respond is expressible here (a
  self-aware sum with a gated respond) but the *gate's* self-typing is still the sealing residual —
  orthogonal, untouched.
- **Perf.** `self` + more cell kinds; the generic walker is already ~2× the specialized ones. Measure.

## 9. Integration plan (for the landing session)

1. **Thread `self` through the kernel `at` + every cell op** (negative ops ignore it). Re-run the
   full `.disp` suite + `metashape.test` (Pi/Sigma/Record respond self-typing) — proven free in
   `at_self_threaded_proto`, so this is mechanical.
2. **Add `rec`/`gen` cell ops + constructors** (`rec_cell`/`gen_cell`), and `rec_at` over a context.
3. **Make `Coproduct` self-aware** (it already uses `make_recognizer`; switch to `make_rec_recognizer`,
   thread `self` to `at`) so `rec` cells work; keep `inductive_respond`.
4. **Migrate the recursive types** `Nat`/`List`/`Ord` from `Mu (λself. Coproduct […])` to `rec`
   cells. (Nat/Bool keep their gated responds.) Re-verify identical recognition + the metashape
   residual.
5. **Add `Nu`/codata**: `gen` cells, `unfold`/`out`, the projection respond delegating to `Record`,
   and wire the `functor` field for general `fmap`.
6. **Decide `Mu`'s fate.** With `rec`/`rec_at` covering direct + mutual recursion, `Mu` can be
   retired — *or* kept as sugar. Recommend retiring once the migrations land.

Land §1–§4 first (they only unify what already exists, no new semantics); §5 (codata) is additive and
can follow once the `functor` field is wired.

## References

- Prototypes: `lib/tests/{mu_delegate,nu,telescope_fixpoint,path_b_respond,at_self_threaded,mutual_rec}_proto.test.disp`.
- [`POSITIVE_TYPES.md`](POSITIVE_TYPES.md) (Track A: pos_cell + Coproduct + Mu — landed), [`NEGATIVE_TYPES.md`](NEGATIVE_TYPES.md)
  (the telescope / `at`), [`KERNEL_SELF_TYPING.md`](KERNEL_SELF_TYPING.md) (Track B / sealing — orthogonal).
- Kernel: `lib/kernel/types.disp` (`at`, the cell ops, `Coproduct`, `Mu`, `Telescope`/`Record`),
  `lib/kernel/engine.disp` (`make_rec_recognizer`, `inductive_respond`, `apply_policed`, `hyp_reduce`).
