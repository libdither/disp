# The Wrapping Invariant — problem, fix, and the road to metacircular checking

**Status:** open problem. The in-language checker is sound where it terminates
cleanly, but has **false-negatives on nested neutral application**. Because of
that, elaborator verification is currently **advisory** (`src/compile.ts`
`compileBinding`: a non-`Ok TT` verdict is *not* a hard error). This document
explains the bug precisely, catalogs the fixes tried (and why each is subtle),
proposes the real fix, and describes what a clean checker unlocks.

Audience: anyone touching `lib/kernel/core.disp`'s walker, recognisers, `elim`,
or the elaborator's verification path. Read `TYPE_THEORY.typ` §3 (CheckerResult)
and §7.5 (the wrapping invariant) first; this file is the implementation reality
that diverges from that spec.

---

## 1. Background: the CheckerResult monad lives in the tree

The kernel threads results through a `CheckerResult` monad, but that monad is
**encoded as ordinary trees** (§2.6 coproducts), in the *same* tree space as
object values:

```disp
Ok   := {v} -> inj "Ok" v          // = fork("Ok",  v)
Fail := inj "Fail" t               // = fork("Fail", t)
is_ok    := {r} -> tree_eq (pair_fst r) "Ok"
ok_value := {r} -> pair_snd r
must_ok_or_self := {r} -> match r { Ok v => v ; Fail _ => Fail }   // peels ONE Ok
```

The dispatcher is `param_apply f x = must_ok_or_self (walk f x)`. `walk` is an
**`Ok`-wrapped reducer**: every successful arm returns `Ok <reduct>`, and
`param_apply` peels exactly one `Ok` off the top.

The composition combinators rely on that. The S-rule (`w_s`, reducing
`(c x)(b x)`) peels one `Ok` off each sub-reduction before re-applying:

```disp
let cx = self c x            // = Ok <reduct-of (c x)>
let bx = self b x
... self (ok_value cx) (ok_value bx)    // peel one Ok, re-apply
```

This is clean **iff every arm returns `Ok <bare-reduct>`** — i.e. iff the thing
inside the `Ok` is a genuine object value (a stuck neutral, a constructor, a
record), never itself an `Ok`.

## 2. The intended invariant (spec §7.5)

> `param_apply f x` evaluates `f x` to normal form and returns that normal form
> directly. A pure reduction returns its bare result (`param_apply (add 2 3) → 5`).
> A reduction whose head is a CheckerResult-producer returns that producer's
> CheckerResult (`param_apply Bool TT → Ok TT`). Results are **never nested** —
> `Ok (Ok _)` does not arise.

So three intended shapes:

| call | intended normal form |
|---|---|
| `param_apply (add 2 3)` | `5` (bare) |
| `param_apply Bool TT` | `Ok TT` (recogniser verdict) |
| `param_apply hyp_Pi zero` | `Ok stuck` (a CheckerResult holding the stuck neutral) |

## 3. Where reality diverges: the routed arm double-wraps

`walk`'s seven arms (`lib/kernel/core.disp`, `let walk = ...`) wrap **bare**
reducts — `Ok (pair_fst x)`, `Ok (neutral_type x)`, `Ok x`, `Ok (t x)`, etc. —
except one. The **routed** arm:

```disp
TT => Ok (f x)    // f is a kernel op: a neutral (hyp_reduce) or a bind_hyp wait-form
```

For a kernel op, `f x` is the *handler's* result, which is **itself a
CheckerResult**. `hyp_reduce` returns `Ok (wait self …)` for `Extend` and
`Ok v` for `Return` (`core.disp` `hyp_reduce`). So:

```
walk(neutral, frame)  =  Ok (f frame)  =  Ok (Ok stuck)     ← DOUBLE-WRAPPED
```

It is the only arm that wraps a value that is already `Ok`. That single extra
`Ok` is the entire bug.

## 4. Why it's invisible at the top, fatal in composition

**Top level — self-cancelling.** `param_apply (neutral) frame =
must_ok_or_self(Ok (Ok stuck)) = Ok stuck`. The double-wrap plus the single peel
lands exactly on the spec's `Ok stuck`. So the *contract holds at the top* — and
~20 tests assert it (`ok_value (param_apply h frame)`, `is_ok stuck`). This is
**why the routed arm wraps**: to make the top-level contract come out right.

**Composition — broken.** When a routed result is a *sub-reduction* fed back into
`w_s`, the single `ok_value` peel leaves a residual `Ok`. Two cases, and the
asymmetry between them is the whole story:

### `compose` = `{f}{g}{x} -> f (g x)` — nesting in *argument* position (HARMLESS)

`w_s` reduces the body to `self f (Ok stuck_gx)` — `f` is the operator (a neutral,
routes fine), and the wrapped `Ok stuck_gx` is the *argument*. The argument flows
to `hyp_reduce` as the frame; `pi_respond` reads only the **codomain** and ignores
the frame's value, so the stuck result is typed correctly. The wrapped argument
just pollutes the (unused) spine. **`compose` type-checks** — its result is well
typed despite the dirty spine.

### `flip` = `{f}{x}{y} -> f y x` = `(f y) x` — nesting in *operator* position (FATAL)

`w_s` reduces to `self (Ok stuck_fy) hyp_x` — now the wrapped neutral is the
**operator**. `Ok stuck_fy = fork("Ok", stuck_fy)` is *not* a kernel op (its root
is the `"Ok"` tag), so `walk` falls to the triage arm and runs
`w_fork self "Ok" stuck_fy hyp_x` — **it triages the `"Ok"` tag and applies it as
a combinator.** Garbage out. The result isn't a clean stuck neutral, so the
codomain check rejects it → **false-negative**.

> **The bug in one sentence:** the routed arm leaves a residual `Ok` around a
> neutral; in *argument* position it's harmless (the type comes from the
> codomain), but in *operator* position the walker applies the `"Ok"` **tag
> itself** as a function and produces garbage.

This is why `flip`, the `set_*` family (`bool_rec … (a x)` feeds a nested neutral
into an eliminator), `snd_of_nats`, `first_or_zero`, `or_zero` all fail to verify.

## 5. Why it's *masked* today — uniform-but-wrong wrapping

The system currently runs on a **consistently-wrong** wrapping. Stuck forms carry
an extra `Ok` in their spines — e.g. `add (succ hyp) m` reduces to
`succ (Ok stuck)`, not `succ stuck`. Type-checking still passes because the
wrapping is **uniform**: every stuck form is wrapped the same way, so the H-rule's
`tree_eq` still matches (`Eq Nat (succ (Ok stuck)) …` compares equal to itself).
The error is invisible precisely because it is everywhere at once.

This matters for any fix: a *partial* correction that makes some stuck forms bare
and leaves others `Ok`-wrapped **breaks the uniformity** and fails *more* checks
than it fixes — every attempt below hit exactly this.

## 6. Root cause: the monad and object values share one tree space

The deepest framing: `Ok`/`Fail` are *monad wrappers*, but they are encoded as
trees (`fork("Ok", v)`) in the **same space** as object values, and the walker
reduces by tree rewriting. So two distinct things become indistinguishable:

- `Ok stuck` produced by `hyp_reduce` — a **monad wrapper** around a neutral that
  should be *peeled* for composition.
- `Ok TT` produced by a recogniser — a **data verdict** that should be *kept*.

After one `ok_value`, both are `Ok <something>`; the walker cannot tell "peel me
(I'm a wrapper)" from "keep me (I'm data)". Worse, *how many* `Ok` layers
accumulate depends on the **reduction order** the bracket-abstracted combinators
happen to take — which is why hand-tracing the wrapping is unreliable (see §8).

## 7. The cost today: advisory verification

Because the checker has false-negatives, `compileBinding` cannot reject:

```ts
// advisory: a non-Ok-TT verdict is NOT a real type error (could be the wrapping
// false-negative), so we warn (opt-in) and let the binding through — the same
// coverage the old check/infer gave (it skipped all non-Pi-codomain bodies).
if (!verdictOk(verdict) && options.warnUnverified) console.warn(...)
```

Concretely this means:

- A genuinely ill-typed value binding (`foo : Nat -> Bool := {n} -> n`) is **not
  caught** — it produces `Ok FF`, same channel as a wrapping false-negative.
- Hard-rejecting on `Ok FF` is *untenable*: the bogus verdict is **arbitrary**
  (`Ok FF` vs `Fail`, reduction-order dependent). Dropping `set_union`'s
  annotation just moved the failure to `set_inter` — same body shape, different
  garbage verdict.

So the in-language checker is the *spec* but cannot yet be the *enforcer*.

## 8. Fixes tried, and why each is subtle

All four were validated against the full suite; none converged.

1. **Routed arm `Ok (f x)` → `f x`** (make `walk` return `Ok <bare>` uniformly).
   Fixes the neutral contract *and* operator-position nesting. **But** breaks the
   recogniser contract: `Pi`/`Sigma`/`Intersection` bodies *tail-call* `bind_hyp`
   (a routed op), so the recogniser's `Ok verdict` becomes the routed arm's direct
   return and `param_apply` peels it to a **bare `TT`/`FF`** — `typecheck` breaks.
   (`Bool`/`Nat` are unaffected: their bodies construct `Ok verdict` non-routed.)
   → ~11 failures.

2. **`w_s`/`w_tfk` deref** — peel a residual `Ok` off operands before re-applying,
   guarded by `is_neutral (ok_value y)`. Fixes operator-position nesting. **But**
   breaks recursors: `elim`'s neutral branch and recursor intermediates are *also*
   `Ok <neutral>`, and derefing them shatters the §5 uniformity → `add`/`pred`
   mismatch. The guard can't distinguish "routed wrapper" from "recursor value" —
   both are `Ok <neutral>`. → `pred`, `typed_add` fail.

3. **Routed `f x` + reify the recogniser body**
   (`FF => match (body meta v) { Ok r => Ok r ; Fail _ => Fail }`, forcing a
   *constructed* `Ok` rather than a routed tail). Hand-analysis says this restores
   the Pi contract; **empirically it did not** — `param_apply (Pi Nat Nat) ({n}->n)`
   still wasn't `Ok TT`. The reduction-order emergence (§6) defeats the local fix.

4. **Per-binding workarounds** (drop annotation / advisory). Green, but gives up
   enforcement — the current state.

**Meta-finding:** that careful hand-traces (1) and (3) *both* mispredicted the
runtime is itself diagnostic. The wrapping count is an emergent property of how
bracket-abstracted combinators reduce under `w_s`, not a property you can read off
the source. **A fix that reasons locally about "how many `Ok`s are here" is
fragile by construction.** The real fix must remove the ambiguity structurally.

## 9. The real fix: separate the monad from the object space

The durable solution is to stop conflating "spine-extension succeeded" (a *monad*
fact) with the stuck neutral (an *object value*). Two viable shapes:

### Option A — `hyp_reduce` returns the bare value; only `bind_hyp` carries `Ok`

`hyp_reduce` *cannot fail* — it always `Extend`s or `Return`s. So its `Ok` is pure
ceremony. Make it return the **bare** reduct:

```disp
hyp_reduce := fix ({self, meta, frame} -> {
  match (respond meta frame) {
    Extend new_type => wait self (extend_neutral_meta (wait self meta) new_type frame)  // bare stuck
    Return v        => v                                                                 // bare value
  }
})
```

Keep the routed arm `Ok (f x)`. Now `walk(neutral, frame) = Ok (bare stuck) =
Ok stuck` (single), `param_apply → stuck` (bare), and `w_s`'s `ok_value → stuck`
(bare) — composition in **both** argument and operator position is clean. Crucially
`bind_hyp` *keeps* its `Ok` (it produces verdicts/escape results, genuine
CheckerResults), so recognisers whose body tail-calls `bind_hyp` still surface
`Ok verdict`. The two routed ops are now treated according to what they actually
produce: `hyp_reduce` → object value, `bind_hyp` → CheckerResult.

*Open subtlety:* `hyp_reduce`'s `Return` channel is used by the Type predicate
H-rule (`type_predicate_h_rule` returns `Return (tree_eq …)`), where the value
*is* a Bool verdict (data, wants `Ok`). So `Return` may need to stay `Ok`-wrapped
while `Extend` goes bare — an asymmetry that must be checked against
`param_apply A_hyp x = Ok TT` (the predicate-side H-rule test). This is the one
seam Option A must get right.

### Option B — tag stuck neutrals distinctly from `Ok`

Give the walker a way to tell "this `Ok` wraps a neutral I should peel" from "this
`Ok` is data". Since `is_neutral (ok_value r)` already *almost* does this, the
failure in fix (2) was that recursor *values* are also `Ok <neutral>`. Option B
makes recursor results structurally distinct (e.g. `elim` returns the **bare**
stuck, never `Ok stuck`), so `Ok <neutral>` *unambiguously* means "routed wrapper,
peel it". This is fix (2) + making `elim` consistent in the same pass.

### Either way: it's a coordinated change, landed atomically

Because of §5's uniformity trap, the change must touch all wrapping sites in one
commit, with the suite as the harness:

1. the routed arm and/or `hyp_reduce` (object values become bare),
2. recogniser wrappers (verdicts stay `Ok`, via reify or because `bind_hyp` keeps
   its `Ok`),
3. `elim` neutral branch (returns bare stuck, consistently),
4. the ~20 tests asserting `param_apply neutral frame = Ok stuck` → bare form
   (`ok_value (param_apply h f)` → `param_apply h f`; `is_ok stuck = TT` →
   `is_neutral stuck = TT`).

The right validation order: get `param_apply (Pi Nat Nat) ({n}->n) = Ok TT`
**and** `is_neutral (param_apply hPi zero) = TT` **and** `flip`/`compose`/`add_zero_r`
green *simultaneously* on a scratch branch before touching the assertion tests.

## 10. What a clean checker unlocks — the downstream design

The point of all this is **the object language is the checker, and the host just
runs it** (CLAUDE.md core discipline; GOALS.md north star). The wrapping fix is
the last thing standing between "advisory" and that endpoint.

### Today

```
elaborator (src/compile.ts)
  ├─ compileType        : annotation syntax  → Pi tree              (host elaboration)
  ├─ compileExpr        : surface lambda      → bracket-abstracted tree
  └─ compileBinding     : param_apply T body  → verdict, but ADVISORY (can't reject)
                                                 │
                                                 └─ in-language checker is the spec
                                                    but NOT trusted to reject
```

- Verification runs but **cannot enforce** — ill-typed bindings slip through.
- The host implicitly retains checking authority by *declining to act* on the
  in-language verdict.
- Proof terms (`add_zero_r`) only "pass" because nothing checks them hard.
- Whole classes of ordinary functions (`flip`, `set_union`) are unverifiable.

### After the fix

```
elaborator
  ├─ compileType   : annotation → type tree
  ├─ compileExpr   : lambda     → tree
  └─ compileBinding: verdict = typecheck T body
                     verdict ≠ Ok TT  ⇒  HARD ERROR
                                          │
                                          └─ the in-language checker IS the
                                             enforcer — one trusted oracle
```

- `compileBinding`'s verification becomes a **hard error** on `Ok FF`/`Fail`. The
  host carries *zero* type-checking logic — it compiles to trees and calls one
  in-language `typecheck`. (`check`/`infer` are already gone; this removes the
  last reason they might come back.)
- The checker is a **total, trustworthy oracle**: every well-typed term verifies,
  every ill-typed one is rejected, deterministically (no reduction-order-dependent
  garbage verdicts).

### Why this is the linchpin for the north star

1. **Self-hosting.** A checker that reliably accepts/rejects can be run *on
   itself* — the metacircular endgame. Today's advisory checker can't certify its
   own library, because the library uses exactly the nested-application patterns it
   mis-handles.

2. **Real proofs.** `add_zero_r`, `add_succ_r`, and the `Eq`/`J` machinery only
   become *meaningful* when their bodies are genuinely verified. Advisory checking
   makes a proof and a non-proof indistinguishable (both compile). A hard checker
   is the difference between "documented intent" and "machine-checked theorem."

3. **Neural-guided synthesis (GOALS.md).** A synthesis loop proposes terms and
   keeps the ones that check. This **requires a sound, complete oracle**: a
   false-negative makes the synthesizer discard valid candidates; a
   false-acceptance poisons the search. The wrapping false-negatives make the
   current checker unusable as a synthesis filter. The fix turns `typecheck` into
   the reward signal the whole self-improving-optimizer story depends on.

4. **Erasure / `strip` (§10).** Stripping a *validated* program to bare positional
   data presupposes a trustworthy validation verdict. Advisory verdicts can't gate
   erasure soundly.

In short: the wrapping invariant is not a cosmetic wart. It is the one defect that
keeps the in-language checker from being *the* checker — and being *the* checker is
the entire premise of the project.

## 11. Quick reference — repro and harness

```
# the canonical false-negatives (operator-position nesting; raw decomposition)
param_apply (Pi …) ({f}{x}{y} -> f y x)        # flip — garbage from Ok-tag-as-fn
param_apply (Pi …) ({p} -> pair_snd p)         # raw triage on a Σ-hyp → Fail

# the contract that must stay true through any fix
param_apply Bool TT                 = Ok TT     # recogniser verdict is data
param_apply (Pi Nat ({_}->Nat)) ({n}->n) = Ok TT   # recogniser whose body tail-routes bind_hyp
is_neutral (param_apply hPi zero)   = TT        # neutral application result (bare, post-fix)
```

Files: `lib/kernel/core.disp` (`walk`, routed arm, `w_s`, `hyp_reduce`, `elim`,
`recognizer_wrap`, `pi_body`); `src/compile.ts` (`compileBinding` verification);
tests in `lib/tests/{kernel,types,soundness}.test.disp`.
