
opt : { A : Type } -> A := 
  // 



# Sealing, `occurs`, and the metadata-extraction leak

**Status:** the leak described below is **CLOSED** (2026-06-07). `bind_hyp` now runs
its body UNDER the walker (merged into `param_walker` as `w_bind_hyp`), so the
raw-body reflection that defeated `occurs` is rejected at the source. §1–§6 below
are retained as the *diagnosis* (how the leak worked and why it mattered); §7 is
rewritten to record the fix and to correct the original "representational poison"
direction, which is **impossible** in pure tree calculus (triage is a total
destructor — see §7). The adversarial/soundness suites now pin the closed behavior.

Audience: anyone touching `bind_hyp` / `occurs` / `support_set` / the walker, or
reasoning about the soundness of the sealing discipline.

---

## 1. What a hypothesis actually *is*

A hypothesis (= a neutral) is just a hash-consed tree with a fixed shape. From
`lib/kernel/core.disp`:

```disp
make_hyp := {ty, id} -> wait hyp_reduce { stored_type := ty; payload := id }
hyp_sig  := checker_sig hyp_reduce
is_neutral := {v} -> tree_eq (pair_fst v) hyp_sig
neutral_type := {v} -> (type_meta v).stored_type      // type_meta v = pair_snd (pair_snd v)
```

So `make_hyp Nat 0` is structurally:

```
fork( hyp_sig , { stored_type := Nat ; payload := 0 } )
       ▲ pair_fst (constant for ALL hyps)   ▲ pair_snd → the "meta" record
```

Two parts:

- **The descriptor** `pair_fst v = hyp_sig`. *Identical for every hypothesis of
  every type* — it's how `is_neutral` recognizes one. Carries no per-hyp info.
- **The meta** `pair_snd v = { stored_type ; payload }`. The `stored_type` is the
  **public** part (which type this hyp stands for); the `payload` is the
  **private** part — the identity (`0`, or for `bind_hyp`, `(pair domain body)`)
  that distinguishes *this* hyp from another of the same type, and, for a
  *derived* neutral, the whole spine of what was applied to it.

The walker's design is built around this public/private split: `STORED_TYPE` is a
sanctioned reader carve-out (you may read `stored_type`), but raw
`pair_snd`/`type_meta` is **forbidden under the walker** (triage-on-neutral)
because it would hand you the payload.

## 2. How `occurs` / `support_set` / `is_closed` work

```disp
let is_hyp_fork = {v} -> and (is_fork v) (tree_eq (pair_fst v) hyp_sig)
let support_set = fix ({self, v} ->
  match (is_hyp_fork v) {
    TT => t (type_meta v) (self (type_meta v))                          // collect this hyp's meta, recurse into it
    FF => (triage t ({c} -> self c) ({l,r} -> t_union (self l) (self r)) v)
  })
occurs    := {h, v} -> set_member (type_meta h) (support_set v)
is_closed := {v} -> is_leaf (support_set v)
```

`support_set v` walks the *entire* tree of `v` and, **every time it lands on a
node that `is_hyp_fork`** (a neutral), it adds that node's `type_meta` to a
running set and recurses *into* the meta (so a hyp buried inside another neutral's
spine is still found — "descends through neutrals").

Then:

- `occurs h v` = "is `h`'s meta one of the metas `support_set` collected?" =
  **does `h` appear *as a neutral* anywhere in `v`?**
- `is_closed v` = "did `support_set` collect *nothing*?" = **`v` contains no
  hypotheses at all.**

One subtlety: `occurs` searches for *one specific* hyp's meta (`type_meta h`), not
"any hyp." That's deliberate — in nested binders
`bind_hyp Type ({A} -> bind_hyp A ({x} -> …))`, releasing `x` searches for `x`; a
result mentioning `A` (still in scope) passes. Searching for the *particular*
released hyp is what distinguishes a legal in-scope reference from one being
smuggled out.

## 3. What "sealing" means, and the three protections

`bind_hyp A ({x} -> body)` is the kernel's way of **opening a binder** —
`Γ, x:A ⊢ body` — where `x` is a *fresh, abstract value of type A*. In the sealing
framing (it's literally generative sealing): `x` is a freshly minted **seal** — an
opaque, unforgeable token standing for "an arbitrary A." For the type system to
stay sound, three things must hold:

1. **Unforgeable** — user code can't fabricate a fake hyp. Enforced by the
   walker's *stem-forge* rejection (you can't build `fork(hyp_sig, …)`).
2. **Opaque** — user code can't read the seal's payload (its identity). Enforced
   by the walker's *triage-on-neutral* rejection (you can't `pair_snd`/`type_meta`
   a hyp). Only the public `stored_type` is readable, via the `STORED_TYPE`
   carve-out.
3. **Non-escaping** — the *result* of the body must not depend on the seal.
   Enforced by `occurs` (the escape check above). This is a **noninterference**
   property: information must not flow from the sealed value to the public output.

(1) and (2) keep `x` abstract *while you compute with it*; (3) keeps it from
leaking *out*.

## 4. The theoretical issue — exactly

The crux: **protections (1) and (2) are enforced by the *walker*, but the
`bind_hyp` body runs *raw* — not under the walker.**

```disp
bind_hyp_handler := {domain, body} -> {
  let h = make_hyp domain (pair domain body)
  bind (body h) (...)          // <-- body h runs RAW, outside param_walker
}
```

So inside the body, `x` is **not** unforgeable and **not** opaque. The body can
freely do `type_meta x`, `pair_snd x`, or even hand-build a `fork(hyp_sig, …)`.
The *only* surviving protection inside the body is (3), the `occurs` escape check.

And `occurs` is a **purely structural channel-monitor**: it triggers on
`is_hyp_fork` — i.e., it only sees a dependency on `x` when `x` (or a neutral
derived from it) appears *as a neutral* in the output. That encodes one
assumption: *the only way to depend on the hypothesis is to carry it as a
neutral.*

That assumption is false in a raw body, and that's the leak:

```disp
bind_hyp Nat ({x} -> Ok (type_meta x))     // returns x's meta — and occurs MISSES it
```

`type_meta x` extracts the meta and returns it **bare**. The bare meta is a plain
`{ stored_type ; payload }` record — `pair_fst` is the record header, **not**
`hyp_sig` — so `is_hyp_fork(meta) = FF`. `support_set` walks right through it and
collects *nothing*. So `occurs x (Ok meta) = FF`. The body has *laundered* the
seal: the very thing `occurs` searches for (`type_meta x`) flows out, but because
it's no longer *wrapped in a hyp-fork*, the collector never fires.

In information-flow terms: the seal is a "high" value; `occurs` watches the
*direct* channel (the hyp appearing as a neutral) but not the *laundering* channel
(extract-the-representation-and-return-it). The deeper root cause is that **the
seal is only opaque *dynamically* (by the walker refusing to look), not
*representationally*.** A hypothesis is a transparent tree with its secret sitting
in `pair_snd`; nothing about the *representation* hides it. Remove the walker (raw
body) and the secret is right there.

And once the meta escapes, you can revive the hypothesis anywhere:

```disp
let stolen  = ok_value (bind_hyp Nat ({x} -> Ok (type_meta x)))
let revived = wait hyp_reduce stolen        // x, reconstructed OUTSIDE its binder
```

So `bind_hyp` does **not** actually seal — the abstraction barrier it's supposed
to erect is porous to representation-extraction.

## 5. Why it isn't (today) a *soundness* hole — the backstop

The leak lets `x` move around in **open** contexts, but it can't become a
**closed proof**, because the boundary into closed-proof land
(`param_lift` / `typecheck`) runs `is_closed` — which uses the *same*
`support_set` scan:

```disp
is_closed revived = FF       // revived IS a hyp-fork → support_set finds it → not closed
typecheck Nat revived = Err  // rejected at the boundary
```

The structural argument for why the backstop is *sufficient*: a hypothesis's
type-theoretic role **is** to be a stuck neutral standing for an abstract value.
To *use* the revived hyp as an inhabitant of a type, it has to be in neutral form
— and `is_closed` catches every neutral. The leaked `meta` is just inert data (a
record); it doesn't inhabit any interesting type, and reconstructing it only gets
you back an open neutral. So no closed term of `False` (or any empty type) is
constructible. That's exactly what the adversarial suite pins.

The asymmetry that makes this hold: `occurs` and `is_closed` share the *same*
blind spot (bare metas), but `occurs` runs *inside* `bind_hyp` (where laundering
happens) while `is_closed` runs *at the closed-world boundary* (where you can only
present a neutral or a closed value). The launder gets you past `occurs`, but to
*cash it in* you must re-form the neutral, and then `is_closed` catches it. The
leak buys mobility, not a forged closed proof.

## 6. Why it still matters — the deep stakes

This isn't merely cosmetic, because the foundational ambition (per the
sealing-framework design) is the conjecture **"sealing preserves parametricity"** —
which is what's supposed to make the system sound *even with* `Type : Type`
(normally inconsistent via Girard's paradox). That conjecture wants the seal to be
a *genuine* abstraction barrier. The `occurs` leak is direct evidence that the
*current implementation* of the barrier is porous: the escape check enforces a
*weaker* property (no neutral escapes) than the *intended* one (no information
about the hypothesis escapes). The `is_closed` backstop appears to contain it for
first-order closed-proof soundness, but "appears to contain it" is exactly the
kind of gap a parametricity proof has to close — a representational launder is
precisely the move a Girard-style paradox would try to exploit.

The honest one-line statement: **`occurs` checks "does the hypothesis occur as a
neutral?" when sealing demands "does the result depend on the hypothesis at
all?" — and representation-extraction is a dependency of the second kind that
isn't of the first.**

## 7. The fix (landed) — and why "representational poison" was the wrong idea

The original direction proposed here was to make the seal opaque
*representationally*: a hypothesis whose payload raw `pair_snd`/`triage` yields a
**poison** value the escape scan catches. **That is impossible in pure tree
calculus, because triage is a *total* destructor.**
A hypothesis is a fork (`type_meta` is a fixed composition of `pair_snd`), and the
fork-elim rule `apply(△(△ c d) b, △ u v) = apply(b u v)` hands *both* children to
any handler — so wherever raw evaluation runs, raw triage fully decomposes the hyp
and reaches its payload. No tree is opaque to triage (that is the whole point of
TC's reflectivity: `tree_eq`, `size`, etc. work on everything). So opacity can
only ever be *dynamic* — a property of *who is evaluating* (the walker withholds
triage on neutrals; raw evaluation does not) — never a property of the value.
Genuine representational sealing would require a substrate primitive (a node kind
triage refuses to decompose), i.e. leaving pure TC.

**The actual fix: don't run the body raw.** Since opacity is dynamic, the leak is
closed by making the `bind_hyp` body run UNDER the walker rather than raw —
protection (2) (opacity) then covers the body, not just the outside. Concretely
`bind_hyp` is merged into `param_walker` (`w_bind_hyp`, `lib/kernel/core.disp`):
the walker intercepts the op-tag (`is_bind_hyp`), mints the hyp, and walks
`body h` via its own `self`. A body that does `type_meta x` / `pair_snd x` now
trips the walker's triage-on-neutral rejection (`Err`), exactly as the same
expression does at top level. `occurs` is no longer the lone guard inside a raw
body — there is no raw body. (The earlier framing in §6 — "`occurs` enforces a
weaker property than intended" — is resolved by removing the raw body, not by
strengthening `occurs`.)

A subtlety that earlier analysis missed: whole-body walking does *not* loop, and
the obstruction was never `occurs` — it was that recognizer bodies used to call
`param_apply` explicitly, and `param_apply T` (the walker partially applied) is an
unrecognized fork the walker refuses to apply to a neutral (over-rejection). The
fix is to write recognizer bodies with **raw applications** (`(B hyp) (v hyp)`) and
let the single enclosing walk police every user sub-term. Because the walker is
*our* evaluator, the user-code-vs-evaluator distinction is ours to define: the
evaluator's own neutral reads (H-rule via `is_neutral`/`neutral_type`) pass; a
`triage`/`pair_snd` *node in the body* is rejected.

Consequence (a clean side effect): a *binder* type (Pi/Intersection/Sigma — those
minting a hyp) applied RAW (`T v`, bypassing `param_apply`) now fails fast, because
its `bind_hyp_handler` fails closed outside the walker. So bypassing the checker is
either safe (non-binder types — no parametricity surface) or an early `Err`, never
a silent unsound accept. Verification therefore goes through `param_apply`
(`verify mod = param_apply mod.typ mod.record`), never raw `typ record`
juxtaposition (which bypasses the walker); the elaborator auto-runs `verify` on
loaded modules' typed exports.

This does not by itself discharge the §6 parametricity *conjecture* (soundness with
`Type:Type`) — that remains the step-indexed-LR program in
`project_sealing_framework`. But the barrier the conjecture reasons about is now a
genuine dynamic abstraction barrier (no representation-extraction channel through a
`bind_hyp` body), rather than the porous escape-only scan diagnosed in §4–§6.

## References

- Code: `lib/kernel/core.disp` — `make_hyp`, `hyp_sig`, `type_meta`, `neutral_type`,
  `support_set` / `occurs` / `is_closed`; the fix: `bind_hyp` (op-tag) + `is_bind_hyp`
  + `w_bind_hyp` (the merged walker branch), `bind_hyp_handler` (fail-closed),
  `verify`; the walker carve-outs and rejections (stem-forge, triage-on-neutral).
- Tests: `lib/tests/adversarial.test.disp` + `soundness.test.disp` — now pin the
  CLOSED behavior: `param_apply (bind_hyp Nat) ({x} -> Ok (type_meta x)) = Err`
  (was the leak), raw reflection in a body = `Err`, and raw binder-type application
  = `Err` (fail-fast). `src/compile.ts` (`resolveUse`) auto-verifies typed exports.
- Spec: `TYPE_THEORY.typ` §7.2 (`bind_hyp`) — the `q_bind_hyp_fn` source already
  specified `param_apply body h_use`; the implementation note records the merged-
  walker realization.
- History: GAP-2 sub-term policing (`pi_body` codomain, Intersection predicate,
  Sigma family, recursor step) closed the *sub-term* reflection holes; the merged
  walker subsumes it (recognizer bodies are now raw applications policed by the
  enclosing walk), closing the residual *raw-body* gap this note diagnosed.
