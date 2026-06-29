# Sealing modality — design (the nested-walker carve-out)

> Status: **design, not built** (2026-06-29). Motivated by the R6 metacircle blocker — `StrictType`
> cannot verify a *recursive* inductive's respond via `GoodRespond` because the nested walk aborts.
>
> **⚠ THIS DOC'S "R6 NEEDS SEALING" CONCLUSION IS SUPERSEDED (2026-06-29).** An instrumented trace
> (param_walker with distinct Err sentinels) overturned the sealing framing. Authoritative current
> findings live in memory `project_telescope_meet_unification` FOLLOW-UP 3; in brief:
> - **`apply_policed` nested = `Ok`** — §7A handles the recursive re-entry. NOT the culprit.
> - The abort is a **`w_stem` stem-forge** (and/or `w_fork` triage-on-neutral), whose PRIMARY cause is
>   GoodRespond's **raw `make_hyp`** (`gr_M`/`gr_s`): constructing `t hyp_sig …` under the outer walker's
>   reduction looks like forging a hyp. **`bind_hyp` avoids it via the `is_bind_hyp` carve-out** — which is
>   why nested `Pi` (bind_hyp mints) composes. So **managed-`M`/`s` is the right direction after all** (the
>   "managed mints don't work / managed-vs-raw isn't the axis" conclusion below was WRONG — that test
>   exposed a *separate* residual, not refutation of the fix).
> - **Universe levels are NOT the wall** (prototyped, 6/6): minting `ih : (M p)` (an impredicative hyp,
>   M:Nat→Type abstract) works top-level AND nested. R6's trap is *mechanical*, orthogonal to `Type:Type`
>   consistency.
> - **Sealing (Option A bypass / Option B relativity) is NOT required for R6.** Option A is still genuinely
>   unsound (forgeable bypass; §4A/§5) and that analysis stands as a general lesson, but R6 does not need it.
> - **RESIDUAL:** the managed version still traps (`unreachable`) somewhere in the recursive case (not the
>   impredicative mint); needs NON-MUTATING instrumentation to pin (the earlier `Err e => e` preserve-change
>   altered control flow → unreliable). 
>
> The original design exploration below is kept for the **Option A forgeability argument** (§4A/§5 — the
> "no unforgeable bypass in a stateless substrate" lesson is sound and reusable), but its premise that R6
> *needs* sealing is wrong. Companion: `ACTIVE_PLAN.md` R6, memory `project_telescope_meet_unification`.

## 1. The problem

`GoodRespond T` is a telescope; `verify_good T R = param_apply (GoodRespond T) R` **composes under one
walker** (it's just an `at` walk). But wiring it into `StrictType` as a third cell — "if `T` is a gated
inductive, check `(type_meta T).respond : GoodRespond T`" — means running that check **inside**
`StrictType`'s own walk. That is a *nested* `param_walker`, and it aborts for recursive inductives:

| candidate | `param_apply StrictType2 _` | why |
|---|---|---|
| `Bool` (non-recursive) | `Ok TT` ✅ | respond fully verified, in-walk |
| `Pi` / `Eq` (non-inductive) | `Ok TT` ✅ | dispatch skips (else-branch) |
| `StrictType2` itself | `Ok TT` ✅ | self-types (its own respond isn't gated) |
| **`Nat` (recursive)** | **`Err`** ❌ | nested-walker abort |

The dispatch and self-typing already work. Only the **recursive eliminator** breaks.

## 2. Root cause

Nesting `param_walker` means the **outer** walker structurally *reduces* the inner walk (a recognizer
body is reduced by `w_fork`), and **`w_fork`'s triage-on-neutral guard fires on the *inner* walk's
hyps**:

```
// engine.disp, w_fork (~L91-98)
({tc, td} ->
  if (is_hyp_fork x)
    then Err            // <-- triage-on-neutral rejection: fires on the INNER walk's hyp
    else (triage ... x))
```

That `Err` escapes past the cell's own `Err → SReject` handler, so the whole `param_apply` returns
`Err`. Confirmed by bisection:

- nested **basic** minting (a `Pi`-check) → `Ok` — nesting a mint per se is fine;
- nested **`GoodRespond`** → `Err` (Bool *and* Nat in a direct recognizer body; Nat in an `at`-cell);
- the existing **§7A carve-out** (`engine.disp ~L122`,
  `pair_fst f == checker_sig self → Ok (self (type_meta f) x)`) sanctions only a *deferred*
  `apply_policed M` (`f = wait param_walker M`). `apply_policed M x` evaluates eagerly
  (`wait a b c = a b c`), so an eager nested verify never reaches that carve-out (`apply_policed`-spelled
  GoodRespond still `Err`s).

The specific neutrals that trip it are GoodRespond's **abstract motive/self** (`make_hyp (Arrow T Type)`,
`make_hyp T`) and the recursive case's `ih : (M p)` (a **neutral-typed** hyp). They are fine to the
*inner* walk but opaque-and-rejected by the *outer* one.

## 3. Why sealing, not universe levels

Verifying Nat's respond is **impredicative**: it quantifies over the motive `M : Nat → Type`, so
*checking `Nat : Type` ranges over `Nat → Type`*.

- A **predicative hierarchy** stratifies this away — `Nat : Type₀`, its eliminator-soundness check at
  `Type₁` — at the cost of making `StrictType : StrictType` a level error.
- disp deliberately chose **`Type : Type`** (StrictType self-types). One universe ⇒ the check quantifies
  over Type to verify Type — a genuine loop. **The nested-walker abort is that impredicative loop,
  operationally**: the substrate refusing the self-check.

The loop is **well-founded** (`GoodRespond Nat` is finite and never re-invokes `StrictType`), so this is
a *rejection*, not divergence. Making it pass = extending `Type:Type` to cover eliminator-soundness,
**sound iff parametricity is preserved** — the standing `Type:Type` conjecture ("sealing preserves
parametricity", `project_sealing_framework`). So sealing is not a perf hack; it is the operational price
of disp's impredicativity. Universe levels are the *alternative design* disp rejected.

## 4. The design

The enclosing walker aborts because it **polices a sub-walk it should trust**. The inner walk *is* a
`param_walker` — it polices its own hyps (escape-check, triage-on-neutral). So the enclosing walker has
no soundness reason to re-police it. The fix is a **sanctioned opaque re-entry**: run a trusted inner
walk as a black box whose internal neutrals never surface to the enclosing walker.

Two implementations, in increasing generality:

### Option A — opaque-verify head (recommended; a §7A sibling)

Add ONE `param_walker` carve-out: a `seal`-marked function is run by the enclosing walker as a *direct
sub-evaluation* (normal `apply`), result wrapped in `Ok`, **never reduced by `w_fork`**.

```
seal := {g} -> wait seal_op g                 // seal_op: a fresh trusted constant; type_meta (seal g) = g
// in param_walker, alongside §7A:
else if (and (is_fork f) (tree_eq (pair_fst f) (checker_sig seal_op)))
  then (Ok ((type_meta f) x))                 // opaque: run (g x) via normal apply, do NOT w_fork into it
```

Then `respond_good` verifies via `seal (λcand. verify_good source cand)` so the enclosing `StrictType`
walk dispatches it through the carve-out instead of reducing into it. Because `g x` runs via the normal
evaluator (not `w_fork`), the inner mints are consumed internally and only the concrete verdict surfaces.

- **Pro:** small, local, mirrors the existing §7A trusted-policer.
- **⚠ SOUNDNESS HOLE (this sinks Option A as written):** the carve-out runs `g x` via **normal apply —
  it BYPASSES policing**. That is the opposite of §7A, which runs `param_walker M x` (real *re*-policing,
  so a forged §7A token is harmless — it just re-polices your own `M`). A `seal` that bypasses policing is
  only safe if it is **unforgeable**, and **in a pure tree-calculus substrate nothing is unforgeable**:
  all values are trees and user code can reconstruct any constant. `seal_op` is a closed kernel tree, so
  user code can rebuild it and form `wait seal_op (λ_. <reflect-on-x>)` — a legit-shaped seal (NOT a
  hand-built fork, so the stem-forge guard at `w_stem` doesn't catch it) — and the carve-out then runs
  arbitrary **unpoliced** code → parametricity break → unsound. The hyp protections don't transfer:
  `make_hyp` is *forgeable too*, but harmlessly, because the walker polices hyp **use** (reflection), not
  creation; `seal` is itself a policing **bypass**, so forgeability bites directly. **Conclusion: a sound
  unforgeable bypass needs either generative freshness (substrate STATE — a per-walk nonce the stateless
  substrate lacks) or HOST support (breaking object-language-is-spec). Option A is rejected.**

### Option B — sealed hyps (general; harder)

Make individual hyps carry a seal so the enclosing `w_fork`/`is_hyp_fork` treats them as **opaque
values**, not neutrals-to-police, while the sealing walker still polices them normally.

```
make_sealed_hyp ty id = wait hyp_reduce { stored_type := ty; payload := id; sealed := seal_token }
```

- **The hard part — relativity:** a hyp sealed by walker A must be opaque to enclosing walker B yet
  *transparent* to A (A still escape-checks it). A binary seal + uniform pass-through can't express
  "inner polices, outer ignores." Candidate disciplines: (i) a depth/scope token threaded through the
  walk (but the substrate is stateless — would need the token in the hyp payload + a depth carried in
  the walker's recursion), or (ii) seal-on-cross — a hyp gets sealed *as it crosses* a walker boundary
  outward, so each walker sees only its own unsealed hyps. (ii) is closest to lexical scoping and worth
  sketching, but it's a real substrate change to `make_hyp` + `w_bind_hyp` + `w_fork`.
- **Pro:** composable (arbitrary nesting depth), the "right" general primitive.
- **Con:** touches the neutral representation and three walker functions; bigger blast radius.
- **Forgeability — subtler than A, maybe NOT exploitable:** the `sealed` marker is forgeable too (any
  tree), but a *pass-through* (vs A's *bypass*) may be harmless: the walker polices type-functions
  against ITS OWN minted hyp, not a user-supplied one. A forged-sealed hyp only "escapes" policing when
  the function is applied to *that specific forged value* — and at a real use-site the function meets real
  values, so the user can't both pass the check and reflect on reals. This needs a proper argument (the
  relativity discipline + a step-indexed check) before relying on it.

**Recommendation (REVISED after the §5 forgeability analysis):** **Option A is REJECTED** — an
unforgeable *bypass* doesn't exist in the pure substrate. The two sound paths are:
1. **Re-policing precision (preferred, no new trust):** keep the §7A *re-policing* path (which is sound
   under forgery) and instead fix *why it aborts for the recursive case* — i.e., make the walker
   correctly police minting `ih : (M p)` (a neutral-typed hyp) under nesting. If the recursive metacircle
   check is genuinely sound (the `Type:Type` bet), the abort is an over-conservative false-positive in the
   walker, and the fix is **precision**, not a trust carve-out. This adds ZERO to the trusted set.
2. **Generative sealing (Option B, done right):** needs *fresh, unforgeable* seals — which require
   substrate STATE (a per-walk nonce) or host support. Bigger, and only if path 1 proves impossible.

## 5. Soundness — why bypass fails and re-policing is the only free lunch

The would-be pass-through is sound only if **restricted to trusted re-entries that user code cannot
spoof** — and that restriction is **unenforceable for a *bypass*** in this substrate:

1. **Forgery is free.** Every value is a tree; `seal_op` is a closed kernel tree, so user code rebuilds
   it and forms `wait seal_op g` — the *legit* shape, not a hand-built `fork(sig,…)`, so the `w_stem`
   stem-forge guard never sees it. There is no per-value provenance in a stateless substrate to tell a
   kernel-made seal from a user-made one.
2. **Bypass ≠ re-policing.** §7A survives forgery because it runs `param_walker M x` — a *real* policing
   walk; a forged `apply_policed` just re-polices your own `M` (harmless). A `seal` whose purpose is to
   run `g x` *unpoliced* has no such backstop: forge it once and you run arbitrary reflecting code.
3. **The hyp analogy misleads.** `make_hyp` is forgeable too, but the walker polices hyp **use**
   (reflection under a mint), not **creation** — so a forged hyp is inert until you try to reflect on it,
   which is exactly what gets caught. `seal` *is* the catch being removed.

So the only soundness-preserving "trust" in this substrate is one that **still polices** (§7A). For the
nested metacircle that means: don't bypass — make the *re-policing* path stop aborting. The `Type:Type`
conjecture ("a sealed inner walk preserves parametricity", `project_sealing_framework`) is then about
whether the recursive eliminator's check is genuinely sound, i.e. whether the abort is a false-positive
that *precision* can remove — NOT about licensing a user-forgeable bypass.

## 6. Relationship to the existing kernel

- **§7A carve-out** (`param_walker ~L122`) is the precedent: it trusts `apply_policed M` because it
  **re-polices** (`param_walker M x`), not bypasses — so Option A (bypass) is rejected for breaking that.
- **Option B does grow the substrate** (a generation threaded through `param_walker` + `make_hyp` +
  `w_fork`), but NOT the *trusted* set in the bypass sense — the generation guard still polices, it just
  scopes *which* hyps each walk owns. Its soundness rests on the §4B pass-through argument, not on an
  unforgeable token.

## 7. Implementation plan (Option B — relativity / generation sealing)

Managed mints were tried and **rejected** (§top): the outer walk triages the inner walk's hyps
regardless. The remaining viable path is RELATIVITY — make the outer walk pass through hyps that belong
to a deeper (inner) walk.

1. **Thread a generation `g` through `param_walker`** (the fix's extra arg). Mint at `g`; a nested
   re-entry (the inner GoodRespond walk) runs at `g+1`.
2. **Tag minted hyps with `g`** (in the `make_hyp` payload, via `w_bind_hyp`).
3. **`w_fork`/`is_hyp_fork` pass through** hyps whose generation `> g` (a deeper walk's — trusted to
   police itself), and reject/triage only `≤ g` (this walk's own + ancestors' — the parametricity guard).
4. **Soundness gate (§4B):** the generation marker is forgeable; argue (step-indexed) that a *pass-through*
   (unlike Option A's *bypass*) is benign — a forged-high-gen hyp only escapes policing on that specific
   forged value, never on the real values a function meets at a use site. If that argument fails, B is
   also out and R6 is genuinely blocked (or needs host support).
5. **Validate + perf:** `StrictType2 Nat = Ok TT`, `BadNat = Ok FF`, self-types, full suite green; measure.

This is invasive (three core walker functions) and soundness-gated — hence "design, not built."

## 8. Open questions

- **Was the abort a false-positive fixable by precision?** — ❌ **NO** (managed mints BUILT + TESTED, still
  abort nested). The hyps are legitimate, but removing the over-rejection needs RELATIVITY (B), not a
  library refactor. The earlier "✅ ANSWERED" here was wrong.
- **Why does nested Pi-mint work but nested GoodRespond not?** — Pi is *trivial* (one mint, hyp-free
  result, no deep nested `apply_policed`), so the outer walk never needs to triage an inner hyp.
  GoodRespond's respond yields hyp-bearing intermediates (`Extend (M s)`) + nested `apply_policed`. The
  axis is **complexity/depth of the inner walk**, not managed-vs-raw mints.
- **REMAINING (the actual blocker):** the outer `param_walker` polices the inner walk's hyps. Removing
  that needs **relativity** (Option B / §7) — a substrate carve-out, soundness-gated on the pass-through
  forgeability argument (§4B). There is NO library-level fix; managed mints are a dead end for nesting.
- **Perf / respond-kind dispatch:** (still relevant once B lands) verifying gated responds at
  universe-check time; and the non-inductive responds (Pi/Record/Eq) still need their own cheaper specs
  for *full* metacircularity.
