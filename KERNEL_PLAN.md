# Unified Mutual-Fix Kernel Plan

## What we proved

The mutual-fix kernel works. All earlier "budget exhaustion" and "OOM" failures were caused by a **swapped ite2 argument bug** (`ite2 MATCH ELSE COND` — match must be first), not fundamental cost issues.

Validated numbers with the full 10-component kernel:
- Compilation: 475 steps (all definitions)
- `Nat(LEAF)`: 282 steps, 163 stack depth
- `NatToNat` construction: 20 steps
- CPS signature queries: 0 compile-time cost (deferred to runtime via `wait`)

## Architecture

### One kernel, one fix

```disp
kernel = fix ({self, query} -> ite2 q_accum_fn k2 (fast_eq query Q_ACCUM) self query)
```

All type checkers + accum + ton_check + signature queries live inside a single `fix`. Components reference each other through `self`. No tags — signatures identify type formers.

### Query dispatch chain

Each level: `ite2 MATCH_FN ELSE_FN (fast_eq query Q_TARGET) self query`

**CRITICAL: match goes FIRST in ite2.** `ite2(then, else, cond)` returns `then` when cond=TT.

```disp
let k9 = {self, query} -> ite2 q_sig_pi_fn    q_sig_type_fn (fast_eq query Q_SIG_TYPE)  self query
let k8 = {self, query} -> ite2 q_sig_accum_fn  k9            (fast_eq query Q_SIG_ACCUM) self query
let k7 = {self, query} -> ite2 q_ton_fn        k8            (fast_eq query Q_TON)       self query
let k6 = {self, query} -> ite2 q_type_fn       k7            (fast_eq query Q_TYPE)      self query
let k5 = {self, query} -> ite2 q_eq_fn         k6            (fast_eq query Q_EQ)        self query
let k4 = {self, query} -> ite2 q_bool_fn       k5            (fast_eq query Q_BOOL)      self query
let k3 = {self, query} -> ite2 q_nat_fn        k4            (fast_eq query Q_NAT)       self query
let k2 = {self, query} -> ite2 q_pi_fn         k3            (fast_eq query Q_PI)        self query
let kernel = fix ({self, query} ->
  ite2 q_accum_fn k2 (fast_eq query Q_ACCUM) self query)
```

### Query constants

```disp
let Q_ACCUM     = t           // accum handler
let Q_PI        = t t         // Pi checker
let Q_NAT       = t t t       // Nat checker
let Q_BOOL      = t (t t)     // Bool checker
let Q_EQ        = t t (t t)   // Eq checker
let Q_TYPE      = t (t (t t)) // Type n checker
let Q_TON       = t t t t     // ton_check
let Q_SIG_ACCUM = t (t t t)   // CPS accum signature
let Q_SIG_PI    = t t (t t t) // CPS Pi signature
let Q_SIG_TYPE  = t (t (t t t)) // CPS Type signature
```

### CPS signature queries (deferred compile-time evaluation)

Signatures are returned as CPS functions: `{check_fn} -> check_fn(signature)`.

```disp
let q_sig_accum_fn = {self, query} -> {c} -> c (pair_fst (wait (self Q_ACCUM) t))
let q_sig_pi_fn    = {self, query} -> {c} -> c (pair_fst (wait (self Q_PI) t))
let q_sig_type_fn  = {self, query} -> {c} -> c (pair_fst (wait (self Q_TYPE) t))
```

Extracted API uses `wait kernel Q_SIG_X` to defer the kernel query to runtime:

```disp
let is_neutral = {x} -> wait kernel Q_SIG_ACCUM (fast_eq (pair_fst x))
let is_pi      = {x} -> wait kernel Q_SIG_PI    (fast_eq (pair_fst x))
let is_universe= {x} -> wait kernel Q_SIG_TYPE  (fast_eq (pair_fst x))
```

`wait(kernel)(Q_SIG_X)` at compile time: just tree construction (~2 steps).
At runtime (first call): kernel unfolding + sig computation (~200 steps, then memoized).

### Component builders

Each `q_*_fn = {self, query} -> <component>` is a **closed function** (no free variables shared with the dispatch condition). Select-then-apply: ite2 picks the function, then `self query` is applied after.

#### Neutral detection inside checkers

Every checker uses this CPS pattern for is_neutral instead of a direct signature comparison:
```disp
// Instead of: fast_eq (pair_fst v) NEUTRAL_SIG
// Use:        wait (self Q_SIG_ACCUM) (fast_eq (pair_fst v))
```

This defers the signature lookup to runtime, avoiding compile-time kernel evaluation.

#### Accum handler

Simple accumulator (no type tracking in this version):
```disp
let q_accum_fn = {self, query} -> fix ({s, m, v} -> wait s (t m v))
```

Metadata: `fork(type, depth)` for hypotheses. Spine accumulates as `fork(inner_meta, arg)`.

#### ton_check

Trivial extraction with CPS neutral guard:
```disp
let q_ton_fn = {self, query} -> {check_fn, v} ->
  ite2
    ({check_fn, v} -> check_fn (pair_fst (type_meta v)))
    ({check_fn, v} -> FF)
    (wait (self Q_SIG_ACCUM) (fast_eq (pair_fst v)))
    check_fn v
```

NOTE: This is the CURRENT spine-walking ton_check simplified. Since accum doesn't track types yet, we still need the spine-walking version for now. The typed-neutral optimization (O(1) ton_check) can be added later by making accum smart.

Actually — for the initial implementation, keep the current CPS spine-walking ton_check from the tag-based design. The spine metadata structure is the same (fork(HYP_TAG, fork(type, id)) for base, fork(inner, arg) for spine, fork(ELIM_TAG, fork(motive, target)) for elim). The only difference: recognition uses CPS signature checks instead of tag constants.

#### Pi checker

```disp
let q_pi_fn = {self, query} -> fix ({s, m, v} ->
  ite2
    // H-rule: v is neutral
    ({s, m, v} -> self Q_TON (fast_eq (wait s m)) v)
    // Body: check codomain
    ({s, m, v} ->
      ite2
        // result is neutral → ton_check
        ({s, m, v} ->
          self Q_TON
            (fast_eq (COD_FN(m) (HYP(m))))
            (v (HYP(m))))
        // result is concrete → raw apply
        ({s, m, v} ->
          fast_eq (COD_FN(m) (HYP(m)) (v (HYP(m)))) TT)
        (wait (self Q_SIG_ACCUM)
          (fast_eq (pair_fst (v (HYP(m))))))
        s m v)
    (wait (self Q_SIG_ACCUM) (fast_eq (pair_fst v)))
    s m v)
```

Where:
- `HYP(m) = wait (self Q_ACCUM) (t (pair_fst m) (pair_fst (pair_snd m)))` — mkVHyp via kernel accum
- `COD_FN(m) = pair_snd (pair_snd m)` — codomain function from Pi metadata
- Pi metadata: `fork(domain, fork(depth, codFn))` — NO tag prefix

#### Nat, Bool, Eq checkers

Same logic as current, but:
- Replace `is_neutral` check with `wait (self Q_SIG_ACCUM) (fast_eq (pair_fst n))`
- Replace `is_tree_fork` check with `ite2 FF (triage ...) (wait (self Q_SIG_ACCUM) (fast_eq (pair_fst n)))` (exclude neutrals)
- Replace `ton_check` call with `self Q_TON`
- Keep `wait self2 meta` for self-type reconstruction (H-rule)

#### Type n checker

Same logic as current, but:
- Replace `is_neutral` with CPS sig check
- Replace `is_universe` with `wait (self Q_SIG_TYPE) (fast_eq (pair_fst x))`
- Replace `is_pi` with `wait (self Q_SIG_PI) (fast_eq (pair_fst x))`
- Replace `is_registered(x)` with `ite2 TT (fast_eq x (wait (wait self Q_BOOL) t)) (fast_eq x (wait (wait self Q_NAT) t))`
- Universe rank: `type_meta x` (no UNIV_TAG prefix, metadata IS the rank)

### Tag-free metadata

| Type | Current metadata | New metadata |
|------|-----------------|--------------|
| Pi | `fork(PI_TAG, fork(dom, fork(depth, codFn)))` | `fork(dom, fork(depth, codFn))` |
| Universe | `fork(UNIV_TAG, rank)` | `rank` |
| Eq | `fork(EQ_TAG, fork(A, fork(x, y)))` | `fork(A, fork(x, y))` |
| Nat | `LEAF` | `LEAF` |
| Bool | `LEAF` | `LEAF` |
| Neutral (hyp) | `fork(HYP_TAG, fork(type, id))` | `fork(type, id)` |
| Neutral (elim) | `fork(ELIM_TAG, fork(motive, target))` | `fork(motive, target)` |
| Neutral (spine) | `fork(inner_meta, arg)` | `fork(inner_meta, arg)` |

**Problem**: Without HYP_TAG/ELIM_TAG, ton_check can't distinguish base hypotheses from spine entries from elim entries. All three are `fork(something, something)`.

**Solutions**:
1. Keep HYP_TAG and ELIM_TAG in neutral metadata only (2 tags instead of 5) — simplest
2. Use structural convention: base = `fork(LEAF, fork(type, id))` with LEAF marker, elim = `fork(fork(LEAF, LEAF), fork(motive, target))` — fragile
3. Use typed neutrals (smart accum) — most elegant but more complex

**Recommended for initial implementation**: Option 1. Keep HYP_TAG and ELIM_TAG. Eliminate PI_TAG, UNIV_TAG, EQ_TAG. This reduces from 5 tags to 2 and proves the kernel architecture works.

### Extracted API (type constructors use double-wait)

```disp
let Nat = wait (wait kernel Q_NAT) t
let Bool = wait (wait kernel Q_BOOL) t
let mkEq = {A, x, y} -> wait (wait kernel Q_EQ) (t A (t x y))
let mkType = {rank} -> wait (wait kernel Q_TYPE) rank
let mkPi = {domain, codFn, depth} -> wait (wait kernel Q_PI) (t domain (t depth codFn))
let mkArrow = {a, b, depth} -> mkPi a ({_} -> b) depth
```

Double-wait: `wait(wait(kernel)(Q_X))(metadata)`. At compile time, `wait(kernel)(Q_X)` is just tree construction. The full `kernel(Q_X)` evaluation is deferred to when the type is applied to a value.

`ton_check` needs special handling since it's a 2-arg function, not a checker:
```disp
let ton_check = wait kernel Q_TON
// ton_check(check_fn)(v) = kernel(Q_TON)(check_fn)(v)
```

`mkVHyp` uses kernel Q_ACCUM directly (not double-wait, since accum is the handler):
```disp
let mkVHyp = {ty, depth} -> wait (kernel Q_ACCUM) (t HYP_TAG (t ty depth))
```

## File structure

### Merge nbe.disp into types.disp

Since the kernel contains everything, the nbe/types split is unnecessary. Types.disp becomes the single kernel + standard library file. nbe.disp becomes a thin re-export for backward compatibility:

```disp
// nbe.disp
open use "prelude.disp"
open use "types.disp"
```

### types.disp layout

1. Helpers: type_meta, conv
2. Query constants
3. HYP_TAG, ELIM_TAG (only 2 neutral tags remain)
4. CPS signature query builders
5. Kernel component builders (q_accum_fn, q_ton_fn, q_pi_fn, q_nat_fn, q_bool_fn, q_eq_fn, q_type_fn)
6. Kernel assembly (k2..k9, kernel = fix ...)
7. Extracted API (is_neutral, is_pi, etc.)
8. Nat helpers (zero, succ, ...)
9. Typed eliminators (bool_rec, nat_rec)
10. Eq operations (refl, eq_J, eq_sym, eq_cong, eq_subst)
11. nat_le, nat_lt (if not inside kernel — they're used by q_type_fn so they must be defined before it)
12. conv_structural
13. Arithmetic (add)
14. Type shorthands (NatToNat, Type0, Type1, Type2)

**NOTE**: nat_le/nat_lt are used inside q_type_fn. They must be defined before the kernel components. Since they don't reference the kernel, this is fine.

## Tests

### nbe.test.disp

- Kernel query dispatch (each Q returns the right component)
- Neutral detection (is_neutral via CPS sig)
- Spine accumulation (applying neutrals)
- Metadata extraction
- Signature stability (all types with same checker share signature)

### types.test.disp

All existing tests should pass with updated API:
- `napply T v` → already `T v` (raw apply via double-wait)
- `val_apply f x` → already `f x` (raw apply, accum handles neutrals)
- `type_apply T v` → eliminated (Pi checker handles internally)
- `mkVHyp_prog` → `mkVHyp`
- Tag-specific tests (HYP_TAG, ELIM_TAG) remain for neutral metadata
- Signature-based recognition tests replace PI_TAG/UNIV_TAG/EQ_TAG tests

Plus new tests: List, Vec, add_comm, add_assoc (as stretch goals).

## Blockers discovered (2026-04-29 implementation attempt)

### 1. Double-wait H-rule identity mismatch — SOLVED

Inside a checker `fix ({s, m, v} -> ...)`, the H-rule reconstructs the type as `wait s m` (= `wait(checker)(metadata)`). But the external type is `wait(wait(kernel)(Q_X))(metadata)` (double-wait). These are DIFFERENT trees:
- `wait(checker)(metadata)` — first arg is the checker fix itself
- `wait(wait(kernel)(Q_X))(metadata)` — first arg is a deferred kernel dispatch

**Fix**: Use `wait (wait kernel_self query) m` for H-rule reconstruction (kernel_self and query are captured from the dispatch). This produces the same tree as the external type. Keep `wait s m` for self-recursion (e.g., Nat succ case) since that just needs to apply the checker, not compare identities.

### 2. Combinator explosion with large closures — RESOLVED by smart accum

Functions like `q_ton_fn` that capture `kernel_self` (a large tree) as a closure variable produce enormous S/K/I combinator trees during bracket abstraction. Applying these to the kernel tree triggers >10M evaluation steps.

**Root cause**: The spine-walking `ton_check_meta` (a large `fix`-based tree) combined with multiple `kernel_self` references (for sig checks) creates closures too expensive to reduce.

**Resolution**: Smart accum eliminates spine-walking ton_check entirely. See "Revised approach" below. The new `q_ton_fn` is a trivial extraction with 1 kernel_self reference — well within the combinator budget.

### 3. Circularity in tag elimination — RESOLVED by smart accum

Eliminating PI_TAG requires `is_pi` to use a signature check: `fast_eq(pair_fst(v), PI_SIG)`. But:
- `PI_SIG` = `pair_fst(wait(pi_checker)(t))` — needs pi_checker
- `pi_checker` uses `ton_check` for H-rule
- `ton_check` uses `is_pi` for spine walking
- Circular!

**Resolution**: Smart accum uses PI_TAG for Pi recognition during accumulation (constant, no signature needed). ton_check no longer needs `is_pi` at all — it just extracts the stored type. The circularity is broken.

PI_TAG remains in Pi metadata. This is acceptable: it's a small constant used only by accum, not a deep architectural dependency.

## Revised approach: Smart accum (O(1) ton_check)

### Core insight

The spine-walking ton_check was the root cause of both the combinator explosion (blocker #2) and the circularity (blocker #3). The fix: **store the type at a fixed position in every neutral's metadata**, so ton_check is a simple extraction.

Instead of dumb accumulation (`wait self (fork meta v)`) followed by expensive O(d) spine walking, make accum **type-aware**: at each accumulation step, if the neutral's type is Pi, compute `codFn(v)` as the result type.

### Metadata format change

```
Old (dumb accum — type must be inferred by walking):
  VHyp:       fork(HYP_TAG, fork(type, id))
  VStuck:     fork(inner_meta, arg)           ← no type stored!
  VStuckElim: fork(ELIM_TAG, fork(motive, target))

New (smart accum — type always at pair_fst):
  VHyp:       fork(type, fork(HYP_TAG, id))
  VStuck:     fork(result_type, fork(old_meta, arg))
  VStuckElim: fork(result_type, fork(ELIM_TAG, fork(motive, target)))
```

Type is always at `pair_fst(type_meta(v))` = `pair_fst(pair_snd(pair_snd(v)))`.

### Smart accum implementation

When a neutral is applied to `v`:
- If the neutral's type is Pi: `result_type = codFn(v)` (codomain applied to argument)
- Otherwise: `result_type = FF` (unknown — ensures ill-typed applications fail)

Uses PI_TAG directly — **no kernel reference needed**.

```disp
let accum_pi_fn = {self, meta, v} ->
  // meta = fork(type, payload). type is Pi.
  // Extract codFn = pair_snd(pair_snd(pair_snd(type_meta(type))))
  // Result type = codFn(v)
  wait self
    (t (pair_snd (pair_snd (pair_snd (type_meta (pair_fst meta)))) v)
       (t meta v))

let accum_non_pi_fn = {self, meta, v} ->
  // Type is not Pi (or is FF/garbage) — store FF as result type
  wait self (t FF (t meta v))

let accum = fix ({self, meta, v} ->
  ite2 accum_pi_fn accum_non_pi_fn
    (fast_eq (pair_fst (type_meta (pair_fst meta))) PI_TAG)
    self meta v)
```

### O(1) ton_check

```disp
// Extract stored type from neutral, pass to check_fn.
// Returns FF for non-neutrals.
let ton_check_neutral_fn = {check_fn, v} -> check_fn (pair_fst (type_meta v))
let ton_check_ff_fn = {check_fn, v} -> FF
let ton_check = {check_fn, v} ->
  ite2 ton_check_neutral_fn ton_check_ff_fn (is_neutral v) check_fn v
```

No `fix`, no spine walking. Just metadata extraction.

### Updated neutral constructors

```disp
// Type goes FIRST in metadata (before HYP_TAG/ELIM_TAG payload)
let mkVHyp = {ty, id} -> wait accum (t ty (t HYP_TAG id))
let mkVStuckElim = {result_type, target} -> wait accum (t result_type (t ELIM_TAG target))

// Convenience accessor
let neutral_type = {v} -> pair_fst (type_meta v)
```

### Why this resolves the kernel blocker

In the unified kernel, `q_ton_fn` was the blocker. With smart accum:

| Component | kernel_self refs | Notes |
|-----------|-----------------|-------|
| `q_accum_fn` | **0** | Uses PI_TAG directly, no kernel needed |
| `q_ton_fn` | **1** | Just is_neutral sig check, no fix, no spine walking |
| `q_pi_fn` | ~3 | Already worked within budget |
| `q_nat_fn` | ~1-2 | Already worked |
| `q_bool_fn` | ~1-2 | Already worked |
| `q_eq_fn` | ~1-2 | Already worked |
| `q_type_fn` | ~3-4 | Should work |

The ton_check_meta fix tree (the large closure) is completely eliminated. `q_ton_fn` becomes:

```disp
let q_ton_fn = {kernel_self, query} -> {check_fn, v} ->
  ite2
    ({check_fn, v} -> check_fn (pair_fst (type_meta v)))
    ({check_fn, v} -> FF)
    (kernel_self Q_SIG_ACCUM (fast_eq (pair_fst v)))
    check_fn v
```

One kernel_self reference, two small closed branch functions. Well within budget.

### Correctness argument

**H-rule**: `ton_check(fast_eq(wait self meta))(v)` extracts v's stored type and compares with the checker's reconstructed type. For VHyp created as `mkVHyp(T, id)`, the stored type IS `T`. If `T = wait(checker)(meta)`, the comparison is `fast_eq(T, wait(self)(meta))` — correct since `self` is the checker's fix.

**Codomain check**: When Pi checker evaluates `v(hyp)` and the result is neutral, `ton_check(fast_eq(codFn(hyp)))(v(hyp))` extracts the result's stored type. If `v` is a Pi-typed neutral, then `v(hyp)` triggers smart accum which computes `codFn_of_v_type(hyp)` as the result type. Since v's type IS the Pi being checked, `codFn_of_v_type = codFn`, so the check is consistent.

**Non-Pi application**: If a neutral's type is not Pi (e.g., Nat-typed), smart accum stores FF. Then `ton_check(check_fn)(result) = check_fn(FF) = FF` since no checker recognizes FF as a valid type. Ill-typed neutral applications correctly fail.

### Performance trade-off

Smart accum does more work per neutral application (~20-30 extra steps for Pi check + codFn extraction) but makes ton_check O(1) instead of O(spine_depth).

| Operation | Dumb accum | Smart accum |
|-----------|-----------|-------------|
| Neutral apply | ~4 steps | ~25-35 steps |
| ton_check | O(d) spine walk | O(1) extraction |
| Total per neutral op | O(d) | O(1) amortized |

For shallow spines (d=1-2), roughly a wash. For deep spines, smart accum wins decisively. Net positive as programs grow more complex.

### Worked example: neutral application chain

```
// 1. Create a Pi-typed hypothesis
PiNatNat = mkPi Nat ({_} -> Nat) depth
hyp_f = mkVHyp PiNatNat id0
  = wait(accum)(fork(PiNatNat, fork(HYP_TAG, id0)))

// 2. Apply hyp_f to zero
hyp_f zero → accum(fork(PiNatNat, fork(HYP_TAG, id0)))(zero)
  pair_fst(meta) = PiNatNat
  type_meta(PiNatNat) = fork(PI_TAG, fork(Nat, fork(depth, {_}->Nat)))
  pair_fst(type_meta(PiNatNat)) = PI_TAG → is Pi!
  codFn = {_} -> Nat
  result_type = codFn(zero) = Nat
  → wait(accum)(fork(Nat, fork(fork(PiNatNat, fork(HYP_TAG, id0)), zero)))

// 3. ton_check on the result
ton_check (fast_eq Nat) (hyp_f zero)
  is_neutral? → TT
  pair_fst(type_meta(result)) = Nat
  fast_eq(Nat, Nat) = TT ✓

// 4. Ill-typed: apply a Nat-typed neutral
hyp_n = mkVHyp Nat id1
  = wait(accum)(fork(Nat, fork(HYP_TAG, id1)))
hyp_n zero → accum(fork(Nat, fork(HYP_TAG, id1)))(zero)
  pair_fst(meta) = Nat
  type_meta(Nat) = LEAF (Nat metadata is `t`)
  pair_fst(LEAF) = garbage ≠ PI_TAG → not Pi!
  result_type = FF
  → wait(accum)(fork(FF, fork(fork(Nat, fork(HYP_TAG, id1)), zero)))

ton_check (fast_eq Nat) (hyp_n zero)
  pair_fst(type_meta(result)) = FF
  fast_eq(Nat, FF) = FF → correctly rejects ✓
```

### Deleted code

With smart accum, the following are eliminated:
- `ton_check_meta` (fix-based spine walker)
- `ton_hyp_fn`, `ton_spine_fn`, `ton_elim_fn`, `ton_spine_cont`
- `ton_check_neutral_fn` (old version that called ton_check_meta)
- All spine-walking logic

### Tags remaining

- **PI_TAG**: used by smart accum for Pi recognition during accumulation. Small constant.
- **HYP_TAG**: distinguishes base hypotheses from spine entries in neutral metadata. Used by debugging/introspection, not required by type checking.
- **ELIM_TAG**: distinguishes stuck eliminators from spine entries. Same role as HYP_TAG.

UNIV_TAG and EQ_TAG: can be eliminated in the unified kernel (signature-based recognition via CPS queries). Or kept for simplicity in standalone mode. Not load-bearing for the architecture.

## Implementation plan (phased)

### Phase 1: Smart accum standalone (no kernel)

Modify nbe.disp and types.disp to use smart accum with the existing separate-checker architecture. This validates the approach independently.

- [x] Implement smart accum in nbe.disp (new `accum`, new metadata format)
- [x] Replace spine-walking ton_check with O(1) extraction in nbe.disp
- [x] Update `mkVHyp`: `{ty, id} -> wait accum (t ty (t HYP_TAG id))`
- [x] Update `mkVStuckElim`: `{result_type, target} -> wait accum (t result_type (t ELIM_TAG target))`
- [x] Add `neutral_type = {v} -> pair_fst (type_meta v)` helper
- [x] Update Pi checker: metadata accessors unchanged, but H-rule and codomain checks use new ton_check (API unchanged, should be transparent)
- [x] Update types.disp: Eq accessors shift by one level (type now at pair_fst of metadata)
  - No change needed if Eq keeps EQ_TAG at pair_fst position (type_meta still works same way)
  - Actually: VHyp metadata changes, not type metadata. Type metadata (PI_TAG/EQ_TAG/UNIV_TAG) is unchanged.
- [x] Run `npm test` — all current tests pass
- [x] Verify step counts haven't regressed significantly

### Phase 2: Unified kernel — BLOCKED (fix body too large)

**Attempted (2026-04-29).** All then-current tests passed with standalone smart accum. The unified kernel compiles and dispatches correctly for 8 of 9 components — Nat, Bool, Pi, Eq checkers, smart accum, sig queries, and H-rule tests all pass. **Only q_type_fn (the Type n checker) exceeds the evaluation budget**, because it has 5 kernel_self references (Q_SIG_ACCUM, Q_ACCUM, Q_NAT, Q_BOOL, plus tag checks). The other checkers have 1-2 kernel_self references and work fine.

The blocker is narrow: `Type0(Nat)` and `Type0(Bool)` fail because q_type_fn's registered-type check does `fast_eq x (wait (ks Q_NAT) t)` and `fast_eq x (wait (ks Q_BOOL) t)` — two extra kernel dispatches inside an already-expensive closure.

#### Targeted fix: reduce q_type_fn's kernel_self references

The registered-type check `fast_eq x (wait (ks Q_NAT) t)` requires `ks Q_NAT` to reconstruct the Nat type at check time — 1 kernel dispatch per registered type. With Nat and Bool, that's 2 extra kernel_self references.

**Fix: add NAT_TAG and BOOL_TAG.** Give Nat and Bool metadata tags like the other types:
```
Nat = wait(nat_checker)(fork(NAT_TAG, LEAF))
Bool = wait(bool_checker)(fork(BOOL_TAG, LEAF))
```
Then q_type_fn recognizes them via `fast_eq(pair_fst(type_meta(x)), NAT_TAG)` — a constant comparison, zero kernel_self references.

This drops q_type_fn from 5 to 3 kernel_self references (Q_SIG_ACCUM + Q_ACCUM + one for Pi codomain hyp), matching the proven budget of the other checkers. The tags are small constants that don't add architectural complexity.

**Implemented in standalone smart-accum mode (2026-04-29).** In that
intermediate version, `Nat` and `Bool` used `NAT_TAG`/`BOOL_TAG`, and
`is_registered` checked metadata tags instead
of comparing against reconstructed `Nat`/`Bool` trees. Current verification:
`npm test` passes, and `npm run disp -- --stats lib/types.test.disp` reports
about 15k evaluator steps and 9k newly allocated hash-cons nodes for 105
library tests.

**Superseded in selector-query kernel mode.** This tag-only registered-type
recognition was useful for getting past the old budget failure, but it is not
the current trusted design. Nat/Bool recognition is now canonical identity;
the tags remain only as constructor metadata payloads and adversarial tests
check that fake `NAT_TAG`/`BOOL_TAG` values are rejected.

**Alternative: precompute Nat/Bool outside the kernel and pass as parameters.** Define q_type_fn as `{ks, q, nat_type, bool_type} -> fix(...)` and partially apply at assembly time. This avoids tags but changes the dispatch interface.

#### Why the kernel is too expensive: an intuitive explanation

**How functions compile.** Lambda expressions like `{x} -> body` compile to combinator trees using bracket abstraction: `[x]body` systematically eliminates the variable `x` using three combinators — S (substitute), K (constant), I (identity). Multi-variable functions nest: `{x, y} -> body` becomes `[x][y]body`. Each free-variable reference in the body creates an S-combinator node that threads the argument through. More references to `x` → more S nodes → bigger tree.

**How recursion works.** Tree calculus is strict — the Y combinator diverges. Recursion uses `fix` built on `wait`:
```
fix f = wait m ({x} -> f (wait m x))   where m = {x} -> x x
```
`fix(f)` is a one-argument function. Calling `fix(f)(arg)` unfolds one step: `f(fix(f))(arg)`. The key cost: `m(g) = g(g)` is a self-application that **duplicates the entire `g` tree**, and `g` contains `f` (the full body). So each fix unfolding pays a cost proportional to the body size.

For a standalone checker like `nat_checker = fix({self, m, n} -> ...)`, the body is small (~50–100 combinator nodes). Fix unfolding is cheap. This is why standalone checkers work fine.

**What the kernel does.** The kernel puts ALL checkers into a single fix with a dispatch table:
```
kernel = fix ({self, query} ->
  if query = Q_ACCUM then accum_handler(self)
  else if query = Q_NAT then nat_checker(self)
  else if query = Q_PI then pi_checker(self)
  ... 8 more components ...)
```
Each component is a function `{kernel_self, query} -> fix(...)` that captures `kernel_self` (the full kernel). After bracket abstraction, the dispatch chain inlines every component as a combinator constant. The kernel body is the **sum** of all components — roughly 1000–3000 nodes.

**Why nested dispatch explodes.** A call like `kernel(Q_NAT)` unfolds the fix once: self-application duplicates the ~2000-node body, dispatches to Q_NAT, and returns the Nat checker. This first unfolding is expensive but within budget for simple checks.

But `Type0(Nat)` requires **nested** dispatches:
1. `kernel(Q_TYPE)` — unfolds the kernel fix (cost: ~2000 nodes duplicated)
2. Type n checks is_neutral(Nat): `kernel(Q_SIG_ACCUM)` — unfolds **again**
3. Type n checks registered(Nat): `kernel(Q_NAT)` — unfolds **again**

Each unfolding duplicates the full body. The runtime has memoization (same function + same argument = cached result), so repeated calls to `kernel(Q_NAT)` with the same args are free after the first. But the first three distinct queries each pay the full unfolding cost. Three unfoldings of a ~2000-node body, with S-combinator reduction creating intermediate trees at each step, exceeds the 10M step budget.

**The core tension.** Standalone checkers have small fix bodies (~100 nodes) and unfold cheaply. The kernel has a huge fix body (~2000 nodes) because it must contain everything. The fix mechanism doesn't distinguish "I need component X" from "I carry the whole dispatch table" — it always duplicates the entire body.

#### Paths forward

##### Path 1: Two-phase kernel

Split the monolithic fix into cooperating smaller pieces.

**Phase 1 fix — infrastructure kernel** (~3 components, ~200 nodes):
```
infra = fix ({self, query} ->
  if query = Q_ACCUM then smart_accum
  else if query = Q_SIG_ACCUM then {c} -> c(pair_fst(wait(self Q_ACCUM) t))
  else if query = Q_SIG_PI then {c} -> c(PI_SIG)  // precomputed
  ...)
```
This is small enough for cheap fix unfolding. It provides:
- `accum` — the smart neutral handler
- Signature queries — for is_neutral checks inside checkers

**Phase 2 — separate checker fixes**, each receiving `infra` as a parameter:
```
nat_checker = fix ({self, m, n} ->
  ite2
    // H-rule: v is neutral
    ({self, m, n} -> fast_eq (wait self m) (neutral_type n))
    // Body: concrete check
    ({self, m, n} -> ... zero/succ logic ...)
    // is_neutral via infra
    (infra Q_SIG_ACCUM (fast_eq (pair_fst n)))
    self m n)
```
Each checker fix body is small (~100-200 nodes) because `infra` is a captured constant, not a huge dispatch chain. The `infra` dispatch is cheap (small fix body).

**Challenge: Type n needs to recognize other types.** Type n checks whether `x` is a Pi, Universe, or registered base type. Currently it uses tags (PI_TAG, UNIV_TAG) and direct comparison (`fast_eq x Nat`). With two-phase:
- Tags work the same (PI_TAG is a constant, no kernel needed).
- Registered check: `fast_eq(x, Nat)` requires `Nat` to be defined, which it is — it's a separate checker fix, defined before Type n. No circular dependency.
- is_neutral: uses `infra Q_SIG_ACCUM`. Cheap dispatch.

**Why this should work:** Each fix body is ~100-200 nodes. Fix unfolding of `nat_checker(m)(n)` duplicates ~150 nodes — well within budget. The `infra` dispatch adds ~50 steps per is_neutral check. Total for `Type0(Nat)`: ~500 steps instead of 10M+.

**What's lost:** True mutual recursion between checkers (e.g., Nat checker calling Bool checker). In practice, no checker currently needs another checker — they only need infrastructure (is_neutral, accum, tags). Type n needs to CHECK other types (`wait self m (pi_dom x)`) but that's self-recursion, not cross-checker calls.

**Soundness constraints.** Two-phase is not automatically sound; it is sound
only if the phase boundary preserves canonical identity:

- Registered closed types (`Nat`, `Bool`) should be recognized by canonical
  equality where possible, or by a checker-signature plus metadata invariant
  derived from the canonical constructor. Public metadata tags alone are not a
  proof because users can construct arbitrary trees.
- Signature constants exported by the infrastructure phase must be derived from
  the actual checker/accum functions, not copied as independent constants. If a
  copied signature diverges from the implementation, H-rule checks can accept or
  reject the wrong neutral class.
- Each checker must have one canonical instance for exported types. Accidentally
  rebuilding an equivalent checker under a different captured `infra` value
  creates a distinct tree, so `fast_eq`-based H-rule identity can fail even if
  the source text is equivalent.
- Checkers in phase 2 may depend on infrastructure (`accum`, neutral
  recognition, signatures), but they should not rely on user-forgeable tags as
  the final authority for typehood.

This makes two-phase an "ideal enough" runtime architecture for the current
tree-calculus constraints: small fix bodies, canonical exported constructors,
and explicit identity checks where the theory needs identity. It is less
architecturally pure than one mutual fix, but the one-fix design pays for every
component at each unfolding.

##### Path 2: Compiler optimization (B/C/S' combinators)

The current bracket abstraction uses only S, K, I. This is correct but produces large trees. Turner's extended combinator set handles common patterns more efficiently:

| Combinator | Reduction | Replaces |
|---|---|---|
| `B f g x` | `f(g(x))` | `S(K f)(g)` — saves one node |
| `C f x y` | `f(y)(x)` | `S(S(K f)(g))(K x)` — saves two nodes |
| `S' c f g x` | `c(f x)(g x)` | Nested S — saves several nodes |

**Impact on kernel body size:** The dispatch chain is mostly constant functions and applications. B (composition) and C (flip) handle the common pattern of "apply a known function to a computed argument" without S's full generality. Estimated reduction: 30–50% smaller combinator trees.

If the kernel body shrinks from ~2000 to ~1000 nodes, fix unfolding cost drops proportionally. Three nested dispatches at 1000 nodes each might fit within the 10M budget.

**Implementation sketch:**
1. Add B/C/S' as tree-calculus constants (each is a small tree: B = ~10 nodes, C = ~12 nodes, S' = ~20 nodes).
2. Extend `abstractName` in `src/parse.ts` to detect B/C/S' patterns before falling through to S.
3. Key patterns:
   - `S(K f)(g)` where f is constant → `B f g` (composition)
   - `S f (K x)` where x is constant → `C f x` (flip)
   - `S(B c f)(g)` → `S' c f g` (guarded S)
4. Benchmark: measure tree sizes and step counts before/after on the standalone checkers, then attempt the kernel.

**Risk:** B/C tree encodings in tree calculus are ~10-20 nodes each. For functions with few variables (2-3), the overhead of encoding B/C outweighs the savings. The optimization must be selective — apply B/C only when the resulting tree is actually smaller. The lambada project's analysis suggests B/C are net positive for 4+ variable functions, which describes the kernel components.

**Additional optimization — size-bounded K-composition:** The current `cirToTree` eagerly evaluates `S(K p)(K q) → K(p q)` at compile time. When `p` is a huge tree (like the kernel fix), `p(q)` triggers an expensive evaluation. Adding a size check:
```
if (size(af.x) > THRESHOLD || size(ax.x) > THRESHOLD) return cap(cap(S, af), ax)
```
would prevent compile-time blowup on large trees while preserving the optimization for small constants. This is orthogonal to B/C combinators and could be tried first as a quick win.

##### Path 3: Selector-query dispatch — IMPLEMENTED

Replace the linear `ite2`/`fast_eq` query chain with Church selectors:

```disp
let Q_NAT = {q_accum, q_pi, q_nat, ...} -> q_nat

let kernel = fix ({self, query} ->
  query q_accum_fn q_pi_fn q_nat_fn q_bool_fn q_eq_fn
        q_type_fn q_ton_fn q_sig_accum_fn q_sig_pi_fn q_sig_eq_fn q_sig_type_fn
        self query)
```

This keeps the single mutual-kernel authority while removing the linear query
comparison chain. Implemented in `lib/nbe.disp`; `lib/types.disp` is now a
re-export layer over the unified selector-query kernel.

Current verification:
- `npm test` passes.
- `npm run disp -- --stats --stats-detail lib/nbe.test.disp`: 15/15 tests pass,
  about 21.6k evaluator steps.
- `npm run disp -- --stats lib/types.test.disp`: 135/135 tests pass, about
  32.9k evaluator steps.

The previous `Type0(Nat)`/`Type0(Bool)` budget failure is gone. The remaining
cost is mostly up-front file opening/record projection: opening the unified
`nbe.disp` costs about 20.7k steps because the file now contains the full
kernel plus the type-level surface. This is acceptable for correctness and is a
better target for compiler/open-use optimization than the old 10M runtime
dispatch failure.

Current soundness cleanup:
- Smart accum recognizes Pi by checker signature, not by forgeable `PI_TAG`.
- Eq has a signature query (`Q_SIG_EQ`), `is_eq` uses it, and `Type n` checks
  Eq formation.
- Nat/Bool registered-type recognition uses canonical identity, not
  `NAT_TAG`/`BOOL_TAG` tag-only checks.
- `lib/types.test.disp` includes adversarial fake-tag tests for Nat, Bool, Eq,
  Universe, and Pi metadata.

## Previous implementation checklist (kernel attempt #1)

Preserved for reference. Items checked off during the 2026-04-29 attempt:

- [x] Define query constants and HYP_TAG/ELIM_TAG
- [x] Define nat_le/nat_lt (needed by q_type_fn, no kernel dependency — uses raw triage)
- [x] Define CPS sig query builders (q_sig_accum_fn, q_sig_pi_fn, q_sig_eq_fn, q_sig_type_fn)
- [x] Define q_accum_fn (simple accumulator, keeps HYP_TAG/ELIM_TAG in metadata)
- [x] Define q_nat_fn, q_bool_fn, q_eq_fn (with H-rule fix: `wait(wait kernel_self query) m`)
- [x] Define q_pi_fn (with H-rule fix)
- [x] Define q_type_fn (with tag-free metadata)
- [x] Assemble kernel dispatch chain (k2..k9, kernel = fix ...)
- [x] Extract API: is_neutral, is_pi, is_universe via CPS sig queries
- [x] Type constructors via double-wait: Nat, Bool, mkEq, mkType, mkPi, mkArrow
- [x] Verified: basic type checks (Nat t = TT, Nat FF = FF, Bool TT = TT) pass through kernel
- [x] Verified: CPS sig queries work correctly (is_neutral returns TT/FF for neutrals/non-neutrals)
- [x] **RESOLVED**: q_ton_fn — smart accum eliminates combinator explosion
- [x] **RESOLVED**: Circularity — smart accum uses the Pi checker signature via selector query, no metadata-tag trust needed
