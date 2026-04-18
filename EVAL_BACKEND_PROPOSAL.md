# Pluggable eval-backend framework for tree-native DTT

Status: **design**, 2026-04-16. Written after discovering we have three
competing NbE/context designs in flight (bind-tree, ctx-tree, semantic-domain)
with no way to test them against each other under identical judgments. This
document proposes an abstraction that lives **in the tree calculus** (not in
TypeScript) so backends are genuine object-language citizens and can be
benchmarked head-to-head on a shared test suite.

Companion to `BIND_TREE_NBE_IDEA.md`, `MERGED_CONTEXT_PROPOSAL.md`,
`ELABORATION_DESIGN.md`. Compatible with `DEVELOPMENT_PHILOSOPHY.md`: all
backends are disp programs; the host provides only parser, runtime, and
benchmark harness.

## 1. Problem

Three NbE-shaped designs are live in the repo:

| Design | Status | Known issue |
|--------|--------|-------------|
| Bind-tree NbE (H-tokens + splice) | implemented in `lib/predicates.disp` | stale `lamB` after under-binder β → `Eq Nat (succ zero) one` fails |
| Ctx-tree NbE (parallel term/ctx, Phase 5) | designed in `MERGED_CONTEXT_PROPOSAL.md` | not yet built |
| Semantic-domain NbE (Dybjer/Filinski-style, Val = Leaf\|Stem\|Fork\|Neutral) | proposal (external research note) | not yet prototyped |

We cannot tell which of these is correct without running them on the same
judgments. Worse, we cannot tell which is fast enough without running them
under the same SKI-step budget. The current code hard-codes the bind-tree
design into `predicates.disp`; swapping requires rewriting the kernel.

The fix is an interface in the object language that each design implements.
The kernel dispatches through the interface, so swapping backends is a
single `def` change, and both backends run the same judgments under the same
runtime.

## 2. The four properties every backend must satisfy

A backend is sound iff it satisfies:

- **P1 — Soundness of conversion.** `Γ ⊢ M ≡β N : A` ⇒ `be_conv(ctx, eval M, eval N) = TT`.
- **P2 — Completeness of conversion.** `be_conv(ctx, eval M, eval N) = TT` ⇒ `Γ ⊢ M ≡β N : A`.
- **P3 — Substitution stability.** `be_eval (M[N/x]) ρ  ≡  be_eval M (ρ[x ↦ be_eval N ρ])`.
- **P4 — Decidability.** `be_conv` terminates on well-typed inputs (budgeted in the runtime).

These are what the shared test suite checks. Any backend that fails a suite
entry on P1/P2 is unsound for that judgment. Any backend that blows the
budget violates P4 in practice.

## 3. Backend interface as a tagged record

A backend is a tree — a tagged record bundling the operations the generic
checker needs. Concretely, nine operations:

```
be_eval       : Term  -> Ctx -> Val                  -- evaluate syntax to value
be_quote      : Ctx   -> Val -> Term                 -- readback (reify) value to syntax
be_app        : Val   -> Val -> Val                  -- semantic application
be_conv       : Ctx   -> Val -> Val -> Bool          -- definitional equality
be_fresh      : Ctx   -> Val -> Pair Val Ctx         -- introduce fresh var of type Val
be_force_pi   : Val   -> Option (Pair Val (Val -> Val))
                                                      -- if Pi, return (dom, codom-closure)
be_force_type : Val   -> Bool                        -- is this the Type value?
be_empty_ctx  : Ctx                                  -- initial (top-level) context
be_lookup_ty  : Ctx   -> Val -> Val                  -- type of a variable-value
```

Encoded as a 9-tuple fork-chain plus a kind tag `KBE` (`stem(stem(stem(stem(stem(stem(leaf))))))`
or similar — pick an unused kind code):

```
mkBackend eval_ quote_ app_ conv_ fresh_ forcePi_ forceType_ emptyCtx_ lookupTy_ :=
  tagged KBE
    (fork
      (fork (fork eval_ quote_) (fork app_ conv_))
      (fork (fork fresh_ forcePi_) (fork (fork forceType_ emptyCtx_) lookupTy_)))

be_eval       b := (payload b).left.left.left
be_quote      b := (payload b).left.left.right
be_app        b := (payload b).left.right.left
be_conv       b := (payload b).left.right.right
be_fresh      b := (payload b).right.left.left
be_force_pi   b := (payload b).right.left.right
be_force_type b := (payload b).right.right.left.left
be_empty_ctx  b := (payload b).right.right.left.right
be_lookup_ty  b := (payload b).right.right.right
```

Exact layout TBD at implementation time; the accessors above are a worked
example. Encoding choice:
- `Val` and `Ctx` types are **internal** to each backend — the generic
  checker never destructures them, only passes them back to backend ops.
  This is exactly the TypeScript `interface<Val, Ctx>` idea but expressed
  via opaque trees.
- `Val -> Val` closures in `be_force_pi` are genuine bracket-abstracted
  functions. The backend wraps/unwraps them as it sees fit.
- `Option` is encoded as `None = leaf | Some(v) = stem(v)` (standard).
- `Pair = fork`; `Bool = leaf (TT) | stem(leaf) (FF)` as already established.

## 4. The generic checker

Lives in `lib/check_generic.disp`. Takes a backend record and a term,
returns an elaborated term. Mirror of `check` in `BIND_TREE_NBE_IDEA.md §4.4`,
but every eval/app/conv/fresh call routes through the backend:

```
check be ctx term ty :=
  triage_on_kind term
    V:    check_via_infer be ctx term ty
    H:    check_via_infer be ctx term ty
    App:  check_via_infer be ctx term ty
    Lam:
      force_pi (be_force_pi be ty)
        None:       FF                              -- not a Pi, reject
        Some piPair:
          piDom    := fst piPair
          piCodom  := snd piPair                    -- a Val -> Val closure
          lamA     := lamA term
          lamA_val := (be_eval be) lamA ctx
          -- domain agreement
          if not ((be_conv be) ctx piDom lamA_val) then FF
          else
            freshPair  := (be_fresh be) ctx piDom
            x_val      := fst freshPair
            ctx'       := snd freshPair
            codomVal   := piCodom x_val
            bodyOpen   := splice (lamBody term) (lamB term) (be_quote be ctx' x_val)
            bodyChecked := check be ctx' bodyOpen (be_quote be ctx' codomVal)
            rebuild_lam lamA (recompute_bindB bodyChecked ...) bodyChecked
    Pi:
      (be_force_type be) ty                         -- Pi : Type
    Type:
      (be_force_type be) ty                         -- Type : Type
    _:
      FF

infer be ctx term :=
  triage_on_kind term
    H:    (term, type_of_H term)
    V:    (term, (be_lookup_ty be) ctx term)
    App:
      headRes  := infer be ctx (appF term)
      headTy   := snd headRes
      headVal  := (be_eval be) (fst headRes) ctx
      piOpt    := (be_force_pi be) headTy
      piPair   := force_some piOpt
      piDom    := fst piPair
      piCodom  := snd piPair
      argCheck := check be ctx (appX term) piDom
      argVal   := (be_eval be) argCheck ctx
      resultTy := piCodom argVal
      (mkApp (fst headRes) argCheck, (be_quote be) ctx resultTy)
    ...
```

Two critical properties of this checker:

1. **It never destructures `Val` or `Ctx`.** The backend is opaque. Swapping
   `bindtree_backend` for `semantic_backend` changes nothing in this file.

2. **It routes every `normalize`/`fast_eq` through the backend.** No direct
   `fast_eq` on normalized trees; all conversion goes through `be_conv`.
   Backends that encode equivalence via hash-cons (bind-tree) implement
   `be_conv = \ctx a b. fast_eq a b`; backends that reify first (semantic-
   domain) implement `be_conv = \ctx a b. fast_eq (reify a) (reify b)`.

Top-level entry:

```
check_top be term ty := check be (be_empty_ctx be) term ty
```

## 5. Backend files

Each backend is a single `.disp` file that exports one definition named
`backend`. Layout:

```
lib/backends/
  bindtree.disp     -- current H-token + splice approach
  ctxtree.disp      -- Phase 5 parallel ctx-tree design
  semantic.disp     -- Dybjer/Filinski semantic-domain (Leaf|Stem|Fork|Neutral)
  closure.disp      -- traditional closure NbE (reference oracle)
```

### 5.1 `bindtree.disp` — baseline (what we have today)

```
; Val = Term (no separate value domain)
; Ctx = snoc-list of H-tokens

be_eval_bt  := \term ctx. normalize term ctx
be_quote_bt := \ctx val. val                        ; identity: values ARE syntax
be_app_bt   := \f x. normalize (mkApp f x) ctx_none
be_conv_bt  := \ctx a b. fast_eq a b                ; hash-cons O(1)
be_fresh_bt := \ctx ty. 
  let h := mkH ty (next_lvl ctx) in
    mkPair h (extend_ctx ctx h)
be_force_pi_bt := \val.
  if is_pi val
    then Some (mkPair (piA val) (\x. splice (piCodom val) (piB val) x))
    else None
be_force_type_bt := is_type
be_empty_ctx_bt  := leaf
be_lookup_ty_bt  := \ctx h. type_of_H h             ; H carries its type

def backend := mkBackend
  be_eval_bt be_quote_bt be_app_bt be_conv_bt
  be_fresh_bt be_force_pi_bt be_force_type_bt
  be_empty_ctx_bt be_lookup_ty_bt
```

### 5.2 `ctxtree.disp` — Phase 5 merged-context

Uses `ctx_enter_binder`, `ctx_exit_binder`, `splice_ctx` from
`MERGED_CONTEXT_PROPOSAL.md §3`. Val = term, Ctx = parallel ctx-tree.

```
; Val = Term (as bindtree backend)
; Ctx = ctx-tree structurally isomorphic to the term being checked

be_eval_ct   := \term ctx. fst (normalize_pair term ctx)
be_quote_ct  := \ctx val. val
be_app_ct    := \f x. normalize_app_pair f x
be_conv_ct   := \ctx a b.
  let na := normalize_pair a ctx in
  let nb := normalize_pair b ctx in
    and (fast_eq (fst na) (fst nb)) (fast_eq (snd na) (snd nb))
be_fresh_ct  := \ctx ty.
  let bid := next_bid ctx in
  let ctx' := ctx_extend ctx bid ty in
    mkPair (mkV bid) ctx'
; ... remaining ops
def backend := mkBackend ...
```

### 5.3 `semantic.disp` — Dybjer/Filinski semantic domain

Introduces a genuinely separate value domain as tagged trees:

```
; Val ::= VLeaf | VStem Val | VFork Val Val | VNeutral Ne
; Ne  ::= NVar BinderId | NApp Ne Val | NTriage Val Val Val Ne

KVL := stem^7 leaf                  ; VLeaf
KVS := stem^8 leaf                  ; VStem  
KVF := stem^9 leaf                  ; VFork
KVN := stem^10 leaf                 ; VNeutral

mkVLeaf          := tagged KVL leaf
mkVStem v        := tagged KVS v
mkVFork l r      := tagged KVF (fork l r)
mkVNeutral ne    := tagged KVN ne

; doApp implements the five reduction rules + three stuck points.
do_app := fix \rec f v.
  is_neutral f   => mkVNeutral (NApp (unNeutral f) v)
  is_VLeaf f     => mkVStem v
  is_VStem f     => mkVFork (unVStem f) v
  is_VFork f     => do_app3 (fork_left f) (fork_right f) v
  _              => error

do_app3 := \a y z.
  is_neutral a   => mkVNeutral (stuck_fork_case a y z)
  is_VLeaf a     => y                              ; K rule
  is_VStem a     => do_app (do_app (unVStem a) z) (do_app y z)   ; S rule
  is_VFork a     => triage_sem (fork_left a) (fork_right a) y z
  _              => error

triage_sem w x y scrut :=
  is_neutral scrut => mkVNeutral (NTriage w x y (unNeutral scrut))
  is_VLeaf scrut   => w                            ; rule 3a
  is_VStem scrut   => do_app x (unVStem scrut)     ; rule 3b
  is_VFork scrut   => do_app (do_app y (fork_left scrut)) (fork_right scrut)  ; 3c

be_eval_sem  := \term ctx. eval_to_val term ctx
be_quote_sem := \ctx val. reify_typed ty val ctx   ; type-directed readback
be_app_sem   := do_app
be_conv_sem  := \ctx a b. fast_eq (reify_typed _ a ctx) (reify_typed _ b ctx)
be_fresh_sem := \ctx ty. 
  let k := next_level ctx in
    mkPair (mkVNeutral (NVar k)) (ctx_cons ctx ty)
; ... 
def backend := mkBackend ...
```

### 5.4 `closure.disp` — reference oracle (traditional NbE)

Val = VLam(Clos) | VPi Val Clos | VType | VNeutral Ne with explicit
closures. This is the textbook design; we include it because it's
well-understood and serves as the oracle: if any other backend disagrees
with `closure.disp` on a judgment, that other backend has the bug.

Closures are `mkClos env body` — a tagged pair. Environment is a snoc-list
of `Val`s. Body is a `Term`. Closure application: extend env, `eval`.

## 6. Parser / tree runtime extensions

We need four parser additions and one runtime addition. All small and
localized.

### 6.1 `backend NAME from FILE` declaration

Load a `.disp` file, assert it exports `backend`, bind that tree to `NAME`
in the current file's scope. Parser-only; no runtime work beyond recursive
`parseProgram`.

```
backend bt from "lib/backends/bindtree.disp"
backend ct from "lib/backends/ctxtree.disp"
backend sem from "lib/backends/semantic.disp"
```

Post-parse, `bt`, `ct`, `sem` are defs pointing to the respective backend
trees.

### 6.2 `suite NAME = { ... }` declaration

A test suite is a list of `(term, type, expected_result)` triples. Syntax:

```
suite core_lambda = {
  (\(x:Nat). x)                          : (Nat -> Nat)                ok
  (\(A:Type). \(x:A). x)                 : ((A:Type) -> A -> A)        ok
  (\(x:Nat). \(y:Nat). x)                : ((x:Nat) -> (y:Nat) -> Eq Nat x y)  fail
  refl_nat_succ_zero_is_one              : (Eq Nat (succ zero) one)    ok
}
```

Each entry compiles to a triple `(elaborated_term, elaborated_ty, expected_bool)`.
The suite itself is a list-tree of these triples.

`ok` and `fail` are new keywords. The bool they encode: `ok = TT`, `fail = FF`.

### 6.3 `test_via BACKEND SUITE` declaration

Run every entry in `SUITE` through `check_top BACKEND term ty`; assert the
result matches the expected bool. Report which entries fail.

```
test_via bt  core_lambda
test_via ct  core_lambda
test_via sem core_lambda
```

Semantics: `test_via B S` desugars to a tree-program that iterates through
`S` and tests `check_top B term ty ≡ expected`. The host-side test runner
reports pass/fail counts per suite per backend.

### 6.4 `bench BACKEND SUITE` declaration

Like `test_via`, but also report SKI step count and wall-clock time per
judgment. Requires exposing the runtime's step count (see 6.5).

```
bench bt  core_lambda
bench ct  core_lambda
bench sem core_lambda
```

Output format (host side):

```
core_lambda × bt:  8/8 passed,  steps: min 124 / max 8,451 / total 32,107,  time 1.2ms
core_lambda × ct:  8/8 passed,  steps: min 156 / max 12,002 / total 45,311, time 1.8ms
core_lambda × sem: 8/8 passed,  steps: min 203 / max 19,884 / total 71,442, time 2.3ms
```

### 6.5 Runtime: expose step count

`src/tree.ts` already has `budget.remaining` (decremented on every reduction).
Expose the difference `(initial - remaining)` as the step count after a
computation. Two changes:

1. `applyBudgeted(f, x, maxSteps)` returns `{ result: Tree, steps: number }`
   instead of just `Tree`. One extra field, no algorithmic change.
2. A new tree-level primitive `count_steps : (leaf -> result)` that runs
   the thunk `(\_. expr)` applied to `leaf` under a fresh budget and pairs
   the result with a Church-encoded step count. Optional — host-side
   reporting via (1) is enough for benchmarking; the tree-level primitive
   is only needed if a disp program itself wants to benchmark.

The tree-level primitive is a **new builtin** (like `fast_eq`): it violates
"no host features without tree analogs" in form, but its semantics
(measuring reduction steps) is a pure function of the tree-calculus
execution trace, not a host feature. The tree analog is conceptually
"count fork-applications during reduction" — fully tree-level, just
needs runtime support.

### 6.6 Parser summary

File: `src/parse.ts`. Additions:

| Form | Keyword(s) | Desugars to |
|------|-----------|-------------|
| `backend NAME from "FILE"` | `backend`, `from` | `def NAME = <tree loaded from FILE's `backend` def>` |
| `suite NAME = { ENTRY* }` | `suite`, `{`, `}`, `ok`, `fail` | `def NAME = <list-tree of (term, ty, bool) triples>` |
| `test_via BE SUITE` | `test_via` | Runs `check_top BE term ty` on every entry, asserts result |
| `bench BE SUITE` | `bench` | Like `test_via`, plus host-side step/time reporting |

Total: 6 new keywords (`backend`, `from`, `suite`, `ok`, `fail`, `test_via`,
`bench`). All reserved; existing programs unaffected. Parser state: add a
"loaded backends" map so `backend X from Y` is resolved at parse time, not
runtime.

## 7. Shared test suite

The suite lives in `lib/suite/core.disp` (and later `eq.disp`,
`church.disp`, `dependent.disp`, etc.). Structure:

```
; core.disp — judgments every backend must handle

; Imports — all backends see the same surface definitions
def id   : (A : Type) -> A -> A        = \(A : Type). \(x : A). x
def const : (A : Type) -> (B : Type) -> A -> B -> A = \(A : Type). \(B : Type). \(x : A). \(y : B). x

suite core_lambda = {
  id                                      : ((A : Type) -> A -> A)         ok
  (id Nat)                                : (Nat -> Nat)                    ok
  (id Nat zero)                           : Nat                              ok
  const                                   : ((A : Type) -> (B : Type) -> A -> B -> A) ok

  ; reject the wrong type
  (\(x : Nat). x)                         : Bool                             fail
}

suite church_equality = {
  ; under-binder β reduction required to succeed
  refl_one_is_succ_zero                   : (Eq Nat (succ zero) one)        ok
  ; different Church numerals
  church_two                              : Nat                              ok
  ; symbolic refl
  (\(P : Nat -> Type). \(p : P zero). p)  : (Eq Nat zero zero)              ok
}

suite dependent = {
  vec_zero                                : (Vec Nat zero)                  ok
  (cons zero vec_zero)                    : (Vec Nat (succ zero))           ok
  ...
}

backend bt  from "lib/backends/bindtree.disp"
backend ct  from "lib/backends/ctxtree.disp"
backend sem from "lib/backends/semantic.disp"
backend cl  from "lib/backends/closure.disp"

test_via bt  core_lambda
test_via ct  core_lambda
test_via sem core_lambda
test_via cl  core_lambda

test_via bt  church_equality     ; expected to fail on refl_one_is_succ_zero today
test_via ct  church_equality     ; expected to pass once Phase 5 lands
test_via sem church_equality     ; expected to pass
test_via cl  church_equality     ; oracle — must pass

bench all_backends core_lambda
bench all_backends church_equality
```

## 8. Benchmarking methodology

For each `(backend, suite_entry)` pair, record:

1. **Result correctness**: `check_top be term ty` produces the expected
   accept/reject. Mismatch = bug in the backend (relative to the oracle).
2. **SKI step count**: `budget.remaining` delta from `apply` invocation.
   Fine-grained cost of each judgment in tree-calculus reductions.
3. **Wall-clock time**: host-side `performance.now()` around the
   `applyBudgeted` call. Reveals where constant factors dominate.
4. **Hash-cons growth**: new tree IDs allocated during the check. Measures
   "scratch work" vs. sharing.

Report in markdown tables, one per suite. Backend rows, metric columns.
Auto-generated after `npm run bench`.

## 9. Philosophy compliance

This proposal adheres to `DEVELOPMENT_PHILOSOPHY.md`:

- **All backends are object-language citizens.** No TypeScript backends.
  The generic checker is a tree program; the backend record is a tree;
  every operation is bracket-abstracted.
- **Host only does parser, runtime, and measurement.** Parser is meta-level
  (surface syntax, not part of the trusted base). Runtime is the apply loop.
  Measurement is test harness infrastructure — meta-level by the philosophy's
  own classification.
- **Cross-validation is built in.** The `closure` backend is the oracle;
  any disagreement with it is a bug. This is exactly the "continuously
  cross-validate" rule from §4.
- **No host-only optimizations.** Step counting is added as a first-class
  runtime-visible quantity, not a hidden host metric. Bench results are
  data, not machinery inside the kernel.

The only delicate spot is the `count_steps` builtin (6.5). It is exposed
as a tree primitive but its implementation is in the host. This is the
same status as `fast_eq` — a primitive whose semantics is a pure function
of tree-calculus execution but whose efficient implementation uses host
state. Acceptable per the existing precedent.

## 10. Implementation phases

**Phase A: parser plumbing (no semantics yet).**
- Add `backend`, `from`, `suite`, `ok`, `fail`, `test_via`, `bench` keywords.
- Parse `backend NAME from FILE` as a file-inclusion + binding.
- Parse `suite NAME = { ... }` as a def to a list-tree.
- Parse `test_via BE SUITE` and `bench BE SUITE` as test-harness declarations.
- Runtime: extend `applyBudgeted` to return `{ result, steps }`.
- No backend files exist yet; `test_via` invocations fail with
  "backend undefined" — that's fine.

**Phase B: bindtree backend wrapped.**
- Extract the current `pred_of_lvl` code path from `predicates.disp` into
  `lib/backends/bindtree.disp` as a backend record.
- Port the existing 111 tests to the new `suite` syntax.
- Run `test_via bt existing_tests`; assert 100% parity with current
  direct-call tests.

**Phase C: closure backend (oracle).**
- Write `lib/backends/closure.disp` — traditional NbE with explicit
  closure trees. Smallest of the three new backends.
- `test_via cl existing_tests` — assert same pass/fail as `bt`.
- First cross-validation data point.

**Phase D: semantic backend.**
- Write `lib/backends/semantic.disp` — Val = {VLeaf, VStem, VFork,
  VNeutral}, with `do_app`/`do_app3`/`triage_sem` implementing the five
  tree-calculus rules + three stuck points.
- `test_via sem existing_tests`; compare with `bt` and `cl`.

**Phase E: ctxtree backend.**
- Write `lib/backends/ctxtree.disp` — port `ctx_sketch.ts` (the
  existing whiteboard) into disp, wrap as a backend record.
- Add the hard suite: `church_equality` including
  `Eq Nat (succ zero) one`.
- `test_via bt church_equality` — expected to fail (stale bind-tree).
- `test_via ct church_equality` — must pass, or the Phase 5 design is
  wrong and needs revision.
- `test_via sem church_equality` — must pass (Dybjer/Filinski is known
  correct).
- `test_via cl church_equality` — must pass (oracle).

**Phase F: benchmarking.**
- Wire `bench` declarations to emit markdown tables.
- Run all four backends on all suites.
- Decide which backend becomes canonical `pred_of` based on correctness
  + performance data.

Phases A-B are infrastructure (no semantic risk). Phase C provides the
oracle. Phases D-E are where the real work + research happens. Phase F
is the payoff: empirical answer to "which NbE design wins."

## 11. What this framework does NOT decide

- **Surface elaboration** (`\(x : T). body`, holes, implicits) — unchanged.
  Those live in `elaborate.ts` / `elab.disp` and produce terms that get
  fed to any backend.
- **Erasure to runtime SKI** (`erase`) — orthogonal. Runs post-check on
  the elaborated term regardless of backend.
- **Meta-variable handling** — each backend can implement metas
  internally. The `be_fresh` op is how metas are introduced as
  opaque-to-caller values.
- **Which backend is the "right" one long-term.** This framework is
  measurement apparatus. The measurement determines the answer; the
  framework just makes the measurement possible.

## 12. Success criteria

1. All existing tests still pass via `test_via bt existing_tests`.
2. `closure` backend agrees with `bindtree` on all judgments not affected
   by the stale-bind-tree bug. (Agreement on `core_lambda` mandatory.)
3. At least one of `ctxtree` / `semantic` passes `Eq Nat (succ zero) one`,
   proving the stale-bind-tree bug is resolvable by a different backend
   choice.
4. `bench` output is reproducible: identical step counts across runs for
   the same backend on the same suite.
5. Swapping backends in a user program is a single-line change
   (`test_via bt X` → `test_via sem X`).

Once all five hold, the project has a tool for making the NbE/context
design debate empirical rather than speculative — which is the whole
point.
