# Bind-Tree NbE: A One-Universe Elaboration with Canonical Hypothesis Tokens

**Status (2026-04-13)**: implemented as the substrate. See `ELABORATION_DESIGN.md` "Current state" for the inventory of what's working and "Plan" for what comes next. Two intentional departures from this doc:

- **`H` wraps its type, not its binder** (vs §3.4). `mkH ty := tagged(KH, ty)`, `type_of_H := payload`. App-of-H typing needs the type directly; the binder-wrapping form forced a four-level dig and didn't compose with `infer`.
- **Bind-tree fork-shapes use distinct kind tags**, not the kind trees in §3.3 directly. `BApp(l,r) = fork(KBA, fork(l,r))`; same for BLam/BPi. Lets `triage` dispatch on the fork case via a single `fast_eq` on the kind slot.

**Companion docs**: `TREE_NATIVE_TYPE_THEORY.md` (types as predicates, Pi as partial application), `ELABORATION_DESIGN.md` (current state and 3-phase plan), `DEVELOPMENT_PHILOSOPHY.md` (the metacircular discipline this design respects).

## 1. Why

The de-Bruijn-level design has a load-bearing bug: a stored Pi or Lam carries its binder at a fixed level, but is later re-encountered at a different context depth, and substitution at `current_level` no longer matches the stored binder's actual level. This manifests as silent mis-specialization of dependent codomains (see critique in earlier review).

The conventional fix is NbE with semantic closures: functions become `(env, body)` pairs in a separate domain, β becomes closure-application, and reification walks values back to syntax. This costs a second universe outside the tree calculus — a target for metacircular creep.

This proposal keeps everything in one tree universe. Binders carry a **bind-tree** that structurally identifies where their bound variable occurs in their body. β-reduction is a structural splice directed by the bind-tree. Hypotheses under binders are represented by **canonical hypothesis tokens** derived deterministically from the binder's own tree, so α-equivalent binders produce identical tokens and hash-cons identity carries the conversion relation.

## 2. Core idea, from first principles

### 2.1 Binders without names or numbers

A binder needs to answer one question: given its body, where does the bound variable occur? Conventional answers: tag every variable use with a name (fragile under α), or tag every variable use with a de Bruijn index/level (fragile under re-encounter at different depth).

Third answer: **describe the positions structurally at the binder itself**. A Lam is `Lam(A, B, body)` where `B` is a tree whose shape mirrors `body`, with a leaf `E` at every position where the variable occurs, and a leaf `N` at every subtree where the variable does not occur. No names, no indices.

Variable uses inside `body` are just `V` — an interchangeable placeholder. Which `V` belongs to which enclosing binder is determined by which binder's `B` has an `E` reaching down to that `V`.

### 2.2 Hypothesis introduction = splice with a token

To check a Lam against a Pi, we need to reason about what happens "if a value of type `A` is provided." Conventional: introduce a fresh variable, extend a context. Here: introduce a fresh **token** `H`, and splice `H` in for the `V`s that the bind-tree marks.

`splice(body, B, H)` walks `body` and `B` in parallel. At `E`, substitute `H`. At `N`, leave the subtree. At structural nodes, recurse into children. The resulting body now has `H` wherever the bound variable was. Compare structurally against the Pi's codomain, similarly spliced with the same `H`.

The token `H` is an *inert* tree. When normalization meets `App(H, x)`, no Lam is present on the left, so the application is stuck. This is the neutral behavior of NbE, realized as a tree rather than a semantic value.

### 2.3 Canonical tokens: `H(binder) := fork(HYP_MARKER, binder)`

The token for a binder's hypothesis is a function of the binder itself. Concretely, it is a distinguished tree shape wrapping the binder:

```
H(binder) := tagged(KIND_HYP, binder)
```

Properties:

- **Deterministic.** No global counter. The same binder always produces the same token.
- **α-canonical.** Two α-equivalent binders are the same tree (bind-trees eliminate the distinction), so they produce the same token. Hash-cons identity works across unrelated checks.
- **Self-describing.** The token's type is `binder.A` — extract the payload, read the domain type. No ambient context lookup is needed.
- **Scope-distinct.** Different binders produce different tokens; no accidental aliasing across scopes.
- **No cycle.** The binder contains `V`s, not tokens; the token wraps the binder. `fork(HYP_MARKER, Lam(A, B, body))` is a finite tree.

### 2.4 Reify = unsplice

After checking under a binder, we may have a body containing `H`. To produce the final elaborated form we walk the body, replace every `H` occurrence with `V`, and produce a bind-tree that marks those positions. This is the inverse of splice; call it `unsplice`. It is a structural walk.

### 2.5 Why this works

Conversion of dependent types is decided by `fastEq` on normal forms. Normal forms here are trees with `H`-tokens at stuck positions. Two dependent types that should be equal up to conversion are reduced (splice + primitives) until their stuck forms share the same `H`-tokens at the same structural positions. Because tokens are canonical, this coincidence is not a coincidence: it is forced by the shared binder structure.

The level-drift bug does not arise because there are no levels. Re-encountering a stored Lam at a different "depth" changes nothing — its bind-tree is intrinsic, the hypothesis token it produces is intrinsic, and splice operates only on the binder's own tree.

## 3. Data structures

### 3.1 Tag encoding

A tagged form is `fork(fork(TAG_ROOT, kind), payload)` where `TAG_ROOT` is a fixed distinguished tree and `kind` is a per-constructor distinguishing tree. Reserved shapes:

```
TAG_ROOT     := stem(stem(stem(stem(stem(leaf)))))

tagged(k, p) := fork(fork(TAG_ROOT, k), p)
kind(t)      := t.left.right          -- when t is tagged
payload(t)   := t.right
is_tagged(t) := isFork(t) && isFork(t.left) && t.left.left.id == TAG_ROOT.id
```

### 3.2 Term kinds

| Constructor | Kind tree | Encoding |
|---|---|---|
| `V` | `KV := leaf` | `tagged(KV, leaf)` |
| `H(b)` | `KH := stem(leaf)` | `tagged(KH, b)` — b is the binder tree |
| `App(f, x)` | `KA := stem(stem(leaf))` | `tagged(KA, fork(f, x))` |
| `Lam(A, Bi, body)` | `KL := stem(stem(stem(leaf)))` | `tagged(KL, fork(fork(A, Bi), body))` |
| `Pi(A, Bi, codom)` | `KP := stem(stem(stem(stem(leaf))))` | `tagged(KP, fork(fork(A, Bi), codom))` |
| `Type` | `KT := fork(leaf, leaf)` | `tagged(KT, leaf)` |
| `Base(v)` | `KB := fork(leaf, stem(leaf))` | `tagged(KB, v)` — v is a runtime tree |

### 3.3 Binding kinds

A bind-tree annotates a term by mirroring its structure. Leaves are `E` (here) or `N` (absent). Compound positions use shape-specific constructors so the walker can advance into the term in lockstep.

| Binding | Kind tree | Meaning |
|---|---|---|
| `E` | `BE := fork(leaf, stem(stem(leaf)))` | the term at this position is `V`, replace it |
| `N` | `BN := fork(stem(leaf), leaf)` | the binder's variable does not appear anywhere in this subtree |
| `BApp(bl, br)` | `BA := fork(stem(leaf), stem(leaf))` | term is `App(f, x)`; recurse bl into f, br into x |
| `BLam(bA, bBody)` | `BL := fork(stem(stem(leaf)), leaf)` | term is `Lam(A, _, body)`; recurse bA into A, bBody into body |
| `BPi(bA, bCodom)` | `BP := fork(stem(stem(leaf)), stem(leaf))` | term is `Pi(A, _, codom)`; recurse bA into A, bCodom into codom |

Note: the inner `Bi` slot of a nested Lam/Pi is itself a bind-tree (metadata), not a value-bearing position. Outer bind-trees never recurse into it.

### 3.4 Hypothesis token

```
HYP_MARKER := KH                      -- reuse the KH kind tree as the marker
H(binder)  := tagged(KH, binder)      -- so H(binder) is a term in kind KH

type_of_H(h) := payload(payload(h)).left.left      -- walk: h.payload = binder; binder.payload.left.left = A
```

Unfolding `type_of_H`: `h` has kind `KH` and payload `binder`. `binder` has kind `KL` or `KP` and payload `fork(fork(A, Bi), body-or-codom)`. So `A = h.payload.payload.left.left`.

(For clarity the pseudocode below uses named accessors like `binder.A` rather than these raw tree walks.)

### 3.5 Context

A context is a snoc-list of tokens that are *currently live* (introduced by an enclosing binder not yet reified out).

```
ctx := leaf | fork(ctx_prev, H_token)

is_live(ctx, h) :=
  if ctx == leaf: false
  if fastEq(ctx.right, h): true
  is_live(ctx.left, h)

extend(ctx, h) := fork(ctx, h)
```

`is_live` is only needed for sanity-checking that tokens in scope are recognized; type lookup does not go through ctx (tokens are self-describing).

## 4. Operations

Each operation is given in pseudocode corresponding directly to a tree-level program. The TypeScript mirror is a host optimization of the same algorithm.

### 4.1 `splice(term, binding, v)` — fill `V`-holes with `v` directed by `binding`

```
splice(t, b, v) :=
  case kind(b):
    BE:      v
    BN:      t
    BA(bl, br):
      -- t must be App
      App(splice(t.f, bl, v), splice(t.x, br, v))
    BL(bA, bBody):
      -- t must be Lam
      Lam(splice(t.A, bA, v), t.Bi, splice(t.body, bBody, v))
    BP(bA, bCodom):
      -- t must be Pi
      Pi(splice(t.A, bA, v), t.Bi, splice(t.codom, bCodom, v))
```

Termination: `b` strictly decreases in every recursive case. O(size(b)).

### 4.2 `unsplice(term, h)` — extract all occurrences of `h` as `V`, produce a binding

Returns a pair `(binding, term_without_h)`. Uses `N` compactly when no occurrence was found in a subtree.

```
unsplice(t, h) :=
  if fastEq(t, h): return (BE, V)
  case kind(t):
    KV:    (BN, V)
    KH:    (BN, t)                    -- different token, leave alone
    KT:    (BN, t)
    KB:    (BN, t)
    KA:
      (bf, f') = unsplice(t.f, h)
      (bx, x') = unsplice(t.x, h)
      if kind(bf) == BN and kind(bx) == BN: (BN, t)
      else: (BA(bf, bx), App(f', x'))
    KL:
      (bA, A') = unsplice(t.A, h)
      (bBody, body') = unsplice(t.body, h)
      if kind(bA) == BN and kind(bBody) == BN: (BN, t)
      else: (BL(bA, bBody), Lam(A', t.Bi, body'))
    KP:
      (bA, A') = unsplice(t.A, h)
      (bCod, cod') = unsplice(t.codom, h)
      if kind(bA) == BN and kind(bCod) == BN: (BN, t)
      else: (BP(bA, bCod), Pi(A', t.Bi, cod'))
```

Termination: `t` strictly decreases. O(size(t)).

### 4.3 `normalize(term, ctx)` — β-reduce to head normal form, reducing under binders

```
normalize(t, ctx) :=
  case kind(t):
    KV:    t                              -- bare V never appears post-splice, but leave pure
    KH:    t                              -- stuck, token is its own normal form
    KT:    t
    KB:    t
    KA:
      f = normalize(t.f, ctx)
      x = normalize(t.x, ctx)
      reduce_app(f, x, ctx)
    KL:
      A' = normalize(t.A, ctx)
      -- walk under the binder: create token, splice, normalize, unsplice back
      binder = Lam(A', t.Bi, t.body)
      h = H(binder)
      open = splice(t.body, t.Bi, h)
      open_norm = normalize(open, extend(ctx, h))
      (Bi_new, body_new) = unsplice(open_norm, h)
      Lam(A', Bi_new, body_new)
    KP:
      A' = normalize(t.A, ctx)
      binder = Pi(A', t.Bi, t.codom)
      h = H(binder)
      open = splice(t.codom, t.Bi, h)
      open_norm = normalize(open, extend(ctx, h))
      (Bi_new, cod_new) = unsplice(open_norm, h)
      Pi(A', Bi_new, cod_new)

reduce_app(f, x, ctx) :=
  case kind(f):
    KL:    normalize(splice(f.body, f.Bi, x), ctx)       -- β
    primitive-match: primitive_reduce(f, x, ctx)
    otherwise: App(f, x)                                 -- stuck
```

Termination: standard — terminates on well-typed terms; unbounded in general, budgeted in the runtime.

### 4.4 `check(ctx, t, T)` and `infer(ctx, t)`

`check` returns the elaborated term (annotations filled in, metas expanded). `infer` returns `(term, type)`.

```
check(ctx, t, T) :=
  case (kind(t), kind(normalize(T, ctx))):
    (KL, KP):
      lam = t
      pi  = normalize(T, ctx)
      assert fastEq(normalize(lam.A, ctx), normalize(pi.A, ctx))
      A'  = normalize(pi.A, ctx)
      binder = Pi(A', pi.Bi, pi.codom)
      h = H(binder)
      body_open  = splice(lam.body, lam.Bi, h)
      codom_open = splice(pi.codom, pi.Bi, h)
      body_checked = check(extend(ctx, h), body_open, codom_open)
      (Bi_new, body_new) = unsplice(body_checked, h)
      Lam(A', Bi_new, body_new)
    _:
      (t', T') = infer(ctx, t)
      assert fastEq(normalize(T, ctx), normalize(T', ctx))
      t'

infer(ctx, t) :=
  case kind(t):
    KV:    error "bare V at inference — should have been spliced"
    KH:    (t, type_of_H(t))
    KT:    (t, Type)                                      -- Type : Type
    KB:    (t, predicate_type_of(t))                      -- deferred; depends on base form
    KA:
      (f', ft) = infer(ctx, t.f)
      ft_norm  = normalize(ft, ctx)
      assert kind(ft_norm) == KP, "applied non-function"
      x' = check(ctx, t.x, ft_norm.A)
      result = normalize(splice(ft_norm.codom, ft_norm.Bi, x'), ctx)
      (App(f', x'), result)
    KP:
      A' = check(ctx, t.A, Type)
      binder = Pi(A', t.Bi, t.codom)
      h = H(binder)
      codom_open = splice(t.codom, t.Bi, h)
      codom_checked = check(extend(ctx, h), codom_open, Type)
      (Bi_new, cod_new) = unsplice(codom_checked, h)
      (Pi(A', Bi_new, cod_new), Type)
    KL:
      -- infer-mode Lam: synthesize a Pi with meta A and meta codom
      -- deferred to the meta design; pseudocode omitted
      ...
```

All data structures and operations above are tree programs. `fastEq` is the existing O(1) hash-cons identity check. `normalize` uses the same budget mechanism as the runtime `apply`.

## 5. Worked examples

In examples we abbreviate tagged forms with constructor names. A step-by-step fastEq/splice/unsplice trace is given for each. Every call is reduced to its constituent trees; no step is elided.

### 5.1 Example A — identity on `Nat`

**Surface**: `id : Nat → Nat = λx. x`.

**Elaborated term**: `t_id = Lam(Nat, BE, V)`. The body is `V`; the bind-tree is `BE`, meaning "the body at this position is the bound variable."

**Expected type**: `T_id = Pi(Nat, BN, Nat)`. Non-dependent: the bind-tree is `BN` (codomain has no occurrence of the bound variable).

**Invocation**: `check(leaf, t_id, T_id)`.

**Step 1**: dispatch on `(kind(t_id), kind(normalize(T_id, leaf)))` = `(KL, KP)`. Both `normalize(t_id.A, leaf)` and `normalize(T_id.A, leaf)` equal `Nat` (`Nat` is a primitive, normalization is identity). `fastEq(Nat, Nat)` = true.

**Step 2**: construct binder and token.
- `A' = Nat`.
- `binder = Pi(Nat, BN, Nat)` — syntactically identical to `T_id`, so by hash-consing `binder.id == T_id.id`.
- `h = H(binder) = tagged(KH, binder)`.

**Step 3**: splice.
- `body_open = splice(V, BE, h)`. Case `BE`: return `h`. Result: `h`.
- `codom_open = splice(Nat, BN, h)`. Case `BN`: return `Nat`. Result: `Nat`.

**Step 4**: recursive `check(ctx = fork(leaf, h), h, Nat)`.
- `normalize(Nat, ctx) = Nat`. `kind(Nat) = KT_primitive` — not `KP`, so no Lam-vs-Pi special case.
- Fall through to `infer(ctx, h)`.
  - `kind(h) = KH`. Return `(h, type_of_H(h))`.
  - `type_of_H(h) = h.payload.A = binder.A = Nat`.
  - Return `(h, Nat)`.
- `fastEq(normalize(Nat, ctx), normalize(Nat, ctx)) = fastEq(Nat, Nat) = true`.
- Return `h`.

**Step 5**: back in Step 2 after recursive call, `body_checked = h`. Unsplice.
- `unsplice(h, h)`: `fastEq(h, h) = true`, return `(BE, V)`.
- `Bi_new = BE`, `body_new = V`.

**Step 6**: return `Lam(Nat, BE, V)` — identical to input (hash-cons match).

### 5.2 Example B — dependent identity

**Surface**: `id_dep : (A : Type) → A → A = λA. λx. x`.

**Elaborated type**:

```
T = Pi(Type, BP(BE, BE), Pi(V, BN, V))
```

The outer Pi binds `A`. Its codomain is `Pi(V, BN, V)` — the inner Pi. The outer binder `A` appears at the inner Pi's A-position (the first `V`) and at the inner Pi's codom-position (the second `V`). The outer bind-tree is therefore `BP(BE, BE)`: navigate into the inner Pi; both children are `BE` (bound variable here).

The inner Pi binds the value `x`. Its bind-tree is `BN`: `x` does not occur in `V` (which is `A`, not `x`).

**Elaborated term**:

```
t = Lam(Type, BL(BE, BN), Lam(V, BE, V))
```

Outer Lam binds `A`. Its body is `Lam(V, BE, V)` — inner Lam. Outer `A` appears at inner Lam's A-position (the first `V`) but not in its body (body is `V` = inner `x`). Outer bind-tree: `BL(BE, BN)`.

Inner Lam: body is `V` = inner binder. Bind-tree: `BE`.

**Invocation**: `check(leaf, t, T)`.

**Step 1**: `(KL, KP)`. `fastEq(normalize(Type, leaf), normalize(Type, leaf)) = true`.

**Step 2**: binder and token.
- `A' = Type`.
- `binder_outer = Pi(Type, BP(BE, BE), Pi(V, BN, V))` — equal to `T`.
- `h_A = H(binder_outer) = tagged(KH, binder_outer)`.

**Step 3**: splice.
- `body_open = splice(Lam(V, BE, V), BL(BE, BN), h_A)`.
  - Case `BL(bA, bBody)` with `bA = BE`, `bBody = BN`. Term is Lam.
  - Recurse: `splice(V, BE, h_A) = h_A`. `splice(V, BN, h_A) = V`.
  - Rebuild: `Lam(h_A, BE, V)`.
  - Note: inner `Bi` (here `BE`) is preserved from the input Lam, not touched by the splice.
- `codom_open = splice(Pi(V, BN, V), BP(BE, BE), h_A)`.
  - Case `BP(bA, bCod)` with `bA = BE`, `bCod = BE`. Term is Pi.
  - Recurse: `splice(V, BE, h_A) = h_A`. `splice(V, BE, h_A) = h_A`.
  - Rebuild: `Pi(h_A, BN, h_A)`.

**Step 4**: recurse `check(fork(leaf, h_A), Lam(h_A, BE, V), Pi(h_A, BN, h_A))`.

  **4a**: `(KL, KP)`. `fastEq(normalize(h_A, ctx), normalize(h_A, ctx)) = fastEq(h_A, h_A) = true` (tokens are their own normal form; hash-cons identity).

  **4b**: binder and token.
  - `A' = h_A`.
  - `binder_inner = Pi(h_A, BN, h_A)`.
  - `h_x = H(binder_inner)`.

  **4c**: splice.
  - `body_open_2 = splice(V, BE, h_x) = h_x`.
  - `codom_open_2 = splice(h_A, BN, h_x)`. Case `BN`: return `h_A`. Result: `h_A`.

  **4d**: recurse `check(fork(fork(leaf, h_A), h_x), h_x, h_A)`.
  - `normalize(h_A, ctx) = h_A`. `kind(h_A) = KH`. Not Pi.
  - Fall through: `infer(ctx, h_x) = (h_x, type_of_H(h_x)) = (h_x, binder_inner.A) = (h_x, h_A)`.
  - `fastEq(h_A, h_A) = true`.
  - Return `h_x`.

  **4e**: unsplice `h_x` from `h_x`. `fastEq(h_x, h_x) = true` → `(BE, V)`.
  - `body_new = V`, `Bi_new = BE`.
  - Return `Lam(h_A, BE, V)`.

**Step 5**: back in Step 3 we have `body_checked = Lam(h_A, BE, V)`. Unsplice `h_A`.
- `unsplice(Lam(h_A, BE, V), h_A)`:
  - Not `fastEq` to `h_A` directly (it's a Lam).
  - Case `KL`: recurse into A and body.
    - `unsplice(h_A, h_A)`: `(BE, V)`.
    - `unsplice(V, h_A)`: `kind(V) = KV`, not matching `h_A`, return `(BN, V)`.
  - `bA = BE` (not `BN`) so the result is `(BL(BE, BN), Lam(V, BE, V))`.
- `Bi_new_outer = BL(BE, BN)`. `body_new_outer = Lam(V, BE, V)`.

**Step 6**: return `Lam(Type, BL(BE, BN), Lam(V, BE, V))` — identical to input.

Notice what happened: the same token `h_A` was spliced into both the Lam's domain annotation and the outer Pi's two V-positions. `fastEq` succeeded at all comparison points because the canonical token was structurally identical on both sides. The value/type distinction dissolved — `h_A` served as both a type (annotating `h_x`) and a value (the thing being returned as the function's output).

### 5.3 Example C — dependent application with conversion

**Setup**: declarations.
- `plus : Pi(Nat, BN, Pi(Nat, BN, Nat))` (primitive, reduces on first arg).
- `Vec : Pi(Nat, BN, Type)` (primitive type constructor).
- `f : Pi(Nat, BA(BN, BE), App(Vec, V))` — a function from `n:Nat` to `Vec n`.

The bind-tree for `f`'s type: its codomain is `App(Vec, V)`, and the bound variable `n` occupies the right child of the App. So the Pi's bind-tree is `BA(BN, BE)` (left is `Vec`, not the binder; right is the binder).

**Query**: given some function body `e : Pi(Nat, BA(BN, BE), App(Vec, V))` (assumed already checked to be `f`), infer the type of `App(f, App(App(plus, Zero), Succ(Succ(Zero))))`.

**Invocation**: `infer(ctx, App(f, App(App(plus, Zero), Succ(Succ(Zero)))))`.

**Step 1**: recurse into `infer(ctx, f) = (f, Pi(Nat, BA(BN, BE), App(Vec, V)))`.

**Step 2**: `ft_norm = normalize(...) = Pi(Nat, BA(BN, BE), App(Vec, V))` (already normal; primitives are their own normal form, Pi under normalize enters binders but nothing reduces).

Actually let me trace the Pi-normalize to show the splice-roundtrip:
- Enter binder: `binder' = Pi(Nat, BA(BN, BE), App(Vec, V))`. `h = H(binder')`.
- `open = splice(App(Vec, V), BA(BN, BE), h) = App(splice(Vec, BN, h), splice(V, BE, h)) = App(Vec, h)`.
- `open_norm = normalize(App(Vec, h), extend(ctx, h))`.
  - `normalize(Vec, _) = Vec` (primitive).
  - `normalize(h, _) = h` (token).
  - `reduce_app(Vec, h, _)`: Vec is a primitive type constructor; not a Lam; no primitive rule fires on tokens. Stuck: `App(Vec, h)`.
- `unsplice(App(Vec, h), h)`:
  - Not `fastEq` directly.
  - Case `KA`: recurse. `unsplice(Vec, h) = (BN, Vec)`. `unsplice(h, h) = (BE, V)`.
  - `bf = BN`, `bx = BE`. Not both `BN`. Result: `(BA(BN, BE), App(Vec, V))`.
- Rebuild: `Pi(Nat, BA(BN, BE), App(Vec, V))` — same as input (roundtrip).

So `ft_norm` is unchanged.

**Step 3**: `kind(ft_norm) = KP`. `A = Nat`, `Bi = BA(BN, BE)`, `codom = App(Vec, V)`.

**Step 4**: `check(ctx, App(App(plus, Zero), Succ(Succ(Zero))), Nat)`.

Nested `infer`:
- `infer(App(App(plus, Zero), Succ(Succ(Zero))))`.
  - `infer(App(plus, Zero))`.
    - `infer(plus) = (plus, Pi(Nat, BN, Pi(Nat, BN, Nat)))`.
    - `ft_norm = Pi(Nat, BN, Pi(Nat, BN, Nat))`.
    - `check(Zero, Nat) ✓`.
    - `result = normalize(splice(Pi(Nat, BN, Nat), BN, Zero), ctx)`.
      - `splice(..., BN, _) = Pi(Nat, BN, Nat)` (case BN returns the term).
      - `normalize(Pi(Nat, BN, Nat), ctx)`: roundtrip as before, unchanged.
    - Return `(App(plus, Zero), Pi(Nat, BN, Nat))`.
  - `ft_norm = Pi(Nat, BN, Nat)`.
  - `check(Succ(Succ(Zero)), Nat) ✓`.
  - `result = normalize(splice(Nat, BN, Succ(Succ(Zero))), ctx) = Nat`.
  - Return `(App(App(plus, Zero), Succ(Succ(Zero))), Nat)`.
- Expected `Nat`, inferred `Nat`. fastEq ✓.
- Return `App(App(plus, Zero), Succ(Succ(Zero)))`.

**Step 5**: back in the outer infer. `x' = App(App(plus, Zero), Succ(Succ(Zero)))`.

`result = normalize(splice(App(Vec, V), BA(BN, BE), x'), ctx)`.

- Splice: `splice(App(Vec, V), BA(BN, BE), x')`. Case `BA(BN, BE)`:
  - `splice(Vec, BN, x') = Vec`.
  - `splice(V, BE, x') = x'`.
  - Rebuild: `App(Vec, x') = App(Vec, App(App(plus, Zero), Succ(Succ(Zero))))`.

- Normalize the spliced result:
  - Normalize `App(Vec, App(App(plus, Zero), Succ(Succ(Zero))))`.
  - Recursively normalize arg: `App(App(plus, Zero), Succ(Succ(Zero)))`.
    - Normalize `App(plus, Zero)`: `plus` primitive, `Zero` primitive. `reduce_app(plus, Zero, ctx)`: not a Lam; `plus` has a primitive rule `plus Zero m → m` but is **partially applied** here. The rule needs two arguments. At this level it stays as `App(plus, Zero)`.
    - Normalize `Succ(Succ(Zero))`: primitive, unchanged.
    - `reduce_app(App(plus, Zero), Succ(Succ(Zero)), ctx)`: now two args visible to `plus`. Primitive rule: since first arg is `Zero` (concrete leaf), fire `plus Zero m → m`. Result: `Succ(Succ(Zero))`.
  - Continue normalize `App(Vec, Succ(Succ(Zero)))`: `Vec` primitive, arg concrete. `reduce_app(Vec, Succ(Succ(Zero)), ctx)`: Vec has no reduction rule; stays stuck as `App(Vec, Succ(Succ(Zero)))`.
  - Result: `App(Vec, Succ(Succ(Zero)))`.

**Step 6**: return `(App(f, App(App(plus, Zero), Succ(Succ(Zero)))), App(Vec, Succ(Succ(Zero))))`.

The inferred type is `Vec 2` — the function's dependent codomain got specialized to the actual argument's normal form. Every step was a splice, a primitive reduction, or a `fastEq` check.

If this result were later compared against an expected `App(Vec, Succ(Succ(Zero)))` from elsewhere, `fastEq` would succeed directly on hash-cons identity (both trees are built by the same constructors on the same primitives, so they share ids).

## 6. Implications of the canonical token choice

Choosing `H(binder) := tagged(KH, binder)` has concrete consequences.

**6.1 No global state in the checker.** There is no meta-counter for token ids. Every step of checking is a pure function of its inputs. This is load-bearing for the metacircular goal — the tree-program version of the checker does not need to thread a counter state, because the token is a structural function of the binder it came from.

**6.2 α-equivalence is realized by hash-cons.** Two α-equivalent binders are literally the same hash-consed tree (bind-trees erase the distinction, since there are no names or indices). Therefore their canonical tokens are the same tree. Therefore the bodies checked under them, once reduced, live in the same "token-land" and `fastEq` compares them directly. No quotation-up-to-renaming step is ever required.

**6.3 Tokens of nested binders carry the nesting.** When checking a body under outer binder `B_o` with token `h_o`, entering an inner binder `B_i` produces a token `h_i = tagged(KH, B_i)`. If `B_i` was constructed after splicing `h_o` into some enclosing Pi, then `B_i` contains `h_o` in its structure; so `h_i` transitively contains `h_o`. This means `h_i`'s tree identity distinguishes inner binders that share syntactic shape but appear under different outer hypotheses — the scope information is folded into the token itself.

Concretely: `λA. λx. x` at top level produces one outer token `h_A`. Inside, we enter the inner binder, producing `h_x = tagged(KH, Pi(h_A, BN, h_A))`. The outer `h_A` is embedded in `h_x`. If the same `λA. λx. x` appears inside another scope (e.g., under a different outer binder from some enclosing check), the embedded `h_A` differs, and so does `h_x`. Shadowing and scope-separation are structural.

**6.4 Hash-cons amplification is real.** Once `H(Pi(Nat, BN, Nat))` is constructed during any check of an identity-on-Nat, every subsequent such check reuses the same token tree by hash-cons lookup. Bodies post-check also share: `Lam(Nat, BE, V)` as an elaborated identity is a canonical tree. A large program with many uses of the same function type sees massive structural sharing at the checker level, not just at the value level.

**6.5 Reify is non-trivial but local.** `unsplice` is O(size(term)) — a linear walk. Each check call performs one splice-in and one unsplice-out per binder entered, so total work is proportional to the sum of body sizes times nesting depth. No worse than level-based substitution.

**6.6 Scope check is optional but cheap.** A well-formed elaboration run will never leak a token outside the scope that introduced it (every introduction is balanced by an unsplice). If paranoid, `is_live(ctx, h)` verifies on every `KH` lookup that `h` is currently in scope. This is a structural walk of ctx, and can be omitted in the trusted kernel after the elaborator has been validated.

**6.7 Self-describing tokens simplify the predicate view.** At runtime (post-erase), hypothesis tokens cease to exist (they are elaboration-only). But during elaboration, a token behaves exactly like a neutral: stuck under `apply`, comparable by `fastEq`, carrying its type transparently. The runtime `apply(T, v)` check at `base(P)` boundaries works without modification — tokens never appear in closed runtime trees.

## 7. Open questions

**7.1 Meta-variables.** This document did not handle implicit/hole elaboration. A meta entry is a second kind of "stuck neutral" distinct from a hypothesis token. Sketch: introduce `KIND_META` with a uniquely-identified payload; metas live in ctx like tokens but carry a solution field that may be filled by unification. Pattern-fragment solving produces a closed term whose free-variable abstraction is a bind-tree `lam(Bi, body)` — the same construct we already use, closing the loop.

**7.2 `infer`-mode Lam.** When a Lam is encountered without an expected Pi, the domain type is unknown. Standard approach: synthesize a fresh domain meta and a fresh codomain meta, then check the body against the codomain meta. This requires the meta machinery of 7.1.

**7.3 Postponement.** When unification cannot proceed due to unsolved metas, Agda-style implementations postpone the constraint. This proposal as written does not. Adding postponement adds a constraint queue as an additional elaboration-universe data structure.

**7.4 Bind-tree well-formedness as a tree program.** A bind-tree must structurally match its term: `BApp` only over App, `BLam` only over Lam, etc., and `BE` only at `V` positions. The elaborator enforces this by construction, but a separate well-formedness predicate (written as a tree program) would be a useful pre-check and a specification the kernel can rely on.

**7.5 `piPred` — the runtime predicate form of Pi.** `erase` in `ELABORATION_DESIGN.md` references `piPred_tree` without specifying it. This is orthogonal to the elaborator design, but the runtime/synthesis story depends on it. Under the present scheme, `erase(Pi(A, Bi, codom))` should produce a closed predicate that, given a runtime value `f`, decides whether `f` is in the Pi's acceptance set. A concrete proposal is deferred to a follow-up.

**7.6 Interaction with `base(P)` boundary checks.** When elaboration encounters `base(P)` where `P` is a predicate tree and we need to verify that some value `v` inhabits it, the current document says `apply(P, erase(v))`. For this to be well-defined during elaboration, `erase` must run on partially-elaborated terms — which is fine for closed subterms but problematic for terms containing hypothesis tokens. The resolution is probably to defer the `apply` check until all enclosing binders are closed. Design-level work needed.

## 8. Fit with the philosophy

`DEVELOPMENT_PHILOSOPHY.md` requires every checker-relevant data structure and operation to have a declared tree encoding. This proposal satisfies that requirement:

- `splice`, `unsplice`, `normalize`, `check`, `infer`, `H`, `type_of_H`, `extend`, `is_live` — all tree programs, defined by cases on tag kinds that are themselves specific tree shapes.
- The ctx is a tree.
- Hypothesis tokens are trees derived canonically from binder trees.
- `fastEq` is the existing O(1) primitive.

No host features (no `Map`, no exceptions for control flow, no mutable state, no closures). The TypeScript implementation, when written, is an optimization of these tree programs; the tree programs are canonical and must be runnable for cross-validation (philosophy rule 4).

The deletion of the de-Bruijn-level scheme eliminates the level-drift hole without introducing semantic closures, staying within one tree universe. This preserves the metacircular property that a neural synthesizer operating in the object language sees a single, uniform grammar — tagged elaboration trees — with splice and unsplice as the binding operations.
