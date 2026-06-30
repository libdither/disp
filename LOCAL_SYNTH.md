# LOCAL_SYNTH.md — `synth`: one in-scope, user-defined term synthesizer

> **Status (2026-06-29): design / idea, not built.** Captures a design developed in dialogue.
> The dual of `param_apply` (`check`): where `check` *verifies* a term against a type, `synth`
> *produces* a term of a type:
>
> ```
> synth : (A : Type) -> Result A          -- Result A = Ok (a : A) | Err
> ```
>
> The thesis: **elaboration completion, proof/tactic, constructor, coercion, and optimization are all
> the same operation** — produce an inhabitant of a type — and a single in-language, user-defined
> `synth` can serve the programmer, the elaborator, the compiler, and the running program through this
> one interface.

---

## 1. The unification: everything is producing an inhabitant of a type

In disp a type **is** a predicate (`A : Tree -> CheckerResult`; `a : A` iff `A a = Ok TT`). Under that
lens these are one operation:

| context | given | produce | really is |
|---|---|---|---|
| synthesize from a spec | a type `A` | `a` with `A a = Ok TT` | inhabit `A` |
| optimize ("satisfy better") | `e : A`, a cost | `e'` with `e' ~_A e`, `cost e' < cost e` | inhabit `A ∧ (~_A e) ∧ (cost ≤ k)` |
| elaborate / tactic / constructor / coercion | partial data `d`, expected `A` | `a : A` using `d` | inhabit `A ∧ (uses d) ∧ canonical` |

Every one is **find `a` with `A a = Ok TT`** — produce an inhabitant of `A`. The "extra constraints"
are *also types* (Intersection is the universal "and"), so `A` is simply the (possibly refined) type
you want inhabited. A *constructor* is this with a trivial guide; the kernel's *H-rule* is this
(reconstruct a neutral); `fmap` is this (produce `F B` from `f` and `F A`); a *proof tactic* is this;
the *optimizer* is this with the hardest guide. They differ only in (a) how `A` is assembled, (b) the
cost/preference order, and (c) how hard the search is.

**The adjoint.** disp has the verify half — `check = param_apply : (A, a) -> CheckerResult`. `synth` is
its converse — `synth : (A : Type) -> Result A`. `synth ⊣ check` (provide vs require); the certificate
mediates.

---

## 2. The interface

```
synth : (A : Type) -> Result A
```

- **In scope, user-defined.** `synth` is an ordinary in-language function the elaborator resolves by
  name. The default lives in the prelude; a module extends or shadows it. This is the metacircular
  discipline taken to elaboration: even completion logic is object-language code.
- **One signature, four callers** — they differ *only* in the type `A` they ask for:
  - **programmer** — `_` / `?A` desugars to `synth A` (fill a hole / discharge a goal);
  - **elaborator** — `synth (domain | field-type | coercion-type)` (named args, record completion, coercions, implicits);
  - **compiler/optimizer** — `synth (A ∧ ~e ∧ cost≤k)` (optimize `e`);
  - **the running program** — the same call at runtime (self-improvement). Because `synth` is
    in-language, the program can synthesize-and-certify versions of itself; and since `synth` is a
    term, it can be a `synth` *target* — improving its own guide.

---

## 3. Why it is safe: untrusted reflective `synth` + cheap total `check`

`synth` is **reflective**, not parametric. A literal parametric `(A : Type) -> A` is uninhabited except
`Err` (you cannot conjure an inhabitant of an *abstract* `A`). To do real work `synth` must inspect the
type's structure ("record? Pi? Coproduct?") — i.e. reflect on `A` *as data* — which is fine because at
a real call site `A` is concrete.

Consequences:

- **The annotation `synth : (A : Type) -> Result A` only verifies *vacuously.*** The kernel verifies it
  by minting an abstract `A`-hyp; raw structural reflection on it trips the parametricity guard, so you
  guard it (`if is_neutral A then Err else <dispatch>`, using the sanctioned sig-reads
  `is_neutral`/`pair_fst`/`checker_sig`). Under the abstract `A` the guard takes the `Err` branch and
  the real dispatch is dead — so the type checks *without* the kernel ever validating the concrete
  branches. The annotation is therefore **documentation/contract, not a guarantee.**
- **Soundness comes from `check`, and it's nearly free.** `synth`'s output is spliced where `A` is
  expected and re-checked — `param_apply A a = Ok TT`, O(1) hash-cons conversion. Even if the
  elaborator skipped the explicit check, disp's pervasive **use-site re-checking** catches a bad term
  downstream. So a *buggy* `synth` can never produce unsound code — worst case it yields a term `check`
  rejects → an elaboration error at that site.
- **Division of labor:** the user owns *completeness/usefulness* (does `synth` handle my case?); the
  kernel owns *soundness*. You can ship a half-working `synth` safely and grow it.

The whole thing rests on the verify/search asymmetry: a *cheap total check* licenses an *arbitrarily
powerful untrusted* `synth` (neural, e-graph, tactic, host I/O). This is the optimizer's
untrusted-search/trusted-checker architecture (FOUNDATIONS §13–15) generalized to all synthesis.

---

## 4. The context lives *in the type* (a type is a dependent context)

No separate `ctx` / `seed` / `budget` / `guide` arguments — each folds into the type `A`, or into which
`synth` is ambient:

- **partial data** (record completion / named args) → the **residual telescope**: consume the supplied
  fields, substitute, ask for what's left. "What's given" = "`A` is now the smaller telescope."
- **seed** (optimization) → a refinement, `A ∧ (x ~_A e)`.
- **scope / hypotheses** (proof search) → the **Pi domain**, `Γ → A`: synth `λΓ. body` with the Γ-hyps
  available as terms.
- **budget** → a coeffect grade on `A` (`A` graded by `cost ≤ k`).
- **guide** (the search strategy) → not in `A`; it's the *ambient in-scope `synth`* itself.

So the interface really is just `synth : (A : Type) -> Result A`. The work *relocates* into
**constructing `A`** (residual telescopes, `Γ →`, refinements, coeffects) — clean telescope
manipulation — plus the substrate for the `~_A` and cost refinements (the same the optimizer needs).

---

## 5. Implementation: `synth` is `at` run backwards

`at TT cells v` **recognizes** — walks a telescope's cells, *consuming* `v`. `synth` walks the *same
cells*, *producing* a value. Each cell op has a synth-meaning dual to its recognize-meaning:

| cell | recognize (`at TT`) | synthesize |
|---|---|---|
| `proj name ty` | check `v.name : ty` | produce a value of `ty`, label `name` |
| `deriv name recipe` | check `v.name ≡ recipe` | *use* `recipe` |
| `mint T` (Pi dom) | ∀-introduce a hyp | λ-bind it (a function awaiting it) |
| `apply (B h)` (Pi cod) | check `(v h) : B h` | the codomain to synth |

The elegant target: a **third cell mode** (`SY`, alongside `TT` recognize / `FF` respond), so record/Σ
synthesis is literally `at` in synth-mode. The one op whose synth-meaning genuinely differs is `mint`
(produce a *binder* vs mint a checking-hyp), so Pi synthesis is a small dedicated handler.

### Dispatcher

```
synth A =
  if is_neutral A then Err                  -- abstract type: no structure to reflect on
  else first_ok A [ synth_unit, synth_eq, synth_record, synth_pi, synth_sum, ...user ]

first_ok A hs = match hs {
  nil       => Err
  cons h tl => match (h A) { Ok t => Ok t ; Err _ => first_ok A tl }
}
```

### Handlers for useful types

```
-- trivial inhabitants
synth_unit A = if tree_eq A Unit then Ok unit_val else Err

-- Eq T x y: refl exactly when the endpoints already convert (O(1) tree_eq = conversion)
synth_eq A = match (eq_endpoints A) {
  Ok {lhs, rhs} => if tree_eq lhs rhs then Ok refl else Err
  Err _ => Err
}

-- Record / Σ: the synth-walk (at backwards over proj/deriv cells), threading each
-- synthesized value into the dependent tail exactly as `at` threads it.
synth_record A =
  if (lead_is_proj_or_deriv (cells_of A)) then go (cells_of A) nil else Err
go cells acc =
  if (is_fork cells) then {
    let cell = pair_fst cells
    match (cell_kind cell) {
      Proj name ty   => bind (synth ty)  ({v} -> go ((pair_snd cells) v)   (cons (pair name v)   acc))
      Deriv name rec =>                          go ((pair_snd cells) rec)  (cons (pair name rec) acc)
      _ => Err
    }
  } else (Ok (mk_record (names acc) (vals acc)))

-- Pi: λ-bind the arg, synth the codomain with it available (else proof-search an in-scope hyp)
synth_pi A =
  if (lead_is_mint (cells_of A))
    then Ok ({h} -> match (synth (pi_cod A h)) { Ok body => body ; Err _ => <search hyps> })
    else Err

-- Coproduct: canonical constructor (a nullary variant if any), else synth a variant's args, else Err
synth_sum A =
  if (is_coproduct A)
    then match (nullary_variant A) { Ok tag => Ok (inj tag t) ; Err _ => synth_variant_args A }
    else Err
```

**Worked example.** `synth ({ n : Nat, pf : Eq Nat (add n 0) n })` with `n := 3` pre-supplied: the walk
threads `n=3`, so the `pf` cell's type is `Eq Nat (add 3 0) 3`; `synth` recurses → `synth_eq` checks
`add 3 0 ≡ 3` by conversion → `refl`; result `mk_record [n, pf] [3, refl]`. The dependent threading
(`pf`'s type mentions `n`) is the *same* mechanism `at` uses to recognize dependent records. Note:
**defaults are `deriv` cells (no `synth` needed)** — `synth`'s job for records is the *auto/implicit/
proof* fields.

---

## 6. Elaborator integration

The elaborator never hardcodes completion logic; it does **type construction** + a uniform loop:

1. **Hole `_ : A`** → ask `synth A`.
2. **Record completion** `{ supplied } : R` → thread supplied fields through `R`'s telescope; each
   *missing, non-derived* field → `synth` on its substituted type; combine with supplied.
3. **Named call** `f { … }` → `f`'s residual domain telescope is the type to inhabit (subsumes the
   landed named-args feature).
4. **Coercion** `x : A`, expected `B` → ask `synth (A -> B)` and apply to `x` (or `synth B` with `x` in scope).

Then, uniformly (reify → run → check → splice):

```
elaborate_synth_site(typeExpr):
  aTree      = compileType(typeExpr)              -- the wanted type, as a tree
  resultTree = session.apply(SYNTH_TREE, aTree)   -- run the in-language `synth` on the backend
  match classify(resultTree):
    Ok term -> if param_apply(aTree, term) == Ok_TT then splice(term)   -- CHECK before use
               else error("synth produced an ill-typed term at <site>")
    Err     -> error("no synth for <typeExpr> at <site>")
```

The integration surface is one new entry point in `src/compile.ts` (and the self-hosting
`lib/elab/compile.disp`): at a completion site, reify the type, run `synth`, check, splice.

---

## 7. Two contracts

- **Determinism (elaboration callers).** Record/named-arg completion must be deterministic so `tree_eq`
  conversion holds (`f{x,y} ≡ f{y,x}`). The handlers above are (single canonical choice per cell; `Err`
  rather than guess). *Optimization* may be nondeterministic — its result is a certified *value*, not a
  conversion-compared tree.
- **Graceful `Err` on abstract types.** A type that is still abstract (under a generic `(A : Type) ->
  …`) has no structure to dispatch on → `Err`. Use the sanctioned sig-reads for the guard so this
  *returns `Err`* rather than raw-triaging the hyp (which would abort the walker). "Synthesize concrete
  types, `Err` on abstract" is the natural behavior — and it's what makes the vacuous typing in §3 work.

---

## 8. The contexts, unified

| caller | type it asks for | guide (= ambient `synth`) |
|---|---|---|
| programmer | `A` (a hole / `?A`) | auto / a tactic in scope |
| elaborator | residual telescope / `A -> B` / field type | deterministic canonical fill |
| compiler / optimizer | `A ∧ ~e ∧ cost ≤ k` (seed `e`) | e-graph + cost-descent + neural |
| running program | whatever it wants of itself | a learned model (self-improvement) |

Constructors, tactics, coercions, implicits, elaboration, and optimization are all `synth` with a
different type, cost-grading, and guide — and the kernel's `check` certifies every result.

---

## 9. What's needed / open questions

- **Type construction in the elaborator** — residual telescopes, `Γ →`, refinement/coeffect wrapping.
  Mostly telescope manipulation; tractable.
- **The `~_A` refinement and cost-coeffect formers** — needed for optimizer-grade types (`A ∧ ~e ∧
  cost≤k`). The same unbuilt substrate the optimizer needs (`OPTIMIZER.typ`; the equality-licensing and
  cost-coeffect work). The elaboration handlers (§5) need *none* of it.
- **The `SY` cell mode** — optional kernel touch to make record/Σ synthesis literal `at` reuse; without
  it `synth_record` is a standalone walk. `mint`'s synth-meaning (produce a binder) differs from
  recognition regardless, so Pi synthesis stays a dedicated handler.
- **Real search** — multi-step proof, non-obvious constructor args: where a handler graduates from "one
  canonical choice" to a genuine guide (tactic / e-graph / neural). Same interface, harder body; the
  bridge to the optimizer (`GOALS.md`, `FOUNDATIONS.md` §13–15).
- **Bootstrap/staging** — `synth` runs at elaboration time (reflection); keep its own definition free
  of synth-requiring sugar to avoid a cycle.

---

## 10. Relationship to existing pieces

- **`param_apply` / the cells / `at`** — `synth` is `param_apply`'s converse and reuses the cell
  telescope (`at` backwards). The kernel's `param_apply` dispatch (reify a neutral's metadata → route
  to the type's in-language `respond`) is the *runtime* precedent; `synth` is the same pattern at
  *elaboration* time.
- **The H-rule** — already a tiny `synth` (reconstruct a neutral of a stored type).
- **Named + default + reorderable args** (landed, commit `ee3e25d`) — a special case: `synth` over a
  function's residual domain telescope. Today it's hardcoded host TS; under this design it's one
  handler.
- **`OPTIMIZER.typ`** — the optimizer is the heaviest `synth` guide (cost-graded, `~_A`-refined type).
- **`ELABORATOR_PLAN.md`** — the self-hosting elaborator is where the in-language `synth` lives and is
  invoked; the "syntax maps / completions, in-language" endpoint generalized to a reflective dispatcher.
- **`GOALS.md` / `FOUNDATIONS.md`** — synthesis (optimizer) + reflection (in-language elaborator) +
  certificate-checking, self-applied, is the unoccupied bet; `synth`/`check` is its elaboration-shaped
  instance.
