# Native Type Theory — Known Issues

Living document tracking open and recently-resolved issues in the tree-native type checker.

## Resolved

### "Variable only in right" S-node re-abstraction

When bracket-abstracting `[x] S(left, right)` where `x` is only free in `right`, the elaborator previously re-converted to application form `apply(apply(leaf, apply(leaf, left)), right)` and re-abstracted. This produced `stem(stem(left_annotated))` wrapped in a K-node — a structure that `stemFnCheck` could not verify at Pi-typed codomains.

**Resolution**: Produce `S(K(left), [x]right)` directly, symmetric to the existing "variable in left" case. piCheck handles the K-node via kCheck → recursive piCheck on the annotated structure.

### Dependent K-check over infinite domains

`kCheck` rejected `K(v) : Pi(A, B)` whenever `B` was dependent and `A` was an infinite domain (Nat, Tree, Type). This blocked all polymorphic definitions.

**Resolution**: Constructor-class representative check. Evaluate the codomain family at one representative per tree constructor — `leaf`, `stem(leaf)`, `fork(leaf, leaf)` — and check the value at each resulting type via `checkValOrPi`. For parametric codomain families, piCheck's acceptance reasoning is reflexive (type equality reduces to identity), so checking at three representatives generalizes to all domain values. This is the combinator analog of checking under a binder.

## Open

### Nested application of parameters

**Blocks**: `compose`, `flip`, `xor`, `implies`, any inline triage with multiple parameters, and in general any expression of the form `f (g x)` where both `f` and `g` are lambda parameters.

**What works**:
- `{f x} -> f x` — single application
- `{f g x} -> f x` — unused parameter
- `{a b} -> and a b` — env lookup applied to parameters
- `{A B C g f x} -> g f` — no nesting

**What fails**:
- `{f x} -> f (f x)` — double application
- `{g f x} -> g (f x)` — compose body
- `{a b} -> triage b ({_} -> false) ({_ _} -> false) a` — inline triage (desugars to nested application)
- `{a b} -> boolElim Bool (not b) b a` — multi-arg application of a typed function

**Pattern**: The issue arises when one lambda parameter is applied to the result of another parameter's application. Env lookups work because they're wrapped in ascriptions/annTrees with known types; parameter-to-parameter applications produce AppAnn nodes whose interaction with nested bracket abstraction creates structures piCheck cannot verify.

**Suspected root cause**: The K-ascription type mismatch across abstraction levels.

When `abstract` encounters a K-node containing a free variable, it re-converts to `app(K_ascribed, body)` and re-abstracts. The ascription carries `kTy = fork(bodyTy, K(exprTy))` — the type of K at that specific inner use site.

After further outer abstraction, the K-ascribed sub-term ends up inside another S-node whose context (via `sCodomain`) expects a *different* derived type. piCheck's ascription rule does `fastEq(claimed_type, expected_type)` — a strict identity check. The inner K-ascription's type does not match the outer S-node's derived expected type, so the ascription is rejected.

This is the same family of problem as the resolved issues: the elaborator produces annotations at one context level, but piCheck verifies them at a different context level after further abstraction. The resolved cases were fixed either structurally (keep K-nodes instead of collapsing to `stem(stem(...))`) or in the checker (extend kCheck with representatives). This issue likely needs either:

1. A fix in how K-ascription types are computed during nested bracket abstraction, so they remain consistent across levels; or
2. A way for piCheck to accept an ascription whose claimed type is related-but-not-identical to the expected type (e.g., via `sCodomain` transformation); or
3. Elimination of the K-ascription escape hatch entirely, using the annotated-tree pattern (store K's annotated form, verify structurally) instead of type-identity ascription.

## Bootstrap / structural limitations

These are not bugs but acknowledged limitations of the current architecture:

- **Bootstrap axioms**: definitions in `types.disp` are compiled in bare mode (before piCheck exists) and appear on the allowlist as trust roots. 6 entries: `and`, `or`, `not`, `isLeaf`, `boolElim`, `natElim`. Auditable, unverifiable.

- **Recursive definitions**: `apply(fix, step)` produces combinator structure from the recursion machinery that doesn't match any typing rule. The fixpoint is added to the allowlist after its step function is verified at `T → T`. Unavoidable without redesigning the recursion encoding.

- **Pathological non-parametric codomain families**: the constructor-class representative check is sound for parametric codomain families and for codomain families defined by structural recursion on their argument. A codomain family that behaves differently at some non-representative input could produce false positives. Not observed in practice; considered acceptable.
