# Scope Verification Investigation

This note captures the current design question around `bind_hyp`, sealed
neutral metadata, trusted handlers, and what needs to be verified if
`safe_apply` disables raw triage outside trusted handlers.

## Starting Assumption

The intended model is:

- `safe_apply` is the security membrane.
- Outside trusted handlers, raw triage and raw effect application are disabled.
- User code can only eliminate protected values through typed handlers.
- Trusted handlers may run raw because they are the typed boundary that wraps
  the unsafe substrate operation.

In this model, handlers play the same conceptual role as eliminators:

- `hyp_reduce` handles applying symbolic hypotheses.
- `eliminator_frame` handles case analysis on concrete values and creates
  stuck eliminations for symbolic values.
- Host/effect handlers should be wrapped behind typed postulates or typed
  capabilities.

## Why Escape Checking Still Exists

Disabling triage blocks raw structural inspection, but it does not by itself
prevent a locally minted hypothesis from escaping its scope.

This must be rejected:

```disp
bind_hyp Nat ({x} -> Ok x)
```

It manufactures a symbolic `Nat` outside the local assumption scope.

This must also be rejected:

```disp
bind_hyp Nat ({x} -> Ok (pair x zero))
```

Even if user code cannot raw-triage the pair, some trusted projection or
Sigma/record eliminator may later recover `x`.

Host/effect payloads are also unsafe by default:

```disp
bind_hyp Nat ({x} ->
  Ok (host_write_stdout (nat_to_string x)))
```

Unless the effect handler is explicitly verified as symbolic-safe, a local
hypothesis must not be smuggled into an effect payload.

The escape check is therefore a scope check, not just a triage check:

> A value introduced by `bind_hyp` may not appear in public output positions
> after the `bind_hyp` scope closes.

## Simplified Rule Under Full Triage Blocking

If raw triage is completely disabled outside trusted handlers, the current
open/closed path machinery can likely be replaced by a simpler rule:

1. `bind_hyp` mints a fresh hypothesis with a fresh scope token.
2. After the body runs, scan the public output.
3. Reject if the fresh hypothesis/scope appears in public output.
4. Do not scan inside sealed neutral/stuck metadata.

This treats neutral/stuck metadata as handler-private state.

Current implementation already approximates this: `q_contains_via_open_path`
stops at any `hyp_reduce` neutral root, treating its metadata as walled off.
In the current code, `StuckElim` is also encoded through `hyp_reduce`, so it
inherits the same sealed treatment.

## The Catch: Metadata Becomes Trusted

Ignoring neutral/stuck metadata is only sound if every trusted consumer of
that metadata preserves the abstraction.

Bad handler pattern:

```disp
// Reads sealed spine metadata and returns an old scoped argument directly.
cod_fn meta arg =
  Return (first_spine_arg meta)
```

This would bypass escape checking:

```disp
bind_hyp Nat ({x} ->
  Ok (some_neutral_with_spine_containing x))
```

The result looks safe if the scanner skips neutral metadata. Later, a trusted
handler reads the metadata and returns `x` publicly.

So the simplified escape rule requires a stronger trusted-handler invariant:

> Handler-private metadata may contain scoped values, but trusted handlers may
> never project scoped metadata back into public output unless they re-seal it
> under an equal or narrower scope.

## Verification Target

The important property is not merely "kernel handlers are typed." It is:

```text
trusted handlers preserve scope and sealing
```

More precise form:

- A handler may inspect sealed metadata.
- A handler may repackage scoped metadata into another sealed symbolic form.
- A handler may return closed/public-safe values.
- A handler must not return scoped metadata as ordinary public data.

This is a noninterference-style property:

```text
private symbolic metadata can influence future symbolic metadata,
but cannot be projected into public data.
```

## Suggested Disp-Level Contracts

The spec can expose this as ordinary library types/validators.

Sketch:

```disp
Scope := Tree

WellScopedPublic := {scope, v} ->
  not (contains_scope_public scope v)

WellScopedAny := {scope, v} ->
  not (contains_scope_any scope v)

HandlerSpec := record {
  sig : Symbol,
  meta_type : Type,
  arg_type : Pi meta_type ({_} -> Type),
  result_type : Pi meta_type ({m} ->
                  Pi (arg_type m) ({_} -> Type)),

  sealed_metadata_policy : MetadataPolicy,

  run : Pi meta_type ({m} ->
        Pi (arg_type m) ({a} ->
        CheckerResult (result_type m a))),

  preserves_scope :
    Pi Scope ({s} ->
    Pi meta_type ({m} ->
    Pi (arg_type m) ({a} ->
      Proof (handler_scope_preserving
        s m a run sealed_metadata_policy))))
}
```

Current Disp may not have the full proof machinery yet, but this is the
contract shape the spec should aim toward.

## Dispatch Environment Should Carry Specs

The current new spec describes `Σ` as a list of handler trees. For scope
verification, this is too weak.

Prefer:

```disp
DispatchEntry := record {
  sig : Symbol,
  handler : Tree,
  spec : HandlerSpec,
  cert : HandlerSatisfies spec handler
}

Σ := List DispatchEntry
```

This also addresses a separate concern: if `safe_apply` checks only that a
wait-form's signature is pinned, then raw-applies the embedded handler, Σ is
only a trust set, not a routing table. A structured `DispatchEntry` lets
`safe_apply` route to the verified handler for that signature, rather than
trusting any embedded tree with the same signature.

If the design keeps embedded-handler wait-forms, the spec needs a proof that
`checker_sig` is injective enough for all trusted signatures, and that users
cannot construct another handler with the same signature.

## Per-Handler Obligations

### `bind_hyp`

- Mint a fresh scope token.
- Run the body under `safe_apply`.
- Reject if the fresh hypothesis/scope appears in public result positions.
- Ignore sealed neutral/stuck metadata only if all metadata consumers are
  certified scope-preserving.

### `hyp_reduce`

- May read neutral metadata.
- `Extend T` may place scoped values into sealed metadata.
- `Return v` must prove `WellScopedPublic scope v`.
- Must never return old spine payloads directly.
- Codomain functions are part of the trusted surface unless constrained.

### `eliminator_frame`

- May inspect whether a target is symbolic.
- If symbolic, must not run cases.
- May store the symbolic target in sealed metadata.
- May compute the stored result type via the motive, but the target remains
  private.
- Concrete dispatch must run through `safe_apply`, not raw user code.

### `postulate` and Host Effects

- Default rule should require `WellScopedAny payload`, not merely
  public-safe payload.
- Real host IO must not receive scoped symbolic values hidden inside sealed
  metadata.
- A symbolic host handler, such as future async, must be explicitly certified
  as a sealed stuck-form producer.

### Type `applicable` / Codomain Functions

The current shape

```disp
codomain_fn : raw_meta -> arg -> Action
```

is too unconstrained. The spec should introduce an `ApplicableSpec` and an
`ActionWellScoped` validator:

- `Extend T`: safe if `T` is valid as sealed stored type metadata.
- `Return v`: safe only if `v` is public-safe for the current scope.
- `Invalid`: always safe.

This is especially important for Type's predicate-side H-rule and Pi's
codomain function.

## What Can Be Verified Mechanically In Disp

Likely feasible:

- Metadata shape.
- Handler signature uniqueness.
- Action result shape.
- Postulate payload closedness.
- Public-result escape after `bind_hyp`.
- Native/in-language dispatcher equivalence tests.
- Adversarial tests for each handler:
  - direct hyp return,
  - pair/record return,
  - checked-wrapper return,
  - metadata smuggling,
  - host payload smuggling,
  - `Return` of sealed metadata contents.

Harder or impossible for arbitrary trees:

- Semantic noninterference: proving a handler never leaks sealed metadata.
- Total correctness of arbitrary recognizers/codomain functions.
- Parametricity of arbitrary raw handler bodies.

For those, either:

1. Keep such handlers in the trusted base and verify them with focused tests
   plus an external proof/model, or
2. Introduce a small trusted handler DSL whose constructors preserve the
   scope/sealing invariant by construction.

## Recommended Next Spec Move

Replace the open/closed path section with a scope/sealing section:

1. Define scopes and public positions.
2. Define sealed symbolic metadata.
3. Define `WellScopedPublic` and `WellScopedSealed`.
4. Make `bind_hyp` scan public output only.
5. Make every sealed-metadata consumer carry a `ScopePreserving` certificate.
6. Make `kernel_handlers` a list of verified dispatch entries rather than
   plain handler trees.

The key invariant to carry forward:

```text
All handlers that can inspect sealed metadata must be verified
scope-preserving.
```

This includes kernel handlers, type codomain functions, symbolic effect
handlers, and future handlers that return stuck forms.
