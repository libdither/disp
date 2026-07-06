# Modules as telescope-typed functions

Status: slice 1 LANDED (2026-07-06): the given layer (explicit fills,
instantiation cache, well-typed linking) plus hermetic per-file scoping and
the kernel fragment headers. The type-theory half was validated first by
`lib/tests/functor_module_proto.test.disp` (commit 5449c59, 15 pins, zero
kernel changes); slice 1 pins live in `lib/tests/given.test.disp` and
`test/modules.test.ts`. Slice 2 (the functor face: readback, unfilled `use`
as the functor tuple, abstract verification) and slice 3 (dividends) remain.

Landed details that refined the design: in-file forward annotation references
are ALSO givens, self-filled by the barrel's raw pass (`Eq`/`Unit` in base,
`Ord` in positive, `Type`/`RespondShape` in universe; the scan is
`given`-order-sensitive: a given's annotation compiles against earlier givens
plus itself); elaboration errors now carry the file they arose in (hermetic
scoping turns a missing import into an unbound-variable error naming the
forgetful file).

## The design in one paragraph

A module with dependencies is a function from its dependencies to its record
of exports, and its type is a Pi into a Record, which is one telescope
(`[mint dep ; apply out : Exports]`). `use "f"` denotes that function;
instantiation is application; `use "f" { fills }` supplies the declared
dependencies explicitly through the named-argument call surface (puns give
the same-name shorthand; record spread is the future whole-context form).
The elaborator keeps its per-item pipeline by compiling each
dependency as a fresh hyp, so every field stays a closed tree; the module
lambda is read back off the hyp-closed record, and re-elaborating the file
per fill is the host fast path whose specification is plain application of
that lambda (the prototype pins the two routes tree-identical). Files become
hermetic: a file sees its own declarations, its own `open`s, and its declared
dependencies, nothing else.

## Why (what the current mechanism costs)

`lookupEntry` walks the whole scope stack and `resolveUse` pushes the used
file's frame on top of the use site's stack, so a used file resolves names
against everything in scope at its first use site. Three concrete costs:

1. The module cache keys on absolute path but the elaboration depended on the
   ambient stack, so load order decides meaning and the cache freezes it.
2. Shadowable policies (`default_guard`, `let`, `test`) leak into modules
   loaded afterward.
3. Any local name can silently satisfy a dangling reference inside a loaded
   module. Only the kernel fragments rely on this (via the barrel); every std
   and test file already declares its imports.

## Surface

One new word, zero new grammar (DECIDED: the word is `given`). `given` is a
request decorator (an ordinary library value in `cut.disp`, host-fallback
while unbound, exactly the `let` discipline), and the rest is existing
syntax.

```rust
// declaring dependencies: one header block per file (the canonical spelling)
open given {
  add : Nat -> Nat -> Nat            // must be filled at link
  cmp : Nat -> Nat -> Bool := nat_le // optional: default fill
}

// filling them: application of the use-atom to a record, i.e. the existing
// named-argument call surface (reordering, puns, defaults all apply)
m := use "sort.disp" { add := my_add }
open use "sort.disp" { add := my_add }
```

The block is parser sugar: each entry desugars to the line form
`given add : Nat -> Nat -> Nat`, a decorated declaration whose head is the
library value `given`, so the pre-scan, fills, ordering, and driver semantics
are those of the per-name declarations (entries separate by newline/','/';';
entry order is binder order). The line form remains the substrate and still
parses; the block is the one spelling usage should take, and it reads as what
slice 2 makes literal: the dependency record being opened into scope.

Fills are EXPLICIT (decided): there is no silent fill-from-scope. The pun
spelling already gives extract-from-scope ergonomics for same-name fills
(`{ add := add, cmp }`), with one known wart: an all-pun braced body parses
as a bare recType/binder, so at least one field must be written with `:=`.
Record spread (`{ ..ctx }`) subsumes this later (planned alongside the synth
work, independent of this arc). Ordinary files have no givens, so `open use`
stays fill-free everywhere outside the kernel and future functor libraries.

A given-bearing module may not be used without a context AT ALL, raw
included: bare `use "f"` on it is an error, and the empty context is passed
explicitly as `use "f" {}`. So "this module takes context" is always visible
at the use site, while the empty record avoids restating the given list
(which lives one screen away in the module's own header and would drift).
Under a checked `{}` the defaults apply and a missing-without-default given
still errors; under a raw `{}` an unfilled given binds NOTHING. The name is
simply absent from the module's scope; nothing implicit flows, and a value
referencing it errors as unbound, attributed to the file. This is what lets
the kernel fragments raw-open earlier siblings with `{}`: their givens are
annotation-only, raw drops annotations, so the absent names are never
referenced. Explicit fills compose with raw, and defaults compile under raw
too (defaults are values; raw drops only the annotation layer).

A RECORD-typed given is one telescope-typed dependency and needs no new
machinery: `given ctx : { add : Nat -> Nat -> Nat, start : Nat }` followed by
`open ctx` splices the fields into the module's scope (pinned in
given.test.disp with the given_rec_mod fixture). The fill is any record
inhabiting the type: a literal, a bound record, or another module's `.record`
(module-to-module wiring). The interface-record pattern composes with
telescope derived fields for per-field defaults, and in slice 2 an `open` on
an ABSTRACT record-given can read its field names off the given's TYPE
instead of the fill. In the block spelling this is
`open given { ctx : { … } }` followed by `open ctx` (the block declares the
dependency; the second `open` splices the record's fields).

Parser changes: one item form, `open given { … }` (pure sugar, above).
`given X : T` and `given X : T := d` are decorated declarations
(`headFieldP` accepts them); `use "f" { ... }` is an application expression;
`open` already takes an expression.

Notes on the surface:

- Givens never export, and a given does not route through guard consultation:
  introducing a lambda parameter is a different act from binding a name a
  guard could own. The driver handles param-requests before the guard path.
- `given` is not `sig`. A `sig`/interface entry says the importer will provide
  an implementation later, under the name's guard (Install polarity). A
  `given` says the linker provides it now (mint polarity). They are the two
  directions of the same valueless-typed-name shape; both belong in the module
  story, and the symmetry should be stated in COMPILATION.typ.
- Given defaults compile in the module's own scope and may reference earlier
  givens, the same rule as binder-parameter defaults. Given order is binder
  order, and a given's annotation compiles in the scope so far (so
  `given Type : Type` precedes `given Tree : Type`). Redeclaring a given
  name is an error in slice 1.
- `use "f"` with unfilled givens: in slice 1 this is an error listing the
  givens; in slice 2 it denotes the unlinked functor tuple.

### Background: the Request protocol, and where givens sit in it

`Request : Type := { value : Tree, ty : Tree, guard : Tree, private : Bool }`
(universe.disp). The first three fields are stem-options (`t` absent, `t x`
present); `base v` is `{ value := (t v); ty := t; guard := t; private := FF }`.
A request lives for one declaration: the driver builds it, the head decorator
transforms it, the incumbent guard (or `default_guard`) answers
`Ok (Bind v | Install g | Both v g) | Err`, the driver applies the action,
and the request is gone. It is a protocol message, not an accumulator.

`given` extends the message vocabulary: `Request` grows `param : Bool`, the
`given` decorator sets `param := TT` (and `private := TT`), and the four
existing decorators emit `param := FF` so that concrete requests keep
inhabiting the grown `Request` type (the `typecheck Request (base zero)`
pins stay green). A param-request tells the driver "this declaration lands
on the mint side of the module telescope"; it is interpreted by the driver
directly rather than consulted with a guard.

### The module accumulator (why assembly happens at the boundary)

The module under construction is a telescope extended declaration by
declaration: each given appends a mint cell, each annotated export a proj
cell. But the compiled telescope VALUE cannot be extended as you go: a cell's
continuation is a function (`t cell ({x} -> rest)`), and appending on the
right would mean inserting new syntax under the existing binders, the one
thing a value cannot do (the same impossibility COMPILATION.typ records for
block `let`). So the accumulator stays unassembled in elaborator state: a
flat list of (name, tree, type, guard, given?) entries, with dependency
openness represented by hyps so every tree stays closed. The driver already
holds most of this (`fieldNames`/`fieldTrees`/`fieldTypes`/`fieldGuards`);
the given list is the addition. Both module faces are folds over the flat
list at the module boundary: the typ by applying the in-scope `Pi`/`Record`
values right-to-left, the lambda by readback (substitute hyps for variables,
bracket-abstract). One assembly step, exactly where bracket abstraction
already sits.

## Semantics: one meaning, two faces

The meaning of a module file with givens `g_i : T_i` and exports `x_j := e_j`
(annotated `A_j`) is:

```
value : {g_1} -> ... -> {g_n} -> { x_j := e_j ... }
typ   : Pi T_1 ({g_1} -> ... Pi T_n ({g_n} -> Record [(x_j, A_j) ...]))
```

Both faces are ordinary trees the object language can hold, check, apply, and
project. The prototype pins that `param_apply typ value` checks the module
once, abstractly (givens minted as hyps, exports landed by the H-rule), that
application reconverges tree-identically with direct compilation, and that a
hyp of the functor type can be applied and projected (consuming a module you
do not have).

The driver realizes this with two mechanisms related by the native-fast-path
discipline:

- Template instantiation (the fast path): the file's parsed items are cached;
  a fill binds the givens as ordinary scope entries and elaboration runs the
  items. This preserves the whole per-item pipeline: tests discharge per
  instantiation, guard consultations run per instantiation (so each
  instantiation's exports carry their own guards), stats and pristine capture
  behave as today. This is exactly the current raw-then-checked mechanism,
  scoped and made explicit.
- The functor face (the specification): givens compiled as fresh hyps, fields
  as closed hyp-bearing trees, the lambda read back by substituting each hyp
  with a variable and bracket-abstracting (tree-to-CIR substitution plus the
  existing `eliminateLams`; `coh_check` is the precedent for readback). The
  equivalence pin: the readback lambda applied to a fill is tree-identical to
  template instantiation with that fill.

Verification: an instantiated module auto-verifies per fill (the existing
deferred batch; `verifiedModules` keys grow the fill), and each fill is
checked against its given's declared type in the same batch, which is the
well-typed linking. For a parametric module the functor face enables the
optimization the Pi story promises: verify the functor once, verify only
`fill : T_i` per instantiation, skip re-verifying exports. Non-parametric
modules (the kernel) keep instantiation-side verification; abstract checking
would over-reject them, and that is fine.

Hyp identity: dependency hyps are minted with id `pair "<abs path>" idx`, so
they cannot collide with user `make_hyp` smallints or walker mints.

## Hermetic scoping

`resolveUse` gives each file a fresh stack (save, install empty, parse,
restore) instead of pushing a frame on the shared one. A file's environment
is its own definitions, its `open`s, and its givens. Consequences:

- The module cache becomes sound: cache key `(abs, raw?, fillKey)` where
  `fillKey` is the hash-cons ids of the fills in given order; a module is a
  pure function of content and fills. Dep-free files have the empty fill and
  behave exactly as today.
- Policy shadowing (`default_guard`, `let`, `test`) is file-local.
- Silent capture is impossible; a missing import is an unbound-variable error
  in the file that forgot it.

## The kernel bootstrap

Value dependencies between fragments are acyclic by construction, so they
become honest imports. Annotation dependencies are mutually recursive, so
they become givens, filled by the barrel. Each fragment gets a header:

```rust
// cut.disp
open use "../prelude.disp"
open given {
  Type : Type
  Tree : Type
  Request : Type
  GuardAction : Type
  MetaShape : Type
  Action : Type
}

// engine.disp adds:
open use raw "cut.disp" {}  // values of earlier siblings, raw = value layer
```

Fragments open earlier siblings raw, matching the existing rule that values
may only reference earlier fragments; annotations may reference sibling
values (CheckerResult, Action constructors) through those raw opens, and raw
trees equal checked trees (pinned in use_raw.test), so the checked semantics
is unchanged. `given Type : Type` is well-defined under the fill model: at
instantiation the fill binds first and the annotation checks the fill against
itself, which is R6. Kernel fragments' functor faces exist like any module's
(slice 2) but nothing abstractly verifies them; the kernel verifies at
instantiation, as today.

### Why raw-open siblings rather than per-value givens

The alternative is declaring every sibling (and prelude) value a fragment
uses as an explicit given: `given Ok : ...`, `given make_record : ...`,
twenty to forty entries per fragment. What that buys: a maximally
self-describing header, and fragments independently instantiable against
alternative implementations, the full ML-functor shape. What it costs:
large drift-prone headers (every new helper use edits the header), dozens of
fills per fragment at the barrel, and, decisively, most of the substrate
layer is deliberately walker-untypable, so those givens would be typed
`Tree` and verify nothing. It buys self-description at the cost of
pretending the fragments are independent components when they are one kernel
split for legibility.

Raw sibling opens keep dependency at file granularity, which is where the
kernel's real structure lives (the value layer is acyclic by the existing
rule, so the opens are honest imports); the raw mode is required so that a
sibling open during the barrel's raw pass cannot trigger a checked load
whose fills do not exist yet, and it is sound because raw and checked value
trees are identical, with the checking happening once at the sibling's own
checked import. Per-value givens remain the right shape for LIBRARY
functors, where the dependency interface is small and honestly typed (sort
over a comparator); the two styles coexist.

The barrel keeps its two passes, which now mean something: the raw pass
builds the fill environment (values, annotations dropped, unfilled givens
bound to `t`, which is safe because kernel givens are annotation-only), and
the checked pass instantiates each fragment with the now-concrete types.
use_raw.test's direct fragment imports keep working by the same mechanism:
its own frame raw-opens everything first, then its checked imports fill explicitly from
it.

`use raw` generally: fills compose (`use raw "f" { ... }`); unfilled givens
bind to `t`; annotations (including given annotations) drop. Raw remains the
bootstrap escape hatch with its existing trust level.

## The tree-identity question (telescope normalization)

The prototype pinned that the surface functor type and a hand-assembled
two-cell telescope are behaviorally one former but distinct trees: `Pi` keeps
`apply_cell (B h)` open under its mint binder while the hand-built spelling
pre-reduces the closed cell. Two answers:

- Now: same-route construction. The driver builds module types by applying
  the in-scope `Pi`/`Record` values, the same route surface annotations take,
  and hash-consing gives identity for free ("deterministic elaboration
  ensures same type, same tree"). This is sufficient for the module arc and
  is pinned (`tree_eq FlatPiT SimpleModT = FF` documents the hazard).
- Later, yes: normalization at formation would resolve it, and it is exactly
  the NbE ideal NEGATIVE_TYPES.md already names as the telescope's frontier.
  Mechanism: when a telescope is formed, apply each continuation to a fresh
  hyp, reduce, and read the binder back (mint plus readback, both existing
  idioms). Constant families normalize to the K form, so the two spellings
  above converge; dependent families converge up to the evaluator's normal
  forms. This changes definitional equality (strictly more equations), so it
  needs its own arc: soundness of readback (hyp escape), backend confluence,
  and the formation-time cost. It is not a prerequisite for modules and
  should not be bundled into this arc.

## Slices, with the behavior spelled out

### Slice 1: hermetic scoping plus `given` (one arc; the kernel needs both)

Library edits: `given` in cut.disp; `Request` grows `param : Bool` in
universe.disp; `base`/`let`/`sig`/`guard` emit `param := FF` (so concrete
requests keep inhabiting the grown type and the existing typecheck pins stay
green); `default_guard` untouched (name-keyed reads).

Driver, declaration side: a param-request mints a hyp
`make_hyp T (pair "<abs path>" idx)` (collision-proof against user smallint
ids and walker mints), binds the name to it, appends (name, T, default?) to
the module's given list. The given's annotation compiles in the scope so far;
order of appearance is binder order.

Driver, use side: `use "f" { fills }` compiles each fill in the CURRENT
scope, resolves fill names against the given list by the named-argument
rules (any order, defaults for omitted), errors on unknown names or missing
fills without defaults. Bare `use "f"` on a given-bearing file errors,
listing the givens. `open use "f" { fills }` composes. Instantiation
re-elaborates the cached parse with each fill bound at its given's
declaration site, so tests discharge and guard consultations run per
instantiation; every fill is scheduled into the existing deferred
verification batch as `param_apply T fill` (well-typed linking). `use raw`:
unfilled givens bind to `t`, annotations (including given annotations) drop,
explicit fills compose; this is what lets fragments raw-open siblings bare,
since kernel givens are annotation-only.

Hermetic mechanics: `resolveUse` installs a fresh stack per file; the module
cache and `verifiedModules` key on `(abs, raw?, fillKey)` where fillKey is
the hash-cons ids of the fills in given order (dep-free files have the empty
fill and behave byte-identically to today).

Kernel: the seven fragment headers; the barrel's checked pass fills each
fragment explicitly (`open use "cut.disp" { Type := Type, Tree, Request,
GuardAction, MetaShape, Action }`, pun spelling); the raw pass unchanged in
text, now meaning "build the fill environment". Kernel load cost unchanged:
one fill per fragment, cached. use_raw.test keeps working by the same
mechanism (its own frame raw-opens everything, then checked imports fill
explicitly).

Observable flips, each pinned: a file referencing an un-imported name errors
naming that file; shadowing `let`/`default_guard`/`test` no longer affects
modules loaded afterward; two different fills of one file are two
instantiations, the same fill twice is a cache hit.

Estimate: one to two sessions; the risk is the driver scope surgery against
the kernel load path (budgets, caches, pristine captures, tree_eq
registration all touch the stack). The pristine WeakMaps are per-session and
capture-first, so re-elaboration per fill does not disturb them.

### Slice 2: the functor face

Tree-to-CIR hyp substitution plus the existing `eliminateLams` gives
readback; unfilled `use "f"` stops erroring and denotes the functor tuple,
with `typ` built via the Pi route (finding 2) and `record` the readback
lambda. The face is built for every given-bearing module (readback is cheap
and needs no judgment call); ABSTRACT verification only runs when someone
calls `verify` on the unfilled tuple, so kernel fragments need no
special-casing: their faces exist and nobody abstractly verifies them.
Pins: readback lambda applied to a fill is tree-identical to template
instantiation with that fill (the driver-side reconvergence, extending
prototype pin 4); a parametric std module's unfilled tuple verifies `Ok TT`.
Estimate: one session.

### Slice 3: dividends

Verify-once-plus-fill-checks for parametric modules (skip re-verifying
exports per instantiation); partial fills (residual functors, which the
named-argument machinery nearly gives); the module guards/sigs face from the
ownership design (interface exports crossing module boundaries) riding the
same Module shape work. Record spread joins whenever the synth work lands,
independent of this arc.

Docs per slice: COMPILATION.typ grows a Modules section (module = telescope-
typed function, instantiation, the barrel staging made explicit, given/sig
polarity); SYNTAX.typ notes `given` and use-application; KERNEL_DESIGN.md the
fragment header discipline; CLAUDE.md operating notes.

## Test plan

- Scope: a used file cannot see use-site names (pin the leak's absence); a
  file missing an import fails with unbound-variable naming that file.
- Givens: explicit fill, pun fill, default, missing-fill error; fill type mismatch
  fails the deferred batch; givens absent from exports and from `typ`.
- Cache: same file with two fills yields two instantiations (distinct trees
  where the fill differs); same fill twice hits the cache; dep-free files
  byte-identical to today.
- Kernel: full suite green through the fragment-header conversion; use_raw
  pins keep passing; kernel load time unchanged (one fill, cached).
- Functor face (slice 2): readback lambda applied to fills tree-equal to
  template instantiation; unfilled tuple verifies abstractly for a parametric
  std module; `verify` of a kernel fragment's functor face correctly rejects
  or is skipped.
- Policy locality: shadowing `let`/`default_guard`/`test` in a file does not
  affect modules it uses (flips today's behavior; pin the new one).

## Decisions

1. DECIDED: the word is `given` (renameable later; it is a library value,
   not a keyword).
2. `Request.param : Bool`, pending review. The alternative (a distinct
   request shape for givens) avoids touching the four decorators but splits
   the protocol vocabulary; the flag keeps every declaration a Request. See
   the Background subsection for the full shape and lifecycle.
3. Fragment self-description depth: raw-open earlier siblings for the
   kernel; per-value givens for library functors. See the dedicated
   subsection under the kernel bootstrap.
4. DECIDED: fills are explicit; no silent fill-from-scope. Puns cover the
   same-name case today (modulo the all-pun parse wart); record spread
   (`{ ..ctx }`) is the future ergonomics, planned with the synth work.
5. DECIDED: unfilled `use "f"` on a given-bearing file errors in slice 1;
   slice 2 makes it the functor tuple.
