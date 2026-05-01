#set document(title: "Disp Compilation Pipeline")
#set page(margin: 2cm, numbering: "1")
#set text(font: "New Computer Modern", size: 10.5pt)
#set heading(numbering: "1.")
#show heading.where(level: 1): set text(size: 18pt, weight: "bold")
#show heading.where(level: 2): set text(size: 14pt)
#show heading.where(level: 3): set text(size: 11.5pt, style: "italic")
#show link: set text(fill: rgb("#0b63b0"))

#align(center, text(22pt, weight: "bold")[Disp Compilation Pipeline])
#v(0.5em)
#align(center)[
  How `.disp` source becomes a set of named tree-calculus terms.\
  Complementary to #raw("SYNTAX.typ") (surface grammar + AST) and
  #raw("TYPE_THEORY.typ") (type system semantics).
]
#v(1em)

= Overview

Compilation is a *single parser-driven walk*: the parser reads source,
builds surface AST incrementally, and invokes a backend elaborator on
each complete expression it produces. The elaborator is a pure function
`Expr → Tree` --- no scope, no statements, no state threaded in or out.

The parser handles every surface-only node (`Let`, `Test`, `Use`,
`Block`, `Def`, `Program`). The elaborator only sees core expressions
(`Var | Leaf | Hole | App | Proj | Binder | RecValue | Ann`).

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Responsibility*][*Owner*],
  [tokenize, grammar, braced-form disambiguation],       [parser],
  [loading files for `use` expressions; cycle detection], [parser],
  [`let` scope, substitution at use sites],              [parser],
  [compile-time `test` evaluation + comparison],         [parser (calls elab on both sides)],
  [converting expressions to backend terms],             [elaborator],
  [metavariable solving, unification, type checks],      [elaborator],
)

The host-side parser in #raw("src/parse.ts") handles all surface-only
nodes and compiles expressions to tree-calculus terms via bracket
abstraction. The elaborator (not yet implemented) will sit between
parsing and emission, providing type-directed compilation.

= Parser

== Responsibilities

- Tokenize and apply the grammar per `SYNTAX.typ`.
- Process statements (`let`, `test`) and items (`let`, `test` at top
  level) in source order (see § `let` desugaring, § `test` discharge).
- Load files for `use` expressions, holding a cycle-detection stack
  (see § Import cycles).
- Maintain a *name table* mapping every `let`-bound name in scope to
  its parsed expression, and inline at use sites so the final AST has
  no `Var` referencing a `let`.
- Simplify empty Blocks: `Block { members: [], trailing: e }` becomes
  `e`.
- Reject shape errors (empty binder `{} -> body`, `:=`/`:` mixing,
  duplicate binder params, intermediate bare expressions in a Block,
  etc.).

Non-responsibilities: the parser does *no* tree construction, *no*
binder-scope resolution (that's the elaborator's job --- `Var` nodes
referencing binder params stay in the AST), and *no* type checking
(ascriptions become `Ann` nodes; elab runs the kernel predicate).

== Scope

Every braced form and every file introduces a nested scope. `let`s
inside the braces bind only within the braces. Name lookup walks scope
frames inner-to-outer; inner bindings shadow outer.

```disp
let x = t
{
  let x = t t                  // shadows outer x within this scope
  test x = t t                 // passes against inner x
}
test x = t                     // passes against outer x
```

== `let` desugaring

Every `let` is resolved at parse time. Type annotations always travel
on the value via `Ann`; `Binder.param.type` is reserved for
user-written lambda/Pi parameters and is always `null` in
let-desugarings.

*Internal* `let x (: T)? = body; rest` (inside a Block): parse `body`,
bind `x` in the surrounding scope's name table, then process `rest`
with `Var x` occurrences inlined. Equivalently:

```
App(Binder([{name: "x", type: null}], rest_as_expr), body')
```

where `body'` is `Ann(body, T)` if typed else `body`. The parser
performs the inlining so the elaborator sees a closed expression.

*Top-level* `let x (: T)? = body`: parse `body`, store under `x` in the
top-level name table, and record a `Def { name: "x", body: body' }` in
the `Program`. Downstream references inline the same way.

Recursive `let` fails naturally: `Var x` inside `body` looks up the
*outer* scope (the new `x` binding takes effect only after `body` is
parsed). An unresolved-name error is reported with a "use a fix
combinator" hint (see § Error reporting).

== `use` expression

`use "path"` is an atom (see `SYNTAX.typ`) that elaborates by loading
and running the referenced file. Steps:

1. Resolve `path` to an absolute path relative to the directory of the
   file containing the `use`.
2. If the resolved path is on the in-progress stack, emit an import
   cycle error with the full cycle (`a.disp → b.disp → a.disp`).
3. Otherwise push, parse the file to completion (discharging its tests
   and building its `Program`), pop.
4. Replace the `Use` node with a `RecValue` whose fields are the
   file's top-level `Def`s in source order.

Parsed-and-popped files may be revisited via independent `use` chains;
memoizing them is an optimization, not a correctness requirement.

== `test` discharge

`test lhs = rhs` parses both sides, invokes the elaborator on each
(inheriting the surrounding scope), and compares the resulting trees
by hash-cons identity. Hash-cons-equal ⇒ discharged; otherwise a
compile error. After discharge, the `Test` node is removed from its
enclosing Block / Program.

*Test under a live binder.* A `test` nested inside a Block that is
itself under a binder body (`{x} -> { test f x = g x; x }`) is
rejected at parse time: the hypothesis `x` has no concrete value, so
hash-cons equality on open terms would be a type/equality judgement
(belongs in an `eq` type), not an assertion.

== Block simplification

After `let` / `test` processing, a Block with an empty `members` list
is collapsed: `Block { members: [], trailing: e } → e`. Discharged
tests are *not* retained in the reduced Block by default.

= Elaborator

== Overview

The elaborator is integrated into the existing parser-driven
compilation pipeline. It extends `compileExpr` (the function that
converts `Expr` to `Tree` via bracket abstraction) with a single
additional parameter: *mode*, which is either `term` or `type`.

In *term mode*, a `Binder` compiles to a lambda (bracket abstraction)
--- the current behavior. In *type mode*, a `Binder` compiles to a
`Pi` type that *contains* the lambda as its codomain function. All
other expression nodes compile identically in both modes.

This design reflects the Curry-Howard correspondence at the
compilation level: `{x : A} -> B` is simultaneously lambda
abstraction and Pi formation, differentiated only by mode. The bracket
abstraction is shared; type mode wraps the result in `Pi(domain, _)`.

== Contract

```
compile(e: Expr, mode: "term" | "type"): Tree
```

- *Input* — a core expression (`Var | Leaf | Hole | App | Proj |
  Binder | RecValue | Ann`). Surface-only nodes have been processed
  away by the parser.
- *Mode* — determines whether binders compile to lambdas or Pi types.
- *Output* — a tree-calculus term. In type mode, the output is a
  kernel predicate (a function that accepts values and returns
  `TT`/`FF`).

== The binder/Pi correspondence

The central insight: for `{x : A} -> B`, bracket abstraction of `B`
over `x` produces a function `f` where `f(v) = B[x := v]`. This is
*exactly* what a Pi type's codomain function is.

```
compile({x : A} -> B, term) = abstract(x, compile(B, term))
compile({x : A} -> B, type) = Pi(compile(A, type), abstract(x, compile(B, type)))
```

The lambda IS the codomain function. Pi wraps it with the domain.

For non-dependent arrows (`A -> B`, sugar for `{_ : A} -> B`):

```
compile(A -> B, term) = K(compile(B, term))
compile(A -> B, type) = Arrow(compile(A, type), compile(B, type))
                       = Pi(compile(A, type), K(compile(B, type)))
```

Multi-param binders desugar right-associatively. Each parameter layer
wraps the remaining body in `Pi(domain, ...)`:

```
compile({x : A, y : B} -> C, type)
  = Pi(compile(A, type), abstract(x,
      Pi(compile(B, type), abstract(y, compile(C, type)))))
```

== Mode propagation

Mode determines how binders compile but does not affect other nodes.
The rules for which sub-expressions get which mode:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Position*][*Mode*],
  [`Ann.type`],                          [type],
  [`Ann.expr`],                          [term],
  [`let` type annotation],               [type],
  [`let` body],                          [term],
  [`Binder` param type (in type mode)],  [type],
  [`Binder` body (in type mode)],        [type],
  [`Binder` body (in term mode)],        [term],
  [`App` function and argument],         [inherit from parent],
  [`RecType` field types],               [type],
  [everything else],                     [inherit from parent],
)

Entry points: `let` bodies start in term mode; `let` type annotations
and `Ann.type` start in type mode. Inside a type-mode binder, both
the param types and the body stay in type mode (types can contain
types). Inside a term-mode binder, the body stays in term mode
(terms contain terms).

== Per-node handling

- `Leaf` --- returns the tree-calculus leaf (both modes).
- `Num` --- builds `succ(...zero...)` via in-scope constructors (both
  modes).
- `Var { name }` --- looks up in scope, returns compiled tree or
  emits free variable for bracket abstraction (both modes, identical).
- `App { fn, arg }` --- compile both in the current mode, apply.
- `Binder` (term mode) --- shadow params, bracket-abstract body.
  Identical to current behavior.
- `Binder` (type mode) --- shadow params, compile param type in type
  mode, bracket-abstract body (compiled in type mode), wrap in
  `Pi(domain, codFn)`. See § The binder/Pi correspondence.
- `RecType` --- compile to a kernel record type (see § Record
  encoding). Only valid in type mode.
- `RecValue` --- Church-encode fields (both modes, identical to
  current behavior).
- `Proj` --- resolve field index from compile-time metadata, emit
  selector application (both modes, identical).
- `Ann { expr, type }` --- the checking node. See § Type checking.
- `Hole` --- not supported in the initial implementation. Future:
  fresh metavariable solved by unification.
- `Use` --- resolved by the parser before reaching the elaborator.

== Type checking

At an `Ann` node (`(e : T)`) or a typed `let` (`let x : T = body`):

1. Compile `T` in type mode → `T_tree` (a kernel predicate).
2. Compile `e`/`body` in term mode → `e_tree` (a value).
3. Apply: `result = applyTree(T_tree, e_tree, budget)`.
4. Assert: `FAST_EQ(result, TT)`. If not, report a type error.
5. Return `e_tree` (the type is erased after checking).

Type checking is function application. The kernel does all the work
--- the elaborator merely calls it.

Optionally, the elaborator can also verify that `T` is a valid type
by checking `applyTree(Type_n_tree, T_tree, budget) = TT` for some
universe level. This is not required for soundness (an ill-formed
"type" will simply reject everything it's applied to) but produces
better error messages.

== Trusted definitions <trusted>

Certain kernel definitions must be available to the elaborator as
known trees so it can construct Pi types, Arrow types, etc. These
are introduced with the `trust` keyword:

```disp
trust Pi = {domain, codFn} -> wait kernel_ref.pi (make_pi_meta domain codFn)
trust Arrow = {a_type, b_type} -> Pi a_type ({_} -> b_type)
trust Type = {rank} -> wait kernel_ref.type rank
```

A `trust` binding behaves identically to `let` for scope and
compilation, but additionally registers the compiled tree under
its name in the elaborator's *trusted table*. The elaborator uses
this table to:

- Look up `Pi_tree` when compiling binders in type mode.
- Look up `Arrow_tree` when compiling non-dependent arrows in type
  mode.
- Look up `Type_tree` when verifying that a type annotation is a
  valid type.

Trusted definitions are ordinary definitions with no special
runtime semantics. The `trust` keyword is purely a signal to the
elaborator: "this tree participates in elaboration-time
construction." It does not bypass any checking or grant elevated
privileges.

The trusted table is populated by processing `trust` items in source
order, like `let`. Trusted names from a `use`d file are available
if that file exports them. The minimal trusted set for type checking
is:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Name*][*Used for*],
  [`Pi`],    [type-mode binder compilation: `{x : A} -> B` → `Pi(A, codFn)`],
  [`Arrow`], [type-mode arrow sugar: `A -> B` → `Arrow(A, B)` (optional; can use `Pi` + K)],
  [`Type`],  [universe checking: verify `T` is a valid type],
)

If a binder in type mode is compiled but `Pi` is not in the trusted
table, the elaborator falls back to term-mode compilation (lambda)
and emits a warning. This allows files that do not import the kernel
to still parse without errors --- they just lose type-checking
capability.

== Interaction with existing compilation

The elaborator extends the existing `compileExpr` / `exprToCir`
pipeline, not replaces it. The changes are:

1. `compileExpr` gains a `typeMode: boolean` parameter (default
   `false` for backward compatibility).
2. The `binder` case in `exprToCir` checks `typeMode` and emits
   `Pi(domain, lam)` when true.
3. The `ann` case in `exprToCir` compiles both sides and runs the
   check instead of erasing.
4. The driver's `runItem` for typed `let` items compiles the type and
   checks.
5. `trust` items are processed like `let` items but additionally
   register in the trusted table.

The `recType` case currently throws; it will be implemented to
compile to the kernel's record type encoding (see § Record encoding).

== Record encoding

For a RecValue with $n ≥ 1$ fields `f1 := e1; ...; fn := en` of
(possibly-dependent) types `X_1, ..., X_n`, the corresponding record
type is the dependent Church product

```disp
{A : Type} -> ({x1 : X_1, ..., xn : X_n} -> A) -> A
```

inhabited by `{A} -> {k} -> k e1 ... en`. Projection `.f_i` compiles
to `{p} -> p ({x_1 : X_1, ..., x_n : X_n} -> x_i)`.

The empty `{}` is the Church unit `{A : Type} -> A -> A`, inhabited by
the polymorphic identity.

Duplicate field names in a RecValue are legal: only the final
occurrence is kept; earlier ones are shadowed, and the projection
morphism for the name targets the last position. Duplicate field names
in a RecType are a parse error.

= Error reporting

All compile errors carry source spans, inherited from the surface AST.

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Kind*][*Source*],
  [*unresolved name*],         [parser: a `Var` whose name matches neither an in-scope `let` nor an enclosing binder param],
  [*recursive `let`*],          [parser: a `Var` whose name matches a `Def` or `Let` currently being parsed; see below],
  [*type mismatch*],            [elab: type predicate returns FF on a typed `let` body or `Ann`],
  [*failed test*],              [parser: `test`'s two sides elaborate to trees that disagree by hash-cons identity],
  [*unsolved hole*],            [elab: `Hole` not yet supported; compile error],
  [*test under live binder*],   [parser: a `test` inside a Block surrounded by binder params with no concrete values],
  [*projection mismatch*],      [elab: `Proj.field` not found on target's record type, or target is not a RecValue],
  [*duplicate binder param*],   [parser: two `Param`s share a name in the same `Binder.params`],
  [*import cycle*],             [parser: a `use` transitively references an ancestor file in the parse stack],
  [*braced-form mixing*],       [parser: `:=` with a `NAME : EXPR` field, intermediate bare expressions, `->` on an incompatible shape],
)

== Recursive-`let` hint

The parser tracks a "currently-being-parsed" set of names (pushed on
entering a `let` body, popped on exit). When an unresolved
`Var { name: n }` is produced and `n` is in that set, the error
attaches:

```
note: recursive `let` is not supported. use an explicit fix combinator:
      let n = fix ({n} -> ...)
```

== Multi-error reporting

The parser accumulates errors and continues where possible: a failed
`test` or an unresolved name does not abort parsing of independent
`Def`s. Backend-elab errors inside one `Def` abort that `Def`'s
elaboration, but subsequent `Def`s still attempt to elaborate.
Diverging elaboration (runaway reduction, infinite `pred_of_lvl`) hits
the evaluation budget and raises a single error for that `Def`. All
errors are reported together at the end of the run.

= Emit

The output of compilation is the final name table: each top-level
`Def.name` mapped to its elaborated tree. The driver prints or
serializes this map per the caller's needs. The vitest suite asserts
the map is produced without errors.

There is no runtime test phase: every `test` was either discharged
during parsing or raised as a compile error.

= Reference vs tree-hosted implementation

The host-side implementation (TS parser + TS backend elaborator) is a
*reference*. The long-term target is the elaborator written in disp
itself --- a tree-calculus program consuming a tagged-tree encoding of
the AST and emitting the elaborated tree. Features the TS reference
has that the disp-hosted elaborator need not replicate verbatim:

- *Span tracking* --- the tree elaborator works on span-stripped AST;
  user-facing error display maps positions back via a side table.
- *Multi-error accumulation* --- the tree elaborator may bail at the
  first error.
- *The parser itself* (tokenizer, grammar, `use` resolution, let
  inlining) stays TS-side until the disp self-hosting story matures.

Points of divergence are flagged in the TS source with `TODO(tree-host)`.
