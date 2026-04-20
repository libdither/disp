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
  the per-backend #raw("lib/*/DESIGN.md") documents.
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

The host-side parser in #raw("src/parse.ts") and the backend elaborator
communicate through a narrow `(e: Expr, expected?: Type) => Tree`
interface. Each NbE backend in `lib/{debruijn,ctxtree,semantic}/` plugs
in behind this interface --- the parser doesn't care which.

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

== Contract

```
elab(e: Expr, expected?: Type): Tree
```

- *Input* — an expression with no `Block`, `Let`, `Test`, `Use`,
  `Def`, or `Program` nodes. Every `Var` refers to a binder param in
  scope within the passed expression.
- *Optional expected type* — guides Pi-vs-lambda disambiguation on
  `Binder` and `Ann` checking.
- *Output* — a tree-calculus term (TS reference) or a
  backend-specific value (`debruijn` / `ctxtree` / `semantic`).
- *Side effects* — none on caller-visible state. The elaborator may
  mutate an internal metavariable solver, but the return value is a
  function of inputs plus unification results.

== Per-node handling

- `Leaf` --- returns the tree-calculus leaf.
- `Var { name }` --- walks the surrounding `Binder`s inner-to-outer;
  the innermost match wins. Backend-dependent representation
  (de Bruijn index, bind-tree slot, etc.). An unmatched name is an
  internal error (should have been caught by the parser).
- `App { fn, arg }` --- elab both, return `fn(arg)` in the backend.
- `Binder { params, body !== null }` --- lambda or Pi, decided from
  expected type (`Type` ⇒ Pi, else lambda). Each `Param` with
  `type === null` takes a fresh metavariable. Multi-param desugars
  right-associatively.
- `Binder { params, body === null }` --- record type. Church-encoded
  dependent product over `params` (see § Record encoding).
- `RecValue { members }` --- record type's constructor applied to the
  field values in source order.
- `Proj { target, field }` --- elab `target`, consult its record type,
  emit the positional projection morphism.
- `Ann { expr, type }` --- elab `type` against `Type`, then elab
  `expr` against `type` via the kernel predicate `pred_of_lvl`.
- `Hole` --- fresh metavariable. Solved by unification during kernel
  runs. An unsolved metavariable at the end of a top-level elab
  invocation is a compile error.

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
  [*type mismatch*],            [elab: `pred_of_lvl` returns FF on a typed `Def` body or `Ann`],
  [*failed test*],              [parser: `test`'s two sides elaborate to trees that disagree by hash-cons identity],
  [*unsolved metavariable*],    [elab: `Hole` without a solution by end of top-level elab invocation],
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
