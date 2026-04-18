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
  Complementary to #raw("SYNTAX.typ") (surface grammar) and
  the per-backend #raw("lib/*/DESIGN.md") documents.
]
#v(1em)

= Overview

Compilation is three phases with a clean boundary between each. Phase
outputs are pure data structures; no phase shares mutable state with
another.

#table(
  columns: (auto, 1fr, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Phase*][*Input*][*Output*],
  [Parse],     [source text (`.disp` file)],      [`ast.Program` (AST, no tree terms)],
  [Elaborate], [`ast.Program`],                    [`Map<name, Tree>` (bound exports) + assertion results],
  [Emit],      [the elaborated map of trees],      [compiler output; runtime artifact (TBD)],
)

The host-side implementation in #raw("src/parse.ts") and
#raw("src/elaborate.ts") is a *reference implementation*. The long-term
target is an elaborator written in disp itself --- a tree-calculus
program that consumes a tagged-tree encoding of the AST and emits the
elaborated tree. The tree elaborator is a drop-in replacement for the TS
one across the same phase boundary.

= Parse

== Responsibilities

- Tokenize per `SYNTAX.typ` § "Lexical structure."
- Apply the grammar; reject ill-formed input with a span-tagged message.
- Expand `use "..."` imports by recursive parse. Each in-progress file
  is held on a stack so cycles become parse-time errors (see § Import
  cycles).
- Disambiguate braced forms (RecValue / Block / Binder / record-type
  Binder) per `SYNTAX.typ` § "Braced forms."
- Desugar internal `let`s into `App(Binder)` form (see § `let`
  desugaring). The AST presented to elab contains no `Let` or `Use`
  nodes.
- Collapse empty Blocks: `Block { members: [], trailing: e }` becomes
  `e`.
- Reject parse-level shape errors (empty binder `{} -> body`,
  intermediate bare expressions, `:=`/`:` mixing, etc.).

== Non-responsibilities

- *No name resolution.* Every `Var` node keeps its source string. The
  elaborator looks up names against the scope stack and top-level
  `Def` table.
- *No type checking.* Type ascriptions become `Ann` nodes; elab runs
  the kernel predicate.
- *No tree-calculus construction.* The parser's output is AST only.

== Import cycles

The parser maintains a stack of the absolute paths of files currently
being parsed. When a `use "P"` is encountered:

1. Resolve `P` to an absolute path relative to the directory of the
   file containing the `use`.
2. If the resolved path is already on the stack, emit a parse error
   with the full cycle path ("`a.disp → b.disp → a.disp`").
3. Otherwise push, parse recursively, pop.

Parsed-and-popped files may be visited again via independent `use`
chains --- only active in-progress files participate in cycle detection.
A simple memoization of parsed files is an optimization, not a
correctness requirement.

== `let` desugaring

Every `let` in the source is resolved at parse time:

- *Internal* `let x (: T)? = body; rest` (inside a Block, where `rest`
  is all members and the trailing expression after the `let`):
  ```
  App(
    Binder([{name: "x", type: null}], rest_as_block_or_expr),
    body'
  )
  ```
  where `body'` is `Ann(body, T)` if a type was given, else `body`.
  Successive `let`s in the same Block peel off one at a time, producing
  nested `App(Binder, ...)` forms.

- *Top-level* `let x (: T)? = body`:
  `Def { name: "x", body: body' }` where `body'` is `Ann(body, T)` if a
  type was given, else `body`.

Type annotations always travel on the value via `Ann`.
`Binder.param.type` is reserved for user-written lambda/Pi binder
parameters and is always `null` in let-desugarings. Elab sees a
uniform "Ann wraps the body" pattern whether the `let` was internal or
top-level.

== Block simplification

After `let` desugaring, a Block may be left with no statements
(`Test` members gone, `Use` inlined). The parser collapses:

```
Block { members: [], trailing: e }   →   e
```

Blocks that still carry `Test` members are preserved: those tests are
compile-time side effects that execute in elab.

== Parse-time errors

Span-tagged; they carry the file path and offsets of the offending
token range. Classes:

- *Unexpected token / unterminated string / unknown punctuation* ---
  standard lexer/parser errors.
- *Braced-form mixing* --- `:=` with `: ` field, intermediate bare
  expressions, `->` on an incompatible shape.
- *Empty binder* --- `{} -> body` is rejected with "a binder must have
  at least one parameter."
- *Import cycle* --- reported with the full cycle path.
- *Duplicate binder param / RecType field name* --- a `Binder` with two
  `Param`s of the same name is rejected, whether in RecType or lambda/Pi
  mode. Duplicate field names in a RecValue are *not* an error --- they
  shadow (last wins).

The parser does not issue "unresolved name" errors --- that's elab's
job.

= Elaborate

== Scope and name resolution

The elaborator maintains two lookup structures:

- A *top-level name table* mapping each encountered `Def.name` to its
  elaborated tree. Populated in source order as `Program.members` is
  walked.
- A *scope stack* with two kinds of frames:
  - *Binder-param frames* pushed on entry into a `Binder.body`, carrying
    the `Param`s of that binder.
  - *Record-field frames* pushed on entry into a `RecValue`, growing one
    name at a time as elab walks the fields in order (each field's
    `value` expression sees earlier field names in scope).

  Blocks do not push a frame --- after `let`-desugaring, a Block is just
  `Test` members plus a trailing expression, with no new bindings of its
  own.

Resolving `Var { name }` proceeds:

1. Walk the scope stack inner-to-outer; first matching `Param.name`
   (binder-param frame) or field name (record-field frame) wins.
2. If no scope-stack match, consult the top-level name table.
3. If neither matches, emit an "unresolved name" error (see § Error
   reporting).

Shadowing is allowed: an inner binder can reuse a name bound by an
outer scope or by a top-level `Def`.

== `Def` handling

For each `Def { name, body }`:

1. Mark `name` as "currently-being-defined" (for the recursive-let
   hint; see § Error reporting).
2. Elaborate `body` under the current scope. If `body` is
   `Ann(inner, T)`, elaborate `T` first, then elaborate `inner`
   against `T` via the kernel predicate `pred_of_lvl`.
3. On success, add `name → elaborated-tree` to the top-level name
   table.
4. Clear the "currently-being-defined" mark.

Subsequent `Def`s see the new binding. Order within a file matters.

== `Test` handling

For each `Test { lhs, rhs }`:

1. Elaborate `lhs` and `rhs` under the current scope.
2. Compare the resulting trees by `FAST_EQ` (hash-cons identity). If
   equal, the test passes silently; if not, emit a compile error.

Tests inside a `Block` under a binder body are rejected: the binder
params are free hypotheses with no concrete value, and hash-cons
equality over open terms is a type-level judgement, not a runtime
test.

== `Binder` handling

For `Binder { params, body }`:

- If `body === null`: record type. Elab walks `params` in order,
  elaborating each param's type under the scope extended by preceding
  params, then emits the Church-encoded dependent product.
- If `body !== null`: lambda or Pi. Elab decides by expected type
  (`Type` ⇒ Pi, else lambda). Each `Param` with `type === null` takes
  a fresh metavariable. Multi-param is desugared right-associatively
  at elaboration.

== `RecValue` handling

Walk `members` in order, elaborating each field's value under the
scope extended by preceding fields. Emit the Church-encoded product's
constructor applied to the field values.

== `Block` handling

Walk `members` (tests) in order, executing each as a compile-time
assertion. Elaborate `trailing` in the same scope and return it as the
Block's value.

== Metavariables

`Hole` nodes become fresh metavariables when first encountered. The
elaborator solves them via unification during kernel-predicate runs.
Unsolved metas at the end of a `Def`'s elaboration are a compile error
("could not infer value for `_` at span").

= Error reporting

All compile errors carry source spans from the offending AST node. The
elaborator categorizes by kind:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Kind*][*Source*],
  [*unresolved name*],      [`Var` with no matching scope entry or top-level Def],
  [*recursive `let`*],      [`Var` whose name equals a `Def.name` or enclosing `Binder.param` currently being defined; see below],
  [*type mismatch*],        [kernel predicate `pred_of_lvl` returns FF on a typed `Def` body],
  [*failed test*],          [`Test` sides disagree by hash-cons identity],
  [*unsolved metavariable*], [`Hole` without a solution by end of enclosing `Def`],
  [*test under binder*],    [`Test` inside a `Block` under a live binder body],
  [*projection mismatch*],  [`Proj.field` not found on target's record type, or target is not a RecValue],
  [*duplicate binder param*], [two `Param`s in the same `Binder.params` share a name --- rejected in both lambda/Pi and record-type modes at parse time],
)

== Recursive-let hint

When elab encounters an unresolved `Var { name: n }`:

1. If `n` matches a `Def` currently being elaborated --- "in body of
   `let n = ...` at the top level."
2. If `n` matches a `Param` of an enclosing `Binder` whose body is
   currently being elaborated, AND that `Binder` is the direct child of
   an `App` --- "in body of `let n = ...` (desugared to App(Binder))."

In either case, append the hint:

```
note: recursive `let` is not supported. use an explicit fix combinator:
      let n = fix ({n} -> ...)
```

Otherwise emit the plain "unresolved name" error.

The "currently-being-defined" set is threaded through the elaborator as
a stack of names, pushed when elab enters a `Def.body` or
`App(Binder)`-from-let-desugaring, popped on exit.

== Multi-error reporting

The elaborator collects errors and continues where possible. Name
resolution errors do not abort elaboration of independent `Def`s ---
later tests and defs still elaborate, and all errors are reported
together at the end of the run.

Fatal-ish exceptions: a `Def` whose elaboration diverges (runaway
reduction, infinite loop in `pred_of_lvl`) hits the evaluation budget
and raises a single error; subsequent `Def`s still elaborate.

= Emit

The elaborated map of `Def.name → Tree` is the output. The current
driver (#raw("src/run.ts"), to be renamed) either (a) pretty-prints
the bindings, (b) writes them to a serialized tree format for
downstream tools, or (c) both. The vitest suite asserts the bindings
elaborate without errors.

There is no runtime test phase: every `Test` was either discharged
during elaboration or raised as a compile error.

= Reference vs tree-hosted implementation

The host-side elaborator has features that the eventual disp-hosted
elaborator does not (yet) need to replicate verbatim:

- Span tracking --- the tree elaborator consumes a tagged-tree AST
  without spans; it reports errors by AST structural position, not
  source offsets. A separate span-lookup side table maps tree-AST
  positions back to source for user display.
- Multi-error collection --- the tree elaborator may bail at the first
  error; batch-error recovery is a TS-only pragmatism.
- The recursive-let hint, metavar solver, and import resolver are all
  implementable as pure tree-calculus programs; they are separable
  modules.

When a feature of the reference implementation diverges from what the
tree elaborator can express, it's marked with a `TODO(tree-host)` in
the TS source.
