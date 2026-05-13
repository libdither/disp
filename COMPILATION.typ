// Reflects codebase as of fb5e4f056940 (2026-05-12)
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

Compilation is a *single driver walk*: `src/parse.ts` tokenizes and
builds the surface AST, while `src/compile.ts` walks top-level items,
maintains scope, resolves `use` files, elaborates expressions, and
emits definitions/tests.

The elaborator is type-directed where annotations are present. It
compiles expressions to tree-calculus terms via bracket abstraction,
uses ordinary scope entries for helper discovery (`Pi`, `Type`, `Hyp`,
etc.), and invokes kernel predicates for typed bindings and
annotations.

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Responsibility*][*Owner*],
  [tokenize, grammar, braced-form disambiguation],       [parser],
  [loading files for `use` expressions; cycle detection], [parser],
  [`let` / `field` / `open` scope],                      [compiler driver],
  [compile-time `test` evaluation + comparison],         [compiler driver],
  [converting expressions to backend terms],             [elaborator],
  [typed binding checks],                                [elaborator + kernel predicates],
)

There is no `trust` keyword or provenance side table. Elaborator
privilege is type-based: names like `Pi`, `Type`, and `Hyp` are found
in the ordinary scope after imports, and typed terms are accepted when
the relevant type predicate evaluates to `TT`.

= Parser

== Responsibilities

- Tokenize and apply the grammar per `SYNTAX.typ`.
- Produce `Item` / `Expr` AST nodes for `let`, `field`, `test`,
  `open`, `use`, `match`, binders, records, and applications.
- Load files for `use` expressions, holding a cycle-detection stack
  (see § Import cycles).
- Disambiguate braced forms (`recValue`, `recType`, `block`,
  `binder`) using local syntax shape.
- Reject shape errors (empty binder `{} -> body`, `:=`/`:` mixing,
  duplicate exported fields, duplicate record-type fields,
  intermediate bare expressions in a Block,
  etc.).

Non-responsibilities: the parser does *no* tree construction and *no*
type checking. Name resolution, file loading, projection metadata,
and annotations are handled by `src/compile.ts`.

== Scope

Every file and inline record/block introduces a scope frame. `let`s
inside braces bind only within those braces. Name lookup walks scope
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

Every `let` is compiled in source order. A typed `let` first compiles
the annotation as a type, then checks the body against that type.

*Internal* `let x (: T)? = body; rest` (inside a Block): compile
`body`, bind `x` in the local scope, then compile `rest` with `x`
available. Equivalently:

```
App(Binder([{name: "x", type: null}], rest_as_expr), body')
```

where `body'` is checked against `T` if typed else `body`.

*Top-level* `let x (: T)? = body`: compile and bind `x`. In a
field-export file, top-level lets are private. In legacy fieldless
files, top-level lets are exported for compatibility.

Recursive `let` is not implicit. The new binding takes effect only
after its body compiles; recursive definitions use `fix` explicitly.

== `use` expression

`use "path"` is an atom (see `SYNTAX.typ`) that elaborates by loading
and running the referenced file. Steps:

1. Resolve `path` to an absolute path relative to the directory of the
   file containing the `use`.
2. If the resolved path is on the in-progress stack, emit an import
   cycle error with the full cycle (`a.disp → b.disp → a.disp`).
3. Otherwise push, parse the file to completion (discharging its tests
   and building its `Program`), pop.
4. Return a record value whose fields are the file's exported
   `:=` fields, or all definitions for legacy fieldless files.

Parsed-and-popped files may be revisited via independent `use` chains;
memoizing them is an optimization, not a correctness requirement.

== `test` discharge

`test lhs = rhs` parses both sides, invokes the elaborator on each
(inheriting the surrounding scope), and compares the resulting trees
by hash-cons identity. Hash-cons-equal ⇒ discharged; otherwise a
compile error. The program driver emits compiled test declarations for
the runner.

Tests compare the compiled left and right trees by hash-cons identity.
They are intended for closed compile-time assertions.

== Block simplification

Blocks are surface syntax only. The parser lowers block `let`s into
nested binder applications; tests are discharged by the driver.

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

The elaborator also tries to verify that an annotation is itself a
type by checking it against the canonical `Type` when the kernel
helpers are available. Today `Type` is structural: it recognizes
guarded `predicate_frame` values. A future implementation should make
`Type` more rigorous about validating metadata, codomain functions,
and library type-former laws; for now this is documented future work.

== Kernel Helper Discovery

Certain kernel/library definitions must be in ordinary scope for the
elaborator to construct Pi types, mint hypotheses, and check
annotations. They are imported like any other definitions, typically
through:

```disp
open use "../kernel/prelude.disp"
```

The minimal helper set for typed compilation is:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Name*][*Used for*],
  [`Pi`],    [type-mode binder compilation: `{x : A} -> B` → `Pi(A, codFn)`],
  [`Arrow`], [type-mode arrow sugar: `A -> B` → `Arrow(A, B)` (optional; can use `Pi` + K)],
  [`Type`],  [universe checking: verify `T` is a valid type],
)

No provenance bit is attached to these names. The security model is
type-based: a definition is usable if it is in scope and the relevant
kernel predicates accept the terms being checked. If `Pi` or `Type`
is absent, the compiler falls back to untyped compilation for the
affected expression.

== Interaction with existing compilation

The elaborator extends the existing `compileExpr` / `exprToCir`
pipeline, not replaces it. The changes are:

1. `checkAsType` compiles type expressions and turns binders into
   `Pi(domain, codFn)` when `Pi` is in scope.
2. The `binder` case in term checking compiles lambdas and checks
   bodies against Pi codomains when the expected type is Pi-shaped.
3. The `ann` case compiles both sides and runs the check instead of
   merely erasing the annotation.
4. The driver's `runItem` for typed `let`/field items compiles the
   type and checks the body before binding the name.

`RecType` is currently compile-time metadata for projection/open
support, not a first-class dependent record type checker.

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

Duplicate field names in a RecValue or RecType are parse errors.
This keeps projection metadata unambiguous; there is no first-wins or
last-wins shadowing rule for exported fields.

= Error reporting

Current compile errors carry parser/driver messages, but source-span
plumbing is future work.

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Kind*][*Source*],
  [*unresolved name*],         [compiler: a `Var` whose name matches neither a scope entry nor a binder param],
  [*type mismatch*],            [elab: type predicate returns FF on a typed `let` body or `Ann`],
  [*failed test*],              [parser: `test`'s two sides elaborate to trees that disagree by hash-cons identity],
  [*unsolved hole*],            [elab: `Hole` not yet supported; compile error],
  [*projection mismatch*],      [elab: `Proj.field` not found on target's record type, or target is not a RecValue],
  [*duplicate field*],          [parser: two exported fields or record-type fields share a name],
  [*import cycle*],             [parser: a `use` transitively references an ancestor file in the parse stack],
  [*braced-form mixing*],       [parser: `:=` with a `NAME : EXPR` field, intermediate bare expressions, `->` on an incompatible shape],
)

== Future diagnostics

Better diagnostics should add source spans, recursive-let hints, and
multi-error accumulation. The current implementation fails fast on the
first parse/elaboration error for a run.

= Emit

The output of compilation is a list of declarations: exported
definitions with compiled trees and tests with compiled left/right
trees. The runner executes tests by hash-cons identity.

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
- *The parser and driver* (tokenizer, grammar, `use` resolution, scope
  management) stay TS-side until the disp self-hosting story matures.

Points of divergence are flagged in the TS source with `TODO(tree-host)`.
