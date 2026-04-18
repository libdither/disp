#set document(title: "Disp Surface Syntax")
#set page(margin: 2cm, numbering: "1")
#set text(font: "New Computer Modern", size: 10.5pt)
#set heading(numbering: "1.")
#show heading.where(level: 1): set text(size: 18pt, weight: "bold")
#show heading.where(level: 2): set text(size: 14pt)
#show heading.where(level: 3): set text(size: 11.5pt, style: "italic")
#show link: set text(fill: rgb("#0b63b0"))

// Production + optional one-line note + disp-source example.
#let rule(name, production, example, note: none) = block(
  breakable: false,
  above: 1em, below: 0.8em,
  {
    grid(
      columns: (auto, 1fr),
      gutter: 8pt,
      text(weight: "bold")[#name],
      raw(production, lang: "ebnf"),
    )
    if note != none {
      block(
        inset: (left: 1.5em, top: 4pt, bottom: 0pt),
        text(style: "italic", fill: rgb("#555555"), size: 9.5pt, note),
      )
    }
    v(-0.2em)
    block(
      inset: (left: 1.5em, y: 4pt),
      raw(example, lang: "disp"),
    )
  }
)

#align(center, text(22pt, weight: "bold")[Disp Surface Syntax])
#v(0.5em)
#align(center)[
  Surface grammar and AST of `.disp` source files.\
  #raw("src/parse.ts") implements the grammar;
  #raw("src/ast.ts") defines the AST;
  #raw("test/parser.test.ts") exercises every production.
]
#v(1em)

= Lexical structure

== Whitespace and comments

Whitespace (spaces, tabs, newlines) separates tokens. Line comments
start with `//` and run to end of line. Block comments `/* ... */` may
span multiple lines and do not nest.

```disp
// line comment
let x = t   // trailing comments work too
/* block
   comment */
```

== Tokens

#table(
  columns: (auto, 1fr, auto),
  align: (left, left, left),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  table.header[*Category*][*Pattern / members*][*Example*],
  [Keyword],     [`let`, `test`, `use`],                  [`let`],
  [Identifier],  [`[A-Za-z_][A-Za-z0-9_']*`, excluding keywords and the bare leaf `t`], [`foo_bar`, `x'`, `_priv`],
  [Leaf],        [`t` (not followed by an identifier char) or `△`],       [`t`, `△`],
  [String],      [`"..."` (no escape sequences; only appears as `use` argument)], [`"lib/foo.disp"`],
  [Punctuation], [`(`  `)`  `{`  `}`  `,`  `.`  `=`  `:`  `:=`  `;`  `->`  `→`], [`{x : A} -> x`],
)

A `t` adjacent to identifier characters is lexed as part of the identifier
(`ty`, `t1` --- only the bare `t` is the leaf).

== Hole marker `_`

`_` is the single-underscore *hole token*. Its meaning is position-sensitive:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Position*][*Meaning*],
  [binder name slot --- `{_ : T} -> e`],  [anonymous binder (equivalent to `T -> e`)],
  [type slot --- `{x : _} -> e`],         [omitted type; elaboration infers it],
  [atom --- `f _`],                       [fresh metavariable],
)

`_foo` remains a regular identifier; only the bare single underscore is a
hole token.

= Grammar

The `expr` grammar covers both term and type positions. A braced `{...}`
form parses into an unresolved member list; the parser commits to one of
four node kinds based on the shape of the members and on whether a `->`
immediately follows the closing brace (see § Braced forms).

== Separators

Two separators are in play, each local to a specific brace shape:

#table(
  columns: (auto, 1fr, auto),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Separator*][*Used in*][*Example*],
  [`,` or NEWLINE],  [RecType / Binder params (type-position lists)],                  [`{x : A, y : B}`],
  [`;` or NEWLINE],  [RecValue fields (value-position lists) and top-level programs], [`{x := t; y := t t}`],
)

Trailing separators are allowed. Mixing `,` and `;` inside the same
brace shape is a parse error.

== Programs and items

#rule(
  "program",
  "program  ::= item (progSep item)* progSep?
progSep  ::= \";\" | NEWLINE
item     ::= let | test | use",
  "let id = {A} -> {x} -> x
test id t = t
use \"lib/predicates.disp\"",
  note: [An ordered sequence of statements evaluated top-to-bottom in one scope. A trailing separator is allowed.],
)

Top-level items are `let`, `test`, `use`. The parser evaluates them in
order:

- a `let` binds a name into the current scope and records a named
  compilation target (an export),
- a `test` becomes a compile-time assertion member of the `Program`,
- a `use` inlines the items of another file into the current scope.

#rule(
  "let",
  "let      ::= \"let\" IDENT (\":\" expr)? \"=\" expr",
  "let K = {x} -> {y} -> x
let id : {A : Type} -> A -> A
       = {A} -> {x} -> x",
  note: [Scoped name binding; visible to subsequent items. With a type ascription, the bound body is wrapped as `Ann(body, T)` — the kernel predicate runs during elaboration.],
)

`let` is parse-time syntactic sugar that resolves into existing AST
constructors --- no `Let` node survives. Type annotations, when
present, are uniformly attached to the value via `Ann(body, T)`;
`Binder.param.type` is reserved for user-written lambda/Pi binders
and is always `null` in let-desugarings.

- *Internal* `let x (: T)? = body; rest` (inside a Block) desugars to
  `App(Binder([{name: "x", type: null}], rest), body')` where `body'`
  is `Ann(body, T)` if a type was given, otherwise just `body`. The
  `rest` is whatever follows the `let` within the Block --- tests and
  the trailing expression are both captured inside the `Binder` body.
- *Top-level* `let x (: T)? = body` becomes a `Def` node in
  `Program.members`, whose `body` is `Ann(body, T)` if a type was
  given, otherwise just `body`.

The parser does not resolve names. `Var` nodes retain their source
strings; the elaborator walks the AST with a scope stack, resolves
each `Var` against in-scope binder params and top-level `Def`s, and
reports unresolved names with their source spans.

Recursive `let` fails naturally at elaboration. `let f = body` desugars
to `App(Binder([{name: "f"}], rest), body)` --- the binder wraps only
`rest`, so any `Var f` inside `body` sees the outer scope. If no outer
`f` exists, the elaborator reports "unresolved `f`." For recursion,
write an explicit fix combinator.

#rule(
  "test",
  "test     ::= \"test\" expr \"=\" expr",
  "test ({x} -> x) t = t",
  note: [Compile-time assertion: both sides must elaborate to hash-cons-equal trees. Failure is a compile error, same channel as a type error.],
)

#rule(
  "use",
  "use      ::= \"use\" STRING",
  "use \"../predicates.disp\"",
  note: [Inlines the items of another file into the current scope. Import cycles are parse-time errors.],
)

== Expressions

#rule(
  "expr",
  "expr     ::= binder | app ((\"->\" | \"→\") expr)?",
  "{x : A} -> x
A -> B
f x",
  note: [An `app` followed by `->` is sugar for a single-param `binder` with an anonymous param: `A -> B` parses to `Binder([{name: null, type: A}], B)`. `->` and `→` are interchangeable.],
)

#rule(
  "binder",
  "binder       ::= binderRecord (\"->\" | \"→\") expr
binderRecord ::= \"{\" binderParam (\",\" binderParam)* \",\"? \"}\"
binderParam  ::= binderName (\":\" expr)?
binderName   ::= IDENT | \"_\"",
  "{x} -> x                       // inferred type
{x : Nat} -> x                 // annotated
{A : Type, x : A} -> x         // multi-param sugar (right-assoc)
{_ : Nat} -> t                 // anonymous (same as Nat -> t)",
  note: [Lambda or Pi depending on the expected type (`Type` ⇒ Pi, else lambda). Surface form is identical; elaboration decides. A binder must have at least one param; `{} -> body` is a parse error.],
)

The multi-param sugar `{p1, p2, ..., pn} -> body` desugars
right-associatively to `{p1} -> {p2} -> ... -> {pn} -> body`, so later
params and the body see earlier params in scope.

#rule(
  "app",
  "app      ::= atom atom*",
  "f x y                          // ((f x) y) -- left-associative
F A B",
  note: [Left-associative juxtaposition; no separator between function and argument.],
)

#rule(
  "atom",
  "atom      ::= simple (\".\" IDENT)*
simple    ::= \"(\" annotated \")\"
            | braced
            | LEAF
            | IDENT
            | \"_\"
annotated ::= expr (\":\" expr)?",
  "(A -> B)
(f x : Nat)                    // parenthesized ascription
{ x := t; y := t t }           // record value atom
point.x                        // projection
point.x.fst                    // chained projection
Type
foo
_",
  note: [Indivisible expression plus optional `.field` projections; binds tighter than application.],
)

`atom` has a postfix `.IDENT` for field projection. It binds tighter
than application: `f.a b` is `(f.a) b`.

Parenthesized forms accept the `annotated` grammar, so `(e : T)` is the
only way to write a type ascription in a non-statement position --- bare
`e : T` outside a `let` or a record field is a parse error.

== Braced forms <braced>

Parsing a `{...}` is a two-step commitment:

1. Parse the members into an unresolved list (each member classified
   syntactically as a typed field `IDENT : EXPR`, a named field
   `IDENT (: T)? := EXPR`, or a bare expression).
2. Peek the next token past `}`. If it is `->` (or `→`), reinterpret as
   a *Binder*; otherwise, commit to one of the three value shapes via
   the member-shape rule below.

The four outcomes are mutually exclusive:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Form*][*Admissible member shapes*],
  [*Binder*],
    [trailing `->` present; every member is a `binderParam` --- a bare IDENT / `_` optionally followed by `: TYPE`. No `:=`, no non-trivial expressions, no statements.],
  [*RecType*],
    [no trailing `->`; one or more typed fields `IDENT : EXPR`. No `:=`, no bare expressions.],
  [*RecValue*],
    [no trailing `->`; one or more named fields `IDENT (: T)? := EXPR`. No typed fields, no bare expressions.],
  [*Block*],
    [no trailing `->`; zero or more `let` / `test` / `use` statements, followed by exactly one trailing bare expression.],
)

Statements (`let`, `test`, `use`) appear only in Blocks and at top
level. RecType and RecValue carry only their respective field kinds.
For local definitions inside a record, wrap in a Block:

```disp
let t = { let Size = Nat; {x : Size, y : Size} }
                         // ^ RecType, built with a local alias
```

Mixing shapes is a parse error with a targeted message:

- any `:=` member mixed with `NAME : EXPR` or a bare expression,
- any bare expression before the final member of a Block,
- trailing `->` on a RecValue, Block, or empty braced form,
- a statement (`let` / `test` / `use`) inside a RecType or RecValue.

A Block whose `members` list is empty (no tests remaining after `let`
desugaring) is collapsed to its trailing expression directly by the
parser --- `Block { members: [], trailing: e }` becomes just `e`.

The empty braced form `{}` is a 0-field RecValue. The elaborator
interprets it as the Church unit value or unit type based on expected
type.

=== Disambiguating `{ IDENT }` and `{ IDENT : EXPR }`

`{ x }` has one bare-IDENT member. Taken alone (no `->` follows), it's a
Block with trailing expression `x`. Followed by `->`, it's a Binder with
one param `x` of inferred type.

`{ x : Nat }` has one typed-field member. Alone, it's a RecType with one
field. Followed by `->`, it's a Binder with one param `x : Nat`.

To write a Block whose trailing expression is ascribed (rare in
practice), parenthesize the ascription: `{ (x : Nat) }`.

=== RecType --- dependent record type

An ordered list of typed fields. Each field's type expression may
mention the names of earlier fields, giving the form the expressive
power of a dependent Σ-product. Duplicate field names are a parse
error.

```disp
{x : Nat, y : Vec Nat x}       // y's type mentions x
{A : Type, a : A}              // dependent pair (Σ-style)
```

=== RecValue --- record value

An ordered list of named fields. Each field may carry an explicit type
ascription. Fields are evaluated in order, and later fields see earlier
ones in scope.

```disp
{ x := t; y := t t }                    // two inferred fields
{ n : Nat := t; fst : Nat := n }        // second field references first
```

If a RecValue declares the same field name more than once, only the
final occurrence is kept; earlier occurrences are shadowed, and the
projection morphism for the name targets the last position.

```disp
let p = { x := t; x := t t }
test p.x = t t                 // last wins
```

=== Block --- statements with a trailing value

Statements followed by a single trailing bare expression. The block's
value *is* the trailing expression. `let`s inside the Block are
desugared at parse time into `App(Binder)` wrappers around the
remaining members (see `COMPILATION.typ` § let-desugaring); `use`s are
inlined; only `test` statements survive as `Block.members`.

```disp
let y = { let a = t
          let b = t t
          f a b }
// desugared: y = App(Binder([{a}], App(Binder([{b}], f a b), t t)), t)
```

=== Projection

`expr.IDENT` on a RecValue resolves at elaboration time. The compiler
looks up `IDENT` in the target's record type and emits the positional
projection morphism. If the target is not a RecValue or lacks the
field, the program is rejected --- there is no runtime fallback.

=== Encoding

For a RecValue with $n ≥ 1$ fields `f1 := e1; ...; fn := en` of
(possibly-dependent) types `X_1, ..., X_n`, the corresponding record
type is the dependent Church product

```disp
{A : Type} -> ({x1 : X_1, ..., xn : X_n} -> A) -> A
```

inhabited by `{A} -> {k} -> k e1 ... en`. Projection `.f_i` compiles
to `{p} -> p ({x_1 : X_1, ..., x_n : X_n} -> x_i)`.

The empty `{}` under the encoding is the Church unit
`{A : Type} -> A -> A`, inhabited by the polymorphic identity.

== Associativity and precedence

- `atom` projection `.`: highest, left-associative.
- `app`: left-associative juxtaposition, looser than `.`.
- The `->` suffix in `expr` and the `binder` form: right-associative.
  `a -> b -> c` parses as `a -> (b -> c)`; `{x} -> {y} -> e` nests the
  inner binder in the outer's body.
- The binder body (after `->`) is a full `expr`, so
  `{x : A} -> A -> B` parses as `{x : A} -> (A -> B)`.

== Module system

`use "path"` loads another `.disp` file, parsing its items as if they
appeared in place of the `use` directive. `path` resolves relative to
the directory of the file containing the `use`. The parser maintains a
stack of in-progress files and rejects import cycles at parse time.

Any braced form introduces a nested scope. `let`s and `use`d imports
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

= Abstract syntax tree

The parser produces an `ast.Program` value (defined in
#raw("src/ast.ts")). Each AST node carries a source `Span` for error
reporting. Spans are not part of the abstract tree: they are stripped
when the AST is serialized to a tree-calculus term for consumption by a
disp-hosted elaborator.

Every surface production maps to exactly one AST node kind. The parser
desugars internal `let`s into `App(Binder)` wrappers and inlines `use`d
files, so the AST presented to the elaborator contains neither `Let`
nor `Use` nodes. `Var` nodes retain their source strings; the
elaborator resolves them against binder params and top-level `Def`s.

```
Expr ::= Var | Leaf | Hole | App | Proj | Binder
       | RecValue | Block | Ann

Var      { name: string }
Leaf     { }
Hole     { }
App      { fn: Expr, arg: Expr }
Proj     { target: Expr, field: string }
Binder   { params: Param[], body: Expr | null }
RecValue { members: NamedField[] }
Block    { members: Stmt[], trailing: Expr }
Ann      { expr: Expr, type: Expr }

Param      { name: string | null, type: Expr | null }
NamedField { name: string, type: Expr | null, value: Expr }

Stmt ::= Test
Test { lhs: Expr, rhs: Expr }

TopLevel ::= Def | Test
Def     { name: string, body: Expr }

Program { members: TopLevel[] }
```

Notes:

- `Binder` is a unified node. `body === null` is the record-type reading
  (a list of typed params with no body, Church-encoded as a product);
  `body !== null` is a lambda or Pi (decided by elab from expected
  type). When `body === null`, every `Param` must have a non-null name
  *and* a non-null type --- record types can't carry holes in either
  slot.
- `A -> B` parses directly to `Binder([{name: null, type: A}], B)`;
  there is no dedicated `Arrow` node.
- Dependent scoping (fields referring to earlier fields, later binder
  params referring to earlier ones) is *not* encoded in AST structure.
  It arises from elaboration walking `members` / `params` in order and
  extending the context; type expressions contain ordinary `Var` nodes
  that resolve via lexical scope.
- Parentheses disappear at parse time. `(e)` produces the same node as
  `e`; `(e : T)` produces `Ann { expr: e, type: T }`.
- `Hole` carries no solver state at parse time. The elaborator tags
  each `Hole` occurrence with a fresh metavariable when it reaches it.
- `Var.name` is a source string. The parser does not resolve names;
  the elaborator walks the AST with a scope stack, resolving each
  `Var` against in-scope binder params and top-level `Def`s. Unresolved
  names are reported with the `Var`'s span.

= Elaboration semantics

#let sem(head, body) = [
  #text(weight: "bold")[#raw(head)] --- #body
]

#sem("Def { name, body }  (top-level)",
  [elaborates `body` and registers `name` as a compilation output. If
   `body` is an `Ann`, the kernel predicate runs against
   `Ann.type` during elaboration; otherwise no type check.])

#sem("Test { lhs, rhs }",
  [elaborates both sides *at compile time*; the program is rejected
   (same channel as a type error) if the resulting trees differ by
   hash-cons identity.])

#sem("Binder { params, body !== null }",
  [lambda or Pi, decided from expected type (`Type` ⇒ Pi, else lambda).
   Multi-param desugars right-associatively; each param with `type = null`
   takes a fresh metavariable.])

#sem("Binder { params, body === null }",
  [a record type. Elaborates to the Church-encoded dependent product
   over `params`.])

#sem("RecValue { members }",
  [elaborates to the record type's constructor applied to the field
   values in source order.])

#sem("Block { members, trailing }",
  [elaborates `trailing` with `members` (tests) run as compile-time
   side effects in source order. Only `Test` statements appear in
   `members` --- `let` was desugared into `App(Binder)` at parse time,
   and `use` was inlined.])

#sem("Ann { expr, type }",
  [type-ascribes `expr`. Guides elaboration; does not change the
   runtime value.])

#sem("Proj { target, field }",
  [projection by field name. Compiles to the positional projection
   morphism for `field`'s source-order index in the target's record
   type.])

#sem("_ (Hole)",
  [introduces a fresh metavariable for elaboration to solve.])

=== Compile-time `test` under a binder

`test` at the top level of a file runs against fully-elaborated
(closed) trees on both sides. A `test` nested inside a Block that is
itself under a binder body (`{x} -> { test f x = g x; x }`) is rejected
at elaboration: the hypothesis `x` has no concrete value, and
hash-cons equality on open terms is a type/equality judgement that
belongs in an `eq` type, not a `test` assertion.

= Compilation

Compilation details --- the parse / elaborate / emit pipeline,
name-resolution algorithm, error-reporting contract, and `let`
desugaring rules --- live in `COMPILATION.typ`. This document covers
only the surface grammar and the AST shape the parser produces.
