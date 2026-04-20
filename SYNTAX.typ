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
  elaboration and desugaring live in #raw("COMPILATION.typ").
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

The `expr` grammar covers both term and type positions. Three separator
shorthands are used below:

```ebnf
COMMA ::= "," | NEWLINE
SEMI  ::= ";" | NEWLINE
ARROW ::= "->" | "→"
```

Trailing separators are always allowed.

== Programs and items

#rule(
  "program",
  "program  ::= item (SEMI item)* SEMI?
item     ::= let | test",
  "let prelude = use \"lib/predicates.disp\"
let id = {A} -> {x} -> x
test id t = t",
  note: [An ordered sequence of statements in one scope.],
)

#rule(
  "let",
  "let      ::= \"let\" IDENT (\":\" expr)? \"=\" expr",
  "let K = {x} -> {y} -> x
let id : {A : Type} -> A -> A
       = {A} -> {x} -> x",
  note: [Scoped name binding; visible to subsequent items.],
)

#rule(
  "test",
  "test     ::= \"test\" expr \"=\" expr",
  "test ({x} -> x) t = t",
  note: [Compile-time assertion: both sides must elaborate to hash-cons-equal trees.],
)

== Expressions

#rule(
  "expr",
  "expr     ::= binder | app (ARROW expr)?",
  "{x : A} -> x
A -> B
f x",
  note: [An `app` followed by `ARROW` is sugar for a single-param `binder` with an anonymous param: `A -> B` parses to `Binder([{name: null, type: A}], B)`.],
)

#rule(
  "binder",
  "binder      ::= \"{\" binderParam (COMMA binderParam)* COMMA? \"}\" ARROW expr
binderParam ::= (IDENT | \"_\") (\":\" expr)?",
  "{x} -> x                       // inferred type
{x : Nat} -> x                 // annotated
{A : Type, x : A} -> x         // multi-param sugar (right-assoc)
{_ : Nat} -> t                 // anonymous (same as Nat -> t)",
  note: [Lambda or Pi depending on expected type (`Type` ⇒ Pi, else lambda). A binder must have at least one param; `{} ARROW body` is a parse error. Multi-param `{p1, ..., pn} -> body` desugars right-associatively to `{p1} -> ... -> {pn} -> body`.],
)

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
simple    ::= \"(\" expr (\":\" expr)? \")\"
            | braced
            | \"use\" STRING
            | LEAF
            | IDENT
            | \"_\"
braced    ::= recValue | recType | block
recValue  ::= \"{\" \"}\"
            | \"{\" namedField (SEMI namedField)* SEMI? \"}\"
recType   ::= \"{\" typedField (COMMA typedField)* COMMA? \"}\"
block     ::= \"{\" (stmt SEMI)* expr SEMI? \"}\"
typedField ::= IDENT \":\" expr
namedField ::= IDENT (\":\" expr)? \":=\" expr
stmt       ::= let | test",
  "(f x : Nat)                    // parenthesized ascription
{ x := t; y := t t }           // recValue
{ x : A, y : B }               // recType
{ let a = t; f a }             // block
use \"../predicates.disp\"     // loads file, yields a recValue of its defs
point.x.fst                    // chained projection",
  note: [`atom`'s postfix `.IDENT` binds tighter than application: `f.a b` is `(f.a) b`. `(e : T)` is the only way to ascribe outside a `let` or record field. `use STRING` is an expression that loads and elaborates the referenced file and yields a recValue whose fields are its top-level `let`-bound names.],
)

The `braced` alternatives are disjoint by member shape (typed field
vs. named field vs. statement-plus-expr). A `braced` followed by
`ARROW` is reparsed as a `binder`: `{x : A}` alone is a recType;
`{x : A} -> e` is a binder. The empty `{}` is a 0-field recValue
(Church unit; see `COMPILATION.typ` § Record encoding).

== Associativity and precedence

- `atom` projection `.`: highest, left-associative.
- `app`: left-associative juxtaposition, looser than `.`.
- The `->` suffix in `expr` and the `binder` form: right-associative.
  `a -> b -> c` parses as `a -> (b -> c)`.
- The binder body (after `->`) is a full `expr`, so
  `{x : A} -> A -> B` parses as `{x : A} -> (A -> B)`.

= Abstract syntax tree

The parser produces a single AST, defined in #raw("src/ast.ts"). Every
node carries a source `Span` for error reporting.

```
Expr ::= Var | Leaf | Hole | App | Proj | Binder | RecValue | Ann | Use | Block

Var      { name: string }
Leaf     { }
Hole     { }
App      { fn: Expr, arg: Expr }
Proj     { target: Expr, field: string }
Binder   { params: Param[], body: Expr | null }
RecValue { members: NamedField[] }
Ann      { expr: Expr, type: Expr }
Use      { path: string }
Block    { members: Stmt[], trailing: Expr }

Param      { name: string | null, type: Expr | null }
NamedField { name: string, type: Expr | null, value: Expr }

Stmt ::= Let | Test
Let  { name: string, type: Expr | null, body: Expr }
Test { lhs: Expr, rhs: Expr }

Program  { members: TopLevel[] }
TopLevel ::= Def | Test
Def      { name: string, body: Expr }
```

== Shape rules

- `Binder.body === null` is a record type --- every `Param` must have
  non-null `name` and `type`. `body !== null` is a lambda or Pi; params
  may have null name (`_`) or null type (inferred).
- `A -> B` parses to `Binder([{name: null, type: A}], B)`; there is no
  dedicated `Arrow` node.
- `(e)` produces the same node as `e`; `(e : T)` produces
  `Ann { expr: e, type: T }`.
- Dependent scoping (later params / fields referring to earlier ones) is
  *not* encoded in AST structure. It arises from elaboration walking
  `params` / `members` in order and extending the context; type
  expressions contain ordinary `Var` nodes that resolve via lexical
  scope.
- `Var.name` references any binder in scope at this position. The
  parser does not resolve binder-param references; that's the
  elaborator's job. The parser does inline `let`-bound names, so no
  surviving `Var` references a `let`.

== What the elaborator sees

The parser is the driver: it walks the surface AST top-to-bottom and
calls the backend elaborator on each complete expression. By the time
the elaborator sees any sub-expression, the statement-kind surface
nodes (`Let`, `Test`, `Block`, `Def`, `Program`) have been processed
away, and every `Use` has been replaced by the `RecValue` it yielded.
The elaborator consumes only the expression kinds
`Var | Leaf | Hole | App | Proj | Binder | RecValue | Ann`.

Each non-core node's parser-time behavior, in one line:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Node*][*Parser-time behavior*],
  [`Let { name, type, body }`],
    [binds `name` in the surrounding scope; body is rewritten as `Ann(body, type)` if typed. Within a Block, produces the equivalent shape `App(Binder([{name}], rest), body')`.],
  [`Use { path }`],
    [recursively parses and elaborates the referenced file (firing its tests as a side effect), then replaces itself with a `RecValue` whose fields are the file's top-level `let`-bound names. Cycles are parse-time errors.],
  [`Test { lhs, rhs }`],
    [calls the elaborator on `lhs` and `rhs` in the current scope, then asserts hash-cons equality of the resulting trees. Fails as a compile error.],
  [`Block { members, trailing }`],
    [processes `members` in order (desugaring lets, discharging tests), leaving the Block reduced to `trailing`. Collapses to `trailing` when `members` is empty.],
  [`Def { name, body }`],
    [top-level form of a `let`; calls the elaborator on `body` and records the resulting tree in the final name table keyed by `name`.],
  [`Program { members }`],
    [the root node. The driver iterates `members` in order, emitting each `Def`'s tree and discharging each `Test`.],
)

See `COMPILATION.typ` for the full parse / elaborate / emit pipeline,
name-resolution algorithm, `let` desugaring rules, record encoding, and
error-reporting contract.
