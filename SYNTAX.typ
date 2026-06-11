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
  #raw("src/parse.ts") implements the grammar and defines the AST;
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
  [Keyword],     [`let`, `test`, `use`, `open`, `match`, `if`, `then`, `else`],     [`let`],
  [Identifier],  [`[A-Za-z_][A-Za-z0-9_']*`, excluding keywords and the bare leaf `t`], [`foo_bar`, `x'`, `_priv`],
  [Leaf],        [`t` (not followed by an identifier char) or `△`],       [`t`, `△`],
  [String],      [`"..."` (no escape sequences). A `use` argument, or a term: a string literal is the `List` of its codepoint `Nat`s (so `"A"` ≡ `[65]`), giving a deterministic, distinct tree per spelling — used as record/coproduct field-name tags.], [`"lib/foo.disp"`, `"respond"`],
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

== Programs, record bodies, and items

A file body and an inline `{ ... }` record value share the same grammar:
an ordered sequence of *members* separated by `SEMI`. Members come in
two flavours --- *fields* (`name := expr`) that are exported, and
*statements* (`let`, `test`, `open`) that are local side-effects.

#rule(
  "program",
  "program  ::= recBody",
  "open use \"lib/prelude.disp\"
id := {A} -> {x} -> x
test id t = t",
  note: [A file body is a `recBody`. When loaded via `use`, the file's exported fields become the record's fields. `let` bindings are private; `test` and `open` are side-effects.],
)

#rule(
  "recBody",
  "recBody  ::= (recMember SEMI)* recMember? SEMI?
recMember ::= field | let | test | \"open\" expr",
  "let helper = {x} -> x x    // private
add := fix ({self, n, m} -> ...) // exported
test add 2 3 = 5              // assertion
open use \"prelude.disp\"     // import",
  note: [Shared by file bodies and inline `{ ... }` record values. Members are processed in order; later members can reference earlier ones.],
)

#rule(
  "field",
  "field    ::= IDENT (\":\" expr)? \":=\" expr",
  "TT := t
id : {A : Type} -> A -> A
   := {A} -> {x} -> x",
  note: [Exported record member. Visible as a field when the enclosing body is loaded via `use` or accessed via projection. Duplicate exported field names are parse errors.],
)

#rule(
  "let",
  "let      ::= \"let\" IDENT (\":\" expr)? \"=\" expr",
  "let K = {x} -> {y} -> x
let id : {A : Type} -> A -> A
       = {A} -> {x} -> x",
  note: [Private name binding; visible to subsequent members but NOT exported. Within a block expression, desugars to `App(Binder, body)`.],
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
  note: [Lambda or Pi depending on expected type (`Type` ⇒ Pi, else lambda). A binder must have at least one param; `{} ARROW body` is a parse error. Multi-param `{p1, ..., pn} -> body` desugars right-associatively to `{p1} -> ... -> {pn} -> body`. Example: `{A : Type, x : A} -> x` means `{A : Type} -> ({x : A} -> x)`, not `({A : Type, x : A}) -> x` — each additional param wraps the remainder on the right.],
)

#rule(
  "app",
  "app      ::= atom atom*",
  "f x y                          // ((f x) y) -- left-associative
F A B",
  note: [Left-associative juxtaposition; no separator between function and argument. After crossing a newline, `IDENT \":=\"` is _not_ consumed as an atom (it starts a field definition).],
)

#rule(
  "atom",
  "atom      ::= simple (\".\" IDENT)*
simple    ::= \"(\" expr (\":\" expr)? \")\"
            | match
            | if
            | braced
            | \"use\" STRING
            | STRING
            | LEAF
            | NUM
            | IDENT
            | \"_\"
match     ::= \"match\" app \"{\" matchArm (SEMI matchArm)* SEMI? \"}\"
matchArm  ::= IDENT IDENT* \"=>\" matchExpr   // Ctor + zero or more binders (\"_\" discards)
matchExpr ::= expr that spans newlines freely but stops before the next arm (\"Ctor binder* =>\")
if        ::= \"if\" app \"then\" ifBody \"else\" ifBody   // boolean conditional -> `cond`
ifBody    ::= matchExpr (multi-line) | newline-terminated expr (line mode)
braced    ::= recValue | recType | block
recValue  ::= \"{\" \"}\"
            | \"{\" recBody \"}\"
recType   ::= \"{\" typedField (COMMA typedField)* COMMA? \"}\"
block     ::= \"{\" (stmt SEMI)* expr SEMI? \"}\"
typedField ::= IDENT (\":\" expr)? (\":=\" expr)?
stmt       ::= let | test | \"open\" expr",
  "(f x : Nat)                    // parenthesized ascription
{ x := t; y := t t }           // recValue (2 exported fields)
{ let h = t; x := h }          // recValue (1 exported field, 1 private let)
{ x : A, y : B }               // recType
{ let a = t; f a }             // block (no fields, trailing expr)
use \"../prelude.disp\"        // loads file, yields the module tuple { record, typ }
point.x.fst                    // chained projection",
  note: [`atom`'s postfix `.IDENT` binds tighter than application: `f.a b` is `(f.a) b`. `(e : T)` is the only way to ascribe outside a `let` or record field. `use STRING` loads and elaborates the referenced file and yields a *module tuple* `{ record, typ }`: `record` is the §2.6 product of the file's exported values (keyed by name), and `typ` is `Record [(name, declaredType)…]` over the *annotated* exports — so a file is verified by ordinary application, `(use f).typ (use f).record = Ok TT` (gradual: unannotated exports are absent from `typ`). `open use f` splices the *values* into scope (the export metadata), independent of this value. (When the cut/`Record` formers aren't in scope, `use` falls back to the bare value record.) `if c then a else b` is the boolean conditional: it desugars to the prelude `cond` (`cond c a b`, a select-then-apply over the Scott Bool, motive `t`), with each branch closed over the free vars the two share so only the taken branch is forced (and recursive bodies dodge the eager compile-time K-reduction). `then`/`else` (keywords) bound the first two parts; the else body's tail is mode-sensitive (multi-line `matchExpr` vs newline-terminated). `else if …` chains right-associatively. `match` is now exclusively the *coproduct* cut: arms `Ctor binder* => body` where the constructor name is the tag *by spelling* (a string), desugaring to the §2.6 cut `(prod (pair [\"Ctor\"…] [handlers…])) c` (needs `prod` in scope). (The old boolean `match { TT/FF }` surface was removed.) A `_` constructor is the wildcard/default arm (its handler is appended past the names, so an unmatched tag falls to it). Multiple binders destructure a right-nested-pair payload (`Ctor a b c` ⇔ `inj \"Ctor\" (pair a (pair b c))`). Each arm body is `matchExpr`, which can span multiple lines — it stops before the next arm pattern.],
)

The `braced` alternatives are distinguished by member shape. A braced
body that contains any `name := expr` field is a `recValue`; one with
only `let`/`test`/`open` statements and a trailing expression is a
`block`; one with only `let`/`test`/`open` statements and no trailing
expression is an empty `recValue`. A `braced` followed by `ARROW` is
reparsed as a `binder`: `{x : A}` alone is a recType;
`{x : A} -> e` is a binder. The empty `{}` is a 0-field recValue
(Church unit; see `COMPILATION.typ` § Record encoding). Duplicate
field names in recValues and recTypes are rejected.

Three telescope-era refinements. (1) *Field puns*: inside a recValue, a
bare `IDENT` member is shorthand for `name := name`, the value resolving
in the *outer* scope (a field's value always compiles before its own
name binds). A pun needs at least one sibling `:=` field — a field-less
`{ x }` stays a bare recType / block trailing expression. (2) *Sequential
field scope*: later recValue fields see earlier ones by name
(`{ a := 2; b := double a }`), with the telescope discipline — a field's
own RHS resolves outward, so `respond := respond` refers to the enclosing
binding. (3) *recType = telescope type*: a recType in expression position
compiles to `Telescope ⟦entries⟧` (the dependent n-ary record former);
later field types may reference earlier field *names*
(`{ T : Type, x : T }`), and a `name := e` / `name : T := e` member is a
*derived* entry — the field is definitionally pinned to recipe `e` over
the prior fields (checked by `tree_eq`, filled by `mk`). A braced body
parseable as a multi-param `binder` (all members `name : T`, followed by
`ARROW`) still reparses as the *curried* binder; record-domain functions
are written `Arrow { a : Nat, b : Nat } R`.

== Associativity and precedence

- `atom` projection `.`: highest, left-associative.
- `app`: left-associative juxtaposition, looser than `.`.
- The `->` suffix in `expr` and the `binder` form: right-associative.
  `a -> b -> c` parses as `a -> (b -> c)`.
- The binder body (after `->`) is a full `expr`, so
  `{x : A} -> A -> B` parses as `{x : A} -> (A -> B)`.

= Abstract syntax tree

The parser produces a single AST, defined in #raw("src/parse.ts").

```
Expr ::= Var | Leaf | Num | Hole | App | Proj | Binder | RecType | RecValue | Ann | Use | Match

Var      { name: string }
Leaf     { }
Num      { value: number }
Hole     { }
App      { f: Expr, x: Expr }
Proj     { target: Expr, field: string }
Binder   { params: Param[], body: Expr }
RecType  { fields: TypedField[] }
RecValue { fields: NamedField[], members?: RecMember[] }
Ann      { expr: Expr, type: Expr }
Use      { path: string }
Match    { cond: Expr, thenBody: Expr, elseBody: Expr }

Param      { name: string | null, type: Expr | null }
TypedField { name: string, type: Expr | null, value?: Expr | null }  -- value ⇒ derived entry
NamedField { name: string, type: Expr | null, value: Expr }

RecMember ::= Field | Let | Test | Open
Field  { name: string, type: Expr | null, value: Expr }  -- exported
Let    { name: string, type: Expr | null, body: Expr }    -- private
Test   { lhs: Expr, rhs: Expr }                           -- assertion
Open   { expr: Expr }                                     -- import

Program  { members: RecMember[] }
```

== Shape rules

- `RecType` is a separate node. A binder always has a non-null `body`;
  params may have null name (`_`) or null type (inferred).
- `A -> B` parses to `Binder([{name: null, type: A}], B)`; there is no
  dedicated `Arrow` node.
- `(e)` produces the same node as `e`; `(e : T)` produces
  `Ann { expr: e, type: T }`.
- Dependent scoping (later params / fields referring to earlier ones) is
  *not* encoded in AST structure. It arises from compilation walking
  `params` / `members` in order and extending the context.
- `Var.name` references either a compiled scope entry or a binder
  variable to be abstracted. Name resolution happens in `src/compile.ts`.

== Export semantics

A `recBody` (file or inline `{ ... }`) distinguishes *exported* and
*private* members:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Member kind*][*Visibility*],
  [`name := expr`],       [*Exported* --- becomes a field of the enclosing record],
  [`let name = expr`],    [*Private* --- in scope for subsequent members, not exported],
  [`test lhs = rhs`],     [Side-effect: assertion discharged at compile time],
  [`open expr`],          [Side-effect: brings record fields into scope as private bindings],
)

When a file is loaded via `use`, files with at least one `field` member
export only those fields. `let` bindings, `test` assertions, and names
imported via `open` are local to the file. A legacy compatibility mode
remains for fieldless shim files: if a file has no `:=` fields, its
top-level lets and opened names are re-exported. New library files
should use explicit field exports.

== What the elaborator sees

The compiler driver walks the surface AST top-to-bottom and calls the
backend elaborator on each complete expression. By the time the
elaborator sees any sub-expression, the statement-kind surface
nodes (`Let`, `Test`, `Open`, `Field`, `Program`) have been processed
away, and every `Use` has been replaced by the `RecValue` it yielded.
The elaborator consumes only the expression kinds
`Var | Leaf | Num | Hole | App | Proj | Binder | RecType | RecValue | Ann | Match`.

Each non-core node's parser-time behavior, in one line:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Node*][*Parser-time behavior*],
  [`Field { name, type, value }`],
    [compiles `value`, binds `name` in scope, and records as an exported `Def` in the output.],
  [`Let { name, type, body }`],
    [binds `name` in the surrounding scope; body is rewritten as `Ann(body, type)` if typed. Within a Block, produces the equivalent shape `App(Binder([{name}], rest), body')`. Not exported.],
  [`Use { path }`],
    [recursively parses and elaborates the referenced file (firing its tests as a side effect), then replaces itself with a `RecValue` whose fields are the file's exported `field` members. Cycles are parse-time errors.],
  [`Test { lhs, rhs }`],
    [calls the elaborator on `lhs` and `rhs` in the current scope, then asserts hash-cons equality of the resulting trees. Fails as a compile error.],
  [`Open { expr }`],
    [resolves `expr` as a record and brings each field into scope. It is private in field-export files and re-exported only in legacy fieldless files.],
  [`Block { members, trailing }`],
    [processes `members` in order (desugaring lets, discharging tests), leaving the Block reduced to `trailing`. Collapses to `trailing` when `members` is empty.],
  [`Program { members }`],
    [the root node. The driver iterates `members` in order, emitting each exported `Field`'s tree and discharging each `Test`.],
)

See `COMPILATION.typ` for the full parse / elaborate / emit pipeline,
name-resolution algorithm, `let` desugaring rules, record encoding, and
error-reporting contract.
