#set document(title: "Disp Surface Syntax")
#set page(margin: 2cm, numbering: "1")
#set text(font: "New Computer Modern", size: 10.5pt)
#set heading(numbering: "1.")
#show heading.where(level: 1): set text(size: 18pt, weight: "bold")
#show heading.where(level: 2): set text(size: 14pt)
#show heading.where(level: 3): set text(size: 11.5pt, style: "italic")
#show link: set text(fill: rgb("#0b63b0"))

// A production paired with a one-line semantic note and a disp-source
// example. Production on top, note in italic gray below, then example.
// The full formal semantics live in their own section at the end.
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
  Formal grammar of `.disp` source files.\
  The parser in #raw("src/parse.ts") implements this spec;
  #raw("test/parser.test.ts") exercises every production.
]
#v(1em)

#block(
  fill: rgb("#fff8e1"),
  stroke: (left: 3pt + rgb("#f0ad4e")),
  inset: (x: 10pt, y: 8pt),
  radius: 2pt,
)[
  *Status: proposal under review.* This revision unifies the typed and
  untyped term grammars into a single #raw("expr") form, replaces
  #raw("\\(x : T). body") / #raw("(x : T) -> B") with a braced-binder
  #raw("{x : T} -> body"), renames #raw("def") to #raw("let"), and
  collapses the old "block" and "record" ideas into one #raw("{...}") form
  --- every brace-delimited construct is a record, with the #raw("-> e")
  suffix turning it into a binder (Pi or $lambda$).
]

= Lexical structure

== Whitespace and comments

Whitespace (spaces, tabs, newlines) separates tokens and is otherwise ignored.
Line comments start with `;` and run to end of line.

```disp
; this is a comment
let x = t   ; trailing comments work too
```

== Tokens

#table(
  columns: (auto, 1fr, auto),
  align: (left, left, left),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  table.header[*Category*][*Pattern / members*][*Example*],
  [Keyword],     [`let`, `test`, `elab`, `raw`, `use`],                  [`let`],
  [Identifier],  [`[A-Za-z_][A-Za-z0-9_']*`, excluding keywords and the standalone leaf `t`], [`foo_bar`, `x'`, `_priv`],
  [Leaf],        [`t` (not followed by an identifier char) or `△`],       [`t`, `△`],
  [String],      [`"..."` (no escape sequences; only used as the `use` argument)], [`"lib/foo.disp"`],
  [Punctuation], [`(`  `)`  `{`  `}`  `,`  `.`  `=`  `:`  `:=`  `;`  `->`  `→`], [`{x : A} -> x`],
)

A `t` adjacent to identifier characters is lexed as part of the identifier
(`t`, `ty`, `t1` --- only bare `t` is the leaf).

== Hole marker `_`

`_` is the single-underscore *hole token*. Its meaning is position-sensitive:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  inset: (x: 6pt, y: 4pt),
  table.header[*Position*][*Meaning*],
  [binder name slot --- `{_ : T} -> e`],  [anonymous binder (equivalent to `T -> e`)],
  [type slot --- `{x : _} -> e`],         [omitted type; elaboration infers it],
  [ordinary atom --- `f _`],              [fresh metavariable],
)

`_foo` remains a regular identifier; only the bare single underscore is the
hole token.

= Grammar

One unified #raw("expr") grammar covers both term and type positions --- the
elaborator decides whether a binder is a $lambda$ or a $Pi$ from context.

== Programs and items

#rule(
  "program",
  "program  ::= item (itemSep item)* itemSep?
itemSep  ::= \";\" | \",\" | NEWLINE
item     ::= let | test | elab | use | field | exprItem",
  "let a = t
test a = a
{ let b = t; b : Type }
use \"other.disp\"",
  note: [An ordered sequence of items evaluated top-to-bottom in one scope.],
)

Items are separated by `;` or a newline; both are accepted interchangeably,
and a trailing separator is allowed.

The `let`, `field`, and `exprItem` forms share the same parser shape ---
each one is an (optionally typed) expression, with different binding
flavours chosen by leading keyword / assignment operator:

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  align: (left, left),
  inset: (x: 6pt, y: 4pt),
  table.header[*Form*][*Role*],
  [`let IDENT (: T)? = EXPR`],      [scoped name binding (visible to later items)],
  [`IDENT (: T)? := EXPR`],         [named field of the enclosing record],
  [`EXPR (: T)?`],                  [anonymous expression; as the final item, the record's trailing anonymous field],
)

When the `: T` is omitted in any of the three forms, the type is implicitly
`_` (an elaboration-solved hole).

#rule(
  "let",
  "let      ::= \"let\" IDENT (\":\" expr)? \"=\" expr",
  "let K = {x} -> {y} -> x
let id : {A : Type} -> A -> A
       = {A} -> {x} -> x",
  note: [Scoped name binding; visible to subsequent items in the same record.],
)

#rule(
  "test",
  "test     ::= \"test\" expr \"=\" expr",
  "test ({x} -> x) t = t",
  note: [Runtime assertion: both sides must reduce to hash-cons-equal trees.],
)

#rule(
  "elab",
  "elab     ::= \"elab\" IDENT \"=\" expr",
  "elab Nat = Type",
  note: [Elaborates the expression and binds the result; no type check runs.],
)

#rule(
  "use",
  "use      ::= \"use\" STRING",
  "use \"../predicates.disp\"",
  note: [Inlines items from another file into the current scope.],
)

#rule(
  "field",
  "field    ::= IDENT (\":\" expr)? \":=\" expr",
  "x := t                         ; product field, type inferred
n : Nat := t                   ; product field with explicit type",
  note: [Declares a named field in the enclosing record.],
)

#rule(
  "exprItem",
  "exprItem ::= annotated
annotated ::= expr (\":\" expr)?",
  "f x                            ; plain expression
f x : Nat                      ; expression with explicit type
t : Type                       ; record's anonymous trailing field",
  note: [Anonymous (optionally type-ascribed) expression; as a record's final item, it is the sole anonymous field (see 1-field iso).],
)

== Expressions

#rule(
  "expr",
  "expr     ::= binder | arrow",
  "{x : A} -> x
A -> B
f x",
  note: [Top of the expression grammar; either a function-introduction form
         or a smaller term.],
)

#rule(
  "binder",
  "binder   ::= record \"->\" expr
         (validated: each item inside `record` must be a bare
          binder param --- see below.)
binderParam ::= binderName (\":\" expr)?
binderName  ::= IDENT | \"_\"",
  "{x} -> x                       ; inferred type
{x : Nat} -> x                 ; annotated
{A : Type, x : A} -> x         ; multi-binder sugar (any itemSep works)
{_ : Nat} -> t                 ; anonymous (same as Nat -> t)",
  note: [Lambda or Pi depending on the expected type (`Type` ⇒ Pi, else lambda).],
)

A binder and a record share the same surface form: both open with `{`, hold
an item list, and close with `}`. The parser speculatively parses `{...}`
as a `record`, then if a trailing `->` is present, attempts to reinterpret
it as a binder. The reinterpretation succeeds only if every item matches
`binderParam` shape --- i.e. a bare IDENT (or `_`) with an optional
`: type` ascription, and *nothing else*. In particular:

- A `field` item (contains `:=`) fails the conversion.
- A `let` item (contains `let` and `=`) fails the conversion.
- Any nested `exprItem` with a non-trivial expression fails.

All of these are reported as a parse-time error when the ambiguous
record-or-binder resolves to "binder" via the trailing `->`.

The surface form is identical for $lambda$ and $Pi$. Elaboration decides from
the expected type: if that type is `Type`, the binder elaborates as $Pi$;
otherwise as $lambda$. The multi-binder sugar
`{p1, p2, ..., pn} -> body` desugars right-associatively to
`{p1} -> {p2} -> ... -> {pn} -> body`.

#rule(
  "arrow",
  "arrow    ::= app (arrowOp expr)?",
  "A -> B                         ; non-dependent; sugar for {_ : A} -> B
A -> B -> C                    ; right-assoc: A -> (B -> C)",
  note: [Non-dependent function type; desugars to `{_ : A} -> B`. Always a Pi.],
)

#rule(
  "app",
  "app      ::= atom atom*",
  "f x y                          ; ((f x) y) -- left-associative
F A B",
  note: [Left-associative juxtaposition; no separator between function and argument.],
)

#rule(
  "atom",
  "atom     ::= simple (\".\" IDENT)*
simple   ::= \"(\" annotated \")\"
           | record
           | \"raw\" \"(\" expr \")\"
           | LEAF
           | IDENT
           | \"_\"",
  "(A -> B)
(f x : Nat)                    ; parenthesized ascription
{ let h = t; h }               ; record as an atom (iso to `t`)
point.x                        ; product-field projection
point.x.fst                    ; chained projection
raw (t t)
Type
foo
_",
  note: [Indivisible expression plus optional `.field` projections; binds tighter than application.],
)

`atom` has a postfix `.IDENT` form for product-field projection. It binds
tighter than application: `f.a b` is `(f.a) b`, not `f.(a b)`.

Parenthesized forms use the same `annotated` grammar as `exprItem`, so
`(e : T)` is valid wherever an atom is accepted. This is the only way to
write a type ascription in a non-item position --- bare `e : T` outside a
`let` / `field` / `exprItem` remains a parse error.

#rule(
  "arrowOp",
  "arrowOp  ::= \"->\" | \"→\"",
  "A -> B
A → B                          ; exactly equivalent",
  note: [ASCII and Unicode forms of the arrow token; interchangeable.],
)

== Records <records>

Every `{...}` form is a *record*. There is no separate "block" construct ---
what earlier revisions called a block (an item sequence whose final
expression is its value) is just a record with one anonymous field.

#rule(
  "record",
  "record   ::= \"{\" (item (itemSep item)*)? itemSep? \"}\"",
  "{ x := t; y := t t }           ; 2-field named record
{ let h = {x} -> x; h t }      ; 1-field anonymous record (iso to `h t`)
{ x := t }                     ; 1-field named record (iso to `t`)
{}                             ; 0-field record (unit)",
  note: [Scoped item sequence; every `{...}` produces a record value (or a record type, or a binder prefix when followed by `->`).],
)

=== Fields <fields-rule>

The fields of the resulting record, in source order, come from exactly two
sources:

- each `field` item (`NAME (: T)? := EXPR`) contributes a *named* field,
- the final item of the record, if it is an `exprItem`, contributes a
  single *anonymous* trailing field.

Everything else (`let`, `test`, `elab`, `use`) is a statement: it executes
in scope order but does not contribute a field.

Mixing named fields with a trailing anonymous expression is *not* allowed
--- a record is either entirely named (any number of `:=` items, no trailing
bare expr) or a single anonymous field (one trailing `exprItem`, no `:=`
items). Intermediate bare expressions (`exprItem`s that are not the final
item) are parse errors: they would be dead code in a pure language.

=== The 1-field iso <one-field-iso>

A record with exactly one field --- named or anonymous --- is treated as
*isomorphic* to that field's value:

- `{ x := 5 }`              is interchangeable with `5`
- `{ let y = t; f y }`      is interchangeable with `f t`
- `{ a := t; b := t t }`    is a genuine 2-field record (no iso)

Where this bites: on a 1-field *named* record, the projection `.name`
compiles to the identity function, since the record value *is* the
field value. `p.name` and `p` are operationally the same. On a record with
two or more fields, `.name` compiles to the true positional projection
morphism.

The trailing-anonymous field has no surface projection syntax; it is
observable only through the iso.

=== Encoding

For a named record with $n ≥ 2$ fields `f1 := e1; ...; fn := en`, the type is
the Church-encoded dependent product

```disp
{A : Type} -> (X1 -> X2 -> ... -> Xn -> A) -> A
```

inhabited by `{A} -> {k} -> k e1 e2 ... en`. Projection `.fi` compiles to
`{p} -> p (\a1 a2 ... an. ai)` at position $i$.

1-field records reduce to their field's value as noted above. The empty
record `{}` is the 0-field Church unit type (`{A : Type} -> A -> A`),
inhabited by the polymorphic identity.

=== Projection

The postfix `.IDENT` on a value is resolved at elaboration time. The
compiler looks up `IDENT` in the target's record type and emits the
positional morphism (or identity, in the 1-field case). If the target is
not a record, or does not have a field named `IDENT`, the compiler rejects
the program --- there is no runtime fallback.

=== Duplicate fields (aliasing)

If a record declares the same field name more than once, only the final
occurrence is kept; earlier ones are shadowed. The projection morphism for
that name targets the last position:

```disp
let p = { x := t; x := t t }
test p.x = t t                 ; last wins
```

=== The "block" reading, recovered

The item-sequence-with-final-value pattern is just a 1-anonymous-field
record, unfolded through the iso:

```disp
; Idiomatic "do some work, return a value":
let y = { let a = t
          let b = t t
          f a b }              ; record {_ := f a b}, iso to f a b
```

Scope rules (`let` binds locally, `use` imports locally) are unchanged; they
were never about records to begin with.

== Disambiguation at `{`

An opening `{` always parses as a `record`. After the closing `}`, the
parser peeks for `->` (or `→`):

#table(
  columns: (auto, 1fr),
  stroke: (x, y) => if y == 0 { (bottom: 0.6pt) } else { none },
  align: (left, left),
  inset: (x: 6pt, y: 4pt),
  table.header[*After the `\}`*][*Interpretation*],
  [`->` or `→`],
    [binder --- reinterpret the items as binder params, erroring if any
     item contains `:=`, `let`, or a non-trivial expression.],
  [anything else],
    [plain record (value or type, resolved by elaboration).],
)

```disp
{x : A} -> x                   ; binder
{ x := t; y := t t }           ; record value
{ let x = t; x }               ; 1-anon-field record, iso to `t`
{}                             ; empty record

{ x := t } -> t                ; parse ERROR: field can't become a binder param
{ let x = t } -> t             ; parse ERROR: let can't become a binder param
```

= Associativity and precedence

- `atom` projection `.`: highest, left-associative.
- `app`: left-associative by juxtaposition, looser than `.`.
- `arrow`, `binder`: right-associative --- `a -> b -> c` parses as
  `a -> (b -> c)`, and `{x} -> {y} -> e` nests the inner binder in the
  outer's body.
- `binder` has lower precedence than `arrow`, so `{x : A} -> A -> B` is
  `{x : A} -> (A -> B)`.

= Module system

#set par(justify: true)

`use "path"` loads another `.disp` file, parsing its items as if they
appeared in place of the `use` directive. `path` resolves relative to the
directory of the file containing the `use` (not the process working
directory).

`{ ... }` introduces a nested scope. `let`s and `use`d imports inside the
record bind names only within the record. `test`s still execute --- their
compiled trees are closed under the bindings visible at parse time.

Name lookup walks scope frames inner-to-outer; inner bindings shadow outer.

```disp
let x = t
{
  let x = t t                  ; shadows outer x within this record
  test x = t t                 ; passes against inner x
}
test x = t                     ; passes against outer x (inner x is gone)
```

= Elaboration semantics

#let sem(head, body) = [
  #text(weight: "bold")[#raw(head)] --- #body
]

#sem("let NAME = EXPR",
  [elaborates `EXPR`; binds `NAME` to the resulting tree. No type check.])

#sem("let NAME : T = EXPR",
  [elaborates `T` and `EXPR`, runs the user-defined `pred_of_lvl` (which
   must be in scope). Binds `NAME` to the elaborated expression iff the
   check succeeds.])

#sem("elab NAME = EXPR",
  [elaborates `EXPR` without running a check; binds `NAME`.])

#sem("test LHS = RHS",
  [elaborates both sides; the run fails if the resulting trees differ by
   hash-cons identity.])

#sem("{ x : T } -> BODY",
  [elaborates as $Pi(x : T). "BODY"$ when the expected type is `Type`,
   otherwise as $lambda(x : T). "BODY"$.])

#sem("{ x } -> BODY",
  [sugar for `{x : _} -> BODY`; the omitted type is solved during
   elaboration.])

#sem("A -> B",
  [sugar for `{_ : A} -> B`; always a $Pi$.])

#sem("{ p1, p2, ..., pn } -> BODY",
  [desugars to `{p1} -> {p2} -> ... -> {pn} -> BODY`.])

#sem("{ ITEM; ... }",
  [opens a fresh scope, runs items in order, emits a record. Fields are
   the `:=` items plus at most one trailing `exprItem` as an anonymous
   field. Records with exactly one field (named or anonymous) are iso to
   the field's value. Mixing named fields with a trailing anonymous
   expression is a parse error.])

#sem("NAME (: T)? := EXPR",
  [declares a named field in the enclosing record. The optional `: T`
   pins the field's type; omitted, it is solved by elaboration (`_`).])

#sem("EXPR : T  (as item or in parens)",
  [type-ascribes the expression. At item level (`let` / `field` / final
   record expression), the `: T` is written bare. In any other position it
   must be parenthesized: `(e : T)`. The runtime value is unchanged;
   ascription only guides elaboration.])

#sem("RECORD.NAME",
  [projection by field name. For a record of two or more fields, compiles
   to the positional projection morphism for `NAME`'s source-order index.
   For a 1-field record, compiles to the identity function (the record
   *is* the field's value under the 1-field iso).])

#sem("raw (EXPR)",
  [bracket-abstracts and reduces `EXPR`, embedding the resulting tree as
   a literal in the surrounding expression.])

#sem("_ (as atom)",
  [introduces a fresh meta token for elaboration to solve.])

= Open questions

_(None remaining at this revision. The record/block/product collapse
subsumes the earlier record-type question: `{x : A, y : B}` is a record
type when the expected type is `Type`, a record value when any item
declares via `:=` or is a trailing expression, and a binder prefix when
followed by `->`.)_
