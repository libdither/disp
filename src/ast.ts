// Surface AST. Produced by src/parse.ts, consumed by src/elaborate.ts.
// Specification: SYNTAX.typ § "Abstract syntax tree".
//
// Shape principles:
//   * Every node carries a Span. Spans are stripped when the AST is
//     serialized to a tree-calculus term for the eventual disp-hosted
//     elaborator — they are a reference-implementation concession for
//     error messages.
//   * The parser does not resolve names. Every Var stays a string;
//     the elaborator walks the AST with a scope and reports unresolved
//     names.
//   * Internal `let`s are desugared at parse time to App(Binder):
//     `let x : T = body; rest` becomes
//     `App(Binder([{name: "x", type: T}], rest), body)`.
//     Top-level `let`s remain as `Def` nodes in `Program.members`.
//     `use` is inlined by the parser; there is no Use node.
//   * Binder is unified: body === null is a record type; body !== null
//     is a lambda/Pi.
//   * `A -> B` parses directly to `Binder([{name: null, type: A}], B)`;
//     there is no dedicated Arrow node.
//   * Dependent scoping is not in the AST — it arises from the
//     elaborator walking params/members in order.

export type Span = { start: number; end: number; file: string }

// ─────────────────────────────── Expressions ────────────────────────────

export type Expr =
  | Var | Leaf | Hole | App | Proj | Binder
  | RecValue | Block | Ann

export type Var   = { tag: "var";   name: string;                  span: Span }
export type Leaf  = { tag: "leaf";                                  span: Span }
export type Hole  = { tag: "hole";                                  span: Span }
export type App   = { tag: "app";   fn: Expr; arg: Expr;            span: Span }
export type Proj  = { tag: "proj";  target: Expr; field: string;    span: Span }

// Unified binder / record-type.
//   body === null : record type (Church-encoded product). Every Param
//                   must have non-null name and non-null type.
//   body !== null : lambda or Pi (elaborator decides from expected type).
//                   Params may have null name (`_`) and null type
//                   (inferred metavariable).
export type Binder = {
  tag: "binder"
  params: Param[]
  body: Expr | null
  span: Span
}

// `name` = null represents the hole binder `_` (only legal when body !== null).
// `type` = null represents an omitted type slot (only legal when body !== null).
export type Param = {
  name: string | null
  type: Expr | null
  span: Span
}

// ─────────────────────────────── Braced-value forms ─────────────────────

// Dependent record value. Later fields see earlier ones in scope.
export type RecValue = {
  tag: "rec_value"
  members: NamedField[]
  span: Span
}

// Statements followed by exactly one trailing expression.
export type Block = {
  tag: "block"
  members: Stmt[]
  trailing: Expr
  span: Span
}

export type NamedField = {
  tag: "named_field"
  name: string
  type: Expr | null
  value: Expr
  span: Span
}

// ─────────────────────────────── Ann ────────────────────────────────────

// Type ascription. `(e : T)` in source. Top-level `let x : T = e`
// becomes `Def { name: "x", body: Ann(e, T) }`. Internal typed lets
// carry their type via the Binder.param.type slot of the desugared
// `App(Binder, body)` form, not via Ann.
export type Ann = {
  tag: "ann"
  expr: Expr
  type: Expr
  span: Span
}

// ─────────────────────────────── Statements ─────────────────────────────
//
// Only Test survives name-resolution. Let is substituted away at parse
// time; Use is inlined; there is no `elab` or `raw` in the language.

export type Stmt = Test

export type Test = {
  tag: "test"
  lhs: Expr
  rhs: Expr
  span: Span
}

// ─────────────────────────────── Program ────────────────────────────────

export type TopLevel = Def | Test

// Named top-level export. Body may be an Ann when the source had
// `let name : T = ...`.
export type Def = {
  tag: "def"
  name: string
  body: Expr
  span: Span
}

export type Program = {
  tag: "program"
  members: TopLevel[]
  span: Span
}
