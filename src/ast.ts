// Surface AST and Core AST. See SYNTAX.typ § "Abstract syntax tree".
//
// Two tiers, in strict-superset relation:
//
//   * Core — the contract every backend elaborator consumes. Pure
//     expressions, no statements, no top-level items, no scope concepts,
//     no source-only sugar. Every `Var` refers to a binder parameter in
//     scope at that position; every `Hole` is a fresh metavariable the
//     elaborator will solve.
//
//   * Surface (aliased as `Expr`) — superset of Core. Adds `Block` as
//     the only surface-only expression kind. Parser also deals in the
//     surface-only non-expression kinds (`Stmt`, `TopLevel`, `Program`),
//     which are never reachable from Core.
//
// The parser drives a backend elaborator as it walks the surface AST:
// internal `let`s desugar to `App(Binder)`; `use` inlines; `test`
// bodies are elaborated immediately and compared by hash-cons identity;
// `Block { members: [] }` collapses to its trailing. By the time elab
// sees any sub-expression, it is a pure `Core` value.

export type Span = { start: number; end: number; file: string }

// ─────────────────────────────── Core AST ───────────────────────────────
//
// The elab contract. Every node here is also a valid `Expr` (Surface).
// No statements, no Def, no Block, no Let/Use/Test.

export type Core =
  | Var | Leaf | Hole | App | Binder | RecValue | Ann | Proj

export type Var  = { tag: "var";  name: string;                  span: Span }
export type Leaf = { tag: "leaf";                                 span: Span }
export type Hole = { tag: "hole";                                 span: Span }
export type App  = { tag: "app";  fn: Expr; arg: Expr;            span: Span }
export type Proj = { tag: "proj"; target: Expr; field: string;    span: Span }

// Unified binder / record-type.
//   body === null : record type (Church-encoded product). Every Param
//                   must have a non-null name AND a non-null type.
//   body !== null : lambda or Pi (elaborator decides from expected type).
//                   Params may have null name (`_`) and null type
//                   (inferred metavariable).
export type Binder = {
  tag: "binder"
  params: Param[]
  body: Expr | null
  span: Span
}

export type Param = {
  name: string | null    // null = hole binder `_` (only legal when body !== null)
  type: Expr | null      // null = omitted type slot (only legal when body !== null)
  span: Span
}

export type RecValue = {
  tag: "rec_value"
  members: NamedField[]
  span: Span
}

export type NamedField = {
  tag: "named_field"
  name: string
  type: Expr | null
  value: Expr
  span: Span
}

// Type ascription. `(e : T)` in source. Typed `let NAME : T = e`
// desugars body to `Ann(e, T)` at parse time.
export type Ann = {
  tag: "ann"
  expr: Expr
  type: Expr
  span: Span
}

// ────────────────────────────── Surface AST ─────────────────────────────
//
// Superset of Core. Adds `Block` (an expression) and the non-expression
// kinds used only during parsing. Fields typed as `Expr` accept either
// Core or Block; by elab time, all Blocks are gone.

export type Expr = Core | Block

// Statements with a trailing value. `let`s are desugared away before
// reaching elab; `use`s are inlined; only `Test` members survive an
// empty-block simplification pass, and a Block with no members collapses
// to its trailing expression.
export type Block = {
  tag: "block"
  members: Stmt[]
  trailing: Expr
  span: Span
}

// ──────────────────────── Surface-only non-expressions ──────────────────
//
// Never reach the elaborator. The parser consumes these directly.

export type Stmt = Let | Test | Use

export type Let = {
  tag: "let"
  name: string
  type: Expr | null    // when set, desugared to Ann(body, type) at parse time
  body: Expr
  span: Span
}

export type Test = {
  tag: "test"
  lhs: Expr
  rhs: Expr
  span: Span
}

export type Use = {
  tag: "use"
  path: string
  span: Span
}

// Top-level items. A top-level `let` becomes a `Def`; a top-level `test`
// stays a `Test`; a top-level `use` is inlined by the parser and never
// appears here.
export type TopLevel = Def | Test

export type Def = {
  tag: "def"
  name: string
  body: Expr    // Ann(body, T) if the source had `let x : T = body`
  span: Span
}

export type Program = {
  tag: "program"
  members: TopLevel[]
  span: Span
}

// ─────────────────────────── Type predicates ────────────────────────────

const CORE_TAGS: ReadonlySet<string> = new Set([
  "var", "leaf", "hole", "app", "binder", "rec_value", "ann", "proj",
])

export function isCore(e: Expr): e is Core {
  return CORE_TAGS.has(e.tag)
}
