// Frontend contract: separates surface syntax (lexing, parsing, desugaring)
// from the syntax-independent core module driver (src/elab/driver.ts).
//
// Any frontend parses source text into CoreItems and lowers its own surface
// expressions strictly into CoreExpr (`lit | var | app | lam`), leaving
// hermetic module loading, declaration/guard execution, bracket abstraction,
// and `check_module` verification to the single core driver.

import type { Tree } from "../eval/eager.js"
import type { CoreExpr } from "./ir.js"
import type { ScopeEntry, CompileSinks, LicenseCert, SigParam } from "../elab/state.js"
import type { Expr } from "../parse.js"

/// Static export/field metadata resolved from a module/record expression.
export interface ResolvedRecord {
  fields: string[]
  fieldTrees?: Tree[]
  fieldTypes?: (Tree | null)[]
  fieldTypeExprs?: (Expr | null)[]
  fieldGuards?: (Tree | null)[]
  fieldCerts?: (LicenseCert | null)[]
  fieldParams?: (SigParam[] | undefined)[]
}

/// Environment passed by the core driver when lowering a frontend expression.
export interface FrontendEnv {
  lookup: (name: string) => ScopeEntry | undefined
  resolveUse: (path: string, raw?: boolean, fills?: Map<string, Tree>) => ScopeEntry
  sinks?: CompileSinks
}

/// Pre-scanned module dependency (`given name : type (:= default)?`).
export interface CoreGiven<SExpr = unknown> {
  name: string
  type: SExpr | null
  dflt: SExpr | null
}

/// Syntax-independent top-level module items consumed by the core driver.
export type CoreItem<SExpr = unknown> =
  | {
      tag: "declare"
      name: string
      head?: SExpr
      /// Bare decorator name when `head` is a simple identifier (e.g. "let", "given").
      headName?: string
      type: SExpr | null
      value: SExpr | null
      docType?: SExpr
      line?: number
      endLine?: number
      /// Opaque frontend-private metadata forwarded to `onDeclared`.
      meta?: unknown
    }
  | { tag: "test"; lhs: SExpr; rhs: SExpr; line?: number; endLine?: number }
  | { tag: "open"; expr: SExpr }

/// Result of parsing a single source module with a frontend.
export interface FrontendModule<SExpr = unknown> {
  items: CoreItem<SExpr>[]
  givens: CoreGiven<SExpr>[]
  hasExports: boolean
}

/// Driver callbacks exposed to optional `Frontend.onDeclared` post-declaration hooks.
export interface DeclareHookCtx<SExpr = unknown> {
  pushed: boolean
  raw: boolean
  exposeLocals: boolean
  env: FrontendEnv
  define: (name: string, entry: ScopeEntry) => void
  declareValue: (name: string, value: SExpr) => { pushDef: boolean; tree?: Tree; type?: Tree | null; guard?: Tree; priv?: boolean }
  emitDef: (name: string, tree: Tree, type?: Tree | null, guard?: Tree | null) => void
  recordFieldItem: (name: string, tree?: Tree) => void
}

/// Pluggable surface syntax frontend.
export interface Frontend<SExpr = unknown> {
  readonly name: string
  parse(src: string, sourcePath?: string): FrontendModule<SExpr>
  lowerExpr(expr: SExpr, env: FrontendEnv): CoreExpr
  lowerType?(expr: SExpr, env: FrontendEnv): CoreExpr
  /// Whether `typeExpr` should be compiled as a checked type annotation in this scope.
  shouldCompileTypeAnnotation?(typeExpr: SExpr, env: FrontendEnv): boolean
  /// Whether `typeExpr` is a syntactic `: Type` alias whose body uses type-mode lowering even under `use raw`.
  isTypeUniverseAnnotation?(typeExpr: SExpr, env: FrontendEnv): boolean
  /// Optional test-equation LHS preprocessor (e.g., Disp's pristine `test` marker peel).
  preprocessTestLhs?(lhs: SExpr, env: FrontendEnv): SExpr
  /// Optional static module/record metadata resolver (for `open` and compile-time projection).
  resolveRecord?(expr: SExpr, env: FrontendEnv): ResolvedRecord | undefined
  /// Optional named-argument parameter signature extractor for a bound declaration.
  extractParams?(valueExpr: SExpr, typeExpr: SExpr | null | undefined): SigParam[] | undefined
  /// Optional fallback arity reporter for item stats when `extractParams` has none.
  declaredArity?(valueExpr: SExpr | null, typeExpr: SExpr | null | undefined, docType?: SExpr): number | undefined
  /// Optional abstract-mode `open <given>` field list (for record-typed module dependencies).
  openAbstractGiven?(openExpr: SExpr, givens: CoreGiven<SExpr>[]): { name: string; fields: string[] } | undefined
  /// Optional frontend-specific post-declaration hook (e.g., Disp `record` fields, sum ctors, `--locals`).
  onDeclared?(item: Extract<CoreItem<SExpr>, { tag: "declare" }>, ctx: DeclareHookCtx<SExpr>): void
}

/// Choose which frontend parses a `use`d file by absolute path.
export type FrontendResolver = (absPath: string, current: Frontend<any>) => Frontend<any>
