// Canonical syntax-independent Core IR and out-of-band source spans.
// Every frontend lowers its surface syntax to CoreExpr (pure lambda calculus
// over trees); bracket abstraction (src/elab/cir.ts) lowers CoreExpr to Tree.
// Source spans live in a WeakMap side-table so AST/IR structural equality and
// hash-consed tree identity stay completely unaffected by source positions.

import type { Tree } from "../eval/eager.js"

/// 1-based source extent of a parsed token, AST node, or lowered IR node.
export interface Span {
  line: number
  col: number
  endLine: number
  endCol: number
  source?: string
}

/// Canonical syntax-independent expression IR: pure lambda calculus over trees.
export type CoreExpr =
  | { tag: "lit"; t: Tree }
  | { tag: "var"; name: string }
  | { tag: "app"; f: CoreExpr; x: CoreExpr }
  | { tag: "lam"; x: string; body: CoreExpr }

export const coreLit = (t: Tree): CoreExpr => ({ tag: "lit", t })
export const coreVar = (name: string): CoreExpr => ({ tag: "var", name })
export const coreApp = (f: CoreExpr, x: CoreExpr): CoreExpr => ({ tag: "app", f, x })
export const coreLam = (x: string, body: CoreExpr): CoreExpr => ({ tag: "lam", x, body })

const spans = new WeakMap<object, Span>()

/// Record a source span for a token, AST node, or IR node (returns the node).
export function attachSpan<T extends object>(node: T, span: Span | undefined): T {
  if (span) spans.set(node, span)
  return node
}

/// Read a node's recorded source span, if any.
export function spanOf(node: object | null | undefined): Span | undefined {
  return node != null ? spans.get(node) : undefined
}

/// Copy a recorded source span onto `to` if `to` does not already carry a tighter span.
export function copySpan<T extends object>(from: object | null | undefined, to: T): T {
  if (!spans.has(to)) {
    const s = spanOf(from)
    if (s) spans.set(to, s)
  }
  return to
}

/// Format a span as `line:col` (or `source:line:col` when a file path is set).
export function formatSpan(span: Span): string {
  const loc = `${span.line}:${span.col}`
  return span.source ? `${span.source}:${loc}` : loc
}
