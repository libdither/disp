// Binding-closure export (EVALUATOR.md).
//
// In disp, elaboration INLINES everything: a compiled binding's tree is pure
// tree calculus with no symbolic references — every definition it uses, tree_eq
// included, is already inline (hash-consing keeps it small in memory, ternary
// expands the sharing). So "closing a binding into a self-contained blob" is just
// a canonical dump; there is no native-substitution step to perform, because any
// evaluator that adopts the standard-natives convention (§3.1) recognizes tree_eq
// inside the blob and fast-paths it, while non-recognizers run the inline
// definition — correct, merely slower. A blob for a session backend and a blob
// for the batch tier are therefore identical.

import type { Session, Budget } from "../eval/types.js"

// Emit a handle as a self-contained ternary blob. (Thin by construction — the
// value of export is that the result is portable to ANY tree-calculus evaluator,
// not that closing it takes work.)
export function emitBlob<H>(session: Session<H>, tree: H, budget?: Budget): string {
  return session.dumpTernary(tree, budget)
}
