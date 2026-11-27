// The elaborator's public surface — a re-export barrel over src/core/, src/frontend/, and src/elab/:
//   core/ir.ts        — Canonical CoreExpr + out-of-band Span side-table
//   core/frontend.ts  — Pluggable Frontend contract (Surface -> CoreExpr/CoreItem)
//   frontend/disp.ts  — Default Disp surface frontend
//   frontend/sexp.ts  — Reference S-expression frontend
//   elab/state.ts     — shared session/budget/caches + ScopeEntry/SigParam/CompileSinks
//   elab/cir.ts       — CIR + bracket abstraction (CoreExpr -> Tree)
//   elab/literals.ts  — Nat/String/accessor encoding + record-header decoding
//   elab/driver.ts    — syntax-agnostic module driver (parseProgram)
// Import from here (unchanged paths for run.ts/tests); reach into src/elab/
// only for elaborator-internal work.

export { parseProgram, type Decl, type ParseItemStats, type ParseProgramOptions } from "./elab/driver.js"
export { collectSessionRoots, type ScopeEntry, type SigParam, type CompileSinks, APPLY_BUDGET } from "./elab/state.js"
export { stringToTree } from "./elab/literals.js"
export { compileExpr, compileType, exprToCir } from "./elab/expr.js"
export { type CoreExpr, type Span, attachSpan, spanOf, copySpan, formatSpan, coreLit, coreVar, coreApp, coreLam } from "./core/ir.js"
export { type Frontend, type FrontendModule, type FrontendEnv, type FrontendResolver, type CoreItem, type CoreGiven } from "./core/frontend.js"
export { dispFrontend } from "./frontend/disp.js"
export { sexpFrontend } from "./frontend/sexp.js"
export { coreToTree } from "./elab/cir.js"
