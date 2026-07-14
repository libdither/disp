// The elaborator's public surface — a re-export barrel over src/elab/:
//   state.ts    — shared session/budget/caches + ScopeEntry/SigParam/CompileSinks
//   cir.ts      — CIR + bracket abstraction (Expr-independent core)
//   sugar.ts    — surface rewrites: select_lazy->if, named args, binder->Pi
//   literals.ts — Nat/String/accessor encoding + record-header decoding
//   expr.ts     — exprToCir + compileExpr/compileType
//   driver.ts   — parseProgram: scope stack, `use` resolution, auto-verify
// Import from here (unchanged paths for run.ts/tests); reach into src/elab/
// only for elaborator-internal work.

export { parseProgram, type Decl, type ParseItemStats, type ParseProgramOptions } from "./elab/driver.js"
export { collectSessionRoots, type ScopeEntry, type SigParam, type CompileSinks, APPLY_BUDGET } from "./elab/state.js"
export { stringToTree } from "./elab/literals.js"
export { compileExpr, compileType, exprToCir } from "./elab/expr.js"
export { cirToAstTree, type Cir } from "./elab/cir.js"
