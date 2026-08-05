// The desugar vocabulary: every name the elaborator resolves from scope by
// convention when compiling surface syntax (literals, if/match/projection,
// binder->Pi, record/sum encodings). All such lookups funnel through sugarTree
// so a future `elab_settings` value can re-point targets per scope without
// renaming the kernel's definitions; today it is a pure name lookup.
// Declaration-protocol names (default_guard/let/test/given/check_module, the
// functor face, the guard request record) are deliberately not routed: they
// run while a module is loading, before any user configuration could exist.
import type { Tree } from "../eval/eager.js"
import type { ScopeEntry } from "./state.js"

export type SugarName =
  | "Pi"            // binder->Pi type desugar target + gate
  | "zero" | "succ" // numeric literals
  | "cond"          // if
  | "prod"          // match (gate; emission stays a var for scope resolution)
  | "dot"           // projection vocabulary hook
  | "faced"         // '#'-marked record face field
  | "make_record" | "list_const" // record value literals + module tuples
  | "Record"        // module tuple typ former
  | "Telescope" | "proj_cell" | "derive_cell" // record type literals
  | "Coproduct" | "pair" // sum type literals

export function sugarTree(
  lookupEntry: (name: string) => ScopeEntry | undefined,
  name: SugarName,
): Tree | undefined {
  return lookupEntry(name)?.tree
}
