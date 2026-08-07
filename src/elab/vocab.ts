// The desugar vocabulary: every name the elaborator resolves from scope by
// convention when compiling surface syntax (literals, if/match/projection,
// binder->Pi, record/sum encodings). All such lookups funnel through sugarTree:
// a scope binding `elab_settings` — an association list of (name, payload)
// pairs, first match wins — re-points any target per scope without renaming
// kernel definitions; otherwise the bare name resolves as before.
// Declaration-protocol names (default_guard/let/test/given/check_module, the
// functor face, the guard request record) are deliberately not routed: they
// run while a module is loading, before any user configuration could exist.
import type { Tree } from "../eval/eager.js"
import { elab, type ScopeEntry } from "./state.js"
import { stringToTree } from "./literals.js"

export type SugarName =
  | "Pi"            // binder->Pi type desugar target + gate
  | "zero" | "succ" // numeric literals
  | "cond"          // if
  | "prod"          // match
  | "eff_bind"      // `x <- e` block bind
  | "dot"           // projection vocabulary hook
  | "faced"         // '#'-marked record face field
  | "make_record" | "list_const" // record value literals + module tuples
  | "Record"        // module tuple typ former
  | "Telescope" | "proj_cell" | "derive_cell" // record type literals
  | "Coproduct" | "pair" // sum type literals
  | "pair_fst" | "pair_snd" // xs[k] constant-index unrolling
  | "idx"           // xs[i] computed-index runtime accessor
  | "fst" | "snd"   // .fst/.snd pair accessors (settings-scoped)

// Desugars that emit a var (binder->Pi, match's prod, the parser's eff_bind)
// mark it with this prefix; exprToCir resolves the mark through sugarTree so
// only sugar-emitted vars are re-pointed, never user-written names.
export const SUGAR_PREFIX = "@sugar:"
export const sugarVarName = (name: SugarName): string => SUGAR_PREFIX + name

// First (name, payload) hit in the elab_settings pair-list — a fork spine of
// fork(interned-name, payload) entries, so a settings value is host-walkable
// without the evaluator and a cons-prepend overrides an opened base list.
function settingsTarget(settings: Tree, name: SugarName): Tree | undefined {
  if (!elab.cs.classify || !elab.cs.equal) return undefined
  const key = stringToTree(name)
  let cur = elab.cs.classify(settings)
  while (cur.tag === "fork") {
    const entry = elab.cs.classify(cur.left)
    if (entry.tag === "fork" && elab.cs.equal(entry.left, key)) return entry.right
    cur = elab.cs.classify(cur.right)
  }
  return undefined
}

export function sugarTree(
  lookupEntry: (name: string) => ScopeEntry | undefined,
  name: SugarName,
): Tree | undefined {
  const settings = lookupEntry("elab_settings")?.tree
  if (settings != null) {
    const hit = settingsTarget(settings, name)
    if (hit != null) return hit
  }
  return lookupEntry(name)?.tree
}

// .fst/.snd are pair projections ONLY in scopes that carry elab_settings (the
// new-syntax worlds); legacy scopes keep the record-cut reading of those field
// names (lib/std/pair.disp's records). A settings entry can re-point them;
// otherwise they resolve to the scope's pair_fst/pair_snd.
export function pairAccessor(
  lookupEntry: (name: string) => ScopeEntry | undefined,
  which: "fst" | "snd",
): Tree | undefined {
  const settings = lookupEntry("elab_settings")?.tree
  if (settings == null) return undefined
  return settingsTarget(settings, which) ?? lookupEntry(which === "fst" ? "pair_fst" : "pair_snd")?.tree
}
