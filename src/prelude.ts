// Prelude loading: native-only registration of builtins and declaration processing.

import { type Tree } from "./tree.js"
import { type SExpr, parseLine } from "./parse.js"
import { compileAndEval, compileRecAndEval } from "./compile.js"
import { registerNativeBuiltinId, onClearNativeBuiltins } from "./native-utils.js"
import { PRIMITIVE_BUILTINS, TREE_NATIVE_BUILTINS } from "./tree-native.js"
import { type KnownDefs, TN_TYPE, TN_TREE, TN_BOOL, TN_NAT, annAscribe } from "./tree-native-checker.js"
import { nativeElabDecl, type NativeEnv, buildNativeWrapped, collapseTypedExpr, NativeElabError } from "./tree-native-elaborate.js"

interface TreeBuiltin { name: string; type: string; data: Tree }

// --- Native-only builtin registration ---

function registerBuiltins(
  builtins: TreeBuiltin[],
  nativeEnv: NativeEnv, nativeDefs: KnownDefs, defs: Map<string, Tree>,
): void {
  for (const builtin of builtins) {
    registerNativeBuiltinId(builtin.data.id)
    const typeSexpr = parseLine(builtin.type) as SExpr
    try {
      const { texpr } = buildNativeWrapped(typeSexpr, nativeEnv, TN_TYPE)
      const nativeType = collapseTypedExpr(texpr)
      nativeDefs.set(builtin.data.id, nativeType)
      nativeEnv.set(builtin.name, {
        tree: builtin.data,
        annTree: annAscribe(nativeType, builtin.data),
        type: nativeType,
      })
    } catch {
      // Skip builtins whose dependent types can't yet be elaborated natively.
      // They are still registered as native builtin IDs and in defs.
      nativeEnv.set(builtin.name, {
        tree: builtin.data,
        annTree: builtin.data,
        type: TN_TREE, // fallback: treat as Tree-typed
      })
    }
    defs.set(builtin.name, builtin.data)
  }
}

// --- Native-only declaration processing ---

export function processDecl(
  name: string, type: SExpr | null, value: SExpr, isRec: boolean,
  nativeEnv: NativeEnv, nativeDefs: KnownDefs, defs: Map<string, Tree>,
): { nativeType: Tree, compiled: Tree, checkOk: boolean } {
  // Try full native elaboration first (handles compilation internally)
  try {
    const result = nativeElabDecl(nativeEnv, defs, nativeDefs, name, type, value, isRec)
    const entry = result.env.get(name)
    if (entry) nativeEnv.set(name, entry)
    if (isRec) registerNativeBuiltinId(result.compiled.id, result.compiled)
    return { nativeType: result.nativeType, compiled: result.compiled, checkOk: result.checkOk }
  } catch (elabError) {
    // Check if this is a real type error that should be propagated
    if (elabError instanceof NativeElabError) {
      const msg = elabError.message
      // Missing type annotation on recursive def is always a user error
      if (msg.includes("requires a type annotation")) {
        throw elabError
      }
    }
    // Elaboration failed — fall through to compile-only path
  }

  // Fallback: compile without type checking (defs must not have name yet for recursive self-ref)
  const compiled = isRec ? compileRecAndEval(name, value, defs) : compileAndEval(value, defs)
  defs.set(name, compiled)
  if (isRec) registerNativeBuiltinId(compiled.id, compiled)

  // Try to elaborate the type annotation alone for better typing.
  // Propagate clear user errors (non-type domain, etc.) but catch elaboration limitations.
  let nativeType: Tree = TN_TREE
  if (type) {
    try {
      const { texpr } = buildNativeWrapped(type, nativeEnv, TN_TYPE)
      nativeType = collapseTypedExpr(texpr)
    } catch (typeErr) {
      if (typeErr instanceof NativeElabError) {
        const msg = typeErr.message
        if (msg.includes("Pi domain must be a type") ||
            msg.includes("Pi codomain must be a type")) {
          throw typeErr
        }
      }
      // Other errors (unbound variable in type, etc.) → use Tree as fallback
    }
  }

  nativeDefs.set(compiled.id, nativeType)
  nativeEnv.set(name, { tree: compiled, annTree: annAscribe(nativeType, compiled), type: nativeType })
  return { nativeType, compiled, checkOk: false }
}

// --- Native-only prelude loading ---

let cachedNativeDefs: KnownDefs | null = null
let cachedNativeEnv: NativeEnv | null = null

export function clearPreludeCache(): void {
  cachedNativeDefs = null
  cachedNativeEnv = null
}

// Automatically invalidate the prelude cache when native builtins are cleared.
// This prevents stale cached state from being used after tests reset globals.
onClearNativeBuiltins(clearPreludeCache)

export function loadPrelude(): { defs: Map<string, Tree>, nativeDefs: KnownDefs, nativeEnv: NativeEnv } {
  const defs = new Map<string, Tree>()

  const skipBuild = cachedNativeDefs !== null
  const nativeDefs: KnownDefs = cachedNativeDefs ? new Map(cachedNativeDefs) : new Map()
  const nativeEnv: NativeEnv = cachedNativeEnv ? new Map(cachedNativeEnv) : new Map()

  if (!skipBuild) {
    // 1. Primitive types
    for (const [name, nativeMarker] of [["Tree", TN_TREE], ["Bool", TN_BOOL], ["Nat", TN_NAT]] as const) {
      nativeDefs.set(nativeMarker.id, TN_TYPE)
      nativeEnv.set(name, { tree: nativeMarker, annTree: nativeMarker, type: TN_TYPE })
      defs.set(name, nativeMarker)
    }

    // 2. Primitive builtins (leaf, stem, fork, triage, true, false, boolElim, zero, succ, natElim, tfst, tsnd, tchild)
    registerBuiltins(PRIMITIVE_BUILTINS, nativeEnv, nativeDefs, defs)

    // 3. Tree-native builtins (step functions, combinators, etc.)
    // No coc-prelude.disp needed!
    registerBuiltins(TREE_NATIVE_BUILTINS, nativeEnv, nativeDefs, defs)

    cachedNativeDefs = new Map(nativeDefs)
    cachedNativeEnv = new Map(nativeEnv)
  } else {
    // Restore defs from cached env
    for (const [name, { tree }] of nativeEnv) {
      defs.set(name, tree)
    }
  }

  return { defs, nativeDefs, nativeEnv }
}

