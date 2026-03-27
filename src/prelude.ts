// Prelude loading: native-only registration of builtins and declaration processing.

import * as fs from "node:fs"
import { fileURLToPath } from "node:url"
import { type Tree, LEAF, fork } from "./tree.js"
import { type SExpr, type SDecl, parseLine, mergeDefinitions } from "./parse.js"
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

// --- Legacy CoC prelude loading (for backward-compatible tests) ---

import {
  encType, wrap, unwrapData,
  TREE_TYPE, BOOL_TYPE, NAT_TYPE,
  buildWrapped, cocCheckDecl, type Env,
} from "./coc.js"
import { convertCocType, annotateTree } from "./tree-native-elaborate.js"
import { checkAnnotated } from "./tree-native-checker.js"

function registerBuiltinsCoc(
  builtins: TreeBuiltin[], env: Env, defs: Map<string, Tree>,
): Env {
  for (const builtin of builtins) {
    registerNativeBuiltinId(builtin.data.id)
    const typeSexpr = parseLine(builtin.type) as SExpr
    const typeWrapped = buildWrapped(typeSexpr, env, encType())
    const typeData = unwrapData(typeWrapped)
    env = new Map(env)
    env.set(builtin.name, wrap(builtin.data, typeData))
    defs.set(builtin.name, builtin.data)
  }
  return env
}

function cocPreludePath(): string {
  return fileURLToPath(new URL("../coc-prelude.disp", import.meta.url))
}

function loadDeclFileCoc(filePath: string, env: Env, defs: Map<string, Tree>): Env {
  const content = fs.readFileSync(filePath, "utf-8")
  for (const block of mergeDefinitions(content)) {
    const trimmed = block.text.trim()
    if (!trimmed || trimmed.startsWith("--")) continue
    const parsed = parseLine(trimmed)
    if (!("isRec" in parsed)) continue
    const sdecl = parsed as SDecl
    const result = cocCheckDecl(env, sdecl.name, sdecl.type, sdecl.value, sdecl.isRec)
    env = result.env
    const compiled = sdecl.isRec
      ? compileRecAndEval(sdecl.name, sdecl.value, defs)
      : compileAndEval(sdecl.value, defs)
    defs.set(sdecl.name, compiled)
    if (sdecl.isRec) {
      const encodedData = unwrapData(env.get(sdecl.name)!)
      registerNativeBuiltinId(encodedData.id, compiled)
    }
  }
  return env
}

export function loadCocPrelude(): { cocEnv: Env, defs: Map<string, Tree>, nativeDefs: KnownDefs, nativeEnv: NativeEnv } {
  let env: Env = new Map()
  const defs = new Map<string, Tree>()
  const nativeDefs: KnownDefs = new Map()
  const nativeEnv: NativeEnv = new Map()

  // 1. Inject primitive types
  const typeMapping: [string, Tree, Tree][] = [
    ["Tree", TREE_TYPE, TN_TREE], ["Bool", BOOL_TYPE, TN_BOOL], ["Nat", NAT_TYPE, TN_NAT],
  ]
  for (const [name, marker, nativeMarker] of typeMapping) {
    env = new Map(env)
    env.set(name, wrap(marker, encType()))
    defs.set(name, marker)
    nativeDefs.set(marker.id, TN_TYPE)
    nativeEnv.set(name, { tree: nativeMarker, annTree: nativeMarker, type: TN_TYPE })
  }

  // 2. Register primitive builtins
  env = registerBuiltinsCoc(PRIMITIVE_BUILTINS, env, defs)
  // Also register in native env
  for (const builtin of PRIMITIVE_BUILTINS) {
    try {
      const typeSexpr = parseLine(builtin.type) as SExpr
      const { texpr } = buildNativeWrapped(typeSexpr, nativeEnv, TN_TYPE)
      const nativeType = collapseTypedExpr(texpr)
      nativeDefs.set(builtin.data.id, nativeType)
      nativeEnv.set(builtin.name, { tree: builtin.data, annTree: builtin.data, type: nativeType })
    } catch {
      const typeWrapped = buildWrapped(parseLine(builtin.type) as SExpr, env, encType())
      const nativeType = convertCocType(unwrapData(typeWrapped))
      nativeDefs.set(builtin.data.id, nativeType)
      nativeEnv.set(builtin.name, { tree: builtin.data, annTree: builtin.data, type: nativeType })
    }
  }

  // 3. Load coc-prelude.disp
  env = loadDeclFileCoc(cocPreludePath(), env, defs)

  // 4. Register tree-native builtins
  env = registerBuiltinsCoc(TREE_NATIVE_BUILTINS, env, defs)
  for (const builtin of TREE_NATIVE_BUILTINS) {
    try {
      const typeSexpr = parseLine(builtin.type) as SExpr
      const { texpr } = buildNativeWrapped(typeSexpr, nativeEnv, TN_TYPE)
      const nativeType = collapseTypedExpr(texpr)
      nativeDefs.set(builtin.data.id, nativeType)
      nativeEnv.set(builtin.name, { tree: builtin.data, annTree: builtin.data, type: nativeType })
    } catch {
      try {
        const typeWrapped = buildWrapped(parseLine(builtin.type) as SExpr, env, encType())
        const nativeType = convertCocType(unwrapData(typeWrapped))
        nativeDefs.set(builtin.data.id, nativeType)
        nativeEnv.set(builtin.name, { tree: builtin.data, annTree: builtin.data, type: nativeType })
      } catch {}
    }
  }

  return { cocEnv: env, defs, nativeDefs, nativeEnv }
}

// Legacy processDecl with CoC support (for tests)
export function processDeclCoc(
  name: string, type: SExpr | null, value: SExpr, isRec: boolean,
  env: Env, defs: Map<string, Tree>,
  nativeDefs?: KnownDefs, nativeEnv?: NativeEnv,
): { env: Env, type: Tree, compiled: Tree, nativeCheckOk?: boolean, nativeType?: Tree } {
  if (nativeEnv && nativeDefs && type) {
    try {
      const nResult = nativeElabDecl(nativeEnv, defs, nativeDefs, name, type, value, isRec)
      const nativeEntry = nResult.env.get(name)
      if (nativeEntry) nativeEnv.set(name, nativeEntry)
      const cocResult = cocCheckDecl(env, name, type, value, isRec)
      env = cocResult.env
      if (isRec) {
        const encodedData = unwrapData(env.get(name)!)
        registerNativeBuiltinId(encodedData.id, nResult.compiled)
      }
      return {
        env, type: cocResult.type, compiled: nResult.compiled,
        nativeCheckOk: nResult.checkOk, nativeType: nResult.nativeType,
      }
    } catch {}
  }

  const result = cocCheckDecl(env, name, type, value, isRec)
  env = result.env
  const compiled = isRec
    ? compileRecAndEval(name, value, defs)
    : compileAndEval(value, defs)
  defs.set(name, compiled)
  if (isRec) {
    const encodedData = unwrapData(env.get(name)!)
    registerNativeBuiltinId(encodedData.id, compiled)
  }
  let nativeCheckOk: boolean | undefined
  let nativeType: Tree | undefined
  if (nativeDefs) {
    try {
      nativeType = convertCocType(result.type)
      nativeDefs.set(compiled.id, nativeType)
      if (nativeEnv) nativeEnv.set(name, { tree: compiled, annTree: compiled, type: nativeType })
      if (isRec) nativeCheckOk = true
      else {
        const annotated = annotateTree(compiled, nativeType, nativeDefs)
        nativeCheckOk = checkAnnotated(nativeDefs, annotated, nativeType)
      }
    } catch {}
  }
  return { env, type: result.type, compiled, nativeCheckOk, nativeType }
}

const NAME_MAP_SKIP = new Set(["leaf", "stem", "fork", "true", "zero", "succ", "tDelta", "encType", "encVar", "wrap"])

export function buildNameMap(env: Env): Map<number, string> {
  const map = new Map<number, string>()
  for (const [name, wrapped] of env) {
    if (NAME_MAP_SKIP.has(name)) continue
    const data = unwrapData(wrapped)
    if (!map.has(data.id)) map.set(data.id, name)
  }
  return map
}
