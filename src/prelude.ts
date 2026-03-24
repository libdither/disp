// Prelude loading: unified registration of builtins, CoC prelude, and env/defs construction.

import * as fs from "node:fs"
import { fileURLToPath } from "node:url"
import { type Tree, LEAF } from "./tree.js"
import { type SExpr, type SDecl, parseLine, mergeDefinitions } from "./parse.js"
import { compileAndEval, compileRecAndEval } from "./compile.js"
import {
  encType,
  wrap, unwrapData,
  TREE_TYPE, BOOL_TYPE, NAT_TYPE,
  buildWrapped, cocCheckDecl, registerNativeBuiltinId, type Env,
} from "./coc.js"
import { PRIMITIVE_BUILTINS, TREE_NATIVE_BUILTINS } from "./tree-native.js"

interface TreeBuiltin { name: string; type: string; data: Tree }

function registerBuiltins(builtins: TreeBuiltin[], env: Env, defs: Map<string, Tree>): Env {
  for (const builtin of builtins) {
    registerNativeBuiltinId(builtin.data.id)
    const typeWrapped = buildWrapped(parseLine(builtin.type) as SExpr, env, encType())
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

function loadDeclFile(filePath: string, env: Env, defs: Map<string, Tree>): Env {
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
    // For recursive defs, the encoded form (omega combinator) doesn't work with
    // eager apply(). Register the compiled form (FIX-based) for evalToNative.
    if (sdecl.isRec) {
      const encodedData = unwrapData(env.get(sdecl.name)!)
      registerNativeBuiltinId(encodedData.id, compiled)
    }
  }
  return env
}

export function loadPrelude(): { cocEnv: Env, defs: Map<string, Tree> } {
  let env: Env = new Map()
  const defs = new Map<string, Tree>()

  // 1. Inject primitive types
  for (const [name, marker] of [["Tree", TREE_TYPE], ["Bool", BOOL_TYPE], ["Nat", NAT_TYPE]] as const) {
    env = new Map(env)
    env.set(name, wrap(marker, encType()))
    defs.set(name, marker)
  }

  // 2. Register primitive builtins (leaf, stem, fork, triage, true, false, etc.)
  env = registerBuiltins(PRIMITIVE_BUILTINS, env, defs)

  // 3. Load coc-prelude.disp (encType, encVar, wrap, unwrap)
  env = loadDeclFile(cocPreludePath(), env, defs)

  // 4. Register tree-native builtins (step functions, etc.)
  env = registerBuiltins(TREE_NATIVE_BUILTINS, env, defs)

  return { cocEnv: env, defs }
}

// Names whose tree data collides with CoC term encodings or other primitives.
// Exclude from name map to avoid the printer showing wrong names.
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
