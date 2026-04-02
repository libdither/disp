// Prelude loading: native-only registration of builtins and declaration processing.

import { type Tree, LEAF, stem, fork, apply } from "./tree.js"
import { type SExpr, parseLine } from "./parse.js"
import { type Expr, eTree, eFvar, eApp, bracketAbstract, collapseAndEval, compileAndEval, compileRecAndEval } from "./compile.js"
import { registerNativeBuiltinId, onClearNativeBuiltins } from "./native-utils.js"
import { PRIMITIVE_BUILTINS, TREE_NATIVE_BUILTINS } from "./tree-native.js"
import { type KnownDefs, TN_TYPE, TN_TREE, TN_BOOL, TN_NAT, tnArrow, annAscribe } from "./tree-native-checker.js"
import { nativeElabDecl, type NativeEnv, buildDirect, NativeElabError } from "./tree-native-elaborate.js"

interface TreeBuiltin { name: string; type: string; data: Tree }

// --- Hand-written dependent eliminator types ---
// These build proper dependent types as tree values using bracket abstraction.
// Required because natElim/boolElim/triage and their *Dep variants share data trees,
// and we need the correct dependent type for constant-motive coercion.

function exprFork(a: Expr, b: Expr): Expr { return eApp(eApp(eTree(LEAF), a), b) }

// S(K(P), LEAF): codomain function mapping n → P(stem(n)) = P(succ(n))
function exprStemCod(P: Expr): Expr {
  const KP = exprFork(eTree(LEAF), P)
  return exprFork(eApp(eTree(LEAF), KP), eTree(LEAF))
}

// forkCodomain(P): \u. fork(TN_TREE, S(K(P), stem(u)))
// = S(K(stem(TN_TREE)), S(K(stem(stem(K(P)))), LEAF))
function exprForkCod(P: Expr): Expr {
  const KP = exprFork(eTree(LEAF), P)
  const sKP = eApp(eTree(LEAF), KP)
  const ssKP = eApp(eTree(LEAF), sKP)
  const KssKP = exprFork(eTree(LEAF), ssKP)
  const sKssKP = eApp(eTree(LEAF), KssKP)
  const innerS = exprFork(sKssKP, eTree(LEAF))
  const sKsTree = eTree(stem(fork(LEAF, stem(TN_TREE))))
  return exprFork(sKsTree, innerS)
}

function exprK(x: Expr): Expr { return exprFork(eTree(LEAF), x) }

// (P : Nat -> Type) -> P(0) -> ((n:Nat) -> P(succ n)) -> (n:Nat) -> P(n)
function buildNatElimDepType(): Tree {
  const P = eFvar("P")
  const P0 = eApp(P, eTree(LEAF))
  const stepDom = exprFork(eTree(TN_NAT), exprStemCod(P))
  const resDom = exprFork(eTree(TN_NAT), P)
  const cod = exprFork(P0, exprK(exprFork(stepDom, exprK(resDom))))
  return fork(tnArrow(TN_NAT, TN_TYPE), collapseAndEval(bracketAbstract("P", cod)))
}

// (P : Bool -> Type) -> P(true) -> P(false) -> (b:Bool) -> P(b)
function buildBoolElimDepType(): Tree {
  const P = eFvar("P")
  const Ptrue = eApp(P, eTree(LEAF))
  const Pfalse = eApp(P, eTree(stem(LEAF)))
  const resDom = exprFork(eTree(TN_BOOL), P)
  const cod = exprFork(Ptrue, exprK(exprFork(Pfalse, exprK(resDom))))
  return fork(tnArrow(TN_BOOL, TN_TYPE), collapseAndEval(bracketAbstract("P", cod)))
}

// (P : Tree -> Type) -> P(leaf) -> ((u:Tree) -> P(stem u)) -> ((u v:Tree) -> P(fork u v)) -> (t:Tree) -> P(t)
function buildTriageDepType(): Tree {
  const P = eFvar("P")
  const Pleaf = eApp(P, eTree(LEAF))
  const stemH = exprFork(eTree(TN_TREE), exprStemCod(P))
  const forkH = exprFork(eTree(TN_TREE), exprForkCod(P))
  const res = exprFork(eTree(TN_TREE), P)
  const cod = exprFork(Pleaf, exprK(exprFork(stemH, exprK(exprFork(forkH, exprK(res))))))
  return fork(tnArrow(TN_TREE, TN_TYPE), collapseAndEval(bracketAbstract("P", cod)))
}

let _handWrittenTypes: Map<string, Tree> | null = null
function getHandWrittenTypes(): Map<string, Tree> {
  if (!_handWrittenTypes) {
    const natElimType = buildNatElimDepType()
    const boolElimType = buildBoolElimDepType()
    const triageType = buildTriageDepType()
    _handWrittenTypes = new Map([
      ["natElim", natElimType], ["natElimDep", natElimType],
      ["boolElim", boolElimType], ["boolElimDep", boolElimType],
      ["triage", triageType], ["triageDep", triageType],
    ])
  }
  return _handWrittenTypes
}

// --- Native-only builtin registration ---

function registerBuiltins(
  builtins: TreeBuiltin[],
  nativeEnv: NativeEnv, nativeDefs: KnownDefs, defs: Map<string, Tree>,
): void {
  const handWritten = getHandWrittenTypes()
  for (const builtin of builtins) {
    registerNativeBuiltinId(builtin.data.id)

    // Use hand-written types for dependent eliminators (fixes data-tree sharing overwrite)
    const hw = handWritten.get(builtin.name)
    let nativeType: Tree
    if (hw) {
      nativeType = hw
    } else {
      const typeSexpr = parseLine(builtin.type) as SExpr
      try {
        const result = buildDirect(typeSexpr, nativeEnv, TN_TYPE)
        nativeType = result.tree
      } catch {
        nativeType = TN_TREE // fallback: treat as Tree-typed
      }
    }

    nativeDefs.set(builtin.data.id, nativeType)
    nativeEnv.set(builtin.name, {
      tree: builtin.data,
      annTree: annAscribe(nativeType, builtin.data),
      type: nativeType,
    })
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
      const typeResult = buildDirect(type, nativeEnv, TN_TYPE)
      nativeType = typeResult.tree
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

