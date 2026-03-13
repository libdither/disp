// Tree-native core operations: actual tree constants that perform computation
// via tree calculus triage when applied to arguments.
//
// These include:
// - Primitive destructors (FST, SND, CHILD)
// - CoC encoding constructors as trees (ENC_APP_T, ENC_LAM_T, ENC_PI_T)
// - 5-way term dispatch (TERM_CASE)
// - Fuel-based step functions (treeEqStep, whnfStep, convertibleStep, inferStep)
// - CoC prelude definitions and builtin registration

import { type Tree, LEAF, stem, fork, apply, I } from "./tree.js"
import { type Expr, eTree, eFvar, eApp, bracketAbstract, collapseAndEval } from "./compile.js"
import { type SExpr, type SDecl, parseLine } from "./parse.js"
import {
  encType, K_SEL, K_STAR_SEL,
  wrap, unwrapData, unwrapType,
  buildWrapped, cocCheckDecl, type Env,
} from "./coc.js"

// --- Expr-level helpers ---

function exprFork(a: Expr, b: Expr): Expr { return eApp(eApp(eTree(LEAF), a), b) }
function exprTriage(c: Expr, d: Expr, b: Expr): Expr { return exprFork(exprFork(c, d), b) }
function mkTriage(onLeaf: Tree, onStem: Tree, onFork: Tree): Tree { return fork(fork(onLeaf, onStem), onFork) }

function compileTree(params: string[], body: Expr): Tree {
  let e = body
  for (let i = params.length - 1; i >= 0; i--) e = bracketAbstract(params[i], e)
  // Use collapseAndEval (eager apply) instead of collapse (structural treeApply).
  // This is critical for tree-native constants that embed complex tree values via eTree().
  // collapse uses treeApply which for fork(a,b) just builds fork(fork(a,b),g) structurally,
  // but this doesn't behave like apply(fork(a,b),g) at runtime. collapseAndEval eagerly
  // reduces all applications, producing trees that behave correctly when later applied.
  return collapseAndEval(e)
}

// --- Primitive tree destructors (single triage, no recursion) ---

// FST: fork(l,r) → l. Fork handler = K.
export const FST: Tree = mkTriage(LEAF, LEAF, stem(LEAF))
// SND: fork(l,r) → r. Fork handler = K*.
export const SND: Tree = mkTriage(LEAF, LEAF, fork(LEAF, I))
// CHILD: stem(u) → u. Stem handler = I.
export const CHILD: Tree = mkTriage(LEAF, I, LEAF)

// --- Encoding constructors as tree constants ---

export const ENC_APP_T: Tree = compileTree(["m", "n"],
  eApp(eTree(stem(LEAF)), exprFork(eFvar("m"), eFvar("n"))))
export const ENC_LAM_T: Tree = compileTree(["d", "b"],
  exprFork(eApp(eTree(LEAF), eFvar("d")), eFvar("b")))
export const ENC_PI_T: Tree = compileTree(["d", "b"],
  exprFork(exprFork(eFvar("d"), eTree(LEAF)), eFvar("b")))

// --- termCase: 5-way dispatch on encoded CoC terms ---

export const TERM_CASE: Tree = (() => {
  const appCase = eApp(eApp(eFvar("onApp"), eApp(eTree(FST), eFvar("right"))),
                                              eApp(eTree(SND), eFvar("right")))
  const lamCase = bracketAbstract("__d",
    eApp(eApp(eFvar("onLam"), eFvar("__d")), eFvar("right")))
  const piCase = bracketAbstract("__dl", bracketAbstract("__",
    eApp(eApp(eFvar("onPi"), eFvar("__dl")), eFvar("right"))))
  const innerTriage = eApp(exprTriage(appCase, lamCase, piCase), eFvar("left"))
  const forkHandler = bracketAbstract("left", bracketAbstract("right", innerTriage))
  const body = eApp(exprTriage(eFvar("onType"), eFvar("onVar"), forkHandler), eFvar("term"))
  return compileTree(["onType", "onVar", "onApp", "onLam", "onPi", "term"], body)
})()

// --- Compiled Bool and logic constants ---

const TT: Tree = compileTree(["R", "t", "f"], eFvar("t"))
const FF: Tree = compileTree(["R", "t", "f"], eFvar("f"))
const KFF: Tree = fork(LEAF, FF)        // {_} -> ff  (absorbs 1 arg)
const KKFF: Tree = fork(LEAF, KFF)      // {_ _} -> ff (absorbs 2 args)

// AND : Bool -> Bool -> Bool (eager — reduces to TT/FF structurally)
// and a b = a LEAF b FF
// This forces full evaluation: TT LEAF b FF → b, FF LEAF b FF → FF.
// Unlike the 5-arg version {a b R t f} -> ..., this produces structurally
// identical TT/FF trees, which is critical for tree-native comparisons.
const AND: Tree = compileTree(["a", "b"],
  eApp(eApp(eApp(eFvar("a"), eTree(LEAF)), eFvar("b")), eTree(FF)))

// --- treeEqStep: fuel-based structural tree equality ---
//
// treeEqStep : (Tree -> Tree -> Bool) -> Tree -> Tree -> Bool
//
// Pattern: triage on a, then triage on b in each case.
//   leaf,  leaf       → tt
//   leaf,  _          → ff
//   stem(ac), stem(bc) → self ac bc
//   stem,  _          → ff
//   fork(al,ar), fork(bl,br) → and (self al bl) (self ar br)
//   fork,  _          → ff
//
// Usage: fuel (Tree -> Tree -> Bool) treeEqStep ({x y} -> ff)
// where fuel is a Church numeral controlling recursion depth.

export const TREE_EQ_STEP: Tree = (() => {
  // Case a=leaf: triage on b → leaf:tt, stem(_):ff, fork(_,_):ff
  const aLeafCase = exprTriage(eTree(TT), eTree(KFF), eTree(KKFF))

  // Case a=stem(ac): triage on b → leaf:ff, stem(bc):self ac bc, fork(_,_):ff
  const aStemCase = bracketAbstract("ac",
    exprTriage(
      eTree(FF),
      bracketAbstract("bc", eApp(eApp(eFvar("self"), eFvar("ac")), eFvar("bc"))),
      eTree(KKFF)
    ))

  // Case a=fork(al,ar): triage on b → leaf:ff, stem(_):ff, fork(bl,br):and(self al bl)(self ar br)
  const aForkCase = bracketAbstract("al", bracketAbstract("ar",
    exprTriage(
      eTree(FF),
      eTree(KFF),
      bracketAbstract("bl", bracketAbstract("br",
        eApp(eApp(eTree(AND),
          eApp(eApp(eFvar("self"), eFvar("al")), eFvar("bl"))),
          eApp(eApp(eFvar("self"), eFvar("ar")), eFvar("br")))
      ))
    )))

  // Outer triage on a, then apply result to b
  const body = eApp(eApp(exprTriage(aLeafCase, aStemCase, aForkCase), eFvar("a")), eFvar("b"))
  return compileTree(["self", "a", "b"], body)
})()

// --- S combinator constructor: mkS f g = fork(stem(f), g) ---
const MK_S: Tree = compileTree(["f", "g"],
  eApp(eApp(eTree(LEAF), eApp(eTree(LEAF), eFvar("f"))), eFvar("g")))

// Expr helpers for building combinator trees inside step functions
// mkK(x) = fork(LEAF, x). Built as: apply(stem(LEAF), x).
function exprMkK(x: Expr): Expr { return eApp(eTree(stem(LEAF)), x) }
// mkS(f, g) = fork(stem(f), g). Built as: apply(apply(LEAF, apply(LEAF, f)), g).
function exprMkS(f: Expr, g: Expr): Expr {
  return eApp(eApp(eTree(LEAF), eApp(eTree(LEAF), f)), g)
}

// --- abstractOutStep: fuel-based bracket abstraction ---
//
// abstractOutStep : (Tree -> Tree -> Bool) -> Tree -> (Tree -> Tree) -> Tree -> Tree
//   eq     : equality predicate (treeEq with fixed fuel)
//   target : the subtree to replace
//   self   : recursive abstraction function
//   tree   : the tree to abstract over
//
// Result f satisfies: apply(f, x) = tree[target := x]
//
// Usage: abstractOut fuel eqFuel target tree =
//   fuel (Tree -> Tree) (abstractOutStep (treeEq eqFuel) target) tK tree

export const ABSTRACT_OUT_STEP: Tree = (() => {
  // Check if tree == target
  const isTarget = eApp(eApp(eFvar("eq"), eFvar("tree")), eFvar("target"))

  // K(LEAF) — constant leaf combinator
  const kLeaf = exprMkK(eTree(LEAF))

  // leaf: K(LEAF)
  const leafCase = kLeaf

  // stem(child): S(K(LEAF))(self(child))
  const stemCase = bracketAbstract("child",
    exprMkS(kLeaf, eApp(eFvar("self"), eFvar("child"))))

  // fork(left, right): S(S(K(LEAF))(self(left)))(self(right))
  const forkCase = bracketAbstract("left", bracketAbstract("right",
    exprMkS(
      exprMkS(kLeaf, eApp(eFvar("self"), eFvar("left"))),
      eApp(eFvar("self"), eFvar("right")))))

  // Triage for the "not target" case
  const triageResult = eApp(exprTriage(leafCase, stemCase, forkCase), eFvar("tree"))

  // Branch: isTarget ? I : triageResult
  // Church bool: isTarget LEAF I triageResult (eager, both branches computed)
  const body = eApp(eApp(eApp(isTarget, eTree(LEAF)), eTree(I)), triageResult)

  return compileTree(["eq", "target", "self", "tree"], body)
})()

// --- whnfStep: fuel-based WHNF of encoded CoC terms ---
//
// whnfStep : (Tree -> Tree) -> Tree -> Tree
//   self : recursive WHNF function
//   t    : encoded CoC term
//
// Only App(Lam(...), arg) reduces. Everything else is WHNF.
//
// Usage: whnf fuel t = fuel (Tree -> Tree) whnfStep tI t

export const WHNF_STEP: Tree = (() => {
  // "stuck" = encApp(funcR, appArg) — rebuilt application when func doesn't reduce
  const stuck = eApp(eApp(eTree(ENC_APP_T), eFvar("funcR")), eFvar("appArg"))

  // Inner termCase on funcR (= self(appFunc)):
  //   Type/Var/App/Pi → stuck (can't beta-reduce)
  //   Lam(dom, body) → self(apply(body, appArg))  — beta!
  const innerOnType = stuck
  const innerOnVar = bracketAbstract("_m", stuck)
  const innerOnApp = bracketAbstract("_f", bracketAbstract("_a", stuck))
  const innerOnLam = bracketAbstract("_dom", bracketAbstract("lamBody",
    eApp(eFvar("self"), eApp(eFvar("lamBody"), eFvar("appArg")))))
  const innerOnPi = bracketAbstract("_pd", bracketAbstract("_pb", stuck))

  const innerDispatch = eApp(eApp(eApp(eApp(eApp(eApp(
    eTree(TERM_CASE), innerOnType), innerOnVar), innerOnApp), innerOnLam), innerOnPi),
    eFvar("funcR"))

  // App handler: receives func and arg from outer termCase
  // Let-bind funcR = self(appFunc), then inner dispatch
  const appHandler = bracketAbstract("appFunc", bracketAbstract("appArg",
    eApp(
      bracketAbstract("funcR", innerDispatch),
      eApp(eFvar("self"), eFvar("appFunc"))
    )))

  // Outer termCase: only App does work, rest returns t unchanged
  const onType = eFvar("t")
  const onVar = bracketAbstract("_m", eFvar("t"))
  const onLam = bracketAbstract("_d", bracketAbstract("_b", eFvar("t")))
  const onPi = bracketAbstract("_d", bracketAbstract("_b", eFvar("t")))

  const body = eApp(eApp(eApp(eApp(eApp(eApp(
    eTree(TERM_CASE), onType), onVar), appHandler), onLam), onPi),
    eFvar("t"))

  return compileTree(["self", "t"], body)
})()

// --- convertibleStep: fuel-based convertibility of encoded CoC terms ---
//
// convertibleStep : (Tree -> Tree) -> (Tree -> Tree -> Bool) -> (Tree -> Tree -> Tree -> Bool) -> Tree -> Tree -> Tree -> Bool
//   whnfFn  : WHNF function (with fixed fuel)
//   treeEqF : tree equality (with fixed fuel)
//   self    : recursive convertibility (marker, a, b → Bool)
//   marker  : seed for fresh neutrals under binders
//   a, b    : encoded CoC terms
//
// Usage: convertible fuel whnfFuel eqFuel a b =
//   fuel (Tree -> Tree -> Tree -> Bool)
//     (convertibleStep (whnf whnfFuel) (treeEq eqFuel))
//     ({m x y} -> ff) tDelta a b

export const CONVERTIBLE_STEP: Tree = (() => {
  // Compare under a binder: apply bodies to encVar(marker), recurse with stem(marker)
  function binderCompare(bodyA: string, bodyB: string): Expr {
    const neutral = eApp(eTree(LEAF), eFvar("marker"))  // stem(marker) = encVar(marker)
    const nextMarker = eApp(eTree(LEAF), eFvar("marker"))
    return eApp(eApp(eApp(eFvar("self"), nextMarker),
      eApp(eFvar(bodyA), neutral)),
      eApp(eFvar(bodyB), neutral))
  }

  // Helper: dispatch on bN using TERM_CASE
  function termCaseOnBN(
    onType: Expr,
    onVar: Expr,
    onApp: Expr,
    onLam: Expr,
    onPi: Expr,
  ): Expr {
    return eApp(eApp(eApp(eApp(eApp(eApp(
      eTree(TERM_CASE), onType), onVar), onApp), onLam), onPi),
      eFvar("bN"))
  }

  // aN=Type: bN must be Type → TT, all other bN tags → FF
  const aTypeCase = termCaseOnBN(
    eTree(TT),
    bracketAbstract("_", eTree(FF)),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF)))
  )

  // aN=Var(ma): bN must be Var(mb) → treeEq(ma, mb), else FF
  const aVarCase = bracketAbstract("ma", termCaseOnBN(
    eTree(FF),
    bracketAbstract("mb", eApp(eApp(eFvar("treeEqF"), eFvar("ma")), eFvar("mb"))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF)))
  ))

  // aN=App(fa,xa): bN must be App(fb,xb) → AND(self(marker,fa,fb))(self(marker,xa,xb))
  const aAppCase = bracketAbstract("fa", bracketAbstract("xa", termCaseOnBN(
    eTree(FF),
    bracketAbstract("_", eTree(FF)),
    bracketAbstract("fb", bracketAbstract("xb",
      eApp(eApp(eTree(AND),
        eApp(eApp(eApp(eFvar("self"), eFvar("marker")), eFvar("fa")), eFvar("fb"))),
        eApp(eApp(eApp(eFvar("self"), eFvar("marker")), eFvar("xa")), eFvar("xb"))))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF)))
  )))

  // aN=Lam(da,ba): bN must be Lam(db,bb) → AND(self(marker,da,db))(binderCompare(ba,bb))
  const aLamCase = bracketAbstract("da", bracketAbstract("ba", termCaseOnBN(
    eTree(FF),
    bracketAbstract("_", eTree(FF)),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("db", bracketAbstract("bb",
      eApp(eApp(eTree(AND),
        eApp(eApp(eApp(eFvar("self"), eFvar("marker")), eFvar("da")), eFvar("db"))),
        binderCompare("ba", "bb")))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF)))
  )))

  // aN=Pi(pda,pba): bN must be Pi(pdb,pbb) → AND(self(marker,pda,pdb))(binderCompare(pba,pbb))
  const aPiCase = bracketAbstract("pda", bracketAbstract("pba", termCaseOnBN(
    eTree(FF),
    bracketAbstract("_", eTree(FF)),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("_", bracketAbstract("_", eTree(FF))),
    bracketAbstract("pdb", bracketAbstract("pbb",
      eApp(eApp(eTree(AND),
        eApp(eApp(eApp(eFvar("self"), eFvar("marker")), eFvar("pda")), eFvar("pdb"))),
        binderCompare("pba", "pbb"))))
  )))

  // Structural comparison: termCase on aN
  const structuralCompare = eApp(eApp(eApp(eApp(eApp(eApp(
    eTree(TERM_CASE), aTypeCase), aVarCase), aAppCase), aLamCase), aPiCase),
    eFvar("aN"))

  // After WHNF: check treeEq on normalized terms, else structural compare
  const whnfEq = eApp(eApp(eFvar("treeEqF"), eFvar("aN")), eFvar("bN"))
  const afterWhnf = eApp(eApp(eApp(whnfEq, eTree(LEAF)), eTree(TT)), structuralCompare)

  // Bind aN = whnf(a), bN = whnf(b)
  const withBN = eApp(bracketAbstract("bN", afterWhnf), eApp(eFvar("whnfFn"), eFvar("b")))
  const withAN = eApp(bracketAbstract("aN", withBN), eApp(eFvar("whnfFn"), eFvar("a")))

  // Early equality short-circuit
  const earlyEq = eApp(eApp(eFvar("treeEqF"), eFvar("a")), eFvar("b"))
  const body = eApp(eApp(eApp(earlyEq, eTree(LEAF)), eTree(TT)), withAN)

  return compileTree(["whnfFn", "treeEqF", "self", "marker", "a", "b"], body)
})()

// --- TYPECHECK: fold-based CoC type inference step function ---
//
// inferStep : (Tree→Tree) → (Tree→Tree→Tree) → (Tree→Tree→Bool) →
//             (Tree→Tree→Tree) → Tree → Tree → Tree
//   whnfFn  : WHNF function
//   absOutFn: bracket abstraction function
//   eqFn    : tree equality function
//   self    : recursive inference (env → term → wrapped), provided by fold
//   env     : Church-list environment
//   term    : encoded CoC term
//
// Usage: fuel (env→term→wrapped) (inferStep whnf absOut eq) base env term
// where fuel is a Church numeral controlling recursion depth.

export const TYPECHECK: Tree = (() => {
  // --- Expr helpers ---
  let uid = 0
  function fresh() { return `__tc${uid++}` }

  function exprWrap(data: Expr, type: Expr): Expr {
    const v = fresh()
    return bracketAbstract(v, eApp(eApp(eFvar(v), data), type))
  }
  function exprUnwrapData(w: Expr): Expr { return eApp(w, eTree(K_SEL)) }
  function exprUnwrapType(w: Expr): Expr { return eApp(w, eTree(K_STAR_SEL)) }

  function exprEnvLookup(env: Expr, marker: Expr): Expr {
    const entV = fresh(), rstV = fresh()
    const handler = bracketAbstract(entV, bracketAbstract(rstV,
      eApp(eApp(eApp(
        eApp(eApp(eFvar("eq"), eApp(eTree(FST), eFvar(entV))), marker),
        eTree(LEAF)),
        eApp(eTree(SND), eFvar(entV))),
        eFvar(rstV))))
    return eApp(eApp(eApp(env, eTree(LEAF)), eTree(LEAF)), handler)
  }

  function exprEnvCons(entry: Expr, env: Expr): Expr {
    const rV = fresh(), nV = fresh(), cV = fresh()
    return bracketAbstract(rV, bracketAbstract(nV, bracketAbstract(cV,
      eApp(eApp(eFvar(cV), entry),
        eApp(eApp(eApp(env, eFvar(rV)), eFvar(nV)), eFvar(cV))))))
  }

  function exprFreshMarker(term: Expr): Expr {
    return eApp(eApp(eTree(LEAF), term), term) // fork(term, term)
  }
  function exprEncVar(marker: Expr): Expr {
    return eApp(eTree(LEAF), marker) // stem(marker)
  }

  // Pre-computed wrap(Type, Type) as a concrete tree
  const WRAP_TYPE_TYPE: Tree = wrap(encType(), encType())

  // --- Build the body with "self" as a free variable ---
  // self(env, term) is the recursive call (fold handles recursion depth)
  function selfCall(env: Expr, term: Expr): Expr {
    return eApp(eApp(eFvar("self"), env), term)
  }

  // Type case
  const typeCase = eTree(WRAP_TYPE_TYPE)

  // Var(m): envLookup
  const varCase = bracketAbstract("__vm",
    exprEnvLookup(eFvar("env"), eFvar("__vm")))

  // App(func, arg)
  const appCase = (() => {
    const fwV = fresh(), ptV = fresh(), awV = fresh()
    return bracketAbstract("__af", bracketAbstract("__ax",
      eApp(bracketAbstract(fwV,
        eApp(bracketAbstract(ptV,
          eApp(bracketAbstract(awV,
            (() => {
              const argData = exprUnwrapData(eFvar(awV))
              const funcData = exprUnwrapData(eFvar(fwV))
              const piBody = eApp(eTree(SND), eFvar(ptV))
              return exprWrap(
                eApp(eApp(eTree(ENC_APP_T), funcData), argData),
                eApp(piBody, argData))
            })()
          ), selfCall(eFvar("env"), eFvar("__ax")))
        ), eApp(eFvar("whnf"), exprUnwrapType(eFvar(fwV))))
      ), selfCall(eFvar("env"), eFvar("__af")))))
  })()

  // Lam(dom, body)
  const lamCase = (() => {
    const dwV = fresh(), lmV = fresh(), lnV = fresh(), bwV = fresh()
    return bracketAbstract("__ld", bracketAbstract("__lb",
      eApp(bracketAbstract(dwV,
        eApp(bracketAbstract(lmV,
          eApp(bracketAbstract(lnV,
            (() => {
              const domData = exprUnwrapData(eFvar(dwV))
              const entry = eApp(eApp(eTree(LEAF), eFvar(lmV)),
                exprWrap(eFvar(lnV), domData))
              const extEnv = exprEnvCons(entry, eFvar("env"))
              return eApp(bracketAbstract(bwV,
                (() => {
                  const bodyData = exprUnwrapData(eFvar(bwV))
                  const bodyType = exprUnwrapType(eFvar(bwV))
                  const abstBody = eApp(eApp(eFvar("absOut"), eFvar(lnV)), bodyData)
                  const abstType = eApp(eApp(eFvar("absOut"), eFvar(lnV)), bodyType)
                  return exprWrap(
                    eApp(eApp(eTree(ENC_LAM_T), domData), abstBody),
                    eApp(eApp(eTree(ENC_PI_T), domData), abstType))
                })()
              ), selfCall(extEnv, eApp(eFvar("__lb"), eFvar(lnV))))
            })()
          ), exprEncVar(eFvar(lmV)))
        ), exprFreshMarker(eFvar("term")))
      ), selfCall(eFvar("env"), eFvar("__ld")))))
  })()

  // Pi(dom, body)
  const piCase = (() => {
    const dwV = fresh(), pmV = fresh(), pnV = fresh(), cwV = fresh()
    return bracketAbstract("__pd", bracketAbstract("__pb",
      eApp(bracketAbstract(dwV,
        eApp(bracketAbstract(pmV,
          eApp(bracketAbstract(pnV,
            (() => {
              const domData = exprUnwrapData(eFvar(dwV))
              const entry = eApp(eApp(eTree(LEAF), eFvar(pmV)),
                exprWrap(eFvar(pnV), domData))
              const extEnv = exprEnvCons(entry, eFvar("env"))
              return eApp(bracketAbstract(cwV,
                (() => {
                  const codData = exprUnwrapData(eFvar(cwV))
                  const abstCod = eApp(eApp(eFvar("absOut"), eFvar(pnV)), codData)
                  return exprWrap(
                    eApp(eApp(eTree(ENC_PI_T), domData), abstCod),
                    eTree(LEAF))
                })()
              ), selfCall(extEnv, eApp(eFvar("__pb"), eFvar(pnV))))
            })()
          ), exprEncVar(eFvar(pmV)))
        ), exprFreshMarker(eFvar("term")))
      ), selfCall(eFvar("env"), eFvar("__pd")))))
  })()

  // termCase dispatch
  const dispatch = eApp(eApp(eApp(eApp(eApp(eApp(
    eTree(TERM_CASE), typeCase), varCase), appCase), lamCase), piCase),
    eFvar("term"))

  // Fold-based step: [whnf][absOut][eq][self][env][term] → body
  // Usage: fuel (env→term→wrapped) (inferStep whnf absOut eq) base env term
  // where fuel is a Church numeral controlling recursion depth.
  return compileTree(["whnf", "absOut", "eq", "self", "env", "term"], dispatch)
})()

// ============================================================
// CoC Prelude: Church-encoded Tree and encoding operations
// ============================================================

export const COC_PRELUDE: string[] = [
  "let Tree : Type := (R : Type) -> R -> (R -> R) -> (R -> R -> R) -> R",
  "let leaf : Tree := {R c d b} -> c",
  "let stem : Tree -> Tree := {t R c d b} -> d (t R c d b)",
  "let fork : Tree -> Tree -> Tree := {l r R c d b} -> b (l R c d b) (r R c d b)",
  "let triage : (R : Type) -> R -> (R -> R) -> (R -> R -> R) -> Tree -> R := {R c d b t} -> t R c d b",
  "let encType : Tree := leaf",
  "let encVar : Tree -> Tree := stem",
  "let encApp : Tree -> Tree -> Tree := {m n} -> fork leaf (fork m n)",
  "let encLam : Tree -> Tree -> Tree := {domain body} -> fork (stem domain) body",
  "let encPi : Tree -> Tree -> Tree := {domain body} -> fork (fork domain leaf) body",
  "let Wrapped : Type := (R : Type) -> (Tree -> Tree -> R) -> R",
  "let wrap : Tree -> Tree -> Wrapped := {d t R sel} -> sel d t",
  "let unwrapData : Wrapped -> Tree := {w} -> w Tree ({d t} -> d)",
  "let unwrapType : Wrapped -> Tree := {w} -> w Tree ({d t} -> t)",
  "let Bool : Type := (R : Type) -> R -> R -> R",
  "let tt : Bool := {R t f} -> t",
  "let ff : Bool := {R t f} -> f",
]

interface TreeBuiltin { name: string; type: string; data: Tree }

export const TREE_NATIVE_BUILTINS: TreeBuiltin[] = [
  { name: "tfst",     type: "Tree -> Tree",   data: FST },
  { name: "tsnd",     type: "Tree -> Tree",   data: SND },
  { name: "tchild",   type: "Tree -> Tree",   data: CHILD },
  { name: "tEncApp",  type: "Tree -> Tree -> Tree", data: ENC_APP_T },
  { name: "tEncLam",  type: "Tree -> Tree -> Tree", data: ENC_LAM_T },
  { name: "tEncPi",   type: "Tree -> Tree -> Tree", data: ENC_PI_T },
  { name: "termCase", type: "Tree -> (Tree -> Tree) -> (Tree -> Tree -> Tree) -> (Tree -> Tree -> Tree) -> (Tree -> Tree -> Tree) -> Tree -> Tree",
    data: TERM_CASE },
  { name: "treeEqStep", type: "(Tree -> Tree -> Bool) -> Tree -> Tree -> Bool",
    data: TREE_EQ_STEP },
  { name: "tI", type: "Tree -> Tree", data: I },
  { name: "tK", type: "Tree -> Tree", data: stem(LEAF) },
  { name: "tS", type: "Tree -> Tree -> Tree", data: MK_S },
  { name: "abstractOutStep",
    type: "(Tree -> Tree -> Bool) -> Tree -> (Tree -> Tree) -> Tree -> Tree",
    data: ABSTRACT_OUT_STEP },
  { name: "whnfStep", type: "(Tree -> Tree) -> Tree -> Tree", data: WHNF_STEP },
  { name: "convertibleStep",
    type: "(Tree -> Tree) -> (Tree -> Tree -> Bool) -> (Tree -> Tree -> Tree -> Bool) -> Tree -> Tree -> Tree -> Bool",
    data: CONVERTIBLE_STEP },
  { name: "inferStep",
    type: "(Tree -> Tree) -> (Tree -> Tree -> Tree) -> (Tree -> Tree -> Bool) -> (Tree -> Tree -> Tree) -> Tree -> Tree -> Tree",
    data: TYPECHECK },
  // Actual tree constructors (not Church-encoded)
  // LEAF is the tree calculus constructor △: apply(LEAF,x)=stem(x), apply(apply(LEAF,x),y)=fork(x,y)
  { name: "tLeaf", type: "Tree", data: LEAF },
  { name: "tStem", type: "Tree -> Tree", data: LEAF },
  { name: "tFork", type: "Tree -> Tree -> Tree", data: LEAF },
]

export function loadCocPrelude(env: Env): Env {
  for (const decl of COC_PRELUDE) {
    const parsed = parseLine(decl)
    if (!("isRec" in parsed)) continue
    const sdecl = parsed as SDecl
    const result = cocCheckDecl(env, sdecl.name, sdecl.type, sdecl.value, sdecl.isRec)
    env = result.env
  }
  for (const builtin of TREE_NATIVE_BUILTINS) {
    const typeWrapped = buildWrapped(parseLine(builtin.type) as SExpr, env, encType())
    const typeData = unwrapData(typeWrapped)
    env = new Map(env)
    env.set(builtin.name, wrap(builtin.data, typeData))
  }
  return env
}

// Names whose tree data collides with CoC term encodings (e.g. tLeaf/tStem/tFork
// are all LEAF, which is also encType()). Exclude from name map to avoid the
// printer showing "tLeaf" where it should show "Type".
const NAME_MAP_SKIP = new Set(["tLeaf", "tStem", "tFork"])

export function buildNameMap(env: Env): Map<number, string> {
  const map = new Map<number, string>()
  for (const [name, wrapped] of env) {
    if (NAME_MAP_SKIP.has(name)) continue
    const data = unwrapData(wrapped)
    if (!map.has(data.id)) map.set(data.id, name)
  }
  return map
}
