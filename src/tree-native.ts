// Tree-native core operations: actual tree constants that perform computation
// via tree calculus triage when applied to arguments.
//
// These include:
// - Primitive destructors (FST, SND, CHILD)
// - CoC encoding constructors as trees (ENC_APP_T, ENC_LAM_T, ENC_PI_T)
// - 5-way term dispatch (TERM_CASE)
// - Fuel-based step functions (treeEqStep, whnfStep, convertibleStep, inferStep)
// - Builtin arrays for prelude registration

import { type Tree, LEAF, stem, fork, apply, I } from "./tree.js"
import { type Expr, eTree, eFvar, eApp, bracketAbstract, collapseAndEval, FIX } from "./compile.js"

// --- Expr-level helpers ---

function exprFork(a: Expr, b: Expr): Expr { return eApp(eApp(eTree(LEAF), a), b) }
function exprTriage(c: Expr, d: Expr, b: Expr): Expr { return exprFork(exprFork(c, d), b) }
function mkTriage(onLeaf: Tree, onStem: Tree, onFork: Tree): Tree { return fork(fork(onLeaf, onStem), onFork) }

// Bool elimination for tree-encoded bools: true=LEAF, false=stem(LEAF)
// exprBoolElim(bool, thenBranch, elseBranch) dispatches via triage:
//   leaf(true) → thenBranch, stem(false) → elseBranch, fork → thenBranch
function exprBoolElim(bool: Expr, thenBranch: Expr, elseBranch: Expr): Expr {
  return eApp(exprTriage(thenBranch, bracketAbstract("__be", elseBranch), bracketAbstract("__be1", bracketAbstract("__be2", thenBranch))), bool)
}

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

// FIX is imported from compile.ts (where it's also used by compileRecAndEval).

// --- TRIAGE: raw tree triage combinator ---
//
// triage c d b t: dispatch on tree t
//   t = leaf       → c
//   t = stem(u)    → d u
//   t = fork(u, v) → b u v
export const TRIAGE: Tree = compileTree(["c", "d", "b"],
  eApp(eApp(eTree(LEAF), eApp(eApp(eTree(LEAF), eFvar("c")), eFvar("d"))), eFvar("b")))

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

// --- Tree-encoded Bool constants ---
// true = LEAF (leaf), false = stem(LEAF) (stem of leaf)

const TT: Tree = LEAF
const FF: Tree = stem(LEAF)
const KFF: Tree = fork(LEAF, FF)        // K(FF): absorbs 1 arg, returns FF
const KKFF: Tree = fork(LEAF, KFF)      // K(KFF): absorbs 2 args, returns FF

// AND : Bool -> Bool -> Bool (triage-based)
// triage on a: leaf(true) → b, stem(false) → FF, fork → FF
const AND: Tree = compileTree(["a", "b"],
  eApp(exprTriage(
    eFvar("b"),
    bracketAbstract("_", eTree(FF)),
    bracketAbstract("_", bracketAbstract("_", eTree(FF)))
  ), eFvar("a")))

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
  // Tree-encoded bool dispatch via triage
  const body = exprBoolElim(isTarget, eTree(I), triageResult)

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
  const afterWhnf = exprBoolElim(whnfEq, eTree(TT), structuralCompare)

  // Bind aN = whnf(a), bN = whnf(b)
  const withBN = eApp(bracketAbstract("bN", afterWhnf), eApp(eFvar("whnfFn"), eFvar("b")))
  const withAN = eApp(bracketAbstract("aN", withBN), eApp(eFvar("whnfFn"), eFvar("a")))

  // Early equality short-circuit
  const earlyEq = eApp(eApp(eFvar("treeEqF"), eFvar("a")), eFvar("b"))
  const body = exprBoolElim(earlyEq, eTree(TT), withAN)

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

  // wrap(data, type) = fork(data, type)
  function exprWrap(data: Expr, type: Expr): Expr {
    return exprFork(data, type)
  }
  // unwrapData = FST (left of fork pair)
  function exprUnwrapData(w: Expr): Expr { return eApp(eTree(FST), w) }
  // unwrapType = SND (right of fork pair)
  function exprUnwrapType(w: Expr): Expr { return eApp(eTree(SND), w) }

  function exprEnvLookup(env: Expr, marker: Expr): Expr {
    const entV = fresh(), rstV = fresh()
    const eqResult = eApp(eApp(eFvar("eq"), eApp(eTree(FST), eFvar(entV))), marker)
    const handler = bracketAbstract(entV, bracketAbstract(rstV,
      exprBoolElim(eqResult, eApp(eTree(SND), eFvar(entV)), eFvar(rstV))))
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
  const WRAP_TYPE_TYPE: Tree = fork(LEAF, LEAF)  // wrap(encType(), encType()) = fork(LEAF, LEAF)

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

// --- Primitive eliminator data ---

// TRIAGE_DATA: {R c d b t} → triage dispatch (discards R, builds fork(fork(c,d),b), applies to t)
// K(TRIAGE) absorbs the erased R type parameter, then TRIAGE takes c, d, b, t.
const TRIAGE_DATA: Tree = fork(LEAF, TRIAGE)

// BOOL_ELIM_DATA: {R x y b} → triage on b: leaf(true)→x, stem(false)→y, fork→x
const BOOL_ELIM_DATA: Tree = compileTree(["R", "x", "y", "b"],
  eApp(exprTriage(
    eFvar("x"),
    bracketAbstract("_", eFvar("y")),
    bracketAbstract("_", bracketAbstract("_", eFvar("x")))
  ), eFvar("b")))

// NAT_ELIM_DATA: {R z s n} → triage on n: leaf(zero)→z, stem(n')→s(n'), fork→z
const NAT_ELIM_DATA: Tree = compileTree(["R", "z", "s", "n"],
  eApp(exprTriage(
    eFvar("z"),
    bracketAbstract("n_pred", eApp(eFvar("s"), eFvar("n_pred"))),
    bracketAbstract("_", bracketAbstract("_", eFvar("z")))
  ), eFvar("n")))

// tDelta: a specific leaf used as initial marker for convertible
const T_DELTA: Tree = LEAF

export interface TreeBuiltin { name: string; type: string; data: Tree }

// Primitive type constructors, eliminators, and essential builtins (loaded before COC_PRELUDE)
export const PRIMITIVE_BUILTINS: TreeBuiltin[] = [
  // Tree primitives
  { name: "leaf",     type: "Tree",                  data: LEAF },
  { name: "stem",     type: "Tree -> Tree",          data: LEAF },
  { name: "fork",     type: "Tree -> Tree -> Tree",  data: LEAF },
  { name: "triage",   type: "(R : Type) -> R -> (Tree -> R) -> (Tree -> Tree -> R) -> Tree -> R",
    data: TRIAGE_DATA },
  // Bool primitives
  { name: "true",     type: "Bool",                  data: LEAF },
  { name: "false",    type: "Bool",                  data: stem(LEAF) },
  { name: "boolElim", type: "(R : Type) -> R -> R -> Bool -> R",
    data: BOOL_ELIM_DATA },
  // Nat primitives
  { name: "zero",     type: "Nat",                   data: LEAF },
  { name: "succ",     type: "Nat -> Nat",            data: LEAF },
  { name: "natElim",  type: "(R : Type) -> R -> (Nat -> R) -> Nat -> R",
    data: NAT_ELIM_DATA },
  // Tree destructors
  { name: "tfst",     type: "Tree -> Tree",   data: FST },
  { name: "tsnd",     type: "Tree -> Tree",   data: SND },
  { name: "tchild",   type: "Tree -> Tree",   data: CHILD },
]

export const TREE_NATIVE_BUILTINS: TreeBuiltin[] = [
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
  { name: "tFix", type: "(Tree -> Tree) -> Tree", data: FIX },
  { name: "tDelta", type: "Tree", data: T_DELTA },
]
