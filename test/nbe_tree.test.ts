// NbE as tree-calculus programs — running on the unmodified tree.ts runtime
import { describe, it, expect } from "vitest"
import { Tree, LEAF, stem, fork, apply, treeApply, treeEqual, FAST_EQ, I, K } from "../src/tree"

// ================================================================
// Compilation: lambda calculus → bracket-abstracted tree
// ================================================================

type Cir =
  | { tag: "lit"; t: Tree }
  | { tag: "var"; name: string }
  | { tag: "app"; f: Cir; x: Cir }
  | { tag: "lam"; x: string; body: Cir }
  | { tag: "S" } | { tag: "K" } | { tag: "I" }

const S_TREE = fork(stem(fork(LEAF, LEAF)), LEAF)

function containsFree(e: Cir, name: string): boolean {
  switch (e.tag) {
    case "lit": case "S": case "K": case "I": return false
    case "var": return e.name === name
    case "app": return containsFree(e.f, name) || containsFree(e.x, name)
    case "lam": return e.x === name ? false : containsFree(e.body, name)
  }
}

// Optimized bracket abstraction: η-reduction + K-composition
function abstractName(name: string, body: Cir): Cir {
  if (!containsFree(body, name)) return { tag: "app", f: { tag: "K" }, x: body }
  switch (body.tag) {
    case "var": return { tag: "I" }
    case "app": {
      // η-optimization: [x](f x) where x ∉ f → f
      if (body.x.tag === "var" && body.x.name === name && !containsFree(body.f, name))
        return body.f

      const af = abstractName(name, body.f)
      const ag = abstractName(name, body.x)

      // S (K p) I → p  (η-reduction)
      if (af.tag === "app" && af.f.tag === "K" && ag.tag === "I") return af.x

      // S (K p) (K q) → K (p q)  (K-composition: compile-time eval)
      if (af.tag === "app" && af.f.tag === "K" && ag.tag === "app" && ag.f.tag === "K")
        return { tag: "app", f: { tag: "K" }, x: { tag: "app", f: af.x, x: ag.x } }

      return { tag: "app", f: { tag: "app", f: { tag: "S" }, x: af }, x: ag }
    }
    case "lam": return abstractName(name, abstractName(body.x, body.body))
    default: throw new Error("unreachable")
  }
}

function eliminateLams(e: Cir): Cir {
  switch (e.tag) {
    case "lit": case "var": case "S": case "K": case "I": return e
    case "app": return { tag: "app", f: eliminateLams(e.f), x: eliminateLams(e.x) }
    case "lam": return abstractName(e.x, eliminateLams(e.body))
  }
}

function cirToTree(e: Cir): Tree {
  switch (e.tag) {
    case "lit": return e.t
    case "var": throw new Error(`unresolved: ${e.name}`)
    case "I": return I
    case "K": return K
    case "S": return S_TREE
    case "app": {
      if (e.f.tag === "app" && e.f.f.tag === "S") return fork(stem(cirToTree(e.f.x)), cirToTree(e.x))
      if (e.f.tag === "K") return fork(LEAF, cirToTree(e.x))
      if (e.f.tag === "I") return cirToTree(e.x)
      return apply(cirToTree(e.f), cirToTree(e.x), { remaining: 50_000_000 })
    }
  }
}


// ================================================================
// DSL
// ================================================================

const lit = (t: Tree): Cir => ({ tag: "lit", t })
const v = (name: string): Cir => ({ tag: "var", name })
const a = (f: Cir, ...xs: Cir[]): Cir => xs.reduce((acc, x): Cir => ({ tag: "app", f: acc, x }), f)
const lam = (names: string, body: Cir): Cir =>
  names.split(" ").reverse().reduce((b, n): Cir => ({ tag: "lam", x: n, body: b }), body)
// thunk: wrap expr in {_} -> expr for deferred evaluation
const thunk = (body: Cir): Cir => lam("_d", body)

function compile(expr: Cir, scope: Record<string, Tree> = {}): Tree {
  function resolve(e: Cir): Cir {
    if (e.tag === "var" && e.name in scope) return { tag: "lit", t: scope[e.name] }
    if (e.tag === "app") return { tag: "app", f: resolve(e.f), x: resolve(e.x) }
    if (e.tag === "lam") return { tag: "lam", x: e.x, body: resolve(e.body) }
    return e
  }
  return cirToTree(eliminateLams(resolve(expr)))
}

function ap(f: Tree, ...xs: Tree[]): Tree {
  const budget = { remaining: 10_000_000 }
  return xs.reduce((acc, x) => apply(acc, x, budget), f)
}
function apBig(f: Tree, ...xs: Tree[]): Tree {
  const budget = { remaining: 50_000_000 }
  return xs.reduce((acc, x) => apply(acc, x, budget), f)
}

// ================================================================
// Fundamental combinators
// ================================================================

const TT = LEAF
const FF = stem(LEAF)

// triage(on_leaf, on_stem, on_fork) builds a triage combinator
// apply(result, leaf) → on_leaf
// apply(result, stem(u)) → apply(on_stem, u)
// apply(result, fork(u,v)) → apply(apply(on_fork, u), v)
function mkTriage(onLeaf: Tree, onStem: Tree, onFork: Tree): Tree {
  return fork(fork(onLeaf, onStem), onFork)
}

// ite2(then, else, cond): EAGER — both branches evaluated before dispatch.
// cond=TT→then, cond=FF→else
const ite2 = compile(lam("then_v else_v cond",
  a(a(a(lit(LEAF),
    a(a(lit(LEAF), v("then_v")),
      a(lit(K), v("else_v")))),
    lit(LEAF)),
    v("cond"))
))

// ited(then_thunk, else_thunk, cond): DEFERRED — branches are {_}->value thunks.
// Only the chosen branch is forced (by applying to LEAF after dispatch).
// ited(t, e, cond) = ite2(t, e, cond) LEAF
//   cond=TT → t, then apply t to LEAF → t(LEAF) = value
//   cond=FF → e, then apply e to LEAF → e(LEAF) = value
const ited = compile(lam("then_thunk else_thunk cond",
  a(a(a(lit(LEAF),
    a(a(lit(LEAF), v("then_thunk")),
      a(lit(K), v("else_thunk")))),
    lit(LEAF)),
    v("cond"),
    lit(LEAF))
))

// first(fork(a,b)) = a, second(fork(a,b)) = b
// Using triage: fork case gives us both children
const pair_fst = compile(lam("p",
  a(lit(mkTriage(LEAF, I, compile(lam("l r", v("l"))))), v("p"))
))

const pair_snd = compile(lam("p",
  a(lit(mkTriage(LEAF, I, compile(lam("l r", v("r"))))), v("p"))
))

// ================================================================
// Tag infrastructure
// ================================================================

const TAG_ROOT = fork(stem(stem(LEAF)), stem(LEAF))
const KV_LAM = LEAF
const KV_HYP = stem(LEAF)
const KV_STUCK = fork(LEAF, LEAF)

function tagged(kind: Tree, payload: Tree): Tree {
  return fork(fork(TAG_ROOT, kind), payload)
}
function mkVLam(meta: Tree, body: Tree): Tree { return tagged(KV_LAM, fork(meta, body)) }
function mkVHyp(type: Tree, id: Tree): Tree { return tagged(KV_HYP, fork(type, id)) }
function mkVStuck(head: Tree, arg: Tree): Tree { return tagged(KV_STUCK, fork(head, arg)) }

// is_tagged: checks fork(fork(TAG_ROOT, _), _)
const is_tagged_prog = compile(lam("t",
  a(lit(mkTriage(
    FF,                   // leaf → FF
    fork(LEAF, FF),       // stem → K(FF)
    compile(lam("l r",    // fork(l,r) →
      a(lit(mkTriage(
        FF,
        fork(LEAF, FF),
        compile(lam("ll lr", a(a(lit(FAST_EQ), v("ll")), lit(TAG_ROOT))))
      )), v("l"))
    ))
  )), v("t"))
))

// tag_kind: t.left.right
const tag_kind_prog = compile(lam("t",
  a(lit(mkTriage(LEAF, I,
    compile(lam("l r",
      a(lit(mkTriage(LEAF, I,
        compile(lam("ll lr", v("lr")))
      )), v("l"))
    ))
  )), v("t"))
))

// tag_payload: t.right
const tag_payload_prog = compile(lam("t",
  a(lit(mkTriage(LEAF, I, compile(lam("l r", v("r"))))), v("t"))
))

// Kind-specific recognizers (deferred: only evaluate kind check if tagged)
function mkKindCheck(kindTag: Tree): Tree {
  return compile(lam("t",
    a(lit(ited),
      thunk(a(a(lit(FAST_EQ), a(lit(tag_kind_prog), v("t"))), lit(kindTag))),
      thunk(lit(FF)),
      a(lit(is_tagged_prog), v("t")))
  ))
}

const is_vhyp = mkKindCheck(KV_HYP)
const is_vlam = mkKindCheck(KV_LAM)
const is_vstuck = mkKindCheck(KV_STUCK)
const is_neutral = compile(lam("t",
  a(lit(ited), thunk(lit(TT)), thunk(a(lit(is_vstuck), v("t"))), a(lit(is_vhyp), v("t")))
))

// Payload accessors
const vlam_meta = compile(lam("t", a(lit(pair_fst), a(lit(tag_payload_prog), v("t")))))
const vlam_body = compile(lam("t", a(lit(pair_snd), a(lit(tag_payload_prog), v("t")))))
const vhyp_type = vlam_meta  // same structure: first of payload
const vhyp_id = vlam_body    // same structure: second of payload
const vstuck_head = vlam_meta
const vstuck_arg = vlam_body

// mk_vstuck as tree program
const mk_vstuck_prog = compile(lam("h arg",
  a(a(lit(LEAF), a(a(lit(LEAF), lit(TAG_ROOT)), lit(KV_STUCK))),
    a(a(lit(LEAF), v("h")), v("arg")))
))

// ================================================================
// wait + fix: tree-calculus recursion
// ================================================================
// wait a b c = a b c, but wait(a)(b) does NOT evaluate a(b) eagerly.
// It defers until c arrives. This is the key to non-divergent recursion.
//
// wait = \a \b \c -> △ (△ a) (△ △ c) b
//
// fix = \f -> wait m (\x -> f (wait m x))  where m = \x -> x x

const wait_tree = compile(lam("a b c",
  a(a(a(lit(LEAF), a(lit(LEAF), v("a"))),       // stem(stem(a))
     a(a(lit(LEAF), lit(LEAF)), v("c"))),         // K(c) = fork(LEAF, c)
   v("b"))                                        // applied to b
))

const m_tree = compile(lam("x", a(v("x"), v("x"))))  // self-applicator

const fix_tree = compile(lam("f",
  a(a(lit(wait_tree), lit(m_tree)),               // wait m
    lam("x", a(v("f"),                             // \x -> f (wait m x)
      a(a(lit(wait_tree), lit(m_tree)), v("x")))))
))

// ================================================================
// napply_simple: tag dispatch, no H-rule
// ================================================================

const napply_simple = ap(fix_tree, compile(lam("self f x",
  a(lit(ited),
    thunk(a(a(lit(mk_vstuck_prog), v("f")), v("x"))),
    thunk(a(lit(ited),
      thunk(a(a(lit(mk_vstuck_prog), v("f")), v("x"))),
      thunk(a(lit(ited),
        thunk(a(a(lit(vlam_body), v("f")), v("x"))),
        thunk(a(v("f"), v("x"))),
        a(lit(is_vlam), v("f")))),
      a(lit(is_vstuck), v("f")))),
    a(lit(is_vhyp), v("f")))
)))

// ================================================================
// Metadata: Pi and Universe reflection
// ================================================================

const PI_TAG = stem(stem(LEAF))
const UNIV_TAG = fork(LEAF, stem(LEAF))
const ERROR_VAL = stem(stem(stem(stem(LEAF))))  // sentinel for type_of_neutral failure

// Pi metadata: vlam_meta = fork(PI_TAG, fork(domain, codomain_fn))
function mkPiMeta(domain: Tree, codomainFn: Tree): Tree {
  return fork(PI_TAG, fork(domain, codomainFn))
}

// is_pi(v): is_vlam(v) AND first(vlam_meta(v)) == PI_TAG
const is_pi = compile(lam("v",
  a(lit(ite2),
    a(a(lit(FAST_EQ), a(lit(pair_fst), a(lit(vlam_meta), v("v")))), lit(PI_TAG)),
    lit(FF),
    a(lit(is_vlam), v("v")))
))

// pi_dom(v): first(second(vlam_meta(v)))
const pi_dom = compile(lam("v",
  a(lit(pair_fst), a(lit(pair_snd), a(lit(vlam_meta), v("v"))))
))

// pi_cod_fn(v): second(second(vlam_meta(v))) — the codomain VLam
const pi_cod_fn = compile(lam("v",
  a(lit(pair_snd), a(lit(pair_snd), a(lit(vlam_meta), v("v"))))
))

// is_universe(v): is_vlam(v) AND first(vlam_meta(v)) == UNIV_TAG
const is_universe = compile(lam("v",
  a(lit(ite2),
    a(a(lit(FAST_EQ), a(lit(pair_fst), a(lit(vlam_meta), v("v")))), lit(UNIV_TAG)),
    lit(FF),
    a(lit(is_vlam), v("v")))
))

// universe_rank(v): second(vlam_meta(v))
const universe_rank = compile(lam("v",
  a(lit(pair_snd), a(lit(vlam_meta), v("v")))
))

// ================================================================
// type_of_neutral: spine inference
// ================================================================

// type_of_neutral takes napply_fn as ARGUMENT (not embedded literal) to avoid tree blowup
const type_of_neutral_core = ap(fix_tree, compile(lam("self napply_fn v",
  a(lit(ited),
    thunk(a(lit(vhyp_type), v("v"))),
    thunk(a(lit(ited),
      thunk(a(lit(ited),
        thunk(a(a(v("napply_fn"),
            a(lit(pi_cod_fn), a(a(v("self"), v("napply_fn")), a(lit(vstuck_head), v("v"))))),
          a(lit(vstuck_arg), v("v")))),
        thunk(lit(ERROR_VAL)),
        a(lit(is_pi), a(a(v("self"), v("napply_fn")), a(lit(vstuck_head), v("v")))))),
      thunk(lit(ERROR_VAL)),
      a(lit(is_vstuck), v("v")))),
    a(lit(is_vhyp), v("v")))
)))
// Deferred partial application via wait
const type_of_neutral = ap(wait_tree, type_of_neutral_core, napply_simple)
// wait(ton_core)(napply_simple)(v) = ton_core(napply_simple)(v)

// ================================================================
// conv: structural Val equality
// ================================================================

// and(a, b) = ite2(b, FF, a) — strict but correct
const and_tree = compile(lam("a b", a(lit(ite2), v("b"), lit(FF), v("a"))))

// Nat encoding helpers for depth counter
const ZERO_N = LEAF
const succ_n = compile(lam("n", a(a(lit(LEAF), lit(LEAF)), v("n"))))  // fork(LEAF, n)

// is_leaf/is_stem/is_fork for DATA (non-tagged) values
// (is_data_leaf removed — using is_tree_leaf instead)

// Simpler approach: check NOT tagged, then triage for shape
// For conv we need to compare data structures. Let me build leaf/stem/fork checks:
const is_tree_leaf = compile(lam("v",
  a(lit(ite2), lit(FF),  // tagged → FF
    a(lit(mkTriage(TT, fork(LEAF, FF), compile(lam("l r", lit(FF))))), v("v")),
    a(lit(is_tagged_prog), v("v")))
))

const is_tree_stem = compile(lam("v",
  a(lit(ite2), lit(FF),
    a(lit(mkTriage(FF, fork(LEAF, TT), compile(lam("l r", lit(FF))))), v("v")),
    a(lit(is_tagged_prog), v("v")))
))

const is_tree_fork = compile(lam("v",
  a(lit(ite2), lit(FF),
    a(lit(mkTriage(FF, fork(LEAF, FF), compile(lam("l r", lit(TT))))), v("v")),
    a(lit(is_tagged_prog), v("v")))
))

// stem_child: extract child of stem(c)
const stem_child = compile(lam("v",
  a(lit(mkTriage(LEAF, I, compile(lam("l r", lit(LEAF))))), v("v"))
))

// fork_left, fork_right: extract children of fork
const fork_left = pair_fst   // same operation
const fork_right = pair_snd

// conv = fast_eq for now. Hash-consing gives O(1) structural equality.
// Full structural conv (with Pi codomain comparison at fresh hyps) would be
// needed for edge cases where semantically-equal Pi types have different tree
// structure. For design validation, all our constructions are deterministic,
// so hash-cons identity suffices.
const conv = compile(lam("a b", a(a(lit(FAST_EQ), v("a")), v("b"))))

// ================================================================
// napply: full evaluator with H-rule
// ================================================================

// Tag dispatch (shared between H-rule branches): factored out to avoid duplication
const tag_dispatch = compile(lam("f x",
  a(lit(ited),
    thunk(a(a(lit(mk_vstuck_prog), v("f")), v("x"))),
    thunk(a(lit(ited),
      thunk(a(a(lit(mk_vstuck_prog), v("f")), v("x"))),
      thunk(a(lit(ited),
        thunk(a(a(lit(vlam_body), v("f")), v("x"))),
        thunk(a(v("f"), v("x"))),
        a(lit(is_vlam), v("f")))),
      a(lit(is_vstuck), v("f")))),
    a(lit(is_vhyp), v("f")))
))

// napply with H-rule: takes conv_fn and ton_fn as ARGUMENTS
const napply_core = ap(fix_tree, compile(lam("self conv_fn ton_fn f x",
  a(lit(ited),
    // THEN (x is neutral): check H-rule
    thunk(a(lit(ited),
      thunk(lit(TT)),
      thunk(a(a(lit(tag_dispatch), v("f")), v("x"))),
      a(a(v("conv_fn"), v("f")), a(a(v("ton_fn"), v("x")))))),
    // ELSE (x not neutral): tag dispatch
    thunk(a(a(lit(tag_dispatch), v("f")), v("x"))),
    a(lit(is_neutral), v("x")))
)))

// Wire up napply WITHOUT compile-time eval (use treeApply for the wrapper)
// napply f x = napply_core conv type_of_neutral f x
function compileNoEval(expr: Cir, scope: Record<string, Tree> = {}): Tree {
  function resolve(e: Cir): Cir {
    if (e.tag === "var" && e.name in scope) return { tag: "lit", t: scope[e.name] }
    if (e.tag === "app") return { tag: "app", f: resolve(e.f), x: resolve(e.x) }
    if (e.tag === "lam") return { tag: "lam", x: e.x, body: resolve(e.body) }
    return e
  }
  function toTreeLazy(e: Cir): Tree {
    switch (e.tag) {
      case "lit": return e.t
      case "var": throw new Error(`unresolved: ${e.name}`)
      case "I": return I
      case "K": return K
      case "S": return S_TREE
      case "app": {
        if (e.f.tag === "app" && e.f.f.tag === "S") return fork(stem(toTreeLazy(e.f.x)), toTreeLazy(e.x))
        if (e.f.tag === "K") return fork(LEAF, toTreeLazy(e.x))
        if (e.f.tag === "I") return toTreeLazy(e.x)
        return treeApply(toTreeLazy(e.f), toTreeLazy(e.x))  // NO reduction
      }
    }
  }
  return toTreeLazy(eliminateLams(resolve(expr)))
}

// Deferred partial application via wait
const napply_w1 = ap(wait_tree, napply_core, conv)
const napply_w2 = ap(wait_tree, napply_w1, type_of_neutral)
const napply = ap(wait_tree, napply_w2)

// Diagnostic: measure napply call cost
{
  const _b = { remaining: 50_000_000 }
  try {
    const _r = apply(apply(napply, LEAF, _b), LEAF, _b)
    console.log("[DIAG] napply(LEAF,LEAF) steps:", 50_000_000 - _b.remaining, "result:", _r.tag)
  } catch (e: any) {
    console.log("[DIAG] napply(LEAF,LEAF) FAILED at step", 50_000_000 - _b.remaining)
  }
}

// ================================================================
// Type constructors
// ================================================================

// mkPiVal(domain, codomainFn, napply_ref, conv_ref, type_of_neutral_ref, depth):
// produces a VLam with PI_TAG metadata whose body does Pi checking
// Simplified: build using TypeScript helpers that embed the tree programs

function buildPiVal(domain: Tree, codomainFn: Tree, depth: Tree): Tree {
  const meta = mkPiMeta(domain, codomainFn)
  // Body: {f} -> let h = mkVHyp(domain, depth); napply(napply(codomainFn, h), napply(f, h))
  const body = compile(lam("f",
    a(a(lit(napply),
        a(a(lit(napply), lit(codomainFn)),
          a(a(lit(mk_vstuck_prog), lit(domain)), lit(depth)))),    // hyp = mkVHyp(domain, depth) ...wait
      a(a(lit(napply), v("f")),
        a(a(lit(mk_vstuck_prog), lit(domain)), lit(depth))))       // same hyp
  ))
  return mkVLam(meta, body)
}

// Wait - mkVHyp not mk_vstuck_prog. Let me use the TS helper:
function buildHyp(type: Tree, id: Tree): Tree {
  return mkVHyp(type, id)
}

function buildPiVal2(domain: Tree, codomainFn: Tree, depth: Tree): Tree {
  const meta = mkPiMeta(domain, codomainFn)
  const hyp = buildHyp(domain, depth)
  // Body = {f} -> napply(napply(codomainFn, hyp), napply(f, hyp))
  // Build using wait to avoid compile-time eval of large napply tree:
  // napply(codFn, hyp) is a constant — compute at build time
  const cod_at_hyp = ap(napply, codomainFn, hyp)
  // Body: {f} -> napply(cod_at_hyp, napply(f, hyp))
  //            = wait(napply)(cod_at_hyp)(napply(f, hyp))
  //            = wait(napply)(cod_at_hyp, napply_f_hyp)  ... but f is the lambda var
  // Simplest: use wait(wait(napply)(cod_at_hyp)) as a 2-arg function
  //   that takes (result) and applies napply(cod_at_hyp)(result)
  // But we also need napply(f, hyp). Let me just use compileNoEval and see if it works
  // with the wait-based napply (which is a different tree structure).
  //
  // Actually, the issue was that compile calls apply on large trees.
  // The napply tree is now wait-based (~3060 nodes). Embedding it in compile(lam(...))
  // triggers compile-time apply on the bracket-abstracted body.
  // Use compileNoEval for the wrapper:
  const body = compileNoEval(lam("f",
    a(a(lit(napply), lit(cod_at_hyp)),
      a(a(lit(napply), v("f")), lit(hyp)))
  ))
  return mkVLam(meta, body)
}

function buildArrow(a_type: Tree, b_type: Tree, depth: Tree): Tree {
  const codFn = mkVLam(LEAF, compile(lam("_", lit(b_type))))  // constant codomain
  return buildPiVal2(a_type, codFn, depth)
}

// ================================================================
// Nat predicate (recursive, with H-rule via napply)
// ================================================================

// Nat = fix({nat, n} ->
//   napply(nat, n) but we need nat to be a VLam...
// Actually: Nat is a VLam whose body checks for Zero/Succ pattern.
// The body calls napply(Nat, sub_value) for recursion.
// We use fix to tie the knot.

const Nat_tree: Tree = (() => {
  return ap(fix_tree, compile(lam("nat_self",
    a(a(lit(LEAF),
        a(a(lit(LEAF), lit(TAG_ROOT)), lit(KV_LAM))),
      a(a(lit(LEAF), lit(LEAF)),
        lam("n",
          a(lit(ited),
            thunk(lit(TT)),
            thunk(a(lit(ited),
              thunk(a(lit(ited),
                thunk(a(a(lit(napply), v("nat_self")), a(lit(fork_right), v("n")))),
                thunk(lit(FF)),
                a(a(lit(FAST_EQ), a(lit(fork_left), v("n"))), lit(LEAF)))),
              thunk(lit(FF)),
              a(lit(is_tree_fork), v("n")))),
            a(a(lit(FAST_EQ), v("n")), lit(LEAF)))
        )))
  )))
})()

// ================================================================
// Tests
// ================================================================

describe("NbE Tree Programs", () => {

  describe("ite2", () => {
    it("TT → then", () => expect(treeEqual(ap(ite2, stem(LEAF), fork(LEAF,LEAF), TT), stem(LEAF))).toBe(true))
    it("FF → else", () => expect(treeEqual(ap(ite2, stem(LEAF), fork(LEAF,LEAF), FF), fork(LEAF,LEAF))).toBe(true))
  })

  describe("pair_fst / pair_snd", () => {
    it("fst(fork(a,b))=a", () => expect(treeEqual(ap(pair_fst, fork(stem(LEAF), LEAF)), stem(LEAF))).toBe(true))
    it("snd(fork(a,b))=b", () => expect(treeEqual(ap(pair_snd, fork(stem(LEAF), LEAF)), LEAF)).toBe(true))
  })

  describe("Tag infrastructure", () => {
    it("is_tagged recognizes tagged", () => {
      expect(treeEqual(ap(is_tagged_prog, mkVHyp(LEAF, LEAF)), TT)).toBe(true)
      expect(treeEqual(ap(is_tagged_prog, mkVLam(LEAF, I)), TT)).toBe(true)
    })
    it("is_tagged rejects non-tagged", () => {
      expect(treeEqual(ap(is_tagged_prog, LEAF), FF)).toBe(true)
      expect(treeEqual(ap(is_tagged_prog, stem(LEAF)), FF)).toBe(true)
      expect(treeEqual(ap(is_tagged_prog, fork(LEAF, LEAF)), FF)).toBe(true)
    })
    it("tag_kind extracts kind", () => {
      expect(treeEqual(ap(tag_kind_prog, mkVLam(LEAF, I)), KV_LAM)).toBe(true)
      expect(treeEqual(ap(tag_kind_prog, mkVHyp(LEAF, LEAF)), KV_HYP)).toBe(true)
    })
    it("tag_payload extracts payload", () => {
      expect(treeEqual(ap(tag_payload_prog, mkVHyp(stem(LEAF), LEAF)), fork(stem(LEAF), LEAF))).toBe(true)
    })
  })

  describe("Val recognizers", () => {
    it("is_vhyp", () => {
      expect(treeEqual(ap(is_vhyp, mkVHyp(LEAF, LEAF)), TT)).toBe(true)
      expect(treeEqual(ap(is_vhyp, mkVLam(LEAF, I)), FF)).toBe(true)
      expect(treeEqual(ap(is_vhyp, LEAF), FF)).toBe(true)
    })
    it("is_vlam", () => {
      expect(treeEqual(ap(is_vlam, mkVLam(LEAF, I)), TT)).toBe(true)
      expect(treeEqual(ap(is_vlam, mkVHyp(LEAF, LEAF)), FF)).toBe(true)
    })
    it("is_vstuck", () => {
      expect(treeEqual(ap(is_vstuck, mkVStuck(LEAF, LEAF)), TT)).toBe(true)
      expect(treeEqual(ap(is_vstuck, LEAF), FF)).toBe(true)
    })
    it("is_neutral", () => {
      expect(treeEqual(ap(is_neutral, mkVHyp(LEAF, LEAF)), TT)).toBe(true)
      expect(treeEqual(ap(is_neutral, mkVStuck(LEAF, LEAF)), TT)).toBe(true)
      expect(treeEqual(ap(is_neutral, mkVLam(LEAF, I)), FF)).toBe(true)
      expect(treeEqual(ap(is_neutral, LEAF), FF)).toBe(true)
    })
  })

  describe("Payload accessors", () => {
    it("vlam_body", () => expect(treeEqual(ap(vlam_body, mkVLam(LEAF, I)), I)).toBe(true))
    it("vlam_meta", () => {
      const m = stem(stem(LEAF))
      expect(treeEqual(ap(vlam_meta, mkVLam(m, I)), m)).toBe(true)
    })
    it("vhyp_type", () => expect(treeEqual(ap(vhyp_type, mkVHyp(stem(LEAF), LEAF)), stem(LEAF))).toBe(true))
    it("mk_vstuck_prog", () => {
      const h = mkVHyp(LEAF, LEAF)
      expect(treeEqual(ap(mk_vstuck_prog, h, stem(LEAF)), mkVStuck(h, stem(LEAF)))).toBe(true)
    })
  })

  describe("wait + fix recursion", () => {
    it("wait a b c = a b c", () => {
      // wait(K)(LEAF)(stem(LEAF)) = K(LEAF)(stem(LEAF)) = LEAF
      expect(treeEqual(ap(wait_tree, K, LEAF, stem(LEAF)), LEAF)).toBe(true)
    })

    it("fix produces correct recursive countdown", () => {
      // countdown(self, n) = triage n: leaf→LEAF, stem(c)→self(c), fork(l,r)→self(r)
      const cd = ap(fix_tree, compile(lam("self n",
        a(a(a(lit(LEAF),
          a(a(lit(LEAF), lit(LEAF)),                    // leaf: return LEAF
            lam("c", a(v("self"), v("c"))))),           // stem(c): self(c)
          lam("l r", a(v("self"), v("r")))),            // fork(l,r): self(r)
          v("n"))
      )))

      expect(treeEqual(ap(cd, LEAF), LEAF)).toBe(true)
      expect(treeEqual(ap(cd, fork(LEAF, LEAF)), LEAF)).toBe(true)
      expect(treeEqual(ap(cd, fork(LEAF, fork(LEAF, LEAF))), LEAF)).toBe(true)
    })
  })

  describe("napply_simple", () => {
    it("leaf → stem", () => {
      expect(treeEqual(ap(napply_simple, LEAF, LEAF), stem(LEAF))).toBe(true)
    })
    it("stem(a) → fork(a,x)", () => {
      expect(treeEqual(ap(napply_simple, stem(LEAF), stem(LEAF)), fork(LEAF, stem(LEAF)))).toBe(true)
    })
    it("K rule", () => {
      const b = stem(stem(LEAF))
      expect(treeEqual(ap(napply_simple, fork(LEAF, b), LEAF), b)).toBe(true)
    })
    it("VHyp → VStuck", () => {
      const hyp = mkVHyp(LEAF, LEAF)
      expect(treeEqual(ap(napply_simple, hyp, stem(LEAF)), mkVStuck(hyp, stem(LEAF)))).toBe(true)
    })
    it("VStuck → deeper VStuck", () => {
      const s = mkVStuck(mkVHyp(LEAF, LEAF), LEAF)
      expect(treeEqual(ap(napply_simple, s, stem(LEAF)), mkVStuck(s, stem(LEAF)))).toBe(true)
    })
    it("VLam body=I → identity", () => {
      expect(treeEqual(ap(napply_simple, mkVLam(LEAF, I), stem(LEAF)), stem(LEAF))).toBe(true)
    })
    it("VLam body=K → K(x)", () => {
      expect(treeEqual(ap(napply_simple, mkVLam(LEAF, K), stem(LEAF)), fork(LEAF, stem(LEAF)))).toBe(true)
    })
    it("I (raw tree) on data → identity", () => {
      expect(treeEqual(ap(napply_simple, I, LEAF), LEAF)).toBe(true)
      expect(treeEqual(ap(napply_simple, I, stem(LEAF)), stem(LEAF))).toBe(true)
      expect(treeEqual(ap(napply_simple, I, fork(LEAF, stem(LEAF))), fork(LEAF, stem(LEAF)))).toBe(true)
    })
    it("I (raw tree) on VHyp → VHyp (triage reconstructs fork)", () => {
      const hyp = mkVHyp(stem(LEAF), LEAF)
      expect(treeEqual(ap(napply_simple, I, hyp), hyp)).toBe(true)
    })
    it("S K K = identity", () => {
      const skk = ap(napply_simple, ap(napply_simple, S_TREE, K), K)
      expect(treeEqual(ap(napply_simple, skk, stem(LEAF)), stem(LEAF))).toBe(true)
    })
  })

  describe("Metadata reflection", () => {
    it("is_pi on Pi type", () => {
      const piVal = buildPiVal2(Nat_tree, mkVLam(LEAF, compile(lam("_", lit(Nat_tree)))), LEAF)
      expect(treeEqual(ap(is_pi, piVal), TT)).toBe(true)
    })
    it("is_pi on non-Pi", () => {
      expect(treeEqual(ap(is_pi, Nat_tree), FF)).toBe(true)
      expect(treeEqual(ap(is_pi, LEAF), FF)).toBe(true)
    })
    it("pi_dom extracts domain", () => {
      const piVal = buildPiVal2(Nat_tree, mkVLam(LEAF, compile(lam("_", lit(Nat_tree)))), LEAF)
      expect(treeEqual(ap(pi_dom, piVal), Nat_tree)).toBe(true)
    })
  })

  describe("type_of_neutral", () => {
    it("bare hyp returns stored type", () => {
      const hyp = mkVHyp(Nat_tree, LEAF)
      expect(treeEqual(ap(type_of_neutral, hyp), Nat_tree)).toBe(true)
    })
    it("stuck app infers from spine", () => {
      // h_f : Nat -> Nat, applied to h_x : Nat → type should be Nat
      const NatToNat = buildArrow(Nat_tree, Nat_tree, LEAF)
      const h_f = mkVHyp(NatToNat, LEAF)
      const h_x = mkVHyp(Nat_tree, stem(LEAF))
      const stuck = mkVStuck(h_f, h_x)
      const inferred = ap(type_of_neutral, stuck)
      // Should be Nat (the codomain of Nat -> Nat at h_x)
      expect(treeEqual(inferred, Nat_tree)).toBe(true)
    })
  })

  describe("conv", () => {
    it("same tree → TT", () => {
      expect(treeEqual(ap(conv, LEAF, LEAF), TT)).toBe(true)
      expect(treeEqual(ap(conv, stem(LEAF), stem(LEAF)), TT)).toBe(true)
    })
    it("different trees → FF", () => {
      expect(treeEqual(ap(conv, LEAF, stem(LEAF)), FF)).toBe(true)
    })
    it("same hyp → TT", () => {
      const h = mkVHyp(LEAF, LEAF)
      expect(treeEqual(ap(conv, h, h), TT)).toBe(true)
    })
    it("different hyps → FF", () => {
      const h1 = mkVHyp(LEAF, LEAF)
      const h2 = mkVHyp(LEAF, stem(LEAF))
      expect(treeEqual(ap(conv, h1, h2), FF)).toBe(true)
    })
    it("same Pi → TT", () => {
      const codFn = mkVLam(LEAF, compile(lam("_", lit(Nat_tree))))
      const pi1 = buildPiVal2(Nat_tree, codFn, LEAF)
      const pi2 = buildPiVal2(Nat_tree, codFn, LEAF)
      expect(treeEqual(ap(conv, pi1, pi2), TT)).toBe(true)
    })
    it("different trees → FF", () => {
      const pi1 = buildPiVal2(Nat_tree, mkVLam(LEAF, compile(lam("_", lit(Nat_tree)))), LEAF)
      const pi2 = buildPiVal2(Nat_tree, mkVLam(LEAF, compile(lam("_", lit(LEAF)))), LEAF)
      expect(treeEqual(ap(conv, pi1, pi2), FF)).toBe(true)
    })
  })

  describe("napply with H-rule", () => {
    it("leaf → stem (same as simple)", () => {
      expect(treeEqual(ap(napply, LEAF, LEAF), stem(LEAF))).toBe(true)
    })
    it("VLam body=I → identity (same as simple)", () => {
      expect(treeEqual(ap(napply, mkVLam(LEAF, I), stem(LEAF)), stem(LEAF))).toBe(true)
    })
    it("H-rule: hyp with matching type → TT", () => {
      const hyp = mkVHyp(Nat_tree, LEAF)
      expect(treeEqual(ap(napply, Nat_tree, hyp), TT)).toBe(true)
    })
    it("H-rule: hyp with non-matching type → predicate runs", () => {
      const BoolPlaceholder = mkVLam(LEAF, compile(lam("b", lit(FF))))  // always rejects
      const hyp = mkVHyp(BoolPlaceholder, LEAF)
      // napply(Nat, hyp): H-rule checks conv(Nat, BoolPlaceholder) = FF.
      // Falls through to Nat's body. Nat pattern-matches hyp: not Zero, not fork → FF.
      expect(treeEqual(ap(napply, Nat_tree, hyp), FF)).toBe(true)
    })
    it("H-rule on stuck app: {f:Nat->Nat} -> f x : Nat", () => {
      const NatToNat = buildArrow(Nat_tree, Nat_tree, LEAF)
      const h_f = mkVHyp(NatToNat, LEAF)
      const h_x = mkVHyp(Nat_tree, stem(LEAF))
      const stuck = mkVStuck(h_f, h_x)
      // napply(Nat, stuck): H-rule checks conv(Nat, type_of_neutral(stuck)) = conv(Nat, Nat) = TT
      expect(treeEqual(ap(napply, Nat_tree, stuck), TT)).toBe(true)
    })
  })

  describe("Nat predicate", () => {
    it("Zero : Nat", () => {
      expect(treeEqual(ap(napply, Nat_tree, LEAF), TT)).toBe(true)
    })
    it("Succ(Zero) : Nat", () => {
      expect(treeEqual(ap(napply, Nat_tree, fork(LEAF, LEAF)), TT)).toBe(true)
    })
    it("Succ(Succ(Zero)) : Nat", () => {
      expect(treeEqual(ap(napply, Nat_tree, fork(LEAF, fork(LEAF, LEAF))), TT)).toBe(true)
    })
    it("FF !: Nat", () => {
      expect(treeEqual(ap(napply, Nat_tree, FF), FF)).toBe(true)
    })
  })

  describe("Arrow type checking", () => {
    it("id : Nat -> Nat", () => {
      const NatToNat = buildArrow(Nat_tree, Nat_tree, LEAF)
      const id_fn = mkVLam(LEAF, I)
      expect(treeEqual(ap(napply, NatToNat, id_fn), TT)).toBe(true)
    })
    it("const Zero : Nat -> Nat", () => {
      const NatToNat = buildArrow(Nat_tree, Nat_tree, LEAF)
      const k0 = mkVLam(LEAF, compile(lam("_", lit(LEAF))))  // always returns Zero=LEAF
      expect(treeEqual(ap(napply, NatToNat, k0), TT)).toBe(true)
    })
    it("const FF !: Nat -> Nat", () => {
      const NatToNat = buildArrow(Nat_tree, Nat_tree, LEAF)
      const bad = mkVLam(LEAF, compile(lam("_", lit(FF))))
      expect(treeEqual(ap(napply, NatToNat, bad), FF)).toBe(true)
    })
  })

  describe("Higher-order: stuck apps with type inference", () => {
    it("{f:Nat->Nat, x:Nat} -> f x : (Nat->Nat) -> Nat -> Nat", () => {
      const NatToNat = buildArrow(Nat_tree, Nat_tree, LEAF)
      const fullType = buildArrow(NatToNat, buildArrow(Nat_tree, Nat_tree, stem(LEAF)), LEAF)
      // Term: {f} -> {x} -> napply(f, x) = VLam applied to VLam
      const term = mkVLam(LEAF, compile(lam("f",
        lit(mkVLam(LEAF, compile(lam("x",
          a(a(lit(napply), v("f")), v("x"))
        ))))
      )))
      expect(treeEqual(ap(napply, fullType, term), TT)).toBe(true)
    })
  })

  describe("Polymorphic identity (hypothesis-as-type)", () => {
    it("{A:Type, x:A} -> x via abstract type hypothesis", () => {
      // Simplified: check that napply(hyp_A, hyp_x) = TT when hyp_x has type hyp_A
      const hyp_A = mkVHyp(LEAF, LEAF)           // A : Type (type irrelevant for this test)
      const hyp_x = mkVHyp(hyp_A, stem(LEAF))    // x : A
      // napply(hyp_A, hyp_x): H-rule. type_of_neutral(hyp_x) = hyp_A. conv(hyp_A, hyp_A) = TT.
      expect(treeEqual(ap(napply, hyp_A, hyp_x), TT)).toBe(true)
    })
  })
})
