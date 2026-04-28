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
// (ERROR_VAL removed: CPS ton_check returns FF on failure, no sentinel needed)

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

// CPS-style type_of_neutral: instead of returning the inferred type (or an
// error sentinel), takes a check_fn callback and returns Bool.
// check_fn is called with the inferred type on success; FF on failure.
// Eliminates ERROR_VAL and short-circuits on spine inference failure.
//
// ton_check(check_fn, v) → Bool
//   VHyp: check_fn(stored_type)
//   VStuck: recurse on head with continuation that extracts pi_cod
//   else: FF

// Branch: v is VHyp → call check_fn on stored type
const ton_vhyp_fn = compile(lam("self napply_fn check_fn v",
  a(v("check_fn"), a(lit(vhyp_type), v("v")))))

// Continuation template for VStuck spine inference:
// {check_fn, napply_fn, arg, head_type} ->
//   is_pi(head_type) ? check_fn(napply_fn(pi_cod(head_type), arg)) : FF
// Partially applied to (check_fn, napply_fn, arg) at each recursion step.
const ton_spine_cont = compile(lam("check_fn napply_fn arg head_type",
  a(lit(ite2),
    a(v("check_fn"), a(a(v("napply_fn"), a(lit(pi_cod_fn), v("head_type"))), v("arg"))),
    lit(FF),
    a(lit(is_pi), v("head_type")))))

// Branch: v is VStuck → recurse with continuation built from template
const ton_vstuck_fn = compile(lam("self napply_fn check_fn v",
  a(a(a(v("self"), v("napply_fn")),
    a(a(a(lit(ton_spine_cont), v("check_fn")), v("napply_fn")), a(lit(vstuck_arg), v("v")))),
    a(lit(vstuck_head), v("v")))))

// Branch: v is neither → FF
const ton_ff_fn = compile(lam("self napply_fn check_fn v", lit(FF)))

// Non-VHyp dispatch: select vstuck or ff, then apply shared args
const ton_non_vhyp_fn = compile(lam("self napply_fn check_fn v",
  a(a(a(a(a(lit(ite2), lit(ton_vstuck_fn), lit(ton_ff_fn),
    a(lit(is_vstuck), v("v"))),
    v("self")), v("napply_fn")), v("check_fn")), v("v"))))

// Top-level: select vhyp or non_vhyp, then apply shared args
const ton_check_core = ap(fix_tree, compile(lam("self napply_fn check_fn v",
  a(a(a(a(a(lit(ite2), lit(ton_vhyp_fn), lit(ton_non_vhyp_fn),
    a(lit(is_vhyp), v("v"))),
    v("self")), v("napply_fn")), v("check_fn")), v("v")))))

// Wired via wait: ton_check(check_fn, v) = ton_check_core(napply_simple, check_fn, v)
const ton_check = ap(wait_tree, ton_check_core, napply_simple)

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

// napply with H-rule: select-then-apply to avoid bracket-abstraction
// evaluating ton_fn(x) when x is not neutral.

// Branch: x IS neutral → check H-rule via ton_check(conv(f), x), fall back to tag_dispatch
const napply_neutral_fn = compile(lam("conv_fn ton_check_fn f x",
  a(lit(ited),
    thunk(lit(TT)),
    thunk(a(a(lit(tag_dispatch), v("f")), v("x"))),
    a(a(v("ton_check_fn"), a(v("conv_fn"), v("f"))), v("x")))))

// Branch: x is NOT neutral → raw tag dispatch (no type inference)
const napply_raw_fn = compile(lam("conv_fn ton_check_fn f x",
  a(a(lit(tag_dispatch), v("f")), v("x"))))

// Select branch via ite2 BEFORE applying shared args
const napply_core = compile(lam("conv_fn ton_check_fn f x",
  a(a(a(a(a(lit(ite2), lit(napply_neutral_fn), lit(napply_raw_fn),
    a(lit(is_neutral), v("x"))),
    v("conv_fn")), v("ton_check_fn")), v("f")), v("x"))))

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
const napply_w2 = ap(wait_tree, napply_w1, ton_check)
const napply = ap(wait_tree, napply_w2)

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
  const body = compile(lam("f",
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

// Nat predicate: the body is a fix-based recursive function, wrapped in VLam.
// fix produces the body function; VLam tag is applied outside fix so that
// Nat_tree IS a VLam (not a wait-encoded fix tree).
//
// Select-then-apply pattern avoids bracket abstraction evaluating recursive
// napply(self, ...) in non-taken branches.

// Branch: n is Zero (LEAF) → TT
const nat_zero_fn = compile(lam("self n", lit(TT)))

// Branch: n is Succ (fork with first=LEAF) → recurse on predecessor
const nat_succ_fn = compile(lam("self n",
  a(a(lit(napply), v("self")), a(lit(fork_right), v("n")))))

// Branch: FF (neither Zero nor valid Succ)
const nat_ff_fn = compile(lam("self n", lit(FF)))

// Branch: n is a fork with first child = LEAF → Succ, recurse
const nat_succ_check_fn = compile(lam("self n",
  a(a(a(lit(ite2), lit(nat_succ_fn), lit(nat_ff_fn),
    a(a(lit(FAST_EQ), a(lit(fork_left), v("n"))), lit(LEAF))),
    v("self")), v("n"))))

// Branch: n is not Zero → check is_tree_fork first, then Succ check
const nat_non_zero_fn = compile(lam("self n",
  a(a(a(lit(ite2), lit(nat_succ_check_fn), lit(nat_ff_fn),
    a(lit(is_tree_fork), v("n"))),
    v("self")), v("n"))))

// Recursive body: fix produces a 2-arg function (self, n)
const nat_body = ap(fix_tree, compile(lam("self n",
  a(a(a(lit(ite2), lit(nat_zero_fn), lit(nat_non_zero_fn),
    a(a(lit(FAST_EQ), v("n")), lit(LEAF))),
    v("self")), v("n")))))

// Wrap in VLam: Nat_tree is a properly-tagged VLam whose body is nat_body
const Nat_tree: Tree = mkVLam(LEAF, nat_body)

// ================================================================
// Tree-program Val constructors
// ================================================================

// mkVLam as tree program: \meta body -> fork(fork(TAG_ROOT, KV_LAM), fork(meta, body))
const mkVLam_prog = compile(lam("meta body",
  a(a(lit(LEAF), a(a(lit(LEAF), lit(TAG_ROOT)), lit(KV_LAM))),
    a(a(lit(LEAF), v("meta")), v("body")))))

// mkVHyp as tree program: \type id -> fork(fork(TAG_ROOT, KV_HYP), fork(type, id))
const mkVHyp_prog = compile(lam("type id",
  a(a(lit(LEAF), a(a(lit(LEAF), lit(TAG_ROOT)), lit(KV_HYP))),
    a(a(lit(LEAF), v("type")), v("id")))))

// fresh_hyp = mkVHyp_prog (identity from binder depth, threaded by elaborator)
const fresh_hyp_prog = mkVHyp_prog

// ================================================================
// Bool predicate
// ================================================================

// Bool(b) = (b == TT) ? TT : (b == FF) ? TT : FF
const Bool_tree: Tree = mkVLam(LEAF, compile(lam("b",
  a(lit(ite2), lit(TT),
    a(a(lit(FAST_EQ), v("b")), lit(FF)),
    a(a(lit(FAST_EQ), v("b")), lit(LEAF))))))

// ================================================================
// Pi construction as tree program
// ================================================================

// Template: \napply_ref A B f -> napply_ref(A, napply_ref(f, B))
// Partial application to (napply, cod_at_hyp, hyp) produces the Pi body closure
const pi_body_template = compile(lam("napply_ref A B f",
  a(a(v("napply_ref"), v("A")),
    a(a(v("napply_ref"), v("f")), v("B")))))

// mkPi_prog: \domain codFn depth -> VLam(piMeta, pi_body)
// Computes cod_at_hyp = napply(codFn, hyp) at construction time,
// then partial-applies pi_body_template to build the body closure.
const mkPi_prog = compile(lam("domain codFn depth",
  a(a(lit(mkVLam_prog),
    // meta = fork(PI_TAG, fork(domain, codFn))
    a(a(lit(LEAF), lit(PI_TAG)), a(a(lit(LEAF), v("domain")), v("codFn")))),
    // body = pi_body_template(napply, napply(codFn, hyp), hyp)
    a(a(a(lit(pi_body_template), lit(napply)),
      a(a(lit(napply), v("codFn")),
        a(a(lit(mkVHyp_prog), v("domain")), v("depth")))),
      a(a(lit(mkVHyp_prog), v("domain")), v("depth"))))))

// mkArrow_prog: \a_type b_type depth -> mkPi(a_type, VLam(LEAF, K(b_type)), depth)
// K(b_type) = fork(LEAF, b_type) is the constant codomain function
const mkArrow_prog = compile(lam("a_type b_type depth",
  a(a(a(lit(mkPi_prog), v("a_type")),
    a(a(lit(mkVLam_prog), lit(LEAF)), a(a(lit(LEAF), lit(LEAF)), v("b_type")))),
    v("depth"))))

// ================================================================
// Nat comparison (le, lt) for universe rank checking
// ================================================================

// nat_le: \a b -> a <= b (Nat encoding: Zero=LEAF, Succ(n)=fork(LEAF,n))
// Zero <= anything = TT
// Succ(a) <= Zero = FF
// Succ(a) <= Succ(b) = a <= b
const nat_le_zero_fn = compile(lam("self a b", lit(TT)))
const nat_le_succ_fn = compile(lam("self a b",
  a(a(a(a(lit(ite2),
    // b is fork (Succ): recurse on predecessors
    lit(compile(lam("self a b",
      a(a(a(v("self"), a(lit(fork_right), v("a"))), a(lit(fork_right), v("b"))))))),
    // b is not fork (Zero or stem): Succ(a) > b → FF
    lit(compile(lam("self a b", lit(FF)))),
    a(lit(is_tree_fork), v("b"))),
    v("self")), v("a")), v("b"))))
const nat_le_nonzero_fn = compile(lam("self a b",
  a(a(a(a(lit(ite2), lit(nat_le_succ_fn), lit(compile(lam("self a b", lit(FF)))),
    a(lit(is_tree_fork), v("a"))),
    v("self")), v("a")), v("b"))))
const nat_le = ap(fix_tree, compile(lam("self a b",
  a(a(a(a(lit(ite2), lit(nat_le_zero_fn), lit(nat_le_nonzero_fn),
    a(a(lit(FAST_EQ), v("a")), lit(LEAF))),
    v("self")), v("a")), v("b")))))

// nat_lt: a < b = Succ(a) <= b
const nat_lt = compile(lam("a b",
  a(a(lit(nat_le), a(a(lit(LEAF), lit(LEAF)), v("a"))), v("b"))))

// ================================================================
// Type n (universe predicate) as tree program
// ================================================================

// Registry: hardcoded for Nat and Bool
const is_registered = compile(lam("t",
  a(lit(ite2), lit(TT),
    a(a(lit(FAST_EQ), v("t")), lit(Bool_tree)),
    a(a(lit(FAST_EQ), v("t")), lit(Nat_tree)))))

// Type body branches (all take self, rank, t as args after ite2 selection):

// Cumulative universe check template:
// {rank, inferred_type} -> is_universe(inferred_type) ? le(universe_rank(inferred_type), rank) : FF
const univ_check_template = compile(lam("rank inferred_type",
  a(lit(ite2),
    a(a(lit(nat_le), a(lit(universe_rank), v("inferred_type"))), v("rank")),
    lit(FF),
    a(lit(is_universe), v("inferred_type")))))

// Neutral branch: use ton_check with the universe check as continuation
// ton_check(univ_check_template(rank), t) → Bool
const type_neutral_fn = compile(lam("self rank t",
  a(a(lit(ton_check), a(lit(univ_check_template), v("rank"))), v("t"))))

// Universe-below branch: universeRank(t) < rank
const type_univ_fn = compile(lam("self rank t",
  a(a(lit(nat_lt), a(lit(universe_rank), v("t"))), v("rank"))))

// Pi-at-rank branch: check domain and codomain against Type(rank)
// Needs a fresh hyp for codomain checking. Use stem(stem(rank)) as depth
// to distinguish from hyps created by Pi construction.
const type_pi_fn = (() => {
  // and(self(rank, pi_dom(t)), self(rank, napply(pi_cod_fn(t), mkVHyp(pi_dom(t), stem(stem(rank))))))
  const dom = a(lit(pi_dom), v("t"))
  const depth = a(a(lit(LEAF), a(lit(LEAF), v("rank"))))  // stem(stem(rank))
  const hyp = a(a(lit(mkVHyp_prog), dom), depth)
  const cod_at_hyp = a(a(lit(napply), a(lit(pi_cod_fn), v("t"))), hyp)
  const check_dom = a(a(v("self"), v("rank")), dom)
  const check_cod = a(a(v("self"), v("rank")), cod_at_hyp)
  return compile(lam("self rank t", a(a(lit(and_tree), check_dom), check_cod)))
})()

// Registered-base branch: is_registered(t)
const type_registered_fn = compile(lam("self rank t",
  a(lit(is_registered), v("t"))))

// FF branch
const type_ff_fn = compile(lam("self rank t", lit(FF)))

// Non-neutral dispatch: universe? Pi? registered? else FF
const type_non_neutral_fn = compile(lam("self rank t",
  a(a(a(a(lit(ite2),
    lit(type_univ_fn),
    lit(compile(lam("self rank t",
      a(a(a(a(lit(ite2),
        lit(type_pi_fn),
        lit(compile(lam("self rank t",
          a(a(a(a(lit(ite2), lit(type_registered_fn), lit(type_ff_fn),
            a(lit(is_registered), v("t"))),
            v("self")), v("rank")), v("t"))))),
        a(lit(is_pi), v("t"))),
        v("self")), v("rank")), v("t"))))),
    a(lit(is_universe), v("t"))),
    v("self")), v("rank")), v("t"))))

// Top-level Type body: dispatch on is_neutral(t)
// Type n uses its own body-level neutral handling (cumulativity needs rank<=n, not exact match)
const type_body_core = ap(fix_tree, compile(lam("self rank t",
  a(a(a(a(lit(ite2), lit(type_neutral_fn), lit(type_non_neutral_fn),
    a(lit(is_neutral), v("t"))),
    v("self")), v("rank")), v("t")))))

// mkType: \rank -> VLam(fork(UNIV_TAG, rank), type_body(rank))
// The body is a 1-arg function (takes t), built by partial-applying type_body_core to rank
function mkType(rank: Tree): Tree {
  const meta = fork(UNIV_TAG, rank)
  const body = ap(type_body_core, rank)
  return mkVLam(meta, body)
}

const Type0 = mkType(LEAF)           // Type 0 (rank = 0 = LEAF)
const Type1 = mkType(fork(LEAF, LEAF)) // Type 1 (rank = 1 = Succ(Zero))
const Type2 = mkType(fork(LEAF, fork(LEAF, LEAF))) // Type 2

// ================================================================
// Structural conv (with Pi codomain comparison)
// ================================================================

// conv_structural: recursive, handles Pi/Universe specially, falls back to fast_eq
const conv_fast_fn = compile(lam("self a b", lit(TT)))
const conv_pi_fn = compile(lam("self a b",
  a(a(lit(and_tree),
    a(a(v("self"), a(lit(pi_dom), v("a"))), a(lit(pi_dom), v("b")))),
    a(a(v("self"),
      a(a(lit(napply), a(lit(pi_cod_fn), v("a"))),
        a(a(lit(mkVHyp_prog), a(lit(pi_dom), v("a"))), lit(stem(stem(stem(LEAF))))))),
      a(a(lit(napply), a(lit(pi_cod_fn), v("b"))),
        a(a(lit(mkVHyp_prog), a(lit(pi_dom), v("a"))), lit(stem(stem(stem(LEAF))))))))))
const conv_univ_fn = compile(lam("self a b",
  a(a(lit(FAST_EQ), a(lit(universe_rank), v("a"))), a(lit(universe_rank), v("b")))))
const conv_ff_fn = compile(lam("self a b", lit(FF)))

const conv_not_fast_fn = compile(lam("self a b",
  a(a(a(a(lit(ite2),
    // both Pi?
    lit(compile(lam("self a b",
      a(a(a(a(lit(ite2), lit(conv_pi_fn), lit(compile(lam("self a b",
        // both Universe?
        a(a(a(a(lit(ite2), lit(conv_univ_fn), lit(conv_ff_fn),
          a(a(lit(and_tree), a(lit(is_universe), v("a"))), a(lit(is_universe), v("b")))),
          v("self")), v("a")), v("b"))))),
        a(a(lit(and_tree), a(lit(is_pi), v("a"))), a(lit(is_pi), v("b")))),
        v("self")), v("a")), v("b"))))),
    lit(conv_ff_fn),
    a(lit(is_tagged_prog), v("a"))),
    v("self")), v("a")), v("b"))))

const conv_structural = ap(fix_tree, compile(lam("self a b",
  a(a(a(a(lit(ite2), lit(conv_fast_fn), lit(conv_not_fast_fn),
    a(a(lit(FAST_EQ), v("a")), v("b"))),
    v("self")), v("a")), v("b")))))

// ================================================================
// Tests
// ================================================================

describe("NbE Tree Programs", () => {

  describe("ite2", () => {
    it("TT → then", () => expect(treeEqual(ap(ite2, stem(LEAF), fork(LEAF,LEAF), TT), stem(LEAF))).toBe(true))
    it("FF → else", () => expect(treeEqual(ap(ite2, stem(LEAF), fork(LEAF,LEAF), FF), fork(LEAF,LEAF))).toBe(true))
    it("ited does not force a non-selected divergent branch", () => {
      const sii = fork(stem(I), I)
      const diverge = compileNoEval(lam("_", a(lit(sii), lit(sii))))
      expect(treeEqual(ap(ited, compile(thunk(lit(LEAF))), diverge, TT), LEAF)).toBe(true)
    })
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
    it("tag_dispatch leaf → stem", () => {
      expect(treeEqual(ap(tag_dispatch, LEAF, LEAF), stem(LEAF))).toBe(true)
    })
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

  describe("ton_check (CPS type_of_neutral)", () => {
    it("bare hyp: check_fn receives stored type", () => {
      const hyp = mkVHyp(Nat_tree, LEAF)
      // Use I as check_fn: returns the type itself (generalizes to any check)
      expect(treeEqual(ap(ton_check, I, hyp), Nat_tree)).toBe(true)
    })
    it("bare hyp: conv check succeeds for matching type", () => {
      const hyp = mkVHyp(Nat_tree, LEAF)
      // check_fn = conv(Nat) = fast_eq(Nat). Should return TT.
      expect(treeEqual(ap(ton_check, ap(conv, Nat_tree), hyp), TT)).toBe(true)
    })
    it("bare hyp: conv check fails for non-matching type", () => {
      const hyp = mkVHyp(Nat_tree, LEAF)
      expect(treeEqual(ap(ton_check, ap(conv, Bool_tree), hyp), FF)).toBe(true)
    })
    it("stuck app: spine inference finds codomain type", () => {
      const NatToNat = buildArrow(Nat_tree, Nat_tree, LEAF)
      const h_f = mkVHyp(NatToNat, LEAF)
      const h_x = mkVHyp(Nat_tree, stem(LEAF))
      const stuck = mkVStuck(h_f, h_x)
      // Use I to extract: should be Nat (codomain of Nat->Nat at h_x)
      expect(treeEqual(ap(ton_check, I, stuck), Nat_tree)).toBe(true)
    })
    it("stuck app: conv check succeeds", () => {
      const NatToNat = buildArrow(Nat_tree, Nat_tree, LEAF)
      const h_f = mkVHyp(NatToNat, LEAF)
      const h_x = mkVHyp(Nat_tree, stem(LEAF))
      const stuck = mkVStuck(h_f, h_x)
      expect(treeEqual(ap(ton_check, ap(conv, Nat_tree), stuck), TT)).toBe(true)
    })
    it("spine failure: non-Pi head returns FF", () => {
      // h_x : h_A where h_A is VHyp (not Pi). h_x(h_x) → VStuck(h_x, h_x)
      const h_A = mkVHyp(LEAF, LEAF)
      const h_x = mkVHyp(h_A, stem(LEAF))
      const stuck = mkVStuck(h_x, h_x)
      // type_of_neutral fails: h_x's type is h_A, not Pi → FF
      expect(treeEqual(ap(ton_check, I, stuck), FF)).toBe(true)
    })
    it("non-neutral input returns FF", () => {
      expect(treeEqual(ap(ton_check, I, LEAF), FF)).toBe(true)
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
    it("napply_core with cheap ton_check leaf → stem", () => {
      // ton_check that always returns FF (no H-rule match)
      const cheapTon = compile(lam("check_fn v", lit(FF)))
      expect(treeEqual(ap(napply_core, conv, cheapTon, LEAF, LEAF), stem(LEAF))).toBe(true)
    })
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
      // Term: {f} -> VLam(LEAF, {x} -> napply(f, x))
      // Inner compile can't see outer "f", so we compile the whole thing as one lambda
      // and build the nested VLam structure at runtime
      const inner_body = compile(lam("f x",
        a(a(lit(napply), v("f")), v("x"))))
      // term = VLam(LEAF, {f} -> VLam(LEAF, inner_body(f)))
      const term = mkVLam(LEAF, compile(lam("f",
        a(a(lit(LEAF),
            a(a(lit(LEAF), lit(TAG_ROOT)), lit(KV_LAM))),
          a(a(lit(LEAF), lit(LEAF)),
            a(lit(inner_body), v("f")))))))
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

  // ==================================================================
  // New tree-program components
  // ==================================================================

  describe("Tree-program Val constructors", () => {
    it("mkVLam_prog matches mkVLam", () => {
      const meta = stem(LEAF)
      expect(treeEqual(ap(mkVLam_prog, meta, I), mkVLam(meta, I))).toBe(true)
    })
    it("mkVHyp_prog matches mkVHyp", () => {
      expect(treeEqual(ap(mkVHyp_prog, Nat_tree, LEAF), mkVHyp(Nat_tree, LEAF))).toBe(true)
    })
    it("mkVHyp_prog produces distinct hyps for different depths", () => {
      const h0 = ap(mkVHyp_prog, Nat_tree, LEAF)
      const h1 = ap(mkVHyp_prog, Nat_tree, stem(LEAF))
      expect(treeEqual(h0, h1)).toBe(false)
    })
  })

  describe("Bool predicate", () => {
    it("TT : Bool", () => {
      expect(treeEqual(ap(napply, Bool_tree, TT), TT)).toBe(true)
    })
    it("FF : Bool", () => {
      expect(treeEqual(ap(napply, Bool_tree, FF), TT)).toBe(true)
    })
    it("Succ(Zero) !: Bool", () => {
      expect(treeEqual(ap(napply, Bool_tree, fork(LEAF, LEAF)), FF)).toBe(true)
    })
  })

  describe("Pi construction (tree-level mkPi_prog)", () => {
    it("mkPi_prog produces a VLam with PI_TAG", () => {
      const codFn = mkVLam(LEAF, compile(lam("_", lit(Nat_tree))))
      const pi = ap(mkPi_prog, Nat_tree, codFn, LEAF)
      expect(treeEqual(ap(is_pi, pi), TT)).toBe(true)
    })
    it("mkPi_prog Pi domain matches", () => {
      const codFn = mkVLam(LEAF, compile(lam("_", lit(Nat_tree))))
      const pi = ap(mkPi_prog, Nat_tree, codFn, LEAF)
      expect(treeEqual(ap(pi_dom, pi), Nat_tree)).toBe(true)
    })
    it("mkArrow_prog id : Nat -> Nat", () => {
      const NatToNat = ap(mkArrow_prog, Nat_tree, Nat_tree, LEAF)
      const id_fn = mkVLam(LEAF, I)
      expect(treeEqual(ap(napply, NatToNat, id_fn), TT)).toBe(true)
    })
    it("mkArrow_prog const Zero : Nat -> Nat", () => {
      const NatToNat = ap(mkArrow_prog, Nat_tree, Nat_tree, LEAF)
      const k0 = mkVLam(LEAF, compile(lam("_", lit(LEAF))))
      expect(treeEqual(ap(napply, NatToNat, k0), TT)).toBe(true)
    })
    it("mkArrow_prog const FF !: Nat -> Nat", () => {
      const NatToNat = ap(mkArrow_prog, Nat_tree, Nat_tree, LEAF)
      const bad = mkVLam(LEAF, compile(lam("_", lit(FF))))
      expect(treeEqual(ap(napply, NatToNat, bad), FF)).toBe(true)
    })
  })

  describe("Nat le/lt", () => {
    it("0 <= 0", () => expect(treeEqual(ap(nat_le, LEAF, LEAF), TT)).toBe(true))
    it("0 <= 3", () => expect(treeEqual(ap(nat_le, LEAF, fork(LEAF, fork(LEAF, fork(LEAF, LEAF)))), TT)).toBe(true))
    it("1 <= 0 = FF", () => expect(treeEqual(ap(nat_le, fork(LEAF, LEAF), LEAF), FF)).toBe(true))
    it("2 <= 3", () => {
      const two = fork(LEAF, fork(LEAF, LEAF))
      const three = fork(LEAF, fork(LEAF, fork(LEAF, LEAF)))
      expect(treeEqual(ap(nat_le, two, three), TT)).toBe(true)
    })
    it("3 <= 2 = FF", () => {
      const two = fork(LEAF, fork(LEAF, LEAF))
      const three = fork(LEAF, fork(LEAF, fork(LEAF, LEAF)))
      expect(treeEqual(ap(nat_le, three, two), FF)).toBe(true)
    })
    it("0 < 1", () => expect(treeEqual(ap(nat_lt, LEAF, fork(LEAF, LEAF)), TT)).toBe(true))
    it("0 < 0 = FF", () => expect(treeEqual(ap(nat_lt, LEAF, LEAF), FF)).toBe(true))
    it("1 < 2", () => expect(treeEqual(ap(nat_lt, fork(LEAF, LEAF), fork(LEAF, fork(LEAF, LEAF))), TT)).toBe(true))
  })

  describe("Type n (universe predicate)", () => {
    it("Nat : Type 0", () => {
      expect(treeEqual(ap(napply, Type0, Nat_tree), TT)).toBe(true)
    })
    it("Bool : Type 0", () => {
      expect(treeEqual(ap(napply, Type0, Bool_tree), TT)).toBe(true)
    })
    it("Nat -> Nat : Type 0", () => {
      const NatToNat = ap(mkArrow_prog, Nat_tree, Nat_tree, LEAF)
      expect(treeEqual(ap(napply, Type0, NatToNat), TT)).toBe(true)
    })
    it("Type 0 : Type 1", () => {
      expect(treeEqual(ap(napply, Type1, Type0), TT)).toBe(true)
    })
    it("Type 0 !: Type 0 (no Type-in-Type)", () => {
      expect(treeEqual(ap(napply, Type0, Type0), FF)).toBe(true)
    })
  })

  describe("Cumulativity", () => {
    it("Nat : Type 1", () => {
      expect(treeEqual(ap(napply, Type1, Nat_tree), TT)).toBe(true)
    })
    it("Type 0 : Type 2", () => {
      expect(treeEqual(ap(napply, Type2, Type0), TT)).toBe(true)
    })
  })

  describe("Structural conv", () => {
    it("same tree → TT", () => {
      expect(treeEqual(ap(conv_structural, LEAF, LEAF), TT)).toBe(true)
    })
    it("different trees → FF", () => {
      expect(treeEqual(ap(conv_structural, LEAF, stem(LEAF)), FF)).toBe(true)
    })
    it("same Pi → TT (via structural comparison)", () => {
      const codFn = mkVLam(LEAF, compile(lam("_", lit(Nat_tree))))
      const pi1 = ap(mkPi_prog, Nat_tree, codFn, LEAF)
      const pi2 = ap(mkPi_prog, Nat_tree, codFn, LEAF)
      expect(treeEqual(ap(conv_structural, pi1, pi2), TT)).toBe(true)
    })
    it("different Pi domains → FF", () => {
      const codFn = mkVLam(LEAF, compile(lam("_", lit(Nat_tree))))
      const pi1 = ap(mkPi_prog, Nat_tree, codFn, LEAF)
      const pi2 = ap(mkPi_prog, Bool_tree, codFn, LEAF)
      expect(treeEqual(ap(conv_structural, pi1, pi2), FF)).toBe(true)
    })
    it("same Universe → TT", () => {
      expect(treeEqual(ap(conv_structural, Type0, Type0), TT)).toBe(true)
    })
    it("different Universes → FF", () => {
      expect(treeEqual(ap(conv_structural, Type0, Type1), FF)).toBe(true)
    })
  })

  describe("Integration: full type-checking pipeline", () => {
    it("{A:Type 0, x:A} -> x : Pi(Type 0, {A}->Pi(A, {_}->A))", () => {
      // Polymorphic identity: build the type using tree-level mkPi
      const polyIdType = ap(mkPi_prog, Type0,
        mkVLam(LEAF, compile(lam("A",
          a(a(a(lit(mkArrow_prog), v("A")), v("A")), lit(stem(LEAF)))))),
        LEAF)
      // Term: {A} -> {x} -> x
      const polyId = mkVLam(LEAF, compile(lam("_A",
        a(a(lit(mkVLam_prog), lit(LEAF)), lit(I)))))
      expect(treeEqual(ap(napply, polyIdType, polyId), TT)).toBe(true)
    })
  })
})
