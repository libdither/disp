// Design sketch: ctx-tree-based pred_of_lvl.
//
// NOT RUNNABLE. This file is the target architecture for the disp port —
// TS is the whiteboard so the primitives can be scrutinized cheaply before
// paying the bracket-abstraction cost of materializing them in disp.
//
// ────────────────────────────────────────────────────────────────────────
// Motivation recap
// ────────────────────────────────────────────────────────────────────────
// Today's pred_of_lvl eliminates V occurrences by splicing mkH tokens into
// the term (predicates.disp `pred_of_lvl` Lam-vs-Pi case). This:
//   1. mutates the term we're checking (pollution of read-only data).
//   2. won't transfer to a future where terms are bracket-abstracted SKI
//      (no binders to plant tokens at).
//   3. is the root shape of the stale-bindtree bug in normalize: after a
//      beta-reduction under a binder, the Lam's `lamB` no longer describes
//      the body's V-positions because splice may have removed/rearranged
//      them.
//
// Replacement: the term stays read-only. A parallel ctx-tree, structurally
// isomorphic to the term (minus uninteresting leaves), carries per-binder
// type information at V-positions. Kernel consults ctx when it needs a
// V-leaf's type; Lam-descent extends ctx instead of splicing.
//
// ────────────────────────────────────────────────────────────────────────
// Data model
// ────────────────────────────────────────────────────────────────────────

// Opaque hash-cons tree (pretend — mirrors src/tree.ts `Tree`).
interface Tree { id: number }

// Tagged forms, modeled as TS sum for readability. In disp these are the
// KV/KH/KA/KL/KP-tagged payloads already built by mkV/mkH/mkApp/mkLam/mkPi.
type Term =
  | { kind: "V" }
  | { kind: "H"; ty: Term; marker: Tree }
  | { kind: "App"; f: Term; x: Term }
  | { kind: "Lam"; a: Term; b: Bind; body: Term }
  | { kind: "Pi"; a: Term; b: Bind; codom: Term }
  | { kind: "Meta"; marker: Tree }

// Bind-trees (BE/BN/BApp/BLam/BPi). BE marks V-positions for this binder.
type Bind =
  | { kind: "BE" }                     // V-leaf belonging to THIS binder
  | { kind: "BN" }                     // leaf belonging to an outer binder
  | { kind: "BApp"; f: Bind; x: Bind }
  | { kind: "BLam"; a: Bind; body: Bind }
  | { kind: "BPi"; a: Bind; codom: Bind }

// BinderId: a context-depth marker. Shape-distinct from leaf-/fork-markers
// used by mkH freshness (see predicates.disp lvl_start/lvl_next), so ctx
// entries never alias mkH tokens.
type BinderId = Tree

// Ctx-tree — parallel to Term but sparse: carries type info only at V-leaf
// positions (and at Meta leaves once metas interact, but defer that here).
//
// Shape invariant: ctx_for(term) always has the same recursive structure
// as `term`, EXCEPT:
//   - at a V-leaf the ctx carries CtxV(binderId, ty)
//   - at any other leaf (H, atom, etc.) the ctx is CtxNone
//
// I.e. ctx is a "skeleton of binder-type info pinned to V positions".
type Ctx =
  | { kind: "CtxNone" }                               // no binder info here
  | { kind: "CtxV"; binder: BinderId; ty: Term }      // THIS V has type `ty`
  | { kind: "CtxApp"; f: Ctx; x: Ctx }
  | { kind: "CtxLam"; a: Ctx; body: Ctx }
  | { kind: "CtxPi"; a: Ctx; codom: Ctx }

const CtxNone: Ctx = { kind: "CtxNone" }

// ────────────────────────────────────────────────────────────────────────
// Primitives
// ────────────────────────────────────────────────────────────────────────

// `ctx_enter_binder(bodyCtx, bindtree, binderId, ty)`:
//   Walk bindtree and bodyCtx in parallel. At every BE leaf, overlay
//   CtxV(binderId, ty). At BN leaves, keep bodyCtx untouched. At structural
//   nodes, recurse in parallel. BApp/BLam/BPi must line up with the body's
//   App/Lam/Pi shape — if not, that's a bug (guaranteed by construction
//   when bindtree was built correctly).
//
// This is the one-stop replacement for `splice(body, lamB, mkH ty depth)`.
function ctx_enter_binder(bodyCtx: Ctx, b: Bind, bid: BinderId, ty: Term): Ctx {
  switch (b.kind) {
    case "BE":
      // ASSERT bodyCtx.kind === "CtxNone" — BE means this spot is a V that
      // (by construction) belonged to no outer binder yet. Overlay.
      return { kind: "CtxV", binder: bid, ty }
    case "BN":
      return bodyCtx
    case "BApp": {
      // ASSERT bodyCtx is CtxApp or CtxNone (CtxNone early in a fresh frame).
      const ca = bodyCtx.kind === "CtxApp" ? bodyCtx.f : CtxNone
      const cx = bodyCtx.kind === "CtxApp" ? bodyCtx.x : CtxNone
      return {
        kind: "CtxApp",
        f: ctx_enter_binder(ca, b.f, bid, ty),
        x: ctx_enter_binder(cx, b.x, bid, ty),
      }
    }
    case "BLam": {
      const ca = bodyCtx.kind === "CtxLam" ? bodyCtx.a : CtxNone
      const cb = bodyCtx.kind === "CtxLam" ? bodyCtx.body : CtxNone
      return {
        kind: "CtxLam",
        a: ctx_enter_binder(ca, b.a, bid, ty),
        body: ctx_enter_binder(cb, b.body, bid, ty),
      }
    }
    case "BPi": {
      const ca = bodyCtx.kind === "CtxPi" ? bodyCtx.a : CtxNone
      const cc = bodyCtx.kind === "CtxPi" ? bodyCtx.codom : CtxNone
      return {
        kind: "CtxPi",
        a: ctx_enter_binder(ca, b.a, bid, ty),
        codom: ctx_enter_binder(cc, b.codom, bid, ty),
      }
    }
  }
}

// `ctx_app_split(ctx)`: project into (ctx_f, ctx_x) for descending an App.
// Just destructures; CtxNone splits to (CtxNone, CtxNone).
function ctx_app_split(c: Ctx): [Ctx, Ctx] {
  if (c.kind === "CtxApp") return [c.f, c.x]
  return [CtxNone, CtxNone]
}
function ctx_lam_split(c: Ctx): [Ctx, Ctx] {
  if (c.kind === "CtxLam") return [c.a, c.body]
  return [CtxNone, CtxNone]
}
function ctx_pi_split(c: Ctx): [Ctx, Ctx] {
  if (c.kind === "CtxPi") return [c.a, c.codom]
  return [CtxNone, CtxNone]
}

// `ctx_lookup_V(ctx)`: only meaningful at a ctx paired with a V-leaf term.
// Returns the binder's type.
function ctx_lookup_V(c: Ctx): Term {
  if (c.kind !== "CtxV") throw new Error("ctx shape does not match V leaf")
  return c.ty
}

// ────────────────────────────────────────────────────────────────────────
// Elaboration state (unchanged from predicates.disp — depth + metas)
// ────────────────────────────────────────────────────────────────────────
interface State { depth: BinderId; metas: unknown }
declare function state_next_depth(s: State): State
declare function mk_res<T>(b: boolean, s: State): { bool: boolean; state: State }
declare function and_(a: boolean, b: boolean): boolean
declare function try_unify(a: Term, b: Term, s: State): { bool: boolean; state: State }
declare function is_pi(t: Term): boolean
declare function is_lam(t: Term): boolean
declare function is_app(t: Term): boolean
declare function is_h(t: Term): boolean
declare function is_v(t: Term): boolean

// `normalize` changes too: takes (term, ctx) and threads ctx through beta.
// Stubbed here — the ctx-aware normalize is the second milestone.
declare function normalize(t: Term, c: Ctx): Term

// `infer(term, ctx)`: H → type_of_H, V → ctx_lookup_V, App(f,x) →
// piCodom(infer f) with x substituted into codom's V-positions. Substitution
// here is STILL via splice on the inferred Pi (an elab-time product, not a
// user term), so this stays as-is at first. Later, infer can become ctx-
// threaded too.
declare function infer(term: Term, ctx: Ctx): Term

// ────────────────────────────────────────────────────────────────────────
// pred_of_lvl (ctx-threaded)
// ────────────────────────────────────────────────────────────────────────
//
// Signature change: add `tyCtx` and `candCtx` alongside ty and cand.
// Public entry (`pred_of`) calls with CtxNone/CtxNone.
//
// Dispatch mirrors the current code exactly:
//   1. cand is Pi → Pi-as-Type (fast_eq Type (normalize ty tyCtx))
//   2. cand is Lam:
//        ntY = normalize(ty, tyCtx)
//        if is_pi(ntY):
//          bid = state.depth
//          state' = state_next_depth(state)
//          dom_res = try_unify(piA(ntY), lamA(cand), state')
//          // Extend each side's ctx for the binder it introduces.
//          // piCodom's V's now mean "an x:piA(ntY)" — tag with bid, type=piA.
//          // lamBody's V's mean "an x:lamA(cand)" — tag with bid, type=lamA.
//          // Since dom_res succeeded, piA and lamA are unifiable; we pick
//          // piA (canonical) for both — lamA may be a meta awaiting solve.
//          tyCtx'   = ctx_enter_binder(tyCtx_codom,   piB(ntY),  bid, piA(ntY))
//          candCtx' = ctx_enter_binder(candCtx_body,  lamB(cand), bid, piA(ntY))
//          rec_res = rec(piCodom(ntY), tyCtx', lamBody(cand), candCtx', dom_res.state)
//          return mk_res(and dom rec, rec_res.state)
//        else: FF
//   3. cand is App: current code, plus ctx_app_split on candCtx for appF/appX.
//   4. cand is H/atom: current check_atom_or_h (no ctx change needed — H
//      leaves carry their type directly; atoms are closed).
//   5. cand is V: NEW CASE. ctx_lookup_V(candCtx) → type. try_unify with ty.
//      (Previously this never arose because mkH splicing eliminated V's
//      before recursion reached them.)
//
// The V case (5) is the one brand-new branch. Everything else is the same
// logic with "splice(.., guide, mkH ty depth)" replaced by
// "ctx_enter_binder(..., guide, bid, ty)".
//
// Normalize stays stale-bindtree-buggy until its ctx rewrite lands; that's
// OK because pred_of_lvl is the smaller, tested-first refactor.
function pred_of_lvl(
  ty: Term, tyCtx: Ctx,
  cand: Term, candCtx: Ctx,
  state: State,
): { bool: boolean; state: State } {
  if (is_pi(cand)) {
    const ntY = normalize(ty, tyCtx)
    return mk_res(ntY /* === Type */ as any, state)
  }
  if (is_lam(cand)) {
    const ntY = normalize(ty, tyCtx)
    if (!is_pi(ntY)) return mk_res(false, state)
    const piA_ = (ntY as any).a as Term
    const piB_ = (ntY as any).b as Bind
    const piCod = (ntY as any).codom as Term
    const lamA_ = (cand as any).a as Term
    const lamB_ = (cand as any).b as Bind
    const lamBody = (cand as any).body as Term

    const s1 = state_next_depth(state)
    const bid = state.depth
    const domRes = try_unify(piA_, lamA_, s1)

    const [, tyCodCtx] = ctx_pi_split(normalizeCtx(tyCtx, ty, ntY))
    const [, candBodyCtx] = ctx_lam_split(candCtx)
    const tyCtx2 = ctx_enter_binder(tyCodCtx, piB_, bid, piA_)
    const candCtx2 = ctx_enter_binder(candBodyCtx, lamB_, bid, piA_)

    const recRes = pred_of_lvl(piCod, tyCtx2, lamBody, candCtx2, domRes.state)
    return mk_res(and_(domRes.bool, recRes.bool), recRes.state)
  }
  // is_app cand → elide; shape-identical to predicates.disp with
  // ctx_app_split threading candCtx into appF/appX recursive calls.
  // is_h cand → check_atom_or_h as today.
  // is_v cand → try_unify(ctx_lookup_V(candCtx), ty, state)
  throw new Error("elided")
}

// `normalizeCtx(ctx, beforeTerm, afterTerm)`: stub — when normalize rewrites
// the term, the parallel ctx must be rebuilt to match the NEW shape. In the
// pred_of_lvl-first milestone we sidestep this because ty/cand get
// normalized only via fast_eq comparison at the top (Pi-as-Type rule) or to
// extract Pi components (Lam-vs-Pi). For the Lam-vs-Pi path, `normalize` on
// ty produces a Pi whose piA/piB/piCodom are SUBTREES of ty's normal form;
// if tyCtx was built for ty's original shape, it is NOT automatically a
// ctx for ntY. Fixes:
//   A) (cheap, first pass) only allow non-reducing ty's at public entry —
//      true today for all 111 tests; normalize(ty) ≡ ty.
//   B) (real fix) ctx-aware normalize (milestone 2) returns (ntY, ntY_ctx).
// For the sketch we assume (A); the assertion in production code is
// `assert fast_eq(ntY, ty) || tyCtx === CtxNone`.
declare function normalizeCtx(ctx: Ctx, before: Term, after: Term): Ctx

// ────────────────────────────────────────────────────────────────────────
// What this buys us
// ────────────────────────────────────────────────────────────────────────
// - Term never mutated during check → `cand` hash-cons identity is stable
//   across recursion. (Today, splice-with-H gives a DIFFERENT tree than
//   the original, so downstream `fast_eq` checks must normalize first.)
// - Direct analogue for SKI-future: ctx_enter_binder is the operation that
//   converts "Pi + bindtree" into per-V type assignments. After bracket
//   abstraction, Lam vanishes but Pi + bindtree survive → ctx still builds
//   the same way from the Pi side. The term side becomes "walk the SKI
//   combinator tree"; the ctx-structural-walk replaces bindtree-guided
//   splice.
// - Normalize fix falls out: ctx-aware beta rewrites `ctx_for_body` with
//   `ctx_for_arg` spliced in (parallel to term rewrite), and fresh lamB's
//   are extracted by walking the OUTPUT ctx tagging positions still bound
//   to the enclosing binder.
//
// ────────────────────────────────────────────────────────────────────────
// Risks / corners to watch in the disp port
// ────────────────────────────────────────────────────────────────────────
// 1. Ctx shape is a SUBSET of Term shape (CtxNone at non-V leaves). The
//    disp implementation must tolerate "ctx ran out of structure before
//    term did" — e.g. an App inside an atom shouldn't care about ctx.
// 2. ctx_enter_binder traverses bindtree structure; bindtree sizes grow
//    ~linearly with binder scope, so no SKI-style blowup (unlike my
//    earlier tokenize-reabstract attempt).
// 3. try_unify with metas doesn't need ctx knowledge today because metas
//    resolve at the term level. If dependent metas become V-sensitive,
//    ctx must thread into try_unify too.
// 4. The V-case in pred_of_lvl is brand-new — no existing test exercises
//    it because splicing eliminated V's first. First new test:
//      `test pred_of (A -> A) (\(x : A). x)` — today passes via splice.
//      Under ctx: lamBody is `V`, candCtx is CtxV(bid, A), and the check
//      becomes try_unify(A, codom-with-bid-V) → must specialize codom's
//      V to bid too (already done by ctx_enter_binder on tyCtx). Then
//      leaf-level V-vs-V with matching bids collapses to try_unify(A, A)
//      → TT.
// 5. Assertion (A) in normalizeCtx — the 111 existing tests all have
//    ty-in-normal-form already; that's the regression oracle. When we
//    add Church-Eq-style tests whose ty reduces (e.g. `Eq A x x` at a
//    check site), we need milestone 2 (ctx-aware normalize) first.
