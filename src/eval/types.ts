// The evaluator Session ABI (EVALUATOR_PLAN.md §3).
//
// The engine (elaborator + verifier + test runner) talks to an evaluator only
// through this interface, so `--evaluator=<backend>` can run the whole pipeline
// on the eager TS reference, a naive conformance backend, or a future Rust/WASM
// interaction-net runtime. Handles are OPAQUE: a session hands them out and
// takes them back, but the engine never inspects their representation — the one
// bridge from handle-space to host-value-space is `dumpTernary`.
//
// The interface is parameterized by the handle type `H` for TS ergonomics; an
// engine that wants true backend-independence treats `H` as `unknown`. The eager
// reference backend instantiates `H = Tree`.

// A reduction budget: a DIVERGENCE bound, not a semantic gate (EVALUATOR_PLAN
// decision 9). `remaining` is decremented per backend-defined unit (eager:
// reduction steps; a net: interactions); BudgetExhausted fires at zero. A
// generous budget admits every total program on every backend, so budget
// CONSUMPTION is a non-portable observation and is never compared across
// backends.
export interface Budget {
  remaining: number
  // Optional: the budget's original limit, reported by the exhaustion error
  // (absent = the message omits the number).
  limit?: number
}

// Weak-head classification of a node (the result of one triage observation).
// Children are themselves handles (possibly suspended under a lazy backend; the
// next classify/equal/dump forces them).
export type Classification<H> =
  | { tag: "leaf" }
  | { tag: "stem"; child: H }
  | { tag: "fork"; left: H; right: H }

// Backend-reported counters. Units are backend-declared and never compared
// across backends; `steps` is the only field every backend is expected to
// supply (others are optional and backend-specific — eager fills in rule
// breakdowns, a net would report interaction counts).
export interface EvalStats {
  readonly steps: number
  readonly [key: string]: number
}

export interface SessionOpts {
  // Per-call default budget, in the backend's own unit — never a shared constant
  // (EVALUATOR_PLAN §8: budget-unit drift).
  defaultBudget?: number
  // Conformance mode: run standard natives (tree_eq) in-language instead of
  // intercepting them, so the native fast-path can be validated against the real
  // compiled definition (EVALUATOR_PLAN §3.1).
  noNativeIntercept?: boolean
  // Replaces the module-global trace buffer.
  trace?: (event: unknown) => void
}

// A session owns all evaluator state (memo tables, arenas, stats, the native
// fast-path id). No module-global evaluator state lives outside it.
export interface Session<H = unknown> {
  // ── term algebra ──
  leaf(): H
  stem(child: H): H
  fork(left: H, right: H): H

  // ── computation ──
  // May return a suspended handle under lazy backends; dump/equal/classify force.
  apply(f: H, x: H, budget?: Budget): H

  // OPTIONAL lazy apply: build the suspended application `P(f,x)` in O(1) WITHOUT
  // reducing, deferring the work to the next forcing observation (dump/equal/
  // classify). Lets a caller build up a large computation and force it all at once
  // at the end — where work-sharing (and, later, parallel reduction) applies. Absent
  // on eager backends (no suspension); callers fall back to eager `apply`.
  applyLazy?(f: H, x: H): H

  // ── bulk ops (the anti-FFI-chattiness path and the interchange format) ──
  loadTernary(s: string): H
  // Forces full normal form, so it is boundable: under a lazy backend THIS is
  // where deferred reduction runs.
  dumpTernary(h: H, budget?: Budget): string

  // ── observations (optional native overrides of engine-side derivations) ──
  // equal: decidable structural equality of normal forms. DERIVABLE via the
  // standard tree_eq tree; backends override for speed (eager: O(1) hash-cons).
  equal?(a: H, b: H, budget?: Budget): boolean
  // classify: weak-head inspection. DERIVABLE via one triage step; backends
  // override natively. Forces WHNF under lazy backends.
  classify?(h: H, budget?: Budget): Classification<H>
  stats?(): EvalStats

  // Tell the backend that `handle` is the standard native `name` (today only
  // "tree_eq"), so it may intercept saturated applications with its native
  // fast-path. Idempotent; a no-op on backends without a fast path (they run the
  // in-language definition). This is the pragmatic registration hook; sourcing
  // the identity from a committed hash registry instead (EVALUATOR_PLAN §3.1,
  // recognition-not-registration) is the planned refinement.
  recognizeNative?(name: string, handle: H): void

  // OPTIONAL scoped reclamation. beginScope() marks a reclamation point; endScope(keep)
  // frees every node the session allocated since the matching beginScope EXCEPT those
  // reachable from `keep` (the handles to preserve out of the scope). For backends with an
  // explicit arena the host can't GC (rust-eager): a scoped evaluation that produces a few
  // results but lots of intermediate garbage (a module's exports, an optimizer candidate's
  // NF) reclaims the garbage in one step. The eager TS backend needs it too — V8 cannot
  // collect an INTERNED Tree (the hash-cons tables pin every node), so its endScope
  // un-interns the scope's unreachable nodes to release them (core/tree.ts § Scoped
  // reclamation). Trust model: the host must list every handle it keeps live past the
  // scope — a forgotten survivor dangles (rust: freed slot; eager: an un-interned tree
  // whose structural rebuild no longer shares its id), caught by the differential oracle
  // / a debug poison.
  beginScope?(): void
  endScope?(keep: H[]): void

  // True iff handles are canonical (a === b implies equal(a,b)). The engine may
  // use this ONLY as an optimization gate (e.g. the run.ts name registry).
  readonly canonicalHandles: boolean

  // Free everything the session owns. Near no-op on the TS backend (GC suffices);
  // load-bearing for a WASM/native backend whose arena the JS GC cannot see.
  dispose(): void
}

// A backend is a factory plus a static, build-time report of which standard
// natives it recognizes (name -> content hashes of the canonical ternary
// encoding). That report is an input to the engine-side compatibility assertion
// (EVALUATOR_PLAN §3.1); it is never consulted during reduction.
export interface EvalBackend<H = unknown> {
  readonly name: string
  natives(): ReadonlyMap<string, readonly string[]>
  createSession(opts?: SessionOpts): Session<H>
}
