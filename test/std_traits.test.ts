// Host-side pins for lexical type-head dot dispatch, anti-action-at-a-distance ambiguity
// errors, promoted-kernel `given` + `map_law` verification rejections, and both-order
// `.opt.disp` open-splice reconciliation on `Type.method` stdlib bindings.

import { describe, it, expect, beforeAll, beforeEach, afterEach, afterAll } from "vitest"
import { join } from "node:path"
import { parseProgram, collectSessionRoots } from "../src/compile.js"
import { getBackend, defaultBackendName } from "../src/eval/registry.js"
import type { Session } from "../src/eval/types.js"
import type { Tree } from "../src/eval/eager.js"

const HERE = join(process.cwd(), "lib/_std_traits_host.disp")

type ScopedSession = Session<Tree> & {
  beginScope?(): void
  endScope?(keep: Tree[]): void
  clearCaches?(): void
  dispose?(): void
}

describe("std traits, Type.method dispatch, and verified overlays", () => {
  let session!: ScopedSession
  const run = (src: string) => parseProgram(src, HERE, { session })
  const getBinding = (src: string, name: string) => {
    const d = run(src).find(x => x.kind === "Def" && x.name === name)
    if (!d || d.kind !== "Def") throw new Error(`missing ${name}`)
    return d.tree
  }

  beforeAll(() => {
    session = getBackend(process.env.DISP_EVALUATOR ?? defaultBackendName)
      .createSession() as unknown as ScopedSession
  })
  beforeEach(() => { session.beginScope?.() })
  afterEach(() => {
    if (session.endScope) session.endScope(collectSessionRoots(session))
    else session.clearCaches?.()
  })
  afterAll(() => { session.dispose?.() })

  it("lexical type-head dot dispatch resolves typed receivers and rejects untyped ambiguity loudly", () => {
    // Typed receivers resolve cleanly even with Option.map, Result.map, and List.map all in scope
    expect(() => run(`
open use "std.disp"
f_opt := {x : Option(Nat)} => x.map(succ).unwrap_or(0)
f_res := {x : Result(Nat, Tree)} => x.map(succ).unwrap_or(0)
f_lst := {x : List(Nat)} => x.map(succ).len
test f_opt(Some(3)) = 4
test f_res(Ok(3)) = 4
test f_lst([1, 2, 3]) = 3
`)).not.toThrow()

    // Untyped receiver with multiple `.map` methods in scope MUST error loudly (no silent hijacking)
    expect(() => run(`
open use "std.disp"
bad := {x} => x.map(succ)
`)).toThrow(/\.map' is ambiguous/)

    // Explicit qualification always resolves with clear provenance
    expect(() => run(`
open use "std.disp"
explicit_opt := {x} => Option.map(x, succ)
explicit_lst := {x} => List.map(x, succ)
test explicit_opt(Some(2)) = Some(3)
test explicit_lst([1, 2]) = [2, 3]
`)).not.toThrow()
  }, 180000)

  it("promoted-kernel `given` checks reject ill-typed fills and lying Monad `map_law` proofs", () => {
    // Ill-typed `pure` fill is caught at load time
    expect(() => run(`
open use "std.disp"
BadPure := use "traits/monad.disp" { M := Option, pure := 42, flat_map := Option.and_then }
`)).toThrow(/type check failed.*monad\.disp/)

    // A buggy `map_impl` paired with a lying `map_law` proof is rejected by the kernel
    expect(() => run(`
open use "std.disp"
let bad_map := {_opt, _f} => None
BadLaw := use "traits/monad.disp" {
  M := Option,
  pure := Some,
  flat_map := Option.and_then,
  map_impl := bad_map,
  map_law := {opt, _f} => if (opt.fst == "Some") { t } else { t }
}
`)).toThrow(/type check failed.*monad\.disp/)
  }, 180000)

  it("lib/nat.opt.disp reconciles in BOTH open orders and rejects buggy Nat.add rebinds", () => {
    const specAdd = getBinding(`open use "nat.disp"\n`, "Nat.add")
    const optAdd = getBinding(`open use "nat.opt.disp"\n`, "Nat.add")
    expect(session.equal!(specAdd, optAdd)).toBe(false)

    const order1 = getBinding(`open use "nat.disp"\nopen use "nat.opt.disp"\n`, "Nat.add")
    const order2 = getBinding(`open use "nat.opt.disp"\nopen use "nat.disp"\n`, "Nat.add")
    expect(session.equal!(order1, optAdd)).toBe(true)
    expect(session.equal!(order2, optAdd)).toBe(true)

    expect(() => run(`
open use "std.disp"
Nat.add := { new := {a, _b} => a; proof := {a, _b} => if (is_zero(a)) { t } else { t } }
`)).toThrow(/rejected by its guard/)
  }, 180000)

  it("lib/list.opt.disp reconciles in BOTH open orders and rejects buggy List.map rebinds", () => {
    const specMap = getBinding(`open use "list.disp"\n`, "List.map")
    const optMap = getBinding(`open use "list.opt.disp"\n`, "List.map")
    expect(session.equal!(specMap, optMap)).toBe(false)

    const order1 = getBinding(`open use "list.disp"\nopen use "list.opt.disp"\n`, "List.map")
    const order2 = getBinding(`open use "list.opt.disp"\nopen use "list.disp"\n`, "List.map")
    expect(session.equal!(order1, optMap)).toBe(true)
    expect(session.equal!(order2, optMap)).toBe(true)

    expect(() => run(`
open use "std.disp"
List.map := { new := {_xs, _f} => []; proof := {xs, _f} => if (xs.is_nil) { t } else { t } }
`)).toThrow(/rejected by its guard/)
  }, 180000)

  it("lib/growth.opt.disp reconciles in BOTH open orders and rejects buggy sizes_upto", () => {
    const specSz = getBinding(`open use "growth.disp"\n`, "sizes_upto")
    const optSz = getBinding(`open use "growth.opt.disp"\n`, "sizes_upto")
    expect(session.equal!(specSz, optSz)).toBe(false)

    const order1 = getBinding(`open use "growth.disp"\nopen use "growth.opt.disp"\n`, "sizes_upto")
    const order2 = getBinding(`open use "growth.opt.disp"\nopen use "growth.disp"\n`, "sizes_upto")
    expect(session.equal!(order1, optSz)).toBe(true)
    expect(session.equal!(order2, optSz)).toBe(true)

    expect(() => run(`
open use "std.disp"
open use "growth.disp"
let rec bad_sz := {k} => if (is_zero(k)) { [] } else { cons(k, bad_sz(nat_pred(k))) }
sizes_upto := { new := bad_sz; proof := {k} => if (is_zero(k)) { t } else { t } }
`)).toThrow(/rejected by its guard/)
  }, 180000)
})
