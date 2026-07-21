// Human decode of a value over the Session ABI (classify-based, so it works on
// JS trees and native handles alike). Display priority per node: leaf → `t`,
// nat chain → the number, codepoint cons-chain → "string", whole-subtree name
// match → the bound name, record shape → { f := v, … }, else structural
// t(…)(…) under a printed-node budget (elided as …). Mirrors the playground
// worker's decoding (website/src/lib/disp/runner.ts) for the CLI.

import type { Session } from "../eval/types.js"

/// Name-map key: hash-cons id for JS trees, the handle itself for handle backends.
export const nameKey = (h: unknown): unknown =>
  h !== null && typeof h === "object" && "id" in (h as object) ? (h as { id: unknown }).id : h

export interface PrintValueOptions {
  names?: Map<unknown, string>
  /// Suppress this name on matches (a def's own root would print as itself).
  skipName?: string
  budget?: number
}

export function printValue<H>(session: Session<H>, root: H, opts: PrintValueOptions = {}): string {
  const classify = session.classify?.bind(session)
  if (!classify) return `<handle ${String(root)}>`
  let left = opts.budget ?? 400

  const decodeNat = (h: H): number | null => {
    let n = 0, cur = h
    for (;;) {
      const c = classify(cur)
      if (c.tag === "leaf") return n
      if (c.tag !== "fork" || classify(c.left).tag !== "leaf") return null
      n++
      cur = c.right
      if (n > 1_000_000) return null
    }
  }
  const decodeString = (h: H): string | null => {
    const codes: number[] = []
    let cur = h
    for (;;) {
      const c = classify(cur)
      if (c.tag === "leaf") break
      if (c.tag !== "fork") return null
      const code = decodeNat(c.left)
      if (code === null || code < 32 || code > 0x10ffff) return null
      codes.push(code)
      cur = c.right
      if (codes.length > 4096) return null
    }
    return codes.length > 0 ? String.fromCodePoint(...codes) : null
  }
  const decodeList = (h: H, max = 256): H[] | null => {
    const out: H[] = []
    let cur = h
    for (;;) {
      const c = classify(cur)
      if (c.tag === "leaf") return out
      if (c.tag !== "fork" || out.length >= max) return null
      out.push(c.left)
      cur = c.right
    }
  }
  /// Record VALUE shape: fork(string cons-list, cons-list of list_const-wrapped fields).
  const decodeRecord = (h: H): { names: string[]; vals: H[] } | null => {
    const c = classify(h)
    if (c.tag !== "fork") return null
    const nameHs = decodeList(c.left)
    if (nameHs === null || nameHs.length === 0) return null
    const names: string[] = []
    for (const nh of nameHs) {
      const s = decodeString(nh)
      if (s === null) return null
      names.push(s)
    }
    const valHs = decodeList(c.right)
    if (valHs === null || valHs.length !== names.length) return null
    const vals = valHs.map(v => {
      const vc = classify(v)
      return vc.tag === "fork" && classify(vc.left).tag === "leaf" ? vc.right : v
    })
    return { names, vals }
  }

  const sizeAtLeast = (h: H, min: number): boolean => {
    let count = 0
    const stack: H[] = [h]
    while (stack.length > 0) {
      const c = classify(stack.pop()!)
      if (++count >= min) return true
      if (c.tag === "stem") stack.push(c.child)
      else if (c.tag === "fork") stack.push(c.left, c.right)
    }
    return false
  }

  // Tiny trees coincide with combinator fragments (`t t` is both `false` and K),
  // so interior positions only decode atoms/names on subtrees big enough to be
  // meaningful; value roots (the binding itself, record fields) always decode.
  const go = (h: H, valueRoot: boolean): string => {
    const c = classify(h)
    if (c.tag === "leaf") return "t"
    const decodeHere = valueRoot || sizeAtLeast(h, 8)
    if (decodeHere) {
      const n = decodeNat(h)
      if (n !== null && n > 0) return String(n)
      const s = decodeString(h)
      if (s !== null) return JSON.stringify(s)
      const name = opts.names?.get(nameKey(h))
      if (name !== undefined && name !== opts.skipName) return name
    }
    if (left-- <= 0) return "…"
    const rec = decodeRecord(h)
    if (rec !== null)
      return `{ ${rec.names.map((f, i) => `${f} := ${go(rec.vals[i], true)}`).join(", ")} }`
    if (c.tag === "stem") return `t(${go(c.child, false)})`
    return `t(${go(c.left, false)})(${go(c.right, false)})`
  }
  return go(root, true)
}
