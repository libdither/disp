// Pretty printer for CoC-encoded terms.
// Separated from coc.ts for modularity — this handles display logic only.

import { type Tree, treeEqual, apply } from "./tree.js"
import {
  termTag, unVar, unApp, unLam, unPi,
  encVar, freshMarker,
  whnfTree, evalToNative,
  TREE_TYPE, BOOL_TYPE, NAT_TYPE,
} from "./coc.js"

let printVarCounter = 0
const PRINT_VAR_NAMES = "abcdefghijklmnopqrstuvwxyz"

function freshPrintVar(): string {
  const idx = printVarCounter++
  if (idx < 26) return PRINT_VAR_NAMES[idx]
  return `v${idx}`
}

export function printEncoded(t: Tree, nameMap?: Map<number, string>, budget = { remaining: 10000 }): string {
  const savedCounter = printVarCounter
  printVarCounter = 0
  const result = printEncodedInner(t, nameMap, budget)
  printVarCounter = savedCounter
  return result
}

// Try to display a natively-evaluated tree as a recognizable value.
// Unlike the encoding-level printer, LEAF here is the data value (true/zero/leaf), not Type.
function printNativeValue(t: Tree, nameMap?: Map<number, string>): string | null {
  if (nameMap) { const name = nameMap.get(t.id); if (name) return name }
  if (treeEqual(t, TREE_TYPE)) return "Tree"
  if (treeEqual(t, BOOL_TYPE)) return "Bool"
  if (treeEqual(t, NAT_TYPE)) return "Nat"
  if (t.tag === "stem") {
    let n: Tree = t, count = 0
    while (n.tag === "stem") { count++; n = n.child }
    if (n.tag === "leaf") return String(count)
  }
  return null
}

function printEncodedInner(t: Tree, nameMap?: Map<number, string>, budget = { remaining: 10000 }): string {
  if (treeEqual(t, TREE_TYPE)) return "Tree"
  if (treeEqual(t, BOOL_TYPE)) return "Bool"
  if (treeEqual(t, NAT_TYPE)) return "Nat"
  if (nameMap) { const name = nameMap.get(t.id); if (name) return name }
  const w = whnfTree(t, budget)
  if (treeEqual(w, TREE_TYPE)) return "Tree"
  if (treeEqual(w, BOOL_TYPE)) return "Bool"
  if (treeEqual(w, NAT_TYPE)) return "Nat"
  if (nameMap && w.id !== t.id) { const name = nameMap.get(w.id); if (name) return name }
  const tag = termTag(w)
  switch (tag) {
    case "type": return "Type"
    case "var": {
      const marker = unVar(w)!
      if (nameMap) { const name = nameMap.get(marker.id); if (name) return name }
      return `?${marker.id}`
    }
    case "app": {
      const nativeResult = evalToNative(w, { remaining: 10000 })
      if (nativeResult !== null) {
        const display = printNativeValue(nativeResult, nameMap)
        if (display) return display
      }
      const a = unApp(w)!
      return `${printEncodedInner(a.func, nameMap, budget)} ${printEncodedAtom(a.arg, nameMap, budget)}`
    }
    case "lam": {
      const l = unLam(w)!
      const varName = freshPrintVar()
      const m = freshMarker()
      const extMap = new Map(nameMap ?? [])
      extMap.set(m.id, varName)
      const bodyApplied = apply(l.body, encVar(m))
      return `{${varName}} -> ${printEncodedInner(bodyApplied, extMap, budget)}`
    }
    case "pi": {
      const p = unPi(w)!
      const m = freshMarker()
      const neutral = encVar(m)
      const bodyApplied = apply(p.body, neutral)
      const domStr = printEncodedInner(p.domain, nameMap, budget)
      const isDep = treeContains(bodyApplied, neutral)
      if (isDep) {
        const varName = freshPrintVar()
        const extMap = new Map(nameMap ?? [])
        extMap.set(m.id, varName)
        return `(${varName} : ${domStr}) -> ${printEncodedInner(bodyApplied, extMap, budget)}`
      }
      return `(${domStr}) -> ${printEncodedInner(bodyApplied, nameMap, budget)}`
    }
    default:
      return `<tree:${w.id}>`
  }
}

function printEncodedAtom(t: Tree, nameMap?: Map<number, string>, budget = { remaining: 10000 }): string {
  if (nameMap) { const name = nameMap.get(t.id); if (name) return name }
  const tag = termTag(t)
  if (tag === "type" || tag === "var") return printEncodedInner(t, nameMap, budget)
  return `(${printEncodedInner(t, nameMap, budget)})`
}

function treeContains(tree: Tree, target: Tree): boolean {
  if (treeEqual(tree, target)) return true
  if (tree.tag === "leaf") return false
  if (tree.tag === "stem") return treeContains(tree.child, target)
  return treeContains(tree.left, target) || treeContains(tree.right, target)
}
