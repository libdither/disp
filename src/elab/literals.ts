// Value/identifier encoding: the lib's canonical Nat/String/accessor trees and
// their decoders, plus reading a SS2.6 record's field header off a compiled tree.

import type { Tree } from "../eval/eager.js"
import { elab, B, type ScopeEntry } from "./state.js"

// treePairFst(p): the left projection of a wait-form fork. A type is a
// wait-form; its left projection is the constant former-signature.
export function treePairFst(p: Tree): Tree | null {
  const c = elab.cs.classify!(p)
  return c.tag === "fork" ? c.left : null
}

// natLitTree(n): the lib's canonical Nat for `n` — zero = LEAF, succ(m) =
// `t t m` = fork(LEAF, m). Matches `succ`/`zero` applied in scope.
export function natLitTree(n: number): Tree {
  let result: Tree = elab.cs.leaf()
  for (let i = 0; i < n; i++) result = elab.cs.fork(elab.cs.leaf(), result)
  return result
}

// stringToTree(s): a string literal as a List of codepoint Nats (cons = fork,
// nil = LEAF) — bit-identical to the array literal `[c0, c1, …]` of its
// codepoints, so a string is a genuine String value and a deterministic,
// distinct tag per spelling. Reused to intern record field-name identifiers.
export function stringToTree(s: string): Tree {
  const codes = [...s].map(c => c.codePointAt(0)!)
  let result: Tree = elab.cs.leaf()
  for (let i = codes.length - 1; i >= 0; i--) result = elab.cs.fork(natLitTree(codes[i]), result)
  return result
}

// accTree(name): the §2.6 accessor for a field name — `acc name = inj name unit
// = fork(name, LEAF)`, with the name interned as a string tag. Applying a
// product (record) to it performs the cut and yields the named field.
export function accTree(name: string): Tree {
  return elab.cs.fork(stringToTree(name), elab.cs.leaf())
}

// treeToNat / treeToString: decode the lib encodings (Nat = nested fork(LEAF,·),
// String = a cons-chain of codepoint Nats) back to host values — the inverse of
// natLitTree / stringToTree. Used to read a record's field-name header.
export function treeToNat(t: Tree): number {
  let n = 0, cur = elab.cs.classify!(t)
  while (cur.tag === "fork") { n++; cur = elab.cs.classify!(cur.right) }
  return n
}
export function treeToString(t: Tree): string {
  const codes: number[] = []
  let cur = elab.cs.classify!(t)
  while (cur.tag === "fork") { codes.push(treeToNat(cur.left)); cur = elab.cs.classify!(cur.right) }
  return codes.length ? String.fromCodePoint(...codes) : ""
}

// recordFieldsFromTree(tree): if `tree` is a §2.6 record VALUE (a cut-rooted
// product — e.g. the output of `make_record` / `Enum`), read its field-name
// header and extract each field via the cut, so `open` works on a *computed*
// record, not only on statically-known `use` modules. Returns undefined for
// anything that isn't such a record — in particular library TYPES are
// recognizer-rooted (not cut-rooted), so they are correctly excluded.
// Needs the kernel in scope (`cut_sig`/`type_meta`/`pair_fst`); without
// it, returns undefined.
export function recordFieldsFromTree(
  tree: Tree,
  lookupEntry: (name: string) => ScopeEntry | undefined,
): { fields: string[]; fieldTrees?: Tree[]; fieldTypes?: (Tree | null)[]; fieldGuards?: (Tree | null)[] } | undefined {
  const cutSig = lookupEntry("cut_sig")?.tree
  const typeMeta = lookupEntry("type_meta")?.tree
  const pairFst = lookupEntry("pair_fst")?.tree
  if (!cutSig || !typeMeta || !pairFst) return undefined
  const sig = treePairFst(tree)
  if (!sig || !elab.cs.equal!(sig, cutSig)) return undefined
  // names = pair_fst (type_meta tree): a cons-chain of interned string tags.
  const namesTree = elab.cs.apply(pairFst, elab.cs.apply(typeMeta, tree, B()), B())
  const fields: string[] = []
  const fieldTrees: Tree[] = []
  let cur = elab.cs.classify!(namesTree)
  while (cur.tag === "fork") {
    const name = treeToString(cur.left)
    fields.push(name)
    fieldTrees.push(elab.cs.apply(tree, accTree(name), B()))
    cur = elab.cs.classify!(cur.right)
  }
  return fields.length > 0 ? { fields, fieldTrees } : undefined
}
