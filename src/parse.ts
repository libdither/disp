// Surface syntax → tree calculus, via bracket abstraction.
//
// Surface:
//   def NAME = EXPR
//   test EXPR = EXPR
//   ; line comment
//
// Term grammar:
//   △ | t            tree leaf (LEAF)
//   x                identifier (variable or top-level def name)
//   \x y z. body     lambda (desugars to nested \x. \y. \z. body)
//   f x              application (left-associative juxtaposition)
//   ( e )            grouping
//
// Compilation pipeline:
//   1. Parse surface → SIR (surface IR with named binders).
//   2. Resolve globals → CIR (combinator IR with explicit S/K/I sentinels).
//   3. Bracket-abstract each lambda → still in CIR.
//   4. cir_to_tree pattern-matches S/K/I uses and emits a tree.
//
// Bracket abstraction stays in CIR throughout so we can distinguish "this fork
// came from an Application" from "this is the data tree to K-wrap as a literal".
// The conversion to tree happens once, at the end.

import { Tree, LEAF, stem, fork, treeApply, applyTree, treeEqual, isLeaf, isStem, isFork, prettyTree, FAST_EQ } from "./tree.js"
import { elab as elabSurface, type Surface, type Env as ElabEnv, freshMarker, mkH } from "./elaborate.js"

// ===== Tokens =====
type Tok =
  | { t: "id"; v: string }
  | { t: "punct"; v: string }
  | { t: "kw"; v: string }
  | { t: "leaf" }
  | { t: "eof" }

const KEYWORDS = new Set(["def", "test", "elab"])
const PUNCT = ["\\", ".", "(", ")", "=", ":", "->", "→"] as const

export function tokenize(src: string): Tok[] {
  const toks: Tok[] = []
  let i = 0
  while (i < src.length) {
    const c = src[i]
    if (/\s/.test(c)) { i++; continue }
    if (c === ";") { while (i < src.length && src[i] !== "\n") i++; continue }
    if (c === "△" || (c === "t" && (i + 1 >= src.length || !/[A-Za-z0-9_']/.test(src[i + 1])))) {
      toks.push({ t: "leaf" }); i += c.length; continue
    }
    let matched = false
    for (const p of PUNCT) {
      if (src.startsWith(p, i)) { toks.push({ t: "punct", v: p }); i += p.length; matched = true; break }
    }
    if (matched) continue
    if (/[A-Za-z_]/.test(c)) {
      let j = i
      while (j < src.length && /[A-Za-z0-9_']/.test(src[j])) j++
      const word = src.slice(i, j)
      toks.push(KEYWORDS.has(word) ? { t: "kw", v: word } : { t: "id", v: word })
      i = j
      continue
    }
    throw new Error(`tokenize: unexpected ${JSON.stringify(c)} at offset ${i}`)
  }
  toks.push({ t: "eof" })
  return toks
}

// ===== Surface IR =====
type SIR =
  | { tag: "leaf" }
  | { tag: "var"; name: string }
  | { tag: "app"; f: SIR; x: SIR }
  | { tag: "lam"; x: string; body: SIR }

// ===== Combinator IR =====
// CIR carries explicit S/K/I sentinels. Bracket abstraction works in this space;
// cir_to_tree converts at the end via pattern matching on S/K/I uses.
type CIR =
  | { tag: "lit"; t: Tree }                 // a closed tree
  | { tag: "var"; name: string }            // free variable (will be abstracted)
  | { tag: "app"; f: CIR; x: CIR }          // application
  | { tag: "lam"; x: string; body: CIR }    // unabstracted lambda
  | { tag: "S" } | { tag: "K" } | { tag: "I" }

const S_SENT: CIR = { tag: "S" }
const K_SENT: CIR = { tag: "K" }
const I_SENT: CIR = { tag: "I" }
const cap = (f: CIR, x: CIR): CIR => ({ tag: "app", f, x })

const I_TREE = fork(fork(LEAF, LEAF), LEAF)
const K_TREE = stem(LEAF)
// S as a closed tree. Derivation:
//   apply(S_TREE, c)             = stem(stem(c))      (via S-rule on (S K_LEAF LEAF))
//   apply(stem(stem(c)), b)      = fork(stem(c), b)   (stem rule)
//   apply(fork(stem(c), b), x)   = apply(apply(c,x), apply(b,x))  (S rule)
const S_TREE = fork(stem(fork(LEAF, LEAF)), LEAF)

// ===== Parser =====

export type Decl =
  | { kind: "Def"; name: string; tree: Tree }
  | { kind: "Test"; lhs: Tree; rhs: Tree }

export function parseProgram(src: string): Decl[] {
  const toks = tokenize(src)
  let p = 0
  const peek = (): Tok => toks[p]
  const eat = (): Tok => toks[p++]
  const expectPunct = (v: string) => {
    const t = eat()
    if (t.t !== "punct" || t.v !== v) throw new Error(`expected '${v}' got ${JSON.stringify(t)}`)
  }

  function parseAtom(): SIR {
    const t = peek()
    if (t.t === "punct" && t.v === "(") { eat(); const e = parseTerm(); expectPunct(")"); return e }
    if (t.t === "leaf") { eat(); return { tag: "leaf" } }
    if (t.t === "id") { eat(); return { tag: "var", name: t.v } }
    throw new Error(`parseAtom: unexpected ${JSON.stringify(t)}`)
  }
  const isAtomStart = (t: Tok) => t.t === "id" || t.t === "leaf" || (t.t === "punct" && t.v === "(")

  function parseApp(): SIR {
    let head = parseAtom()
    while (isAtomStart(peek())) head = { tag: "app", f: head, x: parseAtom() }
    return head
  }

  function parseLam(): SIR {
    expectPunct("\\")
    const names: string[] = []
    while (peek().t === "id") names.push((eat() as { v: string }).v)
    if (names.length === 0) throw new Error("\\: expected at least one binder name")
    expectPunct(".")
    let body = parseTerm()
    for (let i = names.length - 1; i >= 0; i--) body = { tag: "lam", x: names[i], body }
    return body
  }

  function parseTerm(): SIR {
    if (peek().t === "punct" && (peek() as { v: string }).v === "\\") return parseLam()
    return parseApp()
  }

  // ===== Typed surface parser (for `elab` declarations) =====
  // Grammar:
  //   typed   ::= '\(' id ':' typed ')' '.' typed     -- annotated lambda
  //             | '(' id ':' typed ')' '->' typed     -- dependent Pi
  //             | arrow
  //   arrow   ::= app ('->' typed)?                    -- right-assoc non-dep arrow
  //   app     ::= atom atom*
  //   atom    ::= '(' typed ')' | id | '△' | 't'

  function parseTypedAtom(): Surface {
    const t = peek()
    if (t.t === "punct" && t.v === "(") {
      // Could be (e), (x : T) -> R, or (x : T) annotation following \
      // For atoms, just (e). Pi syntax is parsed in parseTyped at the term level.
      eat()
      const e = parseTyped()
      expectPunct(")")
      return e
    }
    if (t.t === "leaf") { eat(); return { tag: "leaf" } }
    if (t.t === "id") { eat(); return { tag: "var", name: t.v } }
    throw new Error(`parseTypedAtom: unexpected ${JSON.stringify(t)}`)
  }
  const isTypedAtomStart = (t: Tok) => t.t === "id" || t.t === "leaf" || (t.t === "punct" && t.v === "(")

  function parseTypedApp(): Surface {
    let head = parseTypedAtom()
    while (isTypedAtomStart(peek())) head = { tag: "app", f: head, x: parseTypedAtom() }
    return head
  }

  function parseTypedArrow(): Surface {
    const left = parseTypedApp()
    const t = peek()
    if (t.t === "punct" && (t.v === "->" || t.v === "→")) {
      eat()
      const right = parseTyped()
      return { tag: "arrow", dom: left, cod: right }
    }
    return left
  }

  function parseTyped(): Surface {
    const t = peek()
    if (t.t === "punct" && t.v === "\\") return parseTypedLam()
    if (t.t === "punct" && t.v === "(") {
      // Could be a dependent Pi `(x : T) -> R`. Look ahead.
      const save = p
      eat()  // consume (
      if (peek().t === "id") {
        const id = (eat() as { v: string }).v
        if (peek().t === "punct" && (peek() as { v: string }).v === ":") {
          eat()  // consume :
          const T = parseTyped()
          expectPunct(")")
          if (peek().t === "punct" && ((peek() as { v: string }).v === "->" || (peek() as { v: string }).v === "→")) {
            eat()
            const R = parseTyped()
            return { tag: "pi", x: id, dom: T, cod: R }
          }
          throw new Error(`elab: '(${id} : T)' must be followed by '->'`)
        }
      }
      // Not a Pi; rewind and let parseTypedArrow handle it as a parenthesized expr.
      p = save
    }
    return parseTypedArrow()
  }

  function parseTypedLam(): Surface {
    expectPunct("\\")
    if (!(peek().t === "punct" && (peek() as { v: string }).v === "(")) {
      throw new Error("elab: '\\' must be followed by '(x : T)'")
    }
    eat()  // (
    if (peek().t !== "id") throw new Error("elab: expected binder name after '\\('")
    const name = (eat() as { v: string }).v
    expectPunct(":")
    const T = parseTyped()
    expectPunct(")")
    expectPunct(".")
    const body = parseTyped()
    return { tag: "alam", x: name, type: T, body }
  }

  // ===== Compiler =====

  const globals = new Map<string, Tree>()
  globals.set("fast_eq", FAST_EQ)

  // Step 1: SIR → CIR with globals resolved to lit nodes.
  function sir_to_cir(e: SIR): CIR {
    switch (e.tag) {
      case "leaf": return { tag: "lit", t: LEAF }
      case "var": {
        const t = globals.get(e.name)
        if (t) return { tag: "lit", t }
        return { tag: "var", name: e.name }   // bound by an enclosing lam
      }
      case "app": return { tag: "app", f: sir_to_cir(e.f), x: sir_to_cir(e.x) }
      case "lam": return { tag: "lam", x: e.x, body: sir_to_cir(e.body) }
    }
  }

  // Free variable check.
  function contains_free(e: CIR, name: string): boolean {
    switch (e.tag) {
      case "lit": return false
      case "var": return e.name === name
      case "app": return contains_free(e.f, name) || contains_free(e.x, name)
      case "lam": return e.x === name ? false : contains_free(e.body, name)
      case "S": case "K": case "I": return false
    }
  }

  // Bracket abstraction: \name. body, in CIR.
  function abstract(name: string, body: CIR): CIR {
    if (!contains_free(body, name)) return cap(K_SENT, body)   // K-wrap
    switch (body.tag) {
      case "var": return I_SENT                                 // body is the var
      case "app": return cap(cap(S_SENT, abstract(name, body.f)), abstract(name, body.x))
      case "lam": {
        // \name. \y. inner: abstract y first, then name.
        const inner = abstract(body.x, body.body)
        return abstract(name, inner)
      }
      // contains_free returns false for these; unreachable here.
      case "lit": case "S": case "K": case "I":
        throw new Error("abstract: unreachable")
    }
  }

  // Eliminate all lambdas in a CIR by bracket abstraction.
  function eliminate_lams(e: CIR): CIR {
    switch (e.tag) {
      case "lit": case "var": case "S": case "K": case "I": return e
      case "app": return cap(eliminate_lams(e.f), eliminate_lams(e.x))
      case "lam": return abstract(e.x, eliminate_lams(e.body))
    }
  }

  // Step 2 result → Tree. Pattern-match S/K/I uses.
  function cir_to_tree(e: CIR): Tree {
    switch (e.tag) {
      case "lit": return e.t
      case "var": throw new Error(`cir_to_tree: free variable ${e.name}`)
      case "I": return I_TREE
      case "K": return K_TREE
      case "S": return S_TREE
      case "app": {
        // ap(ap(S, A), B)  →  fork(stem(A_tree), B_tree)
        if (e.f.tag === "app" && e.f.f.tag === "S") {
          return fork(stem(cir_to_tree(e.f.x)), cir_to_tree(e.x))
        }
        // ap(K, A)  →  fork(LEAF, A_tree)
        if (e.f.tag === "K") {
          return fork(LEAF, cir_to_tree(e.x))
        }
        // ap(I, A)  →  A_tree  (I x = x)
        if (e.f.tag === "I") {
          return cir_to_tree(e.x)
        }
        // Generic: apply via the runtime reducer.
        return applyTree(cir_to_tree(e.f), cir_to_tree(e.x), 10_000_000)
      }
    }
  }

  function compile(e: SIR): Tree {
    const cir = sir_to_cir(e)
    const closed = eliminate_lams(cir)
    return cir_to_tree(closed)
  }

  // ===== Top-level =====
  const decls: Decl[] = []
  while (peek().t !== "eof") {
    const t = eat()
    if (t.t === "kw" && t.v === "def") {
      const id = eat()
      if (id.t !== "id") throw new Error("def: expected name")
      // `def NAME : T = EXPR` — typed form. Elaborates both sides, then runs
      // the user-defined `check` from globals (must be in scope). Stores the
      // elaborated tagged tree if check returns TT (LEAF); throws otherwise.
      if (peek().t === "punct" && (peek() as { v: string }).v === ":") {
        eat()  // consume :
        const typeS = parseTyped()
        expectPunct("=")
        const exprS = parseTyped()
        const typeT = elabSurface(typeS, new Map(globals))
        const exprT = elabSurface(exprS, new Map(globals))
        const checkFn = globals.get("check")
        if (!checkFn) throw new Error(`def ${id.v}: 'check' must be defined before typed defs`)
        const res = applyTree(applyTree(checkFn, exprT), typeT, 10_000_000)
        if (!treeEqual(res, LEAF)) {
          throw new Error(`def ${id.v}: type check failed (got ${prettyTree(res)})`)
        }
        globals.set(id.v, exprT)
        decls.push({ kind: "Def", name: id.v, tree: exprT })
      } else {
        expectPunct("=")
        const sir = parseTerm()
        const tree = compile(sir)
        globals.set(id.v, tree)
        decls.push({ kind: "Def", name: id.v, tree })
      }
    } else if (t.t === "kw" && t.v === "elab") {
      // elab name = SURFACE_EXPR
      // Parses with the typed surface grammar; runs the elaborator with the
      // current globals as the initial environment. Result: a tagged tree
      // registered as a global like a regular def.
      const id = eat()
      if (id.t !== "id") throw new Error("elab: expected name")
      expectPunct("=")
      const surface = parseTyped()
      const tree = elabSurface(surface, new Map(globals))
      globals.set(id.v, tree)
      decls.push({ kind: "Def", name: id.v, tree })
    } else if (t.t === "kw" && t.v === "test") {
      const lhs = parseTerm()
      expectPunct("=")
      const rhs = parseTerm()
      decls.push({ kind: "Test", lhs: compile(lhs), rhs: compile(rhs) })
    } else {
      throw new Error(`top-level: expected 'def' or 'test', got ${JSON.stringify(t)}`)
    }
  }
  return decls
}
