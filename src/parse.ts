// Surface syntax → tree calculus, via bracket abstraction.
//
// Surface:
//   def NAME = EXPR
//   test EXPR = EXPR
//   use "path"                 load another .disp file into current scope
//   { DECLS... }                scoped block — defs/uses visible only inside
//   ; line comment
//
// Term grammar:
//   △ | t            tree leaf (LEAF)
//   x                identifier (variable or top-level def name)
//   \x y z. body     lambda (desugars to nested \x. \y. \z. body)
//   f x              application (left-associative juxtaposition)
//   ( e )            grouping
//
// Module system (Rust-flavoured, minimal):
//   - `use "relative/path.disp"` parses the given file with the CURRENT scope
//     stack, so its defs appear as locally-bound names. Relative to the
//     including file's directory.
//   - `{ ... }` introduces a nested scope. Any `def`/`use` inside is only
//     visible within the braces. Tests inside the braces still fire (their
//     compiled trees are closed, so they survive scope exit).
//   - Lookups walk the scope stack inner-to-outer. Inner definitions shadow.
//   - No explicit namespacing (`mod`/`as`). Imported names dump into the
//     current scope. Pick different names if you need to distinguish
//     implementations in the same scope.
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

import { readFileSync } from "node:fs"
import { dirname, resolve as pathResolve } from "node:path"
import { Tree, LEAF, stem, fork, treeApply, applyTree, treeEqual, isLeaf, isStem, isFork, prettyTree, FAST_EQ } from "./tree.js"
import { elab as elabSurface, type Surface, type Env as ElabEnv, freshMarker, mkH, substituteMetas, decodeMetaSolutions } from "./elaborate.js"

// ===== Tokens =====
type Tok =
  | { t: "id"; v: string }
  | { t: "punct"; v: string }
  | { t: "kw"; v: string }
  | { t: "leaf" }
  | { t: "str"; v: string }
  | { t: "eof" }

const KEYWORDS = new Set(["def", "test", "elab", "raw", "use"])
const PUNCT = ["\\", ".", "(", ")", "=", ":", "->", "→", "{", "}"] as const

export function tokenize(src: string): Tok[] {
  const toks: Tok[] = []
  let i = 0
  while (i < src.length) {
    const c = src[i]
    if (/\s/.test(c)) { i++; continue }
    if (c === ";") { while (i < src.length && src[i] !== "\n") i++; continue }
    if (c === '"') {
      // Double-quoted string literal. No escapes for now (paths don't need them).
      let j = i + 1
      while (j < src.length && src[j] !== '"') j++
      if (j >= src.length) throw new Error(`tokenize: unterminated string starting at offset ${i}`)
      toks.push({ t: "str", v: src.slice(i + 1, j) })
      i = j + 1
      continue
    }
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

export function parseProgram(src: string, sourcePath?: string): Decl[] {
  // Scope stack. Frame 0 is the built-in/global frame (primitives like fast_eq).
  // New frames are pushed on `{` and popped on `}`. `use` parses into the
  // current top frame, so imports go to whoever's running the use directive.
  const scopeStack: Map<string, Tree>[] = [new Map()]
  scopeStack[0].set("fast_eq", FAST_EQ)
  const decls: Decl[] = []
  // Track the current file path so `use "rel/path"` resolves relative to
  // the including file's directory, not the process CWD.
  const pathStack: string[] = [sourcePath ? pathResolve(sourcePath) : pathResolve(process.cwd(), "<anonymous>")]

  function currentDir(): string {
    return dirname(pathStack[pathStack.length - 1])
  }

  function lookupGlobal(name: string): Tree | undefined {
    for (let i = scopeStack.length - 1; i >= 0; i--) {
      const t = scopeStack[i].get(name)
      if (t) return t
    }
    return undefined
  }

  function defineGlobal(name: string, tree: Tree): void {
    scopeStack[scopeStack.length - 1].set(name, tree)
  }

  // Flat view of all scopes (inner-shadowing-outer) for passing to helpers
  // that take a Map (like the surface elaborator).
  function flatGlobals(): Map<string, Tree> {
    const m = new Map<string, Tree>()
    for (const frame of scopeStack) {
      for (const [k, v] of frame.entries()) m.set(k, v)
    }
    return m
  }

  // All parsing logic lives inside `parseSource` so it can recurse
  // for `use "path"`: each invocation has its own token stream + position,
  // but they share scopeStack, decls, and pathStack via closure.
  function parseSource(src: string): void {
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
    if (t.t === "kw" && t.v === "raw") {
      // raw (EXPR) — escape into the untyped grammar. EXPR is parsed,
      // bracket-abstracted, and reduced to a runtime tree; that tree is
      // embedded as a literal in the surrounding typed term. Lets typed
      // defs construct atoms-of-Type and other tagged-form values that
      // require runtime computation (e.g. `mkH Type marker`) without a
      // separate plain-`def` indirection.
      eat()
      expectPunct("(")
      const sir = parseTerm()
      expectPunct(")")
      return { tag: "raw", tree: compile(sir) }
    }
    if (t.t === "punct" && t.v === "(") {
      // Could be (e), (x : T) -> R, or (x : T) annotation following \
      // For atoms, just (e). Pi syntax is parsed in parseTyped at the term level.
      eat()
      const e = parseTyped()
      expectPunct(")")
      return e
    }
    if (t.t === "leaf") { eat(); return { tag: "leaf" } }
    if (t.t === "id") {
      eat()
      // `_` is the surface hole marker — desugars to a fresh meta token at
      // elab time. Distinct from `_foo` (a regular identifier) — only the
      // bare single underscore triggers the hole path.
      if (t.v === "_") return { tag: "hole" }
      return { tag: "var", name: t.v }
    }
    throw new Error(`parseTypedAtom: unexpected ${JSON.stringify(t)}`)
  }
  const isTypedAtomStart = (t: Tok) =>
    t.t === "id" || t.t === "leaf" || (t.t === "punct" && t.v === "(") || (t.t === "kw" && t.v === "raw")

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

  // Step 1: SIR → CIR with globals resolved to lit nodes.
  // Uses the outer `lookupGlobal` so scoped `use`d defs resolve correctly.
  function sir_to_cir(e: SIR): CIR {
    switch (e.tag) {
      case "leaf": return { tag: "lit", t: LEAF }
      case "var": {
        const t = lookupGlobal(e.name)
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

    // ===== Top-level / block body =====
    // terminator: "eof" at the file top level, "}" when parsing a block body.
    function parseDeclsUntil(terminator: "eof" | "}"): void {
      while (true) {
        const t = peek()
        if (terminator === "eof" && t.t === "eof") return
        if (terminator === "}" && t.t === "punct" && t.v === "}") return

        // Scoped block: push a fresh frame, parse until matching '}', pop.
        if (t.t === "punct" && t.v === "{") {
          eat()
          scopeStack.push(new Map())
          try {
            parseDeclsUntil("}")
            expectPunct("}")
          } finally {
            scopeStack.pop()
          }
          continue
        }

        // File-include: `use "relative/path.disp"` loads the file and
        // parses it with the current scope stack, so its defs land in the
        // current (possibly block-scoped) frame. Tests in the loaded file
        // are appended to the top-level decls list and fire normally.
        if (t.t === "kw" && t.v === "use") {
          eat()
          const pathTok = eat()
          if (pathTok.t !== "str") {
            throw new Error(`use: expected quoted path, got ${JSON.stringify(pathTok)}`)
          }
          const resolvedPath = pathResolve(currentDir(), pathTok.v)
          const loadedSrc = readFileSync(resolvedPath, "utf-8")
          pathStack.push(resolvedPath)
          try {
            parseSource(loadedSrc)
          } finally {
            pathStack.pop()
          }
          continue
        }

        eat()
        if (t.t === "kw" && t.v === "def") {
          const id = eat()
          if (id.t !== "id") throw new Error("def: expected name")
          // `def NAME : T = EXPR` — typed form. Elaborates both sides, then
          // runs the user-defined `pred_of_lvl` (must be in scope). Stores
          // the elaborated tagged tree if check returns TT; throws otherwise.
          if (peek().t === "punct" && (peek() as { v: string }).v === ":") {
            eat()  // consume :
            const typeS = parseTyped()
            expectPunct("=")
            const exprS = parseTyped()
            const env = flatGlobals()
            const typeT = elabSurface(typeS, env)
            const exprT = elabSurface(exprS, env)
            const predOfLvl = lookupGlobal("pred_of_lvl")
            const stateInit = lookupGlobal("state_init")
            const ctxNone = lookupGlobal("ctx_none")
            if (!predOfLvl || !stateInit || !ctxNone) {
              throw new Error(`def ${id.v}: 'pred_of_lvl', 'ctx_none', and 'state_init' must be defined before typed defs`)
            }
            // pred_of_lvl signature: ty tyCtx cand candCtx state → (bool, state')
            const resTuple = applyTree(
              applyTree(applyTree(applyTree(applyTree(predOfLvl, typeT), ctxNone), exprT), ctxNone),
              stateInit,
              10_000_000,
            )
            if (!isFork(resTuple)) {
              throw new Error(`def ${id.v}: pred_of_lvl returned non-tuple ${prettyTree(resTuple)}`)
            }
            const okBit = resTuple.left
            const finalState = resTuple.right
            if (!treeEqual(okBit, LEAF)) {
              throw new Error(`def ${id.v}: type check failed (got ${prettyTree(okBit)})`)
            }
            const metasList = isFork(finalState) ? finalState.right : LEAF
            const solutions = decodeMetaSolutions(metasList)
            const finalTree = solutions.size > 0 ? substituteMetas(exprT, solutions) : exprT
            defineGlobal(id.v, finalTree)
            decls.push({ kind: "Def", name: id.v, tree: finalTree })
          } else {
            expectPunct("=")
            const sir = parseTerm()
            const tree = compile(sir)
            defineGlobal(id.v, tree)
            decls.push({ kind: "Def", name: id.v, tree })
          }
        } else if (t.t === "kw" && t.v === "elab") {
          const id = eat()
          if (id.t !== "id") throw new Error("elab: expected name")
          expectPunct("=")
          const surface = parseTyped()
          const tree = elabSurface(surface, flatGlobals())
          defineGlobal(id.v, tree)
          decls.push({ kind: "Def", name: id.v, tree })
        } else if (t.t === "kw" && t.v === "test") {
          const lhs = parseTerm()
          expectPunct("=")
          const rhs = parseTerm()
          decls.push({ kind: "Test", lhs: compile(lhs), rhs: compile(rhs) })
        } else {
          throw new Error(`top-level: expected 'def', 'test', 'elab', 'use', or '{', got ${JSON.stringify(t)}`)
        }
      }
    }

    parseDeclsUntil("eof")
  }

  parseSource(src)
  return decls
}
