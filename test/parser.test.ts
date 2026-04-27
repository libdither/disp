// Exercises the grammar in SYNTAX.typ. The tokenizer and `parseItems` are
// covered at the unit level; `parseProgram` (the driver) is covered by a
// smaller set of end-to-end tests that verify scoping, `use`, and
// compilation semantics.

import { describe, it, expect } from "vitest"
import { mkdtempSync, writeFileSync } from "node:fs"
import { tmpdir } from "node:os"
import { join } from "node:path"

import {
  tokenize, parseItems, parseProgram,
  type Tok, type Sir, type Item,
} from "../src/parse.js"
import {
  LEAF, stem, fork, treeEqual,
} from "../src/tree.js"

// ─────────────────────────── helpers ────────────────────────────────────

const v = (name: string): Sir => ({ tag: "var", name })
const leaf: Sir = { tag: "leaf" }
const ap  = (f: Sir, x: Sir): Sir => ({ tag: "app", f, x })
const lam = (x: string, body: Sir): Sir => ({ tag: "lam", x, body })

function soleDef(src: string): { name: string; expr: Sir } {
  const items = parseItems(src)
  expect(items).toHaveLength(1)
  const it = items[0]
  if (it.tag !== "def") throw new Error(`expected def, got ${it.tag}`)
  return { name: it.name, expr: it.expr }
}

function tmpFile(name: string, contents: string): string {
  const dir = mkdtempSync(join(tmpdir(), "disp-parse-"))
  const path = join(dir, name)
  writeFileSync(path, contents)
  return path
}

// ─────────────────────────── tokenizer ──────────────────────────────────

describe("tokenize", () => {
  it("emits eof sentinel", () => {
    expect(tokenize("")).toEqual([{ t: "eof" }])
  })

  it("skips whitespace and line comments", () => {
    const toks = tokenize("  \n; a comment \n\t foo ; tail\n")
    expect(toks).toEqual([{ t: "id", v: "foo" }, { t: "eof" }])
  })

  it("classifies keywords vs identifiers", () => {
    const toks = tokenize("def test elab raw use defs tests")
    expect(toks.slice(0, -1)).toEqual([
      { t: "kw", v: "def" }, { t: "kw", v: "test" }, { t: "kw", v: "elab" },
      { t: "kw", v: "raw" }, { t: "kw", v: "use" },
      { t: "id", v: "defs" }, { t: "id", v: "tests" },
    ])
  })

  it("treats bare t as leaf, t-prefix as identifier", () => {
    expect(tokenize("t")).toEqual([{ t: "leaf" }, { t: "eof" }])
    expect(tokenize("ty")).toEqual([{ t: "id", v: "ty" }, { t: "eof" }])
    expect(tokenize("t1")).toEqual([{ t: "id", v: "t1" }, { t: "eof" }])
    expect(tokenize("t t")).toEqual([{ t: "leaf" }, { t: "leaf" }, { t: "eof" }])
    expect(tokenize("t'")).toEqual([{ t: "id", v: "t'" }, { t: "eof" }])
  })

  it("accepts △ as leaf", () => {
    expect(tokenize("△")).toEqual([{ t: "leaf" }, { t: "eof" }])
  })

  it("identifier extras (underscore, digits, primes)", () => {
    expect(tokenize("foo_bar Foo1 x' _private")).toEqual([
      { t: "id", v: "foo_bar" }, { t: "id", v: "Foo1" },
      { t: "id", v: "x'" }, { t: "id", v: "_private" }, { t: "eof" },
    ])
  })

  it("multi-char punctuation before single-char", () => {
    const toks = tokenize("-> → . \\ = : ( ) { }")
    expect(toks.slice(0, -1)).toEqual(
      ["->", "→", ".", "\\", "=", ":", "(", ")", "{", "}"].map(v => ({ t: "punct", v })),
    )
  })

  it("parses string literals without escapes", () => {
    expect(tokenize(`"a/b.disp"`)).toEqual([{ t: "str", v: "a/b.disp" }, { t: "eof" }])
  })

  it("throws on unterminated strings", () => {
    expect(() => tokenize(`"oops`)).toThrow(/unterminated string/)
  })

  it("throws on unknown characters", () => {
    expect(() => tokenize("foo @ bar")).toThrow(/unexpected/)
  })
})

// ─────────────────────────── untyped terms ──────────────────────────────

describe("parse: untyped term grammar", () => {
  it("leaf literal", () => {
    expect(soleDef("def f = t").expr).toEqual(leaf)
    expect(soleDef("def f = △").expr).toEqual(leaf)
  })

  it("identifier", () => {
    expect(soleDef("def f = x").expr).toEqual(v("x"))
  })

  it("application is left-associative", () => {
    // `f x y z` = ((f x) y) z
    const e = soleDef("def f = f x y z").expr
    expect(e).toEqual(ap(ap(ap(v("f"), v("x")), v("y")), v("z")))
  })

  it("parenthesized regroups", () => {
    const e = soleDef("def f = f (x y) z").expr
    expect(e).toEqual(ap(ap(v("f"), ap(v("x"), v("y"))), v("z")))
  })

  it("single-binder lambda", () => {
    expect(soleDef("def f = \\x. x").expr).toEqual(lam("x", v("x")))
  })

  it("multi-binder lambda desugars right-associatively", () => {
    // \x y z. b == \x. \y. \z. b
    const e = soleDef("def f = \\x y z. x").expr
    expect(e).toEqual(lam("x", lam("y", lam("z", v("x")))))
  })

  it("lambda body extends as far as possible", () => {
    // \x. f x y  parses as  \x. ((f x) y), not as (\x. f x) y
    const e = soleDef("def f = \\x. f x y").expr
    expect(e).toEqual(lam("x", ap(ap(v("f"), v("x")), v("y"))))
  })

  it("lambda at head of application needs parens", () => {
    // (\x. x) y  → app(lam(x,x), y)
    const e = soleDef("def f = (\\x. x) y").expr
    expect(e).toEqual(ap(lam("x", v("x")), v("y")))
  })
})

// ─────────────────────────── items ──────────────────────────────────────

describe("parse: items", () => {
  it("def, test in sequence", () => {
    const items = parseItems("def a = t\ntest a = a")
    expect(items.map(i => i.tag)).toEqual(["def", "test"])
    const d = items[0]; const t = items[1]
    if (d.tag !== "def" || t.tag !== "test") throw new Error("shape")
    expect(d.name).toBe("a")
    expect(t.lhs).toEqual(v("a"))
    expect(t.rhs).toEqual(v("a"))
  })

  it("use", () => {
    const items = parseItems(`use "./other.disp"`)
    expect(items).toEqual([{ tag: "use", path: "./other.disp" }])
  })

  it("empty program", () => {
    expect(parseItems("")).toEqual([])
    expect(parseItems("  \n ; just a comment \n")).toEqual([])
  })

  it("block wraps inner items", () => {
    const items = parseItems(`{ def a = t\n def b = a }`)
    expect(items).toHaveLength(1)
    const it = items[0]
    expect(it.tag).toBe("block")
    if (it.tag !== "block") return
    expect(it.items.map(i => i.tag)).toEqual(["def", "def"])
  })

  it("nested blocks", () => {
    const items = parseItems(`{ { def a = t } }`)
    expect(items).toHaveLength(1)
    const outer = items[0]
    if (outer.tag !== "block") throw new Error("outer block")
    expect(outer.items).toHaveLength(1)
    const inner = outer.items[0]
    if (inner.tag !== "block") throw new Error("inner block")
    expect(inner.items.map(i => i.tag)).toEqual(["def"])
  })

  it("elab declaration", () => {
    const items = parseItems("elab F = t")
    expect(items).toHaveLength(1)
    const it = items[0]
    expect(it.tag).toBe("elab")
  })
})

// ─────────────────────────── typed terms ────────────────────────────────

describe("parse: typed term grammar (via elab/deftyped)", () => {
  it("typed identifier", () => {
    const items = parseItems("elab F = Type")
    const it = items[0]
    if (it.tag !== "elab") throw new Error("elab")
    expect(it.expr).toEqual({ tag: "var", name: "Type" })
  })

  it("arrow is right-associative", () => {
    const items = parseItems("elab F = A -> B -> C")
    const it = items[0]
    if (it.tag !== "elab") throw new Error("elab")
    expect(it.expr).toEqual({
      tag: "arrow",
      dom: { tag: "var", name: "A" },
      cod: {
        tag: "arrow",
        dom: { tag: "var", name: "B" },
        cod: { tag: "var", name: "C" },
      },
    })
  })

  it("unicode arrow `→` is equivalent to `->`", () => {
    const a = parseItems("elab F = A -> B")[0]
    const b = parseItems("elab F = A → B")[0]
    if (a.tag !== "elab" || b.tag !== "elab") throw new Error("elab")
    expect(a.expr).toEqual(b.expr)
  })

  it("dependent Pi", () => {
    const items = parseItems("elab F = (x : A) -> B")
    const it = items[0]
    if (it.tag !== "elab") throw new Error("elab")
    expect(it.expr).toEqual({
      tag: "pi",
      x: "x",
      dom: { tag: "var", name: "A" },
      cod: { tag: "var", name: "B" },
    })
  })

  it("annotated lambda", () => {
    const items = parseItems("elab F = \\(x : A). x")
    const it = items[0]
    if (it.tag !== "elab") throw new Error("elab")
    expect(it.expr).toEqual({
      tag: "alam",
      x: "x",
      type: { tag: "var", name: "A" },
      body: { tag: "var", name: "x" },
    })
  })

  it("hole marker `_`", () => {
    const items = parseItems("elab F = _")
    const it = items[0]
    if (it.tag !== "elab") throw new Error("elab")
    expect(it.expr).toEqual({ tag: "hole" })
  })

  it("`_foo` is an identifier, not a hole", () => {
    const items = parseItems("elab F = _foo")
    const it = items[0]
    if (it.tag !== "elab") throw new Error("elab")
    expect(it.expr).toEqual({ tag: "var", name: "_foo" })
  })

  it("raw escape embeds a compiled tree", () => {
    const items = parseItems("elab F = raw (t)")
    const it = items[0]
    if (it.tag !== "elab") throw new Error("elab")
    expect(it.expr.tag).toBe("raw")
    if (it.expr.tag !== "raw") return
    expect(treeEqual(it.expr.tree, LEAF)).toBe(true)
  })

  it("typed def has both sides parsed as typed", () => {
    const items = parseItems("def F : A -> B = \\(x : A). y")
    expect(items).toHaveLength(1)
    const it = items[0]
    expect(it.tag).toBe("deftyped")
    if (it.tag !== "deftyped") return
    expect(it.type.tag).toBe("arrow")
    expect(it.expr.tag).toBe("alam")
  })

  it("parenthesized typed sub-term distinct from Pi", () => {
    // (A -> B) -> C  — left paren groups but doesn't introduce a Pi.
    const items = parseItems("elab F = (A -> B) -> C")
    const it = items[0]
    if (it.tag !== "elab") throw new Error("elab")
    expect(it.expr).toEqual({
      tag: "arrow",
      dom: {
        tag: "arrow",
        dom: { tag: "var", name: "A" },
        cod: { tag: "var", name: "B" },
      },
      cod: { tag: "var", name: "C" },
    })
  })
})

// ─────────────────────────── compile / driver ───────────────────────────

describe("parseProgram (compile + driver)", () => {
  it("`def x = t` binds x to LEAF", () => {
    const decls = parseProgram("def x = t\ntest x = t")
    expect(decls).toHaveLength(2)
    expect(decls[0].kind).toBe("Def")
    expect(decls[1].kind).toBe("Test")
    if (decls[1].kind !== "Test") return
    expect(treeEqual(decls[1].lhs, LEAF)).toBe(true)
    expect(treeEqual(decls[1].rhs, LEAF)).toBe(true)
  })

  it("`\\x. x` compiles to I", () => {
    const decls = parseProgram("def id = \\x. x\ntest id = id")
    const def = decls.find(d => d.kind === "Def" && d.name === "id")
    if (!def || def.kind !== "Def") throw new Error("missing")
    expect(treeEqual(def.tree, fork(fork(LEAF, LEAF), LEAF))).toBe(true)
  })

  it("test equality uses hash-cons identity", () => {
    // Two distinct compilations of \x. x should both reduce to I_TREE.
    const decls = parseProgram("test (\\x. x) t = t")
    expect(decls).toHaveLength(1)
    const t = decls[0]
    if (t.kind !== "Test") throw new Error("test")
    expect(treeEqual(t.lhs, t.rhs)).toBe(true)
  })

  it("block scoping: inner defs do not leak", () => {
    const src = `
      def outer = t
      { def inner = t t }
      test outer = outer
    `
    const decls = parseProgram(src)
    // outer + inner are both captured as Def decls (the compiled tree is
    // produced regardless of scope — scope only governs name resolution).
    const names = decls.filter(d => d.kind === "Def").map(d => (d as any).name)
    expect(names).toContain("outer")
    expect(names).toContain("inner")
    // The outer `test` must reference outer, not inner; it passes.
    const test = decls.find(d => d.kind === "Test")
    if (!test || test.kind !== "Test") throw new Error("test")
    expect(treeEqual(test.lhs, test.rhs)).toBe(true)
  })

  it("block scoping: references to an inner name from outside fail", () => {
    // `inner` is defined inside the block but referenced in a test outside.
    // After block exit, the outer scope doesn't see `inner`, so the var
    // survives into compilation and throws at cirToTree.
    const src = `
      { def inner = t }
      test inner = t
    `
    expect(() => parseProgram(src)).toThrow(/free variable inner/)
  })

  it("block scoping: shadowing — inner def shadows outer", () => {
    const src = `
      def x = t
      { def x = t t
        test x = t t }
      test x = t
    `
    const decls = parseProgram(src)
    const tests = decls.filter(d => d.kind === "Test")
    expect(tests).toHaveLength(2)
    for (const t of tests) {
      if (t.kind !== "Test") continue
      expect(treeEqual(t.lhs, t.rhs)).toBe(true)
    }
  })

  it("`use` inlines another file's defs", () => {
    const dep = tmpFile("dep.disp", "def y = t t\n")
    const main = tmpFile("main.disp", `use "${dep}"\ntest y = t t\n`)
    const decls = parseProgram(`use "${main}"`, process.cwd() + "/anon.disp")
    const t = decls.find(d => d.kind === "Test")
    if (!t || t.kind !== "Test") throw new Error("test")
    expect(treeEqual(t.lhs, t.rhs)).toBe(true)
  })

  it("`use` path resolves relative to the including file", () => {
    const dep = tmpFile("dep.disp", "def y = t\n")
    // main.disp references dep.disp by a bare filename; resolution must
    // find it in main.disp's directory.
    const mainPath = join(dep.slice(0, dep.lastIndexOf("/")), "main.disp")
    writeFileSync(mainPath, `use "dep.disp"\ntest y = t\n`)
    const decls = parseProgram(`use "${mainPath}"`, process.cwd() + "/anon.disp")
    const t = decls.find(d => d.kind === "Test")
    if (!t || t.kind !== "Test") throw new Error("test")
    expect(treeEqual(t.lhs, t.rhs)).toBe(true)
  })
})

// ─────────────────────────── parse errors ───────────────────────────────

describe("parse errors", () => {
  it("unterminated lambda", () => {
    expect(() => parseItems("def f = \\x")).toThrow()
  })

  it("lambda with no binders", () => {
    expect(() => parseItems("def f = \\. x")).toThrow()
  })

  it("missing '=' in def", () => {
    expect(() => parseItems("def f t")).toThrow()
  })

  it("Pi-looking form without `->` is rejected", () => {
    // `(x : A)` by itself is not a valid typed sub-term.
    expect(() => parseItems("elab F = (x : A)")).toThrow()
  })

  it("trailing tokens fail", () => {
    expect(() => parseItems("def f = t garbage garbage = nope }")).toThrow()
  })

  it("unmatched `}`", () => {
    expect(() => parseItems("}")).toThrow()
  })

  it("use with non-string arg", () => {
    expect(() => parseItems("use foo")).toThrow()
  })
})
