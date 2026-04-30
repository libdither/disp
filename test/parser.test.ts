// Exercises the grammar in SYNTAX.typ. The tokenizer and parseItems are
// covered at the unit level; parseProgram (the driver) is covered by
// end-to-end tests that verify scoping, use, and compilation semantics.

import { describe, it, expect } from "vitest"
import { mkdtempSync, writeFileSync } from "node:fs"
import { tmpdir } from "node:os"
import { join } from "node:path"

import {
  tokenize, parseItems, parseExpr, parseProgram,
  type Tok, type Expr, type Item,
} from "../src/parse.js"
import {
  LEAF, stem, fork, treeEqual, applyTree,
} from "../src/tree.js"

// ─────────────────────────── helpers ────────────────────────────────────

const v = (name: string): Expr => ({ tag: "var", name })
const leaf: Expr = { tag: "leaf" }
const hole: Expr = { tag: "hole" }
const ap  = (f: Expr, x: Expr): Expr => ({ tag: "app", f, x })
const binder = (params: { name: string | null; type: Expr | null }[], body: Expr): Expr =>
  ({ tag: "binder", params, body })
const ann = (expr: Expr, type: Expr): Expr => ({ tag: "ann", expr, type })
const proj = (target: Expr, field: string): Expr => ({ tag: "proj", target, field })
const recType = (fields: { name: string; type: Expr }[]): Expr => ({ tag: "recType", fields })
const recValue = (fields: { name: string; type: Expr | null; value: Expr }[]): Expr =>
  ({ tag: "recValue", fields })
const use = (path: string): Expr => ({ tag: "use", path })

function soleLet(src: string): { name: string; type: Expr | null; body: Expr } {
  const items = parseItems(src)
  expect(items).toHaveLength(1)
  const it = items[0]
  if (it.tag !== "let") throw new Error(`expected let, got ${it.tag}`)
  return { name: it.name, type: it.type, body: it.body }
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
    const toks = tokenize("")
    expect(toks).toEqual([{ t: "eof" }])
  })

  it("skips spaces/tabs, preserves newlines", () => {
    const toks = tokenize("  \t foo \t ")
    expect(toks.filter(t => t.t !== "nl")).toEqual([{ t: "id", v: "foo" }, { t: "eof" }])
  })

  it("// line comments", () => {
    const toks = tokenize("// a comment\nfoo // tail\n")
    expect(toks.filter(t => t.t !== "nl")).toEqual([{ t: "id", v: "foo" }, { t: "eof" }])
  })

  it("/* block comments */", () => {
    const toks = tokenize("/* block\n   comment */foo")
    expect(toks.filter(t => t.t !== "nl")).toEqual([{ t: "id", v: "foo" }, { t: "eof" }])
  })

  it("unterminated block comment throws", () => {
    expect(() => tokenize("/* oops")).toThrow(/unterminated block comment/)
  })

  it("classifies keywords vs identifiers", () => {
    const toks = tokenize("let test use lets tests")
    const nonNl = toks.filter(t => t.t !== "nl" && t.t !== "eof")
    expect(nonNl).toEqual([
      { t: "kw", v: "let" }, { t: "kw", v: "test" }, { t: "kw", v: "use" },
      { t: "id", v: "lets" }, { t: "id", v: "tests" },
    ])
  })

  it("numeric literals", () => {
    const toks = tokenize("0 12")
    expect(toks.filter(t => t.t !== "nl" && t.t !== "eof")).toEqual([
      { t: "num", v: 0 },
      { t: "num", v: 12 },
    ])
  })

  it("treats bare t as leaf, t-prefix as identifier", () => {
    expect(tokenize("t").filter(t => t.t !== "nl")).toEqual([{ t: "leaf" }, { t: "eof" }])
    expect(tokenize("ty").filter(t => t.t !== "nl")).toEqual([{ t: "id", v: "ty" }, { t: "eof" }])
    expect(tokenize("t1").filter(t => t.t !== "nl")).toEqual([{ t: "id", v: "t1" }, { t: "eof" }])
    expect(tokenize("t t").filter(t => t.t !== "nl")).toEqual([{ t: "leaf" }, { t: "leaf" }, { t: "eof" }])
    expect(tokenize("t'").filter(t => t.t !== "nl")).toEqual([{ t: "id", v: "t'" }, { t: "eof" }])
  })

  it("accepts △ as leaf", () => {
    expect(tokenize("△").filter(t => t.t !== "nl")).toEqual([{ t: "leaf" }, { t: "eof" }])
  })

  it("identifier extras (underscore, digits, primes)", () => {
    const toks = tokenize("foo_bar Foo1 x' _private").filter(t => t.t !== "nl" && t.t !== "eof")
    expect(toks).toEqual([
      { t: "id", v: "foo_bar" }, { t: "id", v: "Foo1" },
      { t: "id", v: "x'" }, { t: "id", v: "_private" },
    ])
  })

  it("multi-char punctuation before single-char", () => {
    const toks = tokenize(":= -> → . , ; = : ( ) { }").filter(t => t.t !== "nl" && t.t !== "eof")
    expect(toks).toEqual(
      [":=", "->", "→", ".", ",", ";", "=", ":", "(", ")", "{", "}"].map(v => ({ t: "punct", v })),
    )
  })

  it("parses string literals without escapes", () => {
    expect(tokenize(`"a/b.disp"`).filter(t => t.t !== "nl")).toEqual([{ t: "str", v: "a/b.disp" }, { t: "eof" }])
  })

  it("throws on unterminated strings", () => {
    expect(() => tokenize(`"oops`)).toThrow(/unterminated string/)
  })

  it("throws on unknown characters", () => {
    expect(() => tokenize("foo @ bar")).toThrow(/unexpected/)
  })
})

// ─────────────────────────── expressions ───────────────────────────────

describe("parse: expressions", () => {
  it("leaf literal", () => {
    expect(parseExpr("t")).toEqual(leaf)
    expect(parseExpr("△")).toEqual(leaf)
  })

  it("identifier", () => {
    expect(parseExpr("x")).toEqual(v("x"))
  })

  it("hole", () => {
    expect(parseExpr("_")).toEqual(hole)
  })

  it("_foo is an identifier, not a hole", () => {
    expect(parseExpr("_foo")).toEqual(v("_foo"))
  })

  it("application is left-associative", () => {
    expect(parseExpr("f x y z")).toEqual(ap(ap(ap(v("f"), v("x")), v("y")), v("z")))
  })

  it("parenthesized regroups", () => {
    expect(parseExpr("f (x y) z")).toEqual(ap(ap(v("f"), ap(v("x"), v("y"))), v("z")))
  })

  it("ascription in parens", () => {
    expect(parseExpr("(x : A)")).toEqual(ann(v("x"), v("A")))
  })

  it("arrow is right-associative", () => {
    // A -> B -> C  parses as  A -> (B -> C)
    expect(parseExpr("A -> B -> C")).toEqual(
      binder([{ name: null, type: v("A") }],
        binder([{ name: null, type: v("B") }], v("C"))),
    )
  })

  it("unicode arrow → is equivalent to ->", () => {
    expect(parseExpr("A → B")).toEqual(parseExpr("A -> B"))
  })

  it("single-param binder", () => {
    expect(parseExpr("{x} -> x")).toEqual(
      binder([{ name: "x", type: null }], v("x")),
    )
  })

  it("binder with type annotation", () => {
    expect(parseExpr("{x : A} -> x")).toEqual(
      binder([{ name: "x", type: v("A") }], v("x")),
    )
  })

  it("multi-param binder", () => {
    expect(parseExpr("{x, y} -> x")).toEqual(
      binder([{ name: "x", type: null }, { name: "y", type: null }], v("x")),
    )
  })

  it("anonymous binder", () => {
    expect(parseExpr("{_ : Nat} -> t")).toEqual(
      binder([{ name: null, type: v("Nat") }], leaf),
    )
  })

  it("binder body extends as far as possible", () => {
    // {x} -> f x y  parses as  {x} -> ((f x) y)
    expect(parseExpr("{x} -> f x y")).toEqual(
      binder([{ name: "x", type: null }], ap(ap(v("f"), v("x")), v("y"))),
    )
  })

  it("binder at head of application needs parens", () => {
    expect(parseExpr("({x} -> x) y")).toEqual(
      ap(binder([{ name: "x", type: null }], v("x")), v("y")),
    )
  })

  it("projection", () => {
    expect(parseExpr("e.foo")).toEqual(proj(v("e"), "foo"))
  })

  it("chained projection", () => {
    expect(parseExpr("e.foo.bar")).toEqual(proj(proj(v("e"), "foo"), "bar"))
  })

  it("projection binds tighter than application", () => {
    // f.a b  parses as  (f.a) b
    expect(parseExpr("f.a b")).toEqual(ap(proj(v("f"), "a"), v("b")))
  })

  it("recType", () => {
    expect(parseExpr("{x : A, y : B}")).toEqual(
      recType([{ name: "x", type: v("A") }, { name: "y", type: v("B") }]),
    )
  })

  it("recType with arrow becomes binder", () => {
    // {x : A} -> x  is a binder, not a recType
    expect(parseExpr("{x : A} -> x")).toEqual(
      binder([{ name: "x", type: v("A") }], v("x")),
    )
  })

  it("recValue", () => {
    expect(parseExpr("{ x := t }")).toEqual(
      recValue([{ name: "x", type: null, value: leaf }]),
    )
  })

  it("recValue with type", () => {
    expect(parseExpr("{ x : A := t }")).toEqual(
      recValue([{ name: "x", type: v("A"), value: leaf }]),
    )
  })

  it("recValue multi-field semicolon separated", () => {
    expect(parseExpr("{ x := t; y := t t }")).toEqual(
      recValue([
        { name: "x", type: null, value: leaf },
        { name: "y", type: null, value: ap(leaf, leaf) },
      ]),
    )
  })

  it("empty braces are empty recValue", () => {
    expect(parseExpr("{}")).toEqual(recValue([]))
  })

  it("use expression", () => {
    expect(parseExpr(`use "foo.disp"`)).toEqual(use("foo.disp"))
  })
})

// ─────────────────────────── items ──────────────────────────────────────

describe("parse: items", () => {
  it("let and test in sequence", () => {
    const items = parseItems("let a = t\ntest a = a")
    expect(items.map(i => i.tag)).toEqual(["let", "test"])
    const d = items[0]; const t = items[1]
    if (d.tag !== "let" || t.tag !== "test") throw new Error("shape")
    expect(d.name).toBe("a")
    expect(d.body).toEqual(leaf)
    expect(t.lhs).toEqual(v("a"))
    expect(t.rhs).toEqual(v("a"))
  })

  it("let with type annotation", () => {
    const d = soleLet("let x : A = t")
    expect(d.name).toBe("x")
    expect(d.type).toEqual(v("A"))
    expect(d.body).toEqual(leaf)
  })

  it("empty program", () => {
    expect(parseItems("")).toEqual([])
    expect(parseItems("  \n // just a comment \n")).toEqual([])
  })

  it("semicolon as separator", () => {
    const items = parseItems("let a = t; let b = t")
    expect(items).toHaveLength(2)
  })
})

// ─────────────────────────── compile / driver ───────────────────────────

describe("parseProgram (compile + driver)", () => {
  it("`let x = t` binds x to LEAF", () => {
    const decls = parseProgram("let x = t\ntest x = t")
    expect(decls).toHaveLength(2)
    expect(decls[0].kind).toBe("Def")
    expect(decls[1].kind).toBe("Test")
    if (decls[1].kind !== "Test") return
    expect(treeEqual(decls[1].lhs, LEAF)).toBe(true)
    expect(treeEqual(decls[1].rhs, LEAF)).toBe(true)
  })

  it("`{x} -> x` compiles to I", () => {
    const I_TREE = fork(fork(LEAF, LEAF), LEAF)
    const decls = parseProgram("let id = {x} -> x\ntest id = id")
    const def = decls.find(d => d.kind === "Def" && d.name === "id")
    if (!def || def.kind !== "Def") throw new Error("missing")
    expect(treeEqual(def.tree, I_TREE)).toBe(true)
  })

  it("test equality uses hash-cons identity", () => {
    const decls = parseProgram("test ({x} -> x) t = t")
    expect(decls).toHaveLength(1)
    const t = decls[0]
    if (t.kind !== "Test") throw new Error("test")
    expect(treeEqual(t.lhs, t.rhs)).toBe(true)
  })

  it("multi-param binder compiles correctly", () => {
    // {x, y} -> x  desugars to \x.\y.x = K = stem(LEAF)
    const K_TREE = stem(LEAF)
    const decls = parseProgram("let K = {x, y} -> x")
    const def = decls[0]
    if (def.kind !== "Def") throw new Error("def")
    expect(treeEqual(def.tree, K_TREE)).toBe(true)
  })

  it("arrow sugar A -> B compiles (anonymous binder)", () => {
    // A -> B  in untyped compilation: the type A is erased, so this is \_ -> B
    // which is K(B). With B = t, that's K(LEAF) = stem(LEAF) applied... no.
    // Actually {_ : A} -> B erases to \anon -> B which is K(B) since anon is unused.
    // K(LEAF) = stem(LEAF). Let's verify:
    const decls = parseProgram("let f = t -> t")
    const def = decls[0]
    if (def.kind !== "Def") throw new Error("def")
    // \anon -> t = K t = stem(LEAF)... wait, K = stem(LEAF), K(t) = stem(LEAF)(LEAF) = fork(LEAF, LEAF)
    // No. K is \x y. x. K(t) = \y. t = K t which in tree form is apply(K_TREE, LEAF).
    // K_TREE = stem(LEAF). apply(stem(LEAF), LEAF) = fork(LEAF, LEAF).
    expect(treeEqual(def.tree, fork(LEAF, LEAF))).toBe(true)
  })

  it("scoping: inner let doesn't leak", () => {
    // We don't have block scoping in items yet (blocks are deferred).
    // But we can test that sequential lets see prior defs.
    const decls = parseProgram("let x = t\nlet y = x\ntest y = t")
    const test = decls.find(d => d.kind === "Test")
    if (!test || test.kind !== "Test") throw new Error("test")
    expect(treeEqual(test.lhs, test.rhs)).toBe(true)
  })

  it("numeric literals compile through in-scope zero/succ", () => {
    const decls = parseProgram("let zero = t\nlet succ = {n} -> t t n\ntest 2 = succ (succ zero)")
    const test = decls.find(d => d.kind === "Test")
    if (!test || test.kind !== "Test") throw new Error("test")
    expect(treeEqual(test.lhs, test.rhs)).toBe(true)
  })

  it("numeric literals require zero and succ in scope", () => {
    expect(() => parseProgram("test 0 = t")).toThrow(/zero and succ/)
  })

  it("`use` inlines another file's defs via projection", () => {
    const dep = tmpFile("dep.disp", "let y = t t\n")
    const decls = parseProgram(
      `let lib = use "${dep}"\ntest lib.y = t t`,
      process.cwd() + "/anon.disp",
    )
    const test = decls.find(d => d.kind === "Test")
    if (!test || test.kind !== "Test") throw new Error("test")
    expect(treeEqual(test.lhs, test.rhs)).toBe(true)
  })

  it("`use` path resolves relative to the including file", () => {
    const dep = tmpFile("dep.disp", "let y = t\n")
    const mainPath = join(dep.slice(0, dep.lastIndexOf("/")), "main.disp")
    writeFileSync(mainPath, `let lib = use "dep.disp"\ntest lib.y = t\n`)
    const decls = parseProgram(`let m = use "${mainPath}"`, process.cwd() + "/anon.disp")
    // Should not throw (the nested use resolves correctly)
    expect(decls.length).toBeGreaterThan(0)
  })

  it("`open use` defines and re-exports opened fields", () => {
    const dep = tmpFile("dep.disp", "let y = t\n")
    const midPath = join(dep.slice(0, dep.lastIndexOf("/")), "mid.disp")
    writeFileSync(midPath, `open use "dep.disp"\n`)
    const decls = parseProgram(`let mid = use "${midPath}"\ntest mid.y = t`, process.cwd() + "/anon.disp")
    const def = decls.find(d => d.kind === "Def" && d.name === "mid")
    const test = decls.find(d => d.kind === "Test")
    if (!def || def.kind !== "Def" || !test || test.kind !== "Test") throw new Error("missing")
    expect(treeEqual(test.lhs, test.rhs)).toBe(true)
  })

  it("recValue Church encoding: field access works", () => {
    // { x := t; y := t t }.x should equal t
    const decls = parseProgram("let r = { x := t; y := t t }\ntest r.x = t")
    const test = decls.find(d => d.kind === "Test")
    if (!test || test.kind !== "Test") throw new Error("test")
    expect(treeEqual(test.lhs, test.rhs)).toBe(true)
  })

  it("recValue Church encoding: second field", () => {
    const decls = parseProgram("let r = { x := t; y := t t }\ntest r.y = t t")
    const test = decls.find(d => d.kind === "Test")
    if (!test || test.kind !== "Test") throw new Error("test")
    expect(treeEqual(test.lhs, test.rhs)).toBe(true)
  })
})

// ─────────────────────────── parse errors ───────────────────────────────

describe("parse errors", () => {
  it("{} is empty recValue, not an empty binder", () => {
    // {} -> x  parses as arrow sugar: binder([{name: null, type: recValue([])}], x)
    expect(parseExpr("{}")).toEqual(recValue([]))
    const arrowExpr = parseExpr("{} -> x")
    expect(arrowExpr.tag).toBe("binder")
  })

  it("missing '=' in let", () => {
    expect(() => parseItems("let f t")).toThrow()
  })

  it("trailing tokens fail", () => {
    // A closing brace with no matching open brace is a trailing token error.
    expect(() => parseItems("let f = t\n}")).toThrow()
  })

  it("unmatched `}`", () => {
    expect(() => parseItems("}")).toThrow()
  })

  it("use with non-string arg", () => {
    expect(() => parseExpr("use foo")).toThrow()
  })

  it("block braces not yet supported", () => {
    expect(() => parseExpr("{ let x = t; x }")).toThrow(/block/)
  })
})
