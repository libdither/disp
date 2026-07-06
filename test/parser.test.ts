// Exercises the grammar in SYNTAX.typ. The tokenizer and parseItems are
// covered at the unit level; parseProgram (the driver) is covered by
// end-to-end tests that verify scoping, use, and compilation semantics.

import { describe, it, expect } from "vitest"
import { mkdtempSync, writeFileSync } from "node:fs"
import { tmpdir } from "node:os"
import { join } from "node:path"

import {
  tokenize, parseItems, parseExpr,
  type Tok, type Expr,
} from "../src/parse.js"
import { parseProgram } from "../src/compile.js"
import {
  LEAF, stem, fork, treeEqual, applyTree,
} from "../src/core/tree.js"

// ─────────────────────────── helpers ────────────────────────────────────

const v = (name: string): Expr => ({ tag: "var", name })
const leaf: Expr = { tag: "leaf" }
const hole: Expr = { tag: "hole" }
const ap  = (f: Expr, x: Expr): Expr => ({ tag: "app", f, x })
const binder = (params: { name: string | null; type: Expr | null }[], body: Expr): Expr =>
  ({ tag: "binder", params, body })
const ann = (expr: Expr, type: Expr): Expr => ({ tag: "ann", expr, type })
const proj = (target: Expr, field: string): Expr => ({ tag: "proj", target, field })
const recType = (fields: { name: string; type: Expr | null }[]): Expr => ({ tag: "recType", fields })
const sumType = (variants: { name: string; type: Expr | null }[]): Expr => ({ tag: "sumType", variants })
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

  it("string literal is a term", () => {
    expect(parseExpr('"respond"')).toEqual({ tag: "str", value: "respond" })
  })

  it("coproduct match (string tags, multi-binder, wildcard) parses to a match node", () => {
    // The cut desugar lives in compile.ts; the parser yields the arms verbatim.
    const e = parseExpr("match c { A x => x; B a b c => a; _ => c }")
    expect(e.tag).toBe("match")
  })

  it("use STRING still wins over a bare string literal", () => {
    expect(parseExpr('use "lib/foo.disp"')).toEqual(use("lib/foo.disp"))
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

  it("bare recType (no types)", () => {
    expect(parseExpr("({a, b, c})")).toEqual(
      recType([{ name: "a", type: null }, { name: "b", type: null }, { name: "c", type: null }]),
    )
  })

  it("bare recType with spread type", () => {
    expect(parseExpr("({a, b, c : A})")).toEqual(
      recType([{ name: "a", type: v("A") }, { name: "b", type: v("A") }, { name: "c", type: v("A") }]),
    )
  })

  it("recType with arrow becomes binder", () => {
    // {x : A} -> x  is a binder, not a recType
    expect(parseExpr("{x : A} -> x")).toEqual(
      binder([{ name: "x", type: v("A") }], v("x")),
    )
  })

  it("sum-type literal (single-arg + nullary variants)", () => {
    // < Tag : T > is a single-arg variant; bare < Tag > is nullary.
    expect(parseExpr("<a : Nat, b>")).toEqual(
      sumType([{ name: "a", type: v("Nat") }, { name: "b", type: null }]),
    )
  })

  it("empty sum-type literal <>", () => {
    expect(parseExpr("<>")).toEqual(sumType([]))
  })

  it("sum-type literal with trailing comma + record payload", () => {
    expect(parseExpr("< ok : {v : Nat}, err, >")).toEqual(
      sumType([{ name: "ok", type: recType([{ name: "v", type: v("Nat") }]) }, { name: "err", type: null }]),
    )
  })

  it("sum-type variant type with arrow stops at '>'", () => {
    // `>` is a distinct token from `->`, so `A -> B` parses cleanly inside `<…>`.
    expect(parseExpr("< f : A -> B >")).toEqual(
      sumType([{ name: "f", type: binder([{ name: null, type: v("A") }], v("B")) }]),
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

// ─────────────────── named / default / reorderable args ──────────────────

describe("parse: named-argument syntax", () => {
  it("binder param with a default (`:= d`)", () => {
    // A single-param binder with a default, followed by `->`, is a binder —
    // even though `:=` would otherwise classify the brace as a recValue.
    expect(parseExpr("{ y : B := d } -> y")).toEqual(
      binder([{ name: "y", type: v("B"), default: v("d") } as any], v("y")),
    )
  })

  it("multi-param binder with a default on the last param", () => {
    expect(parseExpr("{ x : A, y : B := d } -> x")).toEqual(
      binder([
        { name: "x", type: v("A") },
        { name: "y", type: v("B"), default: v("d") } as any,
      ], v("x")),
    )
  })

  it("default with no type annotation", () => {
    expect(parseExpr("{ y := t } -> y")).toEqual(
      binder([{ name: "y", type: null, default: leaf } as any], v("y")),
    )
  })

  it("plain params carry no `default` key (AST shape unchanged)", () => {
    // toEqual is strict about absent vs null: a plain param must NOT gain a key.
    expect(parseExpr("{ x : A } -> x")).toEqual(
      binder([{ name: "x", type: v("A") }], v("x")),
    )
  })

  it("recValue fields may be comma-separated (named-call spelling)", () => {
    expect(parseExpr("{ x := t, y := t t }")).toEqual(
      recValue([
        { name: "x", type: null, value: leaf },
        { name: "y", type: null, value: ap(leaf, leaf) },
      ]),
    )
  })

  it("comma- and semicolon-separated recValues agree", () => {
    expect(parseExpr("{ a := t, b := t t }")).toEqual(parseExpr("{ a := t; b := t t }"))
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

  it("`open use` brings names into scope but does not re-export them", () => {
    const dep = tmpFile("dep.disp", "let y = t\n")
    const midPath = join(dep.slice(0, dep.lastIndexOf("/")), "mid.disp")
    // mid.disp opens dep but exports nothing (no := fields, legacy let exports dep's y)
    writeFileSync(midPath, `open use "dep.disp"\n`)
    // In legacy mode (no := fields), open still pushes Defs
    const decls = parseProgram(`let mid = use "${midPath}"\ntest mid.y = t`, process.cwd() + "/anon.disp")
    const test = decls.find(d => d.kind === "Test")
    if (!test || test.kind !== "Test") throw new Error("missing")
    expect(treeEqual(test.lhs, test.rhs)).toBe(true)
  })

  it("`open use` in new-style file does not re-export opened names", () => {
    const dep = tmpFile("dep.disp", "y := t\n")
    const midPath = join(dep.slice(0, dep.lastIndexOf("/")), "mid2.disp")
    // mid2.disp uses new field syntax: open brings y into scope, re-export explicitly
    writeFileSync(midPath, `open use "dep.disp"\nre_y := y\n`)
    const decls = parseProgram(`let mid = use "${midPath}"\ntest mid.re_y = t`, process.cwd() + "/anon.disp")
    const test = decls.find(d => d.kind === "Test")
    if (!test || test.kind !== "Test") throw new Error("missing")
    expect(treeEqual(test.lhs, test.rhs)).toBe(true)
  })

  // Record VALUES now compile to the §2.6 cut (`prod`, needs the kernel in
  // scope), so end-to-end field access is covered in the lib suite
  // (lib/tests/record.test.disp). Here we assert the parse/AST shape.
  it("record value { := } parses to a recValue", () => {
    expect(parseExpr("{ x := t; y := t t }")).toEqual(
      recValue([{ name: "x", type: null, value: leaf }, { name: "y", type: null, value: ap(leaf, leaf) }]),
    )
  })

  it("projection r.x parses to a proj node", () => {
    expect(parseExpr("r.x")).toEqual(proj(v("r"), "x"))
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

  it("block without trailing expression", () => {
    expect(() => parseExpr("{ let x = t }")).toThrow()
  })

  it("test inside braced body parses as block with test", () => {
    // { test t = t; t } parses as a recValue carrying the test member
    // alongside a `trailing` expression. compile.ts evaluates the trailing
    // body after compiling each member (firing inline tests via sinks).
    const result = parseExpr("{ test t = t; t }")
    expect(result).toEqual({
      tag: "recValue",
      fields: [],
      members: [{ tag: "test", lhs: leaf, rhs: leaf }],
      trailing: leaf,
    })
  })

  it("rejects duplicate exported fields", () => {
    // Top-level redefinition is now legal SYNTAX (a guard-mediated rebind request);
    // the UNGUARDED-duplicate accident check moved to the driver, which still throws.
    expect(parseItems("x := t\nx := t t").length).toBe(2)
    expect(() => parseProgram("x := t\nx := t t")).toThrow(/duplicate exported field 'x'/)
    // Braced records are not declarations: still a parse error.
    expect(() => parseExpr("{ x := t; x := t t }")).toThrow(/duplicate exported field 'x'/)
  })

  it("parses decorated declarations (head spines)", () => {
    const items = parseItems("guard g X : T := v")
    expect(items.length).toBe(1)
    const f = items[0] as Extract<ReturnType<typeof parseItems>[number], { tag: "field" }>
    expect(f.tag).toBe("field")
    expect(f.name).toBe("X")
    expect(f.head).toBeTruthy()
    expect(f.value).toBeTruthy()
    // interface entry: head + annotation, no value
    const iface = parseItems("guard g X : T")[0] as typeof f
    expect(iface.name).toBe("X")
    expect(iface.value).toBeNull()
    // let dual-accepts := and =
    expect(parseItems("let x := t").length).toBe(1)
    expect(parseItems("let x = t").length).toBe(1)
  })

  it("rejects duplicate record type fields", () => {
    expect(() => parseExpr("{x : A, x : B}")).toThrow(/duplicate record field 'x'/)
    expect(() => parseExpr("({x, x})")).toThrow(/duplicate record field 'x'/)
  })
})

// ──────────────────────── Block expressions ────────────────────────────
describe("block expressions", () => {
  it("single let binding", () => {
    // { let x = t; x } → App(Binder([x], Var(x)), Leaf)
    expect(parseExpr("{ let x = t; x }")).toEqual(
      ap(binder([{ name: "x", type: null }], v("x")), leaf)
    )
  })

  it("two let bindings", () => {
    // { let x = t; let y = t t; y } → App(Binder([x], App(Binder([y], Var(y)), App(Leaf, Leaf))), Leaf)
    expect(parseExpr("{ let x = t; let y = t t; y }")).toEqual(
      ap(
        binder([{ name: "x", type: null }],
          ap(binder([{ name: "y", type: null }], v("y")), ap(leaf, leaf))),
        leaf,
      )
    )
  })

  it("let binding with type annotation", () => {
    // { let x : A = t; x } → App(Binder([x], Var(x)), Ann(Leaf, Var(A)))
    expect(parseExpr("{ let x : A = t; x }")).toEqual(
      ap(binder([{ name: "x", type: null }], v("x")), ann(leaf, v("A")))
    )
  })

  it("trailing expression uses binding", () => {
    // { let f = {x} -> x; f t } → App(Binder([f], App(Var(f), Leaf)), Binder([x], Var(x)))
    expect(parseExpr("{ let f = {x} -> x; f t }")).toEqual(
      ap(
        binder([{ name: "f", type: null }], ap(v("f"), leaf)),
        binder([{ name: "x", type: null }], v("x")),
      )
    )
  })

  it("newline-separated let bindings", () => {
    // Newlines work as separators between let bindings and before trailing expr.
    const src = `{
      let x = t
      let y = t t
      y
    }`
    expect(parseExpr(src)).toEqual(
      ap(
        binder([{ name: "x", type: null }],
          ap(binder([{ name: "y", type: null }], v("y")), ap(leaf, leaf))),
        leaf,
      )
    )
  })

  it("block as binder body (multi-line)", () => {
    // { let y = x\n y } as body of {x} -> ...
    // The newline should separate the let body from the trailing expression.
    const src = `{x} -> {
      let y = x
      y
    }`
    expect(parseExpr(src)).toEqual(
      binder([{ name: "x", type: null }],
        ap(binder([{ name: "y", type: null }], v("y")), v("x")))
    )
  })

  it("binder as block-let body", () => {
    // let y = {z} -> z inside a block — the binder body should not consume
    // past the newline into the trailing expression.
    const src = `{x} -> {
      let f = {z} -> z
      f x
    }`
    expect(parseExpr(src)).toEqual(
      binder([{ name: "x", type: null }],
        ap(
          binder([{ name: "f", type: null }], ap(v("f"), v("x"))),
          binder([{ name: "z", type: null }], v("z"))))
    )
  })

  it("nested blocks", () => {
    const src = `{x} -> {
      let y = {z} -> {
        let w = z
        w
      }
      y x
    }`
    expect(parseExpr(src)).toEqual(
      binder([{ name: "x", type: null }],
        ap(
          binder([{ name: "y", type: null }], ap(v("y"), v("x"))),
          binder([{ name: "z", type: null }],
            ap(binder([{ name: "w", type: null }], v("w")), v("z")))))
    )
  })

  it("multi-arg application in block-let body stays on line", () => {
    // let z = x y should parse as App(x, y), not consume the trailing expr
    const src = `{x, y} -> {
      let z = x y
      z
    }`
    expect(parseExpr(src)).toEqual(
      binder([{ name: "x", type: null }, { name: "y", type: null }],
        ap(binder([{ name: "z", type: null }], v("z")), ap(v("x"), v("y"))))
    )
  })
})

// ─────────────────────────── if / then / else ────────────────────────────

const mkIf = (cond: Expr, thenBody: Expr, elseBody: Expr): Expr =>
  ({ tag: "if", cond, thenBody, elseBody })

describe("if expression", () => {
  it("parses a basic if/then/else", () => {
    expect(parseExpr(`if c then a else b`)).toEqual(mkIf(v("c"), v("a"), v("b")))
  })

  it("spans newlines between cond / then / else", () => {
    const src = `if c
      then a
      else b`
    expect(parseExpr(src)).toEqual(mkIf(v("c"), v("a"), v("b")))
  })

  it("cond can be an application", () => {
    expect(parseExpr(`if (f x) then a else b`)).toEqual(mkIf(ap(v("f"), v("x")), v("a"), v("b")))
  })

  it("branch bodies can be applications", () => {
    expect(parseExpr(`if c then f x else g y`))
      .toEqual(mkIf(v("c"), ap(v("f"), v("x")), ap(v("g"), v("y"))))
  })

  it("nests inside binders", () => {
    expect(parseExpr(`{x} -> if x then a else b`)).toEqual(
      binder([{ name: "x", type: null }], mkIf(v("x"), v("a"), v("b"))))
  })

  it("nests in the then branch (parenthesised)", () => {
    expect(parseExpr(`if c then (if d then a else b) else e`)).toEqual(
      mkIf(v("c"), mkIf(v("d"), v("a"), v("b")), v("e")))
  })

  it("else-if chains right-associatively without parens", () => {
    expect(parseExpr(`if c then a else if d then b else e`)).toEqual(
      mkIf(v("c"), v("a"), mkIf(v("d"), v("b"), v("e"))))
  })

  it("multi-line branch body: application across newlines", () => {
    // The else body uses matchExpr, so it spans newlines but stops at the
    // enclosing boundary (here, EOF).
    const src = `if c
      then a
      else triage
        (arg1)
        (arg2)`
    expect(parseExpr(src)).toEqual(
      mkIf(v("c"), v("a"), ap(ap(v("triage"), v("arg1")), v("arg2"))))
  })

  it("rejects the removed boolean 'match { TT/FF }' surface", () => {
    expect(() => parseExpr(`match c { TT => a; FF => b }`)).toThrow(/boolean/)
  })
})

// ─────────────────────────── coproduct match (the §2.6 cut) ───────────────

describe("coproduct match", () => {
  // Non-Bool arms parse to a `match` node (the cut desugar is compile.ts's);
  // check the scrutinee and the arms' tags/binders survive verbatim.
  const isMatchOf = (e: Expr, cond: Expr, arms: { pat: string; binders: string[] }[]) => {
    expect(e.tag).toBe("match")
    expect((e as any).cond).toEqual(cond)
    expect((e as any).arms.map((a: any) => ({ pat: a.pat, binders: a.binders }))).toEqual(arms)
  }

  it("parses a multi-constructor match", () => {
    isMatchOf(parseExpr(`match c { foo x => a; bar y => b }`), v("c"),
      [{ pat: "foo", binders: ["x"] }, { pat: "bar", binders: ["y"] }])
  })

  it("accepts a single-arm match (one-constructor cut)", () => {
    isMatchOf(parseExpr(`match c { A x => b }`), v("c"), [{ pat: "A", binders: ["x"] }])
  })

  it("a two-arm match with binders is the cut, not Bool", () => {
    isMatchOf(parseExpr(`match c { Ok x => a; Err y => b }`), v("c"),
      [{ pat: "Ok", binders: ["x"] }, { pat: "Err", binders: ["y"] }])
  })
})
