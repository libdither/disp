// Two-way programs as raw trees read by their shape, for the interaction net (inet.js). A walker that
// only ever meets one agent at a time reads them: at every node what to do is the node's shape, and
// climbing back out of a fork's left part always means "now the right part", so it never has to
// remember where it is.
//
//   L                  nothing
//   F p q              p, then q
//   S c                a step, c saying which:
//     S L                swap         F a b → F b a
//     S (S L)            rotate       F (F a b) c → F a (F b c)
//     S (S (S L))        rotate back  F a (F b c) → F (F a b) c
//     S (S (S (S n)))    call the n-th definition (n a chain of stems)
//     S (S (F d L))      take part d (d: L left, S L right, F L L a stem's child), the rest kept on the stack
//     S (S (F d (S L)))  put part d back
//     S (F L L)          leaf         x → F L x
//     S (F L (S L))      leaf back    F L x → x
//     S (F L (F L L))    stem         x → S x
//     S (F (S L) L)      stem back    S x → x
//     S (F (S L) (S L))  the end of an arm of a choice
//     S (F (S L) (F L L))  the end of a definition
//     S (F (S L) (S (S L)))  no arm: stop
//     S (F (F pin pout) (F (F armL armS) (F armF none)))   choose by the shape at path pin (a leaf,
//                        a stem or a fork: that arm), the arm leaving that shape at path pout; an arm is
//                        F body (the end of an arm), or no arm. Every arm is two forks down, so a walker
//                        climbing out of one knows which it was by the way it came out of each.
//   a path: L here, S p left then p, F L p right then p, F (S L) p a stem's child then p.
//
// The library is a chain F def (F def … L), each definition F body (the end of a definition), the
// main program first.
"use strict";
(function (root) {

const L = { k: 0 }, S = c => ({ k: 1, c }), F = (l, r) => ({ k: 2, l, r });

// ---- the engine's programs (`iso::encode`, as `inet_program` gives them) -------------------------------
function parseTree(s) {
  let i = 0;
  const go = () => { const c = s[i++]; if (c === "L") return L; if (c === "S") { i++; const a = go(); i++; return S(a); } i++; const l = go(); i++; const r = go(); i++; return F(l, r); };
  return go();
}
const natOf = t => { let n = 0; while (t.k === 2) { n++; t = t.r; } return n; };
const listOf = t => { const out = []; while (t.k === 2) { out.push(t.l); t = t.r; } return out; };
function decodePat(t) {
  if (t.k === 0) return { k: "any" };
  if (t.k === 1) return { k: "var", v: natOf(t.c) };
  const n = natOf(t.l);
  return n === 0 ? { k: "L" } : n === 1 ? { k: "S", a: decodePat(t.r) } : { k: "F", a: decodePat(t.r.l), b: decodePat(t.r.r) };
}
function decode(t) {
  const op = natOf(t.l), a = t.r;
  switch (op) {
    case 0: return { op: "seq", xs: listOf(a).map(decode) };
    case 1: return { op: "re", l: decodePat(a.l), r: decodePat(a.r) };
    case 3: return { op: "on", path: listOf(a.l).map(natOf), f: decode(a.r) };
    case 4: return { op: "case", arms: listOf(a).map(c => ({ when: decodePat(c.l.l), gives: decodePat(c.l.r), body: decode(c.r) })) };
    case 6: return { op: "call", d: natOf(a.l), inv: natOf(a.r) === 1 };
    case 9: return { op: "note", f: decode(a.r) };
    default: return { op: "unsupported", name: ["then", "rearrange", "copy", "on a part", "choose", "loop", "call", "run", "forget", "note", "both"][op] };
  }
}
/// `inet_program`'s text as a raw program, its input, and the definitions' names (main first).
function fromEngine(text) {
  const lines = text.split("\n").map(s => s.split("\t"));
  const defs = lines.filter(([h]) => h.startsWith("def ")).map(([h, t]) => { const [, d, inv, name] = h.split(" "); return { d: +d, inv: inv === "1", name: name + (inv === "1" ? "⁻¹" : ""), body: decode(parseTree(t)) }; });
  const main = decode(parseTree(lines.find(([h]) => h === "main")[1]));
  return { prog: program(main, defs, compile), input: parseTree(lines.find(([h]) => h === "input")[1]), names: ["main", ...defs.map(f => f.name)] };
}
const chain = n => n ? S(chain(n - 1)) : L;
const DIR = { left: L, right: S(L), under: F(L, L) };
const CODE = {
  swap: S(L), rotate: S(S(L)), rotateBack: S(S(S(L))),
  leaf: S(F(L, L)), leafBack: S(F(L, S(L))), stem: S(F(L, F(L, L))), stemBack: S(F(S(L), L)),
  armEnd: S(F(S(L), S(L))), defEnd: S(F(S(L), F(L, L))), noArm: S(F(S(L), S(S(L)))),
};
const take = d => S(S(F(DIR[d], L))), put = d => S(S(F(DIR[d], S(L)))), call = n => S(S(S(S(chain(n)))));
const seq = xs => xs.length ? xs.reduceRight((q, p) => F(p, q)) : L;
const path = ds => ds.reduceRight((p, d) => d === "left" ? S(p) : d === "right" ? F(L, p) : F(S(L), p), L);

// ---- patterns: { k: "L" | "S" | "F" | "var" | "any", a, b, v } (as inet.js decodes them) ---------------
const key = p => p.k === "var" ? `v${p.v}` : p.k === "L" ? "L" : p.k === "S" ? `S(${key(p.a)})` : p.k === "F" ? `F(${key(p.a)},${key(p.b)})` : "_";
const pL = { k: "L" }, pS = a => ({ k: "S", a }), pF = (a, b) => ({ k: "F", a, b });
/// The one-layer steps on a pattern, at its top (undefined where a step does not apply).
const STEPS = {
  swap: p => p.k === "F" ? pF(p.b, p.a) : undefined,
  rotate: p => p.k === "F" && p.a.k === "F" ? pF(p.a.a, pF(p.a.b, p.b)) : undefined,
  rotateBack: p => p.k === "F" && p.b.k === "F" ? pF(pF(p.a, p.b.a), p.b.b) : undefined,
  leaf: p => pF(pL, p),
  leafBack: p => p.k === "F" && p.a.k === "L" ? p.b : undefined,
  stem: p => pS(p),
  stemBack: p => p.k === "S" ? p.a : undefined,
};
const size = p => p.k === "F" ? 1 + size(p.a) + size(p.b) : p.k === "S" ? 1 + size(p.a) : 1;
/// The part of p at a path (left/right/under), or undefined; and p with that part replaced.
const partAt = (p, ds) => ds.reduce((q, d) => q && (d === "under" ? (q.k === "S" ? q.a : undefined) : q.k === "F" ? (d === "left" ? q.a : q.b) : undefined), p);
const withPart = (p, ds, x) => !ds.length ? x : ds[0] === "under" ? pS(withPart(p.a, ds.slice(1), x)) : ds[0] === "left" ? pF(withPart(p.a, ds.slice(1), x), p.b) : pF(p.a, withPart(p.b, ds.slice(1), x));

/// A rearrangement l ↔ r as the fewest one-layer steps (a step at a part costs taking it and putting it
/// back too): a search over the shapes in between.
function compileRe(l, r) {
  const goal = key(r), limit = Math.max(size(l), size(r)) + 1;
  const places = [[], ["left"], ["right"], ["under"], ["left", "left"], ["left", "right"], ["right", "left"], ["right", "right"]];
  // cheapest first: a bucket of states for each cost
  const buckets = [[{ p: l, steps: [] }]], seen = new Map([[key(l), 0]]);
  for (let cost = 0, popped = 0; cost < buckets.length && popped < 200000; cost++) {
    for (const { p, steps } of buckets[cost] ?? []) {
      popped++;
      if (seen.get(key(p)) < cost) continue;
      if (key(p) === goal) return seq(steps.flatMap(([at, st]) => [...at.map(take), CODE[st], ...[...at].reverse().map(put)]));
      for (const at of places) {
        const part = partAt(p, at); if (!part) continue;
        for (const [st, f] of Object.entries(STEPS)) {
          const q = f(part); if (!q) continue;
          const next = withPart(p, at, q); if (size(next) > limit) continue;
          const c = cost + 1 + 2 * at.length, k = key(next);
          if ((seen.get(k) ?? Infinity) <= c) continue;
          seen.set(k, c);
          (buckets[c] ??= []).push({ p: next, steps: [...steps, [at, st]] });
        }
      }
    }
  }
  throw new Error(`no steps found for ${key(l)} ↔ ${key(r)}`);
}

/// The shape (L, S or F) a pattern has at a path, if it has one there.
const shapeAt = (p, ds) => { const q = partAt(p, ds); return q && "LSF".includes(q.k) ? q.k : undefined; };
/// A choice as one test: a path where every arm's pattern has a shape, a different one for each.
function testPath(pats) {
  const cands = [[], ["left"], ["right"], ["under"], ["left", "left"], ["left", "right"], ["right", "left"], ["right", "right"], ["left", "left", "left"], ["left", "left", "right"], ["left", "right", "left"], ["right", "right", "left"]];
  return cands.find(ds => { const sh = pats.map(p => shapeAt(p, ds)); return sh.every(Boolean) && new Set(sh).size === sh.length; });
}

/// A two-way program (as inet.js decodes the engine's trees) as a raw tree; `index(d, inv)` gives a
/// call's place in the library.
function compile(i, index) {
  switch (i.op) {
    case "seq": return seq(i.xs.map(x => compile(x, index)));
    case "re": return compileRe(i.l, i.r);
    case "on": return seq([...i.path.map(d => take(["left", "right", "under"][d])), compile(i.f, index), ...[...i.path].reverse().map(d => put(["left", "right", "under"][d]))]);
    case "note": return compile(i.f, index);
    case "call": return call(index(i.d, i.inv));
    case "case": {
      if (i.arms.length > 3) throw new Error("a choice of more than three arms");
      const pin = testPath(i.arms.map(a => a.when)), pout = testPath(i.arms.map(a => a.gives));
      if (!pin || !pout) throw new Error("a choice no single shape tells apart");
      const arm = { L: CODE.noArm, S: CODE.noArm, F: CODE.noArm };
      i.arms.forEach(a => { arm[shapeAt(a.when, pin)] = F(compile(a.body, index), CODE.armEnd); if (shapeAt(a.gives, pout) !== shapeAt(a.when, pin)) throw new Error("an arm whose shape in and out differ"); });
      return S(F(F(path(pin), path(pout)), F(F(arm.L, arm.S), F(arm.F, CODE.noArm))));
    }
    default: throw new Error(`no ${i.name ?? i.op} yet`);
  }
}
/// The whole program: the library chain, the main program first, each ending in the end of a definition.
function program(main, defs, compileOne) {
  const index = (d, inv) => 1 + defs.findIndex(f => f.d === d && f.inv === inv);
  const bodies = [main, ...defs.map(f => f.body)].map(b => F(compileOne(b, index), CODE.defEnd));
  return bodies.reduceRight((rest, b) => F(b, rest), L);
}

// ---- running a raw program directly (to check the compiling against the engine) -------------------
const eq = (a, b) => a.k === b.k && (a.k === 0 || (a.k === 1 ? eq(a.c, b.c) : eq(a.l, b.l) && eq(a.r, b.r)));
const at = (t, p) => p.k === 0 ? t : p.k === 1 ? (t.k === 2 ? at(t.l, p.c) : null) : p.l.k === 0 ? (t.k === 2 ? at(t.r, p.r) : null) : (t.k === 1 ? at(t.c, p.r) : null);
function run(prog, input) {
  const lib = []; for (let t = prog; t.k === 2; t = t.r) lib.push(t.l.l);
  const stack = [];
  const go = (p, x) => {
    if (p.k === 0) return x;
    if (p.k === 2) return go(p.r, go(p.l, x));
    const c = p.c;
    for (const [name, code] of Object.entries(CODE)) if (eq(code, p)) {
      if (name === "armEnd" || name === "defEnd") return x;
      if (name === "noArm") throw new Error("no arm takes it");
      const y = { swap: x => x.k === 2 && F(x.r, x.l), rotate: x => x.k === 2 && x.l.k === 2 && F(x.l.l, F(x.l.r, x.r)), rotateBack: x => x.k === 2 && x.r.k === 2 && F(F(x.l, x.r.l), x.r.r),
        leaf: x => F(L, x), leafBack: x => x.k === 2 && x.l.k === 0 && x.r, stem: x => S(x), stemBack: x => x.k === 1 && x.c }[name](x);
      if (!y) throw new Error(`${name} does not fit`);
      return y;
    }
    if (c.k === 1 && c.c.k === 1 && c.c.c.k === 1) { let n = 0; for (let t = c.c.c.c; t.k === 1; t = t.c) n++; return go(lib[n], x); }
    if (c.k === 1 && c.c.k === 2) {
      const d = c.c.l, back = c.c.r.k === 1, port = d.k === 0 ? "l" : d.k === 1 ? "r" : "c";
      if (!back) { stack.push([x, port]); return port === "c" ? x.c : x[port]; }
      const [rest, pt] = stack.pop(); return pt === "c" ? S(x) : pt === "l" ? F(x, rest.r) : F(rest.l, x);
    }
    if (c.k === 2 && c.l.k === 2) {
      const shape = t => ["L", "S", "F"][t.k], s = shape(at(x, c.l.l)), arms = c.r, arm = s === "L" ? arms.l.l : s === "S" ? arms.l.r : arms.r.l;
      const y = go(arm, x);
      if (shape(at(y, c.l.r)) !== s) throw new Error("an arm left the wrong shape");
      return y;
    }
    throw new Error("an unknown step");
  };
  return go(lib[0], input);
}
const show = t => t.k === 0 ? "L" : t.k === 1 ? `S(${show(t.c)})` : `F(${show(t.l)},${show(t.r)})`;

root.RawProg = { L, S, F, CODE, parseTree, compile, compileRe, program, fromEngine, run, show, eq };
})(typeof window !== "undefined" ? window : globalThis);
