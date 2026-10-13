// The two-way machine as an interaction net of raw trees (player/inet.html). Every agent is a leaf, a
// stem or a fork, save one walker and the root it hangs from; the program (rawprog.js), the tree it works
// on and the walker's stack are all such trees. Every step is one meeting of the walker with the agent
// its principal port faces, and rewires only their ports.
//
// - The walker goes down into a node by turning that node's principal port toward itself, the wire up
//   kept in the slot it went into; coming back up turns it again. The open path from the root is kept in
//   the nodes, a fork remembering which side the walker went down.
// - What to do is the shape it meets: a leaf, nothing; a fork, its left part then its right (back up
//   out of a fork's left, it goes down its right); a stem, a step, read off the small tree under it.
// - A step on the tree takes at most three meetings, one agent at a time; a choice reads its path, goes
//   down the tree to read one shape, and takes the arm two forks down; leaving the arm it knows which
//   it was by the way it came out of those two forks, and checks the shape the arm leaves.
// - The stack is a chain of forks: what a part was taken out of, and the way back from a call. A call
//   copies the definition's number as a chain of stems, climbs to the root leaving a frame at every node
//   it climbs out of, counts its way along the library and, done, climbs back and follows the frames.
"use strict";
(function (root) {

const ARITY = { L: 1, S: 2, F: 3, Root: 1, W: 7 };
/// The walker's ports: what it meets, its way back up the program, its way back up the tree, the other
/// side (the tree's top while it reads the program, its place in the program while it works on the
/// tree), its stack, and two hands.
const P = 0, PB = 1, DB = 2, OTHER = 3, STACK = 4, R0 = 5, R1 = 6;
const KIND = ["L", "S", "F"];

class Net {
  constructor() { this.a = []; this.made = 0; this.gone = 0; }
  mk(k, role, near) {
    const g = { id: this.a.length, k, role, ports: new Array(ARITY[k]).fill(null), open: -1, x: (near?.x ?? 0) + (Math.random() - 0.5) * 16, y: (near?.y ?? 0) + (Math.random() - 0.5) * 16 };
    this.a.push(g); return g;
  }
  set(g, i, p) { g.ports[i] = p; if (p) this.a[p[0]].ports[p[1]] = [g.id, i]; }
  at(p) { return p ? this.a[p[0]] : null; }
  kill(g) { this.a[g.id] = null; this.gone++; }
  live() { return this.a.filter(Boolean); }
}

class Machine {
  /// A raw program (rawprog.js `program`) on a raw tree.
  constructor(prog, input) {
    const n = this.n = new Net();
    this.root = n.mk("Root", "prog");
    const top = this.plant(prog, "prog", null);
    const data = this.plant(input, "data", null);
    const bottom = n.mk("L", "stack");
    const count = n.mk("L", "count");  // the main program is the library's first definition
    const w = this.w = n.mk("W", "walker");
    n.set(w, PB, [this.root.id, 0]); n.set(w, P, [top.id, 0]);
    n.set(w, OTHER, [data.id, 0]); n.set(w, STACK, [bottom.id, 0]); n.set(w, R1, [count.id, 0]);
    this.st = { mode: "seek" };
    this.steps = 0; n.made = n.gone = 0;
    this.last = null; this.fired = new Map();
    this.place(top, 0, 0, 1); this.place(data, 260, 0, 1); bottom.x = -120; count.x = -150;
  }
  plant(t, role, near) {
    const g = this.n.mk(KIND[t.k], role, near);
    if (t.k === 1) this.n.set(g, 1, [this.plant(t.c, role, g).id, 0]);
    if (t.k === 2) { this.n.set(g, 1, [this.plant(t.l, role, g).id, 0]); this.n.set(g, 2, [this.plant(t.r, role, g).id, 0]); }
    return g;
  }
  /// A first place for each agent: a plain tree layout (the page relaxes it).
  place(g, x, y, depth) {
    g.x = x; g.y = y;
    const kids = g.ports.slice(1).map(p => this.n.at(p)).filter(Boolean);
    const spread = 120 / depth;
    kids.forEach((c, i) => this.place(c, x + (kids.length === 1 ? 0 : (i ? spread : -spread) / 2), y + 26, depth + 1));
  }
  /// The tree below a port, as text.
  show(p, budget = { n: 400 }) {
    const g = this.n.at(p); if (!g) return "_"; if (budget.n-- <= 0) return "…";
    if (g.open >= 0) return "?";
    return g.k === "L" ? "L" : g.k === "S" ? `S(${this.show(g.ports[1], budget)})` : g.k === "F" ? `F(${this.show(g.ports[1], budget)},${this.show(g.ports[2], budget)})` : g.k;
  }
  answer() { return this.show(this.w.ports[OTHER]); }
  done() { return this.st.mode === "halt" || this.st.mode === "error"; }
  partner() { return this.n.at(this.w.ports[P]); }

  // -- the walker's moves; each part of one meeting
  set(g, i, p) { this.n.set(g, i, p); }
  /// Down from x (closed, the walker above) into its slot i; x turns its principal to the walker.
  openDown(x, i, b) { const w = this.w, up = w.ports[b], c = x.ports[i]; this.set(x, i, up); this.set(x, 0, [w.id, b]); this.set(w, P, c); x.open = i; }
  /// Up out of x (opened, the walker below it).
  closeUp(x, b) { const w = this.w, i = x.open, c = w.ports[b], up = x.ports[i]; this.set(x, i, c); this.set(x, 0, [w.id, b]); this.set(w, P, up); x.open = -1; }
  /// From the slot of x it came out of to slot j.
  switchTo(x, j, b) { const w = this.w, i = x.open, c = w.ports[b], up = x.ports[i], cj = x.ports[j]; this.set(x, i, c); this.set(x, j, up); this.set(x, 0, [w.id, b]); this.set(w, P, cj); x.open = j; }
  /// Above x (closed): turn back up.
  turnUp(x, b) { const w = this.w, up = w.ports[b]; this.set(w, b, [x.id, 0]); this.set(w, P, up); }
  /// Below x (opened, or the root): back down to what it came up from.
  turnDown(x, b) { const w = this.w, c = w.ports[b]; this.set(w, b, [x.id, 0]); this.set(w, P, c); }
  /// Face what port q holds, holding what it faced there instead.
  swap(q) { const w = this.w, a = w.ports[P], b = w.ports[q]; this.set(w, P, b); this.set(w, q, a); }
  /// Hold x (met now) in port q; face nothing for the moment.
  hold(q, x) { this.w.ports[P] = null; this.set(this.w, q, [x.id, 0]); }
  /// Face what port q holds, letting go of it there.
  face(q) { const w = this.w, h = w.ports[q]; w.ports[q] = null; this.set(w, P, h); }
  /// The tree's new top is t: hold it on the other side and face the program again.
  newTop(t) { const w = this.w, back = w.ports[OTHER]; this.set(w, OTHER, t); this.set(w, P, back); }
  push(item, near) {
    const n = this.n, w = this.w, f = n.mk("F", "stack", near); n.made++;
    let it = n.mk("L", "stack", near); n.made++;
    for (let i = 0; i < item; i++) { const s = n.mk("S", "stack", near); n.made++; this.set(s, 1, [it.id, 0]); it = s; }
    this.set(f, 1, [it.id, 0]); this.set(f, 2, w.ports[STACK]); this.set(w, STACK, [f.id, 0]);
  }
  fail(msg) { this.st = { mode: "error", msg }; this.last = { rule: msg }; return false; }

  /// One meeting: the rule for the walker's state, the agent met and what it remembers. False at the
  /// end (or on an error: `st.msg`).
  step() {
    if (this.done()) return false;
    const x = this.partner(), st = this.st;
    if (!x) return this.fail("the walker meets nothing");
    const at = meets(x), r = RULES.find(r => r.state === st.mode && r.meets.includes(at) && (!r.when || r.when(st)));
    if (!r) return this.fail(`no rule for ${STATES[st.mode] ?? st.mode} meeting ${at}`);
    this.steps++;
    r.act(this, x, st);
    if (this.st.mode === "error") return false;
    this.st.mode = r.then;
    this.fired.set(r.id, (this.fired.get(r.id) ?? 0) + 1);
    this.last = { rule: r.does, id: r.id, a: this.w.id, b: x.id };
    return true;
  }
  closeTop(x) { const w = this.w, i = x.open, c = w.ports[DB]; this.set(x, i, c); x.open = -1; w.ports[DB] = null; this.set(w, P, w.ports[OTHER]); this.set(w, OTHER, [x.id, 0]); }
  makeCount(first) {
    const n = this.n, w = this.w, s = n.mk(first ? "L" : "S", "count", w); n.made++;
    if (!first) this.set(s, 1, w.ports[R1]);
    this.set(w, R1, [s.id, 0]);
  }

  /// Every wire joins two ports that name each other.
  consistent() {
    for (const g of this.n.live()) g.ports.forEach((p, i) => {
      if (!p) return;
      const h = this.n.a[p[0]];
      if (!h || !h.ports[p[1]] || h.ports[p[1]][0] !== g.id || h.ports[p[1]][1] !== i) throw new Error(`${g.k}${g.id}.${i} → ${p} is not wired back`);
    });
    return true;
  }
}

// ---- the rules -----------------------------------------------------------------------------------
/// The agent met, as the rules tell agents apart: its shape, and for one turned toward the walker which
/// way the walker went down (↓l its left, ↓r its right).
const meets = x => x.k === "Root" ? "Root" : x.open < 0 ? x.k : x.k === "S" ? "S↓" : x.open === 1 ? "F↓l" : "F↓r";
const UP = ["S↓", "F↓l", "F↓r"], ANY = ["L", "S", "F"];
/// The walker's states, as the sidebar names them.
const STATES = {
  run: "going into a node", done: "back out of a part",
  c0: "reading a step", c1: "reading under a stem", c2: "reading a fork's left", c3: "reading under two stems", count: "reading a call's number",
  c4: "reading which part", c5: "read the part", c6: "reading take or put", c7: "read the fork's left", c8: "reading a fork's right", c9: "reading under the right's stem",
  climbCode: "back up to the step",
  swap: "swap", rotate: "rotate", rotate2: "rotate (2)", rotate3: "rotate (3)", rotateBack: "rotate back", rotateBack2: "rotate back (2)", rotateBack3: "rotate back (3)",
  leaf: "add a leaf", stem: "add a stem", stemBack: "drop a stem", leafBack: "drop a leaf", leafBack2: "drop a leaf (2)", leafBack3: "drop a leaf (3)",
  take: "take a part out", put2: "put a part back", put3: "put a part back (2)", closeStep: "the step done",
  pin: "reading a path", pdir: "reading right or child", pdir2: "read right or child", pback: "back up the path", ddown: "down the tree", dup: "back up the tree",
  arms1: "to the arms", arms2: "to the arms (2)", arms3: "to the arm", arms4: "to the arm (2)",
  exit0: "out of the arm", exit1: "out of the arms", exit2: "out of the arms (2)", exit3: "to the test", exit4: "to the test (2)", out1: "out of the choice", out2: "out of the choice (2)", out3: "out of the choice (3)",
  climb: "climbing to the root", seek: "along the library", "count?": "counting", pass: "passing a definition", enter: "into a definition",
  ret: "climbing back", pop: "reading the way back", item: "reading a frame", descend: "down the way back", popNext: "the next frame", halt: "the end",
};
const RULES = [];
let section = "";
/// A rule: in `state`, meeting one of `meets` (and remembering what `when` asks, said `whenText`), it
/// does `act`, said `does`, and goes on in `then`.
const rule = (state, meets, does, then, act, when, whenText) => RULES.push({ id: RULES.length, section, state, meets, does, then, act, when, whenText });
const down = (m, x, st) => { m.openDown(x, 1, PB); st.depth++; };
const ends = op => (m, x, st) => { m.turnUp(x, PB); st.op = op; };

section = "walking the program";
rule("run", ["L"], "a leaf: nothing to do; back up", "done", (m, x) => m.turnUp(x, PB));
rule("run", ["F"], "a fork: down into its left", "run", (m, x) => m.openDown(x, 1, PB));
rule("run", ["S"], "a stem: a step; down to read which", "c0", (m, x, st) => { m.openDown(x, 1, PB); st.depth = 0; });
rule("done", ["F↓l"], "back out of a fork's left: over to its right", "run", (m, x) => m.switchTo(x, 2, PB));
rule("done", ["F↓r"], "back out of a fork's right: the fork is done, up", "done", (m, x) => m.closeUp(x, PB));

section = "reading a step";
rule("c0", ["L"], "a leaf: it is swap", "climbCode", ends("swap"));
rule("c0", ["S"], "a stem: read below", "c1", down);
rule("c0", ["F"], "a fork: read its left", "c2", down);
rule("c1", ["L"], "a leaf: it is rotate", "climbCode", ends("rotate"));
rule("c1", ["S"], "a stem: read below", "c3", down);
rule("c1", ["F"], "a fork: take or put a part; read its left", "c4", down);
rule("c3", ["L"], "a leaf: it is rotate back", "climbCode", ends("rotateBack"));
rule("c3", ["S"], "a stem: a call; a count started in hand two, read its number", "count", (m, x, st) => { down(m, x, st); m.makeCount(true); });
rule("count", ["S"], "a stem of the number: a stem on the count", "count", (m, x, st) => { m.makeCount(false); down(m, x, st); });
rule("count", ["L"], "the number's end: it is a call", "climbCode", ends("call"));
rule("c4", ["L", "S", "F"], "the part: a leaf left, a stem right, a fork a stem's child", "c5", (m, x, st) => { st.dir = { L: "left", S: "right", F: "under" }[x.k]; m.turnUp(x, PB); });
rule("c5", ["F↓l"], "over to the fork's right", "c6", (m, x) => m.switchTo(x, 2, PB));
rule("c6", ["L"], "a leaf: take the part out", "climbCode", ends("take"));
rule("c6", ["S"], "a stem: put the part back", "climbCode", ends("put"));
rule("c2", ["L", "S"], "remember it; back up", "c7", (m, x, st) => { st.a = x.k; m.turnUp(x, PB); });
rule("c2", ["F"], "a fork: a choice; down into its path", "pin", (m, x, st) => { m.openDown(x, 1, PB); st.out = false; st.dirs = []; });
rule("c7", ["F↓l"], "over to the fork's right", "c8", (m, x) => m.switchTo(x, 2, PB));
rule("c8", ["L"], "it is add a leaf", "climbCode", ends("leaf"), st => st.a === "L", "a leaf on the left");
rule("c8", ["S"], "it is drop a leaf", "climbCode", ends("leafBack"), st => st.a === "L", "a leaf on the left");
rule("c8", ["F"], "it is add a stem", "climbCode", ends("stem"), st => st.a === "L", "a leaf on the left");
rule("c8", ["L"], "it is drop a stem", "climbCode", ends("stemBack"), st => st.a === "S", "a stem on the left");
rule("c8", ["F"], "the end of a definition", "climbCode", ends("defEnd"), st => st.a === "S", "a stem on the left");
rule("c8", ["S"], "read below", "c9", down, st => st.a === "S", "a stem on the left");
rule("c9", ["L"], "the end of an arm", "climbCode", ends("armEnd"));
rule("c9", ["S"], "no arm: stop", "climbCode", ends("noArm"));
rule("climbCode", UP, "back up toward the step", "climbCode", (m, x, st) => { m.closeUp(x, PB); st.depth--; }, st => st.depth > 0, "more to climb");
const onTree = (op, says) => rule("climbCode", ["S↓"], `at the step: ${says}; face the tree (the step held on the other side)`, op, m => m.swap(OTHER), st => st.depth === 0 && st.op === op, `it read ${says}`);
onTree("swap", "swap"); onTree("rotate", "rotate"); onTree("rotateBack", "rotate back"); onTree("leaf", "add a leaf"); onTree("leafBack", "drop a leaf");
onTree("stem", "add a stem"); onTree("stemBack", "drop a stem"); onTree("take", "take a part out");
rule("climbCode", ["S↓"], "at the step: put a part back; hold the step, face the stack", "put2", (m, x) => { m.hold(R0, x); m.face(STACK); }, st => st.depth === 0 && st.op === "put", "it read put back");
rule("climbCode", ["S↓"], "at the step: a call; a frame saying so, then climb", "climb", (m, x) => { m.push(3, x); m.closeUp(x, PB); }, st => st.depth === 0 && st.op === "call", "it read a call");
rule("climbCode", ["S↓"], "at the step: the arm is done; out of it", "exit0", (m, x) => m.closeUp(x, PB), st => st.depth === 0 && st.op === "armEnd", "it read an arm's end");
rule("climbCode", ["S↓"], "at the step: the definition is done; climb back", "ret", (m, x) => m.closeUp(x, PB), st => st.depth === 0 && st.op === "defEnd", "it read a definition's end");
rule("climbCode", ["S↓"], "at the step: no arm takes this; stop", "halt", m => m.fail("no arm takes it"), st => st.depth === 0 && st.op === "noArm", "it read no arm");

section = "steps on the tree";
rule("swap", ["F"], "the fork's two sides change places", "closeStep", (m, x) => { const l = x.ports[1], r = x.ports[2]; m.set(x, 1, r); m.set(x, 2, l); m.newTop([x.id, 0]); });
rule("rotate", ["F"], "hold the fork (hand one) and its right (hand two); to its left", "rotate2", (m, x) => { const w = m.w, inner = x.ports[1], c = x.ports[2]; m.hold(R0, x); m.set(w, R1, c); x.ports[1] = x.ports[2] = null; m.set(w, P, inner); });
rule("rotate2", ["F"], "this fork takes its right and hand two's; hold its left, face the first", "rotate3", (m, x) => { const w = m.w, a = x.ports[1], b = x.ports[2], c = w.ports[R1]; m.set(x, 1, b); m.set(x, 2, c); m.set(w, R1, a); const o = w.ports[R0]; m.set(w, R0, [x.id, 0]); m.set(w, P, o); });
rule("rotate3", ["F"], "the first fork takes hand two's and hand one's: the new top", "closeStep", (m, x) => { const w = m.w; m.set(x, 1, w.ports[R1]); w.ports[R1] = null; m.set(x, 2, w.ports[R0]); w.ports[R0] = null; m.newTop([x.id, 0]); });
rule("rotateBack", ["F"], "hold the fork (hand one) and its left (hand two); to its right", "rotateBack2", (m, x) => { const w = m.w, a = x.ports[1], inner = x.ports[2]; m.hold(R0, x); m.set(w, R1, a); x.ports[1] = x.ports[2] = null; m.set(w, P, inner); });
rule("rotateBack2", ["F"], "this fork takes hand two's and its left; hold its right, face the first", "rotateBack3", (m, x) => { const w = m.w, b = x.ports[1], c = x.ports[2], a = w.ports[R1]; m.set(x, 1, a); m.set(x, 2, b); m.set(w, R1, c); const o = w.ports[R0]; m.set(w, R0, [x.id, 0]); m.set(w, P, o); });
rule("rotateBack3", ["F"], "the first fork takes hand one's and hand two's: the new top", "closeStep", (m, x) => { const w = m.w; m.set(x, 1, w.ports[R0]); w.ports[R0] = null; m.set(x, 2, w.ports[R1]); w.ports[R1] = null; m.newTop([x.id, 0]); });
rule("leaf", ["L", "S", "F"], "a fork made above, a leaf made on its left: the new top", "closeStep", (m, x) => { const n = m.n, f = n.mk("F", "data", x), l = n.mk("L", "data", x); n.made += 2; m.set(f, 1, [l.id, 0]); m.set(f, 2, [x.id, 0]); m.newTop([f.id, 0]); });
rule("stem", ["L", "S", "F"], "a stem made above: the new top", "closeStep", (m, x) => { const s = m.n.mk("S", "data", x); m.n.made++; m.set(s, 1, [x.id, 0]); m.newTop([s.id, 0]); });
rule("stemBack", ["S"], "the stem goes; its child is the top", "closeStep", (m, x) => { const c = x.ports[1]; m.n.kill(x); m.newTop(c); });
rule("leafBack", ["F"], "hold the fork (hand one); to its left", "leafBack2", (m, x) => { const l = x.ports[1]; m.hold(R0, x); x.ports[1] = null; m.set(m.w, P, l); });
rule("leafBack2", ["L"], "the leaf goes; face the fork", "leafBack3", (m, x) => { m.n.kill(x); m.face(R0); });
rule("leafBack3", ["F"], "the fork goes; its right is the top", "closeStep", (m, x) => { const r = x.ports[2]; m.n.kill(x); m.newTop(r); });
const take = (m, x, st) => { const n = m.n, w = m.w, port = st.dir === "right" ? 2 : 1, part = x.ports[port]; x.ports[port] = null; const f = n.mk("F", "stack", x); n.made++; m.set(f, 2, w.ports[STACK]); m.set(f, 1, [x.id, 0]); m.set(w, STACK, [f.id, 0]); m.newTop(part); };
rule("take", ["F"], "the part is the top; the rest onto the stack (a fork made, the rest on its left)", "closeStep", take, st => st.dir !== "under", "left or right");
rule("take", ["S"], "the child is the top; the stem onto the stack (a fork made)", "closeStep", take, st => st.dir === "under", "a stem's child");
rule("put2", ["F"], "the frame goes; face the rest it held", "put3", (m, x) => { const w = m.w, rest = x.ports[1], below = x.ports[2]; m.n.kill(x); m.set(w, STACK, below); m.set(w, P, rest); });
rule("put3", ["F", "S"], "the top goes back into the rest, now the top; face the step", "closeStep", (m, x, st) => { const w = m.w; m.set(x, st.dir === "right" ? 2 : 1, w.ports[OTHER]); m.set(w, OTHER, [x.id, 0]); m.face(R0); });
rule("closeStep", ["S↓"], "the step is done; up", "done", (m, x) => m.closeUp(x, PB));

section = "a choice";
rule("pin", ["L"], "the path ends; back up it", "pback", (m, x, st) => { m.turnUp(x, PB); st.left = st.dirs.length; });
rule("pin", ["S"], "the path: left; down it", "pin", (m, x, st) => { st.dirs.push("left"); m.openDown(x, 1, PB); });
rule("pin", ["F"], "the path: right or a stem's child; read which", "pdir", (m, x) => m.openDown(x, 1, PB));
rule("pdir", ["L", "S"], "a leaf: right; a stem: a stem's child", "pdir2", (m, x, st) => { st.pd = x.k === "L" ? "right" : "under"; m.turnUp(x, PB); });
rule("pdir2", ["F↓l"], "over to the path's rest", "pin", (m, x, st) => { st.dirs.push(st.pd); m.switchTo(x, 2, PB); });
rule("pback", UP, "back up the path", "pback", (m, x, st) => { m.closeUp(x, PB); st.left--; }, st => st.left > 0, "more of the path");
rule("pback", ["F↓l", "F↓r"], "at the test: face the tree (the test held on the other side)", "ddown", (m, x, st) => { m.swap(OTHER); st.depthD = 0; }, st => st.left === 0, "the path read");
rule("ddown", ["F", "S"], "down the tree the way the path says", "ddown", (m, x, st) => { const d = st.dirs[st.depthD]; if (x.k !== (d === "under" ? "S" : "F")) return m.fail(`the tree has no ${d} part there`); m.openDown(x, d === "right" ? 2 : 1, DB); st.depthD++; }, st => st.depthD < st.dirs.length, "more of the path");
const tested = (m, st) => { if (st.out && st.seen !== st.letter) m.fail("the arm left the wrong shape"); else st.letter = st.seen; };
rule("ddown", ["L", "S", "F"], "the top's shape picks the arm; face the test", "arms1", (m, x, st) => { st.seen = x.k; m.swap(OTHER); tested(m, st); }, st => !st.dirs.length && !st.out, "the path empty, choosing");
rule("ddown", ["L", "S", "F"], "the top's shape must be the arm's; face the test", "out1", (m, x, st) => { st.seen = x.k; m.swap(OTHER); tested(m, st); }, st => !st.dirs.length && st.out, "the path empty, checking");
rule("ddown", ["L", "S", "F"], "here is the shape; back up the tree", "dup", (m, x, st) => { st.seen = x.k; m.turnUp(x, DB); }, st => st.dirs.length > 0 && st.depthD === st.dirs.length, "at the path's end");
rule("dup", UP, "back up the tree", "dup", (m, x, st) => { m.closeUp(x, DB); st.depthD--; }, st => st.depthD > 1, "below the top");
rule("dup", UP, "the top: face the test; the shape picks the arm", "arms1", (m, x, st) => { m.closeTop(x); tested(m, st); }, st => st.depthD === 1 && !st.out, "at the top, choosing");
rule("dup", UP, "the top: face the test; the shape must be the arm's", "out1", (m, x, st) => { m.closeTop(x); tested(m, st); }, st => st.depthD === 1 && st.out, "at the top, checking");
rule("arms1", ["F↓l"], "out of the test", "arms2", (m, x) => m.closeUp(x, PB));
rule("arms2", ["F↓l"], "over to the arms", "arms3", (m, x) => m.switchTo(x, 2, PB));
rule("arms3", ["F"], "a leaf or a stem: down left; a fork: down right", "arms4", (m, x, st) => m.openDown(x, st.letter === "F" ? 2 : 1, PB));
rule("arms4", ["F"], "a leaf or a fork: down left; a stem: down right; run the arm", "run", (m, x, st) => m.openDown(x, st.letter === "S" ? 2 : 1, PB));
rule("exit0", ["F↓r"], "out of the arm", "exit1", (m, x) => m.closeUp(x, PB));
rule("exit1", ["F↓l", "F↓r"], "out of the arms' fork: remember which side", "exit2", (m, x, st) => { st.first = x.open; m.closeUp(x, PB); });
rule("exit2", ["F↓l", "F↓r"], "out of the arms: the two sides say which arm", "exit3", (m, x, st) => { st.letter = st.first === 1 ? (x.open === 1 ? "L" : "F") : "S"; m.closeUp(x, PB); });
rule("exit3", ["F↓r"], "over to the test", "exit4", (m, x) => m.switchTo(x, 1, PB));
rule("exit4", ["F"], "down into the path the arm must leave its shape at", "pin", (m, x, st) => { m.openDown(x, 2, PB); st.out = true; st.dirs = []; });
rule("out1", ["F↓r"], "out of the test", "out2", (m, x) => m.closeUp(x, PB));
rule("out2", ["F↓l"], "out of the choice's fork", "out3", (m, x) => m.closeUp(x, PB));
rule("out3", ["S↓"], "the choice is done; up", "done", (m, x) => m.closeUp(x, PB));

section = "a call";
rule("climb", UP, "climb out, leaving a frame for the way (a fork; its left a leaf under 0 to 2 stems)", "climb", (m, x) => { m.push(x.k === "S" ? 2 : x.open === 1 ? 0 : 1, x); m.closeUp(x, PB); });
rule("climb", ["Root"], "at the root: turn down into the library", "seek", (m, x) => m.turnDown(x, PB));
rule("seek", ["F"], "hold the library's fork; face the count", "count?", (m, x) => { m.hold(R0, x); m.face(R1); });
rule("count?", ["S"], "a stem of the count goes: pass this definition", "pass", (m, x) => { const next = x.ports[1]; m.n.kill(x); m.set(m.w, R1, next); m.face(R0); });
rule("count?", ["L"], "the count's leaf goes: this definition", "enter", (m, x) => { m.n.kill(x); m.face(R0); });
rule("pass", ["F"], "down the library's right", "seek", (m, x) => m.openDown(x, 2, PB));
rule("enter", ["F"], "down into the definition", "run", (m, x) => m.openDown(x, 1, PB));
rule("ret", UP, "climb out", "ret", (m, x) => m.closeUp(x, PB));
rule("ret", ["Root"], "at the root: hold the library, face the stack", "pop", (m, x) => { const w = m.w, top = w.ports[PB]; m.set(w, PB, [x.id, 0]); m.set(w, R0, top); m.face(STACK); });
rule("pop", ["L"], "the stack's bottom: the end", "halt", (m, x) => { m.set(m.w, STACK, [x.id, 0]); m.w.ports[P] = null; m.face(R0); });
rule("pop", ["F"], "a frame goes; face what it says", "item", (m, x, st) => { const w = m.w, item = x.ports[1], below = x.ports[2]; m.n.kill(x); m.set(w, STACK, below); m.set(w, P, item); st.count = 0; });
rule("item", ["S"], "a stem goes: count it", "item", (m, x, st) => { const next = x.ports[1]; m.n.kill(x); m.set(m.w, P, next); st.count++; });
rule("item", ["L"], "the leaf goes: no stems, left; one, right; two, a stem's child; three, the call; face the node", "descend", (m, x, st) => { m.n.kill(x); m.face(R0); st.way = ["left", "right", "under", "call"][st.count]; });
rule("descend", ["S"], "the call is here, and done: back up", "done", (m, x) => m.turnUp(x, PB), st => st.way === "call", "the frame said the call");
rule("descend", ["F", "S"], "down the way the frame said", "popNext", (m, x, st) => m.openDown(x, st.way === "right" ? 2 : 1, PB), st => st.way !== "call", "the frame said a way");
rule("popNext", ["L", "S", "F"], "hold it; face the stack", "pop", (m, x) => { m.hold(R0, x); m.face(STACK); });

root.INet = { Machine, RULES, STATES, P, PB, DB, OTHER, STACK, R0, R1 };

})(typeof window !== "undefined" ? window : globalThis);
