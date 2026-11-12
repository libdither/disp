// Reflective linear nets, as the player runs them (index.html): the rule table of ../lean/Tables.lean
// (13 kinds of agent, copiers and erasers facing in or out), a parser and compiler for its programs,
// and a recorder that keeps a run so any moment of it can be shown again. No page needed: check.mjs
// runs it under node against ../nets/net.py.
"use strict";
(function (root) {

/// Kinds of agent, by number. `ph` is a placeholder a rewrite uses while it rewires. `pod` is never in a
/// net: the player draws a named part folded shut as one.
const KINDS = ["", "lam", "Lam", "app", "var", "rho", "O", "T", "R", "sig", "dupI", "dupO", "eraI", "eraO", "root", "ph", "pod"];
const K = {}; KINDS.forEach((k, i) => { if (k) K[k] = i; });
/// Each port's polarity, principal first: a value flows out through an `o` port and in through an `i` port.
const POL = ["", "ooi", "ooi", "oii", "oi", "o", "iio", "io", "io", "ioo", "ioo", "oii", "i", "o", "i", "io", "o"];
const ARITY = POL.map(p => Math.max(0, p.length - 1));
const CODE = new Set([K.lam, K.Lam, K.app, K.var, K.rho]);

/// What a rewrite does, for the history bar.
const FAMILIES = ["call", "open", "read", "thaw", "copy", "cancel", "erase"];
const F = {}; FAMILIES.forEach((f, i) => { F[f] = i; });

const VIEW_OF = { fun: "a function", opened: "an opened function", app: "an application", var: "a captured value", prim: "ρ" };
/// The arms of a read, one per kind of value, and the parts each is handed after the other arms.
const ARMS = ["fun", "opened", "app", "var", "prim"];
const PARTS = { fun: ["F"], opened: ["F"], app: ["f", "a"], var: ["w"], prim: [] };

/// The rule for a pair of kinds, tried both ways round: [which agent leads, family, rule], or null.
function ruleFor(ka, kb) {
  for (const [x, y, flip] of [[ka, kb, false], [kb, ka, true]]) {
    let r = null;
    if (x === K.O) r = y === K.lam ? [F.call, "run"] : y === K.Lam ? [F.open, "open"] : y === K.rho ? [F.read, "reflect"] : null;
    else if (x === K.T) r = y === K.app ? [F.thaw, "thawApp"] : y === K.var ? [F.thaw, "thawVar"] : (y === K.lam || y === K.Lam || y === K.rho) ? [F.thaw, "thawValue"] : null;
    else if (x === K.R) r = y === K.lam ? [F.read, "viewFun"] : y === K.Lam ? [F.read, "viewOpened"] : y === K.app ? [F.read, "viewApp"] : y === K.var ? [F.read, "viewVar"] : y === K.rho ? [F.read, "viewPrim"] : null;
    else if (x === K.sig || x === K.dupI) r = CODE.has(y) ? [F.copy, "copy"] : (x === K.dupI && y === K.dupO) ? [F.cancel, "cancel"] : null;
    else if (x === K.dupO) r = y === K.sig ? [F.copy, "copy"] : null;
    else if (x === K.eraI) r = (CODE.has(y) || y === K.dupO) ? [F.erase, "erase"] : null;
    else if (x === K.eraO) r = (y === K.sig || y === K.eraI) ? [F.erase, "erase"] : null;
    if (r) return [flip, r[0], r[1]];
  }
  return null;
}

/// A small seeded generator, for running pairs in a random order.
function rng(seed) {
  let s = (seed >>> 0) || 0x9e3779b9;
  return () => { s ^= s << 13; s >>>= 0; s ^= s >>> 17; s ^= s << 5; s >>>= 0; return s / 4294967296; };
}

class Net {
  /// order: "rounds" (every waiting pair, then the pairs those made), "newest" (the pair made last
  /// goes first) or "random".
  constructor(order = "rounds", seed = 1) {
    this.cap = 1024;
    this.kind = new Uint8Array(this.cap);      // by agent id; 0 once gone
    this.far = new Int32Array(3 * this.cap);   // 3 per agent: the port (4 * agent + index) it is wired to, or -1
    this.where = new Int32Array(this.cap);     // its place in `ids`
    this.ids = new Int32Array(this.cap);       // the agents there are now
    this.live = 0;
    this.op = new Map();                       // which copy each copier belongs to
    // Named parts: the code of a definition, or a view a read built, as one stretch of the net. Each
    // agent of written code knows the part it is in (the innermost); a copy of a part is a part too.
    this.grp = new Map();                      // agent -> part
    this.parts = []; this.partNext = 0;        // { name, src: the part it is a copy of or -1, size: its agents when whole }
    this.partKey = new Map();                  // "part:copy:side" -> the copy's part
    // Parts are numbered in the order they are made, so going back to a moment and running on makes
    // the same parts again under the same numbers: only the count (partNext) is put back. When a run
    // takes another way from a moment, the copies it had made after that moment are forgotten.
    this.cur = -1; this.names = null;          // the part being written; the name of each defined term
    this.next = 0; this.ops = 0;
    this.queue = []; this.head = 0; this.mark = 0;
    this.order = order; this.seed = seed; this.rand = rng(seed);
    this.events = 0; this.round = 0; this.crossed = 0;
    this.counts = new Int32Array(FAMILIES.length);
    this.last = null;
    // four agents kept aside for rewrites to rewire through, so the ids of real agents only count agents made
    this.ph = [this.make(K.ph), this.make(K.ph), this.make(K.ph), this.make(K.ph)];
    for (const h of this.ph) this.remove(h);
  }
  grow() {
    const cap = this.cap * 2, grown = (A, n, fill) => { const B = new A(n); if (fill !== undefined) B.fill(fill); return B; };
    const kind = grown(Uint8Array, cap), far = grown(Int32Array, 3 * cap), where = grown(Int32Array, cap), ids = grown(Int32Array, cap);
    kind.set(this.kind); far.set(this.far); where.set(this.where); ids.set(this.ids);
    Object.assign(this, { cap, kind, far, where, ids });
  }
  make(kind) {
    if (this.next === this.cap) this.grow();
    const a = this.next++;
    this.put(a, kind);
    if (this.cur >= 0) { this.grp.set(a, this.cur); this.parts[this.cur].size++; }
    return a;
  }
  put(a, kind) {
    this.kind[a] = kind; this.far[3 * a] = this.far[3 * a + 1] = this.far[3 * a + 2] = -1;
    this.where[a] = this.live; this.ids[this.live++] = a;
  }
  remove(a) {
    this.kind[a] = 0; this.far[3 * a] = this.far[3 * a + 1] = this.far[3 * a + 2] = -1;
    const w = this.where[a], last = this.ids[--this.live];
    this.ids[w] = last; this.where[last] = w;
    this.op.delete(a); this.grp.delete(a);
  }
  /// A new part, to write code into.
  part(name) { const g = this.partNext++; this.parts[g] = { name, src: -1, size: 0 }; return g; }
  /// The part that side `side` of copy `op` makes of part g.
  copyOf(g, op, side) {
    const key = `${g}:${op}:${side}`;
    let c = this.partKey.get(key);
    if (c === undefined || c >= this.partNext) { const from = this.parts[g]; c = this.partNext++; this.parts[c] = { name: from.name, src: from.src >= 0 ? from.src : g, size: 0 }; this.partKey.set(key, c); }
    return c;
  }
  /// Forget the copies of parts made after the moment the net is at: the run is about to go another way.
  forget() { for (const [key, part] of this.partKey) if (part >= this.partNext) this.partKey.delete(key); }
  /// How many agents part g has when it is whole.
  wholeSize(g) { const p = this.parts[g]; return p.src >= 0 ? this.parts[p.src].size : p.size; }
  mate(p) { return this.far[3 * (p >> 2) + (p & 3)]; }
  link(p, q) {
    this.far[3 * (p >> 2) + (p & 3)] = q; this.far[3 * (q >> 2) + (q & 3)] = p;
    if (!(p & 3) && !(q & 3)) {
      const ka = this.kind[p >> 2], kb = this.kind[q >> 2];
      if (ka !== K.root && ka !== K.ph && kb !== K.root && kb !== K.ph) this.queue.push(p >> 2, q >> 2);
    }
  }

  // ---- building code ----
  /// Code for a term, giving its principal port. A term is a bound name, "ρ", [f, a], ["λ" or "Λ", x, body],
  /// or ["~", port], a splice of whatever is at that port. env maps a bound name to the uses collected for it.
  emit(t, env) {
    const name = this.names && typeof t === "object" ? this.names.get(t) : undefined;
    if (name === undefined) return this.code(t, env);
    const outer = this.cur; this.cur = this.part(name);
    const p = this.code(t, env);
    this.cur = outer;
    return p;
  }
  code(t, env) {
    if (t === "ρ") return this.make(K.rho) * 4;
    if (t[0] === "#") return this.number(t);
    if (typeof t === "string") {
      if (!env.has(t)) throw new Error(`nothing binds ${t}`);
      const v = this.make(K.var);
      env.get(t).push(v * 4 + 1);
      return v * 4;
    }
    if (t.length === 2 && t[0] === "~") { const v = this.make(K.var); this.link(v * 4 + 1, t[1]); return v * 4; }
    if (t.length === 3) {
      const lam = this.make(t[0] === "Λ" ? K.Lam : K.lam), occ = [], inner = new Map(env);
      inner.set(t[1], occ);
      this.link(lam * 4 + 2, this.emit(t[2], inner));
      this.bind(lam * 4 + 1, occ);
      return lam * 4;
    }
    const a = this.make(K.app);
    this.link(a * 4 + 1, this.emit(t[0], env));
    this.link(a * 4 + 2, this.emit(t[1], env));
    return a * 4;
  }
  /// The code of a number written as a literal, ["#", form, n, size]. Form "u" is unary, λs z. s (rest s z)
  /// per successor, and "b" a list of `size` bits, λf n. f bit (rest f n) per bit, lowest first, both
  /// around λs z. z s: built cell by cell in a loop, as the nested term would make it, so a large number
  /// needs no deep recursion. The other forms are made from their terms (see TERMS).
  number([, form, n, width]) {
    if (TERMS[form]) return this.code(TERMS[form](n, width), new Map());
    const unary = form === "u";
    const app = (f, x) => { const a = this.make(K.app); this.link(a * 4 + 1, f); this.link(a * 4 + 2, x); return a * 4; };
    const cell = body => {
      const f = this.make(K.lam), z = this.make(K.lam), uses = [[], []];
      const use = of => { const v = this.make(K.var); uses[of].push(v * 4 + 1); return v * 4; };
      this.link(f * 4 + 2, z * 4);
      this.link(z * 4 + 2, body(use));
      this.bind(f * 4 + 1, uses[0]); this.bind(z * 4 + 1, uses[1]);
      return f * 4;
    };
    let top = -1, hole = -1;                        // the whole number, and where its next cell goes
    const place = p => { if (hole < 0) top = p; else this.link(hole, p); };
    for (let i = 0n, cells = unary ? n : BigInt(width); i < cells; i++) {
      let rest;
      place(cell(use => {
        const head = unary ? use(0) : app(use(0), this.emit(BIT[Number((n >> i) & 1n)], new Map()));
        rest = this.make(K.app);
        this.link(rest * 4 + 2, use(0));
        return app(head, app(rest * 4, use(1)));
      }));
      hole = rest * 4 + 1;
    }
    place(cell(use => { const z = use(1); return app(z, use(0)); }));
    return top;
  }
  /// Wire a binder's x to its uses: an eraser for none, a chain of splits for several.
  bind(port, occ) {
    if (!occ.length) { this.link(port, this.make(K.eraI) * 4); return; }
    let i = 0;
    while (occ.length - i > 1) { const s = this.make(K.sig); this.link(port, s * 4); this.link(s * 4 + 1, occ[i++]); port = s * 4 + 2; }
    this.link(port, occ[i]);
  }
  /// Put a term in the net under a thaw, its answer arriving at a root agent.
  load(t) {
    this.root = this.make(K.root);
    const th = this.make(K.T);
    this.link(th * 4, this.emit(t, new Map()));
    this.link(th * 4 + 1, this.root * 4);
    this.mark = this.queue.length;
    return this.root;
  }

  // ---- rewriting ----
  /// Replace the active pair a, b. build(slot) makes what replaces it; slot(agent, i) is the place the
  /// agent's port i was wired to. Placeholders keep wires between a's ports and b's whole.
  rewrite(a, b, build) {
    const ports = [], ph = new Map();
    for (const x of [a, b]) for (let i = 1; i <= ARITY[this.kind[x]]; i++) ports.push(x * 4 + i);
    ports.forEach((p, n) => { const h = this.ph[n]; this.put(h, K.ph); ph.set(p, h); });
    for (const p of ports) {
      const e = this.mate(p);
      if (ph.has(e)) { if (p < e) this.link(ph.get(p) * 4, ph.get(e) * 4); }
      else this.link(ph.get(p) * 4, e);
    }
    this.remove(a); this.remove(b);
    build((x, i) => ph.get(x * 4 + i) * 4 + 1);
    for (const h of ph.values()) {
      const x = this.mate(h * 4), y = this.mate(h * 4 + 1);
      if (x < 0 || y < 0) throw new Error("a rewrite left a port unwired");
      if (x !== h * 4 + 1) this.link(x, y);
      this.remove(h);
    }
  }
  /// The view a read builds: λk1 … k5. arm others… ~parts, the arm chosen by the kind of value.
  view(kind, ports, out) {
    const ks = ARMS.map((_, i) => `k${i + 1}`), arm = ks[ARMS.indexOf(kind)];
    let body = arm;
    for (const k of ks) if (k !== arm) body = [body, k];
    for (const p of PARTS[kind]) body = [body, ["~", ports[p]]];
    for (let i = ks.length - 1; i >= 0; i--) body = ["λ", ks[i], body];
    this.cur = this.part("view of " + VIEW_OF[kind]);
    const top = this.emit(body, new Map());
    this.cur = -1;
    this.link(top, out);
  }
  /// Fire the pair a, b: the family of what happened, or -1 if no rule fits (the pair is stuck).
  fire(a, b) {
    const rule = ruleFor(this.kind[a], this.kind[b]);
    if (!rule) return -1;
    const [x, y] = rule[0] ? [b, a] : [a, b], ky = this.kind[y], made = this.next, part = this.grp.get(y);
    const keep = w => { if (part !== undefined) this.grp.set(w, part); };
    this.last = { kinds: this.kind[x] * 16 + ky, rule: rule[2], made, a, b };
    const fresh = kind => this.make(kind);
    switch (rule[2]) {
      case "run": this.rewrite(x, y, slot => {
        const t = fresh(K.T);
        this.link(slot(x, 1), slot(y, 1)); this.link(t * 4, slot(y, 2)); this.link(t * 4 + 1, slot(x, 2));
      }); break;
      case "open": this.rewrite(x, y, slot => { this.link(slot(x, 1), slot(y, 1)); this.link(slot(x, 2), slot(y, 2)); }); break;
      case "reflect": this.rewrite(x, y, slot => { const r = fresh(K.R); this.link(r * 4, slot(x, 1)); this.link(r * 4 + 1, slot(x, 2)); }); break;
      case "thawApp": this.rewrite(x, y, slot => {
        const o = fresh(K.O), tf = fresh(K.T), ta = fresh(K.T);
        this.link(tf * 4, slot(y, 1)); this.link(tf * 4 + 1, o * 4);
        this.link(ta * 4, slot(y, 2)); this.link(ta * 4 + 1, o * 4 + 1);
        this.link(o * 4 + 2, slot(x, 1));
      }); break;
      case "thawVar": this.rewrite(x, y, slot => { this.link(slot(x, 1), slot(y, 1)); }); break;
      case "thawValue": this.rewrite(x, y, slot => {
        const w = fresh(ky); keep(w);
        this.link(w * 4, slot(x, 1));
        for (let i = 1; i <= ARITY[ky]; i++) this.link(w * 4 + i, slot(y, i));
      }); break;
      case "viewFun": case "viewOpened": this.rewrite(x, y, slot => {
        const f = fresh(rule[2] === "viewFun" ? K.Lam : K.lam); keep(f);
        this.link(f * 4 + 1, slot(y, 1)); this.link(f * 4 + 2, slot(y, 2));
        this.view(rule[2] === "viewFun" ? "fun" : "opened", { F: f * 4 }, slot(x, 1));
      }); break;
      case "viewApp": this.rewrite(x, y, slot => this.view("app", { f: slot(y, 1), a: slot(y, 2) }, slot(x, 1))); break;
      case "viewVar": this.rewrite(x, y, slot => this.view("var", { w: slot(y, 1) }, slot(x, 1))); break;
      case "viewPrim": this.rewrite(x, y, slot => this.view("prim", {}, slot(x, 1))); break;
      case "copy": {
        const op = this.kind[x] === K.sig ? this.ops++ : this.op.get(x);
        this.rewrite(x, y, slot => {
          const x1 = fresh(ky), x2 = fresh(ky);
          if (part !== undefined) { this.grp.set(x1, this.copyOf(part, op, 1)); this.grp.set(x2, this.copyOf(part, op, 2)); }
          this.link(x1 * 4, slot(x, 1)); this.link(x2 * 4, slot(x, 2));
          for (let i = 1; i <= ARITY[ky]; i++) {
            const d = fresh(POL[ky][i] === "i" ? K.dupI : K.dupO);
            this.op.set(d, op);
            this.link(d * 4, slot(y, i)); this.link(d * 4 + 1, x1 * 4 + i); this.link(d * 4 + 2, x2 * 4 + i);
          }
        });
        break;
      }
      case "cancel":
        if (this.op.get(x) !== this.op.get(y)) this.crossed++;
        this.rewrite(x, y, slot => { this.link(slot(x, 1), slot(y, 1)); this.link(slot(x, 2), slot(y, 2)); });
        break;
      case "erase": this.rewrite(x, y, slot => {
        for (let i = 1; i <= ARITY[ky]; i++) this.link(fresh(POL[ky][i] === "i" ? K.eraI : K.eraO) * 4, slot(y, i));
      }); break;
    }
    this.last.count = this.next - made;
    return rule[1];
  }
  /// Fire the pair a, b and count it: the family of what happened, or -1 if no rule fits.
  apply(a, b) {
    const f = this.fire(a, b);
    if (f >= 0) { this.events++; this.counts[f]++; }
    return f;
  }
  /// Fire the next waiting pair that has a rule: the family of what happened, or -1 when none is left.
  step() {
    for (;;) {
      let a, b;
      if (this.order === "rounds") {
        if (this.head >= this.queue.length) return -1;
        if (this.head >= this.mark) { this.mark = this.queue.length; this.round++; }
        a = this.queue[this.head]; b = this.queue[this.head + 1]; this.head += 2;
        if (this.head > 65536) { this.queue = this.queue.slice(this.head); this.mark -= this.head; this.head = 0; }
      } else {
        if (!this.queue.length) return -1;
        if (this.order === "random") {
          const i = 2 * Math.floor(this.rand() * (this.queue.length / 2)), n = this.queue.length;
          a = this.queue[i]; b = this.queue[i + 1]; this.queue[i] = this.queue[n - 2]; this.queue[i + 1] = this.queue[n - 1];
        } else { a = this.queue[this.queue.length - 2]; b = this.queue[this.queue.length - 1]; }
        this.queue.length -= 2;
      }
      if (!this.kind[a] || !this.kind[b] || this.far[3 * a] !== b * 4) continue;
      const f = this.apply(a, b);
      if (f >= 0) return f;
    }
  }
  /// Wait on the pairs there are now, and nothing else: where a plan starts from a net that was not
  /// reached by stepping (a pair fired by hand, a moment gone back to).
  requeue() {
    this.queue = [];
    for (const [a, b] of this.pairs()) this.queue.push(a, b);
    this.head = 0; this.mark = this.queue.length;
  }
  /// Every pair of agents joined by their principal ports: [a, b, has a rule] each.
  pairs() {
    const out = [];
    for (let n = 0; n < this.live; n++) {
      const a = this.ids[n], f = this.far[3 * a];
      if (f < 0 || (f & 3) || (f >> 2) < a) continue;
      const b = f >> 2, ka = this.kind[a], kb = this.kind[b];
      if (ka === K.root || kb === K.root) continue;
      out.push([a, b, !!ruleFor(ka, kb)]);
    }
    return out;
  }

  // ---- keeping a moment, and going back to it ----
  /// The net as it is (not the pairs waiting: `requeue` finds those again).
  save() {
    const n = this.live, ids = this.ids.slice(0, n), kinds = new Uint8Array(n), far = new Int32Array(3 * n);
    for (let i = 0; i < n; i++) { const a = ids[i]; kinds[i] = this.kind[a]; far.set(this.far.subarray(3 * a, 3 * a + 3), 3 * i); }
    const grp = new Int32Array(n);
    for (let i = 0; i < n; i++) grp[i] = this.grp.get(ids[i]) ?? -1;
    return { ids, kinds, far, grp, parts: this.partNext, op: [...this.op], next: this.next, ops: this.ops,
      events: this.events, crossed: this.crossed, counts: this.counts.slice() };
  }
  restore(s) {
    for (let i = 0; i < this.live; i++) this.kind[this.ids[i]] = 0;
    while (this.cap < s.next) this.grow();
    this.live = s.ids.length; this.ids.set(s.ids);
    for (let i = 0; i < this.live; i++) { const a = s.ids[i]; this.kind[a] = s.kinds[i]; this.where[a] = i; this.far.set(s.far.subarray(3 * i, 3 * i + 3), 3 * a); }
    this.op = new Map(s.op); this.next = s.next; this.ops = s.ops;
    this.grp = new Map(); for (let i = 0; i < this.live; i++) if (s.grp[i] >= 0) this.grp.set(s.ids[i], s.grp[i]);
    this.partNext = s.parts;
    this.queue = []; this.head = 0; this.mark = 0;
    this.events = s.events; this.crossed = s.crossed; this.counts = s.counts.slice();
    this.last = null;
  }

  // ---- reading terms off the net ----
  /// The term whose value comes out of a port. Code reads as it was written: binders named x0, x1, … in
  /// the order met, a captured value as ["~", value]. `strict` reads a finished answer, and throws on
  /// anything else. Otherwise the net may be in any state, and what is under way reads as ["@", f, a] a
  /// running application, ["▸", code] a thaw, ["read", v] a reader, ["copy", n, v] one side of copy n
  /// that has still to pass through v, "∅" a variable whose binder was erased, and "?" what is not a
  /// value. A variable whose binder is outside what is read gets a name of its own. `short`, a map from
  /// an agent to a name, has the name stand for that agent and all under it (["name", n, term], the term
  /// read only when `value` asks for it), except where the reading starts. `where`, a map, is filled with the agent of each function read. Past `budget`
  /// agents the rest reads as "…". It walks with a list of what is left to read, not by calling itself,
  /// so a long number is no trouble.
  read(port, { strict = false, budget = 2000000, short = null, where = null } = {}) {
    const names = new Map(), out = [null], todo = [[port, out, 0]];
    let start = true;
    while (todo.length) {
      const [p, into, at] = todo.pop(), a = p >> 2, q = p & 3, k = p < 0 ? 0 : this.kind[a], first = start;
      start = false;
      if (strict && q) throw new Error("not a value");
      if (--budget < 0) { if (strict) throw new Error("too large to read"); into[at] = "…"; continue; }
      if (!q && !first && short?.has(a)) { into[at] = ["name", short.get(a), () => this.read(p, { budget: 6000 })]; continue; }
      if ((k === K.lam || k === K.Lam) && !q) {
        const x = `x${names.size}`, t = [k === K.lam ? "λ" : "Λ", x, null], uses = this.mate(a * 4 + 1);
        names.set(a * 4 + 1, x); where?.set(t, a);
        if (uses >= 0 && this.kind[uses >> 2] === K.dupO && (uses & 3)) names.set((uses >> 2) * 4, x);   // being copied: its uses are past the copier
        into[at] = t; todo.push([this.mate(a * 4 + 2), t, 2]);
      } else if (k === K.app && !q) {
        const t = [null, null];
        into[at] = t; todo.push([this.mate(a * 4 + 2), t, 1], [this.mate(a * 4 + 1), t, 0]);   // the function part is read first
      } else if (k === K.rho && !q) into[at] = "ρ";
      else if (k === K.var && !q) {
        // up the variable's wire to its binder: through splits, and through the two ends of a copy that are about to cancel
        let src = this.mate(a * 4 + 1);
        for (;;) {
          const b = src >> 2, kb = src < 0 ? 0 : this.kind[b], up = kb === K.dupI && (src & 3) ? this.mate(b * 4) : -1;
          if (kb === K.sig && (src & 3)) src = this.mate(b * 4);
          else if (!strict && up >= 0 && this.kind[up >> 2] === K.dupO && !(up & 3)) src = this.mate((up >> 2) * 4 + (src & 3));
          else break;
        }
        const kb = src < 0 ? 0 : this.kind[src >> 2], binder = ((kb === K.lam || kb === K.Lam) && (src & 3) === 1) || (kb === K.dupO && !(src & 3));
        if (names.has(src)) into[at] = names.get(src);
        else if (!strict && binder) { const x = `x${names.size}`; names.set(src, x); into[at] = x; }
        else if (!strict && kb === K.eraO) into[at] = "∅";
        else { const t = ["~", null]; into[at] = t; todo.push([src, t, 1]); }
      } else if (strict) throw new Error("not finished");
      else if (k === K.O && q === 2) { const t = ["@", null, null]; into[at] = t; todo.push([this.mate(a * 4 + 1), t, 2], [this.mate(a * 4), t, 1]); }
      else if ((k === K.T || k === K.R) && q === 1) { const t = [k === K.T ? "▸" : "read", null]; into[at] = t; todo.push([this.mate(a * 4), t, 1]); }
      else if (k === K.dupI && q) { const t = ["copy", this.op.get(a) ?? 0, null]; into[at] = t; todo.push([this.mate(a * 4), t, 2]); }
      else into[at] = "?";
    }
    return out[0];
  }
  /// The answer at the root, if the run has left one there: a term, or null.
  answer() {
    const p = this.mate(this.root * 4);
    if (p < 0 || (p & 3) || !CODE.has(this.kind[p >> 2])) return null;
    try { return this.read(p, { strict: true }); } catch (e) { return null; }
  }
  /// How many agents cannot be reached from the root (none, once a run has finished).
  garbage() {
    const seen = new Set([this.root]), todo = [this.root];
    while (todo.length) {
      const a = todo.pop();
      for (let i = 0; i <= ARITY[this.kind[a]]; i++) { const f = this.far[3 * a + i]; if (f >= 0 && !seen.has(f >> 2)) { seen.add(f >> 2); todo.push(f >> 2); } }
    }
    return this.live - seen.size;
  }
}

// ---- programs ----
/// The definitions every program may use (the same text as PROGRAMS in ../nets/check.py).
const PRELUDE = String.raw`
-- Church numerals and booleans, a fixpoint, and reflective programs reading code
zero := \f x. x
suc := \n f x. f (n f x)
add := \m n f x. m f (n f x)
true := \t e. t
false := \t e. e
Z := \g. (\h. g (\v. h h v)) (\h. g (\v. h h v))
-- Reading a value: ρ c fun opened app var prim. The arm for c's kind runs, handed the other four arms
-- and then the parts. Arms below name what they are handed: o f a v p for the arms, then the parts.
-- is it a function? each arm drops the arms it is handed and the parts
kindOf := \c. ρ c (\o a v p F. true) (\f a v p L. true) (\f o v p g b. false) (\f o a p w. false) (\f o a v. false)
-- count binders in code: open each binder on ρ, recurse into both sides of an application
size := Z (\self c. ρ c
   (\o a v p F. suc (self (F ρ)))
   (\f a v p L. self L)
   (\f o v p g b. add (self g) (self b))
   (\f o a p w. zero)
   (\f o a v. zero))
-- an arm that is never taken: any value will do
no := ρ
-- an error: a piece of code that is not a function, applied
err := (^d. d) ρ ρ
-- reading a function opens it; reading the opened function closes it again
close := \F. ρ F no (\f a v p L. L) no no no
open := \L. ρ L (\o a v p F. F) no no no no
-- An interpreter for the calculus. Functions, opened functions, spliced values and ρ are values already.
-- An application is interpreted: a function is opened on the argument and its body is interpreted, an
-- opened function hands back its body as code, ρ reads. Applying anything else is an error (err is stuck).
interp := Z (\self c. ρ c
   (\o a v p F. close F)
   (\f a v p L. open L)
   (\f o v p g b. (\x y. ρ x
        (\o a v p F. self (F y))
        (\f a v p L. open L y)
        (\f o v p g b. err)
        (\f o a p w. err)
        (\f o a v. ρ y)) (self g) (self b))
   (\f o a p w. w)
   (\f o a v. ρ))
-- The adds. Each is named for the kind of program it is, the shape of its numbers, and the way it adds.
--   linear      every variable is used, so nothing is erased, and the only copies are the ones the
--               program asks for by using a variable twice
--   ordinary    the usual way: a fixpoint or a test, and what is not picked is dropped
--   reversible  linear, and the answer holds the first number too, so the second can be had back:
--               a subtract undoes the add
-- Numbers are unary, a list of bits, or a tree of bits. A tree is added one bit after another, or in
-- parallel.
--
-- Unary. A linear number is its own loop: each successor copies the step and passes it on, and zero
-- hands it back. 3u is written for lsuc (lsuc (lsuc lzero)).
lzero := \s z. z s
lsuc := \n s z. s (n s z)
-- linear_unary_add m n: the sum, and the step handed back
linear_unary_add := \m n. m (\k n. k (lsuc n)) (\s n k. k n s) n
-- The usual way: a number says whether it is zero or the successor of another, and a fixpoint goes round
-- once for each successor of m. 3s is written for \z s. s (\z s. s (\z s. s (\z s. z))).
ordinary_unary_add := Z (\self m. m (\n. n) (\p n. self p (\z s. s n)))
-- A bit hands two things on: in order, or swapped.
b0 := \p q k. k p q
b1 := \p q k. k q p
-- Lists of bits. A linear one is a loop over its bits, lowest first: each cell copies the step and hands
-- it the bit, and the end hands the step back. 5 is written for cell b1 (cell b0 (cell b1 (cell b0 wend))):
-- every list of bits in a program has one bit more than the largest needs, so a sum of two fits.
wend := \f n. n f
cell := \x w f n. f x (w f n)
-- a chain of pairs: what a number's loop gives when it is run with pair
pair := \x r k. k x r
-- One bit of the add: x is a's bit, k the rest of a's loop, b the rest of the other number as a
-- chain, c the carry. Three compare-and-swaps put the three bits in order (x x y uses x up, and its
-- copy takes its place). Then the middle one swaps the outer two: the sum bit and the next carry.
addbit := \x k b c ret. b (\y rb. x x y (\p q. q q c (\m hi. p p m (\lo mid. mid hi lo (\s cn. k rb cn (\rs. ret (cell s rs)))))))
addend := \f b c ret. ret wend b c f
-- linear_list_add a b: the sum, the carry out, and what is handed back: the end of b's chain and the step
linear_list_add := \a b. a addbit addend (b pair (\p. p)) b0 (\s rest c f k. k s c rest f)
-- Cheaper, with two shapes: a's bits are the cells of its loop (each cell copies the arm for its bit,
-- add0 or add1; 5a is written for w1 (w0 (w1 (w0 (\o i n. n o i))))), and b is a chain of pairs (5p).
-- With a's bit known, one compare-and-swap and one more swap do. The answer holds a again, then the sum
-- as a chain, which cannot be the a of another add, the carry out, and the two arms.
w0 := \w o i n. o (w o i n)
w1 := \w o i n. i (w o i n)
add0 := \k b c ret. b (\y rb. y y c (\lo hi. lo hi b0 (\s cn. k rb cn (\ra rs. ret (w0 ra) (pair s rs)))))
add1 := \k b c ret. b (\y rb. y y c (\lo hi. hi b1 lo (\s cn. k rb cn (\ra rs. ret (w1 ra) (pair s rs)))))
linear_list_add_two_shapes := \a b. a add0 add1 (\o i b c ret. ret (\o i n. n o i) b c o i) b b0 (\a s c o i k. k a s c o i)
-- The usual way: a list says whether it is its end or a bit and a rest, the bits are true and false,
-- and a fixpoint goes down both lists. 5c is written for such a list of the bits of 5.
bool_xor := \x y. x (y false true) y
bool_maj := \x y c. x (y true c) (y c false)
ordinary_list_add_carry := Z (\self a b c. a (\n c. n) (\x ra. b (\n c. n) (\y rb. (\s r n c. c s r) (bool_xor (bool_xor x y) c) (self ra rb (bool_maj x y c)))))
ordinary_list_add := \a b. ordinary_list_add_carry a b false
-- Trees of bits. A tree number is a pair: its bits as a tree, and its depth as a loop. The tree is pairs
-- of pairs with a bit at each leaf, the low half first; the loop runs a step once for each level. 5t is
-- written for pair (pair (pair b1 b0) (pair b1 b0)) (\s z. s (s z)): every tree number in a program has
-- the same depth, the least whose leaves hold one bit more than the largest needs.
-- Three bits added: the sum bit and the carry. The three compare-and-swaps of addbit, then the swap.
add3 := \x y c ret. x x y (\p q. q q c (\m hi. p p m (\lo mid. mid hi lo ret)))
-- An adder for trees twice as large, from one for the halves: the low halves first, then the high
-- halves with the carry. It copies the adder it is given, and that is its only copy.
double := \f a b c ret. a (\alo ahi. b (\blo bhi. f alo blo c (\slo c1. f ahi bhi c1 (\shi c2. ret (pair slo shi) c2))))
-- linear_tree_add a b: the sum and the carry out, one bit after another. a's depth doubles add3 once
-- for each level, and the sum takes b's depth. A linear program cannot ask a tree whether it is a
-- leaf: the arm for the other answer would be left over. So the depth says where the leaves are.
linear_tree_add := \a b. a (\ta la. b (\tb lb. la double add3 ta tb b0 (\s c. pair (pair s lb) c)))
-- The usual way, on the same trees: a bit is tested and the arm not taken is dropped, a bit used twice
-- is copied, a's depth is used twice and b's is dropped.
pick := \x zero one. x zero one (\p q. p)
oxor := \x y. pick x y (pick y b1 b0)
omaj := \x y c. pick x (pick y b0 c) (pick y c b1)
oadd3 := \x y c. pair (oxor (oxor x y) c) (omaj x y c)
odouble := \f a b c. a (\alo ahi. b (\blo bhi. f alo blo c (\slo c1. f ahi bhi c1 (\shi c2. pair (pair slo shi) c2))))
ordinary_tree_add := \a b. a (\ta la. b (\tb lb. la odouble oadd3 ta tb b0 (\s c. pair (pair s la) c)))
-- In parallel, on the same trees. linear_tree_add runs one bit after another, since each waits for the
-- carry before it. Here every part of a tree first works out its carry out for a carry in of 0 and for
-- one of 1 (g and h; g is never more than h). A leaf has them from its two bits, sorted. A pair has them
-- from its halves: the low half's answer picks one of the high half's two. None of that waits for a
-- carry, so all the parts work at once. Then the carry goes down: a pair hands it to its low half and
-- picks the low half's answer for its high half, and a leaf flips its sum bit by it.
-- forget v x: x, with the bit v used up: v swaps two things that are the same, so nothing is erased
forget := \v x. v (\z. z) (\z. z) (\a b. a (b x))
-- a leaf, once its two bits are sorted: g, h, and the sum bit for a carry in c. Each of g, h and c that
-- is 1 swaps b0 and b1, so b1 comes out first when an odd number of them are
pleaf_sorted := \g h k. k g h (\c. g b0 b1 (\u v. h u v (\w z. c w z (\s t. forget t s))))
pleaf := \x y. x x y pleaf_sorted
-- two halves joined. A bit picks one of two by swapping them, and the one not picked is forgotten
join := \lo hi. lo (\gl hl sl. hi (\gh hh sh k. k (gl gh hh (\u v. forget v u)) (hl gh hh (\u v. forget v u))
   (\c. c gl hl (\ch v. forget v (pair (sl c) (sh ch))))))
pdouble := \f a b. a (\alo ahi. b (\blo bhi. join (f alo blo) (f ahi bhi)))
-- linear_tree_add_parallel a b: the sum and the carry out, as linear_tree_add gives them
linear_tree_add_parallel := \a b. a (\ta la. b (\tb lb. la pdouble pleaf ta tb (\g h s. forget h (pair (pair (s b0) lb) g))))
-- the same the usual way: the bit not picked is dropped
opleaf := \x y. (\g h k. k g h (\c. oxor (oxor g h) c)) (pick x b0 y) (pick x y b1)
ojoin := \lo hi. lo (\gl hl sl. hi (\gh hh sh k. k (pick gl gh hh) (pick hl gh hh) (\c. pair (sl c) (sh (pick c gl hl)))))
opdouble := \f a b. a (\alo ahi. b (\blo bhi. ojoin (f alo blo) (f ahi bhi)))
ordinary_tree_add_parallel := \a b. a (\ta la. b (\tb lb. la opdouble opleaf ta tb (\g h s. pair (pair (s b0) la) g)))
-- Reversible, on the same trees. An add alone cannot be undone: 1 + 2 and 2 + 1 give the same sum.
-- reversible_tree_add a b hands back a and the sum, in as many bits as the numbers have: a is used
-- twice, so it is copied once, and the carry out is forgotten (the sum and a say what it was).
-- reversible_tree_sub a s hands back a and s - a, so each undoes the other:
-- reversible_tree_add a b reversible_tree_sub is the pair of a and b again.
reversible_tree_add := \a b. a (\ta la. b (\tb lb. la double add3 ta tb b0 (\s c. forget c (pair a (pair s lb)))))
-- s - a is s + (a with every bit flipped) + 1. A bit that swaps b1 and b0 hands on its flip and itself
-- (nx and x2), with no copy: so the first compare-and-swap of add3, on the flipped bit, is x2 y nx.
sub3 := \x y c ret. x b1 b0 (\nx x2. x2 y nx (\p q. q q c (\m hi. p p m (\lo mid. mid hi lo ret))))
reversible_tree_sub := \a s. a (\ta la. s (\ts ls. la double sub3 ta ts b1 (\d c. forget c (pair a (pair d ls)))))
-- the same pair in parallel
reversible_tree_add_parallel := \a b. a (\ta la. b (\tb lb. la pdouble pleaf ta tb (\g h s. forget g (forget h (pair a (pair (s b0) lb))))))
pleaf_sub := \x y. x b1 b0 (\nx x2. x2 y nx pleaf_sorted)
reversible_tree_sub_parallel := \a s. a (\ta la. s (\ts ls. la pdouble pleaf_sub ta ts (\g h d. forget g (forget h (pair a (pair (d b1) ls))))))
`;

/// The adds the prelude defines, for the page's list of them: by the shape of the numbers and the way
/// they are added, and in each group by the kind of program. A row is the program to run, its kind, what
/// to say of it, and its rewrites and rounds (check.mjs holds them to a run).
const ADDS = [
  ["Unary numbers: 40 + 2", [
    ["linear_unary_add 40u 2u", "linear", "A number is its own loop: one copy for each successor, and the step is handed back.", 1745, 299],
    ["ordinary_unary_add 40s 2s", "ordinary", "A fixpoint goes round once for each successor, and each round drops the arm it does not take.", 3110, 1002],
  ]],
  ["Lists of bits: 1014 + 2413", [
    ["linear_list_add 1014 2413", "linear", "Each bit sorts three bits with swaps. Five copies a bit, and the carry and the step are handed back.", 3792, 1093],
    ["linear_list_add_two_shapes 1014a 2413p", "linear", "Cheaper: each cell of the first number picks the step for its bit. It hands the first number back, and the sum is a chain of pairs.", 2739, 745],
    ["ordinary_list_add 1014c 2413c", "ordinary", "A fixpoint goes down both lists, testing each bit and dropping the arm not taken.", 5904, 632],
  ]],
  ["Trees of bits, one bit after another: 1014 + 2413", [
    ["linear_tree_add 1014t 2413t", "linear", "The adder for a tree is two adders for its halves, and the carry goes from one to the next.", 4636, 1734],
    ["ordinary_tree_add 1014t 2413t", "ordinary", "The same adder with each bit tested and the arm not taken dropped.", 13240, 1056],
    ["reversible_tree_add 1014t 2413t", "reversible", "Hands back 1014 and the sum, so the add can be undone.", 4947, 1751],
    ["reversible_tree_sub 1014t 3427t", "reversible", "The subtract that undoes it: hands back 1014 and 3427 − 1014.", 5511, 1991],
    ["reversible_tree_add 1014t 2413t reversible_tree_sub", "reversible", "There and back: the add, then the subtract on its answer. The two numbers come out again.", 10461, 3744],
  ]],
  ["Trees of bits, in parallel: 1014 + 2413", [
    ["linear_tree_add_parallel 1014t 2413t", "linear", "Every part of a tree works out its carry out for both carries in at once. Then the carry goes down.", 11653, 436],
    ["ordinary_tree_add_parallel 1014t 2413t", "ordinary", "The same with each bit tested and the one not picked dropped.", 15717, 346],
    ["reversible_tree_add_parallel 1014t 2413t", "reversible", "Hands back 1014 and the sum.", 11964, 436],
    ["reversible_tree_sub_parallel 1014t 3427t", "reversible", "Hands back 1014 and 3427 − 1014.", 12528, 450],
    ["reversible_tree_add_parallel 1014t 2413t reversible_tree_sub_parallel", "reversible", "There and back, in parallel.", 24495, 810],
  ]],
];

/// The two bits: b0 hands two things on in order, b1 swapped.
const BIT = [["λ", "p", ["λ", "q", ["λ", "k", [["k", "p"], "q"]]]], ["λ", "p", ["λ", "q", ["λ", "k", [["k", "q"], "p"]]]]];
const TOKEN = /\\|λ|\^|Λ|\.|\(|\)|[A-Za-z_][A-Za-z0-9_']*|\d+[utscap]?|ρ/g;
const bitLength = n => n ? n.toString(2).length : 0;
/// The term of the tree number n: λk. k bits loop. The bits are a tree of pairs λk. k low high `depth`
/// levels deep with a bit at each leaf, the lowest bit leftmost; the loop is the depth, λs z. s (… (s z))
/// with one s for each level.
function treeTerm(n, depth) {
  const bits = (lo, d) => d ? ["λ", "k", [["k", bits(lo, d - 1)], bits(lo + (1n << BigInt(d - 1)), d - 1)]] : BIT[Number((n >> lo) & 1n)];
  let loop = "z";
  for (let i = 0; i < depth; i++) loop = ["s", loop];
  return ["λ", "k", [["k", bits(0n, depth)], ["λ", "s", ["λ", "z", loop]]]];
}
/// The numbers of the ordinary programs, and of the add whose two numbers have different shapes. A list
/// has `width` bits, lowest first.
const TRUE = ["λ", "t", ["λ", "e", "t"]], FALSE = ["λ", "t", ["λ", "e", "e"]];
const bitAt = (n, i) => Number((n >> BigInt(i)) & 1n);
const TERMS = {
  t: treeTerm,
  /// unary, the usual way: λz s. z for zero, λz s. s rest for a successor
  s(n) { let t = ["λ", "z", ["λ", "s", "z"]]; for (let i = 0n; i < n; i++) t = ["λ", "z", ["λ", "s", ["s", t]]]; return t; },
  /// a list of bits, the usual way: λn c. n for the end, λn c. c bit rest for a cell, with true and false
  c(n, width) { let t = ["λ", "n", ["λ", "c", "n"]]; for (let i = width - 1; i >= 0; i--) t = ["λ", "n", ["λ", "c", [["c", bitAt(n, i) ? TRUE : FALSE], t]]]; return t; },
  /// a number whose cells pick an arm by their bit: λo i n. o (rest o i n), or i
  a(n, width) {
    let t = ["λ", "o", ["λ", "i", ["λ", "n", [["n", "o"], "i"]]]];
    for (let i = width - 1; i >= 0; i--) t = ["λ", "o", ["λ", "i", ["λ", "n", [bitAt(n, i) ? "i" : "o", [[[t, "o"], "i"], "n"]]]]];
    return t;
  },
  /// a chain of pairs: λk. k bit rest, ended by λk. k
  p(n, width) { let t = ["λ", "k", "k"]; for (let i = width - 1; i >= 0; i--) t = ["λ", "k", [["k", BIT[bitAt(n, i)]], t]]; return t; },
};
/// The forms that are lists of bits: every one in a program has the same width.
const LISTS = "bcap";

/// A program's term. \x y. body is a function, ^x y. body an opened one; juxtaposition applies; ρ (or
/// rho) reads; `name := term` lines define names for the lines after them; -- starts a comment. A name
/// nothing binds or defines is an error. A number is written with its form after its digits. The linear
/// ones: 5u is unary, 5 a list of bits and 5t a tree of bits. The ordinary ones: 5s is unary and 5c a
/// list of bits. And for the add whose two numbers have different shapes: 5a is a number whose cells
/// pick an arm, 5p a chain of pairs. Every list of bits in a program is written with one bit more than
/// the largest needs, so a sum of two fits, and every tree number with the fewest levels that hold as much.
/// `names`, if given, is filled with the name of each defined term and of each number.
function parse(src, names) {
  const defs = new Map(), lines = [];
  for (const raw of src.split("\n")) {
    const l = raw.split("--")[0];
    if (!l.trim()) continue;
    if (l.includes(":=") || !lines.length || !/^\s/.test(l)) lines.push(l); else lines[lines.length - 1] += " " + l;
  }
  const chunks = lines.map(ch => {
    const at = ch.indexOf(":="), name = at < 0 ? null : ch.slice(0, at).trim(), body = at < 0 ? ch : ch.slice(at + 2);
    const bad = body.replace(TOKEN, "").replace(/\s+/g, "");
    if (bad) throw new Error(`cannot read "${bad[0]}"`);
    return [name, body.match(TOKEN) ?? []];
  });
  const formOf = tok => /\d$/.test(tok) ? "b" : tok.slice(-1), digits = tok => BigInt(tok.replace(/\D$/, ""));
  let largest = 0n, largestTree = 0n;
  for (const [, toks] of chunks) for (const t of toks) {
    if (!/^\d/.test(t)) continue;
    if (LISTS.includes(formOf(t)) && digits(t) > largest) largest = digits(t);
    if (formOf(t) === "t" && digits(t) > largestTree) largestTree = digits(t);
  }
  // 2 ** depth leaves: one more than the largest tree number needs, at least
  const width = bitLength(largest) + 1, depth = Math.max(1, bitLength(BigInt(bitLength(largestTree))));
  const number = tok => {
    const form = formOf(tok), n = digits(tok);
    if ((form === "u" && n > 200000n) || (form === "s" && n > 600n) || (form === "t" && depth > 14) || ("cap".includes(form) && width > 256))
      throw new Error(`${tok} is too large to write out`);
    const t = ["#", form, n, form === "t" ? depth : width];
    names?.set(t, tok);
    return t;
  };
  names?.set(BIT[0], "b0").set(BIT[1], "b1").set(TRUE, "true").set(FALSE, "false");   // the bits of a written number are parts too
  let out = null;
  for (const [name, raw] of chunks) {
    if (!raw.length) throw new Error(name ? `${name} has no body` : "nothing to run");
    const toks = raw.map(t => /^\d/.test(t) ? number(t) : t);
    const [t, i] = expr(toks, 0, defs, []);
    if (i !== toks.length) throw new Error(`an unmatched ${toks[i]}`);
    if (name) { defs.set(name, t); if (names && typeof t === "object" && t[0] !== "#") names.set(t, name); } else out = t;
  }
  if (out === null) throw new Error("nothing to run");
  return out;
}
function expr(toks, i, defs, bound) {
  const isBinder = t => t === "\\" || t === "λ" || t === "^" || t === "Λ";
  if (isBinder(toks[i])) {
    const binder = toks[i] === "\\" || toks[i] === "λ" ? "λ" : "Λ", xs = [];
    for (i++; toks[i] !== "."; i++) {
      if (i >= toks.length || typeof toks[i] !== "string" || !/^[A-Za-z_]/.test(toks[i])) throw new Error("a binder needs names, then a dot");
      xs.push(toks[i]);
    }
    if (!xs.length) throw new Error("a binder needs a name");
    let [body, j] = expr(toks, i + 1, defs, bound.concat(xs));
    for (let k = xs.length - 1; k >= 0; k--) body = [binder, xs[k], body];
    return [body, j];
  }
  let head = null;
  while (i < toks.length && toks[i] !== ")") {
    let a;
    if (isBinder(toks[i])) [a, i] = expr(toks, i, defs, bound);
    else if (toks[i] === "(") {
      [a, i] = expr(toks, i + 1, defs, bound);
      if (toks[i] !== ")") throw new Error("a ( is never closed");
      i++;
    } else if (toks[i] === ".") throw new Error("a dot with no binder");
    else {
      const t = toks[i++];
      if (typeof t !== "string" || t === "ρ") a = t; else if (t === "rho") a = "ρ"; else if (bound.includes(t)) a = t; else if (defs.has(t)) a = defs.get(t);
      else throw new Error(`nothing binds ${t}`);
    }
    head = head === null ? a : [head, a];
  }
  if (head === null) throw new Error("an empty ( )");
  return [head, i];
}

/// A term without the captured-value marks round it, and what a name stands for.
function value(t) {
  for (;;) {
    if (!Array.isArray(t)) return t;
    if (t.length === 2 && t[0] === "~") t = t[1];
    else if (t[0] === "name") { if (typeof t[2] === "function") t[2] = t[2](); t = t[2]; }
    else return t;
  }
}
/// The n binders a value opens with, and the body under them: [names, body], or null.
function binders(t, n) {
  const xs = [];
  for (let i = 0; i < n; i++) {
    t = value(t);
    if (!Array.isArray(t) || t.length !== 3 || t[0] !== "λ") return null;
    xs.push(t[1]); t = t[2];
  }
  return [xs, t];
}
/// The marks `read` puts on what is not plain code.
const MARKS = new Set(["~", "▸", "read", "name"]);
const isApp = t => Array.isArray(t) && t.length === 2 && !MARKS.has(t[0]);
/// 0 or 1 for a value that is b0 or b1, else -1.
function bitOf(t) {
  const got = binders(t, 3);
  if (!got) return -1;
  const [[p, q, k], b] = got;
  if (!isApp(b) || !isApp(b[0]) || b[0][0] !== k) return -1;
  return b[0][1] === p && b[1] === q ? 0 : b[0][1] === q && b[1] === p ? 1 : -1;
}
/// The two halves of a value that is a pair λk. k low high, or null.
function halves(t) {
  const got = binders(t, 1);
  if (!got) return null;
  const [[k], b] = got;
  return isApp(b) && isApp(b[0]) && b[0][0] === k ? [b[0][1], b[1]] : null;
}
/// [n, depth] for a value that is a tree of bits: a bit, or a pair of two trees of one depth. Else null.
function treeOf(t) {
  let depth = 0;
  for (let u = t; bitOf(u) < 0; depth++) { const h = depth < 30 && halves(u); if (!h) return null; u = h[0]; }
  const bits = (t, d) => {                                    // the number in a tree exactly d levels deep
    if (!d) { const b = bitOf(t); return b < 0 ? null : BigInt(b); }
    const h = halves(t), lo = h && bits(h[0], d - 1), hi = lo === null || !h ? null : bits(h[1], d - 1);
    return hi === null ? null : lo | hi << (1n << BigInt(d - 1));
  };
  const n = bits(t, depth);
  return n === null ? null : [n, depth];
}
/// How many times a value λs z. s (… (s z)) runs its step, if it is one; else 0.
function loopOf(t) {
  const got = binders(t, 2);
  if (!got) return 0;
  let [[s, z], body] = got, times = 0;
  while (isApp(body) && body[0] === s) { times++; body = body[1]; }
  return body === z ? times : 0;
}
/// 1 or 0 for a value that is true or false (λt e. t, λt e. e), else -1.
function boolOf(t) {
  const got = binders(t, 2);
  return !got ? -1 : got[1] === got[0][0] ? 1 : got[1] === got[0][1] ? 0 : -1;
}
/// The numbers that are cells in a row, each form with what tells its end and what reads a cell: a cell
/// gives its bit (or 1, for a unary form) and the rest, or null. A number with no cells is not taken for
/// one: its end is much else too (λz s. z is also true).
const ROWS = [
  // unary, the usual way: λz s. s rest, down to λz s. z
  { end: "s", unary: true, binders: 2, isEnd: ([z], b) => b === z, cell: ([, s], b) => isApp(b) && b[0] === s ? [1, b[1]] : null },
  // a list of bits, the usual way: λn c. c bit rest, down to λn c. n
  { end: "c", binders: 2, isEnd: ([n], b) => b === n,
    cell: ([, c], b) => isApp(b) && isApp(b[0]) && b[0][0] === c && boolOf(b[0][1]) >= 0 ? [boolOf(b[0][1]), b[1]] : null },
  // cells that pick an arm: λo i n. o (rest o i n), or i, down to λo i n. n o i
  { end: "a", binders: 3, isEnd: ([o, i, n], b) => isApp(b) && isApp(b[0]) && b[0][0] === n && b[0][1] === o && b[1] === i,
    cell: ([o, i, n], b) => {
      const r = isApp(b) && (b[0] === o || b[0] === i) ? b[1] : null;
      return r && isApp(r) && r[1] === n && isApp(r[0]) && r[0][1] === i && isApp(r[0][0]) && r[0][0][1] === o ? [b[0] === i ? 1 : 0, r[0][0][0]] : null;
    } },
  // a chain of pairs: λk. k bit rest, down to λk. k
  { end: "p", binders: 1, isEnd: ([k], b) => b === k,
    cell: ([k], b) => isApp(b) && isApp(b[0]) && b[0][0] === k && bitOf(b[0][1]) >= 0 ? [bitOf(b[0][1]), b[1]] : null },
];
/// A value that is a number, as it is written: "5u" for a linear unary one, "5" for a linear list of bits,
/// "5t" for a linear tree one, "5s" and "5c" for the ordinary unary one and list of bits, "5a" and "5p"
/// for cells that pick an arm and a chain of pairs; else null. The number with no cells is not taken for
/// one: it is λa b. b a, which is much else too.
function numberOf(t) {
  const h = halves(t), bits = h && treeOf(h[0]);
  if (bits && bits[1] && loopOf(h[1]) === bits[1]) return `${bits[0]}t`;
  rows: for (const row of ROWS) {
    let n = 0n, cells = 0n;
    for (let u = t; ; cells++) {
      const got = binders(u, row.binders);
      if (!got) continue rows;
      if (row.isEnd(got[0], got[1])) break;
      const cell = row.cell(got[0], got[1]);
      if (!cell) continue rows;
      n = row.unary ? n + 1n : n | BigInt(cell[0]) << cells;
      u = cell[1];
    }
    if (cells) return `${n}${row.end}`;
  }
  let n = 0n, width = 0n, successors = 0n;
  for (;;) {
    const got = binders(t, 2);
    if (!got) return null;
    const [[f, z], body] = got;
    if (!isApp(body)) return null;
    if (body[0] === z && body[1] === f) break;
    const handed = body[1];                                   // (rest f) z
    if (!isApp(handed) || handed[1] !== z || !isApp(handed[0]) || handed[0][1] !== f) return null;
    if (body[0] === f) successors++;
    else if (isApp(body[0]) && body[0][0] === f && bitOf(body[0][1]) >= 0) { n |= BigInt(bitOf(body[0][1])) << width; width++; }
    else return null;
    t = handed[0][0];
  }
  if ((successors && width) || !(successors || width)) return null;
  return width ? String(n) : `${successors}u`;
}

/// A tree of bits with four leaves or more and no depth beside it, as text: {n|leaves}. Else null.
function bareTree(t) {
  const bits = treeOf(t);
  return bits && bits[1] > 1 ? `{${bits[0]}|${1 << bits[1]}}` : null;
}
/// The numbers in a term that `read` made with `where`: a map from the agent at the top of each number,
/// bare tree or bit to its text ("b0" and "b1" for the bits). Only the outermost number is marked, and
/// the bits in it.
function marksOf(term, where) {
  const marks = new Map(), todo = [[term, false]];
  while (todo.length) {
    let [t, inside] = todo.pop();
    while (Array.isArray(t) && t.length === 2 && t[0] === "~") t = t[1];
    if (!Array.isArray(t) || t[0] === "name") continue;       // a name's agents are folded away, or tagged with the name
    if (t.length === 3 && (t[0] === "λ" || t[0] === "Λ")) {
      const bit = bitOf(t), number = bit >= 0 ? `b${bit}` : inside ? null : numberOf(t) ?? bareTree(t);
      if (number !== null) marks.set(where.get(t), number);
      if (bit < 0) todo.push([t[2], inside || number !== null]);
    } else if (isApp(t)) todo.push([t[0], inside], [t[1], inside]);
    else for (let i = 1; i < t.length; i++) todo.push([t[i], inside]);
  }
  return marks;
}

/// A term as text: λa b. body, Λ for an opened function, ‹v› for a captured value. A number is shown
/// as it is written, a bit as b0 or b1, a tree of bits with no depth beside it as {n|leaves}, and a
/// name `read` was given as that name. What is under way: ▸c a thaw on code c, f @ a an application
/// that is running, ρ⟨v⟩ a read, ⧉n⟨v⟩ a side of copy n.
function show(t) {
  const letters = new Map();
  const nameOf = x => { if (!letters.has(x)) { const n = letters.size; letters.set(x, "abcdefghijklmnopqrstuvwxyz"[n % 26] + (n >= 26 ? Math.floor(n / 26) : "")); } return letters.get(x); };
  const isLam = t => Array.isArray(t) && t.length === 3 && (t[0] === "λ" || t[0] === "Λ");
  const isRun = t => Array.isArray(t) && t.length === 3 && t[0] === "@";
  const written = t => numberOf(t) !== null || bitOf(t) >= 0 || bareTree(t) !== null;
  const inner = t => (isLam(t) || isApp(t) || isRun(t)) && !written(t) ? `(${go(t)})` : go(t);
  const go = t => {
    if (typeof t === "string") return t === "ρ" || t === "∅" || t === "…" || t === "?" ? t : nameOf(t);
    if (t[0] === "name") return t[1];
    const number = numberOf(t) ?? (bitOf(t) >= 0 ? `b${bitOf(t)}` : bareTree(t));
    if (number !== null) return number;
    if (t[0] === "~") return `‹${go(t[1])}›`;
    if (t[0] === "▸") return `▸${inner(t[1])}`;
    if (t[0] === "read") return `ρ⟨${go(t[1])}⟩`;
    if (t[0] === "copy") return `⧉${t[1]}⟨${go(t[2])}⟩`;
    if (isLam(t)) { const xs = [nameOf(t[1])]; let b = t[2]; while (isLam(b) && b[0] === t[0] && !written(b)) { xs.push(nameOf(b[1])); b = b[2]; } return `${t[0]}${xs.join(" ")}. ${go(b)}`; }
    const run = isRun(t), f = run ? t[1] : t[0], a = run ? t[2] : t[1];
    return `${isLam(f) && !written(f) ? `(${go(f)})` : go(f)}${run ? " @ " : " "}${inner(a)}`;
  };
  return go(t);
}

/// A whole run of a program, kept so any moment can be shown: which pair each rewrite fired and what
/// it did, where each round began, and the net saved every so often (thinner as the run gets long).
/// The run is planned ahead in the chosen order; a pair fired by hand (`fireNow`) replaces what was
/// planned from that moment on, and the rest is planned again from the net it leaves.
class Run {
  constructor(src, { order = "rounds", seed = 1, limit = 400000, prelude = PRELUDE } = {}) {
    const names = new Map();
    this.term = parse(prelude + "\n" + src, names);
    this.net = new Net(order, seed);
    this.net.names = names;
    this.net.load(this.term);
    this.order = order; this.limit = limit;
    const cap = 1024;
    this.fam = new Uint8Array(cap); this.kinds = new Uint8Array(cap); this.hand = new Uint8Array(cap);
    this.pa = new Int32Array(cap); this.pb = new Int32Array(cap);            // the pair each rewrite fired
    this.madeAt = new Int32Array(cap); this.madeN = new Uint16Array(cap);      // the agents it made: madeN of them, from id madeAt
    this.rounds = [0];
    this.saved = [{ t: 0, s: this.net.save() }]; this.gap = 16;
    this.n = 0; this.at = 0; this.byHand = 0;
    this.plan();
  }
  /// Note the rewrite the net just did as the next one of the run.
  note(hand) {
    if (this.n === this.fam.length) {
      const g = a => { const b = new a.constructor(a.length * 2); b.set(a); return b; };
      for (const k of ["fam", "kinds", "hand", "pa", "pb", "madeAt", "madeN"]) this[k] = g(this[k]);
    }
    const l = this.net.last;
    this.fam[this.n] = ruleFor(l.kinds >> 4, l.kinds & 15)[1]; this.kinds[this.n] = l.kinds; this.hand[this.n] = hand ? 1 : 0;
    this.pa[this.n] = l.a; this.pb[this.n] = l.b; this.madeAt[this.n] = l.made; this.madeN[this.n] = l.count; this.n++;
    if (this.n % this.gap === 0) {
      this.saved.push({ t: this.n, s: this.net.save() });
      if (this.saved.length > 400) { this.saved = this.saved.filter((_, i) => i % 2 === 0); this.gap *= 2; }
    }
  }
  /// Run on from the net as it is, which is the run's last moment so far, to the end (or the limit).
  plan() {
    if (this.rounds[this.rounds.length - 1] !== this.n) this.rounds.push(this.n);
    let round = this.net.round;
    this.status = null;
    for (;;) {
      if (this.n >= this.limit) { this.status = "limit"; break; }
      if (this.net.step() < 0) break;
      if (this.net.round !== round) { round = this.net.round; if (this.rounds[this.rounds.length - 1] !== this.n) this.rounds.push(this.n); }
      this.note(false);
    }
    if (this.rounds[this.rounds.length - 1] !== this.n) this.rounds.push(this.n);
    const stuck = this.net.pairs().filter(p => !p[2]).length;
    this.status ??= stuck ? "stuck" : "done";
    this.stuckPairs = stuck;
    const ans = this.status === "limit" ? null : this.net.answer();
    this.answer = ans; this.answerText = ans ? show(ans) : "";
    this.garbage = this.status === "done" ? this.net.garbage() : 0;
    this.crossed = this.net.crossed;
    this.at = this.n;
  }
  /// Show the net as it was after the first t rewrites.
  seek(t) {
    t = Math.max(0, Math.min(this.n, t));
    if (t < this.at || t - this.at > this.gap) {
      let lo = 0, hi = this.saved.length - 1;
      while (lo < hi) { const m = (lo + hi + 1) >> 1; if (this.saved[m].t <= t) lo = m; else hi = m - 1; }
      if (t < this.at || this.saved[lo].t > this.at) { this.net.restore(this.saved[lo].s); this.at = this.saved[lo].t; }
    }
    while (this.at < t) { this.net.apply(this.pa[this.at], this.pb[this.at]); this.at++; }
    this.net.queue.length = 0; this.net.head = this.net.mark = 0;
  }
  /// Whether a, b is a pair that can fire at the moment on show.
  canFire(a, b) { const N = this.net; return !!N.kind[a] && !!N.kind[b] && N.far[3 * a] === b * 4 && !!ruleFor(N.kind[a], N.kind[b]); }
  /// Drop what the run has after the moment on show.
  cut() {
    const t = this.at;
    this.n = t;
    this.rounds = this.rounds.filter(r => r < t); if (!this.rounds.length || t > 0) this.rounds.push(t);
    this.saved = this.saved.filter(s => s.t <= t);
    this.net.forget();
  }
  /// Fire the pair a, b now, at the moment on show, whatever was planned: it becomes the next rewrite,
  /// a round of its own, and the run is planned again from there. False if the pair has no rule.
  fireNow(a, b) {
    if (!this.canFire(a, b)) return false;
    const t = this.at;
    this.cut();
    this.net.apply(a, b);
    this.note(true); this.byHand++;
    this.net.requeue();
    this.plan();
    this.seek(t + 1);
    return true;
  }
  /// Fire the pair a, b at the moment on show and plan nothing after it, for a page that picks every
  /// pair itself. The run stays open, its end not known, until `replan`.
  fireLive(a, b, hand = false) {
    if (!this.canFire(a, b)) return false;
    this.cut();
    this.net.apply(a, b);
    this.note(hand); if (hand) this.byHand++;
    this.rounds.push(this.n); this.at = this.n;
    this.status = "open"; this.answer = null; this.answerText = ""; this.garbage = 0; this.stuckPairs = 0;
    return true;
  }
  /// Plan the rest of the run again from the moment on show, in the run's own order.
  replan() {
    const t = this.at;
    this.cut();
    this.net.requeue();
    this.plan();
    this.seek(t);
  }
  /// How many whole rounds are done when the first t rewrites are.
  roundsDone(t) {
    let lo = 0, hi = this.rounds.length - 1;
    while (lo < hi) { const m = (lo + hi + 1) >> 1; if (this.rounds[m] <= t) lo = m; else hi = m - 1; }
    return lo;
  }
}

const api = { KINDS, K, POL, ARITY, CODE, FAMILIES, ARMS, PARTS, ruleFor, Net, Run, parse, show, numberOf, marksOf, PRELUDE, ADDS };
if (typeof module !== "undefined" && module.exports) module.exports = api; else root.RLN = api;
})(typeof globalThis !== "undefined" ? globalThis : this);
