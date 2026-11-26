// The quote-and-table net, as the player runs it (index.html): the rules of ../ideal/knet.py (14 kinds of
// agent, copiers and erasers facing in or out, as ../ideal/ktable.py has them), a parser for the programs of
// ../ideal/kref.py, a small reference machine, and a recorder that keeps a run so any moment of it can be
// shown again. No page needed: check.mjs runs it under node against knet.py.
"use strict";
(function (root) {

/// Kinds of agent, by number. `ph` is a placeholder a rewrite uses while it rewires. `pod` is never in a
/// net: the player draws a named part folded shut as one. Every kind that can fire is below 16.
const KINDS = ["", "lam", "app", "var", "rho", "tab", "O", "A", "T", "C", "sig", "dupI", "dupO", "eraI", "eraO", "root", "ph", "pod"];
const K = {}; KINDS.forEach((k, i) => { if (k) K[k] = i; });
/// Each port's polarity, principal first: a value flows out through an `o` port and in through an `i` port.
const POL = ["", "ooi", "oii", "oi", "o", "oiiiii", "iio", "iio", "io", "iiiiiio", "ioo", "ioo", "oii", "i", "o", "i", "io", "o"];
const polarity = (k, q) => POL[k][q];
const ARITY = POL.map(p => Math.max(0, p.length - 1));
/// Code: what a copier copies, an eraser erases, and a table reads.
const CODE = new Set([K.lam, K.app, K.var, K.rho, K.tab]);
const NSLOT = 5;

/// What a rewrite does, for the history bar.
const FAMILIES = ["call", "open", "read", "thaw", "copy", "cancel", "erase"];
const F = {}; FAMILIES.forEach((f, i) => { F[f] = i; });

/// A table's slots, one per kind of value, and the parts each is handed after the table itself.
const ARMS = ["function", "application", "quote", "atom", "table"];
const PARTS = { function: ["f"], application: ["g", "b"], quote: ["w"], atom: [], table: ["a", "b", "c", "d", "e"] };
/// The slot a value of each kind is read by.
const SLOT = new Map([[K.lam, 0], [K.app, 1], [K.var, 2], [K.rho, 3], [K.tab, 4]]);
const READS = ["readFun", "readApp", "readQuote", "readAtom", "readTable"];

/// The rule for a pair of kinds, tried both ways round: [which agent leads, family, rule], or null.
/// eraO ⋈ dupI is no rule of ktable.py (no run makes the pair); knet.py erases there, and so does this.
function ruleFor(ka, kb) {
  for (const [x, y, flip] of [[ka, kb, false], [kb, ka, true]]) {
    let r = null;
    if (x === K.O) r = y === K.lam ? [F.call, "run"] : y === K.var ? [F.open, "quote"] : y === K.tab ? [F.read, "read"] : null;
    else if (x === K.A) r = y === K.lam ? [F.open, "open"] : y === K.tab ? [F.read, "readOpen"] : null;
    else if (x === K.C) r = SLOT.has(y) ? [F.read, READS[SLOT.get(y)]] : null;
    else if (x === K.T) r = y === K.app ? [F.thaw, "thawApp"] : y === K.var ? [F.thaw, "thawVar"] : y === K.tab ? [F.thaw, "thawTable"]
      : (y === K.lam || y === K.rho) ? [F.thaw, "thawValue"] : null;
    else if (x === K.sig) r = CODE.has(y) ? [F.copy, "copy"] : null;
    else if (x === K.dupI) r = CODE.has(y) ? [F.copy, "copy"] : y === K.dupO ? [F.cancel, "cancel"] : null;
    else if (x === K.dupO) r = y === K.sig ? [F.copy, "copy"] : y === K.dupI ? [F.cancel, "cancel"] : null;
    else if (x === K.eraI) r = (CODE.has(y) || y === K.dupO || y === K.eraO) ? [F.erase, "erase"] : null;
    else if (x === K.eraO) r = (y === K.sig || y === K.eraI || y === K.dupI) ? [F.erase, "erase"] : null;
    if (r) return [flip, r[0], r[1]];
  }
  return null;
}

/// A small seeded generator, for running pairs in a random order.
function rng(seed) {
  let s = (seed >>> 0) || 0x9e3779b9;
  return () => { s ^= s << 13; s >>>= 0; s ^= s >>> 17; s ^= s << 5; s >>>= 0; return s / 4294967296; };
}

const isLam = t => Array.isArray(t) && t.length === 3 && t[0] === "λ";
const isQuote = t => Array.isArray(t) && t.length === 2 && t[0] === "~";
const isTab = t => Array.isArray(t) && t.length === NSLOT + 1 && t[0] === "tab";

class Net {
  /// order: "rounds" (every waiting pair, then the pairs those made), "newest" (the pair made last goes
  /// first) or "random". A port is the number (agent << 3) | index: a table reading has seven ports.
  constructor(order = "rounds", seed = 1) {
    this.B = 3; this.S = 8; this.M = 7;
    this.cap = 1024;
    this.kind = new Uint8Array(this.cap);              // by agent id; 0 once gone
    this.far = new Int32Array(this.cap << 3).fill(-1); // by port: the port it is wired to, or -1
    this.where = new Int32Array(this.cap);             // its place in `ids`
    this.ids = new Int32Array(this.cap);               // the agents there are now
    this.live = 0;
    this.op = new Map();                               // which copy each copier belongs to
    // Named parts: code written under a name, as one stretch of the net. Each agent made from it knows the
    // part it is in (the innermost); a copy of a part is a part too. Parts are numbered in the order they
    // are made, so going back to a moment and running on makes the same parts again under the same numbers:
    // only the count (partNext) is put back. When a run takes another way, the later copies are forgotten.
    this.grp = new Map();                              // agent -> part
    this.parts = []; this.partNext = 0;                // { name, src: the part it is a copy of or -1, size: its agents when whole }
    this.partKey = new Map();                          // "part:copy:side" -> the copy's part
    this.cur = -1; this.names = null;                  // the part being written; the name of each defined term
    this.next = 0; this.ops = 0; this.loaded = 0;
    this.queue = []; this.head = 0; this.mark = 0;
    this.order = order; this.seed = seed; this.rand = rng(seed);
    this.events = 0; this.round = 0; this.crossed = 0;
    this.counts = new Int32Array(FAMILIES.length);
    this.last = null;
    // agents kept aside for rewrites to rewire through (a reading table meeting a table has eleven side
    // ports), so the ids of real agents only count agents made
    this.PH = 11;
    this.ph = []; for (let i = 0; i < this.PH; i++) this.ph.push(this.make(K.ph));
    for (const h of this.ph) this.remove(h);
  }
  port(a, i) { return (a << 3) | i; }
  agentOf(p) { return p >> 3; }
  indexOf(p) { return p & 7; }
  arity(a) { return ARITY[this.kind[a]]; }
  grow() {
    const cap = this.cap * 2, grown = (A, n, fill) => { const X = new A(n); if (fill !== undefined) X.fill(fill); return X; };
    const kind = grown(Uint8Array, cap), far = grown(Int32Array, cap << 3, -1), where = grown(Int32Array, cap), ids = grown(Int32Array, cap);
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
    this.kind[a] = kind; this.far.fill(-1, a << 3, (a + 1) << 3);
    this.where[a] = this.live; this.ids[this.live++] = a;
  }
  remove(a) {
    this.kind[a] = 0; this.far.fill(-1, a << 3, (a + 1) << 3);
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
  mate(p) { return this.far[p]; }
  link(p, q) {
    this.far[p] = q; this.far[q] = p;
    if (!(p & 7) && !(q & 7)) {
      const ka = this.kind[p >> 3], kb = this.kind[q >> 3];
      if (ka !== K.root && ka !== K.ph && kb !== K.root && kb !== K.ph) this.queue.push(p >> 3, q >> 3);
    }
  }

  // ---- building code ----
  /// Code for a term, giving its principal port: a bound name, "ρ", [f, a], ["λ", x, body], ["~", t] a quote,
  /// or ["tab", k1, …, k5]. env maps a bound name to the uses collected for it. Named code goes in a part.
  emit(t, env) {
    const name = this.names && typeof t === "object" ? this.names.get(t) : undefined;
    if (name === undefined) return this.code(t, env);
    const outer = this.cur; this.cur = this.part(name);
    const p = this.code(t, env);
    this.cur = outer;
    return p;
  }
  code(t, env) {
    const P = (a, i) => (a << 3) | i;
    if (t === "ρ") return P(this.make(K.rho), 0);
    if (typeof t === "string") {
      if (!env.has(t)) throw new Error(`nothing binds ${t}`);
      const v = this.make(K.var);
      env.get(t).push(P(v, 1));
      return P(v, 0);
    }
    if (isQuote(t)) { const v = this.make(K.var); this.link(P(v, 1), this.emit(t[1], env)); return P(v, 0); }
    if (isTab(t)) {
      const a = this.make(K.tab);
      for (let j = 1; j <= NSLOT; j++) this.link(P(a, j), this.emit(t[j], env));
      return P(a, 0);
    }
    if (isLam(t)) {
      const lam = this.make(K.lam), occ = [], inner = new Map(env);
      inner.set(t[1], occ);
      this.link(P(lam, 2), this.emit(t[2], inner));
      this.bind(P(lam, 1), occ);
      return P(lam, 0);
    }
    const a = this.make(K.app);
    this.link(P(a, 1), this.emit(t[0], env));
    this.link(P(a, 2), this.emit(t[1], env));
    return P(a, 0);
  }
  /// Wire a binder's x to its uses: an eraser for none, a chain of splits for several.
  bind(port, occ) {
    if (!occ.length) { this.link(port, this.make(K.eraI) << 3); return; }
    let i = 0;
    while (occ.length - i > 1) { const s = this.make(K.sig); this.link(port, s << 3); this.link((s << 3) | 1, occ[i++]); port = (s << 3) | 2; }
    this.link(port, occ[i]);
  }
  /// Put a term in the net under a thaw, its answer arriving at a root agent.
  load(t) {
    this.root = this.make(K.root);
    const th = this.make(K.T);
    this.link(th << 3, this.emit(t, new Map()));
    this.link((th << 3) | 1, this.root << 3);
    this.mark = this.queue.length;
    this.loaded = this.next;
    return this.root;
  }

  // ---- rewriting ----
  /// Replace the active pair a, b. build(slot) makes what replaces it; slot(agent, i) is the place the
  /// agent's port i was wired to. Placeholders keep wires between a's ports and b's whole.
  rewrite(a, b, build) {
    const ports = [], ph = new Map();
    for (const x of [a, b]) for (let i = 1; i <= ARITY[this.kind[x]]; i++) ports.push((x << 3) | i);
    ports.forEach((p, n) => { const h = this.ph[n]; this.put(h, K.ph); ph.set(p, h); });
    for (const p of ports) {
      const e = this.far[p];
      if (ph.has(e)) { if (p < e) this.link(ph.get(p) << 3, ph.get(e) << 3); }
      else this.link(ph.get(p) << 3, e);
    }
    this.remove(a); this.remove(b);
    build((x, i) => (ph.get((x << 3) | i) << 3) | 1);
    for (const h of ph.values()) {
      const x = this.far[h << 3], y = this.far[(h << 3) | 1];
      if (x < 0 || y < 0) throw new Error("a rewrite left a port unwired");
      if (x !== ((h << 3) | 1)) this.link(x, y);
      this.remove(h);
    }
  }
  /// Fire the pair a, b: the family of what happened, or -1 if no rule fits (the pair is stuck).
  fire(a, b) {
    const rule = ruleFor(this.kind[a], this.kind[b]);
    if (!rule) return -1;
    const [x, y] = rule[0] ? [b, a] : [a, b], kx = this.kind[x], ky = this.kind[y], made = this.next;
    const part = this.grp.get(y), lead = this.grp.get(x), P = (u, i) => (u << 3) | i;
    const keep = (w, g = part) => { if (g !== undefined) this.grp.set(w, g); };
    const fresh = kind => this.make(kind);
    this.last = { kinds: kx * 16 + ky, rule: rule[2], made, a, b };
    switch (rule[2]) {
      case "run": this.rewrite(x, y, slot => {
        const t = fresh(K.T);
        this.link(slot(x, 1), slot(y, 1)); this.link(P(t, 0), slot(y, 2)); this.link(P(t, 1), slot(x, 2));
      }); break;
      case "quote": this.rewrite(x, y, slot => {
        const o = fresh(K.A);
        this.link(P(o, 0), slot(y, 1)); this.link(P(o, 1), slot(x, 1)); this.link(P(o, 2), slot(x, 2));
      }); break;
      case "open": this.rewrite(x, y, slot => { this.link(slot(x, 1), slot(y, 1)); this.link(slot(x, 2), slot(y, 2)); }); break;
      case "read": case "readOpen": this.rewrite(x, y, slot => {
        const c = fresh(K.C); keep(c);
        this.link(P(c, 0), slot(x, 1));
        for (let j = 1; j <= NSLOT; j++) this.link(P(c, j), slot(y, j));
        if (kx === K.O) { const t = fresh(K.T); this.link(P(t, 0), P(c, NSLOT + 1)); this.link(P(t, 1), slot(x, 2)); }
        else this.link(P(c, NSLOT + 1), slot(x, 2));
      }); break;
      case "readFun": case "readApp": case "readQuote": case "readAtom": case "readTable": {
        const i = SLOT.get(ky);
        this.rewrite(x, y, slot => {
          let parts;
          if (ky === K.lam) {
            const f = fresh(K.lam); keep(f);
            this.link(P(f, 1), slot(y, 1)); this.link(P(f, 2), slot(y, 2));
            parts = [P(f, 0)];
          } else { parts = []; for (let j = 1; j <= ARITY[ky]; j++) parts.push(slot(y, j)); }
          const d = fresh(K.dupI);
          this.op.set(d, this.ops++);
          this.link(P(d, 0), slot(x, i + 1));
          const t = fresh(K.tab); keep(t, lead);
          for (let j = 0; j < NSLOT; j++) this.link(P(t, j + 1), j === i ? P(d, 1) : slot(x, j + 1));
          const quoted = p => { const v = fresh(K.var); this.link(P(v, 1), p); return P(v, 0); };
          let cur = quoted(P(d, 2));
          for (const p of [P(t, 0), ...parts]) { const ap = fresh(K.app); this.link(P(ap, 1), cur); this.link(P(ap, 2), quoted(p)); cur = P(ap, 0); }
          this.link(cur, slot(x, NSLOT + 1));
        });
        break;
      }
      case "thawApp": this.rewrite(x, y, slot => {
        const o = fresh(K.O), tf = fresh(K.T), ta = fresh(K.T);
        this.link(P(tf, 0), slot(y, 1)); this.link(P(tf, 1), P(o, 0));
        this.link(P(ta, 0), slot(y, 2)); this.link(P(ta, 1), P(o, 1));
        this.link(P(o, 2), slot(x, 1));
      }); break;
      case "thawVar": this.rewrite(x, y, slot => { this.link(slot(x, 1), slot(y, 1)); }); break;
      case "thawValue": this.rewrite(x, y, slot => {
        const w = fresh(ky); keep(w);
        this.link(P(w, 0), slot(x, 1));
        for (let i = 1; i <= ARITY[ky]; i++) this.link(P(w, i), slot(y, i));
      }); break;
      case "thawTable": this.rewrite(x, y, slot => {
        const w = fresh(K.tab); keep(w);
        this.link(P(w, 0), slot(x, 1));
        for (let j = 1; j <= NSLOT; j++) { const th = fresh(K.T); this.link(P(th, 0), slot(y, j)); this.link(P(th, 1), P(w, j)); }
      }); break;
      case "copy": {
        const op = kx === K.sig ? this.ops++ : this.op.get(x);
        this.rewrite(x, y, slot => {
          const x1 = fresh(ky), x2 = fresh(ky);
          if (part !== undefined) { this.grp.set(x1, this.copyOf(part, op, 1)); this.grp.set(x2, this.copyOf(part, op, 2)); }
          this.link(P(x1, 0), slot(x, 1)); this.link(P(x2, 0), slot(x, 2));
          for (let i = 1; i <= ARITY[ky]; i++) {
            const d = fresh(POL[ky][i] === "i" ? K.dupI : K.dupO);
            this.op.set(d, op);
            this.link(P(d, 0), slot(y, i)); this.link(P(d, 1), P(x1, i)); this.link(P(d, 2), P(x2, i));
          }
        });
        break;
      }
      case "cancel":
        if (this.op.get(x) !== this.op.get(y)) this.crossed++;
        this.rewrite(x, y, slot => { this.link(slot(x, 1), slot(y, 1)); this.link(slot(x, 2), slot(y, 2)); });
        break;
      case "erase": this.rewrite(x, y, slot => {
        for (let i = 1; i <= ARITY[ky]; i++) this.link(fresh(POL[ky][i] === "i" ? K.eraI : K.eraO) << 3, slot(y, i));
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
      if (!this.kind[a] || !this.kind[b] || this.far[a << 3] !== b << 3) continue;
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
      const a = this.ids[n], f = this.far[a << 3];
      if (f < 0 || (f & 7) || (f >> 3) < a) continue;
      const b = f >> 3, ka = this.kind[a], kb = this.kind[b];
      if (ka === K.root || kb === K.root) continue;
      out.push([a, b, !!ruleFor(ka, kb)]);
    }
    return out;
  }

  // ---- keeping a moment, and going back to it ----
  /// The net as it is (not the pairs waiting: `requeue` finds those again).
  save() {
    const n = this.live, ids = this.ids.slice(0, n), kinds = new Uint8Array(n), far = new Int32Array(n << 3);
    for (let i = 0; i < n; i++) { const a = ids[i]; kinds[i] = this.kind[a]; far.set(this.far.subarray(a << 3, (a + 1) << 3), i << 3); }
    const grp = new Int32Array(n);
    for (let i = 0; i < n; i++) grp[i] = this.grp.get(ids[i]) ?? -1;
    return { ids, kinds, far, grp, parts: this.partNext, op: [...this.op], next: this.next, ops: this.ops,
      events: this.events, crossed: this.crossed, counts: this.counts.slice() };
  }
  restore(s) {
    for (let i = 0; i < this.live; i++) { const a = this.ids[i]; this.kind[a] = 0; this.far.fill(-1, a << 3, (a + 1) << 3); }
    while (this.cap < s.next) this.grow();
    this.live = s.ids.length; this.ids.set(s.ids);
    for (let i = 0; i < this.live; i++) { const a = s.ids[i]; this.kind[a] = s.kinds[i]; this.where[a] = i; this.far.set(s.far.subarray(i << 3, (i + 1) << 3), a << 3); }
    this.op = new Map(s.op); this.next = s.next; this.ops = s.ops;
    this.grp = new Map(); for (let i = 0; i < this.live; i++) if (s.grp[i] >= 0) this.grp.set(s.ids[i], s.grp[i]);
    this.partNext = s.parts;
    this.queue = []; this.head = 0; this.mark = 0;
    this.events = s.events; this.crossed = s.crossed; this.counts = s.counts.slice();
    this.last = null;
  }

  // ---- reading terms off the net ----
  /// The term whose value comes out of a port. Code reads as it was written: binders named x0, x1, … in
  /// the order met, a use of a variable that has a value (or a quote) as ["~", what it holds], a table as
  /// ["tab", k1, …, k5]. `strict` reads a finished answer, and throws on anything else. Otherwise the net
  /// may be in any state, and what is under way reads as ["@", f, a] a running application, ["@'", f, a]
  /// one that opens, ["▸", code] a thaw, ["read", v] a table reading v, ["copy", n, v] one side of copy n
  /// that has still to pass through v, "∅" a variable whose binder was erased, and "?" what is not a value.
  /// A variable whose binder is outside what is read gets a name of its own. `short`, a map from an agent
  /// to a name, has the name stand for that agent and all under it (["name", n, term], the term read only
  /// when `value` asks for it), except where the reading starts. `where`, a map, is filled with the agent
  /// of each function read. Past `budget` agents the rest reads as "…". It walks with a list of what is
  /// left to read, not by calling itself, so long code is no trouble.
  read(port, { strict = false, budget = 2000000, short = null, where = null } = {}) {
    const names = new Map(), out = [null], todo = [[port, out, 0]];
    let start = true;
    while (todo.length) {
      const [p, into, at] = todo.pop(), a = p >> 3, q = p & 7, k = p < 0 ? 0 : this.kind[a], first = start;
      start = false;
      if (strict && (q || p < 0)) throw new Error("not a value");
      if (--budget < 0) { if (strict) throw new Error("too large to read"); into[at] = "…"; continue; }
      if (!q && !first && short?.has(a)) { into[at] = ["name", short.get(a), () => this.read(p, { budget: 6000 })]; continue; }
      if (k === K.lam && !q) {
        const x = `x${names.size}`, t = ["λ", x, null], uses = this.far[(a << 3) | 1];
        names.set((a << 3) | 1, x); where?.set(t, a);
        if (uses >= 0 && this.kind[uses >> 3] === K.dupO && (uses & 7)) names.set((uses >> 3) << 3, x);   // being copied: its uses are past the copier
        into[at] = t; todo.push([this.far[(a << 3) | 2], t, 2]);
      } else if (k === K.app && !q) {
        const t = [null, null];
        into[at] = t; todo.push([this.far[(a << 3) | 2], t, 1], [this.far[(a << 3) | 1], t, 0]);   // the function part is read first
      } else if (k === K.rho && !q) into[at] = "ρ";
      else if (k === K.tab && !q) { const t = ["tab", null, null, null, null, null]; into[at] = t; for (let j = NSLOT; j >= 1; j--) todo.push([this.far[(a << 3) | j], t, j]); }
      else if (k === K.var && !q) {
        // up the variable's wire: through splits, and through the two ends of a copy that are about to cancel
        let src = this.far[(a << 3) | 1];
        for (;;) {
          const b = src >> 3, kb = src < 0 ? 0 : this.kind[b], up = kb === K.dupI && (src & 7) ? this.far[b << 3] : -1;
          if (kb === K.sig && (src & 7)) src = this.far[b << 3];
          else if (!strict && up >= 0 && this.kind[up >> 3] === K.dupO && !(up & 7)) src = this.far[((up >> 3) << 3) | (src & 7)];
          else break;
        }
        const kb = src < 0 ? 0 : this.kind[src >> 3], binder = (kb === K.lam && (src & 7) === 1) || (kb === K.dupO && !(src & 7));
        if (names.has(src)) into[at] = names.get(src);
        else if (!strict && binder) { const x = `x${names.size}`; names.set(src, x); into[at] = x; }
        else if (!strict && kb === K.eraO) into[at] = "∅";
        else { const t = ["~", null]; into[at] = t; todo.push([src, t, 1]); }
      } else if (strict) throw new Error("not finished");
      else if ((k === K.O || k === K.A) && q === 2) { const t = [k === K.O ? "@" : "@'", null, null]; into[at] = t; todo.push([this.far[(a << 3) | 1], t, 2], [this.far[a << 3], t, 1]); }
      else if (k === K.T && q === 1) { const t = ["▸", null]; into[at] = t; todo.push([this.far[a << 3], t, 1]); }
      else if (k === K.C && q === NSLOT + 1) { const t = ["read", null]; into[at] = t; todo.push([this.far[a << 3], t, 1]); }
      else if (k === K.dupI && q) { const t = ["copy", this.op.get(a) ?? 0, null]; into[at] = t; todo.push([this.far[a << 3], t, 2]); }
      else into[at] = "?";
    }
    return out[0];
  }
  /// The answer at the root, if the run has left one there: a term, or null.
  answer() {
    const p = this.far[this.root << 3];
    if (p < 0 || (p & 7) || !CODE.has(this.kind[p >> 3])) return null;
    try { return this.read(p, { strict: true }); } catch (e) { return null; }
  }
  /// How many agents cannot be reached from the root (none, once a run has finished).
  garbage() {
    const seen = new Set([this.root]), todo = [this.root];
    while (todo.length) {
      const a = todo.pop();
      for (let i = 0; i <= ARITY[this.kind[a]]; i++) { const f = this.far[(a << 3) | i]; if (f >= 0 && !seen.has(f >> 3)) { seen.add(f >> 3); todo.push(f >> 3); } }
    }
    return this.live - seen.size;
  }
}

// ---- programs ----
/// The definitions every program may use (the same text as PROGRAMS in ../ideal/kcheck.py).
const PRELUDE = String.raw`
-- Church numerals and booleans, as in ../nets/check.py
zero := \f x. x
suc := \n f x. f (n f x)
add := \m n f x. m f (n f x)
true := \t e. t
false := \t e. e
and := \x y. x y false
Z := \g. (\h. g (\v. h h v)) (\h. g (\v. h h v))
-- an error: the atom applied
err := ρ ρ
-- a quote of a value: the code ~v
stem := \v. 'v
-- A table [fun | app | quote | atom | table] applied to a value runs the slot for its kind, handed the
-- table itself and then the parts. Slots below name what they are handed: t (or u, v) is the table.
kindOf := [\t f. true | \t g b. false | \t w. false | \t. false | \t a b c d e. false]
-- count binders in code: a function is opened on ρ ('f ρ is its body, as code), an application is read
-- on both sides. The table is its own loop: no fixpoint.
size := [ \t f. suc (t ('f ρ))
        | \t g b. add (t g) (t b)
        | \t w. zero
        | \t. zero
        | \t a b c d e. add (t a) (add (t b) (add (t c) (add (t d) (t e)))) ]
-- the same with the table threaded through every read: it is never copied, and dropped once at the end
sizeK := \c. [ \t f k. t ('f ρ) (\t n. k t (suc n))
             | \t g b k. t g (\t n. t b (\t m. k t (add n m)))
             | \t w k. k t zero
             | \t k. k t zero
             | \t a b c d e k. t a (\t n1. t b (\t n2. t c (\t n3. t d (\t n4. t e (\t n5. k t (add n1 (add n2 (add n3 (add n4 n5)))))))))
             ] c (\t n. n)
-- An interpreter for the calculus. ap x t y applies the value x to the value y, interpreting. 'x y is the
-- code that x y would run, handed back unrun: for a function its body, for a table the slot for y's kind
-- applied to the table and y's parts. So both are opened on y and the code is interpreted. A quote applied
-- opens what it quotes; nothing runs.
ap := [ \u f t y. t ('f y)
      | \u g b t y. err
      | \u w t y. 'w y
      | \u t y. err
      | \u s1 s2 s3 s4 s5 t y. (\x. t ('x y)) [s1 | s2 | s3 | s4 | s5] ]
interp := [ \t f. f
          | \t g b. ap (t g) t (t b)
          | \t w. w
          | \t. ρ
          | \t a b c d e. [t a | t b | t c | t d | t e] ]
-- Equality of programs. fresh a acc wraps acc in one quote for every node of a, so fresh a (fresh b ρ) is
-- deeper than anything inside a or b: no part of either can be mistaken for it. eqd a d b compares code;
-- two functions are both opened on the mark d and compared under the next mark, 'd.
fresh := [ \t f acc. stem (t ('f ρ) acc)
         | \t g b acc. t g (t b acc)
         | \t w acc. stem (t w acc)
         | \t acc. stem acc
         | \t a b c d e acc. t a (t b (t c (t d (t e acc)))) ]
eqd := [ \t f d b. [ \u g t f d. t ('f d) (stem d) ('g d)
                   | \u g2 b2 t f d. false
                   | \u w t f d. false
                   | \u t f d. false
                   | \u a b c d2 e t f d. false ] b t f d
       | \t g1 b1 d b. [ \u g t g1 b1 d. false
                       | \u g2 b2 t g1 b1 d. and (t g1 d g2) (t b1 d b2)
                       | \u w t g1 b1 d. false
                       | \u t g1 b1 d. false
                       | \u a b c d2 e t g1 b1 d. false ] b t g1 b1 d
       | \t w1 d b. [ \u g t w1 d. false
                    | \u g2 b2 t w1 d. false
                    | \u w2 t w1 d. t w1 d w2
                    | \u t w1 d. false
                    | \u a b c d2 e t w1 d. false ] b t w1 d
       | \t d b. [ \u g t d. false
                 | \u g2 b2 t d. false
                 | \u w t d. false
                 | \u t d. true
                 | \u a b c d2 e t d. false ] b t d
       | \t a1 a2 a3 a4 a5 d b.
                 [ \u g t a1 a2 a3 a4 a5 d. false
                 | \u g2 b2 t a1 a2 a3 a4 a5 d. false
                 | \u w t a1 a2 a3 a4 a5 d. false
                 | \u t a1 a2 a3 a4 a5 d. false
                 | \u b1 b2 b3 b4 b5 t a1 a2 a3 a4 a5 d.
                     and (t a1 d b1) (and (t a2 d b2) (and (t a3 d b3) (and (t a4 d b4) (t a5 d b5))))
                 ] b t a1 a2 a3 a4 a5 d ]
equal := \a b. eqd a (fresh a (fresh b ρ)) b
-- The interpreter with both tables threaded through every read (t interprets, u applies): neither is ever
-- copied, and each is dropped once, at the end. Every slot takes a continuation k.
apT := [ \u f t y k. t ('f y) u k
       | \u g b t y k. err
       | \u w t y k. k t u ('w y)
       | \u t y k. err
       | \u s1 s2 s3 s4 s5 t y k. (\x. t ('x y) u k) [s1 | s2 | s3 | s4 | s5] ]
interpT := \c. [ \t f u k. k t u f
               | \t g b u k. t g u (\t u x. t b u (\t u y. u x t y k))
               | \t w u k. k t u w
               | \t u k. k t u ρ
               | \t a b c d e u k. t a u (\t u va. t b u (\t u vb. t c u (\t u vc. t d u (\t u vd. t e u (\t u ve. k t u [va | vb | vc | vd | ve])))))
               ] c apT (\t u v. v)
-- A reader that erases nothing: it swaps the two sides of every application in a piece of code, uses every
-- part it is handed, and hands its table back beside the answer.
pair := \x y k. k x y
mirror := \c. [ \t f k. k t f
              | \t g b k. t g (\t g2. t b (\t b2. k t '(b2 g2)))
              | \t w k. k t 'w
              | \t k. k t ρ
              | \t a b c d e k. k t [a | b | c | d | e]
              ] c (\t v. pair v t)
`;

/// The page's examples: a program and what to say of it, in groups. check.mjs runs each against
/// expected.json, and holds a count of rewrites one states to a run.
const EXAMPLES = [
  ["Small: one thing at a time", [
    ["(\\x. x) (\\y. y)", "a call"],
    ["(\\x y. x) ρ ρ", "an argument that is not used: an eraser"],
    ["(\\f. f (f ρ)) (\\x. x)", "a function used twice: a copy"],
    ["(\\x. \\y. (\\z. z) x) ρ", "the body of a function stays code until it is called"],
    ["'((\\x. x x) (\\y. y))", "a quote: the code comes back as it is, unrun"],
    ["(\\x. '(x x)) ρ", "a quote that mentions a variable: its value is put in, unrun"],
    ["''(\\x. x x) ρ", "a quote applied: the function is opened, and its body comes back as code"],
    ["(\\g. 'g ρ ('g (\\y. y))) (\\x z. x)", "one function opened twice: the copy happens on the quote"],
  ]],
  ["One table read on each kind of value", [
    ["[\\t f. 'f ρ | \\t g b. b | \\t w. w | \\t. ρ | \\t a b c d e. e] (\\x. x x)", "a function: the function slot opens it on ρ"],
    ["[\\t f. 'f ρ | \\t g b. b | \\t w. w | \\t. ρ | \\t a b c d e. e] '(ρ (\\y. y))", "an application: the slot is handed its two sides"],
    ["[\\t f. 'f ρ | \\t g b. b | \\t w. w | \\t. ρ | \\t a b c d e. e] ''(\\y. y)", "a quote: the slot is handed what it quotes"],
    ["[\\t f. 'f ρ | \\t g b. b | \\t w. w | \\t. ρ | \\t a b c d e. e] ρ", "the atom: the slot is handed nothing"],
    ["[\\t f. 'f ρ | \\t g b. b | \\t w. w | \\t. ρ | \\t a b c d e. e] [ρ | ρ | ρ | ρ | \\z. z]", "a table: the slot is handed its five slots"],
    ["(\\k. 'k (\\x. x)) kindOf", "a table opened: the read is handed back as code, not run"],
  ]],
  ["Reading programs", [
    ["kindOf (\\x. x)", "is it a function?"],
    ["add (suc zero) (suc zero)", "1 + 1, left as a function"],
    ["size (\\x y. y x)", "count the binders in a function (797 rewrites)"],
    ["size suc", "the binders in suc (1,799 rewrites)"],
    ["sizeK suc", "the same, the table threaded through every read (917 rewrites)"],
    ["mirror '((\\x. x x) (ρ ρ) '(ρ (\\y. y)))", "a reader that erases nothing: it swaps the sides of each application"],
    ["equal (\\x. x) (\\y. y)", "equal: λx. x and λy. y (3,509 rewrites)"],
    ["equal (\\x y. x) (\\x y. y)", "equal: λx y. x and λx y. y (4,481 rewrites)"],
    ["equal (\\x. x) (\\x. 'ρ)", "equal: λx. x and λx. 'ρ, a mark forged by hand (2,026 rewrites)"],
  ]],
  ["The interpreter", [
    ["interp '((\\x. x) (\\y. y))", "the interpreter on (λx. x) (λy. y)"],
    ["interp '(''(\\x. x x) ρ)", "the interpreter on an opening"],
    ["interp '(add (suc zero) (suc zero))", "interpret 1 + 1 (3,682 rewrites)"],
    ["interpT '(add (suc zero) (suc zero))", "interpret 1 + 1, tables threaded (1,194 rewrites)"],
  ]],
  ["Heavy: tens of thousands of rewrites", [
    ["interpT '(interpT '((\\x. x) (\\y. y)))", "the threaded interpreter on itself (9,670 rewrites)"],
    ["interp '(interp '((\\x. x) (\\y. y)))", "the interpreter interpreting itself (20,675 rewrites)"],
    ["size size", "size reads its own table (24,527 rewrites)"],
  ]],
  ["Stuck, and never ending", [
    ["ρ (\\x. x)", "stuck: the atom applied"],
    ["'(ρ ρ) ρ", "stuck: code that is not a function applied"],
    ["(\\x. x x) (\\x. x x)", "never ends"],
  ]],
];

const TOKEN = /\\|λ|\.|\(|\)|\[|\]|\||'|[A-Za-z_][A-Za-z0-9_]*|ρ/g;
/// A program's term, as kref.py parses it. \x y. body is a function, juxtaposition applies, 'M is a quote,
/// [a | b | c | d | e] a table, ρ (or rho) the atom; `name := term` lines define names for the lines
/// after them, and a line that starts with a space goes on the line before; -- starts a comment. A name
/// nothing binds or defines is an error. `names`, if given, is filled with the name of each defined term.
function parse(src, names) {
  const defs = new Map(), lines = [];
  for (const raw of src.split("\n")) {
    const l = raw.split("--")[0];
    if (!l.trim()) continue;
    if (l.includes(":=") || !lines.length || !/^\s/.test(l)) lines.push(l); else lines[lines.length - 1] += " " + l;
  }
  let out = null;
  for (const ch of lines) {
    const at = ch.indexOf(":="), name = at < 0 ? null : ch.slice(0, at).trim(), body = at < 0 ? ch : ch.slice(at + 2);
    const bad = body.replace(TOKEN, "").replace(/\s+/g, "");
    if (bad) throw new Error(`cannot read "${bad[0]}"`);
    const toks = body.match(TOKEN) ?? [];
    if (!toks.length) throw new Error(name ? `${name} has no body` : "nothing to run");
    const [t, i] = expr(toks, 0, defs, []);
    if (i !== toks.length) throw new Error(`an unmatched ${toks[i]}`);
    if (name) { defs.set(name, t); if (names && typeof t === "object") names.set(t, name); } else out = t;
  }
  if (out === null) throw new Error("nothing to run");
  return out;
}
const STOP = new Set([")", "|", "]"]);
const isBinder = t => t === "\\" || t === "λ";
function expr(toks, i, defs, bound) {
  if (isBinder(toks[i])) {
    const xs = [];
    for (i++; toks[i] !== "."; i++) {
      if (i >= toks.length || !/^[A-Za-z_]/.test(toks[i])) throw new Error("a binder needs names, then a dot");
      xs.push(toks[i]);
    }
    if (!xs.length) throw new Error("a binder needs a name");
    let [body, j] = expr(toks, i + 1, defs, bound.concat(xs));
    for (let k = xs.length - 1; k >= 0; k--) body = ["λ", xs[k], body];
    return [body, j];
  }
  let head = null;
  while (i < toks.length && !STOP.has(toks[i])) {
    let a;
    if (isBinder(toks[i])) [a, i] = expr(toks, i, defs, bound);
    else [a, i] = atom(toks, i, defs, bound);
    head = head === null ? a : [head, a];
  }
  if (head === null) throw new Error(toks[i] === "|" || toks[i] === "]" ? "an empty slot" : "an empty ( )");
  return [head, i];
}
function atom(toks, i, defs, bound) {
  const t = toks[i];
  if (t === undefined) throw new Error("the program ends too soon");
  if (t === "'") { const [a, j] = atom(toks, i + 1, defs, bound); return [["~", a], j]; }
  if (t === "(") {
    const [a, j] = expr(toks, i + 1, defs, bound);
    if (toks[j] !== ")") throw new Error("a ( is never closed");
    return [a, j + 1];
  }
  if (t === "[") {
    const slots = [];
    for (i++; ; i++) {
      let s; [s, i] = expr(toks, i, defs, bound); slots.push(s);
      if (toks[i] === "]") break;
      if (toks[i] !== "|") throw new Error("a [ is never closed");
    }
    if (slots.length !== NSLOT) throw new Error(`a table has ${NSLOT} slots, not ${slots.length}`);
    return [["tab", ...slots], i + 1];
  }
  if (t === "." ) throw new Error("a dot with no binder");
  if (t === "ρ" || t === "rho") return ["ρ", i + 1];
  if (bound.includes(t)) return [t, i + 1];
  if (defs.has(t)) return [defs.get(t), i + 1];
  if (isBinder(t)) return expr(toks, i, defs, bound);
  throw new Error(`nothing binds ${t}`);
}

// ---- the reference machine (kref.py's Machine) ----
function subst(c, x, s) {
  if (c === x) return s;
  if (typeof c === "string") return c;
  if (isQuote(c)) return ["~", subst(c[1], x, s)];
  if (isTab(c)) return ["tab", ...c.slice(1).map(k => subst(k, x, s))];
  if (isLam(c)) return c[1] === x ? c : ["λ", c[1], subst(c[2], x, s)];
  return [subst(c[0], x, s), subst(c[1], x, s)];
}
/// Running a term as kref.py does, by substitution, call by value, never under a binder. Throws "stuck" or "fuel".
class Machine {
  constructor(fuel) { this.fuel = fuel; }
  run(c) {
    if (typeof c === "string") { if (c === "ρ") return c; throw new Error(`free variable ${c}`); }
    if (isQuote(c)) return c[1];
    if (isLam(c)) return c;
    if (isTab(c)) return ["tab", ...c.slice(1).map(k => this.run(k))];
    return this.apply(this.run(c[0]), this.run(c[1]));
  }
  apply(f, v) {
    if (--this.fuel < 0) throw new Error("fuel");
    if (isLam(f)) return this.run(subst(f[2], f[1], ["~", v]));
    if (isQuote(f)) {
      const g = f[1];
      if (isLam(g)) return subst(g[2], g[1], ["~", v]);
      if (isTab(g)) return this.readOf(g, v);
      throw new Error("stuck");
    }
    if (isTab(f)) return this.run(this.readOf(f, v));
    throw new Error("stuck");
  }
  readOf(tab, v) {
    const [i, parts] = isLam(v) ? [0, [v]] : isQuote(v) ? [2, [v[1]]] : v === "ρ" ? [3, []] : isTab(v) ? [4, v.slice(1)] : [1, [v[0], v[1]]];
    let c = [["~", tab[1 + i]], ["~", tab]];
    for (const p of parts) c = [c, ["~", p]];
    return c;
  }
}
/// Binders renamed x0, x1, … in the order a walk from the left meets them, as kref.py's canon does.
function canon(t) {
  let count = 0;
  const go = (t, env) => {
    if (typeof t === "string") return env.get(t) ?? t;
    if (isQuote(t)) return ["~", go(t[1], env)];
    if (isTab(t)) return ["tab", ...t.slice(1).map(k => go(k, env))];
    if (isLam(t)) { const x = `x${count++}`; return ["λ", x, go(t[2], new Map(env).set(t[1], x))]; }
    return [go(t[0], env), go(t[1], env)];
  };
  return go(t, new Map());
}
const TRUE = JSON.stringify(canon(["λ", "t", ["λ", "e", "t"]])), FALSE = JSON.stringify(canon(["λ", "t", ["λ", "e", "e"]]));
/// What a value means, as kcheck.py's show reads it: "true", "false" (which is also 0), a Church numeral's
/// number (it is run on a step that adds a quote, from ρ), or null.
function meaning(v) {
  if (!v) return null;
  const c = JSON.stringify(canon(v));
  if (c === TRUE) return "true";
  if (c === FALSE) return "false";
  try {
    const m = new Machine(20000);
    let r = m.apply(m.apply(v, ["λ", "q", ["~", "q"]]), "ρ"), n = 0;
    while (isQuote(r)) { r = r[1]; n++; }
    return r === "ρ" ? String(n) : null;
  } catch (e) { return null; }
}

/// A term without the marks round it that `read` puts on a name, and what a name stands for.
function value(t) {
  while (Array.isArray(t) && t[0] === "name") { if (typeof t[2] === "function") t[2] = t[2](); t = t[2]; }
  return t;
}
/// The marks `read` puts on what is not plain code.
const MARKS = new Set(["~", "▸", "read", "name"]);
const isApp = t => Array.isArray(t) && t.length === 2 && !MARKS.has(t[0]);
/// The number of a value written as a Church numeral, λf x. f (… (f x)), one or more: else null.
function numberOf(t) {
  t = value(t);
  if (!isLam(t)) return null;
  const f = t[1], inner = value(t[2]);
  if (!isLam(inner)) return null;
  let x = inner[1], body = value(inner[2]), n = 0;
  while (isApp(body) && body[0] === f) { n++; body = value(body[1]); }
  return n && body === x ? String(n) : null;
}
/// The numbers in a term that `read` made with `where`: a map from the agent at the top of each to its text.
function marksOf(term, where) {
  const marks = new Map(), todo = [term];
  while (todo.length) {
    let t = todo.pop();
    while (isQuote(t)) t = t[1];
    if (!Array.isArray(t) || t[0] === "name") continue;
    if (isLam(t)) { const n = numberOf(t); if (n !== null && where.has(t)) { marks.set(where.get(t), n); continue; } todo.push(t[2]); }
    else if (isApp(t)) todo.push(t[0], t[1]);
    else for (let i = 1; i < t.length; i++) if (typeof t[i] !== "number") todo.push(t[i]);
  }
  return marks;
}

/// A term as text: λa b. body, 'M for a quote, [a | b | c | d | e] a table. What is under way: ▸c a thaw on
/// code c, f @ a an application that is running, f @' a one that opens, read⟨v⟩ a table reading v, ⧉n⟨v⟩
/// a side of copy n. `names` keeps each variable's own name, where the binders are not renamed.
function show(t, { names = false } = {}) {
  const letters = new Map();
  const nameOf = x => { if (names) return x; if (!letters.has(x)) { const n = letters.size; letters.set(x, "abcdefghijklmnopqrstuvwxyz"[n % 26] + (n >= 26 ? Math.floor(n / 26) : "")); } return letters.get(x); };
  const isRun = t => Array.isArray(t) && t.length === 3 && (t[0] === "@" || t[0] === "@'");
  const loose = t => isLam(t) || isApp(t) || isRun(t);
  const inner = t => loose(t) ? `(${go(t)})` : go(t);
  const go = t => {
    if (typeof t === "string") return t === "ρ" || t === "…" || t === "?" || t === "∅" ? t : nameOf(t);
    if (t[0] === "name") return t[1];
    if (isTab(t)) return `[${t.slice(1).map(go).join(" | ")}]`;
    if (isQuote(t)) return `'${inner(t[1])}`;
    if (t[0] === "▸") return `▸${inner(t[1])}`;
    if (t[0] === "read") return `read⟨${go(t[1])}⟩`;
    if (t[0] === "copy") return `⧉${t[1]}⟨${go(t[2])}⟩`;
    if (isLam(t)) { const xs = [nameOf(t[1])]; let b = t[2]; while (isLam(b)) { xs.push(nameOf(b[1])); b = b[2]; } return `λ${xs.join(" ")}. ${go(b)}`; }
    const run = isRun(t), f = run ? t[1] : t[0], a = run ? t[2] : t[1];
    return `${isLam(f) ? `(${go(f)})` : go(f)}${run ? ` ${t[0]} ` : " "}${inner(a)}`;
  };
  return go(t);
}

/// A whole run of a program, kept so any moment can be shown: which pair each rewrite fired and what
/// it did, where each round began, and the net saved every so often (thinner as the run gets long).
/// The run is planned ahead in the chosen order; a pair fired by hand (`fireNow`) replaces what was
/// planned from that moment on, and the rest is planned again from the net it leaves.
class Run {
  /// `term`, if given, is run in place of the source: a term as parse gives one.
  constructor(src, { order = "rounds", seed = 1, limit = 400000, prelude = PRELUDE, term = null } = {}) {
    const names = new Map();
    this.term = term ?? parse(prelude + "\n" + src, names);
    this.net = new Net(order, seed);
    this.net.names = names;
    this.net.load(this.term);
    this.order = order; this.limit = limit;
    const cap = 1024;
    this.fam = new Uint8Array(cap); this.kinds = new Uint8Array(cap); this.hand = new Uint8Array(cap);
    this.pa = new Int32Array(cap); this.pb = new Int32Array(cap);            // the pair each rewrite fired
    this.madeAt = new Int32Array(cap); this.madeN = new Int32Array(cap);       // the agents it made: madeN of them, from id madeAt
    this.rounds = [0];
    this.saved = [{ t: 0, s: this.net.save() }]; this.gap = 16;
    this.n = 0; this.at = 0; this.byHand = 0;
    this.plan();
  }
  /// Agents made by the rewrites so far, as knet.py counts them (`made`).
  get made() { return this.net.next - this.net.loaded; }
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
    this.answer = ans; this.answerText = ans ? show(ans) : ""; this.meaning = meaning(ans);
    this.garbage = this.status === "done" ? this.net.garbage() : 0;
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
  canFire(a, b) { const N = this.net; return !!N.kind[a] && !!N.kind[b] && N.far[a << 3] === b << 3 && !!ruleFor(N.kind[a], N.kind[b]); }
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
    this.status = "open"; this.answer = null; this.answerText = ""; this.meaning = null; this.garbage = 0; this.stuckPairs = 0;
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

const api = { KINDS, K, POL, ARITY, CODE, NSLOT, FAMILIES, ARMS, PARTS, polarity, ruleFor, Net, Run, Machine, parse, show, canon, meaning, numberOf, marksOf, PRELUDE, EXAMPLES };
if (typeof module !== "undefined" && module.exports) module.exports = api; else root.RLN = api;
})(typeof globalThis !== "undefined" ? globalThis : this);
