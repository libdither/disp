<script lang="ts">
  // The interaction net of a two-way program (net.ts), drawn as the strand lattice's net player
  // (evaluators/rust-ic-strands/player/rev.html) draws its nets: agents on wires, laid out by springs, the
  // pairs about to meet in gold, and a bar under it with the whole history, which can be scrubbed either
  // way. A walker is a run: the program nodes it has come down through point back up to it (its way back,
  // drawn thick), so where it is in the tree is where the run is. ⇆ turns every walker around.
  import { onMount } from "svelte";
  import { parseProg, parseVal, valText, type Reads } from "$lib/twoway/text";
  import { PRESETS } from "$lib/twoway/library";
  import type { V } from "$lib/twoway/core";
  import { Net, load, finished, idOf, portOf, ROLE, ROLE_NAMES, ATOM_NAMES, PS_, PFL_, PFR_, type Agent, type Tag } from "./net";

  const EXAMPLES = PRESETS.filter((p) => p.name !== "addw");
  const FIRST = EXAMPLES.findIndex((p) => p.name === "not");
  let preset = $state(FIRST);
  let progSrc = $state(EXAMPLES[FIRST].prog);
  let inputSrc = $state(EXAMPLES[FIRST].input);
  let reads = $state<Reads>(EXAMPLES[FIRST].reads);
  let byRound = $state(true);
  let speed = $state(40);
  let status = $state("loading…");
  let statusBad = $state(false);
  let answer = $state("");
  let counts = $state<[string, string | number][]>([]);
  let detail = $state("point at an agent");
  /** -2 to the start, -1 back, 0 still, 1 forward, 2 to the end. */
  let mode = $state(0);
  const CAP = 150_000;

  let net: Net = new Net();
  let loaded: { p: V; x: V } | null = null;
  let long = false;

  // ---- what each agent is ----

  /** Ports that lead up, toward the top: the rest hang below in the layout. */
  const UP: Record<Tag, number[]> = {
    L: [0], S: [0], F: [0], Top: [],
    Wd: [1], Wu: [0],
    PS: [1], PFL: [1], PFR: [1], PL: [1], Kself: [1], Keval: [1],
    Mv: [1], Mvd: [0], Mv2: [1],
    Mm: [1], Mmr: [0], J: [1], J2: [1],
    Mt: [1], Mt2: [1], Mtb: [1], Mtc: [0], Mtcc: [1], Mtcc2: [1],
    Dup: [1, 2], Mrg: [2], MrgL: [1], MrgS: [2], MrgF: [3]
  };
  const isSource = (t: Tag, q: number) => UP[t].includes(q);
  type Side = "prog" | "val" | "env";
  /** What hangs from a port of an agent that is not a tree node: program, value or environment. */
  const SIDE: Partial<Record<Tag, Record<number, Side>>> = {
    Wd: { 0: "prog", 2: "val", 3: "env" }, Wu: { 1: "prog", 2: "val", 3: "env" },
    PS: { 0: "prog" }, PFL: { 0: "prog", 2: "prog" }, PFR: { 0: "prog", 2: "prog", 3: "env" }, Keval: { 2: "env" },
    Mv: { 0: "val", 2: "env" }, Mvd: { 1: "val", 2: "env" }, Mv2: { 0: "val", 2: "env", 3: "val" },
    Mm: { 0: "val", 2: "env", 3: "prog", 4: "prog" }, Mmr: { 1: "val", 2: "env" },
    J2: { 2: "prog", 3: "val", 4: "env" },
    Mt: { 0: "val", 2: "env", 3: "prog", 4: "prog" }, Mt2: { 0: "val", 2: "env", 3: "prog", 4: "prog", 5: "val" },
    Mtb: { 0: "val", 2: "env", 3: "prog" }, Mtc: { 1: "prog", 2: "env", 3: "val" }, Mtcc: { 0: "val", 2: "env", 3: "prog" },
    Mtcc2: { 0: "val", 2: "env", 3: "prog", 4: "val" },
    Dup: { 0: "env", 1: "env", 2: "env" }, Mrg: { 0: "env", 1: "env", 2: "env" }, MrgL: { 0: "env", 1: "env" },
    MrgS: { 0: "env", 1: "env", 2: "env" }, MrgF: { 0: "env", 1: "env", 2: "env", 3: "env" }
  };
  const TREE = new Set<Tag>(["L", "S", "F"]);
  const CONTEXT = new Set<Tag>(["PS", "PFL", "PFR", "PL", "Kself", "Keval", "J", "J2"]);
  const WALKER = new Set<Tag>(["Wd", "Wu"]);
  const MOVER = new Set<Tag>(["Mv", "Mvd", "Mv2"]);
  const READER = new Set<Tag>(["Mm", "Mmr", "Mt", "Mt2", "Mtb", "Mtc", "Mtcc", "Mtcc2"]);
  const COPY = new Set<Tag>(["Dup", "Mrg", "MrgL", "MrgS", "MrgF"]);

  const PS_NAME = ["an instruction's stem", "a sequence's first part", "one of a move's stems"];
  const PFL_NAME = ["a fork, read by its left", "a fix's or map's fork, read by its left", "map's inner fork, its first part running inside a stem", "tri's inner fork, its first part running"];
  const PFR_NAME = ["a †: its body running the other way", "a sequence: its second part running", "fix: a copy of its body running", "map's inner fork: its second part running on the left half", "tri: its second part running", "tri: its third part running"];
  function describe(a: Agent): string {
    const dir = a.e ? "forward" : "backward";
    switch (a.tag) {
      case "L": case "S": case "F":
        return `a ${{ L: "leaf", S: "stem", F: "fork" }[a.tag]} of a tree`;
      case "Top": return "the top: the run's result comes back here";
      case "Wd": return `a walker going down (${dir}), about to read the node below: ${roleText(a.k)}`;
      case "Wu": return `a walker going up (${dir}), carrying the part it has run: ${roleText(a.k)}`;
      case "PS": return `on a walker's way back: ${PS_NAME[a.k]}, its edge turned to point down at the walker`;
      case "PFL": return `on a walker's way back: ${PFL_NAME[a.k]}; it holds the right half until the walker comes back`;
      case "PFR": return `on a walker's way back: ${PFR_NAME[a.k]}; it holds the left half`;
      case "PL": return `a ${ATOM_NAMES[a.k]}'s leaf, its mover below on the value`;
      case "Kself": return "self: waiting for its copy of the fix body to run, then merging it back";
      case "Keval": return "eval: waiting for the program it runs from the value";
      case "Mv": return `a mover (${dir}) at the value's root, about to ${ATOM_NAMES[a.k]}`;
      case "Mvd": return `a mover (${dir}) that has made its ${ATOM_NAMES[a.k]}, turning back to the program`;
      case "Mv2": return `a mover (${dir}) halfway through ${ATOM_NAMES[a.k]}: it holds part of the value while it reads the next node`;
      case "Mm": return "map reading the value: a leaf, a stem or a fork";
      case "Mmr": return "map starting its right half";
      case "J": return "map's join: waiting for the left half to come back";
      case "J2": return "map's join: the left half is back, waiting for the right";
      case "Mt": case "Mt2": return "tri reading the value's left part, to choose a part";
      case "Mtb": case "Mtc": case "Mtcc": case "Mtcc2": return "tri putting the left part back together";
      case "Dup": return "a duplicator: copies a tree a node at a time (the fix body, for self and for both halves of a map)";
      default: return "a merger: merges two copies of a tree back into one, a node at a time";
    }
  }
  function roleText(k: number): string {
    if (k === ROLE.SKEL) return "a part of the sequence";
    if (k === ROLE.FL) return "the left of a fork, which says what the fork is";
    if (k === ROLE.DG) return "a †'s leaf";
    if (k === ROLE.SA || k === ROLE.SB) return "a sequence's first part, on the way to the second";
    if (k === ROLE.SC) return "under one stem: self, or a move, fix or map";
    if (k >= 6 && k <= 11) return `under ${k - 4} stems: ${ATOM_NAMES[k - 4]}`;
    if (k === ROLE.FML) return "the left of a fix's or map's fork";
    if (k === ROLE.FX) return "a fix's leaf";
    return "map's inner fork";
  }
  /** The mark an agent wears. */
  function glyph(a: Agent): string {
    switch (a.tag) {
      case "Wd": return "↓";
      case "Wu": return "↑";
      case "Mv": case "Mvd": case "Mv2": return ["", "↩", "⇄", "×", "⇅", "+", "⊕", "▷"][a.k] ?? "";
      case "PS": return a.k === PS_.SQ ? ";" : "|";
      case "PFL": return a.k === PFL_.SK ? "<" : a.k === PFL_.FM ? "μ" : a.k === PFL_.MAPS ? "∥" : "?";
      case "PFR": return a.k === PFR_.DG ? "†" : a.k === PFR_.SQ ? ";" : a.k === PFR_.FX ? "μ" : a.k === PFR_.MAPB ? "∥" : "?";
      case "PL": return "·";
      case "Kself": return "↩";
      case "Keval": return "▷";
      case "J": case "J2": case "Mm": case "Mmr": return "∥";
      case "Mt": case "Mt2": case "Mtb": case "Mtc": case "Mtcc": case "Mtcc2": return "?";
      case "Top": return "⊤";
    }
    return "";
  }

  /** The kind of thing an event did, for its colour on the bar. */
  const FAMILIES: [string, string, string][] = [
    ["walk", "#7a5cc4", "a walker moving through its program a node at a time"],
    ["move", "#c98e2b", "a mover rearranging the value"],
    ["call", "#3d9b74", "self, eval, fix, map and tri: starting and closing parts"],
    ["copy", "#9a6fb0", "copying a fix body and merging copies back"],
    ["flip", "#5a90cf", "every walker turned around"]
  ];
  const MOVES = /^(swap|cross|sink|stem†?|unit†?|eval)( \((reading the root|a leaf beside|turning to the value)\))?$/;
  function family(rule: string): number {
    if (rule === "flip") return 4;
    if (rule.startsWith("copy") || rule.startsWith("merge")) return 3;
    if (MOVES.test(rule) || rule.startsWith("move done")) return 1;
    if (/^(self|fix \((into|out)|map|tri|eval \(done)/.test(rule)) return 2;
    return 0;
  }

  // ---- the net as arrays, for layout and drawing ----

  interface View {
    ag: Agent[];
    index: Map<number, number>;
    side: (Side | null)[];
  }
  let V0: View = { ag: [], index: new Map(), side: [] };
  function snapshot(): View {
    const ag = [...net.agents.values()];
    const index = new Map(ag.map((a, i) => [a.id, i]));
    // a tree node is on the side of the first agent above it that is not a tree node
    const side: (Side | null)[] = new Array(ag.length).fill(null);
    const sideOf = (i: number, depth = 0): Side | null => {
      if (side[i] || depth > 4000) return side[i];
      const a = ag[i];
      const up = a.ports[0];
      if (up < 0) return null;
      const j = index.get(idOf(up));
      if (j === undefined) return null;
      const b = ag[j];
      const s = TREE.has(b.tag) ? sideOf(j, depth + 1) : (SIDE[b.tag]?.[portOf(up)] ?? null);
      return (side[i] = s);
    };
    ag.forEach((a, i) => TREE.has(a.tag) && sideOf(i));
    return { ag, index, side };
  }

  // ---- layout: springs along wires, what hangs from a port below it (as rev.html) ----
  const GSP = 26, GRAD = 6, GREP = 26;
  const POS = new Map<number, number[]>();
  let alpha = 1;
  let viewT = { s: 2, ox: 0, oy: 0 };
  const jitter = (id: number, salt: number) => (Math.imul(id * 2 + salt, 2654435761) >>> 0) / 4294967296 - 0.5;
  const far = (i: number, q: number): number => {
    const p = V0.ag[i].ports[q];
    if (p === undefined || p < 0) return -1;
    const j = V0.index.get(idOf(p));
    return j === undefined ? -1 : j * 8 + portOf(p);
  };
  function place() {
    const { ag } = V0;
    const n = ag.length;
    const at = (i: number) => POS.has(ag[i].id);
    let fresh = 0;
    const queue: number[] = [];
    for (let i = 0; i < n; i++)
      if (!at(i)) {
        fresh++;
        for (let q = 0; q < ag[i].ports.length; q++) {
          const f = far(i, q);
          if (f >= 0 && at(f >> 3)) {
            queue.push(i);
            break;
          }
        }
      }
    for (let h = 0; h < queue.length; h++) {
      const i = queue[h];
      if (at(i)) continue;
      let sx = 0, sy = 0, c = 0;
      for (let q = 0; q < ag[i].ports.length; q++) {
        const f = far(i, q), j = f >> 3;
        if (f < 0 || j === i || !at(j)) continue;
        const p = POS.get(ag[j].id)!;
        sx += p[0];
        sy += p[1] + (isSource(ag[i].tag, q) ? 0.6 : -0.6) * GSP;
        c++;
      }
      if (!c) continue;
      POS.set(ag[i].id, [sx / c + jitter(ag[i].id, 0) * GSP * 0.5, sy / c + jitter(ag[i].id, 1) * GSP * 0.2, 0, 0]);
      for (let q = 0; q < ag[i].ports.length; q++) {
        const f = far(i, q);
        if (f >= 0 && !at(f >> 3)) queue.push(f >> 3);
      }
    }
    hang(at, POS.size > 0);
    for (const k of [...POS.keys()]) if (!V0.index.has(k)) POS.delete(k);
    alpha = Math.max(alpha, Math.min(0.6, 0.15 + (2 * fresh) / Math.max(1, n)));
  }
  function inputs(i: number): number[] {
    const out: number[] = [];
    const a = V0.ag[i];
    for (let q = 0; q < a.ports.length; q++) {
      const f = far(i, q);
      if (f >= 0 && !isSource(a.tag, q)) out.push(f >> 3);
    }
    return out;
  }
  /** Lays out what is not yet placed as tidy trees, each hung from a top (after Reingold and Tilford). */
  function hang(at: (i: number) => boolean, beside: boolean) {
    const { ag } = V0;
    const n = ag.length;
    const starts: number[] = [];
    for (let i = 0; i < n; i++) {
      if (at(i)) continue;
      let read = false;
      for (let q = 0; q < ag[i].ports.length; q++) {
        const f = far(i, q);
        if (f >= 0 && isSource(ag[i].tag, q) && !at(f >> 3)) read = true;
      }
      if (!read) starts.push(i);
    }
    starts.sort((a, b) => +(ag[b].tag === "Top") - +(ag[a].tag === "Top"));
    for (let i = 0; i < n; i++) if (!at(i)) starts.push(i);
    const order: number[] = [], tops: number[] = [];
    const depth = new Int32Array(n), first = new Int32Array(n).fill(-1), last = new Int32Array(n).fill(-1), next = new Int32Array(n).fill(-1), seen = new Uint8Array(n);
    for (const s0 of starts) {
      if (seen[s0]) continue;
      tops.push(s0);
      const stack = [s0, -1];
      while (stack.length) {
        const p = stack.pop()!, i = stack.pop()!;
        if (seen[i]) continue;
        seen[i] = 1;
        order.push(i);
        depth[i] = p < 0 ? 0 : depth[p] + 1;
        if (p >= 0) {
          if (last[p] < 0) first[p] = i;
          else next[last[p]] = i;
          last[p] = i;
        }
        const ins = inputs(i);
        for (let k = ins.length - 1; k >= 0; k--) if (!seen[ins[k]] && !at(ins[k])) stack.push(ins[k], i);
      }
    }
    if (!order.length) return;
    type Outline = { l: number[]; r: number[]; off: number; bottom: number };
    const ROW = GSP * 0.85, outline: (Outline | null)[] = new Array(n), rel = new Float64Array(n);
    const kidsOf = (i: number) => {
      const out: number[] = [];
      for (let c = first[i]; c >= 0; c = next[c]) out.push(c);
      return out;
    };
    const join = (kids: number[], d: number): Outline => {
      let acc: Outline | null = null, end = 0;
      for (const c of kids) {
        const o = outline[c]!;
        outline[c] = null;
        if (!acc) {
          acc = o;
          rel[c] = 0;
          continue;
        }
        let need = -Infinity;
        for (let e = d, lo = Math.min(acc.bottom, o.bottom); e <= lo; e++) need = Math.max(need, acc.r[acc.bottom - e] + acc.off - o.l[o.bottom - e] - o.off);
        o.off += need + GSP;
        rel[c] = end = need + GSP;
        if (o.bottom > acc.bottom) {
          for (let e = d; e <= acc.bottom; e++) o.l[o.bottom - e] = acc.l[acc.bottom - e] + acc.off - o.off;
          acc = o;
        } else for (let e = d; e <= o.bottom; e++) acc.r[acc.bottom - e] = o.r[o.bottom - e] + o.off - acc.off;
      }
      acc!.off -= end / 2;
      for (const c of kids) rel[c] -= end / 2;
      return acc!;
    };
    for (let k = order.length - 1; k >= 0; k--) {
      const i = order[k];
      if (first[i] < 0) {
        outline[i] = { l: [0], r: [0], off: 0, bottom: depth[i] };
        continue;
      }
      const o = join(kidsOf(i), depth[i] + 1);
      o.l.push(-o.off);
      o.r.push(-o.off);
      outline[i] = o;
    }
    join(tops, 0);
    let x0 = Infinity, x1 = -Infinity, deepest = 0;
    for (const i of order) {
      for (let c = first[i]; c >= 0; c = next[c]) rel[c] += rel[i];
      x0 = Math.min(x0, rel[i]);
      x1 = Math.max(x1, rel[i]);
      deepest = Math.max(deepest, depth[i]);
    }
    let dx = -(x0 + x1) / 2, dy = (-deepest * ROW) / 2;
    if (beside) {
      let right = -Infinity, top = Infinity;
      for (const [, p] of POS) {
        right = Math.max(right, p[0]);
        top = Math.min(top, p[1]);
      }
      dx = right + GSP * 2 - x0;
      dy = top;
    }
    for (const i of order) POS.set(ag[i].id, [rel[i] + dx, depth[i] * ROW + dy, 0, 0]);
  }
  function relax(steps: number) {
    const { ag } = V0;
    const n = ag.length;
    if (!n) return;
    const x = new Float64Array(n), y = new Float64Array(n), vx = new Float64Array(n), vy = new Float64Array(n), deg = new Uint8Array(n);
    for (let i = 0; i < n; i++) {
      const p = POS.get(ag[i].id)!;
      x[i] = p[0];
      y[i] = p[1];
      vx[i] = p[2];
      vy[i] = p[3];
    }
    const wa: number[] = [], wb: number[] = [], src: number[] = [];
    for (let i = 0; i < n; i++)
      for (let q = 0; q < ag[i].ports.length; q++) {
        const f = far(i, q), j = f >> 3;
        if (f < 0 || j === i) continue;
        deg[i]++;
        if (j < i) continue;
        wa.push(i);
        wb.push(j);
        src.push(isSource(ag[i].tag, q) ? i : isSource(ag[j].tag, f & 7) ? j : -1);
      }
    for (let s = 0; s < steps && alpha > 0.004; s++, alpha *= 0.985) {
      const a = alpha;
      for (let w = 0; w < wa.length; w++) {
        const i = wa[w], j = wb[w];
        let dx = x[j] + vx[j] - x[i] - vx[i], dy = y[j] + vy[j] - y[i] - vy[i];
        const d = Math.sqrt(dx * dx + dy * dy) || 0.01, k = (((d - GSP) / d) * a) / Math.min(deg[i], deg[j]), bias = deg[i] / (deg[i] + deg[j]);
        dx *= k;
        dy *= k;
        vx[j] -= dx * bias;
        vy[j] -= dy * bias;
        vx[i] += dx * (1 - bias);
        vy[i] += dy * (1 - bias);
        const o = src[w];
        if (o >= 0) {
          const r = o === i ? j : i, short = GSP * 0.75 - (y[o] - y[r]);
          if (short > 0) {
            const f = short * a * 0.12;
            vy[o] += f;
            vy[r] -= f;
          }
        }
      }
      const cells = new Map<number, number[]>(), R = GREP, kr = a * 0.3;
      for (let i = 0; i < n; i++) {
        const c = Math.floor(x[i] / R) * 65536 + Math.floor(y[i] / R);
        let l = cells.get(c);
        if (!l) cells.set(c, (l = []));
        l.push(i);
      }
      for (let i = 0; i < n; i++) {
        const cx = Math.floor(x[i] / R), cy = Math.floor(y[i] / R);
        for (let ox = -1; ox <= 1; ox++)
          for (let oy = -1; oy <= 1; oy++)
            for (const j of cells.get((cx + ox) * 65536 + cy + oy) ?? []) {
              if (j <= i) continue;
              let dx = x[j] - x[i], dy = y[j] - y[i], d2 = dx * dx + dy * dy;
              if (d2 >= R * R) continue;
              if (d2 < 1e-6) {
                dx = jitter(i, 0);
                dy = jitter(j, 1);
                d2 = dx * dx + dy * dy;
              }
              const d = Math.sqrt(d2), f = ((R - d) / d) * kr;
              vx[j] += dx * f;
              vy[j] += dy * f;
              vx[i] -= dx * f;
              vy[i] -= dy * f;
            }
      }
      let mx = 0, my = 0;
      for (let i = 0; i < n; i++) {
        mx += x[i];
        my += y[i];
      }
      mx = (mx / n) * a * 0.02;
      my = (my / n) * a * 0.02;
      for (let i = 0; i < n; i++) {
        let u = (vx[i] - mx) * 0.6, v = (vy[i] - my) * 0.6;
        const sp = Math.hypot(u, v);
        if (sp > GSP / 3) {
          u *= GSP / 3 / sp;
          v *= GSP / 3 / sp;
        }
        vx[i] = u;
        vy[i] = v;
        x[i] += u;
        y[i] += v;
      }
    }
    for (let i = 0; i < n; i++) POS.set(ag[i].id, [x[i], y[i], vx[i], vy[i]]);
  }
  let follow = true;
  function fit(glide = false) {
    if (!cv) return;
    let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
    for (const [, p] of POS) {
      x0 = Math.min(x0, p[0]);
      y0 = Math.min(y0, p[1]);
      x1 = Math.max(x1, p[0]);
      y1 = Math.max(y1, p[1]);
    }
    if (!isFinite(x0)) (x0 = y0 = -GSP), (x1 = y1 = GSP);
    const box = cv.getBoundingClientRect(), pad = GSP, top = 40, bottom = 12;
    x0 -= pad; y0 -= pad; x1 += pad; y1 += pad;
    const s = Math.min(2.4, (box.width - 20) / (x1 - x0), (box.height - top - bottom) / (y1 - y0));
    const target = { s, ox: (box.width - (x1 - x0) * s) / 2 - x0 * s, oy: top + (box.height - top - bottom - (y1 - y0) * s) / 2 - y0 * s };
    if (!glide) return void (viewT = target);
    const W2 = box.width / 2, H2 = box.height / 2, k = 0.12;
    const ns = viewT.s * (target.s / viewT.s) ** k, mx = (W2 - viewT.ox) / viewT.s, my = (H2 - viewT.oy) / viewT.s;
    const tx = (W2 - target.ox) / target.s, ty = (H2 - target.oy) / target.s, cx = mx + (tx - mx) * k, cy = my + (ty - my) * k;
    viewT = { s: ns, ox: W2 - cx * ns, oy: H2 - cy * ns };
  }

  // ---- running ----

  let dirty = true;
  const rate = () => 10 ** (speed / 25 - 1.2);
  let credit = 0;
  /** Event numbers where rounds begin, and the end. */
  const roundEnds = () => [...net.rounds.filter((r) => r < net.events.length), net.events.length];
  function roundsDone(t: number) {
    const R = roundEnds();
    let r = 0;
    while (r + 1 < R.length && R[r + 1] <= t) r++;
    return r;
  }
  /** Forward n steps (rounds or events); past the history, the net computes new rounds. */
  function forward(n: number): number {
    let moved = 0;
    for (let k = 0; k < n; k++) {
      if (net.pos < net.events.length) {
        if (byRound) {
          const R = roundEnds();
          const r = roundsDone(net.pos);
          net.seek(R[Math.min(R.length - 1, r + 1)]);
        } else net.redo();
        moved++;
      } else if (net.events.length < CAP && net.round()) moved++;
      else break;
    }
    if (moved) dirty = true;
    return moved;
  }
  function back(n: number): number {
    let moved = 0;
    for (let k = 0; k < n && net.pos > 0; k++) {
      if (byRound) {
        const R = roundEnds();
        const r = roundsDone(net.pos);
        net.seek(R[R[r] < net.pos ? r : Math.max(0, r - 1)]);
      } else net.undo();
      moved++;
    }
    if (moved) dirty = true;
    return moved;
  }
  function runToEnd() {
    while (net.events.length < CAP && net.round());
    long = net.events.length >= CAP;
  }
  function loadRun() {
    let p: V, x: V;
    try {
      p = parseProg(progSrc);
      x = parseVal(inputSrc);
    } catch (e) {
      status = (e as Error).message;
      statusBad = true;
      return;
    }
    net = load(p, x);
    loaded = { p, x };
    status = "running it once…";
    runToEnd();
    net.seek(0);
    POS.clear();
    alpha = 1;
    mode = 0;
    dirty = true;
    sync();
    alpha = 0.3;
    relax(150);
    follow = true;
    fit();
  }
  /** Flip at this point of the history: what came after is dropped, every walker turns, and the run
   * back is computed so the bar holds it. */
  function flipNow() {
    mode = 0;
    net.flip();
    const at = net.pos;
    runToEnd();
    net.seek(at);
    dirty = true;
  }
  function sync() {
    if (!dirty) return;
    dirty = false;
    V0 = snapshot();
    place();
    hud();
  }
  function hud() {
    const fin = finished(net);
    answer = fin ? `${valText(fin.val, reads)}${fin.fwd ? "" : " (run back)"}` : "";
    const R = roundEnds();
    const r = roundsDone(net.pos);
    const pairs = net.pairs().length;
    statusBad = false;
    if (net.pos === 0) status = "the start: the walker at the top of its program, the value hanging from it";
    else if (fin && net.pos === net.events.length) status = fin.fwd ? "the end: the walker is back at the top, the program whole again beside the result" : "back at the start, run backward: ⇆ to run it forward again";
    else if (!pairs && !fin) {
      status = "stuck: no rule fits here";
      statusBad = true;
    } else status = byRound ? `round ${r} of ${R.length - 1}` : `event ${net.pos} of ${net.events.length}`;
    if (long) status += ` (stopped at ${CAP.toLocaleString("en")} events)`;
    let walkers = 0, contexts = 0, copies = 0, movers = 0;
    for (const a of V0.ag) {
      if (WALKER.has(a.tag)) walkers++;
      else if (CONTEXT.has(a.tag)) contexts++;
      else if (COPY.has(a.tag)) copies++;
      else if (MOVER.has(a.tag) || READER.has(a.tag)) movers++;
    }
    counts = [
      ["agents", V0.ag.length],
      ["walkers / on their ways back", `${walkers} / ${contexts}`],
      ["movers and readers on values", movers],
      ["duplicators and mergers", copies],
      ["pairs about to meet", pairs],
      ["events done / in the history", `${net.pos.toLocaleString("en")} / ${net.events.length.toLocaleString("en")}`],
      ["rounds", `${r} of ${R.length - 1}`]
    ];
  }

  // ---- drawing ----

  let cv: HTMLCanvasElement | undefined = $state();
  let sv: HTMLCanvasElement | undefined = $state();
  let host: HTMLDivElement | undefined = $state();
  type Pal = Record<"bg" | "wire" | "gold" | "fwd" | "bwd" | "prog" | "val" | "env" | "ctx" | "call" | "copy" | "fg" | "faint" | "white", string>;
  let P: Pal | null = null;
  function readPalette() {
    if (!host) return;
    const probe = document.createElement("span");
    probe.style.display = "none";
    host.appendChild(probe);
    const px = document.createElement("canvas");
    px.width = px.height = 1;
    const c = px.getContext("2d", { willReadFrequently: true })!;
    const rd = (css: string) => {
      probe.style.color = "";
      probe.style.color = css;
      c.clearRect(0, 0, 1, 1);
      c.fillStyle = getComputedStyle(probe).color;
      c.fillRect(0, 0, 1, 1);
      const d = c.getImageData(0, 0, 1, 1).data;
      return `rgba(${d[0]},${d[1]},${d[2]},${(d[3] / 255).toFixed(3)})`;
    };
    P = {
      bg: rd("var(--bg-elev)"), wire: rd("color-mix(in oklab, var(--fg) 30%, transparent)"), gold: "#e0a526",
      fwd: "#d08a1e", bwd: "#4f86c6", prog: "#9a7ad6", val: "#4aa96c", env: rd("color-mix(in oklab, #9a7ad6 45%, var(--bg-elev))"),
      ctx: "#7a5cc4", call: "#3d9b74", copy: "#b07fcf", fg: rd("var(--fg)"), faint: rd("var(--fg-faint)"), white: "#ffffff"
    };
    probe.remove();
  }
  let hover: number | null = null;
  function draw() {
    if (!cv || !P) return;
    const ctx = cv.getContext("2d")!;
    const d = devicePixelRatio || 1, r = cv.getBoundingClientRect();
    if (cv.width !== Math.round(r.width * d) || cv.height !== Math.round(r.height * d)) {
      cv.width = Math.round(r.width * d);
      cv.height = Math.round(r.height * d);
    }
    ctx.setTransform(1, 0, 0, 1, 0, 0);
    ctx.fillStyle = P.bg;
    ctx.fillRect(0, 0, cv.width, cv.height);
    ctx.setTransform(d * viewT.s, 0, 0, d * viewT.s, d * viewT.ox, d * viewT.oy);
    const lw = (w: number) => w / viewT.s;
    const { ag, side } = V0;
    const Pq = (i: number) => POS.get(ag[i].id)!;
    // wires: a pair about to meet in gold; a walker's way back thick, in its direction's colour
    const plain = new Path2D(), gold = new Path2D(), wayF = new Path2D(), wayB = new Path2D();
    for (let i = 0; i < ag.length; i++)
      for (let q = 0; q < ag[i].ports.length; q++) {
        const f = far(i, q), j = f >> 3;
        if (f < 0 || j < i) continue;
        const a = Pq(i), b = Pq(j), p = f & 7;
        const ti = ag[i].tag, tj = ag[j].tag;
        let path = plain;
        if (q === 0 && p === 0) path = gold;
        else if (!TREE.has(ti) && !TREE.has(tj) && !COPY.has(ti) && !COPY.has(tj) && (isSource(ti, q) || isSource(tj, p))) path = wayDir(isSource(ti, q) ? i : j) ? wayF : wayB;
        path.moveTo(a[0], a[1]);
        path.lineTo(b[0], b[1]);
      }
    ctx.lineCap = "round";
    ctx.strokeStyle = P.wire;
    ctx.lineWidth = lw(1.1);
    ctx.stroke(plain);
    ctx.globalAlpha = 0.55;
    ctx.lineWidth = lw(4);
    ctx.strokeStyle = P.fwd;
    ctx.stroke(wayF);
    ctx.strokeStyle = P.bwd;
    ctx.stroke(wayB);
    ctx.globalAlpha = 1;
    ctx.strokeStyle = P.gold;
    ctx.lineWidth = lw(2.6);
    ctx.stroke(gold);
    // agents
    ctx.textAlign = "center";
    ctx.textBaseline = "middle";
    const big = viewT.s * GRAD > 4;
    for (let i = 0; i < ag.length; i++) {
      const a = ag[i], [x, y] = Pq(i);
      const t = a.tag;
      if (TREE.has(t)) {
        ctx.fillStyle = side[i] === "val" ? P.val : side[i] === "env" ? P.env : P.prog;
        ctx.beginPath();
        if (t === "L") ctx.ellipse(x, y, GRAD * 0.55, GRAD * 0.38, -0.5, 0, 7);
        else ctx.arc(x, y, t === "F" ? GRAD * 0.5 : GRAD * 0.36, 0, 7);
        ctx.fill();
        continue;
      }
      const dirC = a.e ? P.fwd : P.bwd;
      if (WALKER.has(t)) {
        ctx.fillStyle = dirC;
        ctx.beginPath();
        ctx.arc(x, y, GRAD * 1.25, 0, 7);
        ctx.fill();
        ctx.strokeStyle = P.white;
        ctx.lineWidth = lw(1.4);
        ctx.stroke();
      } else if (MOVER.has(t) || READER.has(t)) {
        const s = GRAD * 1.15;
        ctx.fillStyle = READER.has(t) ? P.call : dirC;
        ctx.beginPath();
        ctx.moveTo(x, y - s); ctx.lineTo(x + s, y); ctx.lineTo(x, y + s); ctx.lineTo(x - s, y); ctx.closePath();
        ctx.fill();
      } else if (COPY.has(t)) {
        const s = GRAD * 0.8, up = t === "Dup" ? 1 : -1;
        ctx.fillStyle = P.copy;
        ctx.beginPath();
        ctx.moveTo(x, y + s * up); ctx.lineTo(x + s, y - s * up); ctx.lineTo(x - s, y - s * up); ctx.closePath();
        ctx.fill();
      } else if (t === "Top") {
        ctx.fillStyle = P.faint;
        ctx.fillRect(x - GRAD * 0.8, y - GRAD * 0.8, GRAD * 1.6, GRAD * 1.6);
      } else {
        ctx.fillStyle = P.bg;
        ctx.strokeStyle = t === "J" || t === "J2" || t === "Kself" || t === "Keval" ? P.call : P.ctx;
        ctx.lineWidth = lw(1.6);
        ctx.beginPath();
        ctx.arc(x, y, GRAD * 0.95, 0, 7);
        ctx.fill();
        ctx.stroke();
      }
      if (big) {
        const g = glyph(a);
        if (g) {
          ctx.fillStyle = WALKER.has(t) || MOVER.has(t) || READER.has(t) ? P.white : t === "Top" ? P.bg : P.ctx;
          ctx.font = `700 ${GRAD * (WALKER.has(t) ? 1.5 : 1.15)}px ui-monospace, monospace`;
          ctx.fillText(g, x, y + GRAD * 0.06);
        }
      }
    }
    if (hover !== null && hover < ag.length) {
      const [x, y] = Pq(hover);
      ctx.strokeStyle = P.fg;
      ctx.lineWidth = lw(1.4);
      ctx.beginPath();
      ctx.arc(x, y, GRAD * 1.9, 0, 7);
      ctx.stroke();
    }
    drawStrip();
  }
  /** Which way the walker below a stretch of way back is going (true forward): down from a context is
   * its port 0. */
  function wayDir(lower: number): boolean {
    let k = lower;
    for (let n = 0; n < 4000; n++) {
      const a = V0.ag[k];
      if (WALKER.has(a.tag) || MOVER.has(a.tag) || READER.has(a.tag)) return a.e === 1;
      const f = far(k, 0);
      if (f < 0) break;
      k = f >> 3;
    }
    return true;
  }
  function drawStrip() {
    if (!sv || !P) return;
    const c = sv.getContext("2d")!;
    const d = devicePixelRatio || 1, r = sv.getBoundingClientRect(), W = Math.round(r.width * d), H = Math.round(r.height * d);
    if (sv.width !== W || sv.height !== H) {
      sv.width = W;
      sv.height = H;
    }
    c.fillStyle = P.bg;
    c.fillRect(0, 0, W, H);
    const R = roundEnds(), rounds = R.length - 1;
    if (rounds > 0) {
      let top = 1;
      for (let k = 0; k < rounds; k++) top = Math.max(top, R[k + 1] - R[k]);
      const per = Math.max(1, rounds / W), cols = Math.ceil(rounds / per), cw = W / cols, bw = Math.max(cw - (cw > 3 ? 1 : 0), 1);
      for (let col = 0; col < cols; col++) {
        const a = Math.floor(col * per), b = Math.min(rounds, Math.floor((col + 1) * per) || a + 1);
        const tally = [0, 0, 0, 0, 0];
        let widest = 0;
        for (let k = a; k < b; k++) {
          widest = Math.max(widest, R[k + 1] - R[k]);
          for (let e = R[k]; e < R[k + 1]; e++) tally[family(net.events[e].rule)]++;
        }
        const h = Math.max(2 * d, ((H - 6 * d) * Math.log1p(widest)) / Math.log1p(top));
        c.fillStyle = FAMILIES[tally.indexOf(Math.max(...tally))][1];
        c.globalAlpha = R[a] < net.pos ? 0.85 : 0.25;
        c.fillRect(col * cw, H - h, bw, h);
        c.globalAlpha = 1;
      }
      const x = (roundsDone(net.pos) / rounds) * W;
      c.fillStyle = P.fg;
      c.fillRect(Math.min(W - 2 * d, Math.max(0, x - d)), 0, 2 * d, H);
    }
  }

  // ---- input ----

  function worldAt(cx: number, cy: number) {
    const r = cv!.getBoundingClientRect();
    return [(cx - r.left - viewT.ox) / viewT.s, (cy - r.top - viewT.oy) / viewT.s];
  }
  function pointedAt(cx: number, cy: number): number | null {
    const [wx, wy] = worldAt(cx, cy), reach = Math.max(GRAD * 1.5, 8 / viewT.s) ** 2;
    let best: number | null = null, bd = reach;
    V0.ag.forEach((a, i) => {
      const p = POS.get(a.id);
      if (!p) return;
      const dd = (p[0] - wx) ** 2 + (p[1] - wy) ** 2;
      if (dd < bd) (bd = dd), (best = i);
    });
    return best;
  }
  function showDetail() {
    if (hover === null || hover >= V0.ag.length) return void (detail = "point at an agent; click one of a gold pair to fire it");
    const a = V0.ag[hover];
    const s = V0.side[hover];
    const live = net.agents.get(a.id);
    const meets = live && net.partner(live);
    detail = `<b>${a.tag}</b> (agent ${a.id})${s ? `, on ${s === "prog" ? "a program" : s === "val" ? "a value" : "an environment (a fix body kept for self)"}` : ""}: ${describe(a)}${meets ? `. It is about to meet the ${meets.tag} at its principal port: click to fire them.` : ""}`;
  }
  let drag: { x: number; y: number; ox: number; oy: number; moved: boolean } | null = null;
  function down(ev: PointerEvent) {
    drag = { x: ev.clientX, y: ev.clientY, ox: viewT.ox, oy: viewT.oy, moved: false };
    cv!.setPointerCapture(ev.pointerId);
  }
  function moveP(ev: PointerEvent) {
    if (drag) {
      const dx = ev.clientX - drag.x, dy = ev.clientY - drag.y;
      if (Math.abs(dx) + Math.abs(dy) > 3) drag.moved = true;
      if (drag.moved) {
        viewT = { ...viewT, ox: drag.ox + dx, oy: drag.oy + dy };
        follow = false;
      }
      return;
    }
    hover = pointedAt(ev.clientX, ev.clientY);
    showDetail();
  }
  function upP(ev: PointerEvent) {
    const was = drag;
    drag = null;
    if (!was || was.moved) return;
    const i = pointedAt(ev.clientX, ev.clientY);
    if (i === null) return;
    const a = net.agents.get(V0.ag[i].id);
    const b = a && net.partner(a);
    if (a && b) {
      mode = 0;
      net.events.length = net.pos;
      if (net.fire(a, b)) {
        net.rounds = net.rounds.filter((r) => r < net.pos - 1);
        net.rounds.push(net.pos - 1);
        dirty = true;
      }
    }
  }
  function wheel(ev: WheelEvent) {
    ev.preventDefault();
    const r = cv!.getBoundingClientRect(), k = Math.exp(-ev.deltaY * 0.0015), mx = ev.clientX - r.left, my = ev.clientY - r.top;
    viewT = { s: viewT.s * k, ox: mx - (mx - viewT.ox) * k, oy: my - (my - viewT.oy) * k };
    follow = false;
  }
  let scrubbing = false;
  function scrubTo(ev: PointerEvent) {
    const r = sv!.getBoundingClientRect();
    const R = roundEnds();
    const t = Math.round(Math.min(1, Math.max(0, (ev.clientX - r.left) / r.width)) * (R.length - 1));
    net.seek(R[t]);
    dirty = true;
  }
  function onKey(ev: KeyboardEvent) {
    const tag = (ev.target as HTMLElement).tagName;
    if (tag === "INPUT" || tag === "SELECT") return;
    const k = ev.key;
    if (k === "ArrowRight") (mode = 0), forward(1);
    else if (k === "ArrowLeft") (mode = 0), back(1);
    else if (k === " ") {
      ev.preventDefault();
      mode = ev.shiftKey ? (mode === -1 ? 0 : -1) : mode === 1 ? 0 : 1;
    } else if (k === "f") flipNow();
    else if (k === "End") mode = 2;
    else if (k === "Home") mode = -2;
    else return;
    ev.preventDefault();
  }
  function pick(i: number) {
    preset = i;
    progSrc = EXAMPLES[i].prog;
    inputSrc = EXAMPLES[i].input;
    reads = EXAMPLES[i].reads;
    loadRun();
  }

  onMount(() => {
    readPalette();
    loadRun();
    let raf = 0;
    let tick = 0;
    const frame = () => {
      if (mode === 2 && !forward(400)) mode = 0;
      else if (mode === -2 && !back(400)) mode = 0;
      else if (mode === 1 || mode === -1) {
        credit += rate();
        const k = Math.floor(credit);
        credit -= k;
        if (k && !(mode === 1 ? forward(k) : back(k))) mode = 0;
      }
      sync();
      relax(V0.ag.length > 3000 ? 1 : 3);
      if (follow) fit(true);
      draw();
      if (++tick % 90 === 0) readPalette();
      raf = requestAnimationFrame(frame);
    };
    raf = requestAnimationFrame(frame);
    const onResize = () => follow && fit();
    addEventListener("resize", onResize);
    return () => {
      cancelAnimationFrame(raf);
      removeEventListener("resize", onResize);
    };
  });
</script>

<svelte:window onkeydown={onKey} />

<div class="netvis" bind:this={host}>
  <div class="stage">
    <canvas
      bind:this={cv}
      class="cv"
      onpointerdown={down}
      onpointermove={moveP}
      onpointerup={upP}
      onpointerleave={() => ((hover = null), showDetail())}
      onwheel={wheel}
      aria-label="the interaction net"
    ></canvas>
    <div class="hud">
      <div class="answer">{answer}</div>
      <div class="status" class:bad={statusBad}>{status}</div>
    </div>
  </div>
  <div class="dock">
    <div class="row">
      <select aria-label="examples" value={preset} onchange={(e) => pick(+(e.target as HTMLSelectElement).value)}>
        {#each EXAMPLES as ex, i}<option value={i}>{ex.name}</option>{/each}
      </select>
      <input class="prog" spellcheck="false" bind:value={progSrc} onkeydown={(e) => e.key === "Enter" && loadRun()} aria-label="program" />
      <span class="on">on</span>
      <input class="inp" spellcheck="false" bind:value={inputSrc} onkeydown={(e) => e.key === "Enter" && loadRun()} aria-label="input" />
      <button onclick={loadRun} title="load (Enter)">load</button>
    </div>
    <canvas
      bind:this={sv}
      class="strip"
      onpointerdown={(e) => ((scrubbing = true), sv!.setPointerCapture(e.pointerId), (mode = 0), scrubTo(e))}
      onpointermove={(e) => scrubbing && scrubTo(e)}
      onpointerup={() => (scrubbing = false)}
      aria-label="the history: one column a round; drag to scrub"
    ></canvas>
    <div class="row">
      <button onclick={() => (mode = -2)} title="back to the start (Home)">⏮</button>
      <button class:on={mode === -1} onclick={() => (mode = mode === -1 ? 0 : -1)} title="play backward through the history (shift+space)">◀◀</button>
      <button onclick={() => ((mode = 0), back(1))} title="one step back (←)">◀</button>
      <button onclick={() => ((mode = 0), forward(1))} title="one step on (→)">▶</button>
      <button class:on={mode === 1} onclick={() => (mode = mode === 1 ? 0 : 1)} title="play (space)">▶▶</button>
      <button onclick={() => (mode = 2)} title="to the end (End)">⏭</button>
      <button class="flip" onclick={flipNow} title="flip (f): every walker turned around, nothing computed; what came after this point is dropped">⇆ flip</button>
      <label title="speed"><input type="range" min="0" max="100" bind:value={speed} /> speed</label>
      <label title="each step is a whole round (every pair about to meet fires at once), or one interaction"><input type="checkbox" bind:checked={byRound} onchange={() => (dirty = true)} /> a round at a time</label>
    </div>
  </div>
  <aside class="side">
    <h3>Now</h3>
    <div class="counts">
      {#each counts as [k, v]}<div><span>{k}</span><span>{v}</span></div>{/each}
    </div>
    <h3>Pointed at</h3>
    <div class="detail">{@html detail}</div>
    <h3>Legend</h3>
    <div class="legend">
      <span><i class="dot" style="background:#9a7ad6"></i>program</span>
      <span><i class="dot" style="background:#4aa96c"></i>value</span>
      <span><i class="dot" style="background:color-mix(in oklab, #9a7ad6 45%, var(--bg-elev))"></i>fix body kept for self</span>
      <span><i class="disc" style="background:#d08a1e"></i>walker, forward</span>
      <span><i class="disc" style="background:#4f86c6"></i>walker, backward</span>
      <span><i class="ring" style="border-color:#7a5cc4"></i>on a walker's way back</span>
      <span><i class="dia" style="background:#d08a1e"></i>mover on the value</span>
      <span><i class="dia" style="background:#3d9b74"></i>map or tri reading the value</span>
      <span><i class="tri"></i>copy / merge</span>
      <span><i class="bar" style="background:#e0a526"></i>about to meet</span>
    </div>
    <h3>The bar</h3>
    <div class="legend">
      {#each FAMILIES as [n, c, d]}<span title={d}><i class="dot" style="background:{c}"></i>{n}</span>{/each}
    </div>
  </aside>
</div>

<style>
  .netvis {
    display: grid;
    grid-template-columns: 1fr 260px;
    grid-template-rows: auto auto;
    gap: 0.6rem;
    border: 1px solid var(--border);
    border-radius: 12px;
    background: var(--bg-elev);
    padding: 0.7rem;
  }
  .stage {
    position: relative;
    grid-column: 1;
    grid-row: 1;
    height: 560px;
  }
  .cv {
    position: absolute;
    inset: 0;
    width: 100%;
    height: 100%;
    cursor: grab;
    border-radius: 8px;
    touch-action: none;
  }
  .hud {
    position: absolute;
    top: 6px;
    left: 10px;
    right: 10px;
    pointer-events: none;
    font-family: var(--font-mono);
  }
  .answer {
    font-size: 1.05rem;
    color: var(--fg);
  }
  .status {
    font-size: 0.78rem;
    color: var(--fg-muted);
  }
  .status.bad {
    color: var(--err);
  }
  .dock {
    grid-column: 1;
    grid-row: 2;
    display: flex;
    flex-direction: column;
    gap: 0.4rem;
  }
  .row {
    display: flex;
    gap: 0.35rem;
    align-items: center;
    flex-wrap: wrap;
    justify-content: center;
    font-size: 0.82rem;
  }
  .row input.prog {
    flex: 1;
    min-width: 12rem;
  }
  .row input.inp {
    width: 10rem;
  }
  .row input,
  .row select,
  .row button {
    font: inherit;
    font-family: var(--font-mono);
    background: var(--bg);
    color: var(--fg);
    border: 1px solid var(--border-strong);
    border-radius: 6px;
    padding: 2px 8px;
  }
  .row button {
    cursor: pointer;
    min-width: 2.2rem;
  }
  .row button.on,
  .row button:hover {
    border-color: var(--accent);
    color: var(--accent);
  }
  .row button.flip {
    color: #4f86c6;
  }
  .row label {
    color: var(--fg-muted);
    display: inline-flex;
    gap: 0.25rem;
    align-items: center;
  }
  .on {
    color: var(--fg-faint);
  }
  .strip {
    width: 100%;
    height: 40px;
    cursor: crosshair;
    border-bottom: 1px solid var(--border-strong);
  }
  .side {
    grid-column: 2;
    grid-row: 1 / span 2;
    font-size: 0.8rem;
    color: var(--fg-muted);
    overflow: auto;
    max-height: 700px;
  }
  .side h3 {
    font-size: 0.72rem;
    text-transform: uppercase;
    letter-spacing: 0.06em;
    color: var(--accent);
    margin: 0.6rem 0 0.25rem;
  }
  .counts div {
    display: flex;
    justify-content: space-between;
    font-family: var(--font-mono);
    font-size: 0.74rem;
  }
  .counts span:last-child {
    color: var(--fg);
  }
  .detail {
    min-height: 5em;
    color: var(--fg);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 6px 8px;
    background: var(--bg);
  }
  .legend {
    display: flex;
    flex-wrap: wrap;
    gap: 0.2rem 0.7rem;
  }
  .legend span {
    display: inline-flex;
    align-items: center;
    gap: 0.3rem;
    white-space: nowrap;
  }
  .legend i {
    display: inline-block;
  }
  .dot {
    width: 9px;
    height: 9px;
    border-radius: 50%;
  }
  .disc {
    width: 11px;
    height: 11px;
    border-radius: 50%;
    border: 1.5px solid #fff;
  }
  .ring {
    width: 9px;
    height: 9px;
    border-radius: 50%;
    border: 2px solid;
  }
  .dia {
    width: 9px;
    height: 9px;
    transform: rotate(45deg);
  }
  .tri {
    width: 0;
    height: 0;
    border-left: 5px solid transparent;
    border-right: 5px solid transparent;
    border-top: 9px solid #b07fcf;
  }
  .bar {
    width: 14px;
    height: 3px;
  }
  @media (max-width: 860px) {
    .netvis {
      grid-template-columns: 1fr;
    }
    .side {
      grid-column: 1;
      grid-row: 3;
      max-height: none;
    }
  }
</style>
