<script lang="ts">
  // A run drawn the way the site's tree-calculus visualizer (TreeVis) draws a tree: on a hill, data as
  // leaves and knots, programs in purple. A dot is a program, drawn open on the left, on the value to its
  // right. Its count shows in the program: the parts the value has been through fade, and the part it is
  // at wears a ring. A live dot (dashed ring) wears the move it will make; a dot whose count is at the
  // end (it has run) is a solid ring, and clicking it flips it. When a combinator starts, the node it
  // takes apart is kept where the dot was, ringed (map's in blue, tri's in amber), with the dots of its
  // parts above it; a call that holds no node is a bud, as TreeVis draws an application waiting on the
  // ones above it. The parts a combinator starts travel up out of the program into their dots, leaving a
  // slot, and come back into it once they have run. Every node is keyed by what it is, so a move's nodes
  // glide to where it puts them. It is drawn with WebGL on the stage ($lib/stage), which moves every node
  // in its shaders.
  import { onMount } from "svelte";
  import { Stage, type Drawing, type GlyphSpec, type LabelSpec, type NodeSpec, type RGBA, type StrokeSpec } from "$lib/stage/stage";
  import { F, SHAPE } from "$lib/stage/shaders";
  import { theme } from "$lib/theme.svelte";
  import { asBit, asNat, form, kids as vkids, registersOf, show, size, type V } from "./core";
  import { nameOf, progDef, progLabel, type Reads } from "./text";
  import { backward, cursor, done, fire, glyph, partOf, say, spine, type Node, type Wait } from "./machine";

  interface Props {
    root: Node;
    reads: Reads;
    labels?: boolean;
    motion?: boolean;
    /** The node that moves next when stepping one move at a time; it pulses. */
    next?: number | null;
    /** A new run: the height starts over (it otherwise keeps to the tallest the run has been). */
    epoch?: unknown;
    /** Draw each run's program open (its moves and named parts as pods), or folded into one pod. */
    programs?: boolean;
    onSite: (id: number) => void;
  }
  let { root, reads, labels = true, motion = true, next = null, epoch = null, programs = true, onSite }: Props =
    $props();

  const W = 680;
  const GROUND = 46;
  const WHOLE = 30; // a tree this small opens whole; a bigger one keeps its named parts folded
  const BUDGET = 300; // past this many open program nodes in one run, the rest stay pods until clicked
  const LEAF_TINTS = ["#6dbd7e", "#4aa96c", "#8fce9d", "#3d9b74", "#5cb787"];
  const CODE_TINTS = ["#a990d8", "#9a7ad6", "#b8a3e0"];
  const hash = (s: string) => {
    let h = 0;
    for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) | 0;
    return Math.abs(h);
  };
  const short = (s: string, n = 16) => (s.length > n ? s.slice(0, n - 1) + "…" : s);

  /** Parts in flight that hold no node of the value: drawn as beads on the way down. */
  const BEAD: Partial<Record<Wait, string>> = { self: "↩", fix: "μ", eval: "▷", triF: "?" };
  const PLAYS = "click to play this part until it has run";
  const BEAD_SAYS: Partial<Record<Wait, string>> = {
    self: "self: the fix's body running again",
    fix: "fix: self stands for this program inside",
    eval: "eval: a program from the value running",
    triF: "tri: the left fork opened, the third part running"
  };
  const HOLD_SAYS: Partial<Record<Wait, string>> = {
    mapS: "map holds this stem while its inside runs",
    mapF: "map holds this fork while both halves run at once",
    triL: "tri holds this fork and its △ while the first part runs",
    triS: "tri holds this stem, taken off the left, while the second part runs"
  };

  // ---- folding: a run's program is drawn open, with the moves and big named parts inside it as pods (a
  // move that is a run's whole program opens, so a pod opens as it reaches its run). A click on an
  // open part folds it, a click on a pod opens it. Program nodes are keyed by the run whose program they
  // belong to and by node, so a fold stays put while that part travels down into the runs of its parts ----
  let closed = $state(new Set<string>());
  let opened = $state(new Set<string>());
  const atomic = (v: V) => v.tag === "stem" && form(v).k === "atom";
  const bigName = (v: V) => size(v) >= 6 && !!nameOf(v);
  function toggle(key: string, isPod: boolean) {
    const c = new Set(closed);
    const o = new Set(opened);
    if (isPod) {
      if (!c.delete(key)) o.add(key);
    } else if (!o.delete(key)) c.add(key);
    closed = c;
    opened = o;
  }

  /** Which nodes of a program are read as programs, and as what: the rest join them together. */
  const ROLES = new WeakMap<V, Map<V, string>>();
  function rolesOf(p: V): Map<V, string> {
    let m = ROLES.get(p);
    if (m) return m;
    m = new Map();
    const walk = (t: V) => {
      if (m!.has(t)) return;
      const f = form(t);
      const named = nameOf(t);
      const own = f.k === "atom" ? f.n : f.k === "none" ? "not a program" : f.k === "dag" ? "†" : f.k === "seq" ? ";" : f.k;
      m!.set(t, named && t !== p ? named : own);
      if (f.k === "dag" || f.k === "fix") walk(f.b);
      if (f.k === "seq") (walk(f.a), walk(f.b));
      if (f.k === "tri" || f.k === "map") (walk(f.a), walk(f.b), walk(f.c));
    };
    walk(p);
    ROLES.set(p, m);
    return m;
  }

  type Kind = "leaf" | "knot" | "fork" | "bud" | "done" | "bead" | "hold" | "pod" | "code" | "codeleaf" | "dpod" | "slot";
  interface Laid {
    key: string;
    kind: Kind;
    parent: string | null;
    depth: number;
    x: number;
    y: number;
    site: number | null;
    glyph: string | null;
    stuck: boolean;
    back: boolean;
    /** For a hold: the shape it holds, and whose ring it wears. */
    shape?: "knot" | "fork";
    ring?: "map" | "tri";
    label: string | null;
    name: string | null;
    tip: string | null;
    open?: { v: V; site: string };
    /** A program part the value has been through, and the part its dot is at. */
    spent?: boolean;
    here?: boolean;
  }
  /** Where a node sits in a word, for reading bits: the top of a value, a part of a word, the inside of
   * a word's leaf (a bit or a tuple of bits), or below a bit. △△ is T as a bit, but a one-bit word
   * holding F as a word, so a label needs to know which. */
  type Ctx = "top" | "word" | "leaf" | "bit";
  /** A program part: `site` names the run it belongs to (a run whose program is a part of its parent's
   * keeps the parent's), `root` is that run's whole program, `lent` the parts of a part in flight that
   * are running in dots of their own (each leaves a slot), `note` what to say of a part, `ran` the parts
   * the value has been through, `here` the part the dot is at, and `back` whether that part runs
   * backward. */
  type Prog = {
    t: "prog";
    v: V;
    site: string;
    root: V;
    lent?: V[];
    note?: Map<V, string>;
    ran?: Set<V>;
    here?: V;
    back?: boolean;
    spent?: boolean;
    whole?: boolean;
  };
  type Item = { t: "node"; n: Node; ctx: Ctx; site?: string } | { t: "data"; v: V; underStem: boolean; ctx: Ctx; whole?: boolean } | Prog;
  /** Parts in flight whose dots run parts of the dot's own program: those dots keep its site. */
  const OWN_PARTS = new Set<Wait>(["fix", "mapS", "mapF", "triL", "triS", "triF"]);
  const below = (ctx: Ctx, stem: boolean): Ctx =>
    ctx === "top" || ctx === "word" ? (stem ? "leaf" : "word") : ctx === "leaf" && !stem ? "leaf" : "bit";

  function dataLabel(v: V, underStem: boolean, ctx: Ctx): string | null {
    if (!labels) return null;
    if (reads === "bits" || reads === "regs") {
      if ((ctx === "top" || ctx === "word") && v.tag === "stem") {
        // a word's leaf: registers show their tuple here; a single word shows the bit inside
        const r = reads === "regs" ? registersOf(v) : null;
        return r && r.length > 1 ? r.join("") : null;
      }
      if (ctx === "top" || ctx === "leaf") return v.tag === "leaf" ? "0" : asBit(v) === 1 ? "1" : null;
      return null;
    }
    if (underStem) return null;
    if (reads === "bool") {
      const b = asBit(v);
      if (b !== null) return b ? "T" : "F";
    }
    if (reads === "nat") {
      const n = asNat(v);
      if (n !== null) return String(n);
    }
    return null;
  }

  /** A dot's program, open beside it, with its count: the parts before the count (in the order they run
   * forward) are the ones the value has been through, and a part in flight lends its own parts to their
   * dots. A flip leaves which parts those are as it is, and turns only the way the count goes. */
  function progOf(n: Node, site: string): Prog {
    const parts = spine(n.p);
    const c = cursor(n);
    const ran = new Set(parts.slice(0, c).map(([q]) => q));
    const note = new Map<V, string>();
    parts.forEach(([q], i) => note.set(q, i < c ? (n.fwd ? " — has run" : " — to run back") : n.fwd ? " — to run" : " — has run back"));
    const at = partOf(n);
    let lent: V[] = [];
    if (at && n.kids.length) {
      const p = at.f as { a: V; b: V; c: V };
      lent =
        n.w === "fix" ? [p.b]
        : n.w === "mapS" || n.w === "triL" ? [p.a]
        : n.w === "mapF" ? [p.b, p.c]
        : n.w === "triS" ? [p.b]
        : n.w === "triF" ? [p.c]
        : [];
      note.set(at.q, " — running now");
    } else if (at) note.set(at.q, " — runs next");
    return { t: "prog", v: n.p, site, root: n.p, lent, note, ran, here: at?.q, back: at ? !at.dir : false };
  }

  let tallest = 0;
  let seenEpoch: unknown = undefined;
  function scene(t: Node): { nodes: Laid[]; glyph: number; height: number } {
    const out: Omit<Laid, "y">[] = [];
    const blank = { site: null, glyph: null, stuck: false, back: false, label: null, name: null, tip: null };
    const used = new Map<string, number>();

    function go(it: Item, depth: number, parent: string | null): { rootX: number; w: number } {
      let node: Omit<Laid, "x" | "y" | "depth" | "parent">;
      let kids: Item[] = [];
      let step = 1;
      if (it.t === "node") {
        const n = it.n;
        const g = glyph(n);
        const back = backward(n);
        const sk = it.site ?? `s${n.id}`;
        const w = n.w;
        const kidSite = w && OWN_PARTS.has(w) ? sk : undefined;
        if (!w) {
          const live = !done(n);
          const stuck = live && !g;
          node = {
            ...blank,
            key: `n${n.id}`,
            kind: live ? "bud" : "done",
            site: n.id,
            glyph: g,
            stuck,
            back,
            name: progLabel(n.p),
            tip: live
              ? stuck
                ? `stuck: ${say(n)}`
                : `${say(n)} — click to make this move`
              : `${progLabel(n.p)}: it has run — click to flip it, so that firing runs it back`
          };
          kids = [progOf(n, sk), { t: "data", v: n.x!, underStem: false, ctx: it.ctx }];
        } else if (BEAD[w]) {
          const named = w === "fix" ? nameOf(partOf(n)!.base) : null;
          node = {
            ...blank,
            key: `n${n.id}`,
            kind: "bead",
            site: n.id,
            glyph: g,
            back,
            label: BEAD[w]!,
            name: named,
            tip: `${BEAD_SAYS[w]}${named ? ` (${named})` : ""} — ${fire(n) ? "click to make this move" : PLAYS}`
          };
          kids = [progOf(n, sk), ...n.kids.map((k): Item => ({ t: "node", n: k, ctx: it.ctx, site: kidSite }))];
          step = 0.62;
        } else {
          const ring = w === "mapS" || w === "mapF" ? "map" : "tri";
          node = {
            ...blank,
            key: `v${n.at}`,
            kind: "hold",
            site: n.id,
            glyph: g,
            back,
            shape: w === "mapS" || w === "triS" ? "knot" : "fork",
            ring,
            label: ring,
            tip: `${HOLD_SAYS[w]} — ${fire(n) ? "click to close it" : PLAYS}`
          };
          const ctx = below(it.ctx, w === "mapS" || w === "triS");
          kids = [
            progOf(n, sk),
            ...(w === "triL"
              ? [{ t: "data", v: n.kept!, underStem: false, ctx } as Item, { t: "node", n: n.kids[0], ctx, site: kidSite } as Item]
              : n.kids.map((k): Item => ({ t: "node", n: k, ctx, site: kidSite })))
          ];
        }
      } else if (it.t === "data") {
        const v = it.v;
        const key = `v${v.id}`;
        const prog = size(v) >= 6 ? nameOf(v) : null;
        const whole = it.whole || (opened.has(key) && size(v) <= WHOLE);
        if (prog && !prog.endsWith("†") && !opened.has(key) && !it.whole) {
          node = { ...blank, key, kind: "dpod", name: prog, tip: `${prog}, a program held as data (click to open)`, open: { v, site: "v" } };
        } else {
          const label = dataLabel(v, it.underStem, it.ctx);
          node = {
            ...blank,
            key,
            kind: v.tag === "leaf" ? "leaf" : v.tag === "stem" ? "knot" : "fork",
            label,
            tip: prog ? `${prog} (click to fold)` : null,
            open: prog ? { v, site: "v" } : undefined
          };
          const ctx = label && v.tag === "stem" && it.ctx !== "leaf" ? "bit" : below(it.ctx, v.tag === "stem");
          kids = vkids(v).map((c) => ({ t: "data", v: c, underStem: v.tag === "stem", ctx, whole }));
        }
      } else {
        const v = it.v;
        const key = `${it.site}:${v.id}`;
        const isRoot = v === it.root;
        const role = rolesOf(it.root).get(v) ?? null;
        const spent = !!it.spent || !!it.ran?.has(v);
        const base = { ...blank, key, open: { v, site: it.site }, spent, here: it.here === v, back: !!it.back };
        const count = used.get(it.site) ?? 0;
        const note = it.note?.get(v) ?? "";
        // a tree opened by a click, or a run's own program, opens whole when it is small; inside a bigger
        // one the moves and named parts stay pods
        let whole = !!it.whole;
        let pod: boolean;
        if (it.lent?.includes(v)) {
          // a part out running in a dot of its own: it comes back here when it has run
          out.push({ ...blank, key: `${it.site}:slot${v.id}`, kind: "slot", tip: `${role ?? progLabel(v)}: running above, in a dot of its own`, parent, depth, x: 0 });
          return { rootX: 0, w: 1 };
        }
        if (closed.has(key)) pod = true;
        else if (opened.has(key) || (isRoot && programs)) {
          pod = false;
          whole = size(v) <= WHOLE;
        } else
          pod =
            isRoot || (!whole && ((role !== null && (atomic(v) || bigName(v))) || count >= BUDGET));
        if (pod) {
          const name = isRoot || atomic(v) ? progLabel(v) : (role ?? "…");
          node = { ...base, kind: "pod", name, tip: `${progDef(v)}${note} — click to open` };
        } else {
          used.set(it.site, count + 1);
          if (v.tag === "leaf") node = { ...base, kind: "codeleaf", label: role, tip: role === "id" ? "id: does nothing" : "△" };
          else {
            const what = role ? `${role}: ${short(progLabel(v), 60)}` : short(show(v), 60);
            node = { ...base, kind: "code", label: role, tip: `${what}${note} — click to fold` };
            kids = vkids(v).map((c): Item => ({ ...it, v: c, spent, whole }));
          }
        }
        step = 0.8;
      }
      if (!kids.length) {
        out.push({ ...node, parent, depth, x: 0 });
        return { rootX: 0, w: 1 };
      }
      let cum = 0;
      let first = 0;
      let last = 0;
      kids.forEach((k, i) => {
        const before = out.length;
        const s = go(k, depth + step, node.key);
        for (let j = before; j < out.length; j++) out[j].x += cum;
        if (i === 0) first = s.rootX + cum;
        last = s.rootX + cum;
        cum += s.w;
      });
      const x = (first + last) / 2;
      out.push({ ...node, parent, depth, x });
      return { rootX: x, w: cum };
    }
    go({ t: "node", n: t, ctx: "top" }, 0, null);
    const maxX = Math.max(1, ...out.map((n) => n.x));
    const maxD = Math.max(1, ...out.map((n) => n.depth));
    if (epoch !== seenEpoch) {
      seenEpoch = epoch;
      tallest = 0;
    }
    tallest = Math.max(tallest, maxD);
    const height = Math.max(300, Math.min(760, 150 + tallest * 44));
    const sx = Math.min(70, (W - 70) / maxX);
    const sy = Math.min(62, (height - GROUND - 56) / maxD);
    const ox = (W - maxX * sx) / 2;
    const zoom = Math.max(0.5, Math.min(1.15, Math.min(sx * 1.25, sy) / 40));
    // a pod's name goes above it, cut to the room it has beside its neighbours, or left to its tooltip
    const room = Math.floor((sx * 1.9) / (5.6 * zoom));
    const fit = (s: string | null) => (!s || s.length <= room ? s : room >= 5 ? short(s, room) : null);
    return {
      nodes: out.map((n) => ({
        ...n,
        name: n.kind === "pod" || n.kind === "dpod" ? fit(n.name) : n.name,
        x: ox + n.x * sx,
        y: height - GROUND - 14 - n.depth * sy
      })),
      glyph: zoom,
      height
    };
  }

  // a state the drawing cannot lay out keeps the last one on screen, and says why in the console
  let last: { nodes: Laid[]; glyph: number; height: number } = { nodes: [], glyph: 1, height: 300 };
  const laid = $derived.by(() => {
    try {
      return (last = scene(root));
    } catch (e) {
      console.error("two-way: could not draw this state", e);
      return last;
    }
  });
  const g = $derived(laid.glyph);
  const height = $derived(laid.height);

  const reduced = typeof window !== "undefined" && window.matchMedia?.("(prefers-reduced-motion: reduce)").matches;
  const isCode = (n: Laid) => n.kind === "pod" || n.kind === "code" || n.kind === "codeleaf" || n.kind === "slot";
  const clickable = (n: Laid) => n.site != null || !!n.open;

  function click(n: Laid) {
    if (n.kind === "pod" || n.kind === "dpod") toggle(n.key, true);
    else if (n.kind === "code" || (n.open && n.site == null && n.kind !== "codeleaf")) toggle(n.key, false);
    else if (n.site != null) onSite(n.site);
  }

  // ---- the stage: every laid node becomes a node of the WebGL stage, with the shapes, words and branch
  // it draws, and the stage moves them all. Colours come from the page's, read again when the theme
  // changes ----
  const PALETTE = {
    g2: "var(--g2)",
    g4: "var(--g4)",
    bark: "var(--bark)",
    blossom: "var(--blossom)",
    bgElev: "var(--bg-elev)",
    err: "var(--err)",
    fgFaint: "var(--fg-faint)",
    blue: "#6c9bd2",
    white: "#ffffff",
    budFill: "color-mix(in oklab, var(--g4) 8%, var(--bg-elev))",
    fireFill: "color-mix(in oklab, var(--g4) 22%, var(--bg-elev))",
    fireFillBack: "color-mix(in oklab, #6c9bd2 20%, var(--bg-elev))",
    fireLetter: "color-mix(in oklab, #8a6414 72%, var(--fg))",
    fireLetterBack: "color-mix(in oklab, #2f5f95 72%, var(--fg))",
    doneFill: "color-mix(in oklab, var(--blossom) 10%, var(--bg-elev))",
    doneCore: "color-mix(in oklab, var(--blossom) 70%, var(--bark))",
    backFill: "color-mix(in oklab, #6c9bd2 22%, var(--bg-elev))",
    mapStroke: "#5a90cf",
    mapFill: "color-mix(in oklab, #5a90cf 9%, transparent)",
    mapName: "color-mix(in oklab, #3f73b0 75%, var(--fg))",
    triStroke: "#c98e2b",
    triFill: "color-mix(in oklab, #c98e2b 9%, transparent)",
    triName: "color-mix(in oklab, #a06d16 75%, var(--fg))",
    codeKnot: "#8a6bd1",
    codeEdge: "#9a7ad6",
    codeBadge: "#7a5cc4",
    pod: "#a58ad8",
    podStroke: "color-mix(in oklab, #4b2f8a 60%, transparent)",
    dpod: "#79ae74",
    dpodStroke: "color-mix(in oklab, #2e5e33 60%, transparent)",
    vname: "#2c1c52",
    vnameAbove: "color-mix(in oklab, #7a5cc4 85%, var(--fg))",
    leafStroke: "color-mix(in oklab, var(--fg) 18%, transparent)"
  };
  type Palette = Record<keyof typeof PALETTE, RGBA> & { leaf: RGBA[]; code: RGBA[]; bg: [number, number, number] };

  /** A CSS colour as the stage takes it: sRGB and alpha, 0 to 1, read back from a pixel painted with it. */
  function colorReader(el: HTMLElement) {
    const probe = document.createElement("span");
    probe.style.display = "none";
    el.appendChild(probe);
    const px = document.createElement("canvas");
    px.width = px.height = 1;
    const x = px.getContext("2d", { willReadFrequently: true })!;
    return {
      read(css: string): RGBA {
        probe.style.color = "";
        probe.style.color = css;
        x.clearRect(0, 0, 1, 1);
        x.fillStyle = getComputedStyle(probe).color;
        x.fillRect(0, 0, 1, 1);
        const d = x.getImageData(0, 0, 1, 1).data;
        return [d[0] / 255, d[1] / 255, d[2] / 255, d[3] / 255];
      },
      done: () => probe.remove()
    };
  }
  function readPalette(el: HTMLElement): Palette {
    const r = colorReader(el);
    const out = Object.fromEntries(Object.entries(PALETTE).map(([k, css]) => [k, r.read(css)])) as unknown as Palette;
    out.leaf = LEAF_TINTS.map(r.read);
    out.code = CODE_TINTS.map(r.read);
    // what the branches are drawn over: the first background up the page
    let bg: RGBA = [1, 1, 1, 1];
    for (let e: HTMLElement | null = el; e; e = e.parentElement) {
      const c = r.read(getComputedStyle(e).backgroundColor);
      if (c[3] > 0.5) {
        bg = c;
        break;
      }
    }
    out.bg = [bg[0], bg[1], bg[2]];
    r.done();
    return out;
  }

  const HIT: Record<Kind, number> = { bud: 14, done: 14, bead: 14, hold: 13, pod: 10, dpod: 10, code: 7, codeleaf: 7, leaf: 9, knot: 6, fork: 6, slot: 7 };
  const alpha = (c: RGBA, a: number): RGBA => [c[0], c[1], c[2], c[3] * a];
  const deg = (d: number) => (d * Math.PI) / 180;

  let seen = new Set<string>();
  let marked = new Set<string>();
  let byKey = new Map<string, Laid>();

  /** The stage's drawing of a laid-out run. Offsets are in the run's drawing units, y down, times the
   * glyph scale, as the SVG drew them; the stage's y runs up. */
  function drawing(L: { nodes: Laid[]; glyph: number; height: number }, P: Palette, nextSite: number | null, moving: boolean): Drawing {
    const g = L.glyph;
    const nodes: NodeSpec[] = [{ key: "ground", x: W / 2, y: -(L.height - GROUND + 12) }];
    const glyphs: GlyphSpec[] = [];
    const words: LabelSpec[] = [];
    const strokes: StrokeSpec[] = [];
    const born = L.nodes.filter((n) => !seen.has(n.key));
    const top = Math.min(...born.map((n) => n.depth));
    const nowMarked = new Set<string>();
    const at = (dx: number, dy: number) => ({ dx: dx * g, dy: -dy * g });
    for (const n of L.nodes) {
      const node = n.key;
      const pick = clickable(n) || !!n.tip;
      nodes.push({
        key: node,
        x: n.x,
        y: -n.y,
        parent: n.parent ?? "ground",
        delay: seen.has(node) ? 0 : Math.min(240, (n.depth - top) * 40) / 1000,
        hit: pick ? HIT[n.kind] * g : 0
      });
      const shape = (s: Omit<GlyphSpec, "node">) => glyphs.push({ node, ...s });
      const word = (w: Omit<LabelSpec, "node">) => words.push({ node, ...w });
      const grow = clickable(n) ? F.hoverGrow : 0;
      const isNext = n.site != null && n.site === nextSite;
      /** The move a node will make, in a ring on it (or beside it, small). */
      const mark = (r: number, letter: number, dx = 0, dy = 0) => {
        if (!n.glyph) return;
        nowMarked.add(node);
        const flags = (moving && !marked.has(node) ? F.markIn : 0) | (isNext ? F.pulse : 0);
        shape({ r: r * g, ...at(dx, dy), fill: n.back ? P.fireFillBack : P.fireFill, stroke: n.back ? P.blue : P.g4, sw: 1.2 * g, flags });
        const one = n.glyph.length === 1;
        word({ text: n.glyph, size: (one ? letter : letter * 0.78) * g, weight: 700, color: n.back ? P.fireLetterBack : P.fireLetter, ...at(dx, dy + 0.5), baseline: "middle", flags });
      };
      /** A program part the value has been through is drawn faint. */
      const fade = (c: RGBA) => (n.spent ? alpha(c, 0.32) : c);
      if (n.here) shape({ r: (n.kind === "pod" ? 12.5 : 7) * g, fill: alpha(P.bgElev, 0), stroke: n.back ? P.blue : P.g4, sw: 1.6 * g });
      const blossom = () => {
        if (!isNext) return;
        for (const a of [36, 108, 180, 252, 324])
          shape({ r: 2.6 * g, ...at(9.5 * Math.cos(deg(a)), 9.5 * Math.sin(deg(a))), fill: alpha(P.blossom, 0.85), stroke: P.white, sw: 0.5 * g });
      };
      switch (n.kind) {
        case "bud":
        case "bead": {
          const ring = n.stuck ? P.err : n.back ? P.blue : P.g4;
          shape({ r: 8.5 * g, fill: P.budFill, stroke: ring, sw: 1.5 * g, dashes: n.stuck ? 0 : 9, flags: grow });
          shape({ r: 3 * g, fill: n.back ? P.blue : P.g4 });
          mark(7.5, 11);
          blossom();
          if (n.kind === "bead" && n.name) word({ text: n.name, size: 7.5 * g, color: P.fgFaint, anchor: "end", ...at(-12, 3) });
          break;
        }
        case "done":
          shape({ r: 8.5 * g, fill: P.doneFill, stroke: P.doneCore, sw: 1.5 * g, flags: grow });
          shape({ r: 3.4 * g, fill: P.doneCore });
          shape({ r: 5.6 * g, ...at(9, -9), fill: P.backFill, stroke: P.blue, sw: g, flags: F.hoverOnly });
          word({ text: "⇆", size: 7.5 * g, weight: 700, color: P.fireLetterBack, ...at(9, -8.5), baseline: "middle", flags: F.hoverOnly });
          break;
        case "hold": {
          const map = n.ring === "map";
          shape({ r: 9.5 * g, fill: map ? P.mapFill : P.triFill, stroke: map ? P.mapStroke : P.triStroke, sw: 1.6 * g, dashes: n.back ? 15 : 0, flags: grow });
          shape({ r: (n.shape === "fork" ? 4.2 : 3.2) * g, fill: P.bark });
          if (n.label) word({ text: n.label, size: 7 * g, color: map ? P.mapName : P.triName, anchor: "end", ...at(-12, 3) });
          mark(5.8, 8.5, 8.5, -8.5);
          break;
        }
        case "pod":
        case "dpod": {
          const data = n.kind === "dpod";
          const name = n.name ?? "";
          shape({ r: 9 * g, fill: fade(data ? P.dpod : P.pod), stroke: fade(data ? P.dpodStroke : P.podStroke), sw: 0.9 * g, flags: F.hoverBright | (name.length > 4 ? F.seam : 0) });
          if (name.length <= 4) word({ text: name, size: [10, 10, 8, 7, 6][name.length] * g, weight: 700, color: fade(P.vname), ...at(0, 0.5), baseline: "middle" });
          else word({ text: name, size: 9 * g, weight: 600, color: fade(data ? P.g2 : P.vnameAbove), ...at(0, -13), baseline: "middle" });
          break;
        }
        case "code":
          shape({ r: 3.4 * g, fill: fade(P.codeKnot) });
          break;
        case "codeleaf":
          shape({ shape: SHAPE.leaf, r: 7 * g, fill: fade(P.code[hash(node) % P.code.length]), stroke: fade(P.leafStroke), sw: 0.5 * g, rot: deg((hash(node) % 70) - 35) });
          break;
        case "leaf":
          shape({ shape: SHAPE.leaf, r: 9 * g, fill: P.leaf[hash(node) % P.leaf.length], stroke: P.leafStroke, sw: 0.5 * g, rot: deg((hash(node) % 70) - 35), flags: F.vein });
          break;
        case "slot":
          shape({ r: 3.4 * g, fill: P.bgElev, stroke: P.codeKnot, sw: 1.2 * g, dashes: 6 });
          break;
        default:
          shape({ r: (n.kind === "fork" ? 4.2 : 3.2) * g, fill: P.bark });
      }
      if (n.label && (n.kind === "code" || n.kind === "codeleaf" || n.kind === "leaf" || n.kind === "knot" || n.kind === "fork")) {
        const side = n.kind === "knot" || n.kind === "fork";
        const code = n.kind === "code" || n.kind === "codeleaf";
        word({
          text: n.label,
          size: (code ? 8.5 : 9.5) * g,
          color: code ? fade(P.codeBadge) : P.g2,
          anchor: side ? "start" : "middle",
          ...at(side ? 7 : 0, n.kind === "leaf" || n.kind === "codeleaf" ? -12 : n.kind === "code" ? -8 : 3.2)
        });
      }
      if (n.parent != null)
        strokes.push({
          a: n.parent,
          b: node,
          width: Math.max(1.3, (isCode(n) ? 2.6 : 5 - n.depth * 0.45) * g),
          color: isCode(n) ? alpha(P.codeEdge, n.spent ? 0.2 : 0.55) : alpha(P.bark, 0.8),
          dash: n.kind === "slot" ? [5, 2] : undefined
        });
      else strokes.push({ a: "ground", b: node, width: 5, color: alpha(P.bark, 0.8), bend: [0, -12 / (-n.y + L.height - GROUND + 12)] });
    }
    seen = new Set(L.nodes.map((n) => n.key));
    marked = nowMarked;
    byKey = new Map(L.nodes.map((n) => [n.key, n]));
    return { nodes, glyphs, labels: words, strokes, view: { x0: 0, y0: -L.height, x1: W, y1: 0 } };
  }

  let host: HTMLDivElement | undefined = $state();
  let canvas: HTMLCanvasElement | undefined = $state();
  let stage: Stage | null = null;
  let ready = $state(false);
  let failed = $state(false);
  let palette = $state<Palette | null>(null);
  let fontGen = $state(0);
  let tip = $state<{ text: string; x: number; y: number } | null>(null);
  let pointer = $state(false);
  /** Clicked into: scrolling and pinching zoom the tree itself, not the page. */
  let engaged = $state(false);
  let zoomed = $state(false);
  let hint = $state(false);
  /** The hill's view box, which follows the tree's zoom. */
  let groundBox = $state<string | null>(null);

  // ---- the page's own zoom. The canvas covers the part of the drawing's box that is on screen: unzoomed, all
  // of it; when the page is pinch-zoomed (the visual viewport scaled), the part the zoom shows and a margin
  // round it, drawn with as many pixels as the zoom gives, so the tree stays as sharp as the SVG it
  // replaced. While a pinch goes on, the browser stretches what is drawn; it is drawn again, sharp, when the
  // pinch settles, or at once if the view leaves what is drawn. Ctrl+scroll zoom changes the device pixel
  // ratio, read here too ----
  let cover: { x: number; y: number; w: number; h: number; scale: number } | null = null;
  function placeCanvas(settled: boolean) {
    if (!host || !canvas || !stage) return;
    const r = host.getBoundingClientRect();
    const fullW = host.clientWidth;
    const fullH = host.clientHeight;
    if (!fullW || !fullH) return;
    let box = { x: 0, y: 0, w: fullW, h: fullH };
    const vv = window.visualViewport;
    const scale = vv && vv.scale > 1.01 ? vv.scale : 1;
    if (vv && scale > 1) {
      const left = Math.max(r.left, vv.offsetLeft);
      const top = Math.max(r.top, vv.offsetTop);
      const right = Math.min(r.right, vv.offsetLeft + vv.width);
      const bottom = Math.min(r.bottom, vv.offsetTop + vv.height);
      if (right - left < 1 || bottom - top < 1) return;
      const seen = { x: left - r.left, y: top - r.top, w: right - left, h: bottom - top };
      const c = cover;
      const inside = !!c && seen.x >= c.x && seen.y >= c.y && seen.x + seen.w <= c.x + c.w && seen.y + seen.h <= c.y + c.h;
      if (!settled && inside && scale < c!.scale * 1.5 && scale > c!.scale / 1.5) return;
      const m = 0.3 * Math.max(seen.w, seen.h);
      const x0 = Math.max(0, seen.x - m);
      const y0 = Math.max(0, seen.y - m);
      box = { x: x0, y: y0, w: Math.min(fullW, seen.x + seen.w + m) - x0, h: Math.min(fullH, seen.y + seen.h + m) - y0 };
    }
    cover = { ...box, scale };
    canvas.style.left = `${box.x}px`;
    canvas.style.top = `${box.y}px`;
    canvas.style.width = `${box.w}px`;
    canvas.style.height = `${box.h}px`;
    const ratio = Math.min((window.devicePixelRatio || 1) * scale, 4096 / box.w, 4096 / box.h, Math.sqrt(16e6 / (box.w * box.h)));
    stage.setWindow({ fullW, fullH, ...box, ratio });
  }

  // ---- zooming the tree itself, once it has been clicked into (as a map on a page waits for a click):
  // scroll or pinch zooms about the pointer, a drag moves it, zooming out far enough fits it whole again ----
  const pointers = new Map<number, { x: number; y: number }>();
  let gesture: "tap" | "pan" | "pinch" | null = null;
  let downAt: { x: number; y: number } | null = null;
  let lastHint = 0;
  let hintTimer: ReturnType<typeof setTimeout> | undefined;
  function onWheel(e: WheelEvent) {
    if (!stage) return;
    if (!engaged) {
      // the page scrolls; now and then, say how to zoom the tree instead
      if (!e.ctrlKey && performance.now() - lastHint > 8000) {
        lastHint = performance.now();
        hint = true;
        clearTimeout(hintTimer);
        hintTimer = setTimeout(() => (hint = false), 1600);
      }
      return;
    }
    e.preventDefault();
    tip = null;
    const unit = e.deltaMode === 1 ? 16 : e.deltaMode === 2 ? 400 : 1;
    const dx = e.deltaX * unit;
    const dy = e.deltaY * unit;
    // a pinch on a touchpad comes as ctrl+scroll; a mouse wheel in big steps; a touchpad's two fingers in small ones
    if (e.ctrlKey) stage.zoomBy(Math.exp(-dy * 0.01), e.clientX, e.clientY);
    else if (e.deltaMode !== 0 || (dx === 0 && Math.abs(dy) >= 50)) stage.zoomBy(Math.exp(-dy * 0.0025), e.clientX, e.clientY, true);
    else stage.panBy(-dx, -dy);
  }
  /** Safari's pinch on a touchpad, which comes as gesture events. */
  let gestureScale = 1;
  function onGesture(e: Event & { scale?: number; clientX?: number; clientY?: number }) {
    if (!engaged || !stage) return;
    e.preventDefault();
    if (e.type === "gesturestart") gestureScale = 1;
    else {
      stage.zoomBy((e.scale ?? 1) / gestureScale, e.clientX ?? 0, e.clientY ?? 0);
      gestureScale = e.scale ?? 1;
    }
  }

  onMount(() => {
    const family = getComputedStyle(host!).getPropertyValue("--font-mono").trim() || "monospace";
    try {
      stage = new Stage(canvas!, family);
    } catch (e) {
      console.error("two-way: no WebGL", e);
      failed = true;
      return;
    }
    stage.onZoom = (z) => (zoomed = z);
    stage.onView = (x, y, w, h) => (groundBox = `${x} ${y} ${w} ${h}`);
    ready = true;
    placeCanvas(true);
    // strings drawn before the font arrived are drawn again in it
    document.fonts?.ready.then(() => {
      stage?.refont(family);
      fontGen++;
    });
    let raf = 0;
    let settle: ReturnType<typeof setTimeout> | undefined;
    const place = () => {
      cancelAnimationFrame(raf);
      raf = requestAnimationFrame(() => placeCanvas(false));
      clearTimeout(settle);
      settle = setTimeout(() => placeCanvas(true), 160);
    };
    const vv = window.visualViewport;
    vv?.addEventListener("resize", place);
    vv?.addEventListener("scroll", place);
    window.addEventListener("resize", place);
    window.addEventListener("scroll", place, { passive: true });
    const ro = new ResizeObserver(place);
    ro.observe(host!);
    const letGo = (e: KeyboardEvent) => {
      if (engaged && e.key === "Escape") engaged = false;
    };
    window.addEventListener("keydown", letGo);
    const c = canvas!;
    c.addEventListener("wheel", onWheel, { passive: false });
    c.addEventListener("gesturestart", onGesture as EventListener, { passive: false });
    c.addEventListener("gesturechange", onGesture as EventListener, { passive: false });
    return () => {
      cancelAnimationFrame(raf);
      clearTimeout(settle);
      clearTimeout(hintTimer);
      vv?.removeEventListener("resize", place);
      vv?.removeEventListener("scroll", place);
      window.removeEventListener("resize", place);
      window.removeEventListener("scroll", place);
      window.removeEventListener("keydown", letGo);
      c.removeEventListener("wheel", onWheel);
      c.removeEventListener("gesturestart", onGesture as EventListener);
      c.removeEventListener("gesturechange", onGesture as EventListener);
      ro.disconnect();
      stage?.dispose();
      stage = null;
    };
  });
  // the theme's colours, read once the page has taken them
  $effect(() => {
    void theme.mix;
    void theme.style;
    if (!ready || !host) return;
    const raf = requestAnimationFrame(() => (palette = readPalette(host!)));
    return () => cancelAnimationFrame(raf);
  });
  // a new run starts fitted whole
  $effect(() => {
    void epoch;
    if (ready) stage?.fitAll(true);
  });
  $effect(() => {
    void fontGen;
    const st = stage;
    if (!ready || !st || !palette) return;
    const moving = motion && !reduced;
    st.setBackground(palette.bg);
    st.swayAt = [W / 2, -laid.height];
    st.sway = moving ? (t) => -deg(0.6) * Math.cos((Math.PI * t) / 9) : null;
    st.pulsing = next != null;
    st.show(drawing(laid, palette, next, moving), moving ? { move: 0.48, grow: 0.42, shrink: 0.36 } : { move: 0, grow: 0, shrink: 0 });
  });

  function hovered(e: PointerEvent): Laid | null {
    const key = stage?.pick(e.clientX, e.clientY);
    return (key && byKey.get(key)) || null;
  }
  function hover(e: PointerEvent) {
    const n = hovered(e);
    stage?.setHover(n?.key ?? null);
    pointer = !!n && clickable(n);
    const r = host!.getBoundingClientRect();
    tip = n?.tip ? { text: n.tip, x: e.clientX - r.left, y: e.clientY - r.top } : null;
  }
  function onDown(e: PointerEvent) {
    pointers.set(e.pointerId, { x: e.clientX, y: e.clientY });
    if (pointers.size === 1) {
      gesture = "tap";
      downAt = { x: e.clientX, y: e.clientY };
    } else if (engaged) gesture = "pinch";
    if (engaged) canvas?.setPointerCapture(e.pointerId);
  }
  function onMove(e: PointerEvent) {
    const p = pointers.get(e.pointerId);
    if (!p) return hover(e);
    const px = p.x;
    const py = p.y;
    p.x = e.clientX;
    p.y = e.clientY;
    if (!engaged || !stage) return;
    if (pointers.size >= 2) {
      // two fingers: the midpoint moves the tree, the spread zooms it about the midpoint
      const other = [...pointers.entries()].find(([id]) => id !== e.pointerId)![1];
      const d0 = Math.hypot(px - other.x, py - other.y);
      const d1 = Math.hypot(p.x - other.x, p.y - other.y);
      const mx = (p.x + other.x) / 2;
      const my = (p.y + other.y) / 2;
      stage.panBy((p.x - px) / 2, (p.y - py) / 2);
      if (d0 > 0) stage.zoomBy(d1 / d0, mx, my);
      gesture = "pinch";
      tip = null;
      return;
    }
    if (gesture === "tap" && downAt && Math.hypot(e.clientX - downAt.x, e.clientY - downAt.y) > 5) gesture = "pan";
    if (gesture === "pan") {
      stage.panBy(e.clientX - px, e.clientY - py);
      tip = null;
    }
  }
  function onUp(e: PointerEvent) {
    const had = pointers.delete(e.pointerId);
    const g = gesture;
    if (pointers.size === 0) gesture = null;
    if (!had || g !== "tap") return;
    engaged = true;
    const n = hovered(e);
    if (n && clickable(n)) {
      click(n);
      tip = null;
    }
  }
  function onCancel(e: PointerEvent) {
    pointers.delete(e.pointerId);
    if (pointers.size === 0) gesture = null;
  }
  function onLeave() {
    stage?.setHover(null);
    pointer = false;
    tip = null;
  }
</script>

<svelte:document
  onpointerdown={(e) => {
    if (engaged && host && !host.contains(e.target as globalThis.Node)) engaged = false;
  }}
/>

<div class="stage" class:engaged bind:this={host} style="aspect-ratio: {W} / {height}">
  <svg class="ground" viewBox={groundBox ?? `0 0 ${W} ${height}`} preserveAspectRatio="xMidYMid meet" aria-hidden="true">
    <ellipse cx={W / 2} cy={height - 22} rx={W * 0.36} ry="16" class="hill back" />
    <ellipse cx={W / 2 - 30} cy={height - 16} rx={W * 0.3} ry="13" class="hill front" />
    <g class="grass">
      <path d="M {W / 2 - 118} {height - 24} q 2 -12 7 -16 M {W / 2 - 112} {height - 24} q 3 -8 9 -10" />
      <path d="M {W / 2 + 104} {height - 20} q 2 -12 7 -16 M {W / 2 + 111} {height - 20} q 3 -8 9 -10" />
      <path d="M {W / 2 - 40} {height - 12} q 2 -10 6 -13 M {W / 2 - 34} {height - 12} q 3 -7 8 -9" />
    </g>
    <circle cx={W / 2 - 128} cy={height - 30} r="2.3" class="daisy" />
    <circle cx={W / 2 + 122} cy={height - 26} r="2.3" class="daisy alt" />
  </svg>
  <canvas
    bind:this={canvas}
    class:pointer
    class:grab={engaged && !pointer}
    style:touch-action={engaged ? "none" : "manipulation"}
    aria-label="the run, as a tree"
    onpointermove={onMove}
    onpointerleave={onLeave}
    onpointerdown={onDown}
    onpointerup={onUp}
    onpointercancel={onCancel}
  ></canvas>
  {#if zoomed}
    <button type="button" class="refit" onclick={() => stage?.fitAll()} aria-label="fit the whole tree" title="fit the whole tree">⤢</button>
  {/if}
  {#if engaged}
    <p class="zoomnote">scroll or pinch to zoom the tree, drag to move it · Esc lets go</p>
  {:else if hint}
    <p class="zoomnote hint">click the tree to zoom into it</p>
  {/if}
  {#if tip}<div class="tip" style="left: {tip.x}px; top: {tip.y}px">{tip.text}</div>{/if}
  {#if failed}<p class="nogl">This browser gives no WebGL, so the run cannot be drawn here.</p>{/if}
</div>

<style>
  .stage {
    position: relative;
    width: 100%;
    max-height: min(74vh, 720px);
  }
  .ground,
  canvas {
    position: absolute;
    left: 0;
    top: 0;
    width: 100%;
    height: 100%;
    display: block;
  }
  canvas {
    touch-action: manipulation;
  }
  canvas.pointer {
    cursor: pointer;
  }
  canvas.grab {
    cursor: grab;
  }
  .stage.engaged {
    outline: 1.5px solid color-mix(in oklab, var(--g2) 45%, transparent);
    outline-offset: 3px;
    border-radius: 8px;
  }
  .refit {
    position: absolute;
    top: 6px;
    right: 6px;
    z-index: 3;
    width: 28px;
    height: 28px;
    padding: 0;
    display: grid;
    place-items: center;
    border-radius: 50%;
    border: 1px solid var(--border-strong);
    background: var(--bg-elev);
    color: var(--g2);
    font-size: 0.95rem;
    cursor: pointer;
  }
  .refit:hover {
    border-color: var(--accent);
    color: var(--accent);
  }
  .zoomnote {
    position: absolute;
    top: 6px;
    left: 8px;
    margin: 0;
    z-index: 2;
    font-family: var(--font-mono);
    font-size: 0.68rem;
    color: var(--fg-faint);
    pointer-events: none;
  }
  .zoomnote.hint {
    top: 45%;
    left: 50%;
    transform: translate(-50%, -50%);
    padding: 0.25rem 0.6rem;
    border: 1px solid var(--border);
    border-radius: 6px;
    background: var(--bg-elev);
    color: var(--fg-muted);
    font-size: 0.78rem;
    animation: hintIn 0.2s ease;
  }
  @keyframes hintIn {
    from {
      opacity: 0;
    }
  }
  .hill.back {
    fill: color-mix(in oklab, var(--g1) 16%, var(--bg-elev));
  }
  .hill.front {
    fill: color-mix(in oklab, var(--g2) 13%, var(--bg-elev));
  }
  .grass path {
    stroke: color-mix(in oklab, var(--g2) 55%, transparent);
    stroke-width: 1.5;
    fill: none;
    stroke-linecap: round;
  }
  .daisy {
    fill: var(--blossom);
  }
  .daisy.alt {
    fill: var(--g4);
    opacity: 0.8;
  }
  .tip {
    position: absolute;
    transform: translate(14px, 14px);
    max-width: 22rem;
    padding: 0.3rem 0.55rem;
    border: 1px solid var(--border);
    border-radius: 6px;
    background: var(--bg-elev);
    color: var(--fg);
    font-family: var(--font-mono);
    font-size: 0.72rem;
    line-height: 1.35;
    pointer-events: none;
    z-index: 2;
    box-shadow: 0 2px 10px color-mix(in oklab, var(--fg) 12%, transparent);
  }
  .nogl {
    position: absolute;
    inset: 40% 0 auto;
    text-align: center;
    color: var(--fg-faint);
    font-size: 0.85rem;
  }
</style>
