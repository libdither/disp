<script lang="ts">
  // A run drawn the way the site's tree-calculus visualizer (TreeVis) draws a tree: on a hill, data as
  // leaves and knots, programs in purple. A bud (dashed ring) is a program about to run, its program
  // drawn open on the left and its value on the right; it wears the move it will make. When a
  // combinator starts, the node it takes apart is kept where the bud was, ringed (map's in blue, tri's
  // in amber), with the runs of its parts above it, and the parts of its program travel up into them; a
  // combinator that holds no node is a bead. A finished run is a solid ring that keeps its program.
  // Every node is keyed by what it is, so a move's nodes glide to where it puts them.
  import { fade, scale } from "svelte/transition";
  import { asBit, asNat, form, kids as vkids, registersOf, show, size, type V } from "./core";
  import { nameOf, progDef, progLabel, type Reads } from "./text";
  import { glyph, say, type Node, type Wait } from "./machine";

  interface Props {
    root: Node;
    reads: Reads;
    labels?: boolean;
    motion?: boolean;
    /** Which way the marks point: the moves each node can make forward, or back. */
    fwd?: boolean;
    /** The node that moves next when stepping one move at a time; it pulses. */
    next?: number | null;
    /** A new run: the height starts over (it otherwise keeps to the tallest the run has been). */
    epoch?: unknown;
    /** Draw each run's program open (its moves and named parts as pods), or folded into one pod. */
    programs?: boolean;
    onSite: (id: number) => void;
  }
  let { root, reads, labels = true, motion = true, fwd = true, next = null, epoch = null, programs = true, onSite }: Props =
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

  /** Frames that hold no node of the value: drawn as beads on the way down. */
  const BEAD: Partial<Record<Wait, string>> = { self: "↩", dag: "†", seq1: ";", seq2: ";", fix: "μ", eval: "▷", triF: "?" };
  const BEAD_SAYS: Partial<Record<Wait, string>> = {
    self: "self: the fix's body running again",
    dag: "†: the part inside running the other way",
    seq1: "a ; b, with a running",
    seq2: "a ; b, with b running on what a gave",
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

  type Kind = "leaf" | "knot" | "fork" | "bud" | "done" | "bead" | "hold" | "pod" | "code" | "codeleaf" | "dpod";
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
    /** A program part still to run. */
    pending?: boolean;
  }
  /** Where a node sits in a word, for reading bits: the top of a value, a part of a word, the inside of
   * a word's leaf (a bit or a tuple of bits), or below a bit. △△ is T as a bit, but a one-bit word
   * holding F as a word, so a label needs to know which. */
  type Ctx = "top" | "word" | "leaf" | "bit";
  /** A program part: `site` names the run it belongs to (a run whose program is a part of its parent's
   * keeps the parent's), `root` is that run's whole program, and `pending` is the part of a sequence
   * still to run, which starts folded. */
  type Item =
    | { t: "node"; n: Node; ctx: Ctx; site?: string }
    | { t: "data"; v: V; underStem: boolean; ctx: Ctx; whole?: boolean }
    | { t: "prog"; v: V; site: string; root: V; pending?: V; whole?: boolean };
  /** Frames whose runs run parts of the frame's own program: those runs keep its site. */
  const OWN_PARTS = new Set<Wait>(["dag", "seq1", "seq2", "fix", "mapS", "mapF", "triL", "triS", "triF"]);
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
        const g = glyph(n, fwd);
        const sk = it.site ?? `s${n.id}`;
        const kidSite = n.k === "frame" && OWN_PARTS.has(n.w) ? sk : undefined;
        if (n.k === "run" || n.k === "done") {
          const run = n.k === "run";
          const stuck = run && !glyph(n, true);
          node = {
            ...blank,
            key: `n${n.id}`,
            kind: run ? "bud" : "done",
            site: n.id,
            glyph: run || !fwd ? g : null,
            stuck,
            back: !n.fwd,
            name: progLabel(n.p),
            tip: run
              ? stuck
                ? `stuck: ${say(n)}`
                : `${say(n)}${n.fwd ? "" : " (running backward)"} — click to make this move`
              : `${progLabel(n.p)} finished — click to step it back`
          };
          kids = [
            { t: "prog", v: n.p, site: sk, root: n.p },
            { t: "data", v: run ? n.x : n.y, underStem: false, ctx: it.ctx }
          ];
        } else if (BEAD[n.w]) {
          const named = n.w === "fix" || n.w === "seq1" || n.w === "seq2" ? nameOf(n.p) : null;
          node = {
            ...blank,
            key: `n${n.id}`,
            kind: "bead",
            site: n.id,
            glyph: g,
            back: !n.fwd,
            label: BEAD[n.w]!,
            name: named,
            tip: `${BEAD_SAYS[n.w]}${named ? ` (${named})` : ""} — click to run what is ready inside`
          };
          kids = n.kids.map((k) => ({ t: "node", n: k, ctx: it.ctx, site: kidSite }));
          if (n.w === "seq1") {
            // the part still to run waits beside its bead, and moves into the run when its turn comes
            const f = form(n.p) as { a: V; b: V };
            const rest = n.fwd ? f.b : f.a;
            kids.unshift({ t: "prog", v: rest, site: sk, root: n.p, pending: rest });
          }
          step = 0.62;
        } else {
          const ring = n.w === "mapS" || n.w === "mapF" ? "map" : "tri";
          node = {
            ...blank,
            key: `v${n.at}`,
            kind: "hold",
            site: n.id,
            glyph: g,
            back: !n.fwd,
            shape: n.w === "mapS" || n.w === "triS" ? "knot" : "fork",
            ring,
            label: ring,
            tip: `${HOLD_SAYS[n.w]} — click to run what is ready inside`
          };
          const ctx = below(it.ctx, n.w === "mapS" || n.w === "triS");
          kids =
            n.w === "triL"
              ? [{ t: "data", v: n.kept!, underStem: false, ctx }, { t: "node", n: n.kids[0], ctx, site: kidSite }]
              : n.kids.map((k) => ({ t: "node", n: k, ctx, site: kidSite }));
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
        const base = { ...blank, key, open: { v, site: it.site }, pending: !!it.pending };
        const count = used.get(it.site) ?? 0;
        // a tree opened by a click, or a run's own program, opens whole when it is small; inside a bigger
        // one the moves and named parts stay pods
        let whole = !!it.whole;
        let pod: boolean;
        if (closed.has(key)) pod = true;
        else if (opened.has(key) || (isRoot && programs)) {
          pod = false;
          whole = size(v) <= WHOLE;
        } else
          pod =
            isRoot || v === it.pending || (!whole && ((role !== null && (atomic(v) || bigName(v))) || count >= BUDGET));
        if (pod) {
          const name = isRoot || atomic(v) || v === it.pending ? progLabel(v) : (role ?? "…");
          node = { ...base, kind: "pod", name, tip: `${progDef(v)} — click to open` };
        } else {
          used.set(it.site, count + 1);
          if (v.tag === "leaf") node = { ...base, kind: "codeleaf", label: role, tip: role === "id" ? "id: does nothing" : "△" };
          else {
            const what = role ? `${role}: ${short(progLabel(v), 60)}` : short(show(v), 60);
            node = { ...base, kind: "code", label: role, tip: `${what} — click to fold` };
            kids = vkids(v).map((c) => ({ t: "prog", v: c, site: it.site, root: it.root, pending: it.pending, whole }));
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

  // one tween for every node, with TreeVis's ease (a small overshoot): a node on screen glides from where
  // it was, and a new one starts at its nearest drawn ancestor's old place
  let view = $state<Laid[]>([]);
  let fresh = $state(new Map<string, number>());
  let cur = new Map<string, { x: number; y: number }>();
  let raf = 0;
  const ease = (t: number) => {
    const c = 0.9;
    const u = t - 1;
    return 1 + (c + 1) * u * u * u + c * u * u;
  };
  const reduced = typeof window !== "undefined" && window.matchMedia?.("(prefers-reduced-motion: reduce)").matches;
  $effect(() => {
    const target = laid.nodes;
    cancelAnimationFrame(raf);
    const animate = motion && !reduced && cur.size > 0;
    const born = target.filter((n) => !cur.has(n.key));
    const top = Math.min(...born.map((n) => n.depth));
    fresh = new Map(born.map((n) => [n.key, Math.min(240, (n.depth - top) * 40)]));
    if (!animate) {
      view = target;
      cur = new Map(target.map((n) => [n.key, { x: n.x, y: n.y }]));
      return;
    }
    const byTarget = new Map(target.map((n) => [n.key, n]));
    const from = new Map<string, { x: number; y: number }>();
    const ancestorAt = (n: Laid): { x: number; y: number } | undefined => {
      for (let p = n.parent; p != null; p = byTarget.get(p)?.parent ?? null) {
        const c = cur.get(p);
        if (c) return c;
      }
      return undefined;
    };
    for (const n of target) from.set(n.key, cur.get(n.key) ?? ancestorAt(n) ?? { x: n.x, y: n.y });
    const t0 = performance.now();
    const frame = (now: number) => {
      const k = Math.min(1, (now - t0) / 480);
      const e = ease(k);
      const nextPos = new Map<string, { x: number; y: number }>();
      view = target.map((n) => {
        const f = from.get(n.key)!;
        const p = { x: f.x + (n.x - f.x) * e, y: f.y + (n.y - f.y) * e };
        nextPos.set(n.key, p);
        return { ...n, ...p };
      });
      cur = nextPos;
      if (k < 1) raf = requestAnimationFrame(frame);
    };
    raf = requestAnimationFrame(frame);
    return () => cancelAnimationFrame(raf);
  });

  /** Before the first frame (and on the server) the tree is drawn where it was laid out. */
  const shown = $derived(view.length ? view : laid.nodes);
  const byKey = $derived(new Map(shown.map((n) => [n.key, n])));
  const rootNode = $derived(shown.find((n) => n.parent == null));
  const edgeD = (a: Laid, b: Laid) => `M ${a.x} ${a.y} Q ${a.x + (b.x - a.x) * 0.5} ${a.y + (b.y - a.y) * 0.62} ${b.x} ${b.y}`;
  const isCode = (n: Laid) => n.kind === "pod" || n.kind === "code" || n.kind === "codeleaf";
  const clickable = (n: Laid) => n.site != null || !!n.open;
  const dur = (ms: number) => (motion && !reduced ? ms : 0);

  function click(n: Laid, e: MouseEvent) {
    e.stopPropagation();
    if (n.kind === "pod" || n.kind === "dpod") toggle(n.key, true);
    else if (n.kind === "code" || (n.open && n.site == null && n.kind !== "codeleaf")) toggle(n.key, false);
    else if (n.site != null) onSite(n.site);
  }
</script>

<svg viewBox="0 0 {W} {height}" preserveAspectRatio="xMidYMid meet" role="img" aria-label="the run, as a tree">
  <ellipse cx={W / 2} cy={height - 22} rx={W * 0.36} ry="16" class="hill back" />
  <ellipse cx={W / 2 - 30} cy={height - 16} rx={W * 0.3} ry="13" class="hill front" />
  <g class="grass">
    <path d="M {W / 2 - 118} {height - 24} q 2 -12 7 -16 M {W / 2 - 112} {height - 24} q 3 -8 9 -10" />
    <path d="M {W / 2 + 104} {height - 20} q 2 -12 7 -16 M {W / 2 + 111} {height - 20} q 3 -8 9 -10" />
    <path d="M {W / 2 - 40} {height - 12} q 2 -10 6 -13 M {W / 2 - 34} {height - 12} q 3 -7 8 -9" />
  </g>
  <circle cx={W / 2 - 128} cy={height - 30} r="2.3" class="daisy" />
  <circle cx={W / 2 + 122} cy={height - 26} r="2.3" class="daisy alt" />

  <g class="tree" class:swaying={motion && !reduced}>
    {#if rootNode}
      <path class="branch trunk" d="M {W / 2} {height - GROUND + 12} Q {W / 2} {height - GROUND} {rootNode.x} {rootNode.y}" />
    {/if}
    {#each shown as n (n.key)}
      {#if n.parent != null && byKey.get(n.parent)}
        <path
          class="branch"
          class:code={isCode(n)}
          class:pending={n.pending}
          d={edgeD(byKey.get(n.parent)!, n)}
          style="stroke-width:{Math.max(1.3, (isCode(n) ? 2.6 : 5 - n.depth * 0.45) * g)}px"
          in:fade={{ duration: dur(260), delay: dur(fresh.get(n.key) ?? 0) }}
          out:fade={{ duration: dur(300) }}
        />
      {/if}
    {/each}

    {#each shown as n (n.key)}
      <g class="node" style="transform: translate({n.x}px, {n.y}px)">
        <g in:scale={{ duration: dur(420), start: 0, delay: dur(fresh.get(n.key) ?? 0) }} out:scale={{ duration: dur(360), start: 0 }}>
          <!-- svelte-ignore a11y_click_events_have_key_events, a11y_no_static_element_interactions -->
          <g class="inner" class:clickable={clickable(n)} class:pending={n.pending} onclick={(e) => click(n, e)} style="transform: scale({g})">
            {#if n.kind === "bud"}
              <circle r="14" class="hitzone" />
              <circle r="8.5" class="bud-ring" class:stuck={n.stuck} class:back={n.back} />
              <circle r="3" class="bud-core" class:back={n.back} />
              {#if n.glyph}
                <g class="firemark" class:next={n.site === next} class:eased={motion}>
                  <circle r="7.5" class="fire-ring" class:back={!fwd} />
                  <text class="fire-letter" class:one={n.glyph.length === 1} y="0.5">{n.glyph}</text>
                </g>
              {/if}
              {#if n.site === next}
                <g class="blossom">
                  {#each [36, 108, 180, 252, 324] as a}
                    <circle r="2.6" cx={9.5 * Math.cos((a * Math.PI) / 180)} cy={9.5 * Math.sin((a * Math.PI) / 180)} />
                  {/each}
                </g>
              {/if}
            {:else if n.kind === "done"}
              <circle r="14" class="hitzone" />
              <circle r="8.5" class="donering" />
              <circle r="3.4" class="done-core" />
              <g class="backmark" class:shown={!fwd && n.glyph}>
                <circle r="5.6" cx="9" cy="-9" />
                <text x="9" y="-8.5">↶</text>
              </g>
            {:else if n.kind === "bead"}
              <circle r="11" class="hitzone" />
              <circle r="6.6" class="bead" class:back={n.back} />
              <text class="bead-letter" y="0.6">{n.label}</text>
              {#if n.name}<text class="bead-name" x="-10" y="3">{n.name}</text>{/if}
            {:else if n.kind === "hold"}
              <circle r="13" class="hitzone" />
              <circle r="9.5" class="halo {n.ring}" class:back={n.back} />
              <circle r={n.shape === "fork" ? 4.2 : 3.2} class="knot" />
              <text class="halo-name {n.ring}" x="-12" y="3">{n.label}</text>
            {:else if n.kind === "pod" || n.kind === "dpod"}
              <circle r="9" class="podbody" class:data={n.kind === "dpod"} />
              {#if (n.name ?? "").length <= 4}
                <text class="vname" y="0.5" style="font-size:{[10, 10, 8, 7, 6][(n.name ?? '').length]}px">{n.name}</text>
              {:else}
                <path class="podseam" d="M0,-5.8 Q2.6,0 0,5.8" />
                <text class="vname above" class:data={n.kind === "dpod"} y="-13">{n.name}</text>
              {/if}
            {:else if n.kind === "code"}
              <circle r="7" class="hitzone" />
              <circle r="3.4" class="codeknot" />
            {:else if n.kind === "codeleaf"}
              <path
                class="leafshape"
                d="M0,-7 C4.5,-4 4.5,2.5 0,7 C-4.5,2.5 -4.5,-4 0,-7 Z"
                style="fill:{CODE_TINTS[hash(n.key) % CODE_TINTS.length]}; transform: rotate({(hash(n.key) % 70) - 35}deg)"
              />
            {:else if n.kind === "leaf"}
              <path
                class="leafshape"
                d="M0,-9 C5.8,-5.5 5.8,3 0,9 C-5.8,3 -5.8,-5.5 0,-9 Z"
                style="fill:{LEAF_TINTS[hash(n.key) % LEAF_TINTS.length]}; transform: rotate({(hash(n.key) % 70) - 35}deg)"
              />
              <path class="vein" d="M0,-6 L0,6" style="transform: rotate({(hash(n.key) % 70) - 35}deg)" />
            {:else}
              <circle r={n.kind === "fork" ? 4.2 : 3.2} class="knot" />
            {/if}
            {#if (n.kind === "bead" || n.kind === "hold") && n.glyph}
              <g class="firemark small" class:next={n.site === next} class:eased={motion} transform="translate(8.5, -8.5)">
                <circle r="5.8" class="fire-ring" class:back={!fwd} />
                <text class="fire-letter" class:one={n.glyph.length === 1} y="0.5">{n.glyph}</text>
              </g>
            {/if}
            {#if n.label && (n.kind === "code" || n.kind === "codeleaf" || n.kind === "leaf" || n.kind === "knot" || n.kind === "fork")}
              <text
                class="badge"
                class:codebadge={n.kind === "code" || n.kind === "codeleaf"}
                class:side={n.kind === "knot" || n.kind === "fork"}
                x={n.kind === "knot" || n.kind === "fork" ? 7 : 0}
                y={n.kind === "leaf" || n.kind === "codeleaf" ? -12 : n.kind === "code" ? -8 : 3.2}>{n.label}</text
              >
            {/if}
            {#if n.tip}<title>{n.tip}</title>{/if}
          </g>
        </g>
      </g>
    {/each}
  </g>
</svg>

<style>
  svg {
    width: 100%;
    height: auto;
    max-height: min(74vh, 720px);
    display: block;
    overflow: visible;
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
  .tree {
    transform-origin: 50% 100%;
  }
  .tree.swaying {
    animation: sway 9s ease-in-out infinite alternate;
  }
  @keyframes sway {
    from {
      transform: rotate(-0.6deg);
    }
    to {
      transform: rotate(0.6deg);
    }
  }
  .branch {
    stroke: var(--bark);
    fill: none;
    stroke-linecap: round;
    opacity: 0.8;
  }
  .branch.code {
    stroke: #9a7ad6;
    opacity: 0.55;
  }
  .branch.pending {
    stroke-dasharray: 3 3;
    opacity: 0.35;
  }
  .inner.pending {
    opacity: 0.55;
  }
  .trunk {
    stroke-width: 5;
  }
  .leafshape {
    stroke: color-mix(in oklab, var(--fg) 18%, transparent);
    stroke-width: 0.5;
  }
  .vein {
    stroke: rgba(255, 255, 255, 0.55);
    stroke-width: 0.8;
    fill: none;
  }
  .knot {
    fill: var(--bark);
  }
  .codeknot {
    fill: #8a6bd1;
  }
  .bud-ring {
    fill: color-mix(in oklab, var(--g4) 8%, var(--bg-elev));
    stroke: var(--g4);
    stroke-width: 1.5;
    stroke-dasharray: 3 3;
  }
  .bud-ring.back {
    stroke: #6c9bd2;
  }
  .bud-ring.stuck {
    stroke: var(--err);
    stroke-dasharray: none;
  }
  .bud-core {
    fill: var(--g4);
  }
  .bud-core.back {
    fill: #6c9bd2;
  }
  .done-core {
    fill: color-mix(in oklab, var(--blossom) 70%, var(--bark));
  }
  .donering {
    fill: color-mix(in oklab, var(--blossom) 10%, var(--bg-elev));
    stroke: color-mix(in oklab, var(--blossom) 70%, var(--bark));
    stroke-width: 1.5;
  }
  .backmark {
    opacity: 0;
    transition: opacity 0.15s;
  }
  .backmark.shown,
  .inner.clickable:hover .backmark {
    opacity: 1;
  }
  .backmark circle {
    fill: color-mix(in oklab, #6c9bd2 22%, var(--bg-elev));
    stroke: #6c9bd2;
    stroke-width: 1;
  }
  .backmark text {
    font-size: 7.5px;
    text-anchor: middle;
    dominant-baseline: middle;
    fill: color-mix(in oklab, #2f5f95 72%, var(--fg));
    font-weight: 700;
  }
  .bead {
    fill: color-mix(in oklab, var(--bark) 14%, var(--bg-elev));
    stroke: color-mix(in oklab, var(--bark) 60%, transparent);
    stroke-width: 1.1;
  }
  .bead.back {
    stroke: #6c9bd2;
  }
  .bead-letter {
    font-family: var(--font-mono);
    font-size: 7.5px;
    font-weight: 700;
    fill: var(--fg);
    text-anchor: middle;
    dominant-baseline: middle;
  }
  .bead-name {
    font-family: var(--font-mono);
    font-size: 7.5px;
    fill: var(--fg-faint);
    text-anchor: end;
  }
  .halo {
    fill: none;
    stroke-width: 1.6;
  }
  .halo.map {
    stroke: #5a90cf;
    fill: color-mix(in oklab, #5a90cf 9%, transparent);
  }
  .halo.tri {
    stroke: #c98e2b;
    fill: color-mix(in oklab, #c98e2b 9%, transparent);
  }
  .halo.back {
    stroke-dasharray: 2 2;
  }
  .halo-name {
    font-family: var(--font-mono);
    font-size: 7px;
    text-anchor: end;
  }
  .halo-name.map {
    fill: color-mix(in oklab, #3f73b0 75%, var(--fg));
  }
  .halo-name.tri {
    fill: color-mix(in oklab, #a06d16 75%, var(--fg));
  }
  .fire-ring {
    fill: color-mix(in oklab, var(--g4) 22%, var(--bg-elev));
    stroke: var(--g4);
    stroke-width: 1.2;
  }
  .fire-ring.back {
    fill: color-mix(in oklab, #6c9bd2 20%, var(--bg-elev));
    stroke: #6c9bd2;
  }
  .fire-letter {
    font-family: var(--font-mono);
    font-size: 8.5px;
    font-weight: 700;
    fill: color-mix(in oklab, #8a6414 72%, var(--fg));
    text-anchor: middle;
    dominant-baseline: middle;
  }
  .fire-letter.one {
    font-size: 11px;
  }
  .firemark.small .fire-letter {
    font-size: 7px;
  }
  .firemark.small .fire-letter.one {
    font-size: 8.5px;
  }
  .fire-ring.back + .fire-letter {
    fill: color-mix(in oklab, #2f5f95 72%, var(--fg));
  }
  /* a newly ready mark waits for its node to land, then grows in on it */
  .firemark.eased {
    animation: markin 0.24s ease-out 0.3s backwards;
    transform-origin: 0 0;
    transform-box: fill-box;
  }
  .firemark.next {
    animation: firepulse 1s ease-in-out infinite;
  }
  @keyframes markin {
    from {
      opacity: 0;
    }
  }
  @keyframes firepulse {
    50% {
      opacity: 0.55;
    }
  }
  .blossom circle {
    fill: var(--blossom);
    opacity: 0.85;
    stroke: #fff;
    stroke-width: 0.5;
  }
  .podbody {
    fill: #a58ad8;
    stroke: color-mix(in oklab, #4b2f8a 60%, transparent);
    stroke-width: 0.9;
  }
  .podbody.data {
    fill: #79ae74;
    stroke: color-mix(in oklab, #2e5e33 60%, transparent);
  }
  .inner.clickable:hover .podbody {
    filter: brightness(1.08);
  }
  .podseam {
    stroke: rgba(255, 255, 255, 0.55);
    stroke-width: 1.1;
    fill: none;
    stroke-linecap: round;
  }
  .vname {
    fill: #2c1c52;
    font-size: 8px;
    font-weight: 700;
    text-anchor: middle;
    dominant-baseline: middle;
    font-family: var(--font-mono);
  }
  .vname.above {
    fill: color-mix(in oklab, #7a5cc4 85%, var(--fg));
    font-size: 9px;
    font-weight: 600;
  }
  .vname.above.data {
    fill: var(--g2);
  }
  .badge {
    fill: var(--g2);
    font-size: 9.5px;
    text-anchor: middle;
    font-family: var(--font-mono);
  }
  .badge.side {
    text-anchor: start;
  }
  .badge.codebadge {
    fill: #7a5cc4;
    font-size: 8.5px;
  }
  .inner.clickable {
    cursor: pointer;
  }
  .hitzone {
    fill: transparent;
  }
  .inner.clickable:hover .bud-ring,
  .inner.clickable:hover .halo,
  .inner.clickable:hover .bead,
  .inner.clickable:hover .donering {
    stroke-width: 2.4;
  }
</style>
