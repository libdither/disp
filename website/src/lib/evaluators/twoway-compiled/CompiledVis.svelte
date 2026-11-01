<script lang="ts">
  // The compiled net of a two-way program (net.ts), drawn as the tree it is: the program's nodes laid
  // out from the top, each run (a value's top, or a head or reader in the middle of a step) drawn on the
  // wire it sits on with its value hanging beside it. A tick on each node marks its principal port, so
  // the nodes on the way down to a run all point at it. Pairs about to meet are ringed in gold.
  import { onDestroy } from "svelte";
  import { parseProg, parseVal, valText, type Reads } from "$lib/evaluators/twoway/text";
  import { PRESETS } from "$lib/evaluators/twoway/library";
  import { Net, load, idOf, portOf, CURSOR, type Agent } from "./net";

  const EXAMPLES = PRESETS.filter((p) => !p.prog.includes("eval"));
  const FIRST = EXAMPLES.findIndex((p) => p.name === "notw");
  let preset = $state(FIRST);
  let progSrc = $state(EXAMPLES[FIRST].prog);
  let inputSrc = $state(EXAMPLES[FIRST].input);
  let reads = $state<Reads>(EXAMPLES[FIRST].reads);
  let speed = $state(6);
  let playing = $state(false);
  let error = $state("");
  let tick = $state(0);
  let hovered = $state("");
  let fold = $state(true);

  let net: Net | null = null;
  let peak = 0;

  function loadRun() {
    playing = false;
    error = "";
    try {
      net = load(parseProg(progSrc), parseVal(inputSrc));
      peak = net.ag.size;
    } catch (e) {
      net = null;
      error = (e as Error).message;
    }
    fitNext = true;
    tick++;
  }
  function pick(i: number) {
    preset = i;
    progSrc = EXAMPLES[i].prog;
    inputSrc = EXAMPLES[i].input;
    reads = EXAMPLES[i].reads;
    loadRun();
  }
  function step(): number {
    if (!net) return 0;
    const n = net.round();
    peak = Math.max(peak, net.ag.size);
    tick++;
    return n;
  }
  function toEnd() {
    if (!net) return;
    playing = false;
    for (let i = 0; i < 100_000 && net.round(); i++) peak = Math.max(peak, net.ag.size);
    tick++;
  }
  function flip() {
    if (!net) return;
    net.flip();
    tick++;
  }

  let timer: ReturnType<typeof setInterval> | null = null;
  $effect(() => {
    if (timer) clearInterval(timer);
    timer = null;
    if (!playing) return;
    timer = setInterval(() => {
      if (!step()) playing = false;
    }, 1000 / speed);
  });
  onDestroy(() => timer && clearInterval(timer));

  // ---- layout ----

  type Run = { a: Agent; x: number; y: number; arrow: string; val: Val[] };
  type Val = { x: number; y: number; kind: string; px: number; py: number; root: boolean };
  type Pn = { a: Agent; x: number; y: number; w: number; kids: Pn[]; runs: Agent[][]; label: string; parentPort: number; rest: boolean; size: number; folded: boolean; h: number };

  const LINE: Record<string, [number, number]> = { R: [0, 1], H: [1, 2], Rd: [1, 2] };
  const GX = 12;
  /** A row: one level of the program. A sequence's parts go down in a column, indented by IND. */
  const RH = 44;
  const IND = 46;

  function label(a: Agent): string {
    switch (a.tag) {
      case "Gate":
        return a.s;
      case "Seq":
        return ";";
      case "Tri":
      case "TriR":
      case "TriRC":
        return "tri";
      case "Map":
      case "MapH":
        return "map";
      case "Self":
        return "self";
      case "Call":
        return a.nm ?? "fix";
      case "Id":
        return "id";
      case "Stuck":
        return "✕";
      case "Top":
        return "top";
    }
    return a.tag;
  }
  const kidPorts = (a: Agent): number[] =>
    a.tag === "Top" ? [0] : a.tag === "Seq" ? a.ports.map((_, i) => i).slice(1) : a.tag === "Call" ? [1] : ["Tri", "TriR", "TriRC", "Map", "MapH"].includes(a.tag) ? [1, 2, 3] : [];

  /** From a port, along the wire through any runs on it, to the program node below. */
  function follow(n: Net, ref: number): { node: Agent; runs: Agent[] } | null {
    const runs: Agent[] = [];
    for (let guard = 0; guard < 64; guard++) {
      if (ref < 0) return null;
      const b = n.ag.get(idOf(ref));
      if (!b) return null;
      const line = LINE[b.tag];
      if (!line) return { node: b, runs };
      runs.push(b);
      const at = portOf(ref);
      ref = b.ports[at === line[0] ? line[1] : line[0]];
    }
    return null;
  }

  /** The program tree from a node; with `fold`, a part at rest (no run in it) is drawn as one box. */
  function buildTree(n: Net, a: Agent, fold: boolean): Pn {
    const p: Pn = { a, x: 0, y: 0, w: 0, h: 0, kids: [], runs: [], label: label(a), parentPort: 0, rest: a.p === 0, size: 1, folded: false };
    for (const i of kidPorts(a)) {
      const f = follow(n, a.ports[i]);
      if (!f) continue;
      const k = buildTree(n, f.node, fold);
      p.kids.push(k);
      p.runs.push(f.runs);
      p.rest &&= k.rest && !f.runs.length;
      p.size += k.size;
    }
    if (fold && p.rest && p.kids.length && a.tag !== "Top") {
      p.folded = true;
      p.kids = [];
      p.runs = [];
      p.label = a.nm ? a.nm : a.tag === "Call" ? `${label(a)} …` : `${p.size} parts`;
    }
    return p;
  }
  const nodeW = (p: Pn) => Math.max(26, p.label.length * 7 + 12);
  const column = (p: Pn) => p.a.tag === "Seq";
  function measure(p: Pn) {
    const nw = nodeW(p) + GX;
    p.kids.forEach(measure);
    if (!p.kids.length) [p.w, p.h] = [nw, RH];
    else if (column(p)) [p.w, p.h] = [Math.max(nw, IND + Math.max(...p.kids.map((k) => k.w))), RH + p.kids.reduce((s, k) => s + k.h, 0)];
    else [p.w, p.h] = [Math.max(nw, p.kids.reduce((s, k) => s + k.w, 0)), RH + Math.max(...p.kids.map((k) => k.h))];
  }
  function place(p: Pn, left: number, top: number) {
    p.y = top + 12;
    if (column(p) || !p.kids.length) {
      p.x = left + (nodeW(p) + GX) / 2;
      let y = top + RH;
      for (const k of p.kids) {
        place(k, left + IND, y);
        y += k.h;
      }
      return;
    }
    const inner = p.kids.reduce((s, k) => s + k.w, 0);
    let x = left + (p.w - inner) / 2;
    for (const k of p.kids) {
      place(k, x, top + RH);
      x += k.w;
    }
    p.x = (p.kids[0].x + p.kids[p.kids.length - 1].x) / 2;
  }

  /** Value trees laid out small, side by side, hanging from (x, y): under a root (its shape, then its
   * kids), or plain trees held by a head, reader or map. */
  function valueTree(n: Net, refs: number[], x: number, y: number, rootKind: string | null): Val[] {
    const out: { x: number; y: number; kind: string; parent: number }[] = [];
    let leaf = 0;
    const kidsOf = (a: Agent): Agent[] => (a.tag === "S" ? [1] : a.tag === "F" ? [1, 2] : []).map((i) => n.ag.get(idOf(a.ports[i]))!).filter(Boolean);
    const lay = (kind: string, kids: Agent[], d: number, parent: number): number => {
      const i = out.length;
      out.push({ x: 0, y: d * 11, kind, parent });
      const xs = kids.map((c) => lay(c.tag, kidsOf(c), d + 1, i));
      out[i].x = xs.length ? (xs[0] + xs[xs.length - 1]) / 2 : leaf++ * 9;
      return out[i].x;
    };
    const tops = refs.map((r) => n.ag.get(idOf(r))).filter((a): a is Agent => !!a);
    if (rootKind) lay(rootKind, tops, 0, -1);
    else for (const t of tops) lay(t.tag, kidsOf(t), 0, -1);
    const minX = Math.min(...out.map((v) => v.x));
    return out.map((v) => {
      const p = v.parent >= 0 ? out[v.parent] : v;
      return { x: v.x - minX + x, y: v.y + y, kind: v.kind, px: p.x - minX + x, py: p.y + y, root: v.parent < 0 };
    });
  }

  let fitNext = true;
  let view = $state({ x: -50, y: -30, w: 800, h: 520 });

  const scene = $derived.by(() => {
    void tick;
    void fold;
    if (!net) return null;
    const n = net;
    const top = [...n.ag.values()].find((a) => a.tag === "Top");
    if (!top) return null;
    const tree = buildTree(n, top, fold);
    measure(tree);
    place(tree, 0, 0);
    const nodes: Pn[] = [];
    const edges: { d: string; path: boolean; tick: [number, number] | null }[] = [];
    const runs: Run[] = [];
    const held: Val[][] = [];
    const active = new Set<number>();
    for (const [a, b] of n.pairs()) {
      active.add(a.id);
      active.add(b.id);
    }
    const walk = (p: Pn) => {
      nodes.push(p);
      p.kids.forEach((k, j) => {
        const port = kidPorts(p.a)[j];
        const facing = p.a.p === port;
        // a column's part hangs off a spine down its left side; a branch's part is straight below
        let mid: (f: number) => [number, number];
        let toParent: string;
        let toKid: string;
        if (column(p)) {
          const sx = p.x - nodeW(p) / 2 + 6;
          const ex = k.x - nodeW(k) / 2;
          edges.push({ d: `M${sx},${p.y + 10}V${k.y}H${ex}`, path: facing, tick: facing ? [sx + 7, k.y] : null });
          mid = (f) => [sx + (ex - sx) * f, k.y];
          [toParent, toKid] = ["◀", "▶"];
        } else {
          const [x1, y1, x2, y2] = [p.x, p.y + 10, k.x, k.y - 10];
          edges.push({ d: `M${x1},${y1}L${x2},${y2}`, path: facing, tick: facing ? [x1 + (x2 - x1) * 0.15, y1 + (y2 - y1) * 0.15] : null });
          mid = (f) => [x1 + (x2 - x1) * f, y1 + (y2 - y1) * f];
          [toParent, toKid] = ["▲", "▼"];
        }
        const rs = p.runs[j];
        rs.forEach((r, q) => {
          const [x, y] = mid((q + 1) / (rs.length + 1));
          // which way it is going: whether its principal port leads to the node above
          const toward = r.tag === "R" ? n.ag.get(idOf(r.ports[0])) : null;
          const up = !!toward && (toward.id === p.a.id || (q > 0 && toward.id === rs[q - 1].id));
          let val: Val[] = [];
          if (r.tag === "R") val = valueTree(n, r.ports.slice(2), x + 4, y + 16, r.s);
          else if (r.tag === "H") val = valueTree(n, [r.ports[0]], x + 4, y + 16, null);
          else if (r.tag === "Rd") val = valueTree(n, [r.ports[0], ...r.ports.slice(3)], x + 4, y + 16, null);
          runs.push({ a: r, x, y, arrow: r.tag === "R" ? (up ? toParent : toKid) : "", val });
        });
        walk(k);
      });
      if (p.a.tag === "TriR" || p.a.tag === "TriRC") held.push(valueTree(n, [p.a.ports[4], p.a.ports[5]], p.x + 18, p.y + 14, null));
      if (p.a.tag === "MapH") held.push(valueTree(n, [p.a.ports[4]], p.x + 18, p.y + 14, null));
    };
    walk(tree);
    const res = n.result();
    edges.sort((a, b) => Number(a.path) - Number(b.path));
    return { nodes, edges, runs, held, active, w: tree.w, h: tree.h, res };
  });

  $effect(() => {
    if (scene && fitNext) {
      fitNext = false;
      fit();
    }
  });
  function fit() {
    if (!scene) return;
    const w = Math.max(scene.w + 60, 300);
    const h = Math.max(scene.h + 40, 200);
    const aspect = 520 / Math.max(svgW, 300);
    const vw = Math.max(w, h / aspect, 640);
    view = { x: -30 + (w - vw) / 2, y: -30, w: vw, h: vw * aspect };
  }

  let svg: SVGSVGElement | null = $state(null);
  let svgW = $state(800);
  let drag: { x: number; y: number; vx: number; vy: number } | null = null;
  function wheel(ev: WheelEvent) {
    ev.preventDefault();
    if (!svg) return;
    const r = svg.getBoundingClientRect();
    const fx = (ev.clientX - r.left) / r.width;
    const fy = (ev.clientY - r.top) / r.height;
    const k = Math.exp(ev.deltaY * 0.0015);
    const w = view.w * k;
    const h = view.h * k;
    view = { x: view.x + (view.w - w) * fx, y: view.y + (view.h - h) * fy, w, h };
  }
  function down(ev: PointerEvent) {
    drag = { x: ev.clientX, y: ev.clientY, vx: view.x, vy: view.y };
    (ev.target as Element).setPointerCapture?.(ev.pointerId);
  }
  function move(ev: PointerEvent) {
    if (!drag || !svg) return;
    const r = svg.getBoundingClientRect();
    view = { ...view, x: drag.vx - ((ev.clientX - drag.x) * view.w) / r.width, y: drag.vy - ((ev.clientY - drag.y) * view.h) / r.height };
  }
  const up = () => (drag = null);

  function describe(a: Agent): string {
    const dir = a.e ? "forward" : "backward";
    switch (a.tag) {
      case "R":
        return `the run: the value's top (${a.s === "L" ? "a leaf" : a.s === "S" ? "a stem" : "a fork"}), going ${dir}`;
      case "H":
        return `a head going ${dir}: it makes the node under it the value's top${a.f ? ", then turns" : ""}`;
      case "Rd":
        return `a reader going ${dir}, finishing ${a.s.replace(/S$|F$/, "")} by reading the node a level down${a.f ? ", then turning" : ""}`;
      case "TriR":
        return `tri reading the value's left, to choose its part${a.f ? " (then turning)" : ""}`;
      case "TriRC":
        return `tri reading the value as its third part is left${a.f ? " (then turning)" : ""}`;
      case "MapH":
        return a.p === 4 ? "map turned around: its finished half goes back down" : `map holding its finished ${a.s === "l" ? "left" : "right"} half, waiting for the other`;
      case "Call":
        return `a copy of ${a.nm ?? `fix #${a.n}`}'s body: it rolls back up into a self when the run comes out`;
      case "Self":
        return `self: unrolls fix #${a.n}'s body when the run reaches it`;
      case "Gate":
        return `the move ${a.s} (run backward: ${({ stem: "stem†", "stem†": "stem", unit: "unit†", "unit†": "unit" } as Record<string, string>)[a.s] ?? a.s})`;
      case "Seq":
        return a.p ? `a sequence, running part ${a.p} of ${a.ports.length - 1}` : `a sequence of ${a.ports.length - 1} parts, at rest`;
      case "Tri":
        return a.p ? `tri, running part ${"abc"[a.p - 1]}` : "tri, at rest";
      case "Map":
        return a.p ? `map, running ${a.p === 1 ? "its first part on the stem" : "its two halves"}` : "map, at rest";
    }
    return a.tag;
  }

  const status = $derived.by(() => {
    void tick;
    if (error) return { text: error, bad: true };
    if (!net) return { text: "", bad: false };
    const res = scene?.res;
    if (res) return { text: `${res.fwd ? "done" : "back at the start"}: ${valText(res.val, reads)}`, bad: false };
    if (net.stuck && !net.pairs().some(([a, b]) => a.tag === "E" || b.tag === "E")) return { text: `stuck: ${net.stuck}`, bad: true };
    const runs = [...net.ag.values()].filter((a) => CURSOR.has(a.tag)).length;
    return { text: `running · ${runs} run${runs === 1 ? "" : "s"} going`, bad: false };
  });
  const counts = $derived.by(() => {
    void tick;
    if (!net) return [];
    return [
      ["rounds", net.rounds],
      ["meetings", net.meetings],
      ["erasing", net.erasing],
      ["agents now", net.ag.size],
      ["most agents", peak]
    ] as [string, number][];
  });

  const COLOR = { fwd: "#d08a1e", bwd: "#4f86c6" };
  loadRun();
</script>

<div class="cvis">
  <div class="row">
    <select value={preset} onchange={(e) => pick(+e.currentTarget.value)} aria-label="example">
      {#each EXAMPLES as ex, i}<option value={i}>{ex.name}</option>{/each}
    </select>
    <input class="prog" spellcheck="false" bind:value={progSrc} onkeydown={(e) => e.key === "Enter" && loadRun()} aria-label="program" />
    <span class="on">on</span>
    <input class="inp" spellcheck="false" bind:value={inputSrc} onkeydown={(e) => e.key === "Enter" && loadRun()} aria-label="input" />
    <button onclick={loadRun}>load</button>
  </div>
  <div class="stage">
    <svg
      bind:this={svg}
      bind:clientWidth={svgW}
      viewBox="{view.x} {view.y} {view.w} {view.h}"
      onwheel={wheel}
      onpointerdown={down}
      onpointermove={move}
      onpointerup={up}
      onpointerleave={up}
      role="img"
      aria-label="the compiled net"
    >
      {#if scene}
        {#each scene.edges as e}
          <path d={e.d} class="edge" class:path={e.path} />
          {#if e.tick}<circle cx={e.tick[0]} cy={e.tick[1]} r="2.8" class="tick" />{/if}
        {/each}
        {#each scene.nodes as p}
          {@const w = nodeW(p)}
          <g role="note" class="node" class:folded={p.folded} class:active={scene.active.has(p.a.id)} class:reading={p.a.tag === "TriR" || p.a.tag === "TriRC" || p.a.tag === "MapH"} class:copy={p.a.tag === "Call"} onpointerenter={() => (hovered = p.folded ? `${p.label}: ${p.size} program nodes at rest, folded (untick "fold" to see them)` : describe(p.a))}>
            <rect x={p.x - w / 2} y={p.y - 10} width={w} height="20" rx="6" />
            <text x={p.x} y={p.y + 4}>{p.label}</text>
            {#if p.a.p === 0 && p.a.tag !== "Top"}<circle cx={p.x} cy={p.y - 12} r="2.2" class="tick rest" />{/if}
          </g>
        {/each}
        {#each scene.held as vs}
          {#each vs as v}
            {#if !v.root}<line x1={v.x} y1={v.y} x2={v.px} y2={v.py} class="vedge" />{/if}
          {/each}
          {#each vs as v}<circle cx={v.x} cy={v.y} r="3" class="v {v.kind}" />{/each}
        {/each}
        {#each scene.runs as r}
          {@const c = r.a.e ? COLOR.fwd : COLOR.bwd}
          {#if r.val.length}
            {@const xs = r.val.map((v) => v.x)}
            {@const ys = r.val.map((v) => v.y)}
            <rect x={Math.min(...xs) - 6} y={Math.min(...ys) - 6} width={Math.max(...xs) - Math.min(...xs) + 12} height={Math.max(...ys) - Math.min(...ys) + 12} rx="5" class="card" />
          {/if}
          {#each r.val as v}
            {#if !v.root}<line x1={v.x} y1={v.y} x2={v.px} y2={v.py} class="vedge" />{/if}
          {/each}
          {#if r.val.length}<line x1={r.x} y1={r.y} x2={r.val.find((v) => v.root)?.x ?? r.x} y2={r.val.find((v) => v.root)?.y ?? r.y} class="vedge hang" />{/if}
          {#each r.val as v}<circle cx={v.x} cy={v.y} r="3" class="v {v.kind}" />{/each}
          <g role="note" class="run" class:active={scene.active.has(r.a.id)} onpointerenter={() => (hovered = describe(r.a))}>
            {#if r.a.tag === "R"}
              <circle cx={r.x} cy={r.y} r="8" fill={c} />
              <text x={r.x} y={r.y + 3.5} class="arrow">{r.arrow}</text>
            {:else}
              <rect x={r.x - 6} y={r.y - 6} width="12" height="12" fill={c} transform="rotate(45 {r.x} {r.y})" />
              {#if r.a.f}<text x={r.x + 10} y={r.y - 6} class="shape">⇆</text>{/if}
            {/if}
          </g>
        {/each}
      {/if}
    </svg>
    <div class="hud">
      <div class="status" class:bad={status.bad}>{status.text}</div>
    </div>
  </div>
  <div class="row">
    <button onclick={() => (playing = !playing)} class:on={playing} title="play">{playing ? "❚❚" : "▶▶"}</button>
    <button onclick={() => ((playing = false), step())} title="one round">▶ round</button>
    <button onclick={toEnd} title="to the end">⏭</button>
    <button class="flip" onclick={flip} title="turn every run around">⇆ flip</button>
    <button onclick={fit} title="fit to view">fit</button>
    <label class="speed"><input type="checkbox" bind:checked={fold} /> fold parts at rest</label>
    <label class="speed">speed <input type="range" min="1" max="40" bind:value={speed} /></label>
  </div>
  <div class="side">
    <div class="counts">
      {#each counts as [k, v]}<span><b>{v}</b> {k}</span>{/each}
    </div>
    <div class="detail">{hovered || "point at a node or a run"}</div>
    <div class="legend">
      <span><i class="disc" style="background:{COLOR.fwd}"></i>the run, forward</span>
      <span><i class="disc" style="background:{COLOR.bwd}"></i>the run, backward</span>
      <span><i class="dia"></i>head or reader, mid-step</span>
      <span><i class="tk"></i>a node's principal port</span>
      <span><i class="pth"></i>on the way down to a run</span>
      <span><i class="dot leaf"></i>leaf <i class="dot stem"></i>stem <i class="dot fork"></i>fork</span>
      <span><i class="ring"></i>about to meet</span>
    </div>
  </div>
</div>

<style>
  .cvis {
    border: 1px solid var(--border);
    border-radius: 12px;
    background: var(--bg-elev);
    padding: 0.7rem;
    display: flex;
    flex-direction: column;
    gap: 0.5rem;
  }
  .stage {
    position: relative;
    height: 540px;
    border-radius: 8px;
    background: var(--bg);
  }
  svg {
    width: 100%;
    height: 100%;
    cursor: grab;
    touch-action: none;
    user-select: none;
  }
  .hud {
    position: absolute;
    top: 6px;
    left: 10px;
    pointer-events: none;
    font-family: var(--font-mono);
    font-size: 0.85rem;
    color: var(--fg);
  }
  .status.bad {
    color: var(--err);
  }
  .edge {
    fill: none;
    stroke: color-mix(in oklab, #9a7ad6 55%, var(--bg));
    stroke-width: 1.4;
  }
  .edge.path {
    stroke: #7a5cc4;
    stroke-width: 3;
  }
  .tick {
    fill: #7a5cc4;
  }
  .tick.rest {
    fill: color-mix(in oklab, #9a7ad6 70%, var(--bg));
  }
  .node rect {
    fill: color-mix(in oklab, #9a7ad6 22%, var(--bg-elev));
    stroke: #9a7ad6;
    stroke-width: 1;
  }
  .node.folded rect {
    fill: color-mix(in oklab, #9a7ad6 10%, var(--bg-elev));
    stroke-dasharray: 1 2;
  }
  .node.copy rect {
    stroke-dasharray: 3 2;
  }
  .node.reading rect {
    fill: color-mix(in oklab, #3d9b74 30%, var(--bg-elev));
    stroke: #3d9b74;
  }
  .node.active rect,
  .run.active circle,
  .run.active rect {
    stroke: #e0a526;
    stroke-width: 2.6;
  }
  .node text {
    font-family: var(--font-mono);
    font-size: 11px;
    fill: var(--fg);
    text-anchor: middle;
    pointer-events: none;
  }
  .run text.arrow {
    font-size: 8px;
    fill: white;
    text-anchor: middle;
    pointer-events: none;
  }
  .run text.shape {
    font-family: var(--font-mono);
    font-size: 9px;
    fill: var(--fg-muted);
    pointer-events: none;
  }
  .card {
    fill: color-mix(in oklab, var(--bg) 85%, transparent);
    stroke: color-mix(in oklab, #4aa96c 40%, transparent);
    stroke-width: 0.8;
  }
  .vedge {
    stroke: #4aa96c;
    stroke-width: 1;
  }
  .vedge.hang {
    stroke-dasharray: 2 2;
  }
  .v {
    stroke: #4aa96c;
    stroke-width: 1.2;
  }
  .v.L {
    fill: #4aa96c;
  }
  .v.S {
    fill: var(--bg);
  }
  .v.F {
    fill: color-mix(in oklab, #4aa96c 45%, var(--bg));
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
    width: 11rem;
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
  }
  .row button.on {
    border-color: var(--accent);
  }
  .row button.flip {
    color: var(--accent);
  }
  .speed {
    display: flex;
    gap: 0.3rem;
    align-items: center;
    color: var(--fg-muted);
  }
  .on {
    color: var(--fg-muted);
  }
  .side {
    display: flex;
    flex-direction: column;
    gap: 0.4rem;
    font-size: 0.8rem;
  }
  .counts {
    display: flex;
    gap: 1rem;
    flex-wrap: wrap;
    justify-content: center;
    font-family: var(--font-mono);
    color: var(--fg-muted);
  }
  .counts b {
    color: var(--fg);
  }
  .detail {
    text-align: center;
    min-height: 1.2em;
    color: var(--fg);
  }
  .legend {
    display: flex;
    flex-wrap: wrap;
    gap: 0.4rem 1rem;
    justify-content: center;
    color: var(--fg-muted);
  }
  .legend i {
    display: inline-block;
    vertical-align: middle;
    margin-right: 0.3rem;
  }
  .legend .disc {
    width: 10px;
    height: 10px;
    border-radius: 50%;
  }
  .legend .dia {
    width: 8px;
    height: 8px;
    background: #d08a1e;
    transform: rotate(45deg);
  }
  .legend .tk {
    width: 6px;
    height: 6px;
    border-radius: 50%;
    background: #7a5cc4;
  }
  .legend .pth {
    width: 16px;
    height: 3px;
    background: #7a5cc4;
  }
  .legend .dot {
    width: 7px;
    height: 7px;
    border-radius: 50%;
    border: 1.2px solid #4aa96c;
  }
  .legend .dot.leaf {
    background: #4aa96c;
  }
  .legend .dot.stem {
    background: transparent;
  }
  .legend .dot.fork {
    background: color-mix(in oklab, #4aa96c 45%, transparent);
  }
  .legend .ring {
    width: 9px;
    height: 9px;
    border-radius: 3px;
    border: 2px solid #e0a526;
  }
</style>
