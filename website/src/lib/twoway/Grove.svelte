<script lang="ts">
  // A run drawn the way the site's tree-calculus visualizer (TreeVis) draws one: a tree on a hill,
  // data as leaves and knots, and every call a node with its program on the left and its argument
  // on the right. A bud (dashed ring) is a call still to fire, wearing the clause it will fire; once
  // it fires, its program stays where it was, flipped to its dagger (the residual), and only the
  // right side changes. Named programs past a dozen nodes fold into pods; a click on a pod reveals up
  // to UNFOLD_BUDGET nodes of it, and a click on an opened program's root folds it again. Nodes are
  // keyed, so kept pieces glide, new growth sprouts from the root of what was built, and consumed
  // pieces shrink where they stood.
  import { fade, scale } from "svelte/transition";
  import { isReady, isStuck, ruleOf, type Term, type Done } from "./steps";
  import { kids as vkids, progName, programRoles, programText, size, type Reads, type V } from "./patterns";

  interface Ring {
    id: number;
    name: string;
    mark: string | null;
  }
  interface Props {
    root: Term;
    /** For each fired step's id, what clicking it does now: ▶ forward inside, ↺ back. */
    marks: Map<number, string>;
    labelOf: (t: Term) => string | null;
    /** What a bud's tooltip says will happen when it fires. */
    budTip: (id: number) => string;
    onBud: (id: number) => void;
    onRing: (id: number) => void;
    reads: Reads;
    /** Keep each fired call's program (its residual) beside its result, or draw just a ring. */
    keep?: boolean;
    motion?: boolean;
    /** The call that fires next when stepping one at a time; it pulses. */
    next?: number | null;
    height?: number;
  }
  let { root, marks, labelOf, budTip, onBud, onRing, reads, keep = true, motion = true, next = null, height = 420 }: Props = $props();

  const W = 680;
  const GROUND = 46;
  const FOLD_MIN = 16; // a named program at least this big starts folded
  const UNFOLD_BUDGET = 16; // nodes revealed per click on a pod
  const LEAF_TINTS = ["#6dbd7e", "#4aa96c", "#8fce9d", "#3d9b74", "#5cb787"];
  const CODE_TINTS = ["#a990d8", "#9a7ad6", "#b8a3e0"];
  const hash = (s: string) => {
    let h = 0;
    for (let i = 0; i < s.length; i++) h = (h * 31 + s.charCodeAt(i)) | 0;
    return Math.abs(h);
  };
  const dag = (name: string) => (name.endsWith("†") ? name.slice(0, -1) : name + "†");

  // ---- folding: a program part is a pod when it was folded, or is a big named one not yet opened ----
  let opened = $state(new Set<string>());
  let closed = $state(new Set<string>());
  const named = (v: V, root: boolean, name: string) => (root ? !!name && name !== "f" : !!progName(v));
  /** A residual's dagger fork does not count: it folds exactly when its program did. */
  const bulk = (v: V) => (v.tag === "fork" && v.l.tag === "leaf" ? size(v.r) : size(v));
  const isFolded = (key: string, v: V, root: boolean, name: string) =>
    closed.has(key) || (!opened.has(key) && named(v, root, name) && bulk(v) >= FOLD_MIN);
  function reveal(key: string, v: V, pod: number) {
    const o = new Set(opened);
    const c = new Set(closed);
    let left = UNFOLD_BUDGET;
    const visit = (k: string, u: V) => {
      o.add(k);
      c.delete(k);
      left--;
      for (const ch of vkids(u)) {
        const ck = `p${pod}:${ch.id}`;
        const big = !!progName(ch) && bulk(ch) >= FOLD_MIN;
        if (!big || left > 0) visit(ck, ch);
      }
    };
    visit(key, v);
    opened = o;
    closed = c;
  }
  function fold(key: string) {
    const c = new Set(closed);
    c.add(key);
    closed = c;
  }
  function unfoldResidual(pod: number, v: V) {
    const c = new Set(closed);
    c.delete(`pod${pod}†`);
    closed = c;
    reveal(`pod${pod}`, v, pod);
  }

  type Kind = "leaf" | "knot" | "fork" | "bud" | "match" | "done" | "pod" | "code" | "codeleaf";
  interface Laid {
    key: string;
    kind: Kind;
    parent: string | null;
    depth: number;
    x: number;
    y: number;
    act: number | null;
    rings: Ring[];
    glyph: string | null;
    mark: string | null;
    stuck: boolean;
    label: string | null;
    name: string | null;
    tip: string | null;
    /** For pods and opened program parts: what to fold or reveal (dag: a residual, the program
     * with one fork on top). */
    prog?: { v: V; pod: number; root: boolean; dag?: boolean };
  }

  type Item =
    | { t: Term }
    | { v: V; pod: number; root: boolean; name: string; prog: V }
    | { residual: V; pod: number; name: string };

  function scene(t: Term): { nodes: Laid[]; glyph: number } {
    const out: Omit<Laid, "y">[] = [];
    const ringOf = (d: Done): Ring => ({
      id: d.id,
      name: d.was.k === "call" ? d.was.name : "⇄",
      mark: marks.get(d.id) ?? null,
    });
    const progItem = (v: V, pod: number, name: string): Item => ({ v, pod, root: true, name, prog: v });

    function go(it: Item, depth: number, parent: string | null): { rootX: number; w: number } {
      let node: Omit<Laid, "x" | "y" | "depth" | "parent">;
      let kids: Item[] = [];
      if ("t" in it) {
        let n = it.t;
        const rings: Ring[] = [];
        // a fired match, or a fired call when programs are not kept, is a ring on what it gave
        while (n.k === "done" && !(keep && n.was.k === "call")) {
          rings.push(ringOf(n));
          n = n.body;
        }
        const base = { key: `t${n.id}`, act: n.id, rings, glyph: null, mark: null, stuck: false, label: null, name: null, tip: null };
        if (n.k === "done") {
          const w = n.was as Term & { k: "call" };
          node = { ...base, kind: "done", mark: marks.get(n.id) ?? null, name: w.name };
          kids = [{ residual: w.prog, pod: w.pod, name: w.name }, { t: n.body }];
        } else if (n.k === "call") {
          node = { ...base, kind: "bud", glyph: ruleOf(n) || null, stuck: isStuck(n), name: n.name, tip: budTip(n.id) };
          kids = [progItem(n.prog, n.pod, n.name), { t: n.arg }];
        } else if (n.k === "let") {
          node = { ...base, kind: "match", glyph: isReady(n) ? "⇄" : null, tip: budTip(n.id) };
          kids = n.binds.map((b) => ({ t: b.t }));
        } else {
          node = {
            ...base,
            act: rings.length ? rings[0].id : null,
            kind: n.k === "leaf" ? "leaf" : n.k === "stem" ? "knot" : "fork",
            label: labelOf(n),
          };
          kids = n.k === "stem" ? [{ t: n.c }] : n.k === "fork" ? [{ t: n.l }, { t: n.r }] : [];
        }
      } else if ("residual" in it) {
        // the residual is the same program with one fork on top: one pod if the program was one,
        // else that fork and its leaf under the program, which keeps its place and its opened parts
        const v = it.residual;
        const base = { act: null, rings: [], glyph: null, mark: null, stuck: false, label: null };
        if (closed.has(`pod${it.pod}†`) || isFolded(`pod${it.pod}`, v, true, it.name)) {
          node = {
            ...base,
            key: `pod${it.pod}`,
            kind: "pod",
            name: dag(it.name),
            tip: `${dag(it.name)}: ${it.name} run backward, the program that undoes this call (click to open)`,
            prog: { v, pod: it.pod, root: true, dag: true },
          };
        } else {
          node = { ...base, key: `pod${it.pod}†`, kind: "code", name: null, label: dag(it.name), tip: "click to fold" };
          kids = [
            { v: { tag: "leaf", id: -it.pod }, pod: it.pod, root: false, name: "", prog: v },
            progItem(v, it.pod, it.name),
          ];
        }
      } else {
        const key = it.root ? `pod${it.pod}` : `p${it.pod}:${it.v.id}`;
        const role = it.root
          ? it.name
          : it.v.id < 0
            ? "† the dagger mark"
            : (programRoles(it.prog, reads).get(it.v.id) ?? progName(it.v));
        const prog = { v: it.v, pod: it.pod, root: it.root };
        const base = { key, act: null, rings: [], glyph: null, mark: null, stuck: false, label: null, prog };
        if (it.v.tag === "leaf") node = { ...base, kind: "codeleaf", name: null, tip: role ?? "△" };
        else if (isFolded(key, it.v, it.root, it.name))
          node = {
            ...base,
            kind: "pod",
            name: role ?? `${size(it.v)}`,
            tip: `${it.root ? `${it.name}: ${programText(it.v, reads)}` : (role ?? "part")} (click to open)`,
          };
        else {
          node = { ...base, kind: "code", name: null, label: role ?? null, tip: "click to fold" };
          kids = vkids(it.v).map((v) => ({ v, pod: it.pod, root: false, name: "", prog: it.prog }));
        }
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
        const s = go(k, depth + 1, node.key);
        for (let j = before; j < out.length; j++) out[j].x += cum;
        if (i === 0) first = s.rootX + cum;
        last = s.rootX + cum;
        cum += s.w;
      });
      const x = (first + last) / 2;
      out.push({ ...node, parent, depth, x });
      return { rootX: x, w: cum };
    }
    go({ t }, 0, null);
    const maxX = Math.max(1, ...out.map((n) => n.x));
    const maxD = Math.max(1, ...out.map((n) => n.depth));
    const sx = Math.min(92, (W - 90) / maxX);
    const sy = Math.min(78, (height - GROUND - 70) / maxD);
    const ox = (W - maxX * sx) / 2;
    return {
      nodes: out.map((n) => ({ ...n, x: ox + n.x * sx, y: height - GROUND - 14 - n.depth * sy })),
      glyph: Math.max(0.55, Math.min(1.2, Math.min(sx, sy) / 40)),
    };
  }

  const laid = $derived(scene(root));
  const g = $derived(laid.glyph);

  // one tween for every node, with TreeVis's ease (a small overshoot). A node that was on screen
  // glides from where it was; a new one starts at its nearest drawn ancestor's old place, so growth
  // sprouts from the root of what was built and staggers outward by depth.
  let view = $state<Laid[]>([]);
  let fresh = $state(new Map<string, number>());
  let cur = new Map<string, { x: number; y: number }>();
  let raf = 0;
  const ease = (t: number) => {
    const c = 0.9;
    const u = t - 1;
    return 1 + (c + 1) * u * u * u + c * u * u;
  };
  const reduced =
    typeof window !== "undefined" && window.matchMedia?.("(prefers-reduced-motion: reduce)").matches;
  $effect(() => {
    const target = laid.nodes;
    cancelAnimationFrame(raf);
    const animate = motion && !reduced && cur.size > 0;
    const born = target.filter((n) => !cur.has(n.key));
    const top = Math.min(...born.map((n) => n.depth));
    fresh = new Map(born.map((n) => [n.key, Math.min(330, (n.depth - top) * 55)]));
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
      const k = Math.min(1, (now - t0) / 550);
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

  const byKey = $derived(new Map(view.map((n) => [n.key, n])));
  const rootNode = $derived(view.find((n) => n.parent == null));
  const edgeD = (a: Laid, b: Laid) =>
    `M ${a.x} ${a.y} Q ${a.x + (b.x - a.x) * 0.5} ${a.y + (b.y - a.y) * 0.62} ${b.x} ${b.y}`;
  const isCode = (n: Laid) => n.kind === "pod" || n.kind === "code" || n.kind === "codeleaf";
  const clickable = (n: Laid) =>
    n.kind === "pod" || n.kind === "code" || n.kind === "bud" || n.kind === "match" || n.kind === "done" || n.rings.length > 0;
  const dur = (ms: number) => (motion && !reduced ? ms : 0);

  function click(n: Laid, e: MouseEvent) {
    e.stopPropagation();
    if (n.kind === "pod" && n.prog?.dag) unfoldResidual(n.prog.pod, n.prog.v);
    else if (n.kind === "pod" && n.prog) reveal(n.key, n.prog.v, n.prog.pod);
    else if (n.kind === "code") fold(n.key);
    else if (n.kind === "bud" || n.kind === "match") onBud(n.act!);
    else if (n.kind === "done") onRing(n.act!);
    else if (n.rings.length) onRing(n.rings[0].id);
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
      <path
        class="branch trunk"
        d="M {W / 2} {height - GROUND + 12} Q {W / 2} {height - GROUND} {rootNode.x} {rootNode.y}"
      />
    {/if}
    {#each view as n (n.key)}
      {#if n.parent != null && byKey.get(n.parent)}
        <path
          class="branch"
          class:code={isCode(n)}
          d={edgeD(byKey.get(n.parent)!, n)}
          style="stroke-width:{Math.max(1.4, (isCode(n) ? 3 : 5.4 - n.depth * 0.6) * g)}px"
          in:fade={{ duration: dur(300), delay: dur(fresh.get(n.key) ?? 0) }}
          out:fade={{ duration: dur(350) }}
        />
      {/if}
    {/each}

    {#each view as n (n.key)}
      <g class="node" style="transform: translate({n.x}px, {n.y}px)">
        <g
          in:scale={{ duration: dur(480), start: 0, delay: dur(fresh.get(n.key) ?? 0) }}
          out:scale={{ duration: dur(420), start: 0 }}
        >
          <!-- svelte-ignore a11y_click_events_have_key_events, a11y_no_static_element_interactions -->
          <g class="inner" class:clickable={clickable(n)} onclick={(e) => click(n, e)} style="transform: scale({g})">
            {#each n.rings as r, i (r.id)}
              <circle r={12 + i * 3.5} class="donering" class:live={i === 0 && !!r.mark} />
            {/each}
            {#if n.kind === "bud" || n.kind === "match"}
              <circle r="14" class="hitzone" />
              <circle r="8.5" class="bud-ring" class:match={n.kind === "match"} class:stuck={n.stuck} />
              <circle r="3" class="bud-core" class:match={n.kind === "match"} />
              {#if n.glyph}
                <g class="firemark" class:match={n.kind === "match"} class:next={n.act === next} class:eased={motion}>
                  <circle r="7.5" class="fire-ring" />
                  <text class="fire-letter" y="0.5">{n.glyph}</text>
                </g>
              {/if}
              {#if n.act === next}
                <g class="blossom">
                  {#each [36, 108, 180, 252, 324] as a}
                    <circle r="2.6" cx={9.5 * Math.cos((a * Math.PI) / 180)} cy={9.5 * Math.sin((a * Math.PI) / 180)} />
                  {/each}
                </g>
              {/if}
              <title>{n.tip}</title>
            {:else if n.kind === "done"}
              <circle r="14" class="hitzone" />
              <circle r="8.5" class="donering live" />
              <circle r="3.4" class="done-core" />
              {#if n.mark}
                <g class="ringmark" transform="translate(9, -9)">
                  <circle r="6.5" />
                  <text y="0.5">{n.mark}</text>
                </g>
              {/if}
              <title>{n.mark === "↺" ? `${n.name} has finished: click to step it back` : n.mark ? `click to fire what is ready inside ${n.name}` : n.name}</title>
            {:else if n.kind === "pod"}
              <circle r="10" class="podbody" />
              <path class="podseam" d="M0,-6.5 Q2.8,0 0,6.5" />
              {#if (n.name ?? "").length <= 3}
                <text class="vname" y="0.5" style="font-size:{(n.name ?? '').length <= 1 ? 10 : 8}px">{n.name}</text>
              {:else}
                <text class="vname above" y="-15">{n.name}</text>
              {/if}
              <title>{n.tip}</title>
            {:else if n.kind === "code"}
              <circle r="7" class="hitzone" />
              <circle r="3.6" class="codeknot" />
              <title>{n.tip}</title>
            {:else if n.kind === "codeleaf"}
              <path
                class="leafshape"
                d="M0,-7 C4.5,-4 4.5,2.5 0,7 C-4.5,2.5 -4.5,-4 0,-7 Z"
                style="fill:{CODE_TINTS[hash(n.key) % CODE_TINTS.length]}; transform: rotate({(hash(n.key) % 70) - 35}deg)"
              />
              <title>{n.tip}</title>
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
            {#if n.rings[0]}
              <text class="ringname" x={-14 - (n.rings.length - 1) * 3.5} y={15 + (n.rings.length - 1) * 3}>{n.rings[0].name}</text>
              {#if n.rings[0].mark}
                <g class="ringmark" transform="translate({11 + (n.rings.length - 1) * 2.5}, {-11 - (n.rings.length - 1) * 2.5})">
                  <circle r="6.5" />
                  <text y="0.5">{n.rings[0].mark}</text>
                </g>
              {/if}
            {/if}
            {#if n.label}
              <text class="badge" class:codebadge={n.kind === "code"} y="-12">{n.label}</text>
            {/if}
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
      transform: rotate(-0.7deg);
    }
    to {
      transform: rotate(0.7deg);
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
    opacity: 0.5;
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
    fill: none;
    stroke: var(--g4);
    stroke-width: 1.5;
    stroke-dasharray: 3 3;
  }
  .bud-ring.match {
    stroke: #6c9bd2;
  }
  .bud-ring.stuck {
    stroke: var(--err);
    stroke-dasharray: none;
  }
  .bud-core {
    fill: var(--g4);
  }
  .bud-core.match {
    fill: #6c9bd2;
  }
  .done-core {
    fill: color-mix(in oklab, var(--blossom) 70%, var(--bark));
  }
  .fire-ring {
    fill: color-mix(in oklab, var(--g4) 22%, var(--bg-elev));
    stroke: var(--g4);
    stroke-width: 1.2;
  }
  .firemark.match .fire-ring {
    fill: color-mix(in oklab, #6c9bd2 20%, var(--bg-elev));
    stroke: #6c9bd2;
  }
  .fire-letter {
    font-family: var(--font-mono);
    font-size: 8.5px;
    font-weight: 700;
    fill: #8a6414;
    text-anchor: middle;
    dominant-baseline: middle;
  }
  .firemark.match .fire-letter {
    fill: #2f5f95;
  }
  /* a newly ready mark waits for its node to land, then grows in on it */
  .firemark.eased {
    animation: markin 0.26s ease-out 0.34s backwards;
    transform-origin: 0 0;
  }
  .firemark.next {
    animation: firepulse 1s ease-in-out infinite;
    transform-origin: 0 0;
  }
  .firemark.eased.next {
    animation:
      markin 0.26s ease-out 0.34s backwards,
      firepulse 1s ease-in-out 0.75s infinite;
  }
  @keyframes markin {
    from {
      transform: scale(0.3);
      opacity: 0;
    }
  }
  @keyframes firepulse {
    50% {
      transform: scale(1.25);
    }
  }
  .blossom circle {
    fill: var(--blossom);
    opacity: 0.85;
    stroke: #fff;
    stroke-width: 0.5;
  }
  .podbody {
    fill: #79ae74;
    stroke: color-mix(in oklab, #2e5e33 60%, transparent);
    stroke-width: 0.9;
  }
  .inner.clickable:hover .podbody {
    fill: #8abd85;
  }
  .podseam {
    stroke: rgba(255, 255, 255, 0.55);
    stroke-width: 1.1;
    fill: none;
    stroke-linecap: round;
  }
  .vname {
    fill: #1c3a20;
    font-size: 8px;
    font-weight: 700;
    text-anchor: middle;
    dominant-baseline: middle;
    font-family: var(--font-mono);
  }
  .vname.above {
    fill: var(--g2);
    font-size: 10px;
  }
  .donering {
    fill: none;
    stroke: color-mix(in oklab, var(--blossom) 70%, var(--bark));
    stroke-width: 1.1;
    opacity: 0.55;
  }
  .donering.live {
    opacity: 0.95;
    stroke-width: 1.5;
  }
  .ringname {
    font-family: var(--font-mono);
    font-size: 8px;
    fill: var(--fg-faint);
    text-anchor: end;
  }
  .ringmark circle {
    fill: color-mix(in oklab, var(--blossom) 25%, var(--bg-elev));
    stroke: color-mix(in oklab, var(--blossom) 70%, var(--bark));
    stroke-width: 1;
  }
  .ringmark text {
    font-size: 7.5px;
    text-anchor: middle;
    dominant-baseline: middle;
    fill: var(--fg);
  }
  .badge {
    fill: var(--g2);
    font-size: 10px;
    text-anchor: middle;
    font-family: var(--font-mono);
  }
  .badge.codebadge {
    fill: #7a5cc4;
    font-size: 9px;
  }
  .inner.clickable {
    cursor: pointer;
  }
  .hitzone {
    fill: transparent;
  }
  .inner.clickable:hover .bud-ring {
    stroke-width: 2.6;
  }
  .inner.clickable:hover .donering.live {
    stroke-width: 2.4;
  }
</style>
