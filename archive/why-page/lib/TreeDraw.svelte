<script lang="ts">
  // One real tree, drawn plainly: a stem runs straight down, a fork splits. With `against`, the
  // nodes the other tree does not have at the same place are marked.
  import type { RawTree } from '$lib/disp/protocol'

  interface Props {
    tree: RawTree
    against?: RawTree
    height?: number
    label?: string
  }
  let { tree, against, height = 120, label = 'a tree' }: Props = $props()

  const GAP = 12

  type Box = { t: RawTree; kids: { box: Box; dx: number }[]; lc: number[]; rc: number[] }
  /** Packs a subtree beside its sibling as close as their edges allow, so long chains stay narrow. */
  function pack(t: RawTree): Box {
    if (t === 0) return { t, kids: [], lc: [0], rc: [0] }
    if (t.length === 1) {
      const c = pack(t[0])
      return { t, kids: [{ box: c, dx: 0 }], lc: [0, ...c.lc], rc: [0, ...c.rc] }
    }
    const l = pack(t[0])
    const r = pack(t[1])
    let gap = 0
    for (let d = 0; d < Math.min(l.rc.length, r.lc.length); d++) gap = Math.max(gap, l.rc[d] - r.lc[d] + 1)
    const edge = (a: number[], b: number[]) =>
      Array.from({ length: Math.max(a.length, b.length) }, (_, d) => (d < a.length ? a[d] - gap / 2 : b[d] + gap / 2))
    return {
      t,
      kids: [{ box: l, dx: -gap / 2 }, { box: r, dx: gap / 2 }],
      lc: [0, ...edge(l.lc, r.lc)],
      rc: [0, ...edge(r.rc.map((x) => x + gap), l.rc.map((x) => x - gap))]
    }
  }

  const drawing = $derived.by(() => {
    const dots: { x: number; y: number; kind: 0 | 1 | 2; differs: boolean }[] = []
    const lines: string[] = []
    let lo = 0
    let hi = 0
    let deepest = 0
    const place = (b: Box, other: RawTree | undefined, x: number, y: number, differs: boolean) => {
      const off = differs || (against !== undefined && (other === undefined || kind(other) !== kind(b.t)))
      lo = Math.min(lo, x)
      hi = Math.max(hi, x)
      deepest = Math.max(deepest, y)
      b.kids.forEach((k, i) => {
        lines.push(`M${x * GAP} ${y * GAP}L${(x + k.dx) * GAP} ${(y + 1) * GAP}`)
        place(k.box, off || other === 0 || other === undefined ? undefined : other[i], x + k.dx, y + 1, off)
      })
      dots.push({ x: x * GAP, y: y * GAP, kind: kind(b.t), differs: off })
    }
    place(pack(tree), against, 0, 0, false)
    return { dots, path: lines.join(''), x0: lo * GAP, w: (hi - lo) * GAP, h: deepest * GAP }
  })
  function kind(t: RawTree): 0 | 1 | 2 {
    return t === 0 ? 0 : t.length === 1 ? 1 : 2
  }
  const pad = 8
  // keeps a dot a readable size on screen however far the drawing is scaled down
  const r = $derived(Math.min(5, 1.7 * Math.max(drawing.w / 340, drawing.h / height, 0.6)))
</script>

<svg
  class="td"
  role="img"
  aria-label="{label}: {drawing.dots.length} nodes"
  viewBox="{drawing.x0 - pad} {-pad} {drawing.w + 2 * pad} {drawing.h + 2 * pad}"
  style:height="{height}px"
>
  <path d={drawing.path} />
  {#each drawing.dots as d}
    <circle cx={d.x} cy={d.y} {r} class="k{d.kind}" class:differs={d.differs} />
  {/each}
</svg>

<style>
  .td {
    display: block;
    width: 100%;
    max-width: 100%;
  }
  path {
    stroke: var(--border-strong);
    stroke-width: 1;
    vector-effect: non-scaling-stroke;
    fill: none;
  }
  .k0 {
    fill: var(--g1);
  }
  .k1 {
    fill: var(--g3);
  }
  .k2 {
    fill: var(--fg-muted);
  }
  .differs {
    fill: var(--g4);
  }
</style>
