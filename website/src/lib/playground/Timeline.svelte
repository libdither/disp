<script lang="ts">
  // The reel at a glance: one tick per binder rewrite, then the run's tree size
  // over every step (a log scale, so a storm and a calm both read) over a strip
  // coloured by which rule fired. Drag to scrub.
  import { onMount, untrack } from 'svelte'
  import { theme } from '$lib/theme.svelte'
  import { followTheme } from './theme-follow.ts'
  import { R_K, R_S, R_F, R_LEAF, R_STEM, R_BIND } from './store.ts'
  import type { Trace } from './trace.ts'

  interface Props {
    trace: Trace | null
    /// the reel position: rewrites first, then the trace's frames
    at: number
    /// how many reel positions come before the trace's first frame
    lead: number
    /// bumped as more frames arrive
    version: number
    onScrub: (at: number) => void
  }
  let { trace, at, lead, version, onScrub }: Props = $props()

  let cv: HTMLCanvasElement
  let host: HTMLDivElement
  let w = 0
  const H = 26
  const GAP = 4
  let C: Record<string, string> = {}
  let cols: { hi: number; rule: number }[] = []
  let cachedFor: Trace | null = null
  let cachedLen = -1
  let cachedW = 0
  let maxLog = 1

  function readColors() {
    const probe = document.createElement('span')
    // reduced motion gives every element a tiny transition, which would hand back the old colour
    probe.style.transition = 'none'
    host.appendChild(probe)
    const get = (v: string) => {
      probe.style.color = `var(${v})`
      return getComputedStyle(probe).color
    }
    C = { binder: get('--hl-kw'), fill: get('--accent'), track: get('--bg-panel-hover'), mark: get('--fg'), warn: get('--warn'), kw: get('--hl-kw'), err: get('--err'), build: get('--fg-faint') }
    probe.remove()
  }

  /// The binders' share of the width: enough to read, however long the run is.
  function binderWidth(): number {
    if (lead === 0) return 0
    if (!trace || trace.last === 0) return w
    return Math.round(w * Math.max(0.16, Math.min(0.5, lead / (lead + trace.last))))
  }

  function columns(rw: number) {
    if (!trace) return
    const n = trace.frames.length
    if (trace === cachedFor && n === cachedLen && rw === cachedW) return
    cachedFor = trace
    cachedLen = n
    cachedW = rw
    const count = Math.max(1, Math.floor(rw))
    cols = new Array(count)
    maxLog = 1
    for (let c = 0; c < count; c++) {
      const from = Math.floor((c / count) * n)
      const to = Math.max(from + 1, Math.floor(((c + 1) / count) * n))
      let hi = 0
      const hist = [0, 0, 0, 0, 0, 0, 0]
      for (let i = from; i < to && i < n; i++) {
        if (trace.sizes[i] > hi) hi = trace.sizes[i]
        hist[trace.rules[i]]++
      }
      let rule = 0
      for (let r = 3; r <= 6; r++) if (hist[r] > 0 && hist[r] >= hist[rule]) rule = r
      if (rule === 0 && (hist[R_LEAF] || hist[R_STEM])) rule = R_LEAF
      const lg = Math.log1p(hi)
      if (lg > maxLog) maxLog = lg
      cols[c] = { hi: lg, rule }
    }
  }

  function draw() {
    if (!cv || !trace || !w) return
    const bw = binderWidth()
    const x0 = bw === 0 ? 0 : Math.min(w, bw + GAP)
    const rw = w - x0
    const dpr = window.devicePixelRatio || 1
    const ctx = cv.getContext('2d')!
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0)
    ctx.clearRect(0, 0, w, H)
    const strip = 4
    const plot = H - strip - 2
    if (bw > 0) {
      ctx.fillStyle = C.track
      ctx.fillRect(0, plot - 8, bw, 8)
      ctx.fillStyle = C.binder
      const ticks = Math.max(1, lead)
      const tw = Math.max(1, bw / ticks - 1)
      for (let i = 0; i < ticks; i++) ctx.fillRect((i / ticks) * bw, plot - 8, tw, 8)
    }
    if (rw > 1 && trace.last > 0) {
      columns(rw)
      ctx.fillStyle = C.fill
      ctx.globalAlpha = 0.4
      ctx.beginPath()
      ctx.moveTo(x0, plot)
      cols.forEach((c, i) => ctx.lineTo(x0 + i, plot - (c.hi / maxLog) * (plot - 2)))
      ctx.lineTo(x0 + cols.length, plot)
      ctx.closePath()
      ctx.fill()
      ctx.globalAlpha = 1
      cols.forEach((c, i) => {
        ctx.fillStyle = c.rule === R_BIND ? C.binder : c.rule === R_K ? C.warn : c.rule === R_S ? C.kw : c.rule === R_F ? C.err : c.rule === R_LEAF ? C.build : C.track
        ctx.fillRect(x0 + i, H - strip, 1, strip)
      })
    }
    const x = at < lead ? (at / Math.max(1, lead)) * bw : trace.last > 0 ? x0 + ((at - lead) / trace.last) * (rw - 1) : Math.min(w - 2, bw)
    ctx.fillStyle = C.mark
    ctx.fillRect(Math.round(x) - 0.5, 0, 2, H)
  }

  let dragging = false
  function scrubAt(e: PointerEvent) {
    if (!trace) return
    const r = cv.getBoundingClientRect()
    const px = Math.max(0, Math.min(w, ((e.clientX - r.left) / r.width) * w))
    const bw = binderWidth()
    const x0 = bw === 0 ? 0 : Math.min(w, bw + GAP)
    if (px < x0 && bw > 0) onScrub(Math.min(lead, Math.round((px / bw) * lead)))
    else onScrub(lead + Math.round(((px - x0) / Math.max(1, w - x0)) * trace.last))
  }

  function resize() {
    w = host.clientWidth
    const dpr = window.devicePixelRatio || 1
    cv.width = Math.round(w * dpr)
    cv.height = Math.round(H * dpr)
    draw()
  }

  onMount(() => {
    readColors()
    resize()
    const ro = new ResizeObserver(resize)
    ro.observe(host)
    return () => ro.disconnect()
  })

  $effect(() => {
    void trace
    void at
    void lead
    void version
    draw()
  })
  $effect(() => {
    void theme.mix
    if (host) return untrack(() => followTheme(() => (readColors(), draw())))
  })
</script>

<div class="tl" bind:this={host}>
  <canvas
    bind:this={cv}
    style="height:{H}px"
    role="slider"
    tabindex="-1"
    aria-label="reduction timeline"
    aria-valuemin="0"
    aria-valuemax={lead + (trace?.last ?? 0)}
    aria-valuenow={at}
    onpointerdown={(e) => {
      dragging = true
      cv.setPointerCapture(e.pointerId)
      scrubAt(e)
    }}
    onpointermove={(e) => dragging && scrubAt(e)}
    onpointerup={() => (dragging = false)}
    onpointercancel={() => (dragging = false)}
  ></canvas>
</div>

<style>
  .tl {
    width: 100%;
    line-height: 0;
  }
  canvas {
    width: 100%;
    cursor: ew-resize;
    touch-action: none;
  }
</style>
