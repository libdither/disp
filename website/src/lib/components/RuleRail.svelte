<script lang="ts">
  // A quiet ornament for wide screens: the five reduction rules, one per
  // stretch of page, each firing as you scroll through its window. The
  // before-tree stands on the left; scrolling draws the arrow and grows the
  // after-tree — reading the page IS stepping the calculus.
  import { onMount } from 'svelte'

  interface MiniNode {
    x: number
    y: number
    kind: 'leaf' | 'var' | 'apply'
    label?: string
  }
  interface Mini {
    nodes: MiniNode[]
    edges: [number, number][]
  }
  interface Rule {
    tag: string
    formula: string
    before: Mini
    after: Mini
  }

  // hand-laid schemas (coordinates in a 104x86 box)
  const rules: Rule[] = [
    {
      tag: 'L',
      formula: 't ▷ x = t x',
      before: {
        nodes: [
          { x: 30, y: 30, kind: 'leaf' },
          { x: 72, y: 30, kind: 'var', label: 'x' }
        ],
        edges: []
      },
      after: {
        nodes: [
          { x: 52, y: 18, kind: 'leaf' },
          { x: 52, y: 58, kind: 'var', label: 'x' }
        ],
        edges: [[0, 1]]
      }
    },
    {
      tag: 's',
      formula: 't u ▷ x = t u x',
      before: {
        nodes: [
          { x: 28, y: 16, kind: 'leaf' },
          { x: 28, y: 52, kind: 'var', label: 'u' },
          { x: 74, y: 16, kind: 'var', label: 'x' }
        ],
        edges: [[0, 1]]
      },
      after: {
        nodes: [
          { x: 52, y: 14, kind: 'leaf' },
          { x: 32, y: 54, kind: 'var', label: 'u' },
          { x: 72, y: 54, kind: 'var', label: 'x' }
        ],
        edges: [[0, 1], [0, 2]]
      }
    },
    {
      tag: 'K',
      formula: 't t b ▷ x = b',
      before: {
        nodes: [
          { x: 34, y: 14, kind: 'leaf' },
          { x: 16, y: 52, kind: 'leaf' },
          { x: 52, y: 52, kind: 'var', label: 'b' },
          { x: 84, y: 14, kind: 'var', label: 'x' }
        ],
        edges: [[0, 1], [0, 2]]
      },
      after: {
        nodes: [{ x: 52, y: 34, kind: 'var', label: 'b' }],
        edges: []
      }
    },
    {
      tag: 'S',
      formula: 't (t c) b ▷ x = (c x)(b x)',
      before: {
        nodes: [
          { x: 34, y: 12, kind: 'leaf' },
          { x: 16, y: 42, kind: 'leaf' },
          { x: 16, y: 70, kind: 'var', label: 'c' },
          { x: 54, y: 42, kind: 'var', label: 'b' },
          { x: 86, y: 12, kind: 'var', label: 'x' }
        ],
        edges: [[0, 1], [1, 2], [0, 3]]
      },
      after: {
        nodes: [
          { x: 52, y: 10, kind: 'apply' },
          { x: 28, y: 42, kind: 'apply' },
          { x: 76, y: 42, kind: 'apply' },
          { x: 14, y: 72, kind: 'var', label: 'c' },
          { x: 40, y: 72, kind: 'var', label: 'x' },
          { x: 64, y: 72, kind: 'var', label: 'b' },
          { x: 90, y: 72, kind: 'var', label: 'x' }
        ],
        edges: [[0, 1], [0, 2], [1, 3], [1, 4], [2, 5], [2, 6]]
      }
    },
    {
      tag: 'F',
      formula: 't (t c d) b ▷ t = c',
      before: {
        nodes: [
          { x: 36, y: 10, kind: 'leaf' },
          { x: 18, y: 40, kind: 'leaf' },
          { x: 6, y: 70, kind: 'var', label: 'c' },
          { x: 32, y: 70, kind: 'var', label: 'd' },
          { x: 58, y: 40, kind: 'var', label: 'b' },
          { x: 88, y: 10, kind: 'leaf' }
        ],
        edges: [[0, 1], [1, 2], [1, 3], [0, 4]]
      },
      after: {
        nodes: [{ x: 52, y: 34, kind: 'var', label: 'c' }],
        edges: []
      }
    }
  ]

  interface Props {
    // inline: render in normal flow (e.g. inside the Learn sidebar) instead
    // of as a fixed wide-screen margin ornament
    inline?: boolean
  }
  let { inline = false }: Props = $props()

  let idx = $state(0)
  let t = $state(0) // 0..1 within the current rule's scroll window

  onMount(() => {
    const onScroll = () => {
      const doc = document.documentElement
      const total = doc.scrollHeight - window.innerHeight
      const p = total > 0 ? Math.min(0.999, Math.max(0, window.scrollY / total)) : 0
      const raw = p * rules.length
      idx = Math.min(rules.length - 1, Math.floor(raw))
      const local = raw - idx
      // ease with a plateau so the fired state lingers before the next rule
      t = Math.min(1, local * 1.35)
    }
    window.addEventListener('scroll', onScroll, { passive: true })
    onScroll()
    return () => window.removeEventListener('scroll', onScroll)
  })

  const rule = $derived(rules[idx])
  const ease = $derived(t * t * (3 - 2 * t)) // smoothstep
</script>

<aside class="rail" class:inline aria-hidden="true">
  <div class="vine">
    {#each rules as r, i}
      <span class="station" class:done={i < idx} class:live={i === idx}>{r.tag}</span>
    {/each}
  </div>
  {#key idx}
    <div class="vignette">
      <svg viewBox="0 0 104 190">
        <!-- before -->
        <g class="mini before" style="opacity:{1 - ease * 0.55}">
          {#each rule.before.edges as [a, b]}
            <line
              x1={rule.before.nodes[a].x}
              y1={rule.before.nodes[a].y}
              x2={rule.before.nodes[b].x}
              y2={rule.before.nodes[b].y}
            />
          {/each}
          {#each rule.before.nodes as n}
            {#if n.kind === 'leaf'}
              <path
                class="mleaf"
                d="M0,-5 C3.4,-3 3.4,2 0,5 C-3.4,2 -3.4,-3 0,-5 Z"
                style="transform: translate({n.x}px, {n.y}px)"
              />
            {:else if n.kind === 'apply'}
              <circle class="mapply" cx={n.x} cy={n.y} r="4" />
            {:else}
              <circle class="mvar" cx={n.x} cy={n.y} r="5.5" />
              <text x={n.x} y={n.y + 2.6}>{n.label}</text>
            {/if}
          {/each}
        </g>
        <!-- the firing arrow -->
        <g class="firing">
          <path
            class="arrow"
            d="M 52 96 C 52 104 52 112 52 118"
            style="stroke-dasharray: 26; stroke-dashoffset: {26 * (1 - ease)}"
          />
          <path
            class="arrowhead"
            d="M 47 114 L 52 121 L 57 114"
            style="opacity: {ease > 0.85 ? 1 : 0}"
          />
        </g>
        <!-- after -->
        <g
          class="mini after"
          style="opacity:{ease}; transform: translateY(104px) scale({0.7 + 0.3 * ease}); transform-origin: 52px 40px"
        >
          {#each rule.after.edges as [a, b]}
            <line
              x1={rule.after.nodes[a].x}
              y1={rule.after.nodes[a].y}
              x2={rule.after.nodes[b].x}
              y2={rule.after.nodes[b].y}
            />
          {/each}
          {#each rule.after.nodes as n}
            {#if n.kind === 'leaf'}
              <path
                class="mleaf"
                d="M0,-5 C3.4,-3 3.4,2 0,5 C-3.4,2 -3.4,-3 0,-5 Z"
                style="transform: translate({n.x}px, {n.y}px)"
              />
            {:else if n.kind === 'apply'}
              <circle class="mapply" cx={n.x} cy={n.y} r="4" />
            {:else}
              <circle class="mvar" cx={n.x} cy={n.y} r="5.5" />
              <text x={n.x} y={n.y + 2.6}>{n.label}</text>
            {/if}
          {/each}
        </g>
      </svg>
      <div class="formula">{rule.tag} rule · <span class="mono">{rule.formula}</span></div>
    </div>
  {/key}
</aside>

<style>
  .rail {
    position: fixed;
    right: clamp(8px, 1.8vw, 34px);
    top: 50%;
    transform: translateY(-50%);
    width: 128px;
    z-index: 5;
    pointer-events: none;
    display: none;
  }
  @media (min-width: 1460px) {
    .rail { display: block; }
  }
  .rail.inline {
    display: block;
    position: static;
    transform: none;
    width: 100%;
    max-width: 150px;
    margin-top: 1.2rem;
  }

  .vine {
    display: flex;
    justify-content: center;
    gap: 0.5rem;
    margin-bottom: 0.6rem;
  }
  .station {
    width: 20px;
    height: 20px;
    border-radius: 50%;
    border: 1.5px solid var(--border-strong);
    color: var(--fg-faint);
    font-family: var(--font-mono);
    font-size: 0.62rem;
    display: grid;
    place-items: center;
    background: var(--bg-elev);
    transition: all 0.3s ease;
  }
  .station.done {
    background: color-mix(in oklab, var(--g1) 25%, var(--bg-elev));
    border-color: var(--g2);
    color: var(--g2);
  }
  .station.live {
    background: var(--g2);
    border-color: var(--g2);
    color: #f5faf3;
    box-shadow: 0 0 10px color-mix(in oklab, var(--g2) 45%, transparent);
  }

  .vignette {
    /* solid fill — a backdrop-filter here (inside a sticky container) makes
       Chrome's rasterizer pathologically slow */
    background: color-mix(in oklab, var(--g1) 4%, var(--bg-elev));
    border: 1px solid var(--border);
    border-radius: 14px;
    padding: 0.6rem 0.4rem 0.5rem;
    animation: vin 0.35s ease;
  }
  @keyframes vin {
    from { opacity: 0; transform: translateY(6px); }
  }
  svg { width: 100%; overflow: visible; }
  .mini line {
    stroke: var(--bark);
    stroke-width: 1.4;
    opacity: 0.75;
  }
  .mleaf { fill: var(--g1); stroke: none; }
  .mvar {
    fill: var(--bg-elev);
    stroke: var(--g3);
    stroke-width: 1.3;
  }
  .mini text {
    font-family: var(--font-mono);
    font-size: 6.5px;
    text-anchor: middle;
    fill: var(--fg-muted);
  }
  .mapply {
    fill: none;
    stroke: var(--g4);
    stroke-width: 1.3;
    stroke-dasharray: 2 2;
  }
  .arrow {
    stroke: var(--g2);
    stroke-width: 1.8;
    fill: none;
    stroke-linecap: round;
  }
  .arrowhead {
    stroke: var(--g2);
    stroke-width: 1.8;
    fill: none;
    stroke-linecap: round;
    stroke-linejoin: round;
    transition: opacity 0.2s ease;
  }
  .formula {
    text-align: center;
    font-size: 0.6rem;
    color: var(--fg-faint);
    margin-top: 0.3rem;
  }
  .formula .mono { font-family: var(--font-mono); }
</style>
