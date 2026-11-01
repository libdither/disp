<script lang="ts">
  // Scene 8: half-written programs tried against a spec. A toy in this page's JavaScript (search.ts);
  // the verdicts are real runs of the tree-calculus rules on those half-written programs.
  import { onDestroy } from 'svelte'
  import { SPECS, search, NO_IDEA, proposerFrom, ruledOut, show, treesOfSize } from './search'

  const SIZES = treesOfSize(14)
  const tallest = SIZES[SIZES.length - 1]

  let specName = $state(SPECS[0].name)
  let who = $state<'none' | 'good' | 'liar'>('none')
  const spec = $derived(SPECS.find((s) => s.name === specName)!)
  const answer = $derived(search(spec, NO_IDEA).found!.prog)
  const result = $derived(
    who === 'none' ? search(spec, NO_IDEA) : search(spec, proposerFrom(answer, who === 'good' ? 0.9 : 0.02))
  )

  const W = 640
  const H = 210
  const layout = $derived.by(() => {
    const byDepth = new Map<number, number[]>()
    for (const n of result.nodes) byDepth.set(n.depth, [...(byDepth.get(n.depth) ?? []), n.id])
    const deepest = Math.max(...byDepth.keys(), 1)
    const pos = new Map<number, { x: number; y: number }>()
    for (const [depth, ids] of byDepth)
      ids.forEach((id, i) => pos.set(id, { x: (W * (i + 1)) / (ids.length + 1), y: 10 + ((H - 20) * depth) / deepest }))
    return pos
  })

  let reveal = $state(Infinity)
  let timer: ReturnType<typeof setInterval> | undefined
  onDestroy(() => clearInterval(timer))
  function replay() {
    clearInterval(timer)
    if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) {
      reveal = Infinity
      return
    }
    reveal = 0
    const by = Math.max(1, Math.ceil(result.nodes.length / 90))
    timer = setInterval(() => {
      reveal += by
      if (reveal >= result.nodes.length) {
        reveal = Infinity
        clearInterval(timer)
      }
    }, 35)
  }
  const shown = $derived(result.nodes.slice(0, reveal))
  const edges = $derived(
    shown
      .filter((n) => n.parent !== null)
      .map((n) => {
        const a = layout.get(n.parent!)!
        const b = layout.get(n.id)!
        return `M${a.x} ${a.y}L${b.x} ${b.y}`
      })
      .join('')
  )
  const count = (v: string) => result.nodes.filter((n) => n.outcome.v === v).length
  const blind = $derived(search(spec, NO_IDEA).nodes.length)
  const out14 = $derived(ruledOut(result.nodes, 14))
  const r = $derived(result.nodes.length > 600 ? 1.5 : result.nodes.length > 100 ? 2.6 : 4.5)
</script>

<div class="why-widget">
  <h4>Trees of each size</h4>
  <div class="bars" role="img" aria-label="The number of trees grows from 1 at size 1 to {tallest} at size 14">
    {#each SIZES as n, i}
      <div class="col" title="{n.toLocaleString()} trees of {i + 1} nodes">
        <span class="bar" style:height="{Math.max(1.5, (n / tallest) * 100)}%"></span>
      </div>
    {/each}
  </div>
  <div class="axis"><span>1 node: 1 tree</span><span>14 nodes: {tallest.toLocaleString()} trees</span></div>
  <p class="why-note">Doubling is a tree of 186 nodes. Trying every tree up to that size is out of the question.</p>
</div>

<div class="why-widget">
  <div class="why-row" role="group" aria-label="What to find">
    <span class="why-label">Find a program for:</span>
    {#each SPECS as s (s.name)}
      <button class="why-chip" aria-pressed={specName === s.name} onclick={() => ((specName = s.name), (reveal = Infinity))}>{s.name}</button>
    {/each}
  </div>
  <div class="why-row who" role="group" aria-label="Who proposes">
    <span class="why-label">Who proposes:</span>
    <button class="why-chip" aria-pressed={who === 'none'} onclick={() => ((who = 'none'), (reveal = Infinity))}>no idea</button>
    <button class="why-chip" aria-pressed={who === 'good'} onclick={() => ((who = 'good'), (reveal = Infinity))}>good guesses</button>
    <button class="why-chip" aria-pressed={who === 'liar'} onclick={() => ((who = 'liar'), (reveal = Infinity))}>confidently wrong</button>
    <button class="why-chip" onclick={replay}>▶ Replay</button>
  </div>
  <svg viewBox="0 0 {W} {H}" class:busy={result.nodes.length > 600} role="img" aria-label="{result.nodes.length} half-written programs tried, by how much of each is written">
    <path d={edges} class="edge" />
    {#each shown as n (n.id)}
      {@const p = layout.get(n.id)!}
      <circle cx={p.x} cy={p.y} r={n.outcome.v === 'Proved' ? 7 : r} class={n.outcome.v}><title>{show(n.prog)}: {n.outcome.v}</title></circle>
    {/each}
  </svg>
  <p class="key">
    <span class="k Refuted"></span> Refuted: the branch ends
    <span class="k Open"></span> Open: still alive
    <span class="k Proved"></span> Proved: the search ends
  </p>
  <div class="stats">
    <div><b>{result.nodes.length.toLocaleString()}</b><span>half-written programs tried</span></div>
    <div><b>{count('Refuted').toLocaleString()}</b><span>refuted</span></div>
    <div><b>{(out14 * 100).toFixed(0)}%</b><span>of all trees up to 14 nodes ruled out by those failures</span></div>
    <div><code>{result.found ? show(result.found.prog) : 'none'}</code><span>found. <code>?</code> is a part it never needed</span></div>
  </div>
  <p class="why-note">
    {#if who === 'good'}
      Good guesses took {result.nodes.length} tries against {blind} with no idea. The search pays only for what the proposer did not already know.
    {:else if who === 'liar'}
      Bad guesses took {result.nodes.length.toLocaleString()} tries against {blind}, and the answer is still right, because every candidate went through the checker. A bad proposer costs time, never correctness.
    {:else}
      With no idea, it pays full price: {result.nodes.length} tries. Switch the proposer and watch only that number change.
    {/if}
    This is a toy on a list of examples, in this page's JavaScript. Disp's own search is not built.
  </p>
</div>

<style>
  h4 {
    margin: 0 0 0.5rem;
    font-family: var(--font-body);
    font-size: 0.85rem;
    font-weight: 600;
  }
  .bars {
    display: flex;
    align-items: flex-end;
    gap: 2px;
    height: 6.5rem;
    border-bottom: 1px solid var(--border-strong);
  }
  .col {
    flex: 1;
    height: 100%;
    display: flex;
    align-items: flex-end;
  }
  .bar {
    display: block;
    width: 100%;
    border-radius: 4px 4px 0 0;
    background: var(--g2);
  }
  .axis {
    display: flex;
    justify-content: space-between;
    font-size: 0.74rem;
    color: var(--fg-muted);
    margin-top: 0.25rem;
  }
  .who {
    margin-top: 0.5rem;
  }
  svg {
    display: block;
    width: 100%;
    margin-top: 0.8rem;
    overflow: visible;
  }
  .edge {
    fill: none;
    stroke: var(--border);
    stroke-width: 0.5;
  }
  .busy .edge {
    opacity: 0.4;
  }
  circle.Open {
    fill: var(--bg-elev);
    stroke: var(--fg-faint);
    stroke-width: 1;
  }
  circle.Refuted {
    fill: var(--err);
  }
  circle.Proved {
    fill: var(--g4);
    stroke: var(--fg);
    stroke-width: 1.5;
  }
  .key {
    margin: 0.3rem 0 0;
    font-size: 0.74rem;
    color: var(--fg-muted);
  }
  .k {
    display: inline-block;
    width: 9px;
    height: 9px;
    margin: 0 0.15rem 0 0.6rem;
    border-radius: 50%;
  }
  .k:first-child {
    margin-left: 0;
  }
  .k.Refuted {
    background: var(--err);
  }
  .k.Open {
    border: 1px solid var(--fg-faint);
  }
  .k.Proved {
    background: var(--g4);
  }
  .stats {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(8.5rem, 1fr));
    gap: 0.6rem;
    margin-top: 0.7rem;
  }
  .stats > div {
    display: flex;
    flex-direction: column;
    gap: 0.1rem;
    font-size: 0.74rem;
    color: var(--fg-muted);
  }
  .stats b {
    font-size: 1.15rem;
    color: var(--fg);
  }
  .stats div > code {
    display: block;
    font-size: 0.74rem;
    white-space: normal;
    overflow-wrap: anywhere;
  }
</style>
