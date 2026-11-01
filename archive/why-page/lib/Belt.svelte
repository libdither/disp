<script lang="ts">
  // Scene 5: every tree in order, and a claim asked of each. Each tree's result and the final verdict come
  // from disp; the page only plays them back.
  import { onMount, onDestroy } from 'svelte'
  import { flip } from 'svelte/animate'
  import type { RawTree } from '$lib/disp/protocol'
  import TreeDraw from './TreeDraw.svelte'
  import { ask, listOf, natOf, showTree, verdictOf, type Verdict } from './live'
  import { BELT_DEFS, BELT_LENGTH, BELT_PROGRAMS } from './programs'

  let rows = $state<RawTree[]>([])
  let isNat = $state<boolean[]>([])
  let holds = $state<Record<string, boolean[]>>({})
  let error = $state<string | null>(null)

  let program = $state('sneaky')
  let budget = $state(60)
  let pos = $state(0)
  let playing = $state(false)
  let verdict = $state<Verdict | null>(null)
  let timer: ReturnType<typeof setInterval> | undefined

  onMount(async () => {
    const defs =
      BELT_DEFS +
      `belt := Trees.as_items.bounded(${BELT_LENGTH}).s_take(${BELT_LENGTH}).s_items\nbelt_nat := belt.map(NatR)\n` +
      BELT_PROGRAMS.map((p) => `belt_${p.id} := belt.map(nat_fn (${p.code}))\n`).join('')
    try {
      const got = await ask(defs, ['belt', 'belt_nat', ...BELT_PROGRAMS.map((p) => `belt_${p.id}`)])
      rows = listOf(got.belt)
      isNat = listOf(got.belt_nat).map((x) => x === 0)
      holds = Object.fromEntries(BELT_PROGRAMS.map((p) => [p.id, listOf(got[`belt_${p.id}`]).map((x) => x === 0)]))
    } catch (e) {
      error = e instanceof Error ? e.message : String(e)
    }
  })
  onDestroy(() => clearInterval(timer))

  const results = $derived(holds[program] ?? [])
  const firstFail = $derived(results.indexOf(false))
  const stopAt = $derived(firstFail >= 0 && firstFail < budget ? firstFail + 1 : budget)
  const done = $derived(pos > 0 && pos >= stopAt)

  const stateOf = (i: number) => (results[i] === false ? 'bad' : isNat[i] ? 'good' : 'skip')
  const shown = $derived(
    Array.from({ length: Math.min(pos, 7) }, (_, k) => pos - Math.min(pos, 7) + k).map((i) => ({ i, tree: rows[i] }))
  )

  function reset() {
    clearInterval(timer)
    playing = false
    pos = 0
    verdict = null
  }
  async function play() {
    reset()
    const code = BELT_PROGRAMS.find((p) => p.id === program)!.code
    const here = { program, budget }
    const got = await ask(`${BELT_DEFS}belt_v := by_search nat_fn (${code}) ${budget}\n`, ['belt_v'])
    if (here.program !== program || here.budget !== budget) return
    playing = true
    const still = window.matchMedia('(prefers-reduced-motion: reduce)').matches
    const finish = () => {
      clearInterval(timer)
      playing = false
      verdict = verdictOf(got.belt_v)
    }
    if (still) {
      pos = stopAt
      finish()
      return
    }
    const each = Math.max(14, Math.min(140, 3200 / stopAt))
    timer = setInterval(() => {
      pos += 1
      if (pos >= stopAt) finish()
    }, each)
  }
</script>

<div class="why-widget">
  <p class="claim"><span class="why-label">The claim:</span> feed it any number, and you get a number back.</p>
  <div class="why-row" role="group" aria-label="Program to test">
    <span class="why-label">Program:</span>
    {#each BELT_PROGRAMS as p (p.id)}
      <button class="why-chip" aria-pressed={program === p.id} onclick={() => ((program = p.id), reset())}>{p.label}</button>
    {/each}
  </div>
  <label class="why-row budget">
    <span class="why-label">Budget: {budget} trees</span>
    <input type="range" min="1" max={BELT_LENGTH} bind:value={budget} oninput={reset} />
    <button class="why-chip" onclick={play} disabled={playing || rows.length === 0}>▶ Run the belt</button>
  </label>

  <div class="belt" aria-hidden="true">
    {#each shown as card (card.i)}
      <div class="card {stateOf(card.i)}" animate:flip={{ duration: 90 }}>
        <TreeDraw tree={card.tree} height={46} />
      </div>
    {/each}
  </div>
  <div class="ticks" role="img" aria-label="{pos} of {budget} trees tested">
    {#each rows as _, i (i)}
      <span class={i < pos ? stateOf(i) : i < budget ? 'wait' : 'off'}></span>
    {/each}
  </div>
  <p class="key">
    <span class="k good"></span> a number, passes
    <span class="k skip"></span> not a number, nothing to test
    <span class="k bad"></span> fails
  </p>

  <div class="stamps" aria-live="polite">
    {#if verdict?.kind === 'Refuted'}
      <span class="why-stamp refuted">Refuted</span>
      <span>
        It fails on <strong>{natOf(verdict.at) ?? showTree(verdict.at)}</strong>, tree number {firstFail + 1} on the belt. One failure settles it for good.
      </span>
    {:else if verdict?.kind === 'Open'}
      <span class="why-stamp open">Open</span>
      <span>
        No failure in the first {budget} trees. That is all this means{#if program === 'sneaky'}, and this program is in fact wrong: try a budget above {firstFail}{/if}.
      </span>
    {:else if error}
      <span>Could not run: {error}</span>
    {:else if rows.length === 0}
      <span class="why-label">Compiling…</span>
    {:else if !done}
      <span class="why-label">Three possible stamps: Refuted, Open, and Proved. The belt never ends, so running it can never earn the third.</span>
    {/if}
  </div>
</div>

<style>
  .claim {
    margin: 0 0 0.6rem;
  }
  .budget {
    margin-top: 0.6rem;
  }
  input {
    flex: 1;
    min-width: 8rem;
    accent-color: var(--g2);
  }
  .belt {
    display: flex;
    justify-content: flex-end;
    gap: 0.35rem;
    height: 4.6rem;
    margin-top: 0.9rem;
    padding: 0.35rem;
    overflow: hidden;
    border-radius: 8px;
    background: repeating-linear-gradient(90deg, var(--bg-panel) 0 12px, transparent 12px 24px);
    border-bottom: 3px solid var(--border-strong);
  }
  .card {
    flex: 0 0 3.7rem;
    padding: 0.2rem;
    border: 1.5px solid var(--border);
    border-radius: 7px;
    background: var(--bg-elev);
  }
  .card.good {
    border-color: var(--ok);
  }
  .card.bad {
    border-color: var(--err);
    border-width: 3px;
  }
  .card.skip {
    opacity: 0.55;
  }
  .ticks {
    display: flex;
    gap: 1px;
    margin-top: 0.55rem;
  }
  .ticks span {
    flex: 1;
    height: 10px;
    min-width: 1px;
  }
  .good {
    background: var(--ok);
  }
  .skip {
    background: var(--border-strong);
  }
  .bad {
    background: var(--err);
  }
  .card.good,
  .card.skip,
  .card.bad {
    background: var(--bg-elev);
  }
  .wait {
    background: var(--bg-panel-hover);
  }
  .off {
    background: transparent;
    box-shadow: inset 0 -1px 0 var(--border);
  }
  .key {
    margin: 0.4rem 0 0;
    font-size: 0.74rem;
    color: var(--fg-muted);
  }
  .k {
    display: inline-block;
    width: 9px;
    height: 9px;
    margin: 0 0.15rem 0 0.6rem;
    border-radius: 2px;
  }
  .k:first-child {
    margin-left: 0;
  }
  .stamps {
    display: flex;
    gap: 0.6rem;
    align-items: baseline;
    min-height: 2.6rem;
    margin-top: 0.7rem;
    font-size: 0.9rem;
  }
  .stamps > span:only-child {
    font-size: 0.84rem;
  }
</style>
