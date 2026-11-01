<script lang="ts">
  // Scene 3: the gate and the dial, on real runs. The verdicts are computed live; the costs are recorded (programs.ts).
  import { ask, verdictOf, showTree, type Verdict } from './live'
  import { CANDIDATES, DOUBLING, GATE_BUDGET, GATE_N } from './programs'

  type Seen = { id: string; verdict: Verdict }
  let seen = $state<Seen[]>([])
  let busy = $state(false)
  let error = $state<string | null>(null)

  const has = (id: string) => seen.some((s) => s.id === id)

  async function drop(id: string) {
    if (busy || has(id)) return
    busy = true
    error = null
    const c = CANDIDATES.find((x) => x.id === id)!
    try {
      const got = await ask(`${DOUBLING}gate_v := by_search doubles (${c.code}) ${GATE_BUDGET}\n`, ['gate_v'])
      seen.push({ id, verdict: verdictOf(got.gate_v) })
    } catch (e) {
      error = e instanceof Error ? e.message : String(e)
    } finally {
      busy = false
    }
  }

  const row = (s: Seen) => ({ ...CANDIDATES.find((c) => c.id === s.id)!, verdict: s.verdict })
  const passed = $derived(seen.filter((s) => s.verdict.kind !== 'Refuted').map(row).sort((a, b) => a.steps - b.steps))
  const stopped = $derived(seen.filter((s) => s.verdict.kind === 'Refuted').map(row))
  const most = $derived(Math.max(1, ...passed.map((p) => p.steps)))
</script>

<div class="why-widget">
  <p class="spec"><span class="why-label">The test:</span> does <code>f n</code> equal <code>n + n</code>?</p>
  <div class="why-row" role="group" aria-label="Programs to drop in">
    <span class="why-label">Drop in:</span>
    {#each CANDIDATES as c (c.id)}
      <button class="why-chip" onclick={() => drop(c.id)} disabled={busy || has(c.id)}>{c.label}</button>
    {/each}
  </div>
  <div class="stages">
    <div class="stage">
      <h4>Stopped at the gate</h4>
      {#each stopped as s (s.id)}
        <p class="line">
          {s.label}
          {#if s.verdict.kind === 'Refuted'}<span class="why-stamp refuted">fails on {showTree(s.verdict.at) === 't' ? '0' : showTree(s.verdict.at)}</span>{/if}
        </p>
      {:else}
        <p class="why-label">Nothing yet.</p>
      {/each}
    </div>
    <div class="stage">
      <h4>Through, ranked by cost <span class="why-label">(steps to double {GATE_N})</span></h4>
      {#each passed as p (p.id)}
        <div class="rank">
          <span class="name">{p.label}</span>
          <span class="barwrap"><span class="why-bar" style:display="block" style:width="{(p.steps / most) * 100}%" title="{p.steps.toLocaleString()} steps"></span></span>
          <span class="num">{p.steps.toLocaleString()}</span>
        </div>
      {:else}
        <p class="why-label">Nothing yet.</p>
      {/each}
    </div>
  </div>
  <p class="why-note">Verdicts run live. Step counts are recorded, each from a fresh run on this page's evaluator.</p>
  {#if error}<p class="why-note">Could not run: {error}</p>{/if}
  {#if has('cheat')}
    <p class="why-note catch">
      <strong>That last one is wrong.</strong> Ask the table for 4 and it says 0. The gate only tried the first
      {GATE_BUDGET} trees, and the numbers among those stop at 3. It also came out cheapest, because it does no
      work. A cost meter rewards exactly this kind of cheating, so a gate that only tries examples is not enough.
    </p>
  {/if}
</div>

<style>
  .spec {
    margin: 0 0 0.6rem;
  }
  .stages {
    display: grid;
    grid-template-columns: minmax(0, 2fr) minmax(0, 3fr);
    gap: 1.2rem;
    margin-top: 0.9rem;
  }
  h4 {
    margin: 0 0 0.45rem;
    font-family: var(--font-body);
    font-size: 0.85rem;
    font-weight: 600;
  }
  .line {
    margin: 0 0 0.3rem;
    font-size: 0.85rem;
    display: flex;
    flex-wrap: wrap;
    gap: 0.4rem;
    align-items: center;
    justify-content: space-between;
  }
  .rank {
    display: grid;
    grid-template-columns: 9.5rem minmax(0, 1fr) 3.4rem;
    gap: 0.5rem;
    align-items: center;
    font-size: 0.82rem;
    margin-bottom: 0.3rem;
  }
  .num {
    font-family: var(--font-mono);
    font-size: 0.76rem;
    text-align: right;
    color: var(--fg-muted);
  }
  .catch {
    padding: 0.6rem 0.75rem;
    border-left: 3px solid var(--warn);
    background: color-mix(in oklab, var(--warn) 8%, transparent);
    color: var(--fg);
  }
  @media (max-width: 620px) {
    .stages {
      grid-template-columns: minmax(0, 1fr);
    }
  }
</style>
