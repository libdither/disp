<script lang="ts">
  // Scene 9: one sum, three meters. Small multiples, each on its own scale, because the units differ.
  import { base } from '$app/paths'
  import { PLAYER_SRC, SUM_RUNS } from './programs'

  const METERS = [
    { key: 'steps', name: 'Step counter', what: 'Rewrites on an evaluator that remembers every answer it has worked out and never copies.' },
    { key: 'rewrites', name: 'Interaction net', what: 'Two parts meet and get replaced, and nothing else happens. Sharing has to be paid for.' },
    { key: 'clocks', name: 'Strand lattice', what: 'A grid where wires take up room and nothing has an address. Rewrites far apart can happen at once.' }
  ] as const

  const VIEWS = [
    { id: 'graph', name: 'as a net' },
    { id: 'oblique', name: 'on the lattice' }
  ]
  let view = $state<string | null>(null)
  const playerUrl = $derived(`${base}/repo/evaluators/rust-ic-strands/player/index.html#src=${encodeURIComponent(PLAYER_SRC)}&speed=100&view=${view}`)
</script>

<div class="why-widget">
  <p class="what">
    Two ways to sum four numbers (60, 50, 3 and 63) held as bit trees.
    <strong>One by one</strong> adds them in turn. <strong>Save the carries</strong> holds the carries back and does them
    together at the end.
  </p>
  <div class="meters">
    {#each METERS as m (m.key)}
      {@const a = SUM_RUNS.added[m.key]}
      {@const b = SUM_RUNS.saved[m.key]}
      {@const top = Math.max(a, b)}
      <div class="meter">
        <h4>{m.name}</h4>
        <p class="meter-what">{m.what}</p>
        <div class="line">
          <span>One by one</span>
          <span class="why-bar" class:win={a < b} style:width="{(a / top) * 100}%"></span>
          <b>{a.toLocaleString()}{a < b ? ' ✓' : ''}</b>
        </div>
        <div class="line">
          <span>Save the carries</span>
          <span class="why-bar" class:win={b < a} style:width="{(b / top) * 100}%"></span>
          <b>{b.toLocaleString()}{b < a ? ' ✓' : ''}</b>
        </div>
      </div>
    {/each}
  </div>
  <p class="why-note">
    Same answer, same inputs (one recorded run per program; ✓ marks the lower). Counting work says one by one wins by
    about a third. Counting time on the lattice says saving the carries wins by about 15%, because its work overlaps.
    The winner depends on the meter, so the meter has to be right. Whether any of these tracks real hardware is an open
    question.
  </p>
  <div class="why-row watch">
    <span class="why-label">Watch a smaller sum run side by side (3 + 1 + 3, too few numbers for saving carries to pay off):</span>
    {#each VIEWS as v (v.id)}
      <button class="why-chip" aria-pressed={view === v.id} onclick={() => (view = v.id)}>{v.name}</button>
    {/each}
    {#if view}<a class="why-chip" href={playerUrl} target="_blank" rel="noopener">open full size</a>{/if}
  </div>
  {#if view}
    {#key view}
      <iframe src={playerUrl} title="The strand lattice player" loading="lazy"></iframe>
    {/key}
  {/if}
</div>

<style>
  .what {
    margin: 0 0 0.8rem;
    font-size: 0.92rem;
  }
  .meters {
    display: grid;
    grid-template-columns: repeat(3, minmax(0, 1fr));
    gap: 1rem;
  }
  h4 {
    margin: 0 0 0.2rem;
    font-family: var(--font-body);
    font-size: 0.87rem;
    font-weight: 600;
  }
  .meter {
    display: flex;
    flex-direction: column;
  }
  .meter-what {
    flex: 1;
    margin: 0 0 0.5rem;
    font-size: 0.76rem;
    line-height: 1.45;
    color: var(--fg-muted);
  }
  .line {
    display: flex;
    flex-direction: column;
    gap: 0.1rem;
    margin-bottom: 0.4rem;
    font-size: 0.74rem;
    color: var(--fg-muted);
  }
  .line .why-bar {
    display: block;
    background: var(--border-strong);
  }
  .line .why-bar.win {
    background: var(--g2);
  }
  .line b {
    font-family: var(--font-mono);
    font-size: 0.8rem;
    color: var(--fg);
  }
  .watch {
    margin-top: 0.9rem;
  }
  a.why-chip {
    text-decoration: none;
  }
  iframe {
    display: block;
    width: 100%;
    height: 26rem;
    margin-top: 0.7rem;
    border: 1px solid var(--border);
    border-radius: 8px;
    background: var(--bg-code);
  }
  @media (max-width: 720px) {
    .meters {
      grid-template-columns: minmax(0, 1fr);
    }
  }
</style>
