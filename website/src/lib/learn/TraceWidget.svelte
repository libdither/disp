<script lang="ts">
  // Progressive-disclosure trace: reveal one frame at a time (the original
  // walkthrough's widget 2, re-derived for the two-op kernel by the caller).
  export interface Frame {
    title: string
    html: string
  }
  interface Props {
    title: string
    intro?: string
    frames: Frame[]
  }
  let { title, intro, frames }: Props = $props()
  let shown = $state(1)
  const all = $derived(shown >= frames.length)
</script>

<div class="trace">
  <div class="t-head">
    <span class="t-title">{title}</span>
    <span class="t-counter">{all ? `all (${frames.length})` : `${shown} / ${frames.length}`}</span>
  </div>
  {#if intro}<p class="t-intro">{intro}</p>{/if}
  <ol class="frames">
    {#each frames.slice(0, shown) as f, i}
      <li class="frame" class:current={i === shown - 1}>
        <span class="f-title">{f.title}</span>
        <span class="f-body">{@html f.html}</span>
      </li>
    {/each}
  </ol>
  <div class="t-controls">
    <button class="btn sm" onclick={() => (shown = Math.max(1, shown - 1))} disabled={shown <= 1}>◀ Prev</button>
    <button class="btn sm primary" onclick={() => (shown = Math.min(frames.length, shown + 1))} disabled={all}>Next ▶</button>
    <button class="btn sm" onclick={() => (shown = frames.length)} disabled={all}>Show all</button>
    <button class="btn sm" onclick={() => (shown = 1)}>Reset</button>
  </div>
</div>

<style>
  .trace {
    border: 1px solid var(--border);
    border-radius: var(--radius);
    background: var(--bg-panel);
    padding: 1rem 1.2rem;
    margin: 1.4rem 0;
  }
  .t-head { display: flex; justify-content: space-between; align-items: baseline; gap: 1rem; }
  .t-title {
    font-family: var(--font-display);
    font-weight: 620;
    font-size: 1.05rem;
  }
  .t-counter { color: var(--fg-faint); font-size: 0.8rem; font-family: var(--font-mono); }
  .t-intro { color: var(--fg-muted); font-size: 0.9rem; margin: 0.4rem 0 0.8rem; }
  .frames { list-style: none; margin: 0.8rem 0; padding: 0; }
  .frame {
    padding: 0.55rem 0.8rem;
    border-left: 2px solid var(--border);
    font-size: 0.9rem;
    color: var(--fg-muted);
  }
  .frame.current {
    border-left-color: var(--accent);
    background: color-mix(in oklab, var(--accent) 5%, transparent);
    color: var(--fg);
    border-radius: 0 8px 8px 0;
  }
  .f-title { font-weight: 650; margin-right: 0.45em; }
  .f-body :global(code) { font-size: 0.82em; }
  .t-controls { display: flex; gap: 0.5rem; flex-wrap: wrap; }
  .btn.sm { padding: 0.3em 0.85em; font-size: 0.8rem; }
</style>
