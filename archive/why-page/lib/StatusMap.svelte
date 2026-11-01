<script module lang="ts">
  // The page's one map: what disp's loop is made of, and how much of each part exists today.
  export type Part = 'want' | 'propose' | 'check' | 'cost' | 'loop'
  export type Status = 'works' | 'partly' | 'none'

  export const PARTS: { id: Part; name: string; what: string; status: Status; note: string }[] = [
    { id: 'want', name: 'What you want', what: 'a test, asked of every tree', status: 'works', note: 'Claims run today.' },
    { id: 'propose', name: 'Proposer', what: 'suggests programs', status: 'none', note: 'Only a small loop exists.' },
    { id: 'check', name: 'Checker', what: 'says pass or fail', status: 'partly', note: 'Finds failures. Proves simple cases.' },
    { id: 'cost', name: 'Cost meter', what: 'ranks what passes', status: 'partly', note: 'Counts steps. Not tied to real hardware.' },
    { id: 'loop', name: 'Loop back', what: 'aims it at itself', status: 'none', note: 'Waits on the rest.' }
  ]
  const GLYPH: Record<Status, string> = { works: '●', partly: '◐', none: '○' }
  const WORD: Record<Status, string> = { works: 'works today', partly: 'partly works', none: 'not built' }

</script>

<script lang="ts">
  let { active = [] }: { active?: Part[] } = $props()
</script>

<div class="map" aria-label="The parts of disp's loop and their status">
  <ol>
    {#each PARTS as p (p.id)}
      <li class="part {p.status}" class:on={active.includes(p.id)} aria-current={active.includes(p.id) ? 'true' : undefined}>
        <span class="name">{p.name}</span>
        <span class="what">{p.what}</span>
        <span class="status"><span aria-hidden="true">{GLYPH[p.status]}</span> {WORD[p.status]}</span>
        <span class="note">{p.note}</span>
      </li>
    {/each}
  </ol>
</div>

<style>
  ol {
    list-style: none;
    margin: 0;
    padding: 0;
    display: flex;
    flex-direction: column;
    gap: 0.95rem;
    position: relative;
  }
  .part {
    position: relative;
    display: flex;
    flex-direction: column;
    padding: 0.5rem 0.7rem;
    border: 1px solid var(--border);
    border-radius: 10px;
    background: var(--bg-elev);
    opacity: 0.62;
    transition:
      opacity 0.25s ease,
      border-color 0.25s ease,
      transform 0.25s ease;
  }
  .part:not(:last-child)::after {
    content: '↓';
    position: absolute;
    left: 50%;
    bottom: -1.02rem;
    transform: translateX(-50%);
    font-size: 0.72rem;
    line-height: 1;
    color: var(--fg-faint);
  }
  .part.on {
    opacity: 1;
    border-color: var(--g2);
    transform: translateX(-3px);
    box-shadow: var(--shadow-soft);
  }
  .part.none {
    border-style: dashed;
  }
  .name {
    font-size: 0.88rem;
    font-weight: 600;
    color: var(--fg);
  }
  .what {
    font-size: 0.74rem;
    color: var(--fg-muted);
  }
  .status {
    margin-top: 0.15rem;
    font-size: 0.72rem;
    font-weight: 560;
    color: var(--fg-muted);
  }
  .works .status {
    color: var(--ok);
  }
  .note {
    display: none;
    font-size: 0.72rem;
    line-height: 1.4;
    color: var(--fg-muted);
  }
  .on .note {
    display: block;
  }

  /* narrow screens: a strip along the bottom, the active part in the middle of attention */
  @media (max-width: 980px) {
    ol {
      flex-direction: row;
      gap: 0.3rem;
    }
    .part {
      flex: 1 1 0;
      min-width: 0;
      padding: 0.25rem 0.35rem;
    }
    .part:not(:last-child)::after,
    .what,
    .on .note {
      display: none;
    }
    .name {
      font-size: 0.68rem;
      line-height: 1.2;
    }
    .status {
      font-size: 0.62rem;
    }
    .part.on {
      transform: translateY(-2px);
    }
  }
</style>
