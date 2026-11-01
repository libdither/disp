<script lang="ts">
  // Scene 6: every number lands on one of two cards, then both programs are compared once per card.
  const NUMBERS = [0, 1, 2, 3, 4, 5, 6]
  let folded = $state(false)
  let other = $state<'add2' | 'wrong'>('add2')

  const CASES = $derived(
    other === 'add2'
      ? [
          { card: 'zero', left: '0', right: '0', same: true },
          { card: 'one more than some m', left: 'two more than double m', right: 'two more than double m', same: true }
        ]
      : [
          { card: 'zero', left: '0', right: '0', same: true },
          { card: 'one more than some m', left: 'two more than double m', right: 'one more than double m', same: false }
        ]
  )
</script>

<div class="why-widget">
  <div class="fold" class:folded role="img" aria-label="Every number lands on one of two cards">
    <div class="cards">
      <div class="bigcard"><span>zero</span></div>
      <div class="bigcard"><span>one more than some number <em>m</em></span></div>
    </div>
    <div class="tiles">
      {#each NUMBERS as n}
        <span class="tile" class:first={n === 0} style:--i={n}>{n}</span>
      {/each}
      <span class="more">…</span>
    </div>
  </div>
  <div class="why-row">
    <button class="why-chip" aria-pressed={folded} onclick={() => (folded = !folded)}>{folded ? 'Unfold the belt' : 'Fold the belt onto two cards'}</button>
  </div>
  <div class="why-row pick" role="group" aria-label="Second program">
    <span class="why-label">Compare "add two, once per number" with:</span>
    <button class="why-chip" aria-pressed={other === 'add2'} onclick={() => (other = 'add2')}>call add 2 instead</button>
    <button class="why-chip" aria-pressed={other === 'wrong'} onclick={() => (other = 'wrong')}>a wrong one: add only 1</button>
  </div>
  <table>
    <thead><tr><th>Card</th><th>First program gives</th><th>Second program gives</th><th></th></tr></thead>
    <tbody>
      {#each CASES as c (c.card)}
        <tr>
          <td>{c.card}</td>
          <td>{c.left}</td>
          <td>{c.right}</td>
          <td>{c.same ? '=' : '≠'}</td>
        </tr>
      {/each}
    </tbody>
  </table>
  <p class="why-note">
    {#if other === 'add2'}
      <span class="why-stamp proved">Proved</span> Both cards match, so the two programs agree on every number, with no belt run.
      The second card assumes the two already agree on <em>m</em>, which is safe because <em>m</em> is smaller.
      The table is written out by hand; ▶ below runs the real check.
    {:else}
      <span class="why-stamp refuted">Refuted on 1</span> The cards disagree, and the checker hands back a number you can run to see it: 1, where one says 2 and the other says 1.
      The table is written out by hand; ▶ below runs the real check.
    {/if}
  </p>
</div>

<style>
  .fold {
    position: relative;
    height: 7.2rem;
    margin-bottom: 0.7rem;
  }
  .cards {
    position: absolute;
    inset: 2.6rem 0 0 0;
    display: grid;
    grid-template-columns: 1fr 2fr;
    gap: 0.6rem;
    opacity: 0.35;
    transition: opacity 0.5s ease;
  }
  .folded .cards {
    opacity: 1;
  }
  .bigcard {
    display: flex;
    align-items: flex-end;
    padding: 0.35rem 0.6rem;
    font-size: 0.8rem;
    color: var(--fg-muted);
    border: 1.5px solid var(--border-strong);
    border-radius: 9px;
    background: var(--bg-panel);
  }
  .tiles {
    position: absolute;
    inset: 0;
  }
  .tile {
    position: absolute;
    top: 0;
    left: calc(var(--i) * 2.3rem);
    display: grid;
    place-items: center;
    width: 2rem;
    height: 2rem;
    font-family: var(--font-mono);
    font-size: 0.85rem;
    border: 1px solid var(--border-strong);
    border-radius: 6px;
    background: var(--bg-elev);
    transition:
      left 0.6s cubic-bezier(0.35, 0.1, 0.22, 1),
      top 0.6s cubic-bezier(0.35, 0.1, 0.22, 1);
    transition-delay: calc(var(--i) * 40ms);
  }
  .more {
    position: absolute;
    top: 0.4rem;
    left: 16.3rem;
    color: var(--fg-faint);
    transition: opacity 0.3s ease;
  }
  .folded .more {
    opacity: 0;
  }
  /* every tile drops onto its card: 0 on the first, the rest stacked on the second */
  .folded .tile {
    top: 2.9rem;
    left: calc(33.33% + 0.7rem + (var(--i) - 1) * 0.5rem);
  }
  .folded .tile.first {
    left: 0.6rem;
  }
  .pick {
    margin-top: 0.7rem;
  }
  table {
    width: 100%;
    margin-top: 0.7rem;
    border-collapse: collapse;
    font-size: 0.85rem;
  }
  th {
    text-align: left;
    font-weight: 600;
    font-size: 0.76rem;
    color: var(--fg-muted);
    padding: 0.25rem 0.5rem 0.25rem 0;
    border-bottom: 1px solid var(--border-strong);
  }
  td {
    padding: 0.3rem 0.5rem 0.3rem 0;
    border-bottom: 1px solid var(--border);
  }
</style>
