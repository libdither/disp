<script lang="ts">
  // Scene 10: the optimizer takes the candidate's seat, and the checker stays outside the loop.
  let folded = $state(false)
</script>

<div class="why-widget">
  <div class="stage" class:folded>
    <div class="trusted">
      <span class="why-label">Outside the loop, small enough to read by hand</span>
      <strong>The rules of the checker</strong>
    </div>
    <div class="loop">
      <div class="box want">{folded ? 'Want: a better optimizer' : 'Want: a program that passes the test'}</div>
      <div class="arrow">↓</div>
      <div class="box opt">The optimizer searches</div>
      <div class="arrow">↓</div>
      <div class="box cand" class:hot={folded}>{folded ? 'Candidate: a new optimizer' : 'Candidate: a program'}</div>
      <div class="arrow">↓</div>
      <div class="box gate">Checker and cost meter</div>
      {#if folded}
        <div class="back">↺ a winner replaces the optimizer, and the loop runs again</div>
      {/if}
    </div>
  </div>
  <div class="why-row">
    <button class="why-chip" aria-pressed={folded} onclick={() => (folded = !folded)}>{folded ? 'Aim it at programs' : 'Aim it at itself'}</button>
  </div>
  <p class="why-note">
    This needs nothing new in principle, because the optimizer is a tree like any other program. None of this loop runs yet.
  </p>
</div>

<style>
  .stage {
    display: grid;
    grid-template-columns: minmax(0, 1fr) minmax(0, 2fr);
    gap: 1rem;
    align-items: start;
    margin-bottom: 0.8rem;
  }
  .trusted {
    display: flex;
    flex-direction: column;
    gap: 0.3rem;
    padding: 0.7rem;
    font-size: 0.85rem;
    border: 2px solid var(--g2);
    border-radius: 10px;
  }
  .loop {
    display: flex;
    flex-direction: column;
    align-items: stretch;
    text-align: center;
  }
  .box {
    padding: 0.4rem 0.6rem;
    font-size: 0.85rem;
    border: 1px solid var(--border-strong);
    border-radius: 8px;
    background: var(--bg-panel);
    transition: background 0.4s ease;
  }
  .box.hot {
    background: color-mix(in oklab, var(--g4) 25%, transparent);
    border-color: var(--g4);
  }
  .arrow {
    font-size: 0.75rem;
    line-height: 1.2;
    color: var(--fg-faint);
  }
  .back {
    margin-top: 0.5rem;
    font-size: 0.8rem;
    color: var(--fg-muted);
  }
  @media (max-width: 620px) {
    .stage {
      grid-template-columns: minmax(0, 1fr);
    }
  }
</style>
