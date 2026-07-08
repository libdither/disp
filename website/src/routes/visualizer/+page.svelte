<script lang="ts">
  import NatureTree from '$lib/components/NatureTree.svelte'
</script>

<svelte:head>
  <title>Visualizer — disp</title>
</svelte:head>

<div class="container page">
  <h1><span class="grad-text">Visualizer</span></h1>
  <p class="lede">
    Reserved for the reduction visualizer: step through real programs as trees and interaction
    nets, watch sharing happen, and profile where the work goes. Not built yet — for now, enjoy
    the five rules on loop.
  </p>
  <div class="teaser">
    <NatureTree height={420} stepMs={1100} />
  </div>
</div>

<style>
  .page { padding-block: 3.5rem 2rem; text-align: center; }
  h1 { font-size: clamp(2.2rem, 5vw, 3.2rem); margin: 0 0 0.8rem; }
  .lede { color: var(--fg-muted); max-width: 40rem; margin: 0 auto 2rem; }
  .teaser {
    max-width: 760px;
    margin: 0 auto;
    border: 1px solid var(--border);
    border-radius: 16px;
    background: var(--bg-panel);
    padding: 1.2rem 0.8rem 0.6rem;
  }
</style>
