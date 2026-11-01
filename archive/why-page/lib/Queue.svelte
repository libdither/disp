<script lang="ts">
  // Scene 2: a sketch, with no numbers on purpose. Code arrives at one rate and is read at a fixed other.
  import { onMount } from 'svelte'

  const CAP = 180
  let rate = $state(3)
  let pile = $state(6)
  let visible = $state(false)
  let host: HTMLElement | undefined = $state()

  onMount(() => {
    const still = window.matchMedia('(prefers-reduced-motion: reduce)').matches
    const seen = new IntersectionObserver(([e]) => (visible = e.isIntersecting))
    if (host) seen.observe(host)
    let carry = 0
    const tick = setInterval(() => {
      if (!visible) return
      carry += rate - 1
      const whole = Math.trunc(carry)
      carry -= whole
      pile = Math.max(0, Math.min(CAP, pile + whole))
    }, 450)
    if (still) {
      pile = 60
      clearInterval(tick)
    }
    return () => {
      clearInterval(tick)
      seen.disconnect()
    }
  })
</script>

<div class="why-widget" bind:this={host}>
  <div class="scene">
    <div class="side">
      <span class="why-label">written</span>
      <span class="flow" style:--speed="{Math.max(0.3, 2.4 / rate)}s"></span>
    </div>
    <div class="pile" aria-label="Code nobody has read yet" role="img">
      {#each { length: pile } as _, i (i)}<span></span>{/each}
    </div>
    <div class="side">
      <span class="why-label">read by a person</span>
      <span class="flow" style:--speed="2.4s"></span>
    </div>
  </div>
  <label class="why-row control">
    <span class="why-label">How fast code gets written</span>
    <input type="range" min="1" max="8" step="1" bind:value={rate} />
  </label>
  <p class="why-note">A sketch, not data. People can only read so fast, so anything written faster than that piles up unread.</p>
</div>

<style>
  .scene {
    display: grid;
    grid-template-columns: 5.5rem minmax(0, 1fr) 5.5rem;
    gap: 0.6rem;
    align-items: stretch;
    min-height: 7.5rem;
  }
  .side {
    display: flex;
    flex-direction: column;
    gap: 0.4rem;
    justify-content: flex-end;
  }
  .flow {
    display: block;
    height: 6px;
    border-radius: 3px;
    background: repeating-linear-gradient(90deg, var(--g2) 0 8px, transparent 8px 18px);
    background-size: 36px 100%;
    animation: slide var(--speed) linear infinite;
  }
  @keyframes slide {
    to {
      background-position: 36px 0;
    }
  }
  @media (prefers-reduced-motion: reduce) {
    .flow {
      animation: none;
    }
  }
  .pile {
    display: flex;
    flex-wrap: wrap-reverse;
    gap: 2px;
    align-content: flex-start;
    padding: 4px;
    border-bottom: 2px solid var(--border-strong);
  }
  .pile span {
    width: 9px;
    height: 9px;
    border-radius: 2px;
    background: color-mix(in oklab, var(--g4) 70%, var(--fg-faint));
  }
  .control {
    margin-top: 0.8rem;
  }
  input {
    flex: 1;
    min-width: 8rem;
    accent-color: var(--g2);
  }
</style>
