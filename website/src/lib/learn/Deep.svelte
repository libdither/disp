<script lang="ts">
  import type { Snippet } from 'svelte'
  import { slide } from 'svelte/transition'
  interface Props {
    title: string
    children: Snippet
  }
  let { title, children }: Props = $props()
  let open = $state(false)
</script>

<div class="deep" class:open>
  <button class="summary" onclick={() => (open = !open)} aria-expanded={open}>
    <span class="chev" aria-hidden="true">▸</span>
    {title}
  </button>
  {#if open}
    <div class="body" transition:slide={{ duration: 220 }}>
      {@render children()}
    </div>
  {/if}
</div>

<style>
  .deep {
    border: 1px solid var(--border);
    border-radius: 10px;
    margin: 1.1rem 0;
    background: var(--bg-panel);
    overflow: hidden;
  }
  .summary {
    display: block;
    width: 100%;
    text-align: left;
    background: none;
    border: none;
    color: var(--fg-muted);
    font: inherit;
    font-size: 0.92rem;
    font-weight: 560;
    padding: 0.7rem 1rem;
    cursor: pointer;
  }
  .summary:hover { color: var(--fg); background: var(--bg-panel-hover); }
  .chev {
    display: inline-block;
    transition: transform 0.2s ease;
    color: var(--accent);
    margin-right: 0.35em;
  }
  .open .chev { transform: rotate(90deg); }
  .body {
    padding: 0.2rem 1.1rem 0.9rem;
    font-size: 0.92rem;
    color: var(--fg-muted);
  }
  .body :global(p:first-child) { margin-top: 0.3em; }
  .body :global(pre) { font-size: 0.78rem; }
</style>
