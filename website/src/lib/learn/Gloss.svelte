<script lang="ts">
  import type { Snippet } from 'svelte'
  interface Props {
    tip: string
    children: Snippet
  }
  let { tip, children }: Props = $props()
</script>

<dfn class="gloss" title={tip}>
  {@render children()}<span class="tipbox" role="tooltip">{tip}</span>
</dfn>

<style>
  .gloss {
    font-style: inherit;
    position: relative;
    text-decoration: underline dotted var(--accent-2);
    text-decoration-thickness: 1px;
    text-underline-offset: 3px;
    cursor: help;
  }
  .tipbox {
    position: absolute;
    left: 50%;
    bottom: calc(100% + 8px);
    transform: translateX(-50%);
    width: max-content;
    max-width: 24em;
    background: #fdfef9;
    color: var(--fg);
    border: 1px solid var(--border-strong);
    border-left: 3px solid var(--g2);
    border-radius: 8px;
    padding: 0.5em 0.8em;
    font-size: 0.78rem;
    font-family: var(--font-body);
    line-height: 1.45;
    z-index: 50;
    opacity: 0;
    pointer-events: none;
    transition: opacity 0.15s ease;
    box-shadow: var(--shadow-lift);
  }
  .gloss:hover .tipbox,
  .gloss:focus .tipbox {
    opacity: 1;
  }
</style>
