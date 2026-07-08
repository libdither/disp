<script lang="ts">
  import type { Snippet } from 'svelte'
  interface Props {
    kind?: 'note' | 'warn'
    title?: string
    children: Snippet
  }
  let { kind = 'note', title, children }: Props = $props()
</script>

<aside class={kind}>
  {#if title}<span class="title">{title}</span>{/if}
  {@render children()}
</aside>

<style>
  aside {
    border-radius: 10px;
    padding: 0.85rem 1.1rem;
    margin: 1.2rem 0;
    font-size: 0.94rem;
    border: 1px solid var(--border);
    border-left-width: 3px;
  }
  aside.note {
    background: color-mix(in oklab, var(--g3) 6%, transparent);
    border-left-color: var(--g3);
  }
  aside.warn {
    background: color-mix(in oklab, var(--warn) 7%, transparent);
    border-left-color: var(--warn);
  }
  .title {
    font-weight: 650;
    margin-right: 0.4em;
  }
  aside :global(p:first-of-type) { margin-top: 0; }
  aside :global(p:last-child) { margin-bottom: 0; }
</style>
