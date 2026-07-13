<script lang="ts">
  // Interactive rendering of a ValueNode: the shared value display for every
  // playground output (def blocks, failing-test got/want, the eval strip).
  // Recognized atoms (nats, strings, bound names) and budget cuts (…) are
  // clickable — a click re-renders that subtree via `expand` (raw for atoms,
  // deeper for cuts) and splices it in. Every unfolded subterm wears its own
  // − chip that re-folds exactly that subterm (nested unfolds fold away with
  // their ancestor). Without `expand` the tree renders inert. `onVisualize`
  // adds a trailing button that hands the CURRENT fold-state tree to the
  // host (the playground's reduction-visualizer pop-out).
  import type { ValueNode } from '$lib/disp/protocol'

  interface Props {
    node: ValueNode
    expand?: (handle: number, rawRoot: boolean) => Promise<ValueNode | null>
    // an unfold grew the content — the host may want to un-collapse
    onGrew?: () => void
    // pop the current fold-state tree out (folded atoms stay symbolic there)
    onVisualize?: (tree: ValueNode) => void
  }
  let { node, expand, onGrew, onVisualize }: Props = $props()

  // an unfolded splice remembers what it replaced, so it can re-fold in place
  type UN = ValueNode & { _prev?: ValueNode }
  const prevOf = (n: ValueNode): ValueNode | undefined => (n as UN)._prev

  // local mutable copy: unfolds splice into it. The prop never changes for a
  // mounted instance (hosts recreate on new results), so capturing its
  // initial value is the intent.
  // svelte-ignore state_referenced_locally
  let tree = $state<ValueNode>(JSON.parse(JSON.stringify(node)))
  let busyPath = $state<string | null>(null)

  function getAt(path: number[]): ValueNode {
    let n = tree
    for (const i of path) n = (n as Extract<ValueNode, { k: 'stem' | 'fork' }>).c[i]
    return n
  }
  function setAt(path: number[], fresh: ValueNode) {
    if (path.length === 0) {
      tree = fresh
      return
    }
    const parent = getAt(path.slice(0, -1)) as Extract<ValueNode, { k: 'stem' | 'fork' }>
    parent.c[path[path.length - 1]] = fresh
  }

  async function unfold(e: Event, path: number[], handle: number, rawRoot: boolean) {
    e.stopPropagation()
    if (!expand || busyPath) return
    busyPath = path.join('.')
    try {
      const fresh = await expand(handle, rawRoot)
      if (fresh) {
        const prev = $state.snapshot(getAt(path)) as ValueNode
        setAt(path, { ...fresh, _prev: prev } as UN)
        onGrew?.()
      }
    } finally {
      busyPath = null
    }
  }

  function refold(e: Event, path: number[]) {
    e.stopPropagation()
    const prev = prevOf(getAt(path))
    if (prev) setAt(path, prev)
  }

  function visualize(e: Event) {
    e.stopPropagation()
    onVisualize?.($state.snapshot(tree) as ValueNode)
  }

  // the widget host toggles collapse on mousedown — interactive spans opt out
  const eat = (e: Event) => e.stopPropagation()
</script>

{#snippet render(n: ValueNode, path: number[], atom: boolean)}
  {#if prevOf(n)}<button
      type="button"
      class="tv-fold"
      onmousedown={eat}
      onclick={(e) => refold(e, path)}
      title="re-fold this subterm">↺</button>{/if}
  {#if n.k === 'leaf'}<span class="tv-leaf">t</span>
  {:else if n.k === 'nat'}<button
      type="button"
      class="tv-atom tv-nat"
      class:busy={busyPath === path.join('.')}
      onmousedown={eat}
      onclick={(e) => unfold(e, path, n.h, true)}
      title="unfold the numeral's tree">{n.n}</button>
  {:else if n.k === 'str'}<button
      type="button"
      class="tv-atom tv-str"
      class:busy={busyPath === path.join('.')}
      onmousedown={eat}
      onclick={(e) => unfold(e, path, n.h, true)}
      title="unfold the string's tree">{JSON.stringify(n.s)}</button>
  {:else if n.k === 'name'}<button
      type="button"
      class="tv-atom tv-name"
      class:busy={busyPath === path.join('.')}
      onmousedown={eat}
      onclick={(e) => unfold(e, path, n.h, true)}
      title="unfold what '{n.name}' names">{n.name}</button>
  {:else if n.k === 'more'}<button
      type="button"
      class="tv-more"
      class:busy={busyPath === path.join('.')}
      onmousedown={eat}
      onclick={(e) => unfold(e, path, n.h, false)}
      title="render deeper">…</button>
  {:else if n.k === 'stem'}{#if atom}({/if}<span class="tv-leaf">t</span>{' '}{@render render(n.c[0], [...path, 0], true)}{#if atom}){/if}
  {:else}{#if atom}({/if}<span class="tv-leaf">t</span>{' '}{@render render(n.c[0], [...path, 0], true)}{' '}{@render render(n.c[1], [...path, 1], true)}{#if atom}){/if}
  {/if}
{/snippet}

<span class="tv">
  {@render render(tree, [], false)}{#if onVisualize}{' '}<button
      type="button"
      class="tv-viz"
      onmousedown={eat}
      onclick={visualize}
      title="visualize reduction (folded names stay symbolic)"
      aria-label="visualize reduction"><svg viewBox="0 0 16 16" aria-hidden="true"><path d="M8 4.5 4.8 10.5M8 4.5l3.2 6" stroke="currentColor" stroke-width="1.4" stroke-linecap="round" fill="none" /><circle cx="8" cy="3.4" r="1.6" fill="currentColor" /><circle cx="4.8" cy="11.6" r="1.6" fill="currentColor" /><circle cx="11.2" cy="11.6" r="1.6" fill="currentColor" /></svg></button>{/if}
</span>

<style>
  .tv {
    font-family: inherit;
    font-size: inherit;
    line-height: inherit;
  }
  .tv-leaf {
    opacity: 0.85;
  }
  .tv-atom,
  .tv-more,
  .tv-fold,
  .tv-viz {
    background: none;
    border: none;
    padding: 0;
    margin: 0;
    font: inherit;
    cursor: pointer;
    border-radius: 4px;
  }
  .tv-atom {
    border-bottom: 1px dotted transparent;
  }
  .tv-atom:hover {
    border-bottom-color: currentColor;
  }
  .tv-nat {
    color: #1d7f8a;
  }
  .tv-str {
    color: #2b8a52;
  }
  .tv-name {
    color: #a8770f;
  }
  .tv-more {
    padding: 0 0.45em;
    background: rgba(74, 104, 82, 0.14);
    color: #4a6852;
    font-weight: 600;
  }
  .tv-more:hover {
    background: rgba(74, 104, 82, 0.26);
  }
  .tv-fold {
    padding: 0 0.35em;
    margin-right: 0.15em;
    background: rgba(168, 119, 15, 0.14);
    color: #a8770f;
    font-weight: 600;
    font-size: 0.95em;
  }
  .tv-fold:hover {
    background: rgba(168, 119, 15, 0.28);
  }
  .tv-viz {
    padding: 0 0.25em;
    color: #8ea08b;
    vertical-align: -0.15em;
  }
  .tv-viz:hover {
    color: #2f9e6e;
  }
  .tv-viz svg {
    width: 1.05em;
    height: 1.05em;
  }
  .busy {
    opacity: 0.45;
    pointer-events: none;
  }
</style>
