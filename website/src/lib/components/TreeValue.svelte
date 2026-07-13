<script lang="ts">
  // Interactive rendering of a ValueNode: the shared value display for every
  // playground output (def blocks, failing-test got/want, the eval strip).
  // Recognized atoms (nats, strings, bound names) and budget cuts (…) are
  // clickable — a click re-renders that subtree via `expand` (raw for atoms,
  // deeper for cuts) and splices it in; ↩ pops the unfold history, restoring
  // the folded rendering. Without `expand` the tree renders inert.
  import type { ValueNode } from '$lib/disp/protocol'

  interface Props {
    node: ValueNode
    expand?: (handle: number, rawRoot: boolean) => Promise<ValueNode | null>
    // an unfold grew the content — the host may want to un-collapse
    onGrew?: () => void
  }
  let { node, expand, onGrew }: Props = $props()

  // local mutable copy: unfolds splice into it, history re-folds. The prop
  // never changes for a mounted instance (hosts recreate on new results),
  // so capturing its initial value is the intent.
  // svelte-ignore state_referenced_locally
  let tree = $state<ValueNode>(JSON.parse(JSON.stringify(node)))
  let history = $state<{ path: number[]; prev: ValueNode }[]>([])
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
        history.push({ path, prev: getAt(path) })
        setAt(path, fresh)
        onGrew?.()
      }
    } finally {
      busyPath = null
    }
  }

  function back(e: Event) {
    e.stopPropagation()
    const u = history.pop()
    if (u) setAt(u.path, u.prev)
  }

  // the widget host toggles collapse on mousedown — interactive spans opt out
  const eat = (e: Event) => e.stopPropagation()
</script>

{#snippet render(n: ValueNode, path: number[], atom: boolean)}
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
  {@render render(tree, [], false)}{#if history.length > 0}{' '}<button
      type="button"
      class="tv-back"
      onmousedown={eat}
      onclick={back}
      title="re-fold the last unfold">↩</button>{/if}
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
  .tv-back {
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
  .tv-back {
    padding: 0 0.35em;
    color: #8ea08b;
  }
  .tv-back:hover {
    color: #2f9e6e;
    background: rgba(74, 104, 82, 0.12);
  }
  .busy {
    opacity: 0.45;
    pointer-events: none;
  }
</style>
