<script lang="ts">
  // The left rail: this file's outline (values, verdicts, costs), the library's files, the examples.
  import type { Example } from '$lib/disp/examples'

  export interface OutlineRow {
    line: number
    endLine: number
    kind: 'def' | 'let' | 'test'
    name: string
    value?: string
    pass?: boolean
    cost: number
    costFrac: number
    params?: string[]
  }
  type DirNode = { name: string; path: string; dirs: DirNode[]; files: { path: string; name: string }[] }

  interface Props {
    tab: 'outline' | 'files' | 'examples'
    rows: OutlineRow[]
    activeLine: number | null
    filePaths: string[] | null
    openPaths: Set<string>
    userFiles: Record<string, string>
    activePath: string
    examples: Example[]
    exampleId: string
    onTab: (t: 'outline' | 'files' | 'examples') => void
    onGoto: (line: number) => void
    onOpenFile: (path: string, permanent?: boolean) => void
    onNewFile: (name: string) => void
    onDeleteFile: (path: string) => void
    onExample: (id: string) => void
  }
  let { tab, rows, activeLine, filePaths, openPaths, userFiles, activePath, examples, exampleId, onTab, onGoto, onOpenFile, onNewFile, onDeleteFile, onExample }: Props = $props()

  let filter = $state('')
  let onlyFailing = $state(false)
  const failing = $derived(rows.filter((r) => r.pass === false).length)
  const shown = $derived.by(() => {
    const q = filter.trim().toLowerCase()
    return rows.filter((r) => (!onlyFailing || r.pass === false) && (!q || r.name.toLowerCase().includes(q)))
  })
  const fmt = (n: number) => (n >= 1e6 ? (n / 1e6).toFixed(1) + 'M' : n >= 1e3 ? (n / 1e3).toFixed(1) + 'k' : String(n))

  const tree = $derived.by(() => {
    const root: DirNode = { name: 'lib', path: 'lib', dirs: [], files: [] }
    const byPath = new Map<string, DirNode>([['lib', root]])
    for (const p of filePaths ?? []) {
      const segs = p.replace(/^\/lib\//, '').split('/')
      let node = root
      for (let i = 0; i < segs.length - 1; i++) {
        const dpath = `${node.path}/${segs[i]}`
        let child = byPath.get(dpath)
        if (!child) {
          child = { name: segs[i], path: dpath, dirs: [], files: [] }
          byPath.set(dpath, child)
          node.dirs.push(child)
        }
        node = child
      }
      node.files.push({ path: p, name: segs[segs.length - 1] })
    }
    return root
  })
  let collapsed = $state(new Set(['lib/tests']))
  function toggleDir(path: string) {
    const next = new Set(collapsed)
    if (next.has(path)) next.delete(path)
    else next.add(path)
    collapsed = next
  }
  let newOpen = $state(false)
  let newName = $state('')
</script>

{#snippet dirNode(node: DirNode, depth: number)}
  {#each node.dirs as d (d.path)}
    <button class="dir" style="padding-left:{0.4 + depth * 0.7}rem" onclick={() => toggleDir(d.path)} aria-expanded={!collapsed.has(d.path)}>
      <span class="chev" class:shut={collapsed.has(d.path)}>▾</span>{d.name}
    </button>
    {#if !collapsed.has(d.path)}{@render dirNode(d, depth + 1)}{/if}
  {/each}
  {#each node.files as f (f.path)}
    <div class="file" class:open={openPaths.has(f.path)} class:cur={f.path === activePath} style="padding-left:{0.95 + depth * 0.7}rem">
      <button onclick={() => onOpenFile(f.path)} ondblclick={() => onOpenFile(f.path, true)} title={f.path}>{f.name}</button>
      {#if f.path in userFiles}
        <button class="del" onclick={() => onDeleteFile(f.path)} title="Delete your file" aria-label="delete {f.name}">×</button>
      {/if}
    </div>
  {/each}
{/snippet}

<aside class="side">
  <div class="tabs" role="tablist">
    <button role="tab" class:on={tab === 'outline'} aria-selected={tab === 'outline'} onclick={() => onTab('outline')}>Outline</button>
    <button role="tab" class:on={tab === 'files'} aria-selected={tab === 'files'} onclick={() => onTab('files')}>Files</button>
    <button role="tab" class:on={tab === 'examples'} aria-selected={tab === 'examples'} onclick={() => onTab('examples')}>Examples</button>
  </div>

  {#if tab === 'outline'}
    <div class="filter">
      <input bind:value={filter} placeholder="Filter names" spellcheck="false" autocomplete="off" aria-label="filter names" />
      {#if failing > 0}
        <button class="failchip" class:on={onlyFailing} onclick={() => (onlyFailing = !onlyFailing)} title="Show only failing tests">✗ {failing}</button>
      {/if}
    </div>
    <div class="scroll">
      {#each shown as r (`${r.kind}${r.line}`)}
        <button class="row" class:cur={activeLine != null && activeLine >= r.line && activeLine <= r.endLine} onclick={() => onGoto(r.line)} title="line {r.line}{r.cost ? ` · ${fmt(r.cost)} steps` : ''}">
          {#if r.kind === 'test'}
            <span class="v {r.pass === false ? 'bad' : r.pass ? 'good' : ''}">{r.pass === false ? '✗' : r.pass ? '✓' : '·'}</span>
          {:else}
            <span class="dot" class:priv={r.kind === 'let'}></span>
          {/if}
          <span class="nm" class:isTest={r.kind === 'test'}>{r.name}</span>
          {#if r.value}<span class="val">{r.value}</span>{/if}
          {#if r.cost > 0}<span class="cost" style="width:{Math.max(3, r.costFrac * 26)}px"></span>{/if}
        </button>
      {:else}
        <p class="none">{rows.length ? 'Nothing matches.' : 'No definitions yet. Type name := value.'}</p>
      {/each}
    </div>
  {:else if tab === 'files'}
    <div class="filter">
      {#if newOpen}
        <form
          style="flex:1"
          onsubmit={(e) => {
            e.preventDefault()
            onNewFile(newName)
            newName = ''
            newOpen = false
          }}
        >
          <!-- svelte-ignore a11y_autofocus -->
          <input autofocus bind:value={newName} placeholder="name.disp" spellcheck="false" autocomplete="off" onkeydown={(e) => e.key === 'Escape' && (newOpen = false)} />
        </form>
      {:else}
        <button class="newf" onclick={() => (newOpen = true)} title="New file in lib/tests. Reach it with use &quot;./name.disp&quot;">+ New file</button>
      {/if}
    </div>
    <div class="scroll">
      {#if filePaths == null}
        <p class="none">Listing…</p>
      {:else}
        {@render dirNode(tree, 0)}
      {/if}
    </div>
  {:else}
    <div class="scroll">
      {#each examples as ex (ex.id)}
        <button class="ex" class:cur={ex.id === exampleId} onclick={() => onExample(ex.id)}>{ex.label}</button>
      {/each}
    </div>
  {/if}
</aside>

<style>
  .side {
    height: 100%;
    display: flex;
    flex-direction: column;
    min-height: 0;
    background: var(--bg-elev);
    font-size: 0.8rem;
  }
  .tabs {
    display: flex;
    gap: 0.1rem;
    padding: 0.3rem 0.35rem 0;
    border-bottom: 1px solid var(--border);
    flex: none;
  }
  .tabs button {
    flex: 1;
    background: none;
    border: none;
    border-bottom: 2px solid transparent;
    padding: 0.25rem 0.1rem;
    font: inherit;
    font-size: 0.75rem;
    color: var(--fg-faint);
    cursor: pointer;
  }
  .tabs button.on {
    color: var(--fg);
    border-bottom-color: var(--accent);
  }
  .filter {
    flex: none;
    display: flex;
    gap: 0.3rem;
    padding: 0.35rem 0.4rem;
  }
  .filter input {
    flex: 1;
    width: 100%;
    min-width: 0;
    background: var(--bg-panel);
    border: 1px solid var(--border);
    border-radius: 6px;
    padding: 0.18rem 0.45rem;
    font-size: 0.76rem;
    color: var(--fg);
    outline: none;
  }
  .filter input:focus {
    border-color: var(--accent);
  }
  .failchip {
    flex: none;
    background: none;
    border: 1px solid color-mix(in oklab, var(--err) 45%, transparent);
    border-radius: 6px;
    color: var(--err);
    font-size: 0.72rem;
    padding: 0 0.4rem;
    cursor: pointer;
  }
  .failchip.on {
    background: color-mix(in oklab, var(--err) 18%, transparent);
  }
  .newf {
    width: 100%;
    background: none;
    border: 1px dashed var(--border-strong);
    border-radius: 6px;
    padding: 0.18rem;
    font-size: 0.75rem;
    color: var(--fg-muted);
    cursor: pointer;
  }
  .scroll {
    flex: 1;
    min-height: 0;
    overflow: auto;
    padding: 0 0.25rem 0.5rem;
    scrollbar-width: thin;
  }
  .row {
    display: flex;
    align-items: center;
    gap: 0.35rem;
    width: 100%;
    padding: 0.1rem 0.35rem;
    background: none;
    border: none;
    border-radius: 5px;
    text-align: left;
    font-family: var(--font-mono);
    font-size: 0.73rem;
    color: var(--fg);
    cursor: pointer;
    white-space: nowrap;
  }
  .row:hover {
    background: var(--bg-panel-hover);
  }
  .row.cur {
    background: color-mix(in oklab, var(--accent) 16%, transparent);
  }
  .dot {
    flex: none;
    width: 0.42rem;
    height: 0.42rem;
    margin: 0 0.12rem;
    border-radius: 50%;
    background: var(--hl-fn);
  }
  .dot.priv {
    background: none;
    border: 1.2px solid var(--fg-faint);
  }
  .v {
    flex: none;
    width: 0.7rem;
    text-align: center;
    color: var(--fg-faint);
  }
  .v.good {
    color: var(--ok);
  }
  .v.bad {
    color: var(--err);
  }
  .nm {
    flex: 0 1 auto;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .nm.isTest {
    color: var(--fg-muted);
  }
  .val {
    flex: 1 1 0;
    min-width: 0;
    overflow: hidden;
    text-overflow: ellipsis;
    color: var(--fg-faint);
    font-size: 0.68rem;
  }
  .cost {
    flex: none;
    height: 3px;
    margin-left: auto;
    border-radius: 2px;
    background: var(--warn);
    opacity: 0.8;
  }
  .none {
    margin: 0;
    padding: 0.5rem;
    color: var(--fg-faint);
    font-size: 0.76rem;
  }
  .dir {
    display: block;
    width: 100%;
    background: none;
    border: none;
    text-align: left;
    color: var(--fg-muted);
    font-size: 0.74rem;
    font-weight: 600;
    padding-top: 0.12rem;
    padding-bottom: 0.1rem;
    cursor: pointer;
    border-radius: 5px;
  }
  .dir:hover {
    background: var(--bg-panel-hover);
  }
  .chev {
    display: inline-block;
    width: 0.8em;
    font-size: 0.62rem;
    color: var(--fg-faint);
    transition: transform 0.12s ease;
  }
  .chev.shut {
    transform: rotate(-90deg);
  }
  .file {
    display: flex;
    align-items: center;
    border-radius: 5px;
  }
  .file:hover {
    background: var(--bg-panel-hover);
  }
  .file > button:first-child {
    flex: 1;
    min-width: 0;
    background: none;
    border: none;
    padding: 0.1rem 0.2rem;
    text-align: left;
    font-family: var(--font-mono);
    font-size: 0.73rem;
    color: var(--fg-muted);
    cursor: pointer;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  .file.open > button:first-child {
    color: var(--fg);
  }
  .file.cur {
    background: color-mix(in oklab, var(--accent) 16%, transparent);
  }
  .del {
    flex: none;
    background: none;
    border: none;
    color: var(--fg-faint);
    cursor: pointer;
  }
  .del:hover {
    color: var(--err);
  }
  .ex {
    display: block;
    width: 100%;
    margin-top: 0.1rem;
    padding: 0.3rem 0.45rem;
    background: none;
    border: none;
    border-radius: 6px;
    text-align: left;
    font: inherit;
    font-size: 0.78rem;
    color: var(--fg-muted);
    cursor: pointer;
  }
  .ex:first-child {
    margin-top: 0.35rem;
  }
  .ex:hover {
    background: var(--bg-panel-hover);
    color: var(--fg);
  }
  .ex.cur {
    background: color-mix(in oklab, var(--accent) 16%, transparent);
    color: var(--fg);
  }
</style>
