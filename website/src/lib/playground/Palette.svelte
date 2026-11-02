<script lang="ts">
  // One keyboard surface for everything: `>` commands, plain text for files and
  // examples, `@` for the names in this buffer. Also the rename prompt.
  import { tick } from 'svelte'

  export interface PaletteItem {
    id: string
    title: string
    detail?: string
    hint?: string
    run: () => void
  }

  interface Props {
    open: boolean
    query: string
    /// a prompt is free text with no list: Enter submits it
    prompt?: { title: string; submit: (text: string) => void } | null
    search: (q: string) => PaletteItem[]
    onClose: () => void
  }
  let { open, query = $bindable(), prompt = null, search, onClose }: Props = $props()

  let sel = $state(0)
  let inputEl = $state<HTMLInputElement>()
  let listEl = $state<HTMLDivElement>()
  const items = $derived(open && !prompt ? search(query).slice(0, 60) : [])

  $effect(() => {
    if (!open) return
    void tick().then(() => {
      inputEl?.focus()
      if (prompt) inputEl?.select()
    })
  })
  $effect(() => {
    void query
    sel = 0
  })

  function choose(i: number) {
    const it = items[i]
    if (!it) return
    onClose()
    it.run()
  }

  function onKey(e: KeyboardEvent) {
    if (e.key === 'Escape') {
      e.preventDefault()
      onClose()
    } else if (e.key === 'Enter') {
      e.preventDefault()
      if (prompt) {
        const text = query
        const p = prompt
        onClose()
        p.submit(text)
      } else choose(sel)
    } else if (e.key === 'ArrowDown') {
      e.preventDefault()
      sel = Math.min(items.length - 1, sel + 1)
      queueMicrotask(scrollSel)
    } else if (e.key === 'ArrowUp') {
      e.preventDefault()
      sel = Math.max(0, sel - 1)
      queueMicrotask(scrollSel)
    }
  }
  const scrollSel = () => listEl?.querySelector('.on')?.scrollIntoView({ block: 'nearest' })
</script>

{#if open}
  <!-- svelte-ignore a11y_click_events_have_key_events, a11y_no_static_element_interactions -->
  <div class="scrim" onclick={onClose}>
    <!-- svelte-ignore a11y_click_events_have_key_events, a11y_no_noninteractive_element_interactions -->
    <div class="box" role="dialog" aria-label={prompt?.title ?? 'command palette'} tabindex="-1" onclick={(e) => e.stopPropagation()}>
      {#if prompt}<div class="ptitle">{prompt.title}</div>{/if}
      <input
        bind:this={inputEl}
        bind:value={query}
        onkeydown={onKey}
        placeholder={prompt ? '' : 'Commands, examples and files.  @ jumps to a name'}
        spellcheck="false"
        autocomplete="off"
      />
      {#if !prompt}
        <div class="list" bind:this={listEl}>
          {#each items as it, i (it.id)}
            <button class="row" class:on={i === sel} onmouseenter={() => (sel = i)} onclick={() => choose(i)}>
              <span class="t">{it.title}</span>
              {#if it.detail}<span class="d">{it.detail}</span>{/if}
              {#if it.hint}<kbd>{it.hint}</kbd>{/if}
            </button>
          {:else}
            <div class="none">Nothing matches.</div>
          {/each}
        </div>
      {/if}
    </div>
  </div>
{/if}

<style>
  .scrim {
    position: fixed;
    inset: 0;
    z-index: 300;
    background: color-mix(in oklab, var(--bg) 40%, transparent);
  }
  .box {
    width: min(38rem, 94vw);
    margin: 9vh auto 0;
    background: var(--bg-elev);
    border: 1px solid var(--border-strong);
    border-radius: 10px;
    box-shadow: var(--shadow-lift);
    overflow: hidden;
  }
  .ptitle {
    padding: 0.55rem 0.8rem 0;
    font-size: 0.78rem;
    color: var(--fg-muted);
  }
  input {
    width: 100%;
    border: none;
    background: none;
    outline: none;
    padding: 0.7rem 0.85rem;
    font-size: 0.95rem;
    color: var(--fg);
    font-family: var(--font-body);
  }
  .list {
    max-height: 56vh;
    overflow: auto;
    border-top: 1px solid var(--border);
    padding: 0.25rem;
  }
  .row {
    display: flex;
    align-items: baseline;
    gap: 0.7rem;
    width: 100%;
    padding: 0.32rem 0.6rem;
    background: none;
    border: none;
    border-radius: 6px;
    color: var(--fg);
    font: inherit;
    font-size: 0.86rem;
    text-align: left;
    cursor: pointer;
  }
  .row.on {
    background: color-mix(in oklab, var(--accent) 18%, transparent);
  }
  .t {
    flex: none;
    max-width: 70%;
    overflow: hidden;
    white-space: nowrap;
    text-overflow: ellipsis;
  }
  .d {
    flex: 1;
    min-width: 0;
    color: var(--fg-faint);
    font-size: 0.76rem;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }
  kbd {
    margin-left: auto;
    flex: none;
    font-size: 0.7rem;
    color: var(--fg-muted);
    border: 1px solid var(--border);
    border-radius: 4px;
    padding: 0 0.35em;
    background: var(--bg-panel);
  }
  .none {
    padding: 0.6rem;
    color: var(--fg-faint);
    font-size: 0.85rem;
  }
</style>
