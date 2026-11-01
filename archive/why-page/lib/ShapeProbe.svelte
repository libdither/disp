<script lang="ts">
  // Scene 4: pick a program, see its real tree, and ask a program what shape it is.
  import { onMount } from 'svelte'
  import type { RawTree } from '$lib/disp/protocol'
  import TreeDraw from './TreeDraw.svelte'
  import { ask, nodesOf, strOf } from './live'

  const SUBJECTS = [
    { id: 'p_leaf', label: 't', code: 't' },
    { id: 'p_stem', label: 't t', code: 't t' },
    { id: 'p_fork', label: 't t t', code: 't t t' },
    { id: 'p_three', label: 'the number 3', code: '3' },
    { id: 'p_not', label: 'not', code: 'not' },
    { id: 'p_twice', label: '{x} => pair x x', code: '{x} => pair x x' }
  ]
  let trees = $state<Record<string, RawTree | null>>({})
  let shapes = $state<Record<string, string | null>>({})
  let picked = $state('p_not')
  let error = $state<string | null>(null)

  onMount(async () => {
    const defs =
      'let shape_of := t (t "leaf" ({u} => "stem")) ({u, v} => "fork")\n' +
      SUBJECTS.map((s) => `${s.id} := ${s.code}\n${s.id}_shape := shape_of ${s.id}\n`).join('')
    const names = SUBJECTS.flatMap((s) => [s.id, `${s.id}_shape`])
    try {
      const got = await ask(defs, names)
      trees = got
      shapes = Object.fromEntries(SUBJECTS.map((s) => [s.id, got[`${s.id}_shape`] == null ? null : strOf(got[`${s.id}_shape`]!)]))
    } catch (e) {
      error = e instanceof Error ? e.message : String(e)
    }
  })
  const tree = $derived(trees[picked])
</script>

<div class="why-widget">
  <div class="why-row" role="group" aria-label="Programs">
    {#each SUBJECTS as s (s.id)}
      <button class="why-chip" aria-pressed={picked === s.id} onclick={() => (picked = s.id)}><code>{s.label}</code></button>
    {/each}
  </div>
  {#if tree != null}
    <div class="out">
      <TreeDraw {tree} height={170} label="the compiled program" />
      <p class="why-note">
        This program is a tree of {nodesOf(tree)} node{nodesOf(tree) === 1 ? '' : 's'}.
        <code>shape_of</code> says: <strong>{shapes[picked]}</strong>.
      </p>
    </div>
  {:else if error}
    <p class="why-note">Could not run: {error}</p>
  {:else}
    <p class="why-note">Compiling…</p>
  {/if}
</div>

<style>
  .out {
    margin-top: 0.8rem;
  }
</style>
