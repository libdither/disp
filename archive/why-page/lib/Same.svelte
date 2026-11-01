<script lang="ts">
  // Scene 7: the two doubling programs as real trees, and how far "same" reaches today.
  import { onMount } from 'svelte'
  import type { RawTree } from '$lib/disp/protocol'
  import TreeDraw from './TreeDraw.svelte'
  import { ask, nodesOf } from './live'
  import { DOUBLING_STEPS, SAME_DEFS } from './programs'

  let a = $state<RawTree | null>(null)
  let b = $state<RawTree | null>(null)
  let error = $state<string | null>(null)
  onMount(async () => {
    try {
      const got = await ask(`${SAME_DEFS}same_a := dbl_step\nsame_b := dbl_add\n`, ['same_a', 'same_b'])
      a = got.same_a
      b = got.same_b
    } catch (e) {
      error = e instanceof Error ? e.message : String(e)
    }
  })

  const AT_300 = DOUBLING_STEPS.find((d) => d.n === 300)!
  const LEVELS = [
    {
      name: 'Same text',
      pair: 'A line and a copy of it.',
      who: 'Comparing characters. Instant.',
      gets: 'Nothing. One extra space breaks it.',
      today: true
    },
    {
      name: 'Same tree',
      pair: 'succ (succ x) and pair t (pair t x): written differently, compiled to one tree.',
      who: 'tree_eq. Instant.',
      gets: 'No rewrite at all, because these are one program.',
      today: true
    },
    {
      name: 'Same after one run on a stand-in',
      pair: 'id (5 nodes) and shift (134 nodes), which takes one off a number and adds it back.',
      who: 'Run both once on "some number" and compare. Proved.',
      gets: 'A real rewrite, but only where no loop is involved.',
      today: true
    },
    {
      name: 'Same by induction',
      pair: 'The two doubling programs above.',
      who: 'The two cards from the last scene. Proved, and checked against search.',
      gets: 'A rewrite through recursion. This is as far as the current work has gone.',
      today: true
    },
    {
      name: 'Same through a real optimization',
      pair: 'Map fusion, or swapping a multiply for a shift.',
      who: 'Not attempted in the current work. An older, retired kernel certified map fusion.',
      gets: 'The rewrites that would actually speed up real code.',
      today: false
    },
    {
      name: 'Same on every input, always',
      pair: 'Any two programs at all.',
      who: 'No checker can decide this for every pair. That is a theorem (Rice), not a gap to fill.',
      gets: 'What an optimizer would ideally have. A true claim with no shortcut just stays Open.',
      today: false
    }
  ]
  let level = $state(3)
  const shown = $derived(LEVELS[level])
</script>

<div class="why-widget">
  {#if a !== null && b !== null}
    <div class="trees">
      <figure>
        <TreeDraw tree={a} against={b} height={190} label="doubling by adding two" />
        <figcaption><strong>Add two, once per number</strong><br />{nodesOf(a)} nodes · {AT_300.by_twos.toLocaleString()} steps to double 300</figcaption>
      </figure>
      <figure>
        <TreeDraw tree={b} against={a} height={190} label="doubling by calling add" />
        <figcaption><strong>Call add 2, once per number</strong><br />{nodesOf(b)} nodes · {AT_300.by_add2.toLocaleString()} steps to double 300</figcaption>
      </figure>
    </div>
    <p class="why-note">Gold marks where the two trees differ. Step counts are from one run on the evaluator in this page, not a proof of anything.</p>
  {:else if error}
    <p class="why-note">Could not run: {error}</p>
  {:else}
    <p class="why-note">Compiling…</p>
  {/if}
</div>

<div class="why-widget">
  <label class="why-row">
    <span class="why-label">How much does "same" cover?</span>
    <input type="range" min="0" max={LEVELS.length - 1} bind:value={level} aria-valuetext={shown.name} />
  </label>
  <div class="ruler" aria-hidden="true">
    {#each LEVELS as l, i}
      <button class="notch" class:today={l.today} class:on={i === level} tabindex="-1" onclick={() => (level = i)}><span>{i + 1}</span></button>
    {/each}
  </div>
  <p class="here"><span class="mark" class:reached={shown.today}>{shown.today ? 'Disp reaches this' : 'Disp does not reach this'}</span></p>
  <h4>{shown.name}</h4>
  <dl>
    <dt>For example</dt><dd>{shown.pair}</dd>
    <dt>Who checks it</dt><dd>{shown.who}</dd>
    <dt>What an optimizer gets</dt><dd>{shown.gets}</dd>
  </dl>
</div>

<style>
  .trees {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 1rem;
  }
  figure {
    margin: 0;
  }
  figcaption {
    margin-top: 0.4rem;
    font-size: 0.8rem;
    color: var(--fg-muted);
  }
  input {
    flex: 1;
    min-width: 8rem;
    accent-color: var(--g2);
  }
  .ruler {
    display: flex;
    gap: 3px;
    margin-top: 0.5rem;
  }
  .notch {
    flex: 1;
    height: 1.3rem;
    padding: 0;
    font-size: 0.7rem;
    border: 1px dashed var(--border-strong);
    border-radius: 4px;
    background: none;
    color: var(--fg-muted);
    cursor: pointer;
  }
  .notch.today {
    border-style: solid;
    background: color-mix(in oklab, var(--g1) 20%, transparent);
  }
  .notch.on {
    outline: 2px solid var(--g2);
    outline-offset: 1px;
    color: var(--fg);
  }
  .here {
    margin: 0.6rem 0 0;
    font-size: 0.78rem;
  }
  .mark {
    color: var(--fg-muted);
  }
  .mark.reached {
    color: var(--ok);
  }
  h4 {
    margin: 0.2rem 0 0.4rem;
    font-size: 1.02rem;
  }
  dl {
    display: grid;
    grid-template-columns: 9.5rem minmax(0, 1fr);
    gap: 0.3rem 0.7rem;
    margin: 0;
    font-size: 0.87rem;
  }
  dt {
    color: var(--fg-faint);
    font-size: 0.8rem;
  }
  dd {
    margin: 0;
  }
  @media (max-width: 620px) {
    .trees,
    dl {
      grid-template-columns: minmax(0, 1fr);
    }
  }
</style>
