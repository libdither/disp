<script lang="ts">
  import { base } from '$app/paths'

  const REPO = 'https://github.com/libdither/disp/tree/main'
  const repoPage = (path: string) => `${base}/repo/${path}`
  const shot = (name: string) => `${base}/evaluators/${name}.png`

  interface Card {
    name: string
    kicker: string
    blurb: string
    href?: string
    image?: string
    alt?: string
    reload?: boolean
  }

  const WATCH: Card[] = [
    {
      name: 'Strand lattice',
      kicker: 'Physical wires, no addresses',
      blurb: 'Runs the net on a chip-like grid where wires take up room. Finishes all 160 test terms, and beats the address-based mesh by up to 4.2×.',
      image: shot('strands-3d'),
      alt: 'The fib 2 net drawn in 3D across the lattice layers',
      href: repoPage('evaluators/rust-ic-strands/player/index.html'),
      reload: true
    },
    {
      name: 'Running backward',
      kicker: 'Undoing a net',
      blurb: 'The same net rules, with every rewrite an event you can take back, until each agent is where it started.',
      image: shot('strands-backward'),
      alt: 'The net as a tree of agents, with a history bar under it',
      href: repoPage('evaluators/rust-ic-strands/player/rev.html'),
      reload: true
    },
    {
      name: 'Reversible blocks',
      kicker: 'Erasing without deleting',
      blurb: 'A 1D toy where every update is a permutation, so erasing turns bits into heat and a 512-tick run comes back exactly.',
      image: shot('reversible-ca'),
      alt: 'A strip of cells with heat bits, beside a ledger of ticks and events',
      href: repoPage('research/interaction-combinator/reversible-ca.html'),
      reload: true
    },
    {
      name: 'Linear tree calculus',
      kicker: 'One rule drops, one copies',
      blurb: 'Tree calculus in nine rules, where only K drops and only W copies. Binders come off one step at a time, then the tree runs.',
      image: shot('linear'),
      alt: 'A tree with combinator bands and a reel of coloured steps under it',
      href: `${base}/evaluators/linear/`
    },
    {
      name: 'Reflective nets',
      kicker: 'Programs that read code',
      blurb: 'A λ-calculus as a net where a function body stays frozen until it is called. Click any pair to fire it, follow copies in their own colours, and see each rule as a picture.',
      image: shot('reflective-nets'),
      alt: 'A net of agents hanging from its root with copiers in colour, a history bar, and a rule drawn before and after',
      href: repoPage('research/linear-ic-net/player/index.html'),
      reload: true
    },
    {
      name: 'Two-way programs',
      kicker: 'Every step undoable',
      blurb: 'Programs are trees built from five moves. Flip a run at any point, and firing runs it back.',
      image: shot('two-way'),
      alt: 'A program tree with leaves, a run marker and a transport bar',
      href: `${base}/evaluators/two-way/`
    },
    {
      name: 'Two-way, as a net',
      kicker: 'Only agents meeting',
      blurb: 'The same programs with no counter: a walker moves through the program and leaves it unchanged.',
      image: shot('two-way-inet'),
      alt: 'A net of program agents with a walker at the top, and a legend',
      href: `${base}/evaluators/two-way-inet/`
    },
    {
      name: 'Two-way, compiled',
      kicker: 'No walker',
      blurb: "Each part of the program becomes its own agent, and a move happens where the value meets its gate. Still in progress.",
      image: shot('two-way-compiled'),
      alt: 'Program nodes joined by lines to the runs going through them',
      href: `${base}/evaluators/two-way-compiled/`
    }
  ]

  const BEHIND: Card[] = [
    {
      name: 'eager and naive',
      kicker: 'The reference',
      blurb: 'The plainest correct reducers. eager defines the right answer, and every other evaluator gets compared against it.',
      href: `${REPO}/src/eval`
    },
    {
      name: 'rust-eager',
      kicker: 'The fast checker',
      blurb: 'The same reducer in Rust. Runs the test suite in about 2.5 minutes against 22 on eager, and powers the playground.',
      href: `${REPO}/evaluators/rust-eager`
    },
    {
      name: 'lambada peers',
      kicker: 'Outside check',
      blurb: "11 reducers someone else wrote, used for benchmarks and to compare answers. They don't share our bugs.",
      href: `${REPO}/evaluators/lambada`
    },
    {
      name: 'rust-ic-net',
      kicker: 'For the optimizer',
      blurb: 'A real interaction net in memory. Gives up hash-consing to run in parallel and track which candidate did what.',
      href: `${REPO}/evaluators/rust-ic-net`
    },
    {
      name: 'tree-net',
      kicker: 'Shared rules',
      blurb: 'The net rules, abstract net and independent answer checker that the strand lattice is tested against.',
      href: `${REPO}/evaluators/tree-net`
    },
    {
      name: 'Cascade and mesh',
      kicker: 'Archived',
      blurb: 'The first spatial machines. The cascade jammed on 30 of 160 terms, and the mesh needed an address and a router in every tile.'
    }
  ]
</script>

<svelte:head>
  <title>Evaluators · disp</title>
  <meta name="description" content="The machines that run disp, and what each one was built to find out." />
</svelte:head>

{#snippet card(c: Card)}
  <svelte:element
    this={c.href ? 'a' : 'div'}
    class="ev-card"
    href={c.href}
    data-sveltekit-reload={c.reload ? true : undefined}
  >
    {#if c.image}<img src={c.image} alt={c.alt} loading="lazy" />{/if}
    <div class="text">
      <span class="kicker">{c.kicker}</span>
      <h3>{c.name}</h3>
      <p>{c.blurb}</p>
    </div>
  </svelte:element>
{/snippet}

<div class="container page">
  <h1>Evaluators</h1>
  <p class="lede">Anything that can reduce a tree can run disp, so it's cheap to try very different machines. Each one tests an idea.</p>

  <h2>Watch them run</h2>
  <div class="grid">
    {#each WATCH as c}{@render card(c)}{/each}
  </div>

  <h2>No visuals</h2>
  <div class="grid">
    {#each BEHIND as c}{@render card(c)}{/each}
  </div>
</div>

<style>
  .page { padding-block: 3rem; }
  h2 { margin-top: 2.2rem; }
  .grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(17rem, 1fr)); gap: 1rem; }
  .grid :global(.ev-card) {
    display: flex;
    flex-direction: column;
    overflow: hidden;
    border: 1px solid var(--border);
    border-radius: var(--radius);
    background: var(--bg-elev);
    color: inherit;
    text-decoration: none;
    transition: border-color 0.15s ease, transform 0.15s ease;
  }
  .grid :global(a.ev-card:hover) { border-color: var(--accent); transform: translateY(-2px); }
  img { display: block; width: 100%; aspect-ratio: 16 / 10; object-fit: cover; object-position: top; border-bottom: 1px solid var(--border); }
  .text { padding: 0.8rem 1rem 1rem; }
  .kicker { font-size: 0.75rem; color: var(--fg-faint); }
  h3 { margin: 0.1rem 0 0.35rem; font-size: 1.05rem; }
  p { margin: 0; font-size: 0.9rem; color: var(--fg-muted); }
</style>
