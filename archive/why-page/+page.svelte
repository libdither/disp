<script lang="ts">
  import { onMount } from 'svelte'
  import { base } from '$app/paths'
  import Code from '$lib/learn/Code.svelte'
  import Gloss from '$lib/learn/Gloss.svelte'
  import TreeVis from '$lib/components/TreeVis.svelte'
  import StatusMap, { type Part } from '$lib/why/StatusMap.svelte'
  import Triangle from '$lib/why/Triangle.svelte'
  import Queue from '$lib/why/Queue.svelte'
  import Gate from '$lib/why/Gate.svelte'
  import ShapeProbe from '$lib/why/ShapeProbe.svelte'
  import Belt from '$lib/why/Belt.svelte'
  import Cards from '$lib/why/Cards.svelte'
  import Same from '$lib/why/Same.svelte'
  import Search from '$lib/why/Search.svelte'
  import Cost from '$lib/why/Cost.svelte'
  import Fold from '$lib/why/Fold.svelte'
  import { BELT_CODE, CONFIRMER_CONTEXT, CONTEXT, COVER_CODE, INDUCTION_CODE, SHAPE_OF } from '$lib/why/programs'
  import '$lib/why/why.css'

  const REPO = 'https://github.com/libdither/disp'

  const SCENE_PARTS: Record<string, Part[]> = {
    triangle: [],
    urgent: [],
    gate: ['want', 'propose', 'check', 'cost', 'loop'],
    read: ['check'],
    spec: ['want'],
    shortcut: ['check'],
    same: ['check'],
    search: ['propose'],
    fast: ['cost'],
    itself: ['loop'],
    score: ['want', 'propose', 'check', 'cost', 'loop']
  }
  let scene = $state('triangle')
  const active = $derived(SCENE_PARTS[scene] ?? [])

  onMount(() => {
    const watch = new IntersectionObserver(
      (entries) => {
        for (const e of entries) if (e.isIntersecting) scene = (e.target as HTMLElement).id
      },
      { rootMargin: '-45% 0px -50% 0px' }
    )
    for (const el of document.querySelectorAll('.scene')) watch.observe(el)
    return () => watch.disconnect()
  })

  const SHAPE_LAB = [
    { expr: 't (t a b) c t', tip: 'The input is a leaf, so the first branch comes back' },
    { expr: 't (t a b) c (t u)', tip: 'The input is a stem, so the second branch gets what is inside it' },
    { expr: 't (t a b) c (t u v)', tip: 'The input is a fork, so the third branch gets both halves' },
    { expr: 'not true', tip: 'not is built from one of those rules' }
  ]

  const QUESTIONS = [
    {
      plain: 'Is there a cheap, mechanical kind of "same" that covers the rewrites an optimizer needs?',
      status: 'Partly',
      detail: 'Simple cases and one induction are proved. Real optimizations are not attempted yet.',
      kill: 'The cheap kind of "same" allows almost nothing beyond identical trees.'
    },
    {
      plain: 'Can trying many candidates at once reuse enough work to be affordable?',
      status: 'Open',
      detail: 'The simple form of the idea only holds for code that uses each input once.',
      kill: 'No sound way of copying beats trying candidates one at a time.'
    },
    {
      plain: 'Can it find improvements faster than checking its bad ideas costs?',
      status: 'Open',
      detail: 'Nothing has been built to test this. It is the wall that stopped earlier attempts.',
      kill: 'Disp spends more rejecting bad candidates than the good ones save.'
    },
    {
      plain: 'Do the cheapest programs on the meter run fastest on real machines?',
      status: 'Open',
      detail: 'The lattice runs, and it can reorder programs, but it is not compared with real machines.',
      kill: 'Programs that are cheapest on the meter keep being slow on real machines.'
    },
    {
      plain: 'Can the rules behind Proved be shown sound once, by hand, with all the rest checked by running?',
      status: 'Open',
      detail: 'Five shortcut rules are trusted today. They have not been proved sound.',
      kill: 'Trusting it needs an outside anchor so large that self-checking buys nothing.'
    },
    {
      plain: 'Does all this run at a useful speed, or only describe itself?',
      status: 'Open',
      detail: 'Test runs need large memory budgets, and generality costs about 2× on the older kernel.',
      kill: 'The cost of generality keeps growing with the size of the system.'
    }
  ]
  const PLAIN = [
    ['Type, recognizer', 'a test'],
    ['Witness, challenge', 'an answer, and a try at breaking it'],
    ['Lens', 'a certified shortcut'],
    ['Quantifier', '"for every tree"'],
    ['Interaction net', 'parts that meet and get replaced'],
    ['Behavioural equivalence', 'does the same thing']
  ]
</script>

<svelte:head>
  <title>Why disp · write the test, get the program</title>
  <meta
    name="description"
    content="What disp is trying to do and how: you write a test, a machine finds a fast program that provably passes it. Which parts work today, and which are bets."
  />
</svelte:head>

<div class="container why">
  <header class="intro">
    <h1>Write the test.<br /><span class="grad-text">Get the program.</span></h1>
    <p class="lede">
      Today you write a program and hope it is right. Disp's goal is that you write only the test, and a machine
      finds a fast program that provably passes it. That machine does not exist yet. This page explains why anyone
      would try, how disp is trying, and which parts work.
    </p>
    <p class="how">
      About 15 minutes. The first three sections work alone, in about 3. Code marked ▶ and the widgets that run programs
      use the real compiler, in this tab. Anything recorded, sketched or written out by hand says so.
    </p>
  </header>

  <div class="layout">
    <article>
      <p class="part-title">Part 1 · The problem</p>

      <section class="scene" id="triangle">
        <h2>Three jobs, all done by hand</h2>
        <p>
          Making software means three things: saying what you want, writing how to do it, and checking the two match.
          People do all three by hand. Every tool today gives up something. Hover over a dot to see what.
        </p>
        <Triangle />
      </section>

      <section class="scene" id="urgent">
        <h2>Why now</h2>
        <p>
          AI made writing code cheap. It did not make checking code cheap, so code that nobody has read piles up. A coding
          agent is also the closest thing today to "just tell it what you want", and even it needs a precise way to state
          what you want. Otherwise the only check is reading the result.
        </p>
        <Queue />
      </section>

      <section class="scene" id="gate">
        <h2>What we want instead</h2>
        <p>
          Two machines in a row. A <strong>gate</strong> says pass or fail. A <strong>dial</strong> ranks whatever passes by how
          much it costs to run. If both are programs, then "find the best program" is a search with a score, and anything
          can do the searching, since a wrong answer cannot get through. That is the map at the side of this page, and each
          part shows how much of it exists.
        </p>
        <p>The running example is doubling a number. It is dull, but it is real, so this page can say exactly what happens today. Try every program, and leave the short table for last.</p>
        <Gate />
        <p>That raises four questions, and the rest of the page takes them in order:</p>
        <ol class="four">
          <li>How can a checker read a program at all?</li>
          <li>What exactly is a test, and how can it cover every input?</li>
          <li>Where do candidates come from?</li>
          <li>What does "cheap" mean?</li>
        </ol>
        <div class="short-end">
          <strong>That is the short version.</strong> The rest is how disp tries to do it, and where it might fail.
        </div>
      </section>

      <p class="part-title">Part 2 · How disp tries to do it</p>

      <section class="scene" id="read">
        <h2>How can a checker read a program?</h2>
        <p>
          In most languages a function is a sealed box: you can call it, but not look inside. A checker that wants to reason
          about a function has to work from a separate description of it, and the two can drift apart.
        </p>
        <p>
          Disp is built on Barry Jay's <a href="https://treecalcul.us/" target="_blank" rel="noopener">tree calculus</a>,
          where everything is a tree grown from one leaf, <code>t</code>, including programs. There are five rewrite rules, and
          three of them look at the shape of a tree. So any program can open any other, with no extra machinery.
        </p>
        <ShapeProbe />
        <Code code={SHAPE_OF} runnable context={CONTEXT} />
        <p>Here are those three rules running, one rewrite at a time:</p>
        <TreeVis variant="lab" presets={SHAPE_LAB} exprs={[SHAPE_LAB[0].expr]} height={300} />
      </section>

      <section class="scene" id="spec">
        <h2>What is a test, exactly?</h2>
        <p>
          A <Gloss tip="The research word is a claim. It is what other languages call a type or a specification.">claim</Gloss>:
          a test that always finishes, asked about every tree. Picture every tree there is going past on a conveyor belt, smallest first.
        </p>
        <p>
          Searching the belt can find a failure, and a failure is final. What it can never do is reach the end. So there are
          three stamps, and <strong>Open</strong> only means "no failure so far". Set the budget to 60 and run the program that is
          wrong only on 3. Then raise it to 80.
        </p>
        <Belt />
        <Code code={BELT_CODE} runnable context={CONTEXT} />
        <p>
          The last line is the one way to Proved so far: <code>Bool</code> has two values, so there are only two cases to list. Numbers do not stop at two.
        </p>
      </section>

      <section class="scene" id="shortcut">
        <h2>How do you get Proved on an endless belt?</h2>
        <p>
          With a <Gloss tip="The research word is a lens: it carries answers one way and failures the other.">certified shortcut</Gloss>.
          Fold the belt onto a few cards, so every tree lands on a card that fails whenever that tree does. Then checking the
          cards checks the belt. For numbers, two cards are enough: zero, and one more than some number.
        </p>
        <Cards />
        <Code code={INDUCTION_CODE} runnable context={CONFIRMER_CONTEXT} />
        <p class="small-note">The ▶ above loads the whole symbolic checker first, about 10 seconds.</p>
        <p>
          A shortcut is not trusted because it is clever. Its promise, "fail on the card whenever you fail on the belt", is itself a claim, so
          the belt can go after the shortcut too. Below, a good cover stays Open. A bad one, which sends every tree to
          the same card, is caught at once.
        </p>
        <Code code={COVER_CODE} runnable context={CONTEXT} />
        <p>
          So the trust comes down to a handful of rules for building shortcuts (five today), and everything above them gets
          checked by running. Those five have not themselves been proved sound.
        </p>
      </section>

      <section class="scene" id="same">
        <h2>What does "same" even mean?</h2>
        <p>
          This is the crux of the project. An optimizer replaces a program with a different, faster one that does the same thing.
          So it has to know two different programs do the same thing.
        </p>
        <p>
          Comparing two trees is instant, and it is what makes disp fast, but it is too strict: it says these two doubling programs differ.
          Comparing what they do is what an optimizer needs, and in general no program can decide that.
        </p>
        <Same />
        <p>
          The bet is that a checkable kind of "same" reaches far enough to allow the rewrites that matter. Disp has no proof of
          that. If it reaches only a little past identical trees, the project fails.
        </p>
      </section>

      <section class="scene" id="search">
        <h2>Where do candidates come from?</h2>
        <p>
          The number of trees explodes with size, so nothing can try them all. The way in is half-written programs. Run one, and either it gets an answer
          or it stops at a gap it needs filled:
        </p>
        <ul>
          <li><strong>Refuted:</strong> it failed without looking at its gaps, so every way of filling them fails too, and the whole branch ends.</li>
          <li><strong>Proved:</strong> the search ends.</li>
          <li><strong>Open:</strong> the branch stays alive.</li>
        </ul>
        <Search />
        <p>
          Since nothing a proposer says is trusted, it can be anything, including an LLM. That also moves the risk. Certificates
          stop wrong programs. They do not stop a test that asks the wrong question, and a strong search will find every gap in a weak test.
        </p>
        <p>
          Almost none of this is built in disp: just a small counterexample loop,
          <a href="{REPO}/blob/main/lib/claim.disp#L30" target="_blank" rel="noopener"><code>cegis</code></a>, that keeps the failures it finds and tries them first on the next candidate.
        </p>
      </section>

      <section class="scene" id="fast">
        <h2>What does "fast" mean?</h2>
        <p>
          Step counts are not time. Most cost models also pretend that moving data across a chip is free, and that a thousand
          things cannot happen at once. If the meter is wrong, the search will carefully optimize the wrong thing. So disp has
          one model that keeps the same rewrite rules and makes distance real.
        </p>
        <Cost />
        <details class="side">
          <summary>A side path: programs that run backward</summary>
          <p>
            Erasing information costs heat in physics, so a program that can run backward could in principle avoid that cost. Disp has an
            experiment on it: <a href="{base}/evaluators/two-way/">two-way programs</a>, also
            <a href="{base}/evaluators/two-way-inet/">as an interaction net</a>. Its link to the main goal is the least settled part of the project.
          </p>
        </details>
      </section>

      <section class="scene" id="itself">
        <h2>What if you aim it at itself?</h2>
        <p>
          The optimizer is a program, so it can be a candidate. Give it a test for "a better optimizer" and run it. That is the
          real goal, and it is why every piece has to live inside one language, where it can read itself.
        </p>
        <Fold />
        <p>Three walls stand in the way:</p>
        <ul>
          <li>
            <strong>Trust.</strong> A system cannot fully prove its own checker sound (Gödel, Löb), so a small core stays outside the
            loop, read by hand.
          </li>
          <li>
            <strong>Finding proofs.</strong> Earlier attempts, like Gödel machines, stalled here. Checking a proof is cheap, but finding one is the whole problem.
          </li>
          <li>
            <strong>The wrong question.</strong> A program can be proved correct against a test that does not say what you meant.
          </li>
        </ul>
      </section>

      <section class="scene" id="score">
        <h2>Scoreboard</h2>
        <p>
          Six questions decide this. Each comes with what would count as failure. The full version is in
          <a href="{REPO}/blob/main/FOUNDATIONS.md" target="_blank" rel="noopener">FOUNDATIONS.md</a>.
        </p>
        <div class="board">
          {#each QUESTIONS as row, i (row.plain)}
            <div class="q">
              <span class="n">{i + 1}</span>
              <div>
                <h3>{row.plain}</h3>
                <p><span class="why-stamp" class:open={row.status === 'Open'} class:partly={row.status === 'Partly'}>{row.status}</span> {row.detail}</p>
                <p class="kill"><span>Would kill it:</span> {row.kill}</p>
              </div>
            </div>
          {/each}
        </div>
        <h3 class="sub">If it works</h3>
        <p>
          Nobody has to agree on language features, because syntax and types are library code and programs can be translated
          between dialects with proof. And programming becomes saying what you want, with the search doing the rest.
        </p>
        <blockquote>
          "In a world where there is so much complexity that I simply can't understand on its own, I need a language I can
          structure the world inside, so I can understand what is happening a little bit better."
          <cite>The author of disp</cite>
        </blockquote>
        <div class="next">
          <a class="btn" href="{base}/playground/">Try the playground</a>
          <a class="btn" href="{REPO}" target="_blank" rel="noopener">Read the source</a>
        </div>
      </section>

      <section class="plain">
        <h2>Plain words for the research words</h2>
        <p>This page avoids the terms on the left. You will meet them if you read the repo.</p>
        <table>
          <thead><tr><th>Research word</th><th>On this page</th></tr></thead>
          <tbody>
            {#each PLAIN as [word, plain] (word)}
              <tr><td>{word}</td><td>{plain}</td></tr>
            {/each}
          </tbody>
        </table>
      </section>
    </article>

    <aside class="rail">
      <StatusMap {active} />
      <p class="rail-note">The whole loop, and how much of each part exists today.</p>
    </aside>
  </div>
</div>

<style>
  .why {
    padding-block: 2.6rem 3rem;
  }
  .intro {
    max-width: 46rem;
    margin-bottom: 2.4rem;
  }
  h1 {
    margin: 0 0 1rem;
    font-size: clamp(2.5rem, 6vw, 4rem);
    line-height: 1.05;
  }
  .lede {
    margin: 0 0 0.8rem;
    font-size: 1.15rem;
    line-height: 1.6;
  }
  .how {
    margin: 0;
    color: var(--fg-muted);
    font-size: 0.92rem;
  }
  .layout {
    display: grid;
    grid-template-columns: minmax(0, 1fr) 15rem;
    gap: 3rem;
    align-items: start;
  }
  article {
    min-width: 0;
    max-width: 46rem;
  }
  .rail {
    position: sticky;
    top: calc(var(--nav-h) + 1rem);
  }
  .rail-note {
    margin: 0.7rem 0 0;
    font-size: 0.72rem;
    color: var(--fg-faint);
  }
  .part-title {
    margin: 3rem 0 0;
    padding-bottom: 0.3rem;
    font-size: 0.78rem;
    font-weight: 600;
    letter-spacing: 0.08em;
    text-transform: uppercase;
    color: var(--accent);
    border-bottom: 1px solid var(--border-strong);
  }
  .part-title:first-child {
    margin-top: 0;
  }
  .scene {
    padding-top: 1.6rem;
    scroll-margin-top: calc(var(--nav-h) + 0.5rem);
  }
  h2 {
    margin: 0 0 0.6rem;
    font-size: clamp(1.45rem, 3vw, 1.9rem);
  }
  p,
  li {
    line-height: 1.7;
  }
  .four {
    margin: 0.3rem 0 1rem;
  }
  .short-end {
    padding: 0.8rem 1rem;
    border-radius: var(--radius);
    background: color-mix(in oklab, var(--g1) 12%, var(--bg-elev));
    border: 1px solid var(--border);
  }
  .small-note {
    margin-top: -0.6rem;
    font-size: 0.8rem;
    color: var(--fg-faint);
  }
  .side {
    margin-top: 1rem;
  }
  .side summary {
    cursor: pointer;
    color: var(--fg-muted);
  }
  .board {
    display: flex;
    flex-direction: column;
    gap: 0.9rem;
    margin: 1rem 0 1.6rem;
  }
  .q {
    display: grid;
    grid-template-columns: 1.8rem minmax(0, 1fr);
    gap: 0.5rem;
    padding: 0.8rem 1rem;
    border: 1px solid var(--border);
    border-radius: var(--radius);
    background: var(--bg-elev);
  }
  .n {
    font-family: var(--font-mono);
    color: var(--accent);
  }
  .q h3 {
    margin: 0 0 0.3rem;
    font-family: var(--font-body);
    font-size: 0.96rem;
    font-weight: 600;
    line-height: 1.4;
  }
  .q p {
    margin: 0 0 0.25rem;
    font-size: 0.86rem;
    line-height: 1.5;
    color: var(--fg-muted);
  }
  .partly {
    color: var(--warn);
  }
  .kill span {
    color: var(--err);
    font-weight: 560;
  }
  .sub {
    margin: 1.4rem 0 0.4rem;
    font-size: 1.15rem;
  }
  blockquote {
    margin: 1.2rem 0;
    padding-left: 1rem;
    font-family: var(--font-display);
    font-size: 1.08rem;
    line-height: 1.55;
    border-left: 3px solid var(--g2);
  }
  cite {
    display: block;
    margin-top: 0.3rem;
    font-family: var(--font-body);
    font-size: 0.8rem;
    font-style: normal;
    color: var(--fg-muted);
  }
  .next {
    display: flex;
    flex-wrap: wrap;
    gap: 0.7rem;
  }
  .plain {
    margin-top: 3rem;
    padding-top: 1.4rem;
    border-top: 1px solid var(--border-strong);
  }
  table {
    width: 100%;
    border-collapse: collapse;
    font-size: 0.92rem;
  }
  th {
    text-align: left;
    font-size: 0.8rem;
    color: var(--fg-muted);
    border-bottom: 1px solid var(--border-strong);
    padding: 0.3rem 0.5rem 0.3rem 0;
  }
  td {
    padding: 0.35rem 0.5rem 0.35rem 0;
    border-bottom: 1px solid var(--border);
  }

  @media (max-width: 980px) {
    .layout {
      display: block;
    }
    .rail {
      position: fixed;
      left: 0;
      right: 0;
      bottom: 0;
      top: auto;
      z-index: 50;
      padding: 0.35rem;
      background: var(--bg);
      border-top: 1px solid var(--border-strong);
    }
    .rail-note {
      display: none;
    }
    .why {
      padding-bottom: 6rem;
    }
  }
</style>
