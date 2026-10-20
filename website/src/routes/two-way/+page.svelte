<script lang="ts">
  import TwoWayVis from "$lib/twoway/TwoWayVis.svelte";
  import { LIBRARY } from "$lib/twoway/library";

  const GROUPS: { key: (typeof LIBRARY)[number]["group"]; title: string }[] = [
    { key: "bits", title: "Bits" },
    { key: "trees", title: "Trees and numbers" },
    { key: "words", title: "Words, all bits at once" },
    { key: "adder", title: "The adder" }
  ];
</script>

<svelte:head>
  <title>Two-way programs · disp</title>
  <meta
    name="description"
    content="A reversible calculus on trees: programs are trees put together from five moves, every step of a run is one small local rewrite, and every step can run backward."
  />
</svelte:head>

<div class="container page">
  <h1>Two-way programs, <span class="grad-text">step by step</span></h1>
  <p class="lede">
    A program here is a tree, put together from five moves that rearrange a tree and a few ways to
    combine programs. Running one is a series of small rewrites, each touching one node of the run and
    the nodes just under it. Every rewrite can run backward, and rewrites that do not wait on each
    other happen at once.
  </p>

  <TwoWayVis />

  <h2>How to read the tree</h2>
  <p>
    A run starts as one <strong>bud</strong> (the dashed ring): a program about to run on the value to
    its right. Its program is the purple tree on its left, with the moves and big named parts inside it
    as pods, which open when they reach a run of their own; click any part to fold it, and a pod to open
    it again. The mark on a bud is the move it will
    make. When a combinator starts, it takes its value apart and hands the parts to new buds, and the
    parts of its program travel up into them: <code>map</code> and <code>tri</code> hold on to the node they opened,
    which stays where the bud was inside a blue (map) or amber (tri) ring, and the others leave a small
    <strong>bead</strong> on the way down: <code>;</code> for a sequence, <code>†</code>,
    <code>μ</code> for <code>fix</code>, <code>↩</code> for <code>self</code>,
    <code>▷</code> for <code>eval</code>, and <code>?</code> for a <code>tri</code> whose left part is
    a fork, which it opens by rotating rather than holding. A <code>;</code> bead keeps the part of the
    sequence still to run beside it, dashed, until its turn. When every part under a ring or bead is done, it closes,
    putting back what it held. A solid ring is a finished run, which keeps its program.
  </p>
  <p>
    <kbd>▸</kbd> makes every move that can be made right now (one round), or just the next one with
    parallel off; <kbd>◂</kbd> undoes the last step, and <kbd>▶</kbd> plays. Clicking a bud makes its
    move, clicking a ring or bead makes the moves that are ready inside it, and clicking a finished run
    steps it back. The <kbd>⇆</kbd> switch runs everything backward: each round undoes every move
    that can be undone, by the move's own inverse, until the run is back where it started. Blue marks
    a run going backward.
  </p>

  <h2>The pieces</h2>
  <p>
    A program is read by its top shape, the way tree calculus reads one: a leaf, a stem or a fork, and
    for a fork, the shape of its left part.
  </p>
  <table class="pieces">
    <thead>
      <tr><th>tree</th><th>written</th><th>on a value x</th></tr>
    </thead>
    <tbody>
      <tr><td><code>△</code></td><td><code>id</code></td><td>x as it is</td></tr>
      <tr><td><code>(△, b)</code></td><td><code>b†</code></td><td>b run backward</td></tr>
      <tr><td><code>(△a, b)</code></td><td><code>a ; b</code></td><td>a, then b on what a gave</td></tr>
      <tr>
        <td><code>((a, b), c)</code></td><td><code>tri(a, b, c)</code></td><td
          >x is a fork (s, y), and s's shape picks: a on y for a leaf, b on (u, y) for a stem △u, c on
          (p, (q, y)) for a fork (p, q); then s goes back together</td
        >
      </tr>
      <tr>
        <td><code>△((a, b), c)</code></td><td><code>map(a, b, c)</code></td><td
          >a on a stem's inside, or b and c on a fork's two halves, at once</td
        >
      </tr>
      <tr><td><code>△(△, b)</code></td><td><code>fix(b)</code></td><td>b, where <code>self</code> runs b again</td></tr>
      <tr><td><code>△△</code></td><td><code>self</code></td><td>the innermost fix's b, again</td></tr>
      <tr><td><code>△△△</code></td><td><code>not</code></td><td>△ and △△ trade places</td></tr>
      <tr><td><code>△△△△</code></td><td><code>swap</code></td><td>(u, v) becomes (v, u)</td></tr>
      <tr><td><code>△△△△△</code></td><td><code>rot</code></td><td>((u, v), w) becomes (u, (v, w))</td></tr>
      <tr><td><code>△△△△△△</code></td><td><code>stem</code></td><td>x becomes △x</td></tr>
      <tr><td><code>△△△△△△△</code></td><td><code>unit</code></td><td>x becomes (△, x)</td></tr>
      <tr><td><code>△△△△△△△△</code></td><td><code>eval</code></td><td>(q, z) becomes (q, q z): the left part run as a program</td></tr>
    </tbody>
  </table>
  <p>
    Any other tree is not a program, and running it gets stuck. So does a move with nothing to undo:
    <code>rot</code> needs a fork on the left, and backward, <code>stem</code> needs a stem and
    <code>unit</code> a (△, x). <code>first(f)</code>, <code>second(f)</code> and
    <code>par(f, g)</code> are short for <code>map(id, f, id)</code>, <code>map(id, id, f)</code>
    and <code>map(id, f, g)</code>, and <code>when(f)</code> for
    <code>tri(id, tri(f, id, id), id)</code>: when the left part is T, f runs on the right. So
    <code>cnot</code> is <code>when(not)</code>, and flipping every bit of a word is
    <code>fix(map(not, self, self))</code>.
  </p>

  <h2>One step is one rewrite</h2>
  <p>
    Each move rewrites one node of the run and looks no further than the nodes just under it. There
    are thirty kinds: a run starts (<code>id</code> is done at once, the five moves rearrange its value,
    a combinator becomes a frame over runs of its parts), a frame whose parts are done closes, or a
    sequence goes on to its second part. No move copies or drops a tree. The five moves only relink,
    add or take away the nodes they name, and a frame keeps every node it takes apart until it puts
    it back. Copying is a program like any other (<code>copy</code> below), and it walks the tree it
    copies a node at a time.
  </p>
  <p>
    Every one of the thirty has an inverse that looks just as far. The ⇆ switch runs those inverses a
    round at a time, so a finished run walks back to its start the way it came, and it needs no record
    of the run to do it: as in
    <a href="https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/Bennett_Reversibiity.pdf#page=2"
      >Bennett's reversible machines</a
    >, nothing was thrown away. A program's dagger is the other way back: <code>f†</code> is a new run,
    forward, that takes what <code>f</code> gave back to what <code>f</code> was given.
  </p>

  <h2>What runs in parallel</h2>
  <p>
    <code>map</code> starts both halves of a fork at once, and nothing else waits on them until they
    are done. A word is a balanced tree of bits, each in a stem, so <code>notw</code> reaches all of
    its bits together: 8 bits take 17 rounds and 64 bits take 29, four more each time the word doubles.
    The work grows with the number of bits; the rounds grow with the depth of the tree.
  </p>
  <p>
    Adding is harder, because a carry can travel the whole word. <code>addw</code> adds in place,
    <code>(a, b) → (a, a + b)</code>, with nothing left over, the way a carry-lookahead adder does: a
    status for every part of the word goes up the tree (does it kill a carry, pass one on, or make
    one), the carries come down it, every b becomes its bit of the sum, and then the carries are taken
    back out, which works because the carries of <code>a + not(a + b)</code> are the carries of
    <code>a + b</code>. It is a long program in small moves, and how long depends a little on the
    bits: 1,400 to 2,000 rounds at 4 bits, 2,000 to 2,500 at 8, and under 4,000 at 64. Its rounds,
    too, grow with the depth of the tree and not its width.
  </p>

  <h2>Programs are trees</h2>
  <p>
    A program is a tree, so a program can be a value. <code>unit</code> puts (△, ·) on top of a tree,
    which turns a program into its dagger, the program that runs it backward, and <code>eval</code>
    runs the left part of a fork on the right part and keeps it. So <code>first(unit) ; eval</code>,
    given <code>([add], (2, 7))</code>, subtracts. Tree calculus reads a program the same way, by its
    top shape. Its triage, which picks by the shape of a value and forgets it, comes back here as
    <code>tri</code> and <code>map</code>, which put the shape back. Its K and S, which drop and copy
    their argument, have no counterpart: nothing is dropped, and copying is a program.
  </p>

  <h2>The library</h2>
  <p>
    Everything in the examples is built from the pieces above, nothing else. A name marked
    <em>part</em> is a piece of a bigger program, and its <code>self</code> is that program's.
  </p>
  {#each GROUPS as g}
    <h3>{g.title}</h3>
    <table class="lib">
      <tbody>
        {#each LIBRARY.filter((d) => d.group === g.key) as d}
          <tr>
            <td class="name"><code>{d.name}</code>{#if d.part}<span class="part">part</span>{/if}</td>
            <td><code class="def">{d.src}</code><div class="doc">{d.doc}</div></td>
          </tr>
        {/each}
      </tbody>
    </table>
  {/each}

  <h2>Where it comes from</h2>
  <p>
    Putting reversible programs together from primitive bijections with sequence, a choice and a
    parallel step is the design of James and Sabry's language Π, whose three ways to combine programs
    are sequential, choice and parallel composition (<a href="https://arxiv.org/pdf/1811.03678#page=10"
      >Carette, James and Sabry</a
    >). Here the choice is made by a tree's shape instead of a sum type, as tree calculus
    makes it, and the result is put back so the choice is never lost. The strand lattice runs a
    similar instruction set as agents (<code>evaluators/rust-ic-strands/crate/src/twoway.rs</code>).
  </p>
</div>

<style>
  .page {
    padding-bottom: 3rem;
  }
  h2 {
    margin-top: 2rem;
  }
  h3 {
    margin: 1.2rem 0 0.3rem;
    font-size: 1rem;
  }
  table {
    border-collapse: collapse;
    width: 100%;
    font-size: 0.88rem;
  }
  th,
  td {
    text-align: left;
    padding: 0.3rem 0.5rem;
    border-bottom: 1px solid var(--border);
    vertical-align: top;
  }
  th {
    font-weight: 600;
    color: var(--fg-faint);
    font-size: 0.78rem;
  }
  .pieces td:first-child {
    white-space: nowrap;
  }
  .lib {
    table-layout: fixed;
  }
  .lib .name {
    width: 8.5rem;
    white-space: nowrap;
  }
  .part {
    margin-left: 0.4rem;
    font-size: 0.7rem;
    color: var(--fg-faint);
    font-style: italic;
  }
  .def {
    white-space: normal;
    overflow-wrap: anywhere;
  }
  .doc {
    color: var(--fg-muted);
    font-size: 0.82rem;
    margin-top: 0.15rem;
  }
</style>
