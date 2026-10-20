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
    content="A reversible calculus on trees: programs are trees put together from five moves; a run is fired one small local move at a time, and flipped to run it back."
  />
</svelte:head>

<div class="container page">
  <h1>Two-way programs, <span class="grad-text">step by step</span></h1>
  <p class="lede">
    A program here is a tree, put together from five moves that rearrange a tree and a few ways to
    combine programs. A run is a tree of dots, each a program on a value, and there are two things you
    can do to it: fire makes one small move at one dot, and flip turns a dot around without computing
    anything, so that firing runs it back. Dots that do not wait on each other fire at once.
  </p>

  <TwoWayVis />

  <h2>How to read the tree</h2>
  <p>
    Each <strong>dot</strong> is a program on a value: the purple tree on its left, the value on its
    right. A live dot (the dashed ring) wears the move it will make. When a combinator starts, it takes
    the value apart and hands the parts to new dots, and the parts of its program that run travel up
    into them, each leaving a dotted <strong>slot</strong> in the program, which stays open where it
    was. <code>map</code> and <code>tri</code> hold on to the node they opened, which stays where the dot
    was inside a blue (map) or amber (tri) ring; the other combinators stay a dashed bud, the way the
    tree-calculus visualizer draws an application waiting on the ones inside it. A part that has run
    comes back into its slot, and when every dot above a ring or bud has run, it closes, putting back
    what it held, with its program whole again. A dot that has run has its program
    <strong>held</strong>: a lid, a fork with △△ on its left, sits on top of the program, and the ring
    turns solid.
  </p>
  <p>
    <kbd>▸</kbd> fires every move that can be made right now (one round), or just the next one with
    parallel off; <kbd>▶</kbd> keeps firing, and <kbd>◂</kbd> takes the last action back.
    <kbd>⇆</kbd> flips the whole run. Clicking a live dot fires it, clicking a ring or a waiting bud
    plays the part under it until that part has run, and clicking a dot that has run flips just that
    dot. A program with a † on top
    runs backward, and its dot is drawn blue. The moves and big named parts inside a program are pods,
    which open when they reach a dot of their own; click any part to fold it, and a pod to open it.
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
      <tr><td><code>(△△, b)</code></td><td><code>held(b)</code></td><td>nothing: b has run, and is held until it is flipped</td></tr>
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
      <tr><td><code>△△△</code></td><td><code>swap</code></td><td>(u, v) becomes (v, u)</td></tr>
      <tr><td><code>△△△△</code></td><td><code>cross</code></td><td>((u, v), w) becomes ((u, w), v)</td></tr>
      <tr><td><code>△△△△△</code></td><td><code>sink</code></td><td>△(u, v) becomes (△u, v), and (△u, v) becomes △(u, v)</td></tr>
      <tr><td><code>△△△△△△</code></td><td><code>stem</code></td><td>x becomes △x</td></tr>
      <tr><td><code>△△△△△△△</code></td><td><code>unit</code></td><td>x becomes (△, x)</td></tr>
      <tr><td><code>△△△△△△△△</code></td><td><code>eval</code></td><td>(q, z) becomes (q, q z): the left part run as a program</td></tr>
    </tbody>
  </table>
  <p>
    <code>swap</code>, <code>cross</code> and <code>sink</code> leave a tree they do not fit as it is,
    so each is its own inverse, and the others build on them: <code>rot</code>, which makes
    ((u, v), w) into (u, (v, w)), is <code>first(swap) ; cross ; swap</code>. Any other tree is not a
    program, and running it gets stuck. So does a move with nothing to undo: backward,
    <code>stem</code> needs a stem and <code>unit</code> a (△, x). A held part is never started, since it stands for a run that has already
    happened, so a program that would start one is stuck there too. <code>first(f)</code>,
    <code>second(f)</code> and <code>par(f, g)</code> are short for <code>map(id, f, id)</code>,
    <code>map(id, id, f)</code> and <code>map(id, f, g)</code>, and <code>when(f)</code> for
    <code>tri(id, tri(f, id, id), id)</code>: when the left part is T, f runs on the right.
  </p>
  <p>
    Even <code>not</code> is built from the moves:
    <code>unit ; swap ; tri(stem, id, id) ; swap ; tri(stem, id, id)† ; swap ; unit†</code>. It
    makes an empty slot beside the bit, sets the slot where the bit is F, clears the bit where the slot
    stayed F, and drops the cleared bit. Then <code>cnot</code> is <code>when(not)</code>, and
    flipping every bit of a word is <code>fix(map(not, self, self))</code>.
  </p>

  <h2>Fire and flip</h2>
  <p>
    Fire rewrites one node of the run and looks no further than the nodes just under it. There are
    twenty-eight kinds: a live dot starts (<code>id</code> does nothing, one of the five moves
    rearranges its value, a combinator becomes a frame over dots for its parts), a frame whose dots have
    all run closes, or a sequence goes on to its second part. A dot that has run gets its program held.
    The same rules run a program backward: they read through the †s on top of a program, a move runs
    as its inverse (<code>stem</code> as <code>stem†</code>, <code>unit</code> as <code>unit†</code>,
    and the other three as themselves), and each part is turned around as it starts. No move
    copies or drops a tree: the five moves only relink, add or take away the nodes they name, and a
    frame keeps every node it takes apart until it puts it back. Copying is a program like any other
    (<code>copy</code> below), and it walks the tree it copies a node at a time.
  </p>
  <p>
    Flip computes nothing. On a dot, it takes the lid off and turns the program around, or turns it
    around and puts a lid on: <code>held(p)</code> becomes <code>p†</code>, and <code>p</code> becomes
    <code>held(p†)</code>. On a frame, it flips every dot under it. Two lids or two †s cancel, so flip
    is its own inverse on every tree. Undoing a move is flip, fire, flip, which is why there is no table
    of inverse rules, and why going back needs no record of the run: as in
    <a href="https://mathweb.ucsd.edu/~sbuss/CourseWeb/Math268_2013W/Bennett_Reversibiity.pdf#page=2"
      >Bennett's reversible machines</a
    >, nothing was thrown away. Firing <code>add</code> on (3, 2) ends with (3, 5) and
    <code>held(add)</code>. Flipped, that is <code>add†</code> on (3, 5); firing it ends with (3, 2) and
    <code>held(add†)</code>, and flipped again it is <code>add</code> on (3, 2), where it started.
  </p>

  <h2>What runs in parallel</h2>
  <p>
    <code>map</code> starts both halves of a fork at once, and nothing else waits on them until they
    are done. A word is a balanced tree of bits, each in a stem, so <code>notw</code> reaches all of
    its bits together: 8 bits take 45 rounds and 64 bits take 57, four more each time the word doubles,
    and most of the 45 is <code>not</code> itself. The work grows with the number of bits; the rounds
    grow with the depth of the tree.
  </p>
  <p>
    Adding is harder, because a carry can travel the whole word. <code>addw</code> adds in place,
    <code>(a, b) → (a, a + b)</code>, with nothing left over, the way a carry-lookahead adder does: a
    status for every part of the word goes up the tree (does it kill a carry, pass one on, or make
    one), the carries come down it, every b becomes its bit of the sum, and then the carries are taken
    back out, which works because the carries of <code>a + not(a + b)</code> are the carries of
    <code>a + b</code>. It is a long program in small moves, and how long depends a little on the
    bits: 2,300 to 2,700 rounds at 4 bits, 2,900 to 3,600 at 8, and about 5,700 at 64. Its rounds,
    too, grow with the depth of the tree and not its width.
  </p>

  <h2>Programs are trees</h2>
  <p>
    A program is a tree, so a program can be a value. <code>unit</code> puts (△, ·) on top of a tree,
    which turns a program into its dagger, the program that runs it backward, and <code>eval</code>
    runs the left part of a fork on the right part and keeps it. So <code>first(unit) ; eval</code>,
    given <code>([add], (2, 7))</code>, subtracts. Whether a run has happened is part of the tree too:
    the lid. Tree calculus reads a program the same way, by its top shape. Its triage, which picks by
    the shape of a value and forgets it, comes back here as <code>tri</code> and <code>map</code>, which
    put the shape back. Its K and S, which drop and copy their argument, have no counterpart: nothing is
    dropped, and copying is a program.
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
