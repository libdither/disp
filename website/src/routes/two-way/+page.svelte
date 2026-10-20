<script lang="ts">
  import PatternVis from "$lib/twoway/PatternVis.svelte";
</script>

<svelte:head>
  <title>Two-way programs · disp</title>
  <meta
    name="description"
    content="A reversible calculus read like tree calculus: every tree is a program, every step of a run can be taken back, and independent calls run at once."
  />
</svelte:head>

<div class="container page">
  <h1>Two-way programs, <span class="grad-text">step by step</span></h1>
  <p class="lede">
    A program here is a tree, read the way tree calculus reads one, and every tree is a program that
    can run backward. A run starts as one bud: the program applied to its input. Firing a bud grows
    what its clause builds, and the calls in there are new buds; calls that do not wait on each other
    fire together. Every step can be taken back.
  </p>

  <PatternVis />

  <h2>How to read the tree</h2>
  <p>
    It is drawn and driven like the tree-calculus visualizer: <kbd>▸</kbd> fires the next step,
    <kbd>▶</kbd> runs, <kbd>◂</kbd> takes the last step back, and the arrow keys do the same. A
    <strong>bud</strong> (the dashed ring) is a call waiting to fire. On its left is the program, in
    purple, folded into a green pod when it is a big named one (click it to open it, click an opened
    part to fold it); on its right is the argument. A ready bud wears the clause it will fire, and
    the line under the tree says which clause fired and what it was.
  </p>
  <p>
    When a call fires, its program stays where it was, flipped to its dagger: <code>not</code>
    becomes <code>not†</code>, the program that undoes it, and only the right side changes. That is
    <code>apply(f, x) = (f†, y)</code> at every call. Once an enclosing call takes the value in, that
    residual becomes part of the enclosing call's record, and undoing the enclosing call brings it
    back. Click a fired call and what is ready above it runs; once it is done, clicking steps it back
    (↺) until it is a bud again. Going back is checked, not replayed: a call is only undone if running
    its program backward on what it gave returns what it took.
  </p>
  <p>
    A clause run backward matches the side that has the calls in it. Each of those becomes a call the
    other way, and a blue <strong>match</strong> node waits for them before it builds (try
    <em>sub = add†</em>).
  </p>

  <h2>What one step is</h2>
  <p>
    One step here is one clause firing: its whole left side is matched and its whole right side
    built, at once. Nothing in between is hidden, because the clause is the rule. But it is a bigger
    step than tree calculus takes: each of tree calculus's five rules looks at two or three nodes,
    while a clause looks at as many as its patterns have. Taking clauses apart into steps that small
    means compiling them into a fixed set of local rules, which is still an open design question
    here.
  </p>

  <h2>What runs in parallel</h2>
  <p>
    F is <code>△</code> and T is <code>△△</code>, the two smallest trees, so a boolean is a bit, and a
    word is a balanced tree of bits. NOT on a word is three clauses: flip a bit, or split a fork into
    two calls, <code>(x, y) ⇄ (self(x), self(y))</code>. The two halves never wait on each other, so
    all the calls at one depth fire together: 8 bits take 4 steps, 64 bits take 7. CNOT on two
    registers stored bit by bit is the same shape.
  </p>
  <p>
    Adding is harder, because a carry can travel the whole word. The adder here adds in place,
    <code>(a, b) → (a, a + b)</code>, with nothing left over, in passes that each run over the tree in
    parallel: it works out every carry the way a carry-lookahead adder does (each subtree's status,
    whether it kills, passes or makes a carry, goes up the tree, then the carries come down it), adds
    in place, and then takes the carries back out. They can be taken out because the carries of
    a + not(sum) are exactly the carries of a + b. The number of steps grows with the depth of the
    tree, not the width: 86 for 8 bits, 149 for 64, where adding numbers counted in stems takes one
    call at a time.
  </p>

  <h2>A program is a tree</h2>
  <p>
    A clause is a pair of patterns, and a program is a chain of clauses ending in a leaf, which
    leaves anything no clause fits as it is. <code>not</code>, written <code>F ⇄ T; T ⇄ F</code>, is
    the tree
  </p>
  <pre><code>((△, △△), ((△△, △), △))</code></pre>
  <p>
    Its first clause is <code>(△, △△)</code>: F on the left, T on the right. A constant with a fork
    in it gets a stem on the left part of each fork when it is written in a pattern, because a pattern
    fork with a bare leaf on its left is a variable and one with a fork there is a call; leaves and
    stems are written as they are.
  </p>

  <h2>Tree calculus, made reversible</h2>
  <p>
    Tree calculus has five rules, chosen by the program's shape. The first two only build (a leaf
    makes a stem, a stem makes a fork), and building loses nothing. The second is kept as it is:
    <code>△a</code> pairs its input with a. The leaf becomes the program that does nothing, which ends
    every chain of clauses. The other three rules lose their input: K throws it away, S copies it, and
    triage forgets which shape it saw. Their places go to the dagger <code>(△, b)</code>, composition
    <code>(△a, b)</code>, and a clause <code>((l, r), b)</code>. Inside a clause the same five shapes
    are read again as patterns, and matching a pattern is building it backward: that is why a clause
    runs both ways.
  </p>
  <p>
    Running a program gives a value and a residual, the program that undoes the run:
    <code>apply(f, x) = (f†, y)</code> with <code>f† = (△, f)</code>, and
    <code>apply(f†, y) = (f, x)</code>. Nothing was thrown away, so nothing else is needed to go
    back.
  </p>

  <h2>Where it comes from</h2>
  <p>
    Clauses that run both ways, with “a clause may only give what no earlier clause's output side
    fits”, are the reversible functional languages' <em>symmetric first-match policy</em> (<a
      href="https://arxiv.org/pdf/2105.09929#page=2">Kaarsgaard and Rennela</a
    >). The strand lattice runs a fixed instruction set instead
    (<code>evaluators/rust-ic-strands/crate/src/twoway.rs</code>), each instruction a small rewrite
    agents can do on their own; every one of them is a program of a clause or two here.
  </p>
</div>

<style>
  .page {
    padding-bottom: 3rem;
  }
  h2 {
    margin-top: 2rem;
  }
  pre {
    overflow-x: auto;
    font-size: 0.86rem;
  }
</style>
