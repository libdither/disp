<script lang="ts">
  import LinearVis from '$lib/evaluators/linear/LinearVis.svelte'
</script>

<svelte:head>
  <title>Linear tree calculus · disp</title>
  <meta
    name="description"
    content="A tree calculus in nine rules where only one rule drops and only one rule copies: programs written with binders, the binders taken off one step at a time, then the tree run one rule at a time."
  />
</svelte:head>

<div class="container page">
  <h1>Linear tree calculus, <span class="grad-text">step by step</span></h1>
  <p class="lede">
    Tree calculus with its copying rule split by where the argument goes. Nine rules; only K drops
    something and only W copies something, and triage hands back the arms it did not pick. Write a
    program with binders: bracket abstraction takes each binder off as a step of its own, and then the
    tree runs, one rule at a time.
  </p>

  <LinearVis />

  <h2>How to read the tree</h2>
  <p>
    The tree hangs from its root. A <strong>bud</strong> is an application waiting to run; once it can,
    it wears the letter of the rule it will fire, and the next one to fire is ringed. Parts that a step
    keeps travel to where the step puts them, the argument K drops falls off the tree, and both copies
    W makes start from the argument. A <strong>binder</strong> is the pill with its name in braces;
    when it comes off, it moves down into the side of the body that holds its variable (both sides for W),
    or knocks its variable off when the body is just that variable. With <em>Show combinators</em> on,
    the shapes that are combinators sit in a tinted band with their letter, coloured by what they do to
    their argument.
  </p>
  <p>
    <kbd>▶</kbd> plays, <kbd>▶|</kbd> and <kbd>◀</kbd> step (or the arrow keys), and the reel under the tree
    is the whole run: binders first, then the rules, each cell coloured by the rule it fired. Drag the
    reel to scrub. Click a ready bud to fire it early, a folded part to open it, and an open named part's
    tag to fold it again.
  </p>

  <h2>The rules</h2>
  <p>
    A function is a fork <code>△ a b</code>. Applied to <code>x</code>, its left child
    <code>a</code> and one more node pick the rule, and the rule eats exactly the three nodes it read.
  </p>
  <table class="rules">
    <thead>
      <tr><th></th><th>rule</th><th>redex → result</th><th>what happens to x</th></tr>
    </thead>
    <tbody>
      <tr><td><i class="ok"></i></td><td>I</td><td><code>△ △ △ x → x</code></td><td>it is the answer</td></tr>
      <tr><td><i class="warn"></i></td><td>K</td><td><code>△ △ (△ q) x → q</code></td><td>dropped: the only rule that drops</td></tr>
      <tr><td><i class="left"></i></td><td>C</td><td><code>△ △ (△ q r) x → q x r</code></td><td>goes between q and r</td></tr>
      <tr><td><i class="kw"></i></td><td>W</td><td><code>△ (△ △) b x → b x x</code></td><td>into b, twice: the only rule that copies</td></tr>
      <tr><td><i class="acc"></i></td><td>B</td><td><code>△ (△ (△ f)) b x → f (b x)</code></td><td>into b, then f</td></tr>
      <tr><td><i class="acc"></i></td><td>B′</td><td><code>△ (△ (△ f g)) b x → f (g (b x))</code></td><td>into b, then g, then f</td></tr>
      <tr><td><i class="err"></i></td><td>triage</td><td><code>△ (△ c d) b △ → c d b</code><br /><code>△ (△ c d) b (△ u) → d c b u</code><br /><code>△ (△ c d) b (△ u v) → b c d u v</code></td><td>looked at: its shape picks an arm, and the arm gets the two arms it beat, then x's children</td></tr>
    </tbody>
  </table>
  <p>
    The left child <code>△</code> reads the right child (I, K, C); a stem <code>△p</code> reads p (W, B,
    B′); a fork reads x (triage). Applying <code>△</code> itself to one or two things only builds a stem or
    a fork.
  </p>

  <h2>Taking binders off</h2>
  <p>A binder comes off once its body has no binders left. Which step it takes depends on where its variable is:</p>
  <table class="rules">
    <tbody>
      <tr><td><code>{'{x} => x'}</code></td><td>becomes <code>I</code></td></tr>
      <tr><td><code>{'{x} => M'}</code>, x unused</td><td>becomes <code>K M</code></td></tr>
      <tr><td><code>{'{x} => M x'}</code></td><td>becomes <code>M</code></td></tr>
      <tr><td><code>{'{x} => M N'}</code>, x only in N</td><td>becomes <code>B M ({'{x} => N'})</code>, and <code>B M (B g r)</code> merges into <code>B′ M g r</code></td></tr>
      <tr><td><code>{'{x} => M N'}</code>, x only in M</td><td>becomes <code>C ({'{x} => M'}) N</code></td></tr>
      <tr><td><code>{'{x} => M N'}</code>, x in both</td><td>becomes <code>W ({'{x} => {x′} => M N′'})</code>, with x renamed x′ in N</td></tr>
    </tbody>
  </table>
  <p>
    A tree <code>△ a b</code> inside a body reads as the application <code>(△ a) b</code>. These are the
    same trees the prototype's compiler makes.
  </p>

  <h2>Writing programs</h2>
  <p>
    <code>{'{x, y} => body'}</code> is a binder, <code>f x y</code> or <code>f(x, y)</code> an application,
    <code>t</code> (or <code>△</code>) the leaf, and <code>3</code> three stems on a leaf.
    <code>name := term</code> defines a name to use below, and the last line is what runs. Any other name
    is a symbol and stays as it is, so a run that ends on a symbol stops there.
  </p>
</div>

<style>
  .page {
    padding-top: 28px;
    padding-bottom: 60px;
  }
  .lede {
    max-width: 62rem;
  }
  .rules {
    border-collapse: collapse;
    font-size: 0.9rem;
    margin: 10px 0 14px;
  }
  .rules td,
  .rules th {
    padding: 5px 10px;
    border-bottom: 1px solid var(--border);
    text-align: left;
    vertical-align: top;
  }
  .rules i {
    display: inline-block;
    width: 10px;
    height: 10px;
    border-radius: 3px;
    background: var(--c);
  }
  .ok { --c: var(--hl-number); }
  .warn { --c: var(--warn); }
  .left { --c: var(--hl-atom); }
  .kw { --c: var(--hl-kw); }
  .acc { --c: var(--accent); }
  .err { --c: var(--err); }
</style>
